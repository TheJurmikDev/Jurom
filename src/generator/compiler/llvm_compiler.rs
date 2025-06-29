use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::OptimizationLevel;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::types::{BasicTypeEnum, BasicType};
use inkwell::{AddressSpace, IntPredicate};
use crate::{Expression, Statement};
use std::path::Path;

pub struct LLVMCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    pass_manager: PassManager<FunctionValue<'ctx>>,

    variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    global_variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
    string_literals: HashMap<String, PointerValue<'ctx>>,

    current_function: Option<FunctionValue<'ctx>>,
    printf_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCompiler<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Result<Self, String> {
        let module = context.create_module(module_name);
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| format!("❌ Failed to create execution engine: {}", e))?;

        let pass_manager = PassManager::create(&module);
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_basic_alias_analysis_pass();
        pass_manager.add_promote_memory_to_register_pass();
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.finalize();

        let builder = context.create_builder();

        Ok(LLVMCompiler {
            context,
            module,
            builder,
            execution_engine,
            pass_manager,
            variables: HashMap::new(),
            functions: HashMap::new(),
            global_variables: HashMap::new(),
            string_literals: HashMap::new(),
            current_function: None,
            printf_function: None,
        })
    }

    pub fn compile(&mut self, statements: Vec<Statement>) -> Result<(), String> {
        self.create_printf_function();

        for statement in &statements {
            match statement {
                Statement::FunctionDeclaration { name, .. } => {
                    self.declare_function(name.clone())?;
                }
                Statement::ClassDeclaration { methods, .. } => {
                    for method in methods {
                        if let Statement::FunctionDeclaration { name, .. } = method {
                            self.declare_function(name.clone())?;
                        }
                    }
                }
                _ => {}
            }
        }

        for statement in statements {
            self.compile_statement(statement)?;
        }

        if !self.functions.contains_key("main") {
            self.create_default_main()?;
        }

        Ok(())
    }

    fn create_printf_function(&mut self) {
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();

        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_fn = self.module.add_function("printf", printf_type, None);

        self.printf_function = Some(printf_fn);
    }

    fn declare_function(&mut self, name: String) -> Result<(), String> {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function(&name, fn_type, None);
        self.functions.insert(name, function);
        Ok(())
    }

    fn create_default_main(&mut self) -> Result<(), String> {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let main_function = self.module.add_function("main", fn_type, None);

        let basic_block = self.context.append_basic_block(main_function, "entry");
        self.builder.position_at_end(basic_block);

        let zero = i32_type.const_int(0, false);
        self.builder.build_return(Some(&zero)).unwrap();

        self.functions.insert("main".to_string(), main_function);
        Ok(())
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<(), String> {
        match statement {
            Statement::FunctionDeclaration { name, body } => {
                self.compile_function_declaration(name, body)?;
            }
            Statement::ClassDeclaration { methods, .. } => {
                for method in methods {
                    if let Statement::FunctionDeclaration { name, body } = method {
                        self.compile_function_declaration(name, body)?;
                    }
                }
            }
            Statement::VariableDeclaration { name, value } => {
                self.compile_variable_declaration(name, value)?;
            }
            Statement::Assignment { name, value } => {
                self.compile_assignment(name, value)?;
            }
            Statement::MethodCall { method, args, .. } => {
                self.compile_method_call(method, args)?;
            }
            Statement::IfStatement { condition, then_body, else_body } => {
                self.compile_if_statement(condition, then_body, else_body)?;
            }
            Statement::WhileStatement { condition, body } => {
                self.compile_while_statement(condition, body)?;
            }
        }
        Ok(())
    }

    fn compile_function_declaration(&mut self, name: String, body: Vec<Statement>) -> Result<(), String> {
        let function = self.functions.get(&name).ok_or("❌ Function not found")?.clone();
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        self.current_function = Some(function);
        let saved_vars = self.variables.clone();

        for stmt in body {
            self.compile_statement(stmt)?;
        }

        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            let i32_type = self.context.i32_type();
            let zero = i32_type.const_int(0, false);
            self.builder.build_return(Some(&zero)).unwrap();
        }

        self.pass_manager.run_on(&function);
        self.variables = saved_vars;
        self.current_function = None;

        Ok(())
    }

    fn compile_variable_declaration(&mut self, name: String, value: Expression) -> Result<(), String> {
        let value_result = self.compile_expression(value)?;

        let (alloca, var_type) = match value_result {
            BasicValueEnum::IntValue(int_val) => {
                let i64_type = self.context.i64_type();
                let alloca = self.builder.build_alloca(i64_type, &name).unwrap();
                self.builder.build_store(alloca, int_val).unwrap();
                (alloca, i64_type.as_basic_type_enum())
            }
            BasicValueEnum::PointerValue(ptr_val) => {
                let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                let alloca = self.builder.build_alloca(i8_ptr_type, &name).unwrap();
                self.builder.build_store(alloca, ptr_val).unwrap();
                (alloca, i8_ptr_type.as_basic_type_enum())
            }
            _ => return Err("❌ Unsupported value type".to_string()),
        };

        if self.current_function.is_some() {
            self.variables.insert(name, (alloca, var_type));
        } else {
            self.global_variables.insert(name, (alloca, var_type));
        }

        Ok(())
    }

    fn compile_assignment(&mut self, name: String, value: Expression) -> Result<(), String> {
        let value_result = self.compile_expression(value)?;

        let (var_ptr, _) = self.variables.get(&name)
            .or_else(|| self.global_variables.get(&name))
            .ok_or_else(|| format!("❌ Variable '{}' not found", name))?;

        self.builder.build_store(*var_ptr, value_result).unwrap();
        Ok(())
    }

    fn compile_method_call(&mut self, method: String, args: Vec<Expression>) -> Result<(), String> {
        if method == "println" {
            self.compile_println(args)?;
        } else {
            let function = self.functions.get(&method)
                .ok_or_else(|| format!("❌ Function '{}' not found", method))?;

            self.builder.build_call(*function, &[], "call").unwrap();
        }
        Ok(())
    }

    fn compile_println(&mut self, args: Vec<Expression>) -> Result<(), String> {
        let printf_fn = self.printf_function.ok_or("❌ Printf function not available")?;

        for arg in args {
            let value = self.compile_expression(arg)?;

            match value {
                BasicValueEnum::IntValue(int_val) => {
                    let format_str = self.create_string_literal("%lld\n");
                    self.builder.build_call(
                        printf_fn,
                        &[format_str.into(), int_val.into()],
                        "printf_call"
                    ).unwrap();
                }
                BasicValueEnum::PointerValue(ptr_val) => {
                    let format_str = self.create_string_literal("%s\n");
                    self.builder.build_call(
                        printf_fn,
                        &[format_str.into(), ptr_val.into()],
                        "printf_call"
                    ).unwrap();
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn compile_if_statement(
        &mut self,
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>
    ) -> Result<(), String> {
        let current_fn = self.current_function.ok_or("❌ We are not within the function")?;

        let cond_value = self.compile_expression(condition)?;
        let cond_int = match cond_value {
            BasicValueEnum::IntValue(int_val) => int_val,
            _ => return Err("❌ The condition must be a number".to_string()),
        };

        let zero = self.context.i64_type().const_int(0, false);
        let condition = self.builder.build_int_compare(
            IntPredicate::NE,
            cond_int,
            zero,
            "if_condition"
        ).unwrap();

        let then_block = self.context.append_basic_block(current_fn, "then");
        let else_block = self.context.append_basic_block(current_fn, "else");
        let merge_block = self.context.append_basic_block(current_fn, "merge");

        self.builder.build_conditional_branch(condition, then_block, else_block).unwrap();

        self.builder.position_at_end(then_block);
        for stmt in then_body {
            self.compile_statement(stmt)?;
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_block).unwrap();
        }

        self.builder.position_at_end(else_block);
        if let Some(else_statements) = else_body {
            for stmt in else_statements {
                self.compile_statement(stmt)?;
            }
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_block).unwrap();
        }

        self.builder.position_at_end(merge_block);

        Ok(())
    }

    fn compile_while_statement(&mut self, condition: Expression, body: Vec<Statement>) -> Result<(), String> {
        let current_fn = self.current_function.ok_or("Nejsme v rámci funkce")?;

        let loop_header = self.context.append_basic_block(current_fn, "loop_header");
        let loop_body = self.context.append_basic_block(current_fn, "loop_body");
        let loop_exit = self.context.append_basic_block(current_fn, "loop_exit");

        self.builder.build_unconditional_branch(loop_header).unwrap();

        self.builder.position_at_end(loop_header);
        let cond_value = self.compile_expression(condition)?;
        let cond_int = match cond_value {
            BasicValueEnum::IntValue(int_val) => int_val,
            _ => return Err("❌ The loop condition must be a number".to_string()),
        };

        let zero = self.context.i64_type().const_int(0, false);
        let condition = self.builder.build_int_compare(
            IntPredicate::NE,
            cond_int,
            zero,
            "while_condition"
        ).unwrap();

        self.builder.build_conditional_branch(condition, loop_body, loop_exit).unwrap();

        self.builder.position_at_end(loop_body);
        for stmt in body {
            self.compile_statement(stmt)?;
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(loop_header).unwrap();
        }

        self.builder.position_at_end(loop_exit);

        Ok(())
    }

    fn compile_expression(&mut self, expression: Expression) -> Result<BasicValueEnum<'ctx>, String> {
        match expression {
            Expression::Number(n) => {
                let i64_type = self.context.i64_type();
                let int_val = i64_type.const_int(n as u64, false);
                Ok(int_val.into())
            }
            Expression::String(s) => {
                let str_ptr = self.create_string_literal(&s);
                Ok(str_ptr.into())
            }
            Expression::Variable(name) => {
                let (var_ptr, var_type) = self.variables.get(&name)
                    .or_else(|| self.global_variables.get(&name))
                    .ok_or_else(|| format!("❌ Variable '{}' not found", name))?;

                let loaded_val = self.builder.build_load(*var_type, *var_ptr, &format!("load_{}", name)).unwrap();
                Ok(loaded_val)
            }
            Expression::BinaryOp { left, operator, right } => {
                self.compile_binary_operation(*left, operator, *right)
            }
        }
    }

    fn compile_binary_operation(
        &mut self,
        left: Expression,
        operator: String,
        right: Expression
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let left_val = self.compile_expression(left)?;
        let right_val = self.compile_expression(right)?;

        match (left_val, right_val) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                let result = match operator.as_str() {
                    "+" => self.builder.build_int_add(l, r, "add").unwrap(),
                    "-" => self.builder.build_int_sub(l, r, "sub").unwrap(),
                    "*" => self.builder.build_int_mul(l, r, "mul").unwrap(),
                    "/" => self.builder.build_int_signed_div(l, r, "div").unwrap(),
                    "==" => {
                        let cmp = self.builder.build_int_compare(IntPredicate::EQ, l, r, "eq").unwrap();
                        self.builder.build_int_z_extend(cmp, self.context.i64_type(), "eq_ext").unwrap()
                    }
                    "!=" => {
                        let cmp = self.builder.build_int_compare(IntPredicate::NE, l, r, "ne").unwrap();
                        self.builder.build_int_z_extend(cmp, self.context.i64_type(), "ne_ext").unwrap()
                    }
                    "<" => {
                        let cmp = self.builder.build_int_compare(IntPredicate::SLT, l, r, "lt").unwrap();
                        self.builder.build_int_z_extend(cmp, self.context.i64_type(), "lt_ext").unwrap()
                    }
                    ">" => {
                        let cmp = self.builder.build_int_compare(IntPredicate::SGT, l, r, "gt").unwrap();
                        self.builder.build_int_z_extend(cmp, self.context.i64_type(), "gt_ext").unwrap()
                    }
                    "<=" => {
                        let cmp = self.builder.build_int_compare(IntPredicate::SLE, l, r, "le").unwrap();
                        self.builder.build_int_z_extend(cmp, self.context.i64_type(), "le_ext").unwrap()
                    }
                    ">=" => {
                        let cmp = self.builder.build_int_compare(IntPredicate::SGE, l, r, "ge").unwrap();
                        self.builder.build_int_z_extend(cmp, self.context.i64_type(), "ge_ext").unwrap()
                    }
                    "&&" => {
                        let zero = self.context.i64_type().const_int(0, false);
                        let l_bool = self.builder.build_int_compare(IntPredicate::NE, l, zero, "l_bool").unwrap();
                        let r_bool = self.builder.build_int_compare(IntPredicate::NE, r, zero, "r_bool").unwrap();
                        let and_result = self.builder.build_and(l_bool, r_bool, "and").unwrap();
                        self.builder.build_int_z_extend(and_result, self.context.i64_type(), "and_ext").unwrap()
                    }
                    "||" => {
                        let zero = self.context.i64_type().const_int(0, false);
                        let l_bool = self.builder.build_int_compare(IntPredicate::NE, l, zero, "l_bool").unwrap();
                        let r_bool = self.builder.build_int_compare(IntPredicate::NE, r, zero, "r_bool").unwrap();
                        let or_result = self.builder.build_or(l_bool, r_bool, "or").unwrap();
                        self.builder.build_int_z_extend(or_result, self.context.i64_type(), "or_ext").unwrap()
                    }
                    _ => return Err(format!("❌ Unsupported operator: {}", operator)),
                };
                Ok(result.into())
            }
            _ => Err("❌ Binary operations support only numbers".to_string()),
        }
    }

    fn create_string_literal(&mut self, text: &str) -> PointerValue<'ctx> {
        if let Some(existing) = self.string_literals.get(text) {
            return *existing;
        }

        let i8_type = self.context.i8_type();
        let string_type = i8_type.array_type(text.len() as u32 + 1);
        let string_global = self.module.add_global(string_type, Some(AddressSpace::default()), "str");

        let string_value = self.context.const_string(text.as_bytes(), true);
        string_global.set_initializer(&string_value);
        string_global.set_constant(true);

        let zero = self.context.i32_type().const_int(0, false);
        let string_ptr = unsafe {
            self.builder.build_gep(
                string_type,
                string_global.as_pointer_value(),
                &[zero, zero],
                "str_ptr"
            ).unwrap()
        };

        self.string_literals.insert(text.to_string(), string_ptr);
        string_ptr
    }

    pub fn write_object_file(&self, output_path: &str) -> Result<(), String> {
        Target::initialize_all(&InitializationConfig::default());

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|e| format!("❌ Failed to create target: {}", e))?;

        let target_machine = target
            .create_target_machine(
                &triple,
                "x86-64",
                "+sse4.2,+avx,+aes",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Small,
            )
            .ok_or("❌ Cant create target machine")?;

        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(output_path))
            .map_err(|e| format!("❌ Failed to write object file: {}", e))?;

        Ok(())
    }
}