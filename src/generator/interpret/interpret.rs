use std::collections::HashMap;
use crate::{Expression, Statement};

pub struct Interpreter {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Vec<Statement>>,
    global_variables: HashMap<String, Value>,
    function_call_depth: usize,
}

#[derive(Debug, Clone)]
enum Value {
    Number(i64),
    String(String),
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            functions: HashMap::new(),
            global_variables: HashMap::new(),
            function_call_depth: 0,
        }
    }

    pub fn execute(&mut self, statements: Vec<Statement>) {
        for statement in &statements {
            if let Statement::ClassDeclaration { methods, .. } = statement {
                for method in methods {
                    if let Statement::FunctionDeclaration { name, body } = method {
                        self.functions.insert(name.clone(), body.clone());
                    }
                }
            } else if let Statement::FunctionDeclaration { name, body } = statement {
                self.functions.insert(name.clone(), body.clone());
            }
        }
        
        if let Some(main_body) = self.functions.get("main").cloned() {
            for stmt in main_body {
                self.execute_statement(stmt);
            }
        } else {
            for statement in statements {
                match statement {
                    Statement::ClassDeclaration { .. } | Statement::FunctionDeclaration { .. } => {}
                    _ => {
                        self.execute_statement(statement);
                    }
                }
            }
        }
    }

    fn execute_statement(&mut self, statement: Statement) {
        match statement {
            Statement::ClassDeclaration { .. } => {}
            Statement::FunctionDeclaration { .. } => {}
            Statement::VariableDeclaration { name, value } => {
                let val = self.evaluate_expression(value);
                self.variables.insert(name.clone(), val.clone());
                
                if self.function_call_depth == 0 {
                    self.global_variables.insert(name, val);
                }
            }
            Statement::Assignment { name, value } => {
                let val = self.evaluate_expression(value);
                
                if self.function_call_depth > 0 && self.variables.contains_key(&name) {
                    self.variables.insert(name, val);
                }
                else if self.function_call_depth > 0 && self.global_variables.contains_key(&name) {
                    self.global_variables.insert(name, val);
                }
                else {
                    self.variables.insert(name.clone(), val.clone());
                    if self.function_call_depth == 0 {
                        self.global_variables.insert(name, val);
                    }
                }
            }
            Statement::MethodCall { object: _object, method, args } => {
                if method == "println" {
                    for arg in args {
                        let val = self.evaluate_expression(arg);
                        match val {
                            Value::Number(n) => println!("{}", n),
                            Value::String(s) => println!("{}", s),
                        }
                    }
                } else {
                    if let Some(function_body) = self.functions.get(&method).cloned() {
                        let saved_variables = self.variables.clone();
                        
                        self.function_call_depth += 1;
                        
                        for (name, value) in &self.global_variables {
                            self.variables.insert(name.clone(), value.clone());
                        }
                        
                        for stmt in function_body {
                            self.execute_statement(stmt);
                        }
                        
                        self.function_call_depth -= 1;
                        
                        self.variables = saved_variables;
                    }
                }
            }
            Statement::IfStatement { condition, then_body, else_body } => {
                let cond_val = self.evaluate_expression(condition);
                let should_execute = match cond_val {
                    Value::Number(n) => n != 0,
                    Value::String(s) => !s.is_empty(),
                };

                if should_execute {
                    for stmt in then_body {
                        self.execute_statement(stmt);
                    }
                } else if let Some(else_statements) = else_body {
                    for stmt in else_statements {
                        self.execute_statement(stmt);
                    }
                }
            }
            Statement::WhileStatement { condition, body } => {
                loop {
                    let cond_val = self.evaluate_expression(condition.clone());
                    let should_continue = match cond_val {
                        Value::Number(n) => n != 0,
                        Value::String(s) => !s.is_empty(),
                    };

                    if !should_continue {
                        break;
                    }

                    for stmt in &body {
                        self.execute_statement(stmt.clone());
                    }
                }
            }
        }
    }

    fn evaluate_expression(&self, expression: Expression) -> Value {
        match expression {
            Expression::Number(n) => Value::Number(n),
            Expression::String(s) => Value::String(s),
            Expression::Variable(name) => {
                self.variables.get(&name)
                    .or_else(|| self.global_variables.get(&name))
                    .cloned()
                    .unwrap_or(Value::Number(0))
            }
            Expression::BinaryOp { left, operator, right } => {
                let left_val = self.evaluate_expression(*left);
                let right_val = self.evaluate_expression(*right);

                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => {
                        let result = match operator.as_str() {
                            "+" => l + r,
                            "-" => l - r,
                            "*" => l * r,
                            "/" => if r != 0 { l / r } else { 0 },
                            "==" => if l == r { 1 } else { 0 },
                            "!=" => if l != r { 1 } else { 0 },
                            "<" => if l < r { 1 } else { 0 },
                            ">" => if l > r { 1 } else { 0 },
                            "<=" => if l <= r { 1 } else { 0 },
                            ">=" => if l >= r { 1 } else { 0 },
                            "&&" => if l != 0 && r != 0 { 1 } else { 0 },
                            "||" => if l != 0 || r != 0 { 1 } else { 0 },
                            _ => 0,
                        };
                        Value::Number(result)
                    }
                    (Value::String(l), Value::String(r)) => {
                        let result = match operator.as_str() {
                            "==" => if l == r { 1 } else { 0 },
                            "!=" => if l != r { 1 } else { 0 },
                            "+" => return Value::String(format!("{}{}", l, r)),
                            _ => 0,
                        };
                        Value::Number(result)
                    }
                    (Value::String(_), Value::Number(_)) |
                    (Value::Number(_), Value::String(_)) => {
                        let result = match operator.as_str() {
                            "==" => 0,
                            "!=" => 1,
                            _ => 0,
                        };
                        Value::Number(result)
                    }
                }
            }
        }
    }
}