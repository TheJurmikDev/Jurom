use std::collections::HashMap;
use crate::{Expression, Statement};

pub struct CppCodeGenerator {
    variables: HashMap<String, (String, String)>,
    variable_counter: usize,
    functions: HashMap<String, Vec<Statement>>,
}

impl CppCodeGenerator {
    pub fn new() -> Self {
        CppCodeGenerator {
            variables: HashMap::new(),
            variable_counter: 0,
            functions: HashMap::new(),
        }
    }

    pub fn generate(&mut self, statements: Vec<Statement>) -> String {
        let mut code = String::new();
        
        code.push_str("#include <iostream>\n");
        code.push_str("#include <string>\n");
        code.push_str("#include <cstdlib>\n\n");
        code.push_str("using namespace std;\n\n");
        
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
        
        for function_name in self.functions.keys() {
            if function_name != "main" {
                code.push_str(&format!("void {}();\n", function_name));
            }
        }
        code.push_str("\n");
        
        let functions_clone: Vec<(String, Vec<Statement>)> = self.functions
            .iter()
            .filter(|(name, _)| *name != "main")
            .map(|(name, body)| (name.clone(), body.clone()))
            .collect();

        for (function_name, body) in functions_clone {
            let saved_variables = self.variables.clone();
            let saved_counter = self.variable_counter;

            code.push_str(&format!("void {}() {{\n", function_name));
            for stmt in body {
                code.push_str(&self.generate_statement(stmt, 1));
            }
            code.push_str("}\n\n");

            self.variables = saved_variables;
            self.variable_counter = saved_counter;
        }
        
        code.push_str("int main() {\n");

        if let Some(main_body) = self.functions.get("main").cloned() {
            for stmt in main_body {
                code.push_str(&self.generate_statement(stmt, 1));
            }
        } else {
            for statement in statements {
                match statement {
                    Statement::ClassDeclaration { .. } | Statement::FunctionDeclaration { .. } => {}
                    _ => {
                        code.push_str(&self.generate_statement(statement, 1));
                    }
                }
            }
        }

        code.push_str("    return 0;\n");
        code.push_str("}\n");

        code
    }

    fn generate_statement(&mut self, statement: Statement, indent: usize) -> String {
        let indent_str = "    ".repeat(indent);
        let mut code = String::new();

        match statement {
            Statement::ClassDeclaration { .. } => {}
            Statement::FunctionDeclaration { .. } => {}
            Statement::VariableDeclaration { name, value } => {
                let c_var_name = format!("var_{}", self.variable_counter);
                self.variable_counter += 1;

                match &value {
                    Expression::String(_) => {
                        self.variables.insert(name, (c_var_name.clone(), "string".to_string()));
                        code.push_str(&format!("{}string {} = {};\n",
                                               indent_str, c_var_name, self.generate_expression(&value)));
                    }
                    _ => {
                        self.variables.insert(name, (c_var_name.clone(), "number".to_string()));
                        code.push_str(&format!("{}long long {} = {};\n",
                                               indent_str, c_var_name, self.generate_expression(&value)));
                    }
                }
            }
            Statement::Assignment { name, value } => {
                if let Some((c_var_name, var_type)) = self.variables.get(&name).cloned() {
                    match (&value, var_type.as_str()) {
                        (Expression::String(_), "string") => {
                            code.push_str(&format!("{}{} = {};\n",
                                                   indent_str, c_var_name, self.generate_expression(&value)));
                        }
                        (_, "number") => {
                            code.push_str(&format!("{}{} = {};\n",
                                                   indent_str, c_var_name, self.generate_expression(&value)));
                        }
                        _ => {
                            code.push_str(&format!("{}{} = {};\n",
                                                   indent_str, c_var_name, self.generate_expression(&value)));
                        }
                    }
                } else {
                    let c_var_name = format!("var_{}", self.variable_counter);
                    self.variable_counter += 1;

                    match &value {
                        Expression::String(_) => {
                            self.variables.insert(name, (c_var_name.clone(), "string".to_string()));
                            code.push_str(&format!("{}string {} = {};\n",
                                                   indent_str, c_var_name, self.generate_expression(&value)));
                        }
                        _ => {
                            self.variables.insert(name, (c_var_name.clone(), "number".to_string()));
                            code.push_str(&format!("{}long long {} = {};\n",
                                                   indent_str, c_var_name, self.generate_expression(&value)));
                        }
                    }
                }
            }
            Statement::MethodCall { object: _object, method, args } => {
                if method == "println" {
                    for arg in args {
                        match &arg {
                            Expression::String(_) | Expression::Variable(_) => {
                                code.push_str(&format!("{}cout << {} << endl;\n",
                                                       indent_str, self.generate_expression(&arg)));
                            }
                            _ => {
                                code.push_str(&format!("{}cout << {} << endl;\n",
                                                       indent_str, self.generate_expression(&arg)));
                            }
                        }
                    }
                } else {
                    if self.functions.contains_key(&method) {
                        code.push_str(&format!("{}{}();\n", indent_str, method));
                    }
                }
            }
            Statement::IfStatement { condition, then_body, else_body } => {
                code.push_str(&format!("{}if ({}) {{\n",
                                       indent_str, self.generate_condition(&condition)));

                for stmt in then_body {
                    code.push_str(&self.generate_statement(stmt, indent + 1));
                }

                if let Some(else_statements) = else_body {
                    code.push_str(&format!("{}}} else {{\n", indent_str));
                    for stmt in else_statements {
                        code.push_str(&self.generate_statement(stmt, indent + 1));
                    }
                }

                code.push_str(&format!("{}}}\n", indent_str));
            }
            Statement::WhileStatement { condition, body } => {
                code.push_str(&format!("{}while ({}) {{\n",
                                       indent_str, self.generate_condition(&condition)));

                for stmt in body {
                    code.push_str(&self.generate_statement(stmt, indent + 1));
                }

                code.push_str(&format!("{}}}\n", indent_str));
            }
        }

        code
    }

    fn generate_condition(&self, expression: &Expression) -> String {
        match expression {
            Expression::Variable(name) => {
                if let Some((c_var_name, var_type)) = self.variables.get(name) {
                    match var_type.as_str() {
                        "string" => format!("!{}.empty()", c_var_name),
                        _ => format!("({} != 0)", c_var_name),
                    }
                } else {
                    "false".to_string()
                }
            }
            Expression::String(s) => {
                if s.is_empty() {
                    "false".to_string()
                } else {
                    "true".to_string()
                }
            }
            Expression::Number(n) => {
                if *n == 0 {
                    "false".to_string()
                } else {
                    "true".to_string()
                }
            }
            Expression::BinaryOp { left, operator, right } => {
                self.generate_binary_expression(left, operator, right)
            }
        }
    }

    fn generate_expression(&self, expression: &Expression) -> String {
        match expression {
            Expression::Number(n) => n.to_string(),
            Expression::String(s) => format!("\"{}\"", s),
            Expression::Variable(name) => {
                if let Some((c_var_name, _)) = self.variables.get(name) {
                    c_var_name.clone()
                } else {
                    "0".to_string()
                }
            }
            Expression::BinaryOp { left, operator, right } => {
                self.generate_binary_expression(left, operator, right)
            }
        }
    }

    fn generate_binary_expression(&self, left: &Expression, operator: &str, right: &Expression) -> String {
        let left_str = self.generate_expression(left);
        let right_str = self.generate_expression(right);

        match operator {
            "/" => {
                format!("(({}) != 0 ? ({}) / ({}) : 0)", right_str, left_str, right_str)
            }
            "==" => {
                format!("({} == {})", left_str, right_str)
            }
            "!=" => {
                format!("({} != {})", left_str, right_str)
            }
            "+" => {
                if self.is_string_expression(left) || self.is_string_expression(right) {
                    format!("({} + {})", left_str, right_str)
                } else {
                    format!("({} + {})", left_str, right_str)
                }
            }
            "&&" => {
                let left_cond = self.generate_condition(left);
                let right_cond = self.generate_condition(right);
                format!("({} && {})", left_cond, right_cond)
            }
            "||" => {
                let left_cond = self.generate_condition(left);
                let right_cond = self.generate_condition(right);
                format!("({} || {})", left_cond, right_cond)
            }
            _ => {
                format!("({} {} {})", left_str, operator, right_str)
            }
        }
    }

    fn is_string_expression(&self, expression: &Expression) -> bool {
        match expression {
            Expression::String(_) => true,
            Expression::Variable(name) => {
                if let Some((_, var_type)) = self.variables.get(name) {
                    var_type == "string"
                } else {
                    false
                }
            }
            Expression::BinaryOp { left, operator, right } => {
                if operator == "+" {
                    self.is_string_expression(left) || self.is_string_expression(right)
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}