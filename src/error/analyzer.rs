use std::collections::{HashMap, HashSet};
use crate::{Expression, Statement};

pub struct Analyzer {
    declared_variables: HashSet<String>,
    functions: HashMap<String, Vec<Statement>>,
    errors: Vec<String>,
}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {
            declared_variables: HashSet::new(),
            functions: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn analyze(&mut self, statements: &[Statement]) -> Result<(), Vec<String>> {
        for statement in statements {
            self.collect_function_declarations(statement);
        }

        if let Some(main_body) = self.functions.get("main").cloned() {
            for stmt in &main_body {
                self.analyze_statement(stmt);
            }
        } else {
            for statement in statements {
                match statement {
                    Statement::ClassDeclaration { .. } | Statement::FunctionDeclaration { .. } => {}
                    _ => {
                        self.analyze_statement(statement);
                    }
                }
            }
        }
        
        let functions_to_analyze: Vec<(String, Vec<Statement>)> = self.functions
            .iter()
            .filter(|(name, _)| *name != "main")
            .map(|(name, body)| (name.clone(), body.clone()))
            .collect();

        for (_func_name, body) in functions_to_analyze {
            let saved_vars = self.declared_variables.clone();

            for stmt in &body {
                self.analyze_statement(stmt);
            }

            self.declared_variables = saved_vars;
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn collect_function_declarations(&mut self, statement: &Statement) {
        match statement {
            Statement::ClassDeclaration { methods, .. } => {
                for method in methods {
                    if let Statement::FunctionDeclaration { name, body } = method {
                        self.functions.insert(name.clone(), body.clone());
                    }
                }
            }
            Statement::FunctionDeclaration { name, body } => {
                self.functions.insert(name.clone(), body.clone());
            }
            _ => {}
        }
    }

    fn analyze_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::VariableDeclaration { name, value } => {
                self.analyze_expression(value);
                self.declared_variables.insert(name.clone());
            }
            Statement::Assignment { name, value } => {
                self.analyze_expression(value);
                if !self.declared_variables.contains(name) {
                    self.declared_variables.insert(name.clone());
                }
            }
            Statement::MethodCall { method, args, .. } => {
                for arg in args {
                    self.analyze_expression(arg);
                }

                if !self.functions.contains_key(method) && method != "println" {}
            }
            Statement::IfStatement { condition, then_body, else_body } => {
                self.analyze_expression(condition);

                for stmt in then_body {
                    self.analyze_statement(stmt);
                }

                if let Some(else_statements) = else_body {
                    for stmt in else_statements {
                        self.analyze_statement(stmt);
                    }
                }
            }
            Statement::WhileStatement { condition, body } => {
                self.analyze_expression(condition);

                for stmt in body {
                    self.analyze_statement(stmt);
                }
            }
            _ => {}
        }
    }

    fn analyze_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Variable(name) => {
                if !self.declared_variables.contains(name) {
                    self.errors.push(format!("Undefined variable: '{}'", name));
                }
            }
            Expression::BinaryOp { left, right, .. } => {
                self.analyze_expression(left);
                self.analyze_expression(right);
            }
            Expression::Number(_) | Expression::String(_) => {}
        }
    }
}