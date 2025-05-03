use crate::parser::{Expr, Literal, Stmt};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
}

pub struct Runtime {
    functions: HashMap<String, Vec<Stmt>>,
    variables: HashMap<String, Value>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn register_function(&mut self, name: String, body: Vec<Stmt>) {
        self.functions.insert(name, body);
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    pub fn evaluate_expr(&self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(Literal::String(s)) => Ok(Value::String(s.clone())),
            Expr::Literal(Literal::Number(n)) => Ok(Value::Number(*n)),
            Expr::Literal(Literal::Boolean(b)) => Ok(Value::Boolean(*b)),
            Expr::Variable(name) => {
                self.get_variable(name)
                    .cloned()
                    .ok_or_else(|| format!("Variable {} not found", name))
            }
            Expr::BinaryOp(left, op, right) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => match op.as_str() {
                        "+" => Ok(Value::Number(l + r)),
                        "*" => Ok(Value::Number(l * r)),
                        "/" => {
                            if r == 0.0 {
                                Err("Division by zero".to_string())
                            } else {
                                Ok(Value::Number(l / r))
                            }
                        }
                        _ => Err(format!("Unknown operator: {}", op)),
                    },
                    _ => Err("Binary operations only supported for numbers".to_string()),
                }
            }
            Expr::Comparison(left, op, right) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => {
                        let result = match op.as_str() {
                            "==" => l.partial_cmp(&r) == Some(std::cmp::Ordering::Equal),
                            "<" => l.partial_cmp(&r) == Some(std::cmp::Ordering::Less),
                            ">" => l.partial_cmp(&r) == Some(std::cmp::Ordering::Greater),
                            _ => return Err(format!("Unknown comparison operator: {}", op)),
                        };
                        Ok(Value::Boolean(result))
                    }
                    _ => Err("Comparison operations only supported for numbers".to_string()),
                }
            }
            Expr::FunctionCall(name, args) => {
                if name == "println" {
                    if let Some(expr) = args.get(0) {
                        let value = self.evaluate_expr(expr)?;
                        match value {
                            Value::String(s) => Ok(Value::String(s)),
                            Value::Number(n) => {
                                let temp = format!("{:.10}", n);
                                let formatted = temp.trim_end_matches('0').trim_end_matches('.');
                                Ok(Value::String(formatted.parse().unwrap()))
                            }
                            Value::Boolean(b) => Ok(Value::String(b.to_string())),
                        }
                    } else {
                        Err("No argument provided for println".to_string())
                    }
                } else {
                    Err("Function calls not supported in expressions".to_string())
                }
            }
        }
    }

    pub fn call_function(&mut self, name: &str, args: &[Expr]) -> Result<(), String> {
        if name == "println" {
            if let Some(expr) = args.get(0) {
                let value = self.evaluate_expr(expr)?;
                match value {
                    Value::String(s) => println!("{}", s),
                    Value::Number(n) => {
                        let temp = format!("{:.10}", n);
                        let formatted = temp.trim_end_matches('0').trim_end_matches('.');
                        println!("{}", formatted);
                    }
                    Value::Boolean(b) => println!("{}", b),
                }
                Ok(())
            } else {
                Err("No argument provided for system.console.println".to_string())
            }
        } else {
            let body = self
                .functions
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Invalid function: {}", name))?;
            for stmt in body {
                match stmt {
                    Stmt::Expr(Expr::FunctionCall(func_name, func_args)) => {
                        self.call_function(&func_name, &func_args)?;
                    }
                    Stmt::VariableDecl(_type_name, var_name, expr) => {
                        let value = self.evaluate_expr(&expr)?;
                        self.set_variable(var_name, value);
                    }
                    Stmt::If(condition, body, else_branch) => {
                        self.execute_if(&condition, &*body, &else_branch)?;
                    }
                    _ => {}
                }
            }
            Ok(())
        }
    }

    pub(crate) fn execute_if(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<(), String> {
        let condition_value = self.evaluate_expr(condition)?;
        match condition_value {
            Value::Boolean(true) => {
                for stmt in body {
                    match stmt {
                        Stmt::Expr(Expr::FunctionCall(name, args)) => {
                            self.call_function(name, args)?;
                        }
                        Stmt::VariableDecl(_type_name, var_name, expr) => {
                            let value = self.evaluate_expr(expr)?;
                            self.set_variable(var_name.clone(), value);
                        }
                        Stmt::If(condition, body, else_branch) => {
                            self.execute_if(condition, body, else_branch)?;
                        }
                        _ => {}
                    }
                }
            }
            Value::Boolean(false) => {
                if let Some(else_stmt) = else_branch {
                    match &**else_stmt {
                        Stmt::Block(stmts) => {
                            for stmt in stmts {
                                match stmt {
                                    Stmt::Expr(Expr::FunctionCall(name, args)) => {
                                        self.call_function(name, args)?;
                                    }
                                    Stmt::VariableDecl(_type_name, var_name, expr) => {
                                        let value = self.evaluate_expr(expr)?;
                                        self.set_variable(var_name.clone(), value);
                                    }
                                    Stmt::If(condition, body, else_branch) => {
                                        self.execute_if(condition, body, else_branch)?;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => return Err("Invalid else branch".to_string()),
                    }
                }
            }
            _ => return Err("If condition must evaluate to a boolean".to_string()),
        }
        Ok(())
    }
}