use crate::parser::{Expr, Literal, Stmt, ParseError};
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

    pub fn evaluate_expr(&self, expr: &Expr) -> Result<Value, ParseError> {
        match expr {
            Expr::Literal(Literal::String(s), _line, _column) => Ok(Value::String(s.clone())),
            Expr::Literal(Literal::Number(n), _line, _column) => Ok(Value::Number(*n)),
            Expr::Literal(Literal::Boolean(b), _line, _column) => Ok(Value::Boolean(*b)),
            Expr::Variable(name, line, column) => {
                self.get_variable(name)
                    .cloned()
                    .ok_or_else(|| ParseError::new(format!("Variable {} not found", name), *line, *column))
            }
            Expr::BinaryOp(left, op, right, line, column) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => match op.as_str() {
                        "+" => Ok(Value::Number(l + r)),
                        "*" => Ok(Value::Number(l * r)),
                        "/" => {
                            if r == 0.0 {
                                Err(ParseError::new("Division by zero".to_string(), *line, *column))
                            } else {
                                Ok(Value::Number(l / r))
                            }
                        }
                        _ => Err(ParseError::new(format!("Unknown operator: {}", op), *line, *column)),
                    },
                    _ => Err(ParseError::new("Binary operations only supported for numbers".to_string(), *line, *column)),
                }
            }
            Expr::Comparison(left, op, right, line, column) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => {
                        let result = match op.as_str() {
                            "==" => l.partial_cmp(&r) == Some(std::cmp::Ordering::Equal),
                            "<" => l.partial_cmp(&r) == Some(std::cmp::Ordering::Less),
                            ">" => l.partial_cmp(&r) == Some(std::cmp::Ordering::Greater),
                            _ => return Err(ParseError::new(format!("Unknown comparison operator: {}", op), *line, *column)),
                        };
                        Ok(Value::Boolean(result))
                    }
                    _ => Err(ParseError::new("Comparison operations only supported for numbers".to_string(), *line, *column)),
                }
            }
            Expr::FunctionCall { name, args, line, column } => {
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
                        Err(ParseError::new("No argument provided for println".to_string(), *line, *column))
                    }
                } else {
                    Err(ParseError::new("Function calls not supported in expressions".to_string(), *line, *column))
                }
            }
        }
    }

    pub fn call_function(&mut self, name: &str, args: &[Expr], line: usize, column: usize) -> Result<(), ParseError> {
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
                Err(ParseError::new("No argument provided for system.console.println".to_string(), line, column))
            }
        } else {
            let body = self
                .functions
                .get(name)
                .cloned()
                .ok_or_else(|| ParseError::new(format!("Invalid function: {}", name), line, column))?;
            for stmt in body {
                match stmt {
                    Stmt::Expr(Expr::FunctionCall { name, args, line, column }) => {
                        self.call_function(&name, &args, line, column)?;
                    }
                    Stmt::VariableDecl(_type_name, var_name, expr) => {
                        let value = self.evaluate_expr(&expr)?;
                        self.set_variable(var_name, value);
                    }
                    Stmt::Assignment(var_name, expr) => {
                        if self.get_variable(&var_name).is_none() {
                            return Err(ParseError::new(
                                format!("Variable {} not found", var_name),
                                line,
                                column,
                            ));
                        }
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
    ) -> Result<(), ParseError> {
        let condition_value = self.evaluate_expr(condition)?;
        match condition_value {
            Value::Boolean(true) => {
                for stmt in body {
                    match stmt {
                        Stmt::Expr(Expr::FunctionCall { name, args, line, column }) => {
                            self.call_function(name, args, *line, *column)?;
                        }
                        Stmt::VariableDecl(_type_name, var_name, expr) => {
                            let value = self.evaluate_expr(expr)?;
                            self.set_variable(var_name.clone(), value);
                        }
                        Stmt::Assignment(var_name, expr) => {
                            if self.get_variable(&var_name).is_none() {
                                return Err(ParseError::new(
                                    format!("Variable {} not found", var_name),
                                    condition.get_line(),
                                    condition.get_column(),
                                ));
                            }
                            let value = self.evaluate_expr(&expr)?;
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
                                    Stmt::Expr(Expr::FunctionCall { name, args, line, column }) => {
                                        self.call_function(name, args, *line, *column)?;
                                    }
                                    Stmt::VariableDecl(_type_name, var_name, expr) => {
                                        let value = self.evaluate_expr(expr)?;
                                        self.set_variable(var_name.clone(), value);
                                    }
                                    Stmt::Assignment(var_name, expr) => {
                                        if self.get_variable(&var_name).is_none() {
                                            return Err(ParseError::new(
                                                format!("Variable {} not found", var_name),
                                                condition.get_line(),
                                                condition.get_column(),
                                            ));
                                        }
                                        let value = self.evaluate_expr(&expr)?;
                                        self.set_variable(var_name.clone(), value);
                                    }
                                    Stmt::If(condition, body, else_branch) => {
                                        self.execute_if(condition, body, else_branch)?;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => return Err(ParseError::new("Invalid else branch".to_string(), 0, 0)),
                    }
                }
            }
            _ => return Err(ParseError::new("If condition must evaluate to a boolean".to_string(), 0, 0)),
        }
        Ok(())
    }
}

// Pomocná metoda pro získání řádku a sloupce z Expr
impl Expr {
    pub fn get_line(&self) -> usize {
        match self {
            Expr::FunctionCall { line, .. } => *line,
            Expr::Literal(_, line, _) => *line,
            Expr::Variable(_, line, _) => *line,
            Expr::BinaryOp(_, _, _, line, _) => *line,
            Expr::Comparison(_, _, _, line, _) => *line,
        }
    }

    pub fn get_column(&self) -> usize {
        match self {
            Expr::FunctionCall { column, .. } => *column,
            Expr::Literal(_, _, column) => *column,
            Expr::Variable(_, _, column) => *column,
            Expr::BinaryOp(_, _, _, _, column) => *column,
            Expr::Comparison(_, _, _, _, column) => *column,
        }
    }
}