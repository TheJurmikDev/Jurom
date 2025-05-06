use crate::parser::{Expr, Literal, Stmt, ParseError};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Number(Number),
    Boolean(bool),
}

pub struct Runtime {
    functions: HashMap<String, Vec<Stmt>>,
    variable_scopes: Vec<HashMap<String, Value>>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            functions: HashMap::new(),
            variable_scopes: vec![HashMap::new()],
        }
    }

    pub fn register_function(&mut self, name: String, body: Vec<Stmt>) {
        self.functions.insert(name, body);
    }

    pub fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.variable_scopes.len() > 1 {
            self.variable_scopes.pop();
        }
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        self.variable_scopes.last_mut().unwrap().insert(name, value);
    }

    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        for scope in self.variable_scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn execute_while(&mut self, condition: &Expr, body: &[Stmt]) -> Result<(), ParseError> {
        while let Value::Boolean(true) = self.evaluate_expr(condition)? {
            self.push_scope();
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
                        if self.get_variable(var_name).is_none() {
                            return Err(ParseError::new(
                                format!("Variable {} not found", var_name),
                                condition.get_line(),
                                condition.get_column(),
                            ));
                        }
                        let value = self.evaluate_expr(expr)?;
                        for scope in self.variable_scopes.iter_mut().rev() {
                            if scope.contains_key(var_name) {
                                scope.insert(var_name.clone(), value);
                                break;
                            }
                        }
                    }
                    Stmt::If { condition, body, else_if, else_branch } => {
                        self.execute_if(condition, body, else_if, else_branch)?;
                    }
                    Stmt::While { condition, body } => {
                        self.execute_while(condition, body)?;
                    }
                    _ => {}
                }
            }
            self.pop_scope();
        }
        Ok(())
    }

    pub fn evaluate_expr(&self, expr: &Expr) -> Result<Value, ParseError> {
        match expr {
            Expr::Literal(Literal::String(s), _line, _column) => Ok(Value::String(s.clone())),
            Expr::Literal(Literal::Number(n), _line, _column) => {
                if n.fract() == 0.0 && *n >= (i64::MIN as f64) && *n <= (i64::MAX as f64) {
                    Ok(Value::Number(Number::Integer(*n as i64)))
                } else {
                    Ok(Value::Number(Number::Float(*n)))
                }
            }
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
                    (Value::Number(left_num), Value::Number(right_num)) => match (left_num, right_num) {
                        (Number::Integer(l), Number::Integer(r)) => match op.as_str() {
                            "+" => Ok(Value::Number(Number::Integer(l + r))),
                            "*" => Ok(Value::Number(Number::Integer(l * r))),
                            "/" => {
                                if r == 0 {
                                    Err(ParseError::new("Cannot divide by zero".to_string(), *line, *column))
                                } else {
                                    let result = l / r;
                                    if result >= i64::MIN && result <= i64::MAX && result % 1 == 0 {
                                        Ok(Value::Number(Number::Integer(result)))
                                    } else {
                                        Ok(Value::Number(Number::Float(l as f64 / r as f64)))
                                    }
                                }
                            }
                            _ => Err(ParseError::new(format!("Unknown operator: {}", op), *line, *column)),
                        },
                        (Number::Float(l), Number::Float(r)) => match op.as_str() {
                            "+" => Ok(Value::Number(Number::Float(l + r))),
                            "*" => Ok(Value::Number(Number::Float(l * r))),
                            "/" => {
                                if r == 0.0 {
                                    Err(ParseError::new("Cannot divide by zero".to_string(), *line, *column))
                                } else {
                                    Ok(Value::Number(Number::Float(l / r)))
                                }
                            }
                            _ => Err(ParseError::new(format!("Unknown operator: {}", op), *line, *column)),
                        },
                        (Number::Integer(l), Number::Float(r)) => match op.as_str() {
                            "+" => Ok(Value::Number(Number::Float(l as f64 + r))),
                            "*" => Ok(Value::Number(Number::Float(l as f64 * r))),
                            "/" => {
                                if r == 0.0 {
                                    Err(ParseError::new("Cannot divide by zero".to_string(), *line, *column))
                                } else {
                                    Ok(Value::Number(Number::Float(l as f64 / r)))
                                }
                            }
                            _ => Err(ParseError::new(format!("Unknown operator: {}", op), *line, *column)),
                        },
                        (Number::Float(l), Number::Integer(r)) => match op.as_str() {
                            "+" => Ok(Value::Number(Number::Float(l + r as f64))),
                            "*" => Ok(Value::Number(Number::Float(l * r as f64))),
                            "/" => {
                                if r == 0 {
                                    Err(ParseError::new("Cannot divide by zero".to_string(), *line, *column))
                                } else {
                                    Ok(Value::Number(Number::Float(l / r as f64)))
                                }
                            }
                            _ => Err(ParseError::new(format!("Unknown operator: {}", op), *line, *column)),
                        },
                    },
                    _ => Err(ParseError::new("Binary operations only supported for numbers".to_string(), *line, *column)),
                }
            }
            Expr::Comparison(left, op, right, line, column) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match (left_val, right_val) {
                    (Value::Number(left_num), Value::Number(right_num)) => {
                        let result = match (left_num, right_num) {
                            (Number::Integer(l), Number::Integer(r)) => match op.as_str() {
                                "==" => l == r,
                                "!=" => l != r,
                                "<" => l < r,
                                ">" => l > r,
                                "<=" => l <= r,
                                ">=" => l >= r,
                                _ => {
                                    return Err(ParseError::new(
                                        format!("Unknown comparison operator: {}", op),
                                        *line,
                                        *column,
                                    ))
                                }
                            },
                            (Number::Float(l), Number::Float(r)) => {
                                const EPSILON: f64 = 1e-10;
                                match op.as_str() {
                                    "==" => (l - r).abs() < EPSILON,
                                    "!=" => (l - r).abs() >= EPSILON,
                                    "<" => l < r - EPSILON,
                                    ">" => l > r + EPSILON,
                                    "<=" => l <= r + EPSILON,
                                    ">=" => l >= r - EPSILON,
                                    _ => {
                                        return Err(ParseError::new(
                                            format!("Unknown comparison operator: {}", op),
                                            *line,
                                            *column,
                                        ))
                                    }
                                }
                            }
                            (Number::Integer(l), Number::Float(r)) => {
                                const EPSILON: f64 = 1e-10;
                                match op.as_str() {
                                    "==" => ((l as f64) - r).abs() < EPSILON,
                                    "!=" => ((l as f64) - r).abs() >= EPSILON,
                                    "<" => (l as f64) < r - EPSILON,
                                    ">" => (l as f64) > r + EPSILON,
                                    "<=" => (l as f64) <= r + EPSILON,
                                    ">=" => (l as f64) >= r - EPSILON,
                                    _ => {
                                        return Err(ParseError::new(
                                            format!("Unknown comparison operator: {}", op),
                                            *line,
                                            *column,
                                        ))
                                    }
                                }
                            }
                            (Number::Float(l), Number::Integer(r)) => {
                                const EPSILON: f64 = 1e-10;
                                match op.as_str() {
                                    "==" => (l - (r as f64)).abs() < EPSILON,
                                    "!=" => (l - (r as f64)).abs() >= EPSILON,
                                    "<" => l < (r as f64) - EPSILON,
                                    ">" => l > (r as f64) + EPSILON,
                                    "<=" => l <= (r as f64) + EPSILON,
                                    ">=" => l >= (r as f64) - EPSILON,
                                    _ => {
                                        return Err(ParseError::new(
                                            format!("Unknown comparison operator: {}", op),
                                            *line,
                                            *column,
                                        ))
                                    }
                                }
                            }
                        };
                        Ok(Value::Boolean(result))
                    }
                    _ => Err(ParseError::new(
                        "Comparison operations only supported for numbers".to_string(),
                        *line,
                        *column,
                    )),
                }
            }
            Expr::FunctionCall { name, args, line, column } => {
                if name == "println" {
                    if let Some(expr) = args.get(0) {
                        let value = self.evaluate_expr(expr)?;
                        match value {
                            Value::String(s) => Ok(Value::String(s)),
                            Value::Number(Number::Integer(n)) => Ok(Value::String(n.to_string())),
                            Value::Number(Number::Float(n)) => {
                                let temp = format!("{:.10}", n);
                                let formatted = temp.trim_end_matches('0').trim_end_matches('.');
                                Ok(Value::String(formatted.to_string()))
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
                    Value::Number(Number::Integer(n)) => println!("{}", n),
                    Value::Number(Number::Float(n)) => {
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
            self.push_scope();
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
                        for scope in self.variable_scopes.iter_mut().rev() {
                            if scope.contains_key(&var_name) {
                                scope.insert(var_name.clone(), value);
                                break;
                            }
                        }
                    }
                    Stmt::If {
                        condition,
                        body,
                        else_if,
                        else_branch,
                    } => {
                        self.execute_if(&condition, &body, &else_if, &else_branch)?;
                    }
                    Stmt::While { condition, body } => {
                        self.execute_while(&condition, &*body)?;
                    }
                    _ => {}
                }
            }
            self.pop_scope();
            Ok(())
        }
    }

    pub fn execute_if(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_if_branches: &[(Expr, Vec<Stmt>)],
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<(), ParseError> {
        let condition_value = self.evaluate_expr(condition)?;
        if let Value::Boolean(true) = condition_value {
            self.push_scope();
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
                        if self.get_variable(var_name).is_none() {
                            return Err(ParseError::new(
                                format!("Variable {} not found", var_name),
                                condition.get_line(),
                                condition.get_column(),
                            ));
                        }
                        let value = self.evaluate_expr(expr)?;
                        for scope in self.variable_scopes.iter_mut().rev() {
                            if scope.contains_key(var_name) {
                                scope.insert(var_name.clone(), value);
                                break;
                            }
                        }
                    }
                    Stmt::If {
                        condition,
                        body,
                        else_if,
                        else_branch,
                    } => {
                        self.execute_if(condition, body, else_if, else_branch)?;
                    }
                    Stmt::While { condition, body } => {
                        self.execute_while(condition, body)?;
                    }
                    _ => {}
                }
            }
            self.pop_scope();
        } else {
            let mut executed = false;
            for (else_if_condition, else_if_body) in else_if_branches {
                let else_if_value = self.evaluate_expr(else_if_condition)?;
                if let Value::Boolean(true) = else_if_value {
                    self.push_scope();
                    for stmt in else_if_body {
                        match stmt {
                            Stmt::Expr(Expr::FunctionCall { name, args, line, column }) => {
                                self.call_function(name, args, *line, *column)?;
                            }
                            Stmt::VariableDecl(_type_name, var_name, expr) => {
                                let value = self.evaluate_expr(expr)?;
                                self.set_variable(var_name.clone(), value);
                            }
                            Stmt::Assignment(var_name, expr) => {
                                if self.get_variable(var_name).is_none() {
                                    return Err(ParseError::new(
                                        format!("Variable {} not found", var_name),
                                        else_if_condition.get_line(),
                                        else_if_condition.get_column(),
                                    ));
                                }
                                let value = self.evaluate_expr(expr)?;
                                for scope in self.variable_scopes.iter_mut().rev() {
                                    if scope.contains_key(var_name) {
                                        scope.insert(var_name.clone(), value);
                                        break;
                                    }
                                }
                            }
                            Stmt::If {
                                condition,
                                body,
                                else_if,
                                else_branch,
                            } => {
                                self.execute_if(condition, body, else_if, else_branch)?;
                            }
                            _ => {}
                        }
                    }
                    self.pop_scope();
                    executed = true;
                    break;
                }
            }
            if !executed {
                if let Some(else_stmt) = else_branch {
                    self.push_scope();
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
                                        if self.get_variable(var_name).is_none() {
                                            return Err(ParseError::new(
                                                format!("Variable {} not found", var_name),
                                                condition.get_line(),
                                                condition.get_column(),
                                            ));
                                        }
                                        let value = self.evaluate_expr(expr)?;
                                        for scope in self.variable_scopes.iter_mut().rev() {
                                            if scope.contains_key(var_name) {
                                                scope.insert(var_name.clone(), value);
                                                break;
                                            }
                                        }
                                    }
                                    Stmt::If {
                                        condition,
                                        body,
                                        else_if,
                                        else_branch,
                                    } => {
                                        self.execute_if(condition, body, else_if, else_branch)?;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => return Err(ParseError::new("Invalid else branch".to_string(), condition.get_line(), condition.get_column())),
                    }
                    self.pop_scope();
                }
            }
        }
        Ok(())
    }
}

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