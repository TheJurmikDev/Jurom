use crate::parser::{Expr, Literal, Stmt};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Array(Vec<Value>),
    Null,
}

pub struct Runtime {
    functions: HashMap<String, (Vec<String>, Vec<Stmt>)>,
    variables: HashMap<String, Value>,
    expr_cache: HashMap<Expr, Value>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            functions: HashMap::new(),
            variables: HashMap::new(),
            expr_cache: HashMap::new(),
        }
    }

    pub fn register_function(&mut self, name: String, params: Vec<String>, body: Vec<Stmt>) {
        self.functions.insert(name, (params, body));
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    pub fn check_type(&mut self, expr: &Expr, expected_type: &str) -> Result<(), String> {
        let value = self.evaluate_expr(expr)?;
        match (expected_type, &value) {
            ("num", Value::Number(_)) => Ok(()),
            ("string", Value::String(_)) => Ok(()),
            ("boolean", Value::Boolean(_)) => Ok(()),
            ("array", Value::Array(_)) => Ok(()),
            _ => Err(format!(
                "Type mismatch: expected {}, got {}",
                expected_type,
                match value {
                    Value::String(_) => "string",
                    Value::Number(_) => "number",
                    Value::Boolean(_) => "boolean",
                    Value::Array(_) => "array",
                    Value::Null => "null",
                }
            )),
        }
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        if let Some(cached) = self.expr_cache.get(expr) {
            return Ok(cached.clone());
        }

        let result = match expr {
            Expr::Literal(Literal::String(s)) => Value::String(s.clone()),
            Expr::Literal(Literal::Number(n)) => Value::Number(*n),
            Expr::Literal(Literal::Boolean(b)) => Value::Boolean(*b),
            Expr::Variable(name) => self
                .get_variable(name)
                .cloned()
                .ok_or_else(|| format!("Variable {} not found", name))?,
            Expr::BinaryOp(left, op, right) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => match op.as_str() {
                        "+" => Value::Number(l + r),
                        "-" => Value::Number(l - r),
                        "*" => Value::Number(l * r),
                        "/" => {
                            if r == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Value::Number(l / r)
                        }
                        _ => return Err(format!("Unknown operator: {}", op)),
                    },
                    _ => return Err("Binary operations only supported for numbers".to_string()),
                }
            }
            Expr::Comparison(left, op, right) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => {
                        const EPSILON: f64 = 1e-10;
                        match op.as_str() {
                            "==" => Value::Boolean((l - r).abs() < EPSILON),
                            "!=" => Value::Boolean((l - r).abs() >= EPSILON),
                            "<" => Value::Boolean(l < r),
                            ">" => Value::Boolean(l > r),
                            "<=" => Value::Boolean(l <= r),
                            ">=" => Value::Boolean(l >= r),
                            _ => return Err(format!("Unknown comparison operator: {}", op)),
                        }
                    }
                    _ => return Err("Comparison operations only supported for numbers".to_string()),
                }
            }
            Expr::Array(elements) => {
                let mut array = Vec::new();
                for elem in elements {
                    array.push(self.evaluate_expr(elem)?);
                }
                Value::Array(array)
            }
            Expr::ArrayAccess(array_expr, index_expr) => {
                let array = self.evaluate_expr(array_expr)?;
                let index = self.evaluate_expr(index_expr)?;
                match (array, index) {
                    (Value::Array(arr), Value::Number(idx)) => {
                        let idx = idx as usize;
                        if idx >= arr.len() {
                            return Err(format!(
                                "Index {} out of bounds for array of size {}",
                                idx,
                                arr.len()
                            ));
                        }
                        arr[idx].clone()
                    }
                    _ => return Err("Array access requires array and numeric index".to_string()),
                }
            }
            Expr::FunctionCall(name, args) => {
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.evaluate_expr(arg)?);
                }
                self.call_function(name, &arg_values)?.unwrap_or(Value::Null)
            }
        };

        self.expr_cache.insert(expr.clone(), result.clone());
        Ok(result)
    }

    pub fn call_function(&mut self, name: &str, args: &[Value]) -> Result<Option<Value>, String> {
        if name == "println" {
            if let Some(value) = args.get(0) {
                match value {
                    Value::String(s) => println!("{}", s),
                    Value::Number(n) => {
                        let temp = format!("{:.10}", n);
                        let formatted = temp.trim_end_matches('0').trim_end_matches('.');
                        println!("{}", formatted);
                    }
                    Value::Boolean(b) => println!("{}", b),
                    Value::Array(arr) => println!("{:?}", arr),
                    Value::Null => println!("null"),
                }
                Ok(None)
            } else {
                Err("No argument provided for system.console.println".to_string())
            }
        } else {
            let (params, body) = self
                .functions
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Invalid function: {}", name))?;

            if params.len() != args.len() {
                return Err(format!(
                    "Function {} expects {} arguments, got {}",
                    name,
                    params.len(),
                    args.len()
                ));
            }

            for (param, arg) in params.iter().zip(args.iter()) {
                self.set_variable(param.clone(), arg.clone());
            }

            let mut return_value = None;
            for stmt in body {
                match stmt {
                    Stmt::Expr(Expr::FunctionCall(func_name, func_args)) => {
                        let mut arg_values = Vec::new();
                        for arg in func_args {
                            arg_values.push(self.evaluate_expr(&arg)?);
                        }
                        if let Some(value) = self.call_function(&func_name, &arg_values)? {
                            return_value = Some(value);
                            break;
                        }
                    }
                    Stmt::VariableDecl(type_name, var_name, expr) => {
                        self.check_type(&expr, type_name.as_str())?;
                        let value = self.evaluate_expr(&expr)?;
                        self.set_variable(var_name.clone(), value);
                    }
                    Stmt::Return(expr) => {
                        return_value = expr.as_ref().map(|e| self.evaluate_expr(e)).transpose()?;
                        break;
                    }
                    _ => {}
                }
            }
            Ok(return_value)
        }
    }
}