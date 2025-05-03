use crate::parser::{Expr, Stmt, ParseError};
use crate::runtime::Runtime;

pub fn execute(stmts: Vec<Stmt>) -> Result<(), ParseError> {
    let mut runtime = Runtime::new();
    let mut main_body = Vec::new();

    for stmt in &stmts {
        match stmt {
            Stmt::ClassDecl(_, body) => {
                for class_stmt in body {
                    match class_stmt {
                        Stmt::FunctionDecl(func_name, func_body) => {
                            runtime.register_function(func_name.clone(), func_body.clone());
                            if func_name == "main" {
                                main_body = func_body.clone();
                            }
                        }
                        Stmt::VariableDecl(_type_name, var_name, expr) => {
                            let value = runtime.evaluate_expr(expr)?;
                            runtime.set_variable(var_name.clone(), value);
                        }
                        Stmt::Assignment(var_name, expr) => {
                            if runtime.get_variable(&var_name).is_none() {
                                return Err(ParseError::new(
                                    format!("Variable {} not found", var_name),
                                    expr.get_line(),
                                    expr.get_column(),
                                ));
                            }
                            let value = runtime.evaluate_expr(expr)?;
                            runtime.set_variable(var_name.clone(), value);
                        }
                        _ => {}
                    }
                }
            }
            Stmt::FunctionDecl(func_name, func_body) => {
                runtime.register_function(func_name.clone(), func_body.clone());
                if func_name == "main" {
                    main_body = func_body.clone();
                }
            }
            Stmt::VariableDecl(_type_name, var_name, expr) => {
                let value = runtime.evaluate_expr(expr)?;
                runtime.set_variable(var_name.clone(), value);
            }
            Stmt::Assignment(var_name, expr) => {
                if runtime.get_variable(&var_name).is_none() {
                    return Err(ParseError::new(
                        format!("Variable {} not found", var_name),
                        expr.get_line(),
                        expr.get_column(),
                    ));
                }
                let value = runtime.evaluate_expr(expr)?;
                runtime.set_variable(var_name.clone(), value);
            }
            _ => {}
        }
    }

    if !main_body.is_empty() {
        for stmt in &main_body {
            match stmt {
                Stmt::Expr(Expr::FunctionCall { name, args, line, column }) => {
                    runtime.call_function(name, args, *line, *column)?;
                }
                Stmt::VariableDecl(_type_name, var_name, expr) => {
                    let value = runtime.evaluate_expr(expr)?;
                    runtime.set_variable(var_name.clone(), value);
                }
                Stmt::Assignment(var_name, expr) => {
                    if runtime.get_variable(&var_name).is_none() {
                        return Err(ParseError::new(
                            format!("Variable {} not found", var_name),
                            expr.get_line(),
                            expr.get_column(),
                        ));
                    }
                    let value = runtime.evaluate_expr(expr)?;
                    runtime.set_variable(var_name.clone(), value);
                }
                Stmt::If(condition, body, else_branch) => {
                    runtime.execute_if(condition, body, else_branch)?;
                }
                _ => {}
            }
        }
    } else {
        return Err(ParseError::new("No main function found".to_string(), 0, 0));
    }

    Ok(())
}