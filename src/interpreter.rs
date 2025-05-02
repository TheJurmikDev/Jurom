use crate::parser::{Expr, Stmt};
use crate::runtime::{Runtime, Value};

pub fn execute(stmts: Vec<Stmt>) -> Result<(), String> {
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
            _ => {}
        }
    }

    if !main_body.is_empty() {
        for stmt in &main_body {
            match stmt {
                Stmt::Expr(Expr::FunctionCall(name, args)) => {
                    runtime.call_function(name, args)?;
                }
                Stmt::VariableDecl(_type_name, var_name, expr) => {
                    let value = runtime.evaluate_expr(expr)?;
                    runtime.set_variable(var_name.clone(), value);
                }
                Stmt::If(condition, body, else_branch) => {
                    let condition_value = runtime.evaluate_expr(condition)?;
                    match condition_value {
                        Value::Boolean(true) => {
                            for stmt in body {
                                match stmt {
                                    Stmt::Expr(Expr::FunctionCall(name, args)) => {
                                        runtime.call_function(name, args)?;
                                    }
                                    Stmt::VariableDecl(_type_name, var_name, expr) => {
                                        let value = runtime.evaluate_expr(expr)?;
                                        runtime.set_variable(var_name.clone(), value);
                                    }
                                    Stmt::If(_, _, _) => {
                                        execute(vec![stmt.clone()])?;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        Value::Boolean(false) => {
                            if let Some(else_stmt) = else_branch {
                                match &**else_stmt {
                                    Stmt::If(cond, body, else_br) => {
                                        execute(vec![Stmt::If(cond.clone(), body.clone(), else_br.clone())])?;
                                    }
                                    _ => {
                                        execute(vec![(**else_stmt).clone()])?;
                                    }
                                }
                            }
                        }
                        _ => return Err("If condition must evaluate to a boolean".to_string()),
                    }
                }
                _ => {}
            }
        }
    } else {
        return Err("No main function found".to_string());
    }

    Ok(())
}