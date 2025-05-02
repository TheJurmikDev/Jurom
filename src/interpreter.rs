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
                        Stmt::FunctionDecl(func_name, params, func_body) => {
                            runtime.register_function(func_name.clone(), params.clone(), func_body.clone());
                            if func_name == "main" {
                                main_body = func_body.clone();
                            }
                        }
                        Stmt::VariableDecl(type_name, var_name, expr) => {
                            runtime.check_type(expr, type_name)?;
                            let value = runtime.evaluate_expr(expr)?;
                            runtime.set_variable(var_name.clone(), value);
                        }
                        _ => {}
                    }
                }
            }
            Stmt::FunctionDecl(func_name, params, func_body) => {
                runtime.register_function(func_name.clone(), params.clone(), func_body.clone());
                if func_name == "main" {
                    main_body = func_body.clone();
                }
            }
            Stmt::VariableDecl(type_name, var_name, expr) => {
                runtime.check_type(expr, type_name)?;
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
                    let mut arg_values = Vec::new();
                    for arg in args {
                        arg_values.push(runtime.evaluate_expr(arg)?);
                    }
                    runtime.call_function(name, &arg_values)?;
                }
                Stmt::VariableDecl(type_name, var_name, expr) => {
                    runtime.check_type(expr, type_name)?;
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
                                        let mut arg_values = Vec::new();
                                        for arg in args {
                                            arg_values.push(runtime.evaluate_expr(arg)?);
                                        }
                                        runtime.call_function(name, &arg_values)?;
                                    }
                                    Stmt::VariableDecl(type_name, var_name, expr) => {
                                        runtime.check_type(expr, type_name)?;
                                        let value = runtime.evaluate_expr(expr)?;
                                        runtime.set_variable(var_name.clone(), value);
                                    }
                                    Stmt::If(_, _, _) | Stmt::While(_, _) | Stmt::For(_, _, _) => {
                                        execute(vec![stmt.clone()])?;
                                    }
                                    Stmt::Return(expr) => {
                                        if let Some(value) = expr {
                                            runtime.evaluate_expr(value)?;
                                        }
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
                Stmt::While(condition, body) => {
                    while runtime.evaluate_expr(condition)? == Value::Boolean(true) {
                        for stmt in body {
                            match stmt {
                                Stmt::Expr(Expr::FunctionCall(name, args)) => {
                                    let mut arg_values = Vec::new();
                                    for arg in args {
                                        arg_values.push(runtime.evaluate_expr(arg)?);
                                    }
                                    runtime.call_function(name, &arg_values)?;
                                }
                                Stmt::VariableDecl(type_name, var_name, expr) => {
                                    runtime.check_type(expr, type_name)?;
                                    let value = runtime.evaluate_expr(expr)?;
                                    runtime.set_variable(var_name.clone(), value);
                                }
                                Stmt::If(_, _, _) | Stmt::While(_, _) | Stmt::For(_, _, _) => {
                                    execute(vec![stmt.clone()])?;
                                }
                                Stmt::Return(expr) => {
                                    if let Some(value) = expr {
                                        runtime.evaluate_expr(value)?;
                                    }
                                    return Ok(());
                                }
                                _ => {}
                            }
                        }
                    }
                }
                Stmt::For(var_name, array_expr, body) => {
                    let array = runtime.evaluate_expr(array_expr)?;
                    if let Value::Array(arr) = array {
                        for value in arr {
                            runtime.set_variable(var_name.clone(), value);
                            for stmt in body {
                                match stmt {
                                    Stmt::Expr(Expr::FunctionCall(name, args)) => {
                                        let mut arg_values = Vec::new();
                                        for arg in args {
                                            arg_values.push(runtime.evaluate_expr(arg)?);
                                        }
                                        runtime.call_function(name, &arg_values)?;
                                    }
                                    Stmt::VariableDecl(type_name, var_name, expr) => {
                                        runtime.check_type(expr, type_name)?;
                                        let value = runtime.evaluate_expr(expr)?;
                                        runtime.set_variable(var_name.clone(), value);
                                    }
                                    Stmt::If(_, _, _) | Stmt::While(_, _) | Stmt::For(_, _, _) => {
                                        execute(vec![stmt.clone()])?;
                                    }
                                    Stmt::Return(expr) => {
                                        if let Some(value) = expr {
                                            runtime.evaluate_expr(value)?;
                                        }
                                        return Ok(());
                                    }
                                    _ => {}
                                }
                            }
                        }
                    } else {
                        return Err("For loop requires an array expression".to_string());
                    }
                }
                Stmt::Return(expr) => {
                    if let Some(value) = expr {
                        runtime.evaluate_expr(value)?;
                    }
                    return Ok(());
                }
                _ => {}
            }
        }
    } else {
        return Err("No main function found".to_string());
    }

    Ok(())
}