use crate::parser::{Expr, Stmt};
use crate::runtime::{Runtime};

pub fn execute(stmts: Vec<Stmt>) -> Result<(), String> {
    let mut runtime = Runtime::new();
    let mut main_body = Vec::new();

    println!("Processing statements: {:?}", stmts);
    for stmt in &stmts {
        println!("Processing stmt: {:?}", stmt);
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
        println!("Executing main body: {:?}", main_body);
        for stmt in &main_body {
            println!("Executing stmt: {:?}", stmt);
            match stmt {
                Stmt::Expr(Expr::FunctionCall(name, args)) => {
                    runtime.call_function(name, args)?;
                }
                Stmt::VariableDecl(_type_name, var_name, expr) => {
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
        return Err("No main function found".to_string());
    }

    Ok(())
}