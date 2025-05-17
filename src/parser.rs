use std::collections::HashMap;
use std::fmt;
use colored::*;
use nom::{
    error::{ErrorKind},
};
use crate::parser::parse_line::parse_line;

mod parse_line;
mod parse_string;
mod parse_number;
mod parse_boolean;
mod parse_variable;
mod parse_operand;
mod parse_comparison;
mod parse_if_condition;
mod parse_condition_in_parens;
mod parse_expresion;
mod parse_println;
mod parse_variable_decl;
mod parse_function_call;
mod parse_assignment;
mod parse_statement_body;
mod parse_else_if;
mod parse_if;
mod parse_while;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    FunctionCall { name: String, args: Vec<Expr>, line: usize, column: usize },
    Literal(Literal, usize, usize),
    Variable(String, usize, usize),
    BinaryOp(Box<Expr>, String, Box<Expr>, usize, usize),
    Comparison(Box<Expr>, String, Box<Expr>, usize, usize),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    ClassDecl(String, Vec<Stmt>),
    FunctionDecl(String, Vec<Stmt>),
    VariableDecl(String, String, Expr),
    Assignment(String, Expr),
    Expr(Expr),
    FunctionCall(String),
    If {
        condition: Expr,
        body: Vec<Stmt>,
        else_if: Vec<(Expr, Vec<Stmt>)>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub hint: Option<String>,
}

impl ParseError {
    pub fn new(message: String, line: usize, column: usize) -> Self {
        ParseError {
            message,
            line,
            column,
            hint: None,
        }
    }

    pub fn with_hint(message: String, line: usize, column: usize, hint: String) -> Self {
        ParseError {
            message,
            line,
            column,
            hint: Some(hint),
        }
    }

    pub fn print(&self, code: &str) {
        let lines: Vec<&str> = code.lines().collect();
        eprintln!("{}", "┏────────────────────────────────────────".bright_red());
        eprintln!(
            "{} {}: {}",
            "┃".bright_red(),
            "Error".bright_red().bold(),
            self.message
        );
        eprintln!(
            "{} {}: Line {}, Column {}",
            "┃".bright_red(),
            "Location".bright_red().bold(),
            self.line,
            self.column
        );
        if self.line > 0 && self.line <= lines.len() {
            let line_content = lines[self.line - 1];
            let trimmed_content = line_content.trim_start();
            let leading_spaces = line_content.len() - trimmed_content.len();
            eprintln!("{} {}: {}", "┃".bright_red(), "Code".bright_red(), trimmed_content);
            let padding = " ".repeat(self.column.saturating_sub(1 + leading_spaces));
            eprintln!("{}       {}^", "┃".bright_red(), padding.bright_red());
        }
        if let Some(hint) = &self.hint {
            eprintln!("{} {}: {}", "┃".bright_red(), "Hint".bright_red().bold(), hint);
        }
        eprintln!("{}", "┃".bright_red());
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} at line {}, column {}",
            self.message, self.line, self.column
        )?;
        if let Some(hint) = &self.hint {
            write!(f, " (Hint: {})", hint)?;
        }
        Ok(())
    }
}

pub fn parse_program(code: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut stmts = Vec::new();
    let mut current_class: Option<String> = None;
    let mut current_function: Option<String> = None;
    let mut if_stack: Vec<(Expr, Vec<Stmt>, Vec<(Expr, Vec<Stmt>)>, Vec<Stmt>, bool, bool)> = Vec::new();
    let mut class_body: Vec<Stmt> = Vec::new();
    let mut function_body: Vec<Stmt> = Vec::new();
    let mut block_depth = 0;
    let mut if_block_depth = 0;
    let mut declared_variables: HashMap<String, (usize, usize)> = HashMap::new();
    let mut while_stack: Vec<(Expr, Vec<Stmt>)> = Vec::new();
    let mut while_block_depth: isize = 0;

    let lines: Vec<&str> = code.lines().collect();
    let mut line_index = 0;

    while line_index < lines.len() {
        let line = lines[line_index];
        let trimmed = line.trim();
        if trimmed.is_empty() {
            line_index += 1;
            continue;
        }

        let column = line.chars().take_while(|c| c.is_whitespace()).count() + 1;

        if trimmed.starts_with("public class") {
            let class_name = trimmed
                .replace("public class", "")
                .replace("{", "")
                .trim()
                .to_string();
            if current_class.is_some() {
                return Err(ParseError::with_hint(
                    "Nested classes are not supported".to_string(),
                    line_index + 1,
                    column,
                    "Remove the nested class or move it outside the current class.".to_string(),
                ));
            }
            current_class = Some(class_name);
            class_body = Vec::new();
            declared_variables.clear();
            block_depth += 1;
            line_index += 1;
            continue;
        } else if trimmed.starts_with("function") {
            if current_function.is_some() {
                return Err(ParseError::with_hint(
                    "Nested functions are not supported".to_string(),
                    line_index + 1,
                    column,
                    "Move the function outside the current function.".to_string(),
                ));
            }
            let func_name = trimmed
                .replace("function", "")
                .replace("() {", "")
                .trim()
                .to_string();
            current_function = Some(func_name);
            function_body = Vec::new();
            declared_variables.clear();
            block_depth += 1;
            line_index += 1;
            continue;
        }

        let (rest, (stmt, else_part)) = parse_line(trimmed, line_index + 1, column).map_err(|e| {
            let message = match &e {
                nom::Err::Error(err) | nom::Err::Failure(err) => {
                    if trimmed.starts_with("if ") {
                        format!("Failed to parse if statement: unexpected '{}'", err.input)
                    } else if trimmed.starts_with("else ") {
                        format!("Failed to parse else statement: unexpected '{}'", err.input)
                    } else if err.code == ErrorKind::Tag && trimmed.contains("=") && !trimmed.ends_with(";") {
                        "Missing semicolon".to_string()
                    } else {
                        format!("Invalid syntax: unexpected '{}'", err.input)
                    }
                }
                nom::Err::Incomplete(_) => "Incomplete input".to_string(),
            };
            let hint = match &e {
                nom::Err::Error(err) | nom::Err::Failure(err) => {
                    if trimmed.starts_with("if ") {
                        Some("Ensure the if statement has a valid condition in parentheses and a block body.".to_string())
                    } else if trimmed.starts_with("else ") {
                        Some("Ensure the else statement follows a valid if or else if block.".to_string())
                    } else if err.code == ErrorKind::Tag && trimmed.contains("=") && !trimmed.ends_with(";") {
                        Some("Add a semicolon (;) at the end of the statement.".to_string())
                    } else {
                        Some("Check the syntax and ensure all keywords and operators are used correctly.".to_string())
                    }
                }
                nom::Err::Incomplete(_) => Some("Complete the statement or expression.".to_string()),
            };
            let error_input = match &e {
                nom::Err::Error(err) | nom::Err::Failure(err) => err.input,
                nom::Err::Incomplete(_) => trimmed,
            };
            let error_column = column + (trimmed.len() - error_input.len());
            ParseError::with_hint(message, line_index + 1, error_column, hint.unwrap())
        })?;

        if !rest.is_empty() {
            return Err(ParseError::with_hint(
                format!("Unexpected tokens: {}", rest),
                line_index + 1,
                column + (trimmed.len() - rest.len()),
                "Remove or correct the unexpected tokens.".to_string(),
            ));
        }

        if let Some((ref else_type, ref condition)) = else_part {
            if if_stack.is_empty() {
                return Err(ParseError::with_hint(
                    format!("{} without matching if", else_type),
                    line_index + 1,
                    column,
                    format!("Add a matching 'if' statement before '{}'.", else_type),
                ));
            }
            if else_type == "else if" {
                if let Some(last) = if_stack.last_mut() {
                    last.5 = true;
                    last.2.push((condition.clone(), Vec::new()));
                    if_block_depth += 1;
                    block_depth += 1;
                }
            } else if else_type == "else" {
                if let Some(last) = if_stack.last_mut() {
                    last.4 = true;
                    last.5 = false;
                    last.3 = Vec::new();
                    if_block_depth += 1;
                    block_depth += 1;
                }
            }
        }

        match &stmt {
            Stmt::Expr(Expr::Literal(Literal::String(s), _, _)) if s == "END_BLOCK" => {
                block_depth -= 1;
                if !while_stack.is_empty() {
                    while_block_depth -= 1;
                    if while_block_depth < 0 {
                        return Err(ParseError::with_hint(
                            "Mismatched while block closure".to_string(),
                            line_index + 1,
                            column,
                            "Check for missing or extra closing braces in while blocks.".to_string(),
                        ));
                    }
                    if while_block_depth == 0 || (while_stack.len() > 1 && while_block_depth == (while_stack.len() - 1) as isize) {
                        if let Some((condition, while_body)) = while_stack.pop() {
                            let while_stmt = Stmt::While {
                                condition,
                                body: while_body,
                            };
                            if !while_stack.is_empty() {
                                if let Some(last) = while_stack.last_mut() {
                                    last.1.push(while_stmt.clone());
                                }
                            } else if !if_stack.is_empty() {
                                if let Some(last) = if_stack.last_mut() {
                                    if last.4 {
                                        last.3.push(while_stmt.clone());
                                    } else if last.5 && !last.2.is_empty() {
                                        if let Some(last_branch) = last.2.last_mut() {
                                            last_branch.1.push(while_stmt.clone());
                                        }
                                    } else {
                                        last.1.push(while_stmt.clone());
                                    }
                                }
                            } else if current_function.is_some() {
                                function_body.push(while_stmt.clone());
                            } else if current_class.is_some() {
                                class_body.push(while_stmt.clone());
                            } else {
                                stmts.push(while_stmt.clone());
                            }
                            while_block_depth = while_stack.len() as isize;
                        }
                    }
                }
                if !if_stack.is_empty() {
                    if_block_depth -= 1;
                    if if_stack.last().map_or(false, |last| last.4 && if_block_depth <= 0) {
                        if let Some((condition, if_body, else_if_branches, else_body, _, _)) = if_stack.pop() {
                            let else_stmt = if else_body.is_empty() {
                                None
                            } else {
                                Some(Box::new(Stmt::Block(else_body)))
                            };
                            let if_stmt = Stmt::If {
                                condition,
                                body: if_body,
                                else_if: else_if_branches,
                                else_branch: else_stmt,
                            };
                            if !if_stack.is_empty() {
                                if let Some(last) = if_stack.last_mut() {
                                    if last.4 {
                                        last.3.push(if_stmt.clone());
                                    } else if last.5 && !last.2.is_empty() {
                                        if let Some(last_branch) = last.2.last_mut() {
                                            last_branch.1.push(if_stmt.clone());
                                        }
                                    } else {
                                        last.1.push(if_stmt.clone());
                                    }
                                }
                            } else if current_function.is_some() {
                                function_body.push(if_stmt.clone());
                            } else if current_class.is_some() {
                                class_body.push(if_stmt.clone());
                            } else {
                                stmts.push(if_stmt.clone());
                            }
                            if_block_depth = if_stack.iter().map(|_| 1).sum();
                        }
                    } else if if_stack.last().map_or(false, |last| last.5 && if_block_depth <= 0) {
                        if let Some(last) = if_stack.last_mut() {
                            last.5 = false;
                        }
                    } else if if_block_depth >= 0 && if_stack.last().map_or(false, |last| !last.5 && !last.4) {
                        if let Some((condition, if_body, else_if_branches, else_body, _, _)) = if_stack.pop() {
                            let else_stmt = if else_body.is_empty() {
                                None
                            } else {
                                Some(Box::new(Stmt::Block(else_body)))
                            };
                            let if_stmt = Stmt::If {
                                condition,
                                body: if_body,
                                else_if: else_if_branches,
                                else_branch: else_stmt,
                            };
                            if !if_stack.is_empty() {
                                if let Some(last) = if_stack.last_mut() {
                                    if last.4 {
                                        last.3.push(if_stmt.clone());
                                    } else if last.5 && !last.2.is_empty() {
                                        if let Some(last_branch) = last.2.last_mut() {
                                            last_branch.1.push(if_stmt.clone());
                                        }
                                    } else {
                                        last.1.push(if_stmt.clone());
                                    }
                                }
                            } else if current_function.is_some() {
                                function_body.push(if_stmt.clone());
                            } else if current_class.is_some() {
                                class_body.push(if_stmt.clone());
                            } else {
                                stmts.push(if_stmt.clone());
                            }
                            if_block_depth = if_stack.iter().map(|_| 1).sum();
                        }
                    }
                }

                if current_function.is_some() && block_depth == 1 {
                    if let Some(func_name) = current_function.take() {
                        class_body.push(Stmt::FunctionDecl(func_name.clone(), function_body.clone()));
                        function_body = Vec::new();
                        declared_variables.clear();
                    }
                } else if current_class.is_some() && block_depth == 0 {
                    if let Some(class_name) = current_class.take() {
                        stmts.push(Stmt::ClassDecl(class_name.clone(), class_body.clone()));
                        class_body = Vec::new();
                        declared_variables.clear();
                    }
                }
            }
            Stmt::While { condition, .. } => {
                while_stack.push((condition.clone(), Vec::new()));
                block_depth += 1;
                while_block_depth += 1;
                line_index += 1;
                continue;
            }
            Stmt::If { condition, .. } => {
                if_stack.push((condition.clone(), Vec::new(), Vec::new(), Vec::new(), false, false));
                block_depth += 1;
                if_block_depth += 1;
                line_index += 1;
                continue;
            }
            Stmt::VariableDecl(_type_name, var_name, _) => {
                if declared_variables.contains_key(var_name) {
                    return Err(ParseError::with_hint(
                        format!("Variable {} already declared", var_name),
                        line_index + 1,
                        column,
                        "Use a different variable name or remove the duplicate declaration.".to_string(),
                    ));
                }
                declared_variables.insert(var_name.clone(), (line_index + 1, column));
            }
            Stmt::Assignment(var_name, _) => {
                if !declared_variables.contains_key(var_name) {
                    return Err(ParseError::with_hint(
                        format!("Variable {} not declared before assignment", var_name),
                        line_index + 1,
                        column,
                        format!("Declare the variable using 'num {} = value;' before assigning to it.", var_name),
                    ));
                }
            }
            _ => {}
        }

        if !matches!(&stmt, Stmt::Expr(Expr::Literal(Literal::String(s), _, _)) if s == "END_BLOCK") {
            if !while_stack.is_empty() {
                let target_stack_index = while_block_depth.saturating_sub(1) as usize;
                if target_stack_index < while_stack.len() {
                    if let Some(last) = while_stack.get_mut(target_stack_index) {
                        last.1.push(stmt.clone());
                    }
                } else {
                    return Err(ParseError::with_hint(
                        "Statement outside of expected while block".to_string(),
                        line_index + 1,
                        column,
                        "Ensure the statement is inside a valid while block or remove it.".to_string(),
                    ));
                }
            } else if !if_stack.is_empty() {
                if let Some(last) = if_stack.last_mut() {
                    if last.4 {
                        last.3.push(stmt.clone());
                    } else if last.5 && !last.2.is_empty() {
                        if let Some(last_branch) = last.2.last_mut() {
                            last_branch.1.push(stmt.clone());
                        }
                    } else {
                        last.1.push(stmt.clone());
                    }
                }
            } else if current_function.is_some() {
                function_body.push(stmt.clone());
            } else if current_class.is_some() {
                class_body.push(stmt.clone());
            } else {
                stmts.push(stmt.clone());
            }
        }

        line_index += 1;
    }

    if !if_stack.is_empty() {
        return Err(ParseError::with_hint(
            "Unclosed if block".to_string(),
            lines.len(),
            1,
            "Add a closing brace '}' to complete the if block.".to_string(),
        ));
    }
    if !while_stack.is_empty() {
        return Err(ParseError::with_hint(
            "Unclosed while block".to_string(),
            lines.len(),
            1,
            "Add a closing brace '}' to complete the while block.".to_string(),
        ));
    }
    if current_function.is_some() {
        return Err(ParseError::with_hint(
            "Unclosed function block".to_string(),
            lines.len(),
            1,
            "Add a closing brace '}' to complete the function block.".to_string(),
        ));
    }
    if current_class.is_some() {
        if let Some(class_name) = current_class.take() {
            stmts.push(Stmt::ClassDecl(class_name, class_body));
        }
    }
    if block_depth != 0 {
        return Err(ParseError::with_hint(
            "Unclosed block".to_string(),
            lines.len(),
            1,
            "Add a closing brace '}' to complete the block.".to_string(),
        ));
    }

    Ok(stmts)
}