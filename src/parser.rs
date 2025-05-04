use std::collections::HashMap;
use std::fmt;
use colored::*;
use nom::{
    IResult,
    bytes::complete::{tag, take_until},
    sequence::delimited,
    character::complete::{alphanumeric1, multispace0, one_of},
    branch::alt,
    combinator::{map, opt},
    sequence::tuple,
    number::complete::double,
    error::{context, Error, ErrorKind},
};

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
    Block(Vec<Stmt>),
    While { condition: Expr, body: Vec<Stmt> },
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl ParseError {
    pub fn new(message: String, line: usize, column: usize) -> Self {
        ParseError { message, line, column }
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
            "{} {}: {}, {}",
            "┃".bright_red(),
            "Location".bright_red().bold(),
            format!("Line {}", self.line),
            format!("Column {}", self.column)
        );
        if self.line > 0 && self.line <= lines.len() {
            let line_content = lines[self.line - 1].trim_end();
            eprintln!("{} {}: {}", "┃".bright_red(), "Code".bright_red(), line_content);
        }
        eprintln!("{}", "┃".bright_red());
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at line {}, column {}", self.message, self.line, self.column)
    }
}

fn parse_string(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "string literal",
        map(
            delimited(tag("\""), take_until("\""), tag("\"")),
            |s: &str| Expr::Literal(Literal::String(s.to_string()), line, column),
        ),
    )(input)
}

fn parse_number(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "number literal",
        map(double, |n: f64| Expr::Literal(Literal::Number(n), line, column)),
    )(input)
}

fn parse_boolean(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "boolean literal",
        alt((
            map(tag("true"), |_| Expr::Literal(Literal::Boolean(true), line, column)),
            map(tag("false"), |_| Expr::Literal(Literal::Boolean(false), line, column)),
        )),
    )(input)
}

fn parse_variable(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "variable",
        map(alphanumeric1, |name: &str| Expr::Variable(name.to_string(), line, column)),
    )(input)
}

fn parse_operand(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "operand",
        alt((
            |i| parse_number(i, line, column),
            |i| parse_boolean(i, line, column),
            |i| parse_variable(i, line, column),
        )),
    )(input)
}

fn parse_binary_op(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "binary operation",
        map(
            tuple((
                |i| parse_operand(i, line, column),
                multispace0,
                one_of("+*/"),
                multispace0,
                |i| parse_operand(i, line, column),
            )),
            |(left, _, op, _, right)| {
                Expr::BinaryOp(Box::new(left), op.to_string(), Box::new(right), line, column)
            },
        ),
    )(input)
}

fn parse_comparison(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "comparison operation",
        map(
            tuple((
                |i| {
                    parse_operand(i, line, column)
                },
                multispace0,
                alt((tag("=="), tag("!="), tag("<="), tag(">="), tag("<"), tag(">"))),
                multispace0,
                |i| {
                    parse_operand(i, line, column)
                },
            )),
            |(left, _, op, _, right)| {
                Expr::Comparison(Box::new(left), op.to_string(), Box::new(right), line, column)
            },
        ),
    )(input)
}

fn parse_if_condition(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "if condition",
        alt((
            |i| parse_comparison(i, line, column),
            |i| parse_boolean(i, line, column),
            |i| parse_variable(i, line, column),
        )),
    )(input)
}

fn parse_condition_in_parens(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "condition in parentheses",
        delimited(
            tuple((multispace0, tag("("), multispace0)),
            |i| parse_if_condition(i, line, column),
            tuple((multispace0, tag(")"), multispace0)),
        ),
    )(input)
}

fn parse_expression(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "expression",
        alt((
            |i| parse_comparison(i, line, column),
            |i| parse_binary_op(i, line, column),
            |i| parse_string(i, line, column),
            |i| parse_number(i, line, column),
            |i| parse_boolean(i, line, column),
            |i| parse_variable(i, line, column),
        )),
    )(input)
}

fn parse_println(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "println",
        map(
            tuple((
                tag("system.console.println("),
                |i| {
                    parse_expression(i, line, column)
                },
                tag(")"),
                opt(tag(";")),
            )),
            |(_, content, _, _)| Expr::FunctionCall {
                name: "println".to_string(),
                args: vec![content],
                line,
                column,
            },
        ),
    )(input)
}

fn parse_variable_decl(input: &str, line: usize, column: usize) -> IResult<&str, Stmt> {
    context(
        "variable declaration",
        map(
            tuple((
                alt((tag("num"), tag("string"), tag("boolean"))),
                multispace0,
                alphanumeric1,
                multispace0,
                tag("="),
                multispace0,
                |i| parse_expression(i, line, column),
                multispace0,
                tag(";"),
            )),
            |(type_name, _, var_name, _, _, _, value, _, _)| {
                Stmt::VariableDecl(type_name.to_string(), var_name.to_string(), value)
            },
        ),
    )(input)
}

fn parse_assignment(input: &str, line: usize, column: usize) -> IResult<&str, Stmt> {
    context(
        "assignment",
        map(
            tuple((
                alphanumeric1,
                multispace0,
                tag("="),
                multispace0,
                |i| parse_expression(i, line, column),
                multispace0,
                tag(";"),
            )),
            |(var_name, _, _, _, value, _, _)| Stmt::Assignment(var_name.to_string(), value),
        ),
    )(input)
}

fn parse_function_call(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "function call",
        map(
            tuple((take_until("();"), tag("();"))),
            |(name, _): (&str, &str)| Expr::FunctionCall {
                name: name.trim().to_string(),
                args: vec![],
                line,
                column,
            },
        ),
    )(input)
}

fn parse_block(input: &str, _line: usize, _column: usize) -> IResult<&str, ()> {
    context(
        "block",
        map(
            tuple((multispace0, tag("{"), multispace0)),
            |_| (),
        ),
    )(input)
}

fn parse_while(input: &str, line: usize, column: usize) -> IResult<&str, Stmt> {
    context(
        "while statement",
        map(
            tuple((
                tag("while"),
                multispace0,
                |i| parse_condition_in_parens(i, line, column),
                multispace0,
                |i| parse_block(i, line, column),
            )),
            |(_, _, condition, _, _)| Stmt::While {
                condition,
                body: Vec::new(),
            },
        ),
    )(input)
}

fn parse_if(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "if statement",
        map(
            tuple((
                tag("if"),
                multispace0,
                |i| {
                    parse_condition_in_parens(i, line, column)
                },
                multispace0,
                |i| parse_block(i, line, column),
            )),
            |(_, _, condition, _, _)| condition,
        ),
    )(input)
}

fn parse_else_if(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "else if statement",
        map(
            tuple((
                tag("else if"),
                multispace0,
                |i| parse_condition_in_parens(i, line, column),
                multispace0,
                |i| parse_block(i, line, column),
            )),
            |(_, _, condition, _, _)| condition,
        ),
    )(input)
}

fn parse_line<'a>(input: &'a str, line: usize, column: usize) -> IResult<&'a str, (Stmt, Option<(String, Expr)>)> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(nom::Err::Error(Error {
            input: trimmed,
            code: ErrorKind::Fail,
        }));
    }
    context(
        "line",
        alt((
            map(
                tuple((
                    |i| parse_if(i, line, column),
                    opt(|i: &'a str| {
                        let (rest, _) = multispace0(i)?;
                        alt((
                            map(
                                |i| parse_else_if(i, line, column),
                                |cond| ("else if".to_string(), cond),
                            ),
                            map(
                                tag("else {"),
                                |_| ("else".to_string(), Expr::Literal(Literal::Boolean(true), line, column)),
                            ),
                        ))(rest)
                    }),
                )),
                |(condition, else_part)| (
                    Stmt::If {
                        condition,
                        body: vec![],
                        else_if: vec![],
                        else_branch: None,
                    },
                    else_part,
                ),
            ),
            map(|i| parse_assignment(i, line, column), |stmt| (stmt, None)),
            map(|i| parse_variable_decl(i, line, column), |stmt| (stmt, None)),
            map(|i| parse_println(i, line, column), |expr| (Stmt::Expr(expr), None)),
            map(|i| parse_function_call(i, line, column), |expr| (Stmt::Expr(expr), None)),
            map(|i| parse_while(i, line, column), |stmt| (stmt, None)),
            map(
                tuple((
                    tag("}"),
                    opt(|i: &'a str| {
                        let (rest, _) = multispace0(i)?;
                        alt((
                            map(
                                |i| parse_else_if(i, line, column),
                                |cond| ("else if".to_string(), cond),
                            ),
                            map(
                                tag("else {"),
                                |_| ("else".to_string(), Expr::Literal(Literal::Boolean(true), line, column)),
                            ),
                        ))(rest)
                    }),
                )),
                |(_, else_part)| (
                    Stmt::Expr(Expr::Literal(Literal::String("END_BLOCK".to_string()), line, column)),
                    else_part,
                ),
            ),
            map(tag("main();"), |_| (Stmt::FunctionCall("main".to_string()), None)),
        )),
    )(trimmed)
}

pub fn parse_program(code: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut stmts = Vec::new();
    let mut current_class: Option<String> = None;
    let mut current_function: Option<String> = None;
    let mut if_stack: Vec<(Expr, Vec<Stmt>, Vec<(Expr, Vec<Stmt>)>, Vec<Stmt>, bool, bool)> = Vec::new();
    let mut while_stack: Vec<(Expr, Vec<Stmt>)> = Vec::new();
    let mut class_body: Vec<Stmt> = Vec::new();
    let mut function_body: Vec<Stmt> = Vec::new();
    let mut block_depth = 0;
    let mut if_block_depth = 0;
    let mut while_block_depth = 0;
    let mut declared_variables: HashMap<String, (usize, usize)> = HashMap::new();

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
                return Err(ParseError::new(
                    "Nested classes are not supported".to_string(),
                    line_index + 1,
                    column,
                ));
            }
            current_class = Some(class_name);
            class_body = Vec::new();
            declared_variables.clear();
            block_depth += 1;
            line_index += 1;
            continue;
        } else if trimmed.starts_with("function") {
            let func_name = trimmed
                .replace("function", "")
                .replace("() {", "")
                .trim()
                .to_string();
            if current_function.is_some() {
                return Err(ParseError::new(
                    "Nested functions are not supported".to_string(),
                    line_index + 1,
                    column,
                ));
            }
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
            let error_input = match &e {
                nom::Err::Error(err) | nom::Err::Failure(err) => err.input,
                nom::Err::Incomplete(_) => trimmed,
            };
            let error_column = column + (trimmed.len() - error_input.len());
            ParseError::new(message, line_index + 1, error_column)
        })?;

        if !rest.is_empty() {
            return Err(ParseError::new(
                format!("Unexpected tokens: {}", rest),
                line_index + 1,
                column + (trimmed.len() - rest.len()),
            ));
        }

        if let Some((ref else_type, ref condition)) = else_part {
            if if_stack.is_empty() {
                return Err(ParseError::new(
                    format!("{} without matching if", else_type),
                    line_index + 1,
                    column,
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
                if !while_stack.is_empty() {
                    while_block_depth -= 1;
                    if while_block_depth <= 0 {
                        if let Some((condition, while_body)) = while_stack.pop() {
                            let while_stmt = Stmt::While {
                                condition,
                                body: while_body,
                            };
                            if !if_stack.is_empty() {
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
                            while_block_depth = while_stack.iter().map(|_| 1).sum();
                        }
                    }
                }
                if block_depth == 0 {
                    if let Some(func_name) = current_function.take() {
                        class_body.push(Stmt::FunctionDecl(func_name.clone(), function_body));
                        function_body = Vec::new();
                    }
                    if let Some(class_name) = current_class.take() {
                        stmts.push(Stmt::ClassDecl(class_name.clone(), class_body));
                        class_body = Vec::new();
                    }
                }
            }
            Stmt::If { condition, .. } => {
                if_stack.push((condition.clone(), Vec::new(), Vec::new(), Vec::new(), false, false));
                block_depth += 1;
                if_block_depth += 1;
                line_index += 1;
                continue;
            }
            Stmt::While { condition, .. } => {
                while_stack.push((condition.clone(), Vec::new()));
                block_depth += 1;
                while_block_depth += 1;
                line_index += 1;
                continue;
            }
            Stmt::VariableDecl(_type_name, var_name, _) => {
                if declared_variables.contains_key(var_name) {
                    return Err(ParseError::new(
                        format!("Variable {} already declared", var_name),
                        line_index + 1,
                        column,
                    ));
                }
                declared_variables.insert(var_name.clone(), (line_index + 1, column));
            }
            Stmt::Assignment(var_name, _) => {
                if !declared_variables.contains_key(var_name) {
                    return Err(ParseError::new(
                        format!("Variable {} not declared before assignment", var_name),
                        line_index + 1,
                        column,
                    ));
                }
            }
            _ => {}
        }

        if !matches!(&stmt, Stmt::Expr(Expr::Literal(Literal::String(s), _, _)) if s == "END_BLOCK") {
            if !if_stack.is_empty() {
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
            } else if !while_stack.is_empty() {
                if let Some(last) = while_stack.last_mut() {
                    last.1.push(stmt.clone());
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
        return Err(ParseError::new(
            "Unclosed if block".to_string(),
            lines.len(),
            1,
        ));
    }
    if !while_stack.is_empty() {
        return Err(ParseError::new(
            "Unclosed while block".to_string(),
            lines.len(),
            1,
        ));
    }
    if current_function.is_some() || current_class.is_some() {
        return Err(ParseError::new(
            "Unclosed class or function block".to_string(),
            lines.len(),
            1,
        ));
    }
    if block_depth != 0 {
        return Err(ParseError::new(
            "Unclosed block".to_string(),
            lines.len(),
            1,
        ));
    }

    Ok(stmts)
}