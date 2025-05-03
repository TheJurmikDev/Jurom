use std::fmt;
use nom::{
    IResult,
    bytes::complete::{tag, take_until},
    sequence::delimited,
    character::complete::{alphanumeric1, multispace0, one_of},
    branch::alt,
    combinator::map,
    sequence::tuple,
    number::complete::double,
    error::{context, Error, ErrorKind},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    FunctionCall(String, Vec<Expr>),
    Literal(Literal),
    Variable(String),
    BinaryOp(Box<Expr>, String, Box<Expr>),
    Comparison(Box<Expr>, String, Box<Expr>),
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
    Expr(Expr),
    FunctionCall(String),
    If(Expr, Vec<Stmt>, Option<Box<Stmt>>),
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

fn parse_string(input: &str) -> IResult<&str, &str> {
    context("string literal", delimited(tag("\""), take_until("\""), tag("\"")))(input)
}

fn parse_number(input: &str) -> IResult<&str, f64> {
    context("number literal", double)(input)
}

fn parse_boolean(input: &str) -> IResult<&str, bool> {
    context(
        "boolean literal",
        alt((
            map(tag("true"), |_| true),
            map(tag("false"), |_| false),
        )),
    )(input)
}

fn parse_variable(input: &str) -> IResult<&str, Expr> {
    context(
        "variable",
        map(alphanumeric1, |name: &str| Expr::Variable(name.to_string())),
    )(input)
}

fn parse_operand(input: &str) -> IResult<&str, Expr> {
    context(
        "operand",
        alt((
            map(parse_number, |n: f64| Expr::Literal(Literal::Number(n))),
            map(parse_boolean, |b: bool| Expr::Literal(Literal::Boolean(b))),
            parse_variable,
        )),
    )(input)
}

fn parse_binary_op(input: &str) -> IResult<&str, Expr> {
    context(
        "binary operation",
        map(
            tuple((
                parse_operand,
                multispace0,
                one_of("+*/"),
                multispace0,
                parse_operand,
            )),
            |(left, _, op, _, right)| {
                Expr::BinaryOp(Box::new(left), op.to_string(), Box::new(right))
            },
        ),
    )(input)
}

fn parse_comparison(input: &str) -> IResult<&str, Expr> {
    println!("Attempting to parse comparison: {}", input);
    context(
        "comparison operation",
        map(
            tuple((
                parse_operand,
                multispace0,
                alt((tag("=="), tag("<"), tag(">"))),
                multispace0,
                parse_operand,
            )),
            |(left, _, op, _, right)| {
                println!("Parsed comparison: {:?} {} {:?}", left, op, right);
                Expr::Comparison(Box::new(left), op.to_string(), Box::new(right))
            },
        ),
    )(input)
}

fn parse_if_condition(input: &str) -> IResult<&str, Expr> {
    println!("Attempting to parse if condition: {}", input);
    let (rest, expr) = context(
        "if condition",
        alt((
            parse_comparison,
            map(parse_boolean, |b: bool| Expr::Literal(Literal::Boolean(b))),
            parse_variable,
        )),
    )(input)?;
    println!("Remaining input after if condition: {}", rest);
    Ok((rest, expr))
}

fn parse_condition_in_parens(input: &str) -> IResult<&str, Expr> {
    println!("Attempting to parse condition in parens: {}", input);
    let (rest, expr) = context(
        "condition in parentheses",
        delimited(
            tuple((multispace0, tag("("), multispace0)),
            parse_if_condition,
            tuple((multispace0, tag(")"), multispace0)),
        ),
    )(input)?;
    println!("Remaining input after condition in parens: {}", rest);
    Ok((rest, expr))
}

fn parse_expression(input: &str) -> IResult<&str, Expr> {
    context(
        "expression",
        alt((
            parse_comparison,
            parse_binary_op,
            map(parse_string, |s: &str| Expr::Literal(Literal::String(s.to_string()))),
            map(parse_number, |n: f64| Expr::Literal(Literal::Number(n))),
            map(parse_boolean, |b: bool| Expr::Literal(Literal::Boolean(b))),
            parse_variable,
        )),
    )(input)
}

fn parse_println(input: &str) -> IResult<&str, Expr> {
    context(
        "println",
        map(
            tuple((
                tag("system.console.println("),
                parse_expression,
                tag(")"),
                nom::combinator::opt(tag(";")),
            )),
            |(_, content, _, _)| Expr::FunctionCall("println".to_string(), vec![content]),
        ),
    )(input)
}

fn parse_variable_decl(input: &str) -> IResult<&str, Stmt> {
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
                parse_expression,
                multispace0,
                tag(";"),
            )),
            |(type_name, _, var_name, _, _, _, value, _, _)| {
                Stmt::VariableDecl(type_name.to_string(), var_name.to_string(), value)
            },
        ),
    )(input)
}

fn parse_function_call(input: &str) -> IResult<&str, Expr> {
    context(
        "function call",
        map(
            tuple((take_until("();"), tag("();"))),
            |(name, _): (&str, &str)| Expr::FunctionCall(name.trim().to_string(), vec![]),
        ),
    )(input)
}

#[allow(dead_code)]
fn parse_block(input: &str) -> IResult<&str, Vec<Stmt>> {
    println!("Attempting to parse block: {}", input);
    let (rest, stmts) = context(
        "block",
        delimited(
            tuple((multispace0, tag("{"), multispace0)),
            nom::multi::many0(parse_line),
            tuple((multispace0, tag("}"), multispace0)),
        ),
    )(input)?;
    println!("Remaining input after block: {}", rest);
    Ok((rest, stmts))
}

fn parse_if(input: &str) -> IResult<&str, (Expr, Vec<Stmt>)> {
    println!("Attempting to parse if: {}", input);
    let (rest, (condition, _)): (&str, (Expr, Vec<Stmt>)) = context(
        "if statement",
        map(
            tuple((
                tag("if"),
                multispace0,
                parse_condition_in_parens,
                multispace0,
                tag("{"),
            )),
            |(_, _, condition, _, _)| (condition, vec![]),
        ),
    )(input)?;
    println!("Parsed if condition: {:?}", condition);
    println!("Remaining input after if: {}", rest);
    Ok((rest, (condition, vec![])))
}

fn parse_line(line: &str) -> IResult<&str, Stmt> {
    let trimmed = line.trim();
    println!("Attempting to parse line: {}", trimmed);
    if trimmed.is_empty() {
        return Err(nom::Err::Error(Error {
            input: trimmed,
            code: ErrorKind::Fail,
        }));
    }
    context(
        "line",
        alt((
            map(parse_if, |(condition, body)| Stmt::If(condition, body, None)),
            parse_variable_decl,
            map(parse_println, Stmt::Expr),
            map(parse_function_call, Stmt::Expr),
            map(tag("}"), |_| {
                Stmt::Expr(Expr::Literal(Literal::String("END_BLOCK".to_string())))
            }),
            map(tag("main();"), |_| Stmt::FunctionCall("main".to_string())),
        )),
    )(trimmed)
}

pub fn parse_program(code: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut stmts = Vec::new();
    let mut current_class: Option<String> = None;
    let mut current_function: Option<String> = None;
    let mut current_if: Option<(Expr, Vec<Stmt>)> = None;
    let mut class_body: Vec<Stmt> = Vec::new();
    let mut function_body: Vec<Stmt> = Vec::new();
    let mut if_body: Vec<Stmt> = Vec::new();
    let mut else_body: Vec<Stmt> = Vec::new();
    let mut in_else = false;

    for line in code.lines() {
        let trimmed = line.trim();
        println!("Parsing line: {}", trimmed);
        if trimmed.is_empty() {
            continue;
        }
        if trimmed.starts_with("public class") {
            let class_name = trimmed
                .replace("public class", "")
                .replace("{", "")
                .trim()
                .to_string();
            if current_class.is_some() {
                return Err(ParseError {
                    message: "Nested classes are not supported".to_string(),
                });
            }
            current_class = Some(class_name);
            class_body = Vec::new();
            continue;
        } else if trimmed.starts_with("function") {
            let func_name = trimmed
                .replace("function", "")
                .replace("() {", "")
                .trim()
                .to_string();
            if current_function.is_some() {
                return Err(ParseError {
                    message: "Nested functions are not supported".to_string(),
                });
            }
            current_function = Some(func_name);
            function_body = Vec::new();
            continue;
        } else if trimmed == "} else {" {
            println!("Detected combined }} else {{");
            // Process the closing brace as END_BLOCK
            if in_else {
                if let Some((condition, if_body)) = current_if.take() {
                    let else_stmt = Some(Box::new(Stmt::Block(else_body.clone())));
                    if current_function.is_some() {
                        function_body.push(Stmt::If(condition.clone(), if_body, else_stmt));
                    } else if current_class.is_some() {
                        class_body.push(Stmt::If(condition.clone(), if_body, else_stmt));
                    } else {
                        stmts.push(Stmt::If(condition.clone(), if_body, else_stmt));
                    }
                    current_if = Some((condition, vec![])); // Restore current_if for else branch
                }
                else_body.clear();
            } else if let Some((condition, mut body)) = current_if.take() {
                body.append(&mut if_body);
                let else_stmt = if else_body.is_empty() {
                    None
                } else {
                    Some(Box::new(Stmt::Block(else_body.clone())))
                };
                if current_function.is_some() {
                    function_body.push(Stmt::If(condition.clone(), body, else_stmt));
                    println!("Final If: {:?}", function_body);
                } else if current_class.is_some() {
                    class_body.push(Stmt::If(condition.clone(), body, else_stmt));
                } else {
                    stmts.push(Stmt::If(condition.clone(), body, else_stmt));
                }
                current_if = Some((condition, vec![])); // Restore current_if for else branch
                if_body.clear();
                else_body.clear();
            }
            // Start the else branch
            in_else = true;
            else_body = Vec::new();
            continue;
        } else if trimmed == "else {" {
            if current_if.is_none() {
                return Err(ParseError {
                    message: "Else without matching if".to_string(),
                });
            }
            in_else = true;
            else_body = Vec::new();
            continue;
        }

        let (rest, stmt) = parse_line(trimmed).map_err(|e| {
            let message = match e {
                nom::Err::Error(err) | nom::Err::Failure(err) => {
                    format!(
                        "Error while parsing line '{}': input='{}', code={:?}",
                        trimmed, err.input, err.code
                    )
                }
                nom::Err::Incomplete(_) => {
                    format!("Incomplete input while parsing line '{}'", trimmed)
                }
            };
            ParseError { message }
        })?;
        if !rest.is_empty() {
            return Err(ParseError {
                message: format!("Unexpected tokens after parsing line '{}': {}", trimmed, rest),
            });
        }
        println!("Parsed statement: {:?}", stmt);

        match &stmt {
            Stmt::Expr(Expr::Literal(Literal::String(s))) if s == "END_BLOCK" => {
                if in_else {
                    if let Some((condition, if_body)) = current_if.take() {
                        let else_stmt = Some(Box::new(Stmt::Block(else_body.clone())));
                        if current_function.is_some() {
                            function_body.push(Stmt::If(condition, if_body, else_stmt));
                        } else if current_class.is_some() {
                            class_body.push(Stmt::If(condition, if_body, else_stmt));
                        } else {
                            stmts.push(Stmt::If(condition, if_body, else_stmt));
                        }
                    }
                    in_else = false;
                    else_body.clear();
                } else if let Some((condition, mut body)) = current_if.take() {
                    body.append(&mut if_body);
                    let else_stmt = if else_body.is_empty() {
                        None
                    } else {
                        Some(Box::new(Stmt::Block(else_body.clone())))
                    };
                    if current_function.is_some() {
                        function_body.push(Stmt::If(condition, body, else_stmt));
                        println!("Final If: {:?}", function_body);
                    } else if current_class.is_some() {
                        class_body.push(Stmt::If(condition, body, else_stmt));
                    } else {
                        stmts.push(Stmt::If(condition, body, else_stmt));
                    }
                    if_body.clear();
                    else_body.clear();
                } else if let Some(func_name) = current_function.take() {
                    class_body.push(Stmt::FunctionDecl(func_name, function_body.clone()));
                    function_body.clear();
                } else if let Some(class_name) = current_class.take() {
                    stmts.push(Stmt::ClassDecl(class_name, class_body.clone()));
                    class_body.clear();
                }
            }
            Stmt::If(condition, _, _) => {
                if current_if.is_some() {
                    return Err(ParseError {
                        message: "Nested if statements are not supported".to_string(),
                    });
                }
                current_if = Some((condition.clone(), vec![]));
                if_body = Vec::new();
            }
            _ => {
                if in_else {
                    else_body.push(stmt.clone());
                } else if current_if.is_some() {
                    if_body.push(stmt.clone());
                } else if current_function.is_some() {
                    function_body.push(stmt.clone());
                } else if current_class.is_some() {
                    class_body.push(stmt.clone());
                } else {
                    stmts.push(stmt.clone());
                }
            }
        }
    }

    if current_if.is_some() {
        return Err(ParseError {
            message: "Unclosed if block".to_string(),
        });
    }
    if current_function.is_some() || current_class.is_some() {
        return Err(ParseError {
            message: "Unclosed class or function block".to_string(),
        });
    }

    println!("Final statements: {:?}", stmts);
    Ok(stmts)
}