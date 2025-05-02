use std::fmt;
use std::hash::{Hash, Hasher};
use nom::{
    IResult,
    bytes::complete::{tag, take_until},
    sequence::{delimited, preceded},
    multi::separated_list0,
    character::complete::{alphanumeric1, multispace0, one_of},
    branch::alt,
    combinator::map,
    sequence::tuple,
    number::complete::double,
    error::context,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    FunctionCall(String, Vec<Expr>),
    Literal(Literal),
    Variable(String),
    BinaryOp(Box<Expr>, String, Box<Expr>),
    Comparison(Box<Expr>, String, Box<Expr>),
    ArrayAccess(Box<Expr>, Box<Expr>),
    Array(Vec<Expr>),
}

impl Eq for Expr {}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Expr::FunctionCall(name, args) => {
                name.hash(state);
                args.hash(state);
            }
            Expr::Literal(literal) => literal.hash(state),
            Expr::Variable(name) => name.hash(state),
            Expr::BinaryOp(left, op, right) => {
                left.hash(state);
                op.hash(state);
                right.hash(state);
            }
            Expr::Comparison(left, op, right) => {
                left.hash(state);
                op.hash(state);
                right.hash(state);
            }
            Expr::ArrayAccess(array, index) => {
                array.hash(state);
                index.hash(state);
            }
            Expr::Array(elements) => elements.hash(state),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::String(s) => s.hash(state),
            Literal::Boolean(b) => b.hash(state),
            Literal::Number(n) => {
                if n.is_nan() {
                    "NaN".hash(state);
                } else if n.is_infinite() {
                    if n.is_sign_positive() {
                        "Inf".hash(state);
                    } else {
                        "-Inf".hash(state);
                    }
                } else {
                    n.to_bits().hash(state);
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    ClassDecl(String, Vec<Stmt>),
    FunctionDecl(String, Vec<String>, Vec<Stmt>),
    VariableDecl(String, String, Expr),
    Expr(Expr),
    FunctionCall(String),
    If(Expr, Vec<Stmt>, Option<Box<Stmt>>),
    While(Expr, Vec<Stmt>),
    For(String, Expr, Vec<Stmt>),
    Return(Option<Expr>),
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error on line {}: {}", self.line, self.message)
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

fn parse_array(input: &str) -> IResult<&str, Expr> {
    context(
        "array literal",
        map(
            delimited(
                tag("["),
                separated_list0(tuple((multispace0, tag(","), multispace0)), parse_expression),
                tag("]"),
            ),
            Expr::Array,
        ),
    )(input)
}

fn parse_array_access(input: &str) -> IResult<&str, Expr> {
    context(
        "array access",
        map(
            tuple((
                parse_variable,
                delimited(tag("["), parse_expression, tag("]")),
            )),
            |(var, index)| Expr::ArrayAccess(Box::new(var), Box::new(index)),
        ),
    )(input)
}

fn parse_operand(input: &str) -> IResult<&str, Expr> {
    context(
        "operand",
        alt((
            map(parse_number, |n: f64| Expr::Literal(Literal::Number(n))),
            map(parse_boolean, |b: bool| Expr::Literal(Literal::Boolean(b))),
            parse_array,
            parse_array_access,
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
                one_of("+*/-"),
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
    context(
        "comparison operation",
        map(
            tuple((
                parse_operand,
                multispace0,
                alt((tag("=="), tag("<"), tag(">"), tag("<="), tag(">="), tag("!="))),
                multispace0,
                parse_operand,
            )),
            |(left, _, op, _, right)| {
                Expr::Comparison(Box::new(left), op.to_string(), Box::new(right))
            },
        ),
    )(input)
}

fn parse_expression(input: &str) -> IResult<&str, Expr> {
    context(
        "expression",
        alt((
            parse_comparison,
            parse_binary_op,
            map(parse_string, |s: &str| Expr::Literal(Literal::String(s.to_string()))),
            map(parse_number, |n| Expr::Literal(Literal::Number(n))), // Fixed
            map(parse_boolean, |b| Expr::Literal(Literal::Boolean(b))), // Fixed
            parse_array,
            parse_array_access,
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
                tag(");"),
            )),
            |(_, content, _)| Expr::FunctionCall("println".to_string(), vec![content]),
        ),
    )(input)
}

fn parse_variable_decl(input: &str) -> IResult<&str, Stmt> {
    context(
        "variable declaration",
        map(
            tuple((
                alt((tag("num"), tag("string"), tag("boolean"), tag("array"))),
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
            tuple((
                alphanumeric1,
                tag("("),
                separated_list0(tuple((multispace0, tag(","), multispace0)), parse_expression),
                tag(")"),
                tag(";"),
            )),
            |(name, _, args, _, _)| Expr::FunctionCall(name.to_string(), args),
        ),
    )(input)
}

fn parse_while(input: &str) -> IResult<&str, Stmt> {
    context(
        "while statement",
        map(
            tuple((
                tag("while"),
                multispace0,
                tag("("),
                parse_expression,
                tag(")"),
                multispace0,
                parse_block,
            )),
            |(_, _, _, condition, _, _, body)| Stmt::While(condition, body),
        ),
    )(input)
}

fn parse_for(input: &str) -> IResult<&str, Stmt> {
    context(
        "for statement",
        map(
            tuple((
                tag("for"),
                multispace0,
                tag("("),
                alphanumeric1,
                multispace0,
                tag("in"),
                multispace0,
                parse_expression,
                tag(")"),
                multispace0,
                parse_block,
            )),
            |(_, _, _, var_name, _, _, _, array_expr, _, _, body)| {
                Stmt::For(var_name.to_string(), array_expr, body)
            },
        ),
    )(input)
}

fn parse_return(input: &str) -> IResult<&str, Stmt> {
    context(
        "return statement",
        map(
            tuple((
                tag("return"),
                multispace0,
                nom::combinator::opt(parse_expression),
                tag(";"),
            )),
            |(_, _, expr, _)| Stmt::Return(expr),
        ),
    )(input)
}

fn parse_block(input: &str) -> IResult<&str, Vec<Stmt>> {
    context(
        "block",
        delimited(
            tuple((multispace0, tag("{"), multispace0)),
            nom::multi::many0(parse_line),
            tuple((multispace0, tag("}"), multispace0)),
        ),
    )(input)
}

fn parse_if(input: &str) -> IResult<&str, Stmt> {
    context(
        "if statement",
        map(
            tuple((
                tag("if"),
                multispace0,
                tag("("),
                parse_expression,
                tag(")"),
                multispace0,
                parse_block,
                multispace0,
                nom::combinator::opt(tuple((
                    tag("else"),
                    multispace0,
                    alt((
                        map(
                            tuple((
                                tag("if"),
                                multispace0,
                                tag("("),
                                parse_expression,
                                tag(")"),
                                multispace0,
                                parse_block,
                            )),
                            |(_, _, _, expr, _, _, body)| {
                                Stmt::If(expr, body, None)
                            },
                        ),
                        map(parse_block, |body| {
                            Stmt::If(Expr::Literal(Literal::Boolean(true)), body, None)
                        }),
                    )),
                ))),
            )),
            |(_, _, _, condition, _, _, if_body, _, else_branch)| {
                let else_stmt = else_branch.map(|(_, _, stmt)| Box::new(stmt));
                Stmt::If(condition, if_body, else_stmt)
            },
        ),
    )(input)
}

fn parse_function_decl(input: &str) -> IResult<&str, Stmt> {
    context(
        "function declaration",
        map(
            tuple((
                tag("function"),
                multispace0,
                alphanumeric1,
                tag("("),
                separated_list0(tag(","), preceded(multispace0, alphanumeric1)),
                tag(")"),
                multispace0,
                parse_block,
            )),
            |(_, _, name, _, params, _, _, body)| {
                let params = params.into_iter().map(|p| p.to_string()).collect();
                Stmt::FunctionDecl(name.to_string(), params, body)
            },
        ),
    )(input)
}

fn parse_line(line: &str) -> IResult<&str, Stmt> {
    context(
        "line",
        alt((
            parse_if,
            parse_while,
            parse_for,
            parse_return,
            parse_variable_decl,
            parse_function_decl,
            map(parse_println, Stmt::Expr),
            map(parse_function_call, Stmt::Expr),
            map(tag("}"), |_| {
                Stmt::Expr(Expr::Literal(Literal::String("END_BLOCK".to_string())))
            }),
            map(tag("main();"), |_| Stmt::FunctionCall("main".to_string())),
            map(|_| Ok(("", "")), |_| {
                Stmt::Expr(Expr::Literal(Literal::String("".to_string())))
            }),
        )),
    )(line.trim())
}

pub fn parse_program(code: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut stmts = Vec::new();
    let mut current_class: Option<String> = None;
    let mut current_function: Option<String> = None;
    let mut class_body: Vec<Stmt> = Vec::new();
    let mut function_body: Vec<Stmt> = Vec::new();
    let mut line_number = 0;

    for line in code.lines() {
        line_number += 1;
        let trimmed = line.trim();
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
                    line: line_number,
                });
            }
            current_class = Some(class_name);
            class_body = Vec::new();
            continue;
        } else if trimmed.starts_with("function") {
            let func_name = trimmed
                .replace("function", "")
                .replace("(", "")
                .replace(")", "")
                .replace("{", "")
                .trim()
                .to_string();
            if current_function.is_some() {
                return Err(ParseError {
                    message: "Nested functions are not supported".to_string(),
                    line: line_number,
                });
            }
            current_function = Some(func_name);
            function_body = Vec::new();
            continue;
        }

        let (_, stmt) = parse_line(trimmed).map_err(|e| ParseError {
            message: format!("Error parsing line '{}': {:?}", trimmed, e),
            line: line_number,
        })?;

        match &stmt {
            Stmt::Expr(Expr::Literal(Literal::String(s))) if s == "END_BLOCK" => {
                if let Some(func_name) = current_function.take() {
                    class_body.push(Stmt::FunctionDecl(func_name, vec![], function_body.clone()));
                    function_body.clear();
                } else if let Some(class_name) = current_class.take() {
                    stmts.push(Stmt::ClassDecl(class_name, class_body.clone()));
                    class_body.clear();
                }
            }
            _ => {
                if current_function.is_some() {
                    function_body.push(stmt.clone());
                } else if current_class.is_some() {
                    class_body.push(stmt.clone());
                } else {
                    stmts.push(stmt.clone());
                }
            }
        }
    }

    Ok(stmts)
}