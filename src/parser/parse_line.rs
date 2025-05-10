use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::{map, opt};
use nom::error::{context, Error, ErrorKind};
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::{Expr, Literal, Stmt};
use crate::parser::parse_assignment::parse_assignment;
use crate::parser::parse_else_if::parse_else_if;
use crate::parser::parse_function_call::parse_function_call;
use crate::parser::parse_if::parse_if;
use crate::parser::parse_println::parse_println;
use crate::parser::parse_variable_decl::parse_variable_decl;
use crate::parser::parse_while::parse_while;

pub fn parse_line<'a>(input: &'a str, line: usize, column: usize) -> IResult<&'a str, (Stmt, Option<(String, Expr)>)> {
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
            map(
                |i| parse_while(i, line, column),
                |stmt| (stmt, None),
            ),
            map(|i| parse_assignment(i, line, column), |stmt| (stmt, None)),
            map(|i| parse_variable_decl(i, line, column), |stmt| (stmt, None)),
            map(|i| parse_println(i, line, column), |expr| (Stmt::Expr(expr), None)),
            map(|i| parse_function_call(i, line, column), |expr| (Stmt::Expr(expr), None)),
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