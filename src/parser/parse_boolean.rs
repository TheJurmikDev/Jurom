use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use crate::parser::{Expr, Literal};

pub fn parse_boolean(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "boolean literal",
        alt((
            map(tag("true"), |_| Expr::Literal(Literal::Boolean(true), line, column)),
            map(tag("false"), |_| Expr::Literal(Literal::Boolean(false), line, column)),
        )),
    )(input)
}