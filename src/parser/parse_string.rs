use nom::bytes::complete::{tag, take_until};
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::delimited;
use crate::parser::{Expr, Literal};

pub fn parse_string(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "string literal",
        map(
            delimited(tag("\""), take_until("\""), tag("\"")),
            |s: &str| Expr::Literal(Literal::String(s.to_string()), line, column),
        ),
    )(input)
}