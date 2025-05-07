use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::number::complete::double;
use crate::parser::{Expr, Literal};

pub fn parse_number(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "number literal",
        map(double, |n: f64| Expr::Literal(Literal::Number(n), line, column)),
    )(input)
}