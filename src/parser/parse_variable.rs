use nom::character::complete::alphanumeric1;
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use crate::parser::Expr;

pub fn parse_variable(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "variable",
        map(alphanumeric1, |name: &str| Expr::Variable(name.to_string(), line, column)),
    )(input)
}