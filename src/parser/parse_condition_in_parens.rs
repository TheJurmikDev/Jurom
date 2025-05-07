use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::error::context;
use nom::IResult;
use nom::sequence::{delimited, tuple};
use crate::parser::Expr;
use crate::parser::parse_if_condition::parse_if_condition;

pub fn parse_condition_in_parens(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "condition in parentheses",
        delimited(
            tuple((multispace0, tag("("), multispace0)),
            |i| parse_if_condition(i, line, column),
            tuple((multispace0, tag(")"), multispace0)),
        ),
    )(input)
}