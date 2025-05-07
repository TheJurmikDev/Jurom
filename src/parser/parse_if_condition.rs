use nom::branch::alt;
use nom::error::context;
use nom::IResult;
use crate::parser::Expr;
use crate::parser::parse_boolean::parse_boolean;
use crate::parser::parse_comparison::parse_comparison;
use crate::parser::parse_variable::parse_variable;

pub fn parse_if_condition(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "if condition",
        alt((
            |i| parse_comparison(i, line, column),
            |i| parse_boolean(i, line, column),
            |i| parse_variable(i, line, column),
        )),
    )(input)
}