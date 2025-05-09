use nom::{
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{map},
    error::context,
    sequence::{tuple},
    IResult,
};
use crate::parser::{Expr};
use crate::parser::parse_statement_body::parse_statement_body;
use crate::parser::parse_condition_in_parens::parse_condition_in_parens;

pub fn parse_if(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
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
                |i| parse_statement_body(i, line, column),
            )),
            |(_, _, condition, _, _)| condition,
        ),
    )(input)
}