use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, multispace0};
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::parse_expresion::parse_expression;
use crate::parser::Stmt;

pub fn parse_assignment(input: &str, line: usize, column: usize) -> IResult<&str, Stmt> {
    context(
        "assignment",
        map(
            tuple((
                alphanumeric1,
                multispace0,
                tag("="),
                multispace0,
                |i| parse_expression(i, line, column),
                multispace0,
                tag(";"),
            )),
            |(var_name, _, _, _, value, _, _)| Stmt::Assignment(var_name.to_string(), value),
        ),
    )(input)
}