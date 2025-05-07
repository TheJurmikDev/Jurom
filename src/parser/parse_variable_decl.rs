use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, multispace0};
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::parse_expresion::parse_expression;
use crate::parser::Stmt;

pub fn parse_variable_decl(input: &str, line: usize, column: usize) -> IResult<&str, Stmt> {
    context(
        "variable declaration",
        map(
            tuple((
                alt((tag("num"), tag("string"), tag("boolean"))),
                multispace0,
                alphanumeric1,
                multispace0,
                tag("="),
                multispace0,
                |i| parse_expression(i, line, column),
                multispace0,
                tag(";"),
            )),
            |(type_name, _, var_name, _, _, _, value, _, _)| {
                Stmt::VariableDecl(type_name.to_string(), var_name.to_string(), value)
            },
        ),
    )(input)
}