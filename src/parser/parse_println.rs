use nom::bytes::complete::tag;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::Expr;
use crate::parser::parse_expresion::parse_expression;

pub fn parse_println(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "println",
        map(
            tuple((
                tag("system.console.println("),
                |i| {
                    parse_expression(i, line, column)
                },
                tag(")"),
                opt(tag(";")),
            )),
            |(_, content, _, _)| Expr::FunctionCall {
                name: "println".to_string(),
                args: vec![content],
                line,
                column,
            },
        ),
    )(input)
}