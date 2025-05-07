use nom::bytes::complete::{tag, take_until};
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::Expr;

pub fn parse_function_call(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "function call",
        map(
            tuple((take_until("();"), tag("();"))),
            |(name, _): (&str, &str)| Expr::FunctionCall {
                name: name.trim().to_string(),
                args: vec![],
                line,
                column,
            },
        ),
    )(input)
}