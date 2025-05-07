use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::Expr;
use crate::parser::parse_block::parse_block;
use crate::parser::parse_condition_in_parens::parse_condition_in_parens;

pub fn parse_else_if(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "else if statement",
        map(
            tuple((
                tag("else if"),
                multispace0,
                |i| parse_condition_in_parens(i, line, column),
                multispace0,
                |i| parse_block(i, line, column),
            )),
            |(_, _, condition, _, _)| condition,
        ),
    )(input)
}