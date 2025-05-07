use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::Expr;
use crate::parser::parse_operand::parse_operand;

pub fn parse_comparison(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "comparison operation",
        map(
            tuple((
                |i| {
                    parse_operand(i, line, column)
                },
                multispace0,
                alt((tag("=="), tag("!="), tag("<="), tag(">="), tag("<"), tag(">"))),
                multispace0,
                |i| {
                    parse_operand(i, line, column)
                },
            )),
            |(left, _, op, _, right)| {
                Expr::Comparison(Box::new(left), op.to_string(), Box::new(right), line, column)
            },
        ),
    )(input)
}