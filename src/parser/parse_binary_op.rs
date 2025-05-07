use nom::character::complete::{multispace0, one_of};
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;
use crate::parser::Expr;
use crate::parser::parse_operand::parse_operand;

pub fn parse_binary_op(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "binary operation",
        map(
            tuple((
                |i| parse_operand(i, line, column),
                multispace0,
                one_of("-+*/"),
                multispace0,
                |i| parse_operand(i, line, column),
            )),
            |(left, _, op, _, right)| {
                Expr::BinaryOp(Box::new(left), op.to_string(), Box::new(right), line, column)
            },
        ),
    )(input)
}