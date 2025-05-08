use nom::branch::alt;
use nom::character::complete::{multispace0, one_of};
use nom::combinator::opt;
use nom::error::context;
use nom::sequence::tuple;
use nom::IResult;
use crate::parser::Expr;
use crate::parser::parse_boolean::parse_boolean;
use crate::parser::parse_comparison::parse_comparison;
use crate::parser::parse_number::parse_number;
use crate::parser::parse_string::parse_string;
use crate::parser::parse_variable::parse_variable;

pub fn parse_expression(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context("expression", |input| {
        let (input, left) = alt((
            |i| parse_comparison(i, line, column),
            |i| parse_string(i, line, column),
            |i| parse_number(i, line, column),
            |i| parse_boolean(i, line, column),
            |i| parse_variable(i, line, column),
        ))(input)?;

        parse_binary_op_tail(left, input, line, column)
    })(input)
}

fn parse_binary_op_tail(
    left: Expr,
    input: &str,
    line: usize,
    column: usize,
) -> IResult<&str, Expr> {
    let (input, opt_tail) = opt(tuple((
        multispace0,
        one_of("-+*/"),
        multispace0,
        |i| parse_expression(i, line, column),
    )))(input)?;

    match opt_tail {
        Some((_, op, _, right)) => {
            let new_left = Expr::BinaryOp(
                Box::new(left),
                op.to_string(),
                Box::new(right),
                line,
                column,
            );
            parse_binary_op_tail(new_left, input, line, column)
        }
        None => Ok((input, left)),
    }
}