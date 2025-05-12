use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    error::context,
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};
use crate::parser::Expr;
use crate::parser::parse_boolean::parse_boolean;
use crate::parser::parse_comparison::parse_comparison;
use crate::parser::parse_number::parse_number;
use crate::parser::parse_string::parse_string;
use crate::parser::parse_variable::parse_variable;

pub fn parse_expression(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context("expression", |input| parse_additive(input, line, column))(input)
}

fn parse_additive(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    let (input, mut expr) = parse_multiplicative(input, line, column)?;
    let (input, ops) = many0(tuple((
        multispace0,
        alt((tag("+"), tag("-"))),
        multispace0,
        |i| parse_multiplicative(i, line, column),
    )))(input)?;
    for (_, op, _, right) in ops {
        expr = Expr::BinaryOp(
            Box::new(expr),
            op.to_string(),
            Box::new(right),
            line,
            column,
        );
    }
    Ok((input, expr))
}

fn parse_multiplicative(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    let (input, mut expr) = parse_term(input, line, column)?;
    let (input, ops) = many0(tuple((
        multispace0,
        alt((tag("*"), tag("/"))),
        multispace0,
        |i| parse_term(i, line, column),
    )))(input)?;
    for (_, op, _, right) in ops {
        expr = Expr::BinaryOp(
            Box::new(expr),
            op.to_string(),
            Box::new(right),
            line,
            column,
        );
    }
    Ok((input, expr))
}

fn parse_term(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    alt((
        delimited(
            tag("("),
            delimited(multispace0, |i| parse_expression(i, line, column), multispace0),
            tag(")"),
        ),
        |i| parse_comparison(i, line, column),
        |i| parse_string(i, line, column),
        |i| parse_number(i, line, column),
        |i| parse_boolean(i, line, column),
        |i| parse_variable(i, line, column),
    ))(input)
}