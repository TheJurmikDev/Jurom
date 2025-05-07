use nom::branch::alt;
use nom::error::context;
use nom::IResult;
use crate::parser::Expr;
use crate::parser::parse_binary_op::parse_binary_op;
use crate::parser::parse_boolean::parse_boolean;
use crate::parser::parse_comparison::parse_comparison;
use crate::parser::parse_number::parse_number;
use crate::parser::parse_string::parse_string;
use crate::parser::parse_variable::parse_variable;

pub fn parse_expression(input: &str, line: usize, column: usize) -> IResult<&str, Expr> {
    context(
        "expression",
        alt((
            |i| parse_comparison(i, line, column),
            |i| parse_binary_op(i, line, column),
            |i| parse_string(i, line, column),
            |i| parse_number(i, line, column),
            |i| parse_boolean(i, line, column),
            |i| parse_variable(i, line, column),
        )),
    )(input)
}