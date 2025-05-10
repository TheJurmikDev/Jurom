use nom::{
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::map,
    error::context,
    sequence::tuple,
    IResult,
};
use crate::parser::Stmt;
use crate::parser::parse_condition_in_parens::parse_condition_in_parens;
use crate::parser::parse_statement_body::parse_statement_body;

pub fn parse_while(input: &str, line: usize, column: usize) -> IResult<&str, Stmt> {
    context(
        "while statement",
        map(
            tuple((
                tag("while"),
                multispace0,
                |i| parse_condition_in_parens(i, line, column),
                multispace0,
                |i| parse_statement_body(i, line, column),
            )),
            |(_, _, condition, _, _)| Stmt::While {
                condition,
                body: vec![],
            },
        ),
    )(input)
}