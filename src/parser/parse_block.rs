use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::map;
use nom::error::context;
use nom::IResult;
use nom::sequence::tuple;

pub fn parse_block(input: &str, _line: usize, _column: usize) -> IResult<&str, ()> {
    context(
        "block",
        map(
            tuple((multispace0, tag("{"), multispace0)),
            |_| (),
        ),
    )(input)
}