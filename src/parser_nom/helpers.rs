use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{multispace0, multispace1, not_line_ending};
use nom::combinator::value;
use nom::multi::many0;
use nom::sequence::preceded;

/// Skip whitespace and line comments (`#` to end of line).
pub(super) fn ws(input: &str) -> IResult<&str, ()> {
    let (input, _) = many0(alt((
        value((), multispace1),
        value((), preceded(tag("#"), not_line_ending)),
    )))(input)?;
    Ok((input, ()))
}

/// Skip optional whitespace (zero or more).
#[allow(dead_code)]
pub(super) fn ws0(input: &str) -> IResult<&str, ()> {
    let (input, _) = multispace0(input)?;
    Ok((input, ()))
}
