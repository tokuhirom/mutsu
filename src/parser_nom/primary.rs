use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::char;
use nom::combinator::{map, opt, recognize};
use nom::sequence::{delimited, pair};

use crate::ast::Expr;
use crate::value::Value;

/// Parse an integer literal.
fn integer(input: &str) -> IResult<&str, Expr> {
    let (input, neg) = opt(char('-'))(input)?;
    let (input, digits) = take_while1(|c: char| c.is_ascii_digit())(input)?;
    let n: i64 = digits.parse().unwrap_or(0);
    let n = if neg.is_some() { -n } else { n };
    Ok((input, Expr::Literal(Value::Int(n))))
}

/// Parse a decimal number literal.
fn decimal(input: &str) -> IResult<&str, Expr> {
    let (input, neg) = opt(char('-'))(input)?;
    let (input, num_str) = recognize(pair(
        take_while1(|c: char| c.is_ascii_digit()),
        pair(char('.'), take_while1(|c: char| c.is_ascii_digit())),
    ))(input)?;
    let n: f64 = num_str.parse().unwrap_or(0.0);
    let n = if neg.is_some() { -n } else { n };
    Ok((input, Expr::Literal(Value::Num(n))))
}

/// Parse a single-quoted string literal.
fn single_quoted_string(input: &str) -> IResult<&str, Expr> {
    let (input, content) = delimited(
        char('\''),
        recognize(nom::multi::many0(alt((
            recognize(pair(char('\\'), nom::character::complete::anychar)),
            recognize(take_while1(|c: char| c != '\'' && c != '\\')),
        )))),
        char('\''),
    )(input)?;
    // Simple unescaping: \\ -> \, \' -> '
    let s = content.replace("\\'", "'").replace("\\\\", "\\");
    Ok((input, Expr::Literal(Value::Str(s))))
}

/// Parse a double-quoted string literal (no interpolation yet).
fn double_quoted_string(input: &str) -> IResult<&str, Expr> {
    let (input, content) = delimited(
        char('"'),
        recognize(nom::multi::many0(alt((
            recognize(pair(char('\\'), nom::character::complete::anychar)),
            recognize(take_while1(|c: char| c != '"' && c != '\\')),
        )))),
        char('"'),
    )(input)?;
    let s = content
        .replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\\\", "\\")
        .replace("\\\"", "\"");
    Ok((input, Expr::Literal(Value::Str(s))))
}

/// Parse a primary expression (literal value).
pub(super) fn primary(input: &str) -> IResult<&str, Expr> {
    alt((
        decimal,
        integer,
        single_quoted_string,
        double_quoted_string,
        map(tag("True"), |_| Expr::Literal(Value::Bool(true))),
        map(tag("False"), |_| Expr::Literal(Value::Bool(false))),
    ))(input)
}
