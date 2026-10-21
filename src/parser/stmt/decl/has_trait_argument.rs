//! The argument forms of custom attribute traits.

use crate::ast::{Expr, spelled::Spelling};
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::PResult;
use crate::value::Value;

pub(super) fn parse(input: &str) -> PResult<'_, Option<Expr>> {
    let bracket_adjacent = input.starts_with('[');
    let (input, _) = ws(input)?;
    if bracket_adjacent {
        let (rest, array) = crate::parser::primary::container::array_literal(input)?;
        return Ok((
            rest,
            Some(Expr::spelled(array, || Spelling::BracketTraitArgument)),
        ));
    }
    if let Some(stripped) = input.strip_prefix('(') {
        let mut depth = 1u32;
        let mut end = 0;
        for (i, ch) in stripped.char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        end = i;
                        break;
                    }
                }
                _ => {}
            }
        }
        return Ok((&stripped[end + 1..], call_args(stripped[..end].trim())));
    }
    // A single quote word is a Str; multiple words are one List argument.
    if let Some(stripped) = input.strip_prefix('<')
        && let Some(close) = stripped.find('>')
    {
        let words: Vec<&str> = stripped[..close].split_whitespace().collect();
        let argument = match words.as_slice() {
            [] => None,
            [one] => Some(Expr::Literal(Value::str_from(one))),
            many => Some(Expr::ArrayLiteral(
                many.iter()
                    .map(|w| Expr::Literal(Value::str_from(w)))
                    .collect(),
            )),
        };
        return Ok((&stripped[close + 1..], argument));
    }
    if input.starts_with('{') {
        let (rest, argument) = crate::parser::primary::misc::block_or_hash_expr(input)?;
        return Ok((rest, Some(argument)));
    }
    Ok((input, None))
}

/// One positional argument stays scalar; a comma list is itemized as a List.
/// Preserve the existing conservative handling of unparseable parentheses.
fn call_args(inner: &str) -> Option<Expr> {
    let inner = inner.trim();
    if inner.is_empty() {
        return None;
    }
    let (leftover, first) = expression(inner).ok()?;
    let (leftover, _) = ws(leftover).ok()?;
    if leftover.is_empty() {
        return Some(first);
    }
    let mut items = vec![first];
    let mut rest = leftover;
    while let Some(after_comma) = rest.strip_prefix(',') {
        let (r, _) = ws(after_comma).ok()?;
        if r.is_empty() {
            rest = r;
            break;
        }
        let (r, expr) = expression(r).ok()?;
        let (r, _) = ws(r).ok()?;
        items.push(expr);
        rest = r;
    }
    rest.is_empty().then_some(Expr::ArrayLiteral(items))
}
