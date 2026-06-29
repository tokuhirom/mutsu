use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Parse an array literal [...].
pub(crate) fn array_literal(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '[')?;
    let (input, _) = ws(input)?;
    let mut items = Vec::new();
    if let Ok((input, _)) = parse_char(input, ']') {
        return Ok((input, Expr::BracketArray(items, false)));
    }
    // A top-level `;` sections an array composer just like a parenthesized list:
    // `[1,2;3,4]` is `[(1,2),(3,4)]`. `items` is the current section; completed
    // sections move into `sections`.
    let mut sections: Vec<Vec<Expr>> = Vec::new();
    let mut saw_semicolon = false;
    let (mut rest, first) = expression(input)?;
    items.push(first);
    loop {
        let (r, _) = ws(rest)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((
                    r,
                    finalize_array_sections(sections, items, saw_semicolon, true),
                ));
            }
            // After a separator we need another element or a closing `]`.
            // Hitting unparseable/end-of-input here means the array composer
            // was never closed (e.g. `[1,`): X::Comp::FailGoal.
            let (r, next) = expression(r).map_err(|_| array_fail_goal(r))?;
            items.push(next);
            rest = r;
        } else if r.starts_with(';') && !r.starts_with(";;") {
            // Semicolon section separator.
            let (r, _) = parse_char(r, ';')?;
            let (r, _) = ws(r)?;
            saw_semicolon = true;
            sections.push(std::mem::take(&mut items));
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((
                    r,
                    finalize_array_sections(sections, items, saw_semicolon, false),
                ));
            }
            let (r, next) = expression(r).map_err(|_| array_fail_goal(r))?;
            items.push(next);
            rest = r;
        } else {
            let (r, _) = ws(r)?;
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((
                    r,
                    finalize_array_sections(sections, items, saw_semicolon, false),
                ));
            }
            // Neither a separator nor the closing bracket: if another term
            // follows, this is "Two terms in a row" (X::Syntax::Confused),
            // e.g. `["a" "b"]`.
            if expression(r).is_ok() {
                return Err(two_terms_confused());
            }
            // We have parsed a valid array composer but cannot find the
            // closing `]` (e.g. `[1,2` at end of input): X::Comp::FailGoal.
            let (r, _) = parse_char(r, ']').map_err(|_| array_fail_goal(r))?;
            return Ok((
                r,
                finalize_array_sections(sections, items, saw_semicolon, false),
            ));
        }
    }
}

/// Combine semicolon-separated sections of an array composer. With no semicolon
/// the array is flat. Otherwise each non-empty section becomes one element: a
/// multi-item section is a sub-list, a single-item section is the bare item. A
/// single overall section (e.g. a trailing `;`: `[1,2,3;]`) stays flat.
fn finalize_array_sections(
    mut sections: Vec<Vec<Expr>>,
    current: Vec<Expr>,
    saw_semicolon: bool,
    trailing_comma: bool,
) -> Expr {
    if !saw_semicolon {
        return Expr::BracketArray(merge_sequence_seeds(current), trailing_comma);
    }
    if !current.is_empty() {
        sections.push(current);
    }
    if sections.len() <= 1 {
        let flat = sections.into_iter().next().unwrap_or_default();
        return Expr::BracketArray(merge_sequence_seeds(flat), false);
    }
    let elems = sections.into_iter().map(build_array_section).collect();
    Expr::BracketArray(elems, false)
}

/// Render one array section: a single item stays bare; multiple items form a
/// sub-list (`ArrayLiteral`, so they render as `(a, b)`).
fn build_array_section(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        items.into_iter().next().unwrap()
    } else {
        Expr::ArrayLiteral(merge_sequence_seeds(items))
    }
}

/// Build a fatal `X::Comp::FailGoal` parse error for an unterminated array
/// composer (`[1,2` with no closing `]`).
fn array_fail_goal(pos: &str) -> PError {
    fail_goal_error_at("array composer", "']'", Some(pos))
}

/// Build a fatal `X::Comp::FailGoal` parse error: an opening bracket/quote
/// construct (`dba`) was started but its terminator (`goal`) was never found.
/// `pos` (the parser slice where the terminator search gave up) sets the
/// exception's `.line` to the 1-based line of that position within the original
/// source — matching Rakudo, whose FailGoal `.line` points at where parsing
/// stopped (the EOF line), not the starter. Pass `None` to leave `.line` unset.
pub(crate) fn fail_goal_error_at(dba: &str, goal: &str, pos: Option<&str>) -> PError {
    let message = format!(
        "Unable to parse expression in {}; couldn't find final {} (corresponding starter was at line 1)",
        dba, goal
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("dba".to_string(), Value::str(dba.to_string()));
    attrs.insert("goal".to_string(), Value::str(goal.to_string()));
    if let Some(pos) = pos {
        let line = crate::parser::primary::current_line_number(pos);
        attrs.insert("line".to_string(), Value::Int(line));
    }
    let exception = Value::make_instance(Symbol::intern("X::Comp::FailGoal"), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

/// Build a fatal `X::Syntax::Confused` parse error with reason
/// "Two terms in a row".
fn two_terms_confused() -> PError {
    let reason = "Two terms in a row";
    let message = format!("Confused: {}", reason);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("reason".to_string(), Value::str(reason.to_string()));
    let exception = Value::make_instance(Symbol::intern("X::Syntax::Confused"), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

fn merge_sequence_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let last = items.last().unwrap();
    if let Expr::Binary { left, op, right } = last
        && matches!(
            op,
            crate::token_kind::TokenKind::DotDotDot | crate::token_kind::TokenKind::DotDotDotCaret
        )
    {
        let mut seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
        seeds.push(*left.clone());
        let merged = Expr::Binary {
            left: Box::new(Expr::ArrayLiteral(seeds)),
            op: op.clone(),
            right: right.clone(),
        };
        vec![merged]
    } else {
        items
    }
}

/// Parse a hash constructor literal: %(key => value, :name, ...)
pub(crate) fn percent_hash_literal(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '%')?;
    let (input, _) = parse_char(input, '(')?;
    let (mut rest, _) = ws(input)?;
    let mut pairs = Vec::new();

    if let Ok((rest_after, _)) = parse_char(rest, ')') {
        return Ok((rest_after, Expr::Hash(pairs)));
    }

    loop {
        let (r, item) = expression(rest)?;
        let (key, value) = match item {
            Expr::Binary {
                left,
                op: TokenKind::FatArrow,
                right,
            } => {
                let key = match *left {
                    Expr::Literal(Value::Str(s)) => s.to_string(),
                    Expr::Literal(Value::Int(i)) => i.to_string(),
                    _ => return Err(PError::expected("hash pair key")),
                };
                (key, Some(*right))
            }
            _ => return Err(PError::expected("hash pair")),
        };
        pairs.push((key, value));

        let (r, _) = ws(r)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r_after, _)) = parse_char(r, ')') {
                return Ok((r_after, Expr::Hash(pairs)));
            }
            rest = r;
            continue;
        }
        // Newline-separated colonpairs without commas: treat as separate
        // statements (like Raku), keeping only the last entry.
        if r.starts_with(':') {
            pairs.clear();
            rest = r;
            continue;
        }
        let (r, _) = parse_char(r, ')')?;
        return Ok((r, Expr::Hash(pairs)));
    }
}
