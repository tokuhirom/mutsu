use crate::ast::Expr;
use crate::parser::expr::operators::{
    enrich_expected_error, parse_pure_concat_op, parse_replication_op,
};
use crate::parser::helpers::ws;
use crate::parser::parse_result::PResult;

use super::arith::{OpPrecedence, additive_expr, classify_base_op, try_custom_infix_at_level};
use super::meta_bracket::block_newline_terminates;

/// Parse hyper operator: >>op<<, >>op>>, <<op<<, <<op>>
/// Also supports Unicode variants: \u{00BB}op\u{00AB}, \u{00BB}op\u{00BB}, \u{00AB}op\u{00AB}, \u{00AB}op\u{00BB}
/// and mixed forms like >>op\u{00AB}, \u{00BB}op<<, etc.
fn parse_hyper_op(input: &str) -> Option<(String, bool, bool, usize)> {
    // Determine left delimiter and dwim_left
    let (dwim_left, left_len, after_left) = if let Some(r) = input.strip_prefix('\u{00BB}') {
        // \u{00BB} = >> (non-DWIM left)
        (false, '\u{00BB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix('\u{00AB}') {
        // \u{00AB} = << (DWIM left)
        (true, '\u{00AB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix(">>") {
        (false, 2, r)
    } else if let Some(r) = input.strip_prefix("<<") {
        (true, 2, r)
    } else {
        return None;
    };

    // Search for right delimiter within the operator string
    let mut search_limit = after_left.len().min(10);
    // Ensure we don't slice in the middle of a multi-byte UTF-8 character
    while search_limit > 0 && !after_left.is_char_boundary(search_limit) {
        search_limit -= 1;
    }
    let search = &after_left[..search_limit];

    // Find the earliest right delimiter (any of >>, <<, \u{00BB}, \u{00AB})
    let mut best: Option<(usize, bool, usize)> = None; // (byte_offset, dwim_right, marker_len)
    for (marker, dwim_right, marker_len) in [
        (">>", true, 2usize),
        ("<<", false, 2),
        ("\u{00BB}", true, '\u{00BB}'.len_utf8()),
        ("\u{00AB}", false, '\u{00AB}'.len_utf8()),
    ] {
        if let Some(pos) = search.find(marker)
            && pos > 0
            && (best.is_none() || pos < best.unwrap().0)
        {
            best = Some((pos, dwim_right, marker_len));
        }
    }

    if let Some((end, dwim_right, right_marker_len)) = best {
        let op = &after_left[..end];
        // Reject bare `=` as the operator — that is hyper-assignment syntax
        // (e.g. `»=»`), handled at the statement level.
        if op == "=" {
            return None;
        }
        return Some((
            op.to_string(),
            dwim_left,
            dwim_right,
            left_len + end + right_marker_len,
        ));
    }
    None
}

/// Parse hyper operator with function reference: >>[&func]<<, <<[&func]>>, etc.
/// Returns (func_name, dwim_left, dwim_right, total_consumed_length)
fn parse_hyper_func_op(input: &str) -> Option<(String, bool, bool, usize)> {
    // Determine left delimiter and dwim_left
    let (dwim_left, left_len, after_left) = if let Some(r) = input.strip_prefix('\u{00BB}') {
        (false, '\u{00BB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix('\u{00AB}') {
        (true, '\u{00AB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix(">>") {
        (false, 2, r)
    } else if let Some(r) = input.strip_prefix("<<") {
        (true, 2, r)
    } else {
        return None;
    };

    // Check for [&func] pattern (e.g. [&infix:<+>])
    if !after_left.starts_with("[&") {
        return None;
    }
    let bracket_content = &after_left[2..]; // skip "[&"
    let end = bracket_content.find(']')?;
    let name = &bracket_content[..end];
    // Allow alphanumeric, hyphens, underscores, and operator-style names like infix:<+>
    if name.is_empty()
        || !name.chars().all(|c| {
            c.is_alphanumeric()
                || matches!(
                    c,
                    '-' | '_'
                        | ':'
                        | '<'
                        | '>'
                        | '+'
                        | '*'
                        | '/'
                        | '~'
                        | '%'
                        | '!'
                        | '?'
                        | '|'
                        | '^'
                        | '='
                        | '.'
                        | '&'
                        | ','
                        | '\\'
                )
        })
    {
        return None;
    }
    let after_bracket = &after_left[2 + end + 1..]; // skip "[&name]"
    let bracket_len = 2 + end + 1;

    // Determine right delimiter and dwim_right
    let (dwim_right, right_len) = if let Some(_r) = after_bracket.strip_prefix('\u{00BB}') {
        (true, '\u{00BB}'.len_utf8())
    } else if let Some(_r) = after_bracket.strip_prefix('\u{00AB}') {
        (false, '\u{00AB}'.len_utf8())
    } else if after_bracket.starts_with(">>") {
        (true, 2)
    } else if after_bracket.starts_with("<<") {
        (false, 2)
    } else {
        return None;
    };

    let total_len = left_len + bracket_len + right_len;
    Some((name.to_string(), dwim_left, dwim_right, total_len))
}

/// Numeric precedence ordering for a hyper operator's base op.
/// Hyper operators inherit the precedence of the operator they are based on,
/// so `(1,2,3) »+« (10,20,30) »*« (2,3,4)` multiplies before adding.
fn hyper_op_prec(op: &str) -> i32 {
    match classify_base_op(op) {
        OpPrecedence::Multiplicative => 50,
        OpPrecedence::Additive => 40,
        OpPrecedence::Concatenation => 30,
        OpPrecedence::Comparison => 20,
        OpPrecedence::Other => 10,
    }
}

/// Parse the right-hand side of a hyper operator, folding in any following
/// hyper operators whose base precedence is *tighter* than `parent_prec`.
/// This makes hyper operators honour their base operator's precedence while
/// keeping equal-precedence chains left-associative (the outer `concat_expr`
/// loop combines those).
fn parse_hyper_rhs(input: &str, parent_prec: i32) -> PResult<'_, Expr> {
    let (mut rest, mut left) = replication_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        if let Some((op, dwim_left, dwim_right, len)) = parse_hyper_op(r) {
            let prec = hyper_op_prec(&op);
            if prec <= parent_prec {
                break;
            }
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = parse_hyper_rhs(r, prec).map_err(|err| {
                enrich_expected_error(err, "expected expression after hyper operator", r.len())
            })?;
            left = Expr::HyperOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                dwim_left,
                dwim_right,
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// String concatenation: ~
pub(crate) fn concat_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = replication_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        // Hyper operators with function reference: >>[&func]<<, <<[&func]>>, etc.
        if let Some((func_name, dwim_left, dwim_right, len)) = parse_hyper_func_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = replication_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after hyper function operator",
                    r.len(),
                )
            })?;
            left = Expr::HyperFuncOp {
                func_name,
                left: Box::new(left),
                right: Box::new(right),
                dwim_left,
                dwim_right,
            };
            rest = r;
            continue;
        }
        // Hyper operators: >>op<<, >>op>>, <<op<<, <<op>>
        if let Some((op, dwim_left, dwim_right, len)) = parse_hyper_op(r) {
            let parent_prec = hyper_op_prec(&op);
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = parse_hyper_rhs(r, parent_prec).map_err(|err| {
                enrich_expected_error(err, "expected expression after hyper operator", r.len())
            })?;
            left = Expr::HyperOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                dwim_left,
                dwim_right,
            };
            rest = r;
            continue;
        }
        // Custom infix ops between structural and additive levels
        // (covers is equiv<~>, is tighter<~>, is looser<+>)
        {
            use crate::parser::stmt::simple::{PREC_ADDITIVE, PREC_STRUCTURAL};
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut left,
                PREC_STRUCTURAL,
                PREC_ADDITIVE - 1,
                replication_expr,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some((op, len)) = parse_pure_concat_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = replication_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after concatenation operator",
                    r.len(),
                )
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Replication: x, xx, o (function composition)
/// These bind tighter than ~ (concatenation) but looser than + (additive).
pub(super) fn replication_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = additive_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        if let Some((op, len)) = parse_replication_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = additive_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after replication operator",
                    r.len(),
                )
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}
