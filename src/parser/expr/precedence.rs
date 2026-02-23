use super::super::helpers::{is_ident_char, ws};
use super::super::parse_result::{PError, PResult, merge_expected_messages, parse_tag};

use crate::ast::Expr;
use crate::token_kind::TokenKind;

use super::operators::*;
use super::postfix::prefix_expr;
use super::{contains_whatever, wrap_whatevercode};

/// Ternary: expr ?? expr !! expr
pub(super) fn ternary(input: &str) -> PResult<'_, Expr> {
    ternary_mode(input, ExprMode::Full)
}

pub(super) fn ternary_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, cond) = or_expr_mode(input, mode)?;
    let (rest_ws, _) = ws(rest)?;
    if let Ok((input, _)) = parse_tag(rest_ws, "??") {
        let (input, _) = ws(input)?;
        // Parse then-expr at ternary precedence (stops before `!!`)
        let (input, then_expr) = if mode == ExprMode::Full {
            ternary_mode(input, mode).map_err(|err| {
                enrich_expected_error(err, "expected then-expression after '??'", input.len())
            })?
        } else {
            ternary_mode(input, mode)?
        };
        let (input, _) = ws(input)?;
        let (input, _) = if mode == ExprMode::Full {
            parse_tag(input, "!!").map_err(|err| {
                enrich_expected_error(err, "expected '!!' in ternary expression", input.len())
            })?
        } else {
            parse_tag(input, "!!")?
        };
        let (input, _) = ws(input)?;
        // Parse else-expr at ternary precedence (allows nested ternary)
        let (input, else_expr) = if mode == ExprMode::Full {
            ternary_mode(input, mode).map_err(|err| {
                enrich_expected_error(err, "expected else-expression after '!!'", input.len())
            })?
        } else {
            ternary_mode(input, mode)?
        };
        return Ok((
            input,
            Expr::Ternary {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
        ));
    }
    Ok((rest, cond))
}

/// Low-precedence: or / orelse
pub(super) fn or_expr(input: &str) -> PResult<'_, Expr> {
    or_expr_mode(input, ExprMode::Full)
}

fn or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = assign_or_and_expr(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::Or | LogicalOp::OrElse), len)) = parse_word_logical_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                assign_or_and_expr(r, mode).map_err(|err| {
                    enrich_expected_error(err, "expected expression after 'or'/'orelse'", r.len())
                })?
            } else {
                assign_or_and_expr(r, mode)?
            };
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

/// Assignment expressions at the or/and level: $var = expr
/// This sits between or/and and not in precedence.
fn assign_or_and_expr(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    // Try to parse: $var = expr (simple variable assignment as expression)
    if let Ok((rest, expr)) = and_expr_mode(input, mode) {
        let (r, _) = ws(rest)?;
        if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
            // Check if LHS is a variable
            if let Expr::Var(name) = &expr {
                let r = &r[1..];
                let (r, _) = ws(r)?;
                let (r, rhs) = and_expr_mode(r, mode)?;
                return Ok((
                    r,
                    Expr::AssignExpr {
                        name: name.clone(),
                        expr: Box::new(rhs),
                    },
                ));
            }
        }
        return Ok((rest, expr));
    }
    and_expr_mode(input, mode)
}

/// Low-precedence: and / andthen / notandthen
fn and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = not_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                not_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after 'and'/'andthen'/'notandthen'",
                        r.len(),
                    )
                })?
            } else {
                not_expr_mode(r, mode)?
            };
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

fn not_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    if input.starts_with("not")
        && !is_ident_char(input.as_bytes().get(3).copied())
        && !input[3..].starts_with('(')
    {
        let r = &input[3..];
        let (r, _) = ws(r)?;
        let (r, expr) = not_expr_mode(r, mode)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    or_or_expr_mode(input, mode)
}

/// || and //
fn or_or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_and_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_or_or_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                and_and_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after logical operator",
                        r.len(),
                    )
                })?
            } else {
                and_and_expr_mode(r, mode)?
            };
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

/// &&
fn and_and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = comparison_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_and_and_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                comparison_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(err, "expected expression after '&&'", r.len())
                })?
            } else {
                comparison_expr_mode(r, mode)?
            };
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

/// Boolean bitwise / junction: ?| ?& ?^ | & ^
fn junctive_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let next_fn = if mode == ExprMode::Full {
        sequence_expr
    } else {
        range_expr
    };
    let (mut rest, mut left) = next_fn(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_junctive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                next_fn(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after junctive operator",
                        r.len(),
                    )
                })?
            } else {
                next_fn(r)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Junction infix operators: |, &, ^
        if let Some((op, len)) = parse_junction_infix_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                next_fn(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after junction operator",
                        r.len(),
                    )
                })?
            } else {
                next_fn(r)?
            };
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

/// Sequence: ... and ...^
fn sequence_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, mut left) = range_expr(input)?;
    let (r, _) = ws(rest)?;

    // Helper: wrap LHS WhateverCode before building the sequence node.
    // E.g. `*+* ... *` → the `*+*` should be a WhateverCode generator.
    fn maybe_wrap_lhs(left: &mut Expr) {
        if contains_whatever(left) && !matches!(left, Expr::Whatever) {
            *left = wrap_whatevercode(left);
        }
    }

    if let Some(r) = r.strip_prefix("...^") {
        let (r, _) = ws(r)?;
        let (r, right) = range_expr(r).map_err(|err| {
            enrich_expected_error(err, "expected expression after '...^'", r.len())
        })?;
        maybe_wrap_lhs(&mut left);
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if r.starts_with("...") && !r.starts_with("....") {
        let r = &r[3..];
        let (r, _) = ws(r)?;
        let (r, right) = range_expr(r).map_err(|err| {
            enrich_expected_error(err, "expected expression after '...'", r.len())
        })?;
        maybe_wrap_lhs(&mut left);
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotDot,
                right: Box::new(right),
            },
        ));
    }
    Ok((rest, left))
}

/// Comparison: ==, !=, <, >, <=, >=, eq, ne, lt, gt, le, ge, ~~, !~~, ===, <=>
fn comparison_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, left) = junctive_expr_mode(input, mode)?;
    let (r, _) = ws(rest)?;
    // Detect Perl 5 =~ and !~ brainos (only when followed by space or m/)
    if r.starts_with("=~") && !r.starts_with("=~=") && !r.starts_with("=:=") {
        let after = &r[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::expected_at(
                "Unsupported use of =~ to do pattern matching; in Raku please use ~~",
                r,
            ));
        }
    }
    if r.starts_with("!~") && !r.starts_with("!~~") {
        let after = &r[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::expected_at(
                "Unsupported use of !~ to do pattern matching; in Raku please use !~~",
                r,
            ));
        }
    }
    if let Some((op, len)) = parse_comparison_op(r) {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, mut right) = if mode == ExprMode::Full {
            junctive_expr_mode(r, mode).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected expression after comparison operator",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?
        } else {
            junctive_expr_mode(r, mode)?
        };
        if mode == ExprMode::Full {
            // For smartmatch (~~ / !~~), transform WhateverCode on the RHS into a Lambda
            if matches!(op, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch)
                && contains_whatever(&right)
            {
                right = wrap_whatevercode(&right);
            }
            let mut result = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right.clone()),
            };
            // Chained comparisons: 2 < $_ < 4 → (2 < $_) && ($_ < 4)
            let mut prev_right = right;
            let mut r = r;
            loop {
                let (r2, _) = ws(r)?;
                if let Some((cop, chain_len)) = parse_comparison_op(r2) {
                    let r2 = &r2[chain_len..];
                    let (r2, _) = ws(r2)?;
                    let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                        messages: merge_expected_messages(
                            "expected expression after chained comparison operator",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r2.len())),
                    })?;
                    let next_cmp = Expr::Binary {
                        left: Box::new(prev_right),
                        op: cop.token_kind(),
                        right: Box::new(next_right.clone()),
                    };
                    result = Expr::Binary {
                        left: Box::new(result),
                        op: TokenKind::AndAnd,
                        right: Box::new(next_cmp),
                    };
                    prev_right = next_right;
                    r = r2;
                } else {
                    break;
                }
            }
            return Ok((r, result));
        } else {
            return Ok((
                r,
                Expr::Binary {
                    left: Box::new(left),
                    op: op.token_kind(),
                    right: Box::new(right),
                },
            ));
        }
    }

    Ok((rest, left))
}

/// Extract a comparison operator from the start of the input, returning the op and its length.
fn parse_comparison_op(r: &str) -> Option<(ComparisonOp, usize)> {
    // Unicode comparison operators
    if r.starts_with('\u{2A75}') {
        // ⩵ (U+2A75) — numeric equality (alias for ==)
        return Some((ComparisonOp::NumEq, '\u{2A75}'.len_utf8()));
    } else if r.starts_with('\u{2A76}') {
        // ⩶ (U+2A76) — value identity (alias for ===)
        return Some((ComparisonOp::StrictEq, '\u{2A76}'.len_utf8()));
    } else if r.starts_with('\u{2260}') {
        // ≠ (U+2260) — numeric inequality (alias for !=)
        return Some((ComparisonOp::NumNe, '\u{2260}'.len_utf8()));
    } else if r.starts_with('\u{2264}') {
        // ≤ (U+2264) — numeric less-than-or-equal (alias for <=)
        return Some((ComparisonOp::NumLe, '\u{2264}'.len_utf8()));
    } else if r.starts_with('\u{2265}') {
        // ≥ (U+2265) — numeric greater-than-or-equal (alias for >=)
        return Some((ComparisonOp::NumGe, '\u{2265}'.len_utf8()));
    }
    // ≅ (U+2245) — approximately equal
    if r.starts_with('\u{2245}') {
        return Some((ComparisonOp::ApproxEq, '\u{2245}'.len_utf8()));
    }
    if r.starts_with("=~=") {
        return Some((ComparisonOp::ApproxEq, 3));
    }
    if r.starts_with("=:=") {
        return Some((ComparisonOp::ContainerEq, 3));
    }
    if r.starts_with("===") {
        Some((ComparisonOp::StrictEq, 3))
    } else if r.starts_with("==") && !r.starts_with("===") {
        Some((ComparisonOp::NumEq, 2))
    } else if r.starts_with("!%%") {
        Some((ComparisonOp::NotDivisibleBy, 3))
    } else if r.starts_with("!=") {
        Some((ComparisonOp::NumNe, 2))
    } else if r.starts_with("!~~") {
        Some((ComparisonOp::SmartNotMatch, 3))
    } else if r.starts_with("~~") {
        Some((ComparisonOp::SmartMatch, 2))
    } else if r.starts_with("<=>") {
        Some((ComparisonOp::Spaceship, 3))
    } else if r.starts_with("<=") && !r.starts_with("<=>") {
        Some((ComparisonOp::NumLe, 2))
    } else if r.starts_with(">=") {
        Some((ComparisonOp::NumGe, 2))
    } else if r.starts_with('<') && !r.starts_with("<<") && !r.starts_with("<=") {
        Some((ComparisonOp::NumLt, 1))
    } else if r.starts_with('>') && !r.starts_with(">>") && !r.starts_with(">=") {
        Some((ComparisonOp::NumGt, 1))
    } else if r.starts_with("eq") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrEq, 2))
    } else if r.starts_with("ne") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrNe, 2))
    } else if r.starts_with("lt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLt, 2))
    } else if r.starts_with("gt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGt, 2))
    } else if r.starts_with("le") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLe, 2))
    } else if r.starts_with("ge") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGe, 2))
    } else if r.starts_with("leg") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Leg, 3))
    } else if r.starts_with("cmp") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Cmp, 3))
    } else if r.starts_with("eqv") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Eqv, 3))
    } else if r.starts_with("before") && !is_ident_char(r.as_bytes().get(6).copied()) {
        Some((ComparisonOp::Before, 6))
    } else if r.starts_with("after") && !is_ident_char(r.as_bytes().get(5).copied()) {
        Some((ComparisonOp::After, 5))
    } else {
        None
    }
}

/// Range: ..  ..^  ^..  ^..^
pub(super) fn range_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = structural_expr(input)?;
    let (r, _) = ws(rest)?;

    // Helper: wrap WhateverCode expressions in range endpoints.
    // `*-2` should become a WhateverCode lambda, but bare `*` should stay as Whatever.
    fn maybe_wrap_range_endpoint(expr: Expr) -> Expr {
        if contains_whatever(&expr) && !matches!(&expr, Expr::Whatever) {
            wrap_whatevercode(&expr)
        } else {
            expr
        }
    }

    if let Some(stripped) = r.strip_prefix("^..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r).map_err(|err| {
            enrich_expected_error(err, "expected range RHS after '^..^'", r.len())
        })?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDotCaret,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("^..") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '^..'", r.len()))?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDot,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..^'", r.len()))?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotCaret,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    if r.starts_with("..") && !r.starts_with("...") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..'", r.len()))?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDot,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    Ok((rest, left))
}

/// Parse infixed function call: [&func], R[&func], X[&func], Z[&func]
/// Returns (modifier, func_name, total_consumed_len)
fn parse_infix_func_op(input: &str) -> Option<(Option<String>, String, usize)> {
    let (modifier, bracket_start) = if input.starts_with("R[&") {
        (Some("R".to_string()), 1)
    } else if input.starts_with("X[&") {
        (Some("X".to_string()), 1)
    } else if input.starts_with("Z[&") {
        (Some("Z".to_string()), 1)
    } else if input.starts_with("[&") {
        (None, 0)
    } else {
        return None;
    };
    let r = &input[bracket_start + 2..]; // skip "[&"
    let end = r.find(']')?;
    let name = &r[..end];
    if name.is_empty()
        || !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return None;
    }
    let total_len = bracket_start + 2 + end + 1; // prefix + "[&" + name + "]"
    Some((modifier, name.to_string(), total_len))
}

/// Parse meta operator: R-, X+, Zcmp, etc.
fn parse_meta_op(input: &str) -> Option<(&str, &str, usize)> {
    let meta = if input.starts_with('R') {
        "R"
    } else if input.starts_with('X') {
        "X"
    } else if input.starts_with('Z') {
        "Z"
    } else {
        return None;
    };
    let r = &input[1..];
    // Try symbolic operators first (multi-char then single-char)
    let ops: &[&str] = &[
        "**", "==", "!=", "<=", ">=", "~~", "%%", "//", "~", "+", "-", "*", "/", "%", "<", ">",
    ];
    for op in ops {
        if r.starts_with(op) {
            return Some((meta, op, 1 + op.len()));
        }
    }
    // Try word operators: cmp, min, max, eq, ne, lt, gt, le, ge, leg
    let word_ops: &[&str] = &[
        "cmp", "min", "max", "eq", "ne", "lt", "gt", "le", "ge", "leg",
    ];
    for op in word_ops {
        if r.starts_with(op) && !is_ident_char(r.as_bytes().get(op.len()).copied()) {
            return Some((meta, op, 1 + op.len()));
        }
    }
    // Bare Z (zip with comma) or bare X (cross product) — followed by non-ident, non-operator char
    if (meta == "Z" || meta == "X") && !is_ident_char(r.as_bytes().first().copied()) {
        return Some((meta, "", 1));
    }
    None
}

fn parse_set_op(input: &str) -> Option<(TokenKind, usize)> {
    if input.starts_with("(|)") {
        Some((TokenKind::SetUnion, 3))
    } else if input.starts_with("(&)") {
        Some((TokenKind::SetIntersect, 3))
    } else if input.starts_with("(-)") {
        Some((TokenKind::SetDiff, 3))
    } else if input.starts_with("(^)") {
        Some((TokenKind::SetSymDiff, 3))
    } else if input.starts_with("(<=)") {
        Some((TokenKind::SetSubset, 4))
    } else if input.starts_with("(>=)") {
        Some((TokenKind::SetSuperset, 4))
    } else if input.starts_with("(<)") {
        Some((TokenKind::SetStrictSubset, 3))
    } else if input.starts_with("(>)") {
        Some((TokenKind::SetStrictSuperset, 3))
    } else if input.starts_with("(elem)") {
        Some((TokenKind::SetElem, 6))
    } else if input.starts_with("(cont)") {
        Some((TokenKind::SetCont, 6))
    } else {
        None
    }
}

/// Structural infix: but, does, set operators
fn structural_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = concat_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with("but") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'but'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("but".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("does") && !is_ident_char(r.as_bytes().get(4).copied()) {
            let r = &r[4..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'does'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("does".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // S-metaop variants used as infix operators (e.g. S&)
        if r.starts_with("S&") && !is_ident_char(r.as_bytes().get(2).copied()) {
            let r = &r[2..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'S&'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("S&".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Set operators: (|), (&), (-), (^), (<=), (>=), (<), (>), (elem), (cont)
        if let Some((tok, len)) = parse_set_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after set operator", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: tok,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Infixed function call: [&func], R[&func], X[&func], Z[&func]
        if let Some((modifier, name, len)) = parse_infix_func_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            // For X[&func], consume comma-separated list on right
            let (r, right_exprs) = if modifier.as_deref() == Some("X") {
                let (r, first) = range_expr(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after infixed function",
                        r.len(),
                    )
                })?;
                let mut items = vec![first];
                let mut r = r;
                loop {
                    let (r2, _) = ws(r)?;
                    if !r2.starts_with(',') || r2.starts_with(",,") {
                        break;
                    }
                    let r2 = &r2[1..];
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(';')
                        || r2.is_empty()
                        || r2.starts_with('}')
                        || r2.starts_with(')')
                    {
                        break;
                    }
                    if let Ok((r3, next)) = range_expr(r2) {
                        items.push(next);
                        r = r3;
                    } else {
                        break;
                    }
                }
                (r, items)
            } else {
                let (r, expr) = concat_expr(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after infixed function",
                        r.len(),
                    )
                })?;
                (r, vec![expr])
            };
            left = Expr::InfixFunc {
                name,
                left: Box::new(left),
                right: right_exprs,
                modifier,
            };
            rest = r;
            continue;
        }
        // Meta operators: R-, X+, Zcmp, etc.
        if let Some((meta, op, len)) = parse_meta_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            // For bare Z and X (list infix), consume comma-separated list on right
            // In Raku, Z and X have lower precedence than comma
            let (r, right) = if (meta == "Z" || meta == "X") && op.is_empty() {
                let (r, first) = range_expr(r).map_err(|err| {
                    enrich_expected_error(err, "expected expression after meta operator", r.len())
                })?;
                let mut items = vec![first];
                let mut r = r;
                loop {
                    let (r2, _) = ws(r)?;
                    if !r2.starts_with(',') || r2.starts_with(",,") {
                        break;
                    }
                    let r2 = &r2[1..];
                    let (r2, _) = ws(r2)?;
                    // Stop at statement-ending tokens
                    if r2.starts_with(';')
                        || r2.is_empty()
                        || r2.starts_with('}')
                        || r2.starts_with(')')
                    {
                        break;
                    }
                    if let Ok((r3, next)) = range_expr(r2) {
                        items.push(next);
                        r = r3;
                    } else {
                        break;
                    }
                }
                if items.len() == 1 {
                    (r, items.into_iter().next().unwrap())
                } else {
                    (r, Expr::ArrayLiteral(items))
                }
            } else {
                range_expr(r).map_err(|err| {
                    enrich_expected_error(err, "expected expression after meta operator", r.len())
                })?
            };
            left = Expr::MetaOp {
                meta: meta.to_string(),
                op: op.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Parse hyper operator: >>op<<, >>op>>, <<op<<, <<op>>
fn parse_hyper_op(input: &str) -> Option<(String, bool, bool, usize)> {
    // >>op<< or >>op>>
    if let Some(r) = input.strip_prefix(">>") {
        let search_limit = r.len().min(8);
        let search = &r[..search_limit];
        let op_end = search.find("<<").or_else(|| search.find(">>"));
        if let Some(end) = op_end {
            let op = &r[..end];
            if !op.is_empty() {
                let right_marker = &r[end..end + 2];
                let dwim_left = false;
                let dwim_right = right_marker == ">>";
                return Some((op.to_string(), dwim_left, dwim_right, 2 + end + 2));
            }
        }
    }
    // <<op<< or <<op>>
    if let Some(r) = input.strip_prefix("<<") {
        let search_limit = r.len().min(8);
        let search = &r[..search_limit];
        let op_end = search.find("<<").or_else(|| search.find(">>"));
        if let Some(end) = op_end {
            let op = &r[..end];
            if !op.is_empty() {
                let right_marker = &r[end..end + 2];
                let dwim_left = true;
                let dwim_right = right_marker == ">>";
                return Some((op.to_string(), dwim_left, dwim_right, 2 + end + 2));
            }
        }
    }
    None
}

/// String concatenation: ~
fn concat_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = additive_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        // Hyper operators: >>op<<, >>op>>, <<op<<, <<op>>
        if let Some((op, dwim_left, dwim_right, len)) = parse_hyper_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = additive_expr(r).map_err(|err| {
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
        if let Some((op, len)) = parse_concat_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = additive_expr(r).map_err(|err| {
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

/// Addition/subtraction: + -
fn additive_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = multiplicative_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_additive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after additive operator", r.len())
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

/// Multiplication/division: * / % div mod gcd lcm
fn multiplicative_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = power_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_multiplicative_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after multiplicative operator",
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

/// Exponentiation: **
pub(super) fn power_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, base) = prefix_expr(input)?;
    let (r, _) = ws(rest)?;
    if let Some(stripped) = r.strip_prefix("**") {
        let (r, _) = ws(stripped)?;
        let (r, exp) = power_expr(r).map_err(|err| {
            enrich_expected_error(err, "expected exponent expression after '**'", r.len())
        })?; // right-associative
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(base),
                op: TokenKind::StarStar,
                right: Box::new(exp),
            },
        ));
    }
    Ok((rest, base))
}
