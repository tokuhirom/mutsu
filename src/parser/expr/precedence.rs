use super::super::helpers::{is_ident_char, ws};
use super::super::parse_result::{PError, PResult, merge_expected_messages, parse_tag};

use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::Value;

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
        if mode == ExprMode::Full
            && let Expr::BareWord(name) = &then_expr
            && !name.contains("::")
        {
            return Err(PError::expected("expected '!!' in ternary expression"));
        }
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
        if let Expr::Binary { left, op, right } = cond {
            if matches!(
                op,
                TokenKind::AndAnd
                    | TokenKind::AndThen
                    | TokenKind::NotAndThen
                    | TokenKind::OrWord
                    | TokenKind::OrElse
            ) {
                return Ok((
                    input,
                    Expr::Binary {
                        left,
                        op,
                        right: Box::new(Expr::Ternary {
                            cond: right,
                            then_expr: Box::new(then_expr),
                            else_expr: Box::new(else_expr),
                        }),
                    },
                ));
            }
            return Ok((
                input,
                Expr::Ternary {
                    cond: Box::new(Expr::Binary { left, op, right }),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                },
            ));
        }
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
            // Check if LHS is a subscripted variable: @a[1] = ..., %h{k} = ...
            if let Expr::Index { target, index } = expr {
                let r = &r[1..];
                let (r, _) = ws(r)?;
                let (r, rhs) = and_expr_mode(r, mode)?;
                return Ok((
                    r,
                    Expr::IndexAssign {
                        target,
                        index,
                        value: Box::new(rhs),
                    },
                ));
            }
            if let Expr::Call { name, args } = expr {
                let r = &r[1..];
                let (r, _) = ws(r)?;
                let (r, rhs) = and_expr_mode(r, mode)?;
                return Ok((
                    r,
                    Expr::Call {
                        name: "__mutsu_assign_named_sub_lvalue".to_string(),
                        args: vec![
                            Expr::Literal(Value::Str(name)),
                            Expr::ArrayLiteral(args),
                            rhs,
                        ],
                    },
                ));
            }
            if let Expr::CallOn { target, args } = expr {
                let r = &r[1..];
                let (r, _) = ws(r)?;
                let (r, rhs) = and_expr_mode(r, mode)?;
                return Ok((
                    r,
                    Expr::Call {
                        name: "__mutsu_assign_callable_lvalue".to_string(),
                        args: vec![*target, Expr::ArrayLiteral(args), rhs],
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

/// || , ^^ , and //
fn or_or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_and_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        // Check for negated logical ops first: !|| , !^^
        let (negated, op_result) = if let Some((op, len)) = parse_negated_logical_op(r) {
            if matches!(op, LogicalOp::OrOr | LogicalOp::XorXor) {
                (true, Some((op, len)))
            } else {
                (false, None)
            }
        } else {
            (false, None)
        };
        let (op, len) = if let Some(pair) = op_result {
            pair
        } else if let Some(pair) = parse_or_or_op(r) {
            pair
        } else {
            break;
        };
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
            if r.starts_with("not")
                && !is_ident_char(r.as_bytes().get(3).copied())
                && !r[3..].starts_with('(')
            {
                not_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after logical operator",
                        r.len(),
                    )
                })?
            } else {
                and_and_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after logical operator",
                        r.len(),
                    )
                })?
            }
        } else if r.starts_with("not")
            && !is_ident_char(r.as_bytes().get(3).copied())
            && !r[3..].starts_with('(')
        {
            not_expr_mode(r, mode)?
        } else {
            and_and_expr_mode(r, mode)?
        };
        let binary = Expr::Binary {
            left: Box::new(left),
            op: op.token_kind(),
            right: Box::new(right),
        };
        left = if negated {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(binary),
            }
        } else {
            binary
        };
        rest = r;
    }
    Ok((rest, left))
}

/// &&
fn and_and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = comparison_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        // Check for negated !&&
        let (negated, op, len) = if let Some((op, len)) = parse_negated_logical_op(r) {
            if matches!(op, LogicalOp::AndAnd) {
                (true, op, len)
            } else if let Some((op, len)) = parse_and_and_op(r) {
                (false, op, len)
            } else {
                break;
            }
        } else if let Some((op, len)) = parse_and_and_op(r) {
            (false, op, len)
        } else {
            break;
        };
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
            comparison_expr_mode(r, mode).map_err(|err| {
                enrich_expected_error(err, "expected expression after '&&'", r.len())
            })?
        } else {
            comparison_expr_mode(r, mode)?
        };
        let binary = Expr::Binary {
            left: Box::new(left),
            op: op.token_kind(),
            right: Box::new(right),
        };
        left = if negated {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(binary),
            }
        } else {
            binary
        };
        rest = r;
    }
    Ok((rest, left))
}

/// Boolean bitwise / junction: ?| ?& ?^ | & ^
fn junctive_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let next_fn = if mode == ExprMode::Full {
        sequence_expr
    } else {
        list_infix_expr
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

/// List-infix operators only: Z, X, meta-ops, infix funcs.
/// Used in NoSequence mode (e.g. inside parenthesized expressions).
fn list_infix_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = range_expr(input)?;
    rest = parse_list_infix_loop(rest, &mut left)?;
    Ok((rest, left))
}

/// Sequence (..., ...^) and list-infix (Z, X, meta-ops, infix funcs).
/// All have Raku precedence level f= (list associative).
fn sequence_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = range_expr(input)?;

    // Helper: wrap LHS WhateverCode before building the sequence node.
    fn maybe_wrap_lhs(left: &mut Expr) {
        if contains_whatever(left) && !matches!(left, Expr::Whatever) {
            *left = wrap_whatevercode(left);
        }
    }

    loop {
        let (r, _) = ws(rest)?;

        // ...^ / …^ (exclusive-end sequence), ... / … (sequence)
        if let Some((r2, op, op_str)) = strip_sequence_op(r) {
            let (r2, _) = ws(r2)?;
            let (r2, mut right) =
                comparison_expr_mode(r2, ExprMode::NoSequence).map_err(|err| {
                    enrich_expected_error(
                        err,
                        format!("expected expression after '{op_str}'").as_str(),
                        r2.len(),
                    )
                })?;
            if contains_whatever(&right) && !matches!(right, Expr::Whatever) {
                right = wrap_whatevercode(&right);
            }
            maybe_wrap_lhs(&mut left);
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            rest = r2;
            continue;
        }
        // Try Z/X/meta/infix-func operators
        let new_rest = parse_list_infix_loop(rest, &mut left)?;
        if new_rest.len() < rest.len() {
            rest = new_rest;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Shared loop for Z/X meta operators and infix function calls.
/// Modifies `left` in place and returns the remaining input.
fn parse_list_infix_loop<'a>(input: &'a str, left: &mut Expr) -> Result<&'a str, PError> {
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        // Infixed function call: [&func], R[&func], X[&func], Z[&func]
        if let Some((modifier, name, len)) = parse_infix_func_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right_exprs) = if modifier.as_deref() == Some("X") {
                parse_comma_list_of_range(r)?
            } else {
                let (r, expr) = range_expr(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after infixed function",
                        r.len(),
                    )
                })?;
                (r, vec![expr])
            };
            *left = Expr::InfixFunc {
                name,
                left: Box::new(left.clone()),
                right: right_exprs,
                modifier,
            };
            rest = r;
            continue;
        }
        // Negated bracket infix operators: ![op]
        if let Some(r_after_bang) = r.strip_prefix('!')
            && let Some(bracket_infix) = parse_bracket_infix_op(r_after_bang)
        {
            match bracket_infix {
                BracketInfix::PlainOp(op, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket infix op",
                            r.len(),
                        )
                    })?;
                    let binary = if let Some(tk) = op_str_to_token_kind(&op) {
                        Expr::Binary {
                            left: Box::new(left.clone()),
                            op: tk,
                            right: Box::new(right),
                        }
                    } else {
                        Expr::InfixFunc {
                            name: op,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        }
                    };
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(binary),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::MetaOp(meta, op, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket meta infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(Expr::MetaOp {
                            meta,
                            op,
                            left: Box::new(left.clone()),
                            right: Box::new(right),
                        }),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::UserInfix(name, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket user infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(Expr::InfixFunc {
                            name,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        }),
                    };
                    rest = r;
                    continue;
                }
            }
        }
        // Bracket infix operators: [+], [R-], [Z*], [Z[cmp]], [blue], etc.
        if let Some(bracket_infix) = parse_bracket_infix_op(r) {
            match bracket_infix {
                BracketInfix::PlainOp(op, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after bracket infix op",
                            r.len(),
                        )
                    })?;
                    // Resolve plain op to a Binary expr
                    if let Some(tk) = op_str_to_token_kind(&op) {
                        *left = Expr::Binary {
                            left: Box::new(left.clone()),
                            op: tk,
                            right: Box::new(right),
                        };
                    } else {
                        // Unknown plain op: treat as user-defined infix
                        *left = Expr::InfixFunc {
                            name: op,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        };
                    }
                    rest = r;
                    continue;
                }
                BracketInfix::MetaOp(meta, op, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let needs_comma_list = meta == "Z"
                        || meta == "X"
                        || op == "..."
                        || op == "...^"
                        || op == "…"
                        || op == "…^";
                    let (r, right) = if needs_comma_list {
                        let (r, items) = parse_comma_list_of_range_raw(r)?;
                        if items.len() == 1 {
                            (r, items.into_iter().next().unwrap())
                        } else {
                            (r, Expr::ArrayLiteral(items))
                        }
                    } else {
                        range_expr(r).map_err(|err| {
                            enrich_expected_error(
                                err,
                                "expected expression after bracket meta infix op",
                                r.len(),
                            )
                        })?
                    };
                    *left = Expr::MetaOp {
                        meta,
                        op,
                        left: Box::new(left.clone()),
                        right: Box::new(right),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::UserInfix(name, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after bracket user infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::InfixFunc {
                        name,
                        left: Box::new(left.clone()),
                        right: vec![right],
                        modifier: None,
                    };
                    rest = r;
                    continue;
                }
            }
        }
        // Meta operators: R-, X+, Z~, bare Z, bare X, R[...], etc.
        if let Some((meta, op, len)) = parse_meta_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let needs_comma_list = (meta == "Z" || meta == "X")
                || op == "..."
                || op == "...^"
                || op == "…"
                || op == "…^";
            let (r, right) = if needs_comma_list {
                let (r, items) = parse_comma_list_of_range_raw(r)?;
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
            *left = Expr::MetaOp {
                meta,
                op,
                left: Box::new(left.clone()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // User-defined infix words (typically via my &infix:<...> = ...),
        // e.g. `42 same-in-Int "42"`.
        // Do not span statement boundaries across newlines.
        let ws_before = &rest[..rest.len() - r.len()];
        if !ws_before.contains('\n')
            && let Some((name, len)) = parse_custom_infix_word(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = range_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after infix operator", r.len())
            })?;
            *left = Expr::InfixFunc {
                name,
                left: Box::new(left.clone()),
                right: vec![right],
                modifier: None,
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok(rest)
}

fn parse_custom_infix_word(input: &str) -> Option<(String, usize)> {
    let first = input.chars().next()?;
    if !first.is_alphabetic() && first != '_' {
        return None;
    }
    let mut end = first.len_utf8();
    for ch in input[end..].chars() {
        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
            end += ch.len_utf8();
        } else {
            break;
        }
    }
    let name = &input[..end];
    if is_reserved_infix_word(name) {
        return None;
    }
    Some((name.to_string(), end))
}

fn is_reserved_infix_word(name: &str) -> bool {
    if name
        .chars()
        .next()
        .map(|c| c.is_ascii_uppercase())
        .unwrap_or(false)
    {
        return true;
    }
    if name.chars().all(|c| c.is_ascii_uppercase()) {
        return true;
    }
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "with"
            | "without"
            | "my"
            | "our"
            | "state"
            | "has"
            | "sub"
            | "method"
            | "class"
            | "role"
            | "grammar"
            | "module"
            | "package"
            | "token"
            | "rule"
            | "regex"
            | "multi"
            | "proto"
            | "constant"
            | "enum"
            | "subset"
            | "unit"
            | "use"
            | "need"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "fail"
            | "take"
            | "do"
            | "try"
            | "quietly"
            | "react"
            | "whenever"
            | "loop"
            | "repeat"
            | "let"
            | "temp"
            | "where"
            | "is"
            | "does"
            | "as"
            | "of"
            | "and"
            | "or"
            | "not"
            | "xor"
            | "andthen"
            | "orelse"
            | "notandthen"
            | "min"
            | "max"
            | "cmp"
            | "leg"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "eqv"
            | "after"
            | "before"
            | "gcd"
            | "lcm"
            | "x"
            | "xx"
            | "o"
    )
}

/// Parse a comma-separated list of range_expr, returning (rest, items).
fn parse_comma_list_of_range_raw<'a>(input: &'a str) -> PResult<'a, Vec<Expr>> {
    let (r, first) = range_expr(input)
        .map_err(|err| enrich_expected_error(err, "expected expression", input.len()))?;
    let mut items = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if !r2.starts_with(',') || r2.starts_with(",,") {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')') {
            break;
        }
        if let Ok((r3, next)) = range_expr(r2) {
            items.push(next);
            r = r3;
        } else {
            break;
        }
    }
    Ok((r, items))
}

/// Parse a comma-separated list of range_expr, returning (rest, Vec<Expr>).
fn parse_comma_list_of_range<'a>(input: &'a str) -> PResult<'a, Vec<Expr>> {
    parse_comma_list_of_range_raw(input)
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
    // Detect !. which is not a valid metaop
    if r.starts_with("!.") && !r.starts_with("!..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Unsupported use of !. to concatenate strings; in Raku please use ~",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate . because it is too fiddly",
            r,
        ));
    }
    if let Some((op, len)) = parse_negated_meta_comparison_op(r) {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
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
        let mut result = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right.clone()),
            }),
        };
        // Support chaining: "a" !after "b" !after "c" → (!after) && (!after)
        let mut prev_right = right;
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            // Try negated meta comparison for chaining
            if let Some((cop, chain_len)) = parse_negated_meta_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained negated comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                })?;
                let next_cmp = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(prev_right),
                        op: cop.token_kind(),
                        right: Box::new(next_right.clone()),
                    }),
                };
                result = Expr::Binary {
                    left: Box::new(result),
                    op: TokenKind::AndAnd,
                    right: Box::new(next_cmp),
                };
                prev_right = next_right;
                r = r2;
                continue;
            }
            // Also try regular comparison for mixed chaining
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
                continue;
            }
            break;
        }
        return Ok((r, result));
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
        if matches!(op, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch) {
            right = wrap_smartmatch_rhs(right);
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
            if let Some((cop, chain_len)) = parse_negated_meta_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained negated comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                })?;
                let next_cmp = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(prev_right),
                        op: cop.token_kind(),
                        right: Box::new(next_right.clone()),
                    }),
                };
                result = Expr::Binary {
                    left: Box::new(result),
                    op: TokenKind::AndAnd,
                    right: Box::new(next_cmp),
                };
                prev_right = next_right;
                r = r2;
                continue;
            }
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
                continue;
            }
            break;
        }
        return Ok((r, result));
    }

    Ok((rest, left))
}

fn wrap_smartmatch_rhs(right: Expr) -> Expr {
    match right {
        // Keep Pair shape for `%hash ~~ key => !*.foo` by wrapping only value side.
        Expr::Binary {
            left,
            op: TokenKind::FatArrow,
            right,
        } => {
            let value = if contains_whatever(&right) && !matches!(&*right, Expr::Whatever) {
                wrap_whatevercode(&right)
            } else {
                *right
            };
            Expr::Binary {
                left,
                op: TokenKind::FatArrow,
                right: Box::new(value),
            }
        }
        other => {
            if contains_whatever(&other) && !matches!(&other, Expr::Whatever) {
                wrap_whatevercode(&other)
            } else {
                other
            }
        }
    }
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

fn parse_negated_meta_comparison_op(r: &str) -> Option<(ComparisonOp, usize)> {
    let inner = r.strip_prefix('!')?;
    let (op, len) = parse_comparison_op(inner)?;
    // Operators that already have their own !-prefixed spelling are not meta-negated forms.
    if matches!(
        op,
        ComparisonOp::NumNe
            | ComparisonOp::NotDivisibleBy
            | ComparisonOp::SmartMatch
            | ComparisonOp::SmartNotMatch
    ) {
        return None;
    }
    Some((op, len + 1))
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

/// Convert an operator string to its TokenKind for bracket infix notation.
fn op_str_to_token_kind(op: &str) -> Option<TokenKind> {
    match op {
        "..." | "…" => Some(TokenKind::DotDotDot),
        "...^" | "…^" => Some(TokenKind::DotDotDotCaret),
        "+" => Some(TokenKind::Plus),
        "-" => Some(TokenKind::Minus),
        "*" => Some(TokenKind::Star),
        "/" => Some(TokenKind::Slash),
        "%" => Some(TokenKind::Percent),
        "**" => Some(TokenKind::StarStar),
        "~" => Some(TokenKind::Tilde),
        "==" => Some(TokenKind::EqEq),
        "!=" => Some(TokenKind::BangEq),
        "<" => Some(TokenKind::Lt),
        ">" => Some(TokenKind::Gt),
        "<=" => Some(TokenKind::Lte),
        ">=" => Some(TokenKind::Gte),
        "<=>" => Some(TokenKind::LtEqGt),
        "===" => Some(TokenKind::EqEqEq),
        "~~" => Some(TokenKind::SmartMatch),
        "&&" => Some(TokenKind::AndAnd),
        "||" => Some(TokenKind::OrOr),
        "^^" => Some(TokenKind::XorXor),
        "//" => Some(TokenKind::SlashSlash),
        "%%" => Some(TokenKind::PercentPercent),
        "+&" => Some(TokenKind::BitAnd),
        "+|" => Some(TokenKind::BitOr),
        "+^" => Some(TokenKind::BitXor),
        // Word operators are represented as Ident tokens
        "eq" | "ne" | "lt" | "gt" | "le" | "ge" | "leg" | "cmp" | "min" | "max" | "gcd" | "lcm"
        | "and" | "or" | "not" | "after" | "before" => Some(TokenKind::Ident(op.to_string())),
        _ => None,
    }
}

/// Find the index of the closing `]` that matches the opening `[` at position 0.
/// Returns the index of the matching `]`, or None if not found.
fn find_matching_bracket(input: &str) -> Option<usize> {
    let mut depth = 0;
    for (i, c) in input.char_indices() {
        match c {
            '[' => depth += 1,
            ']' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Flatten nested bracket operator notation into a simple string.
/// e.g., "+" → "+", "R-" → "R-", "R[R-]" → "RR-", "[+]" → "+", "R[R[R-]]" → "RRR-"
fn flatten_bracket_op(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let first = s.as_bytes()[0];
    // Handle plain bracket: [op] → flatten(op)
    if first == b'['
        && let Some(end) = find_matching_bracket(s)
    {
        let inner = &s[1..end];
        return flatten_bracket_op(inner);
    }
    // Handle meta + bracket: R[op] → R + flatten(op)
    if (first == b'R' || first == b'Z' || first == b'X')
        && s.len() > 1
        && s.as_bytes()[1] == b'['
        && let Some(end) = find_matching_bracket(&s[1..])
    {
        let inner = &s[2..1 + end];
        let flattened_inner = flatten_bracket_op(inner);
        let rest = &s[1 + end + 1..];
        return format!("{}{}{}", first as char, flattened_inner, rest);
    }
    s.to_string()
}

/// Known operators for bracket infix and meta-op bracket notation.
const KNOWN_OPS: &[&str] = &[
    "...^", "...", "…^", "…", "**", "==", "!=", "<=", ">=", "<=>", "===", "~~", "%%", "//", "||",
    "&&", "~", "+", "-", "*", "/", "%", "<", ">", "+&", "+|", "+^", "?&", "?|", "?^", "cmp", "min",
    "max", "eq", "ne", "lt", "gt", "le", "ge", "leg", "and", "or", "not", "after", "before", "gcd",
    "lcm", ",",
];

/// Parse meta operator: R-, X+, Zcmp, R[+], Z[~], R[R[R-]], RR[R-], etc.
fn parse_meta_op(input: &str) -> Option<(String, String, usize)> {
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

    // Try bracket notation: R[op], Z[op], X[op]
    if r.starts_with('[')
        && let Some(end) = find_matching_bracket(r)
    {
        let inner = &r[1..end];
        let op = flatten_bracket_op(inner);
        return Some((meta.to_string(), op, 1 + end + 1));
    }

    // Try chained meta with brackets: RR[op], RZ[op], etc.
    if let Some((inner_meta, inner_op, inner_len)) = parse_meta_op(r) {
        let op = format!("{}{}", inner_meta, inner_op);
        return Some((meta.to_string(), op, 1 + inner_len));
    }

    // Try symbolic operators first (multi-char then single-char)
    let ops: &[&str] = &[
        "...^", "...", "…^", "…", "**", "=>", "==", "!=", "<=", ">=", "~~", "%%", "//", "~", "+",
        "-", "*", "/", "%", "<", ">",
    ];
    for op in ops {
        if r.starts_with(op) {
            return Some((meta.to_string(), op.to_string(), 1 + op.len()));
        }
    }
    // Try word operators: cmp, min, max, eq, ne, lt, gt, le, ge, leg
    let word_ops: &[&str] = &[
        "cmp", "min", "max", "eq", "ne", "lt", "gt", "le", "ge", "leg",
    ];
    for op in word_ops {
        if r.starts_with(op) && !is_ident_char(r.as_bytes().get(op.len()).copied()) {
            return Some((meta.to_string(), op.to_string(), 1 + op.len()));
        }
    }
    // Custom word operators: Xwtf, Zfoo-bar, Rcustom-op
    if let Some((name, len)) = parse_meta_word_op(r) {
        return Some((meta.to_string(), name, 1 + len));
    }
    // Bare Z (zip with comma) or bare X (cross product) — followed by non-ident, non-operator char
    if (meta == "Z" || meta == "X") && !is_ident_char(r.as_bytes().first().copied()) {
        return Some((meta.to_string(), String::new(), 1));
    }
    None
}

fn parse_meta_word_op(input: &str) -> Option<(String, usize)> {
    let first = input.chars().next()?;
    if !first.is_alphabetic() && first != '_' {
        return None;
    }
    let mut end = first.len_utf8();
    for ch in input[end..].chars() {
        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
            end += ch.len_utf8();
        } else {
            break;
        }
    }
    Some((input[..end].to_string(), end))
}

fn strip_sequence_op(input: &str) -> Option<(&str, TokenKind, &str)> {
    if let Some(rest) = input.strip_prefix("...^") {
        Some((rest, TokenKind::DotDotDotCaret, "...^"))
    } else if let Some(rest) = input.strip_prefix("…^") {
        Some((rest, TokenKind::DotDotDotCaret, "…^"))
    } else if input.starts_with("...") && !input.starts_with("....") {
        Some((&input[3..], TokenKind::DotDotDot, "..."))
    } else if let Some(rest) = input.strip_prefix("…") {
        Some((rest, TokenKind::DotDotDot, "…"))
    } else {
        None
    }
}

/// Parse bracket infix operator: [+], [R-], [Z*], [Z[cmp]], [blue], etc.
/// Returns (kind, total_consumed_len) where kind is an enum describing
/// whether it's a plain op, meta op, or user-defined infix.
enum BracketInfix {
    /// A plain operator like `+`, `-`, `*`, `cmp`
    PlainOp(String, usize),
    /// A meta operator like `R-`, `Z*`, `Zcmp`
    MetaOp(String, String, usize),
    /// A user-defined infix like `blue`
    UserInfix(String, usize),
}

fn parse_bracket_infix_op(input: &str) -> Option<BracketInfix> {
    if !input.starts_with('[') {
        return None;
    }
    // Don't match [&func] (handled by parse_infix_func_op)
    if input.starts_with("[&") {
        return None;
    }
    let end = find_matching_bracket(input)?;
    let inner = &input[1..end];
    if inner.is_empty() {
        return None;
    }
    let total_len = end + 1;
    let flattened = flatten_bracket_op(inner);

    // Check if it's a meta-prefixed op
    let first = flattened.as_bytes()[0];
    if first == b'R' || first == b'Z' || first == b'X' || first == b'!' {
        let meta_ch = &flattened[..1];
        let op = &flattened[1..];
        // Make sure inner op is valid (known op or known meta pattern)
        if !op.is_empty() {
            return Some(BracketInfix::MetaOp(
                meta_ch.to_string(),
                op.to_string(),
                total_len,
            ));
        }
    }

    // Check if it's a known plain operator
    if KNOWN_OPS.contains(&flattened.as_str()) {
        return Some(BracketInfix::PlainOp(flattened, total_len));
    }

    // Check if it's a user-defined infix name (identifier)
    if flattened
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return Some(BracketInfix::UserInfix(flattened, total_len));
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

/// Classify an operator string by its precedence level.
enum OpPrecedence {
    Multiplicative,
    Additive,
    Concatenation,
    Comparison,
    Other,
}

fn classify_base_op(op: &str) -> OpPrecedence {
    // Strip all meta prefixes to find the base operator
    let mut s = op;
    while let Some(rest) = s
        .strip_prefix('R')
        .or_else(|| s.strip_prefix('Z'))
        .or_else(|| s.strip_prefix('X'))
    {
        s = rest;
    }
    match s {
        "*" | "/" | "%" | "gcd" | "lcm" => OpPrecedence::Multiplicative,
        "+" | "-" => OpPrecedence::Additive,
        "~" => OpPrecedence::Concatenation,
        "==" | "!=" | "<" | ">" | "<=" | ">=" | "<=>" | "===" | "eq" | "ne" | "lt" | "gt"
        | "le" | "ge" | "leg" | "cmp" | "~~" | "%%" => OpPrecedence::Comparison,
        _ => OpPrecedence::Other,
    }
}

/// Try to parse a bracket meta-op or bracket infix at a specific precedence level.
/// Returns Some((meta, op, total_len)) for meta-ops,
/// or Some(("", op, total_len)) for plain bracket infix.
fn try_bracket_op_at_level(input: &str, level: &OpPrecedence) -> Option<(String, String, usize)> {
    // Try R[op], Z[op], X[op]
    // Skip Z/X meta-ops — they need list-infix comma handling
    if let Some((meta, op, len)) = parse_meta_op(input)
        && meta != "Z"
        && meta != "X"
    {
        let full_op = format!("{}{}", meta, op);
        if std::mem::discriminant(&classify_base_op(&full_op)) == std::mem::discriminant(level) {
            return Some((meta, op, len));
        }
    }
    // Try [op] bracket infix
    if let Some(bracket_infix) = parse_bracket_infix_op(input) {
        match &bracket_infix {
            BracketInfix::PlainOp(op, len) => {
                if std::mem::discriminant(&classify_base_op(op)) == std::mem::discriminant(level) {
                    return Some((String::new(), op.clone(), *len));
                }
            }
            BracketInfix::MetaOp(meta, op, len) if meta != "Z" && meta != "X" => {
                let full_op = format!("{}{}", meta, op);
                if std::mem::discriminant(&classify_base_op(&full_op))
                    == std::mem::discriminant(level)
                {
                    return Some((meta.clone(), op.clone(), *len));
                }
            }
            _ => {}
        }
    }
    None
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
        // Try bracket meta-op at additive level: R[+], R[-], [R+], [R-], etc.
        if let Some((meta, op, len)) = try_bracket_op_at_level(r, &OpPrecedence::Additive) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after bracket additive operator",
                    r.len(),
                )
            })?;
            if meta.is_empty() {
                // Plain bracket infix: [+] or [-]
                if let Some(tk) = op_str_to_token_kind(&op) {
                    left = Expr::Binary {
                        left: Box::new(left),
                        op: tk,
                        right: Box::new(right),
                    };
                }
            } else {
                left = Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
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
        // Try bracket meta-op at multiplicative level: R[*], R[/], [R*], [R/], etc.
        if let Some((meta, op, len)) = try_bracket_op_at_level(r, &OpPrecedence::Multiplicative) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after bracket multiplicative operator",
                    r.len(),
                )
            })?;
            if meta.is_empty() {
                if let Some(tk) = op_str_to_token_kind(&op) {
                    left = Expr::Binary {
                        left: Box::new(left),
                        op: tk,
                        right: Box::new(right),
                    };
                }
            } else {
                left = Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
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
