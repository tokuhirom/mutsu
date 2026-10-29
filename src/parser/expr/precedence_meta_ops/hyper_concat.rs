use crate::ast::Expr;
use crate::parser::expr::operators::{
    enrich_expected_error, parse_pure_concat_op, parse_replication_op,
};
use crate::parser::helpers::ws;
use crate::parser::parse_result::PResult;

use super::arith::{OpPrecedence, additive_expr, classify_base_op, try_custom_infix_at_level};
use super::hyper_spelling::{parse_hyper_func_op, parse_hyper_op};
use super::meta_bracket::block_newline_terminates;

/// `TARGET »=» VALUE` as written: kept in its RakuAST shape for the conversion
/// when the target is a literal list of lvalues, lowered at once otherwise
/// (ADR-10723 Stage 2, #14966).
// Cost: O(n), n = size of the target and value.
fn written_hyper_assignment(target: Expr, value: Expr, dwim_left: bool, dwim_right: bool) -> Expr {
    if matches!(target.peel_parens(), Expr::ArrayLiteral(_)) {
        return Expr::Ast(Box::new(crate::ast::parsed::ParsedExpr::HyperAssign(
            crate::ast::parsed::HyperAssign {
                target,
                value,
                dwim_left,
                dwim_right,
            },
        )));
    }
    crate::rakuast::hyper_assign::lower_hyper_assignment(target, value, dwim_left, dwim_right)
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
        if block_newline_terminates(input, rest, r, &left) {
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
            left = if op == "=" {
                written_hyper_assignment(left, right, dwim_left, dwim_right)
            } else {
                Expr::HyperOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    dwim_left,
                    dwim_right,
                }
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
        if block_newline_terminates(input, rest, r, &left) {
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
            left = if op == "=" {
                written_hyper_assignment(left, right, dwim_left, dwim_right)
            } else {
                Expr::HyperOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    dwim_left,
                    dwim_right,
                }
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
                form: Default::default(),
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
        if block_newline_terminates(input, rest, r, &left) {
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
                form: Default::default(),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}
