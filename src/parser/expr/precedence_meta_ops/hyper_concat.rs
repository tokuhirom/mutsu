use crate::ast::Expr;
use crate::parser::expr::operators::{
    enrich_expected_error, parse_pure_concat_op, parse_replication_op,
};
use crate::parser::helpers::ws;
use crate::parser::parse_result::PResult;
use crate::parser::stmt::simple::TMP_INDEX_COUNTER;

use std::sync::atomic::Ordering;

use super::arith::{OpPrecedence, additive_expr, classify_base_op, try_custom_infix_at_level};
use super::hyper_spelling::{parse_hyper_func_op, parse_hyper_op};
use super::meta_bracket::block_newline_terminates;

/// Build the *shape* operand for a hyper assignment into a literal list of
/// lvalues.
///
/// `»=»`'s leaf op yields its right operand, so the left operand's values never
/// reach the result -- only its shape does, and that shape is what the dwim
/// rules measure. Reading the target back is the shape: a listy leaf carries
/// its own length, which is why raku fills `@a` in `(@a, $x) »=» 5` with one
/// `5` per existing element rather than a single one.
///
/// One leaf cannot be read: a target element that *declares* its variable
/// (`(my $a, my $b) »=» (1, 2)`) has nothing to read yet, and evaluating the
/// declaration here would run it twice. Such a leaf stands in as a scalar
/// placeholder, which is exactly what a freshly declared scalar contributes.
fn hyper_assign_shape(target: &Expr) -> Expr {
    match target {
        Expr::Grouped(inner) => hyper_assign_shape(inner),
        Expr::ArrayLiteral(items) => {
            Expr::ArrayLiteral(items.iter().map(hyper_assign_shape).collect())
        }
        Expr::DoStmt(_) => Expr::Literal(crate::value::Value::NIL),
        other => other.clone(),
    }
}

fn lower_hyper_assign_target(target: Expr, source: Expr) -> Expr {
    match target {
        Expr::Grouped(inner) => lower_hyper_assign_target(*inner, source),
        Expr::ArrayLiteral(items) => Expr::desugar_block(
            items
                .into_iter()
                .enumerate()
                .map(|(index, item)| {
                    crate::ast::Stmt::Expr(lower_hyper_assign_target(
                        item,
                        Expr::Index {
                            target: Box::new(source.clone()),
                            index: Box::new(Expr::Literal(crate::value::Value::int(index as i64))),
                            is_positional: true,
                        },
                    ))
                })
                .collect(),
        ),
        target => crate::parser::expr::precedence::assign_to_target_expr(target, source),
    }
}

fn lower_hyper_assignment(target: Expr, value: Expr, dwim_left: bool, dwim_right: bool) -> Expr {
    // A literal list of lvalues destructures positionally, nested sublists
    // included (`(($a, ($b, $c)), $d) »=« ((4, (5, 6)), 7)`), so it keeps the
    // element-wise lowering below.
    //
    // Every other target is an ordinary hyper op: `=` distributes its RIGHT
    // operand across the LEFT's shape, and the compiler's assignment-hyper-op
    // write-back stores the resulting list — the same route `»+=»` takes.
    // Lowering to a plain `target = value` instead made `@a »=» 7` store a
    // ONE-element array, and left a slice target to be filled by the store
    // path broadcasting its short RHS, which is not the same rule (a plain
    // `@a[0,1,2] = 7` must pad, not broadcast).
    if !matches!(target.peel_parens(), Expr::ArrayLiteral(_)) {
        return Expr::HyperOp {
            op: "=".to_string(),
            left: Box::new(target),
            right: Box::new(value),
            dwim_left,
            dwim_right,
        };
    }
    let index = TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed);
    let shape_name = format!("__mutsu_hyper_shape_{}", index);
    let temp_name = format!("__mutsu_hyper_assign_{}", index);
    // Distribute the RHS across the target's shape BEFORE destructuring it, so
    // the destructuring below is a plain positional walk over an already
    // correctly-sized list. Indexing the raw RHS instead re-implemented the
    // distribution rule with a bare `source[i]`, which made a scalar RHS
    // (`($x, $y) »=» 5`, a broadcast in raku) an out-of-range index, a short
    // list pad with Any instead of cycling, and a non-dwim length mismatch
    // truncate silently instead of raising X::HyperOp::NonDWIM.
    //
    // `»=»`'s leaf yields its right operand, so this hyper op is exactly the
    // distribution and nothing else. Its left operand is the shape temp rather
    // than the target itself because the compiler routes an assignment hyper op
    // over a literal list of lvalues through `__mutsu_assign_callable_lvalue`,
    // which cannot reach nested sublists; the write-back it emits for a plain
    // scalar left just re-stores the shape temp, which is inert.
    //
    // The RHS still evaluates exactly once: it is this hyper op's right
    // operand, and the result is bound to a temp before any target is touched.
    let distributed = Expr::HyperOp {
        op: "=".to_string(),
        left: Box::new(Expr::Var(shape_name.clone())),
        right: Box::new(value),
        dwim_left,
        dwim_right,
    };
    let temp_decl = |name: String, expr: Expr| crate::ast::Stmt::VarDecl {
        name,
        expr,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    };
    Expr::desugar_block(vec![
        temp_decl(shape_name, hyper_assign_shape(&target)),
        temp_decl(temp_name.clone(), distributed),
        crate::ast::Stmt::Expr(lower_hyper_assign_target(
            target,
            Expr::Var(temp_name.clone()),
        )),
        // The assignment's own value is the distributed list, not the last
        // element stored: `my $r = (($x, $y) »=» (5, 6))` is `$(5, 6)` in
        // raku. The temp is itemized by its `my $` declaration, which is
        // the itemization raku shows.
        crate::ast::Stmt::Expr(Expr::Var(temp_name)),
    ])
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
                lower_hyper_assignment(left, right, dwim_left, dwim_right)
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
                lower_hyper_assignment(left, right, dwim_left, dwim_right)
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
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}
