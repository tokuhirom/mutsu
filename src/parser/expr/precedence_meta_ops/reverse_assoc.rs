//! The `R` metaoperator reverses its base operator's associativity (#18907).
//!
//! `-` and `/` are left-associative, so `R-` and `R/` are right-associative:
//! `1 R- 2 R- 3` is `1 R- (2 R- 3)` (rakudo `0`). A reversed operator also
//! takes the nearest operand on its left at its own level, so `1 + 2 R- 3` is
//! `1 + (2 R- 3)`, while a left-associative operator after it groups to the
//! left as usual (`1 R- 2 + 3` is `(1 R- 2) + 3`).

use super::arith::{OpPrecedence, try_bracket_op_at_level};
use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::parser::parse_result::PResult;

/// The right operand of a reversed left-associative operator at `level`,
/// starting at `input` (after the operator): one operand, or a chain of
/// further `R` operators at the same level folded to the right.
// Cost: O(n), n = length of the chain.
pub(super) fn right_chain<'a>(
    input: &'a str,
    level: &OpPrecedence,
    operand: fn(&str) -> PResult<'_, Expr>,
) -> PResult<'a, Expr> {
    let (mut rest, mut last) = operand(input)?;
    // Each operand before the last, with the operator after it.
    let mut pending: Vec<(Expr, String)> = Vec::new();
    loop {
        let (r, _) = ws(rest)?;
        let Some((meta, op, len)) = try_bracket_op_at_level(r, level) else {
            break;
        };
        if meta != "R" {
            break;
        }
        let (r, _) = ws(&r[len..])?;
        let (r, next) = operand(r)?;
        pending.push((std::mem::replace(&mut last, next), op));
        rest = r;
    }
    let mut acc = last;
    while let Some((left, op)) = pending.pop() {
        acc = reversed(op, left, acc);
    }
    Ok((rest, acc))
}

/// `left R<op> right`, attached to `chain`, the expression its level has
/// built so far: a reversed operator binds the operand nearest to it, so when
/// `chain` is a left-associative application this level made (`1 + 2`), its
/// right operand is the left one (`1 + (2 R- 3)`).
// Cost: O(1).
pub(super) fn attach(chain: Expr, built_here: bool, op: String, right: Expr) -> Expr {
    match chain {
        Expr::Binary {
            left,
            op: binary_op,
            right: inner,
            form,
        } if built_here => Expr::Binary {
            left,
            op: binary_op,
            right: Box::new(reversed(op, *inner, right)),
            form,
        },
        other => reversed(op, other, right),
    }
}

// Cost: O(1).
fn reversed(op: String, left: Expr, right: Expr) -> Expr {
    Expr::MetaOp {
        meta: "R".to_string(),
        op,
        left: Box::new(left),
        right: Box::new(right),
    }
}
