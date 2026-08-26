//! Chained-comparison expansion, deferred out of the parser
//! (`todo/tickets/chained-compare-ast-node.md`, ADR-0033's "Phase-4
//! prerequisite" section).
//!
//! The parser only builds `Expr::ChainedCompare { operands, ops }` for a real
//! chain (`a < m < b`, `a !before b before c`, ...); this module expands that
//! marker into the runtime `&&`-conjunction shape, invoked from the
//! compiler's `Expr::ChainedCompare` arm. This mirrors ADR-0033 Phase 1's
//! `whatever_curry::build_closure` deferral: the algorithm that builds the
//! expansion (`build_chain_cmp_expr`, formerly in
//! `src/parser/expr/precedence/chain_cmp.rs`) is unchanged, only *when* it
//! runs moved from parse time to compile time.
//!
//! # Why the old "repeated middle" duplicate-expansion path is gone
//!
//! Before this node existed, a chain containing a `*` (Whatever) placeholder
//! was expanded with the shared middle operand *duplicated* as two separate
//! AST nodes (`(a OP m) && (m OP b)`, no temp variable) instead of routing
//! through the single-evaluation temp-variable form every other chain used.
//! That existed only so the Whatever-curry walkers (`count_whatever`,
//! `replace_whatever_*`) — which pattern-matched the *expanded* `&&` tree —
//! could still see and substitute the placeholder inside what would otherwise
//! be an opaque `Stmt::VarDecl` in a `DoBlock` they never descended into.
//!
//! With `Expr::ChainedCompare` as a first-class marker, Whatever substitution
//! now runs on `operands` directly (see `whatever_curry::{build,replace}`'s
//! `ChainedCompare` arms) *before* this module's `expand` ever runs — by the
//! time `expand` sees the operands, any `*` has already been replaced with a
//! plain parameter variable, or the chain never had one. The visibility
//! problem the duplicate path existed to work around cannot occur anymore, so
//! `expand` always uses the safe single-evaluation form
//! (`build_chain_cmp_expr`), which is also a latent correctness fix: the old
//! `operands.iter().any(contains_whatever)` gate chose the duplicating path
//! whenever *any* operand contained a Whatever, even one unrelated to the
//! shared middle, so an effectful non-Whatever middle sitting next to an
//! unrelated Whatever operand could have been evaluated twice.

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use std::sync::atomic::{AtomicUsize, Ordering};

static CHAIN_CMP_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Build a single (possibly negated) comparison `left OP right`.
pub(crate) fn make_chain_cmp(left: Expr, op: TokenKind, right: Expr, negated: bool) -> Expr {
    let cmp = Expr::Binary {
        left: Box::new(left),
        op,
        right: Box::new(right),
    };
    if negated {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cmp),
        }
    } else {
        cmp
    }
}

/// Expand `operands`/`ops` into the runtime conjunction shape, evaluating
/// every intermediate operand exactly once via a synthesized temp variable.
/// `ops.len() == operands.len() - 1` is required (the parser only builds a
/// `ChainedCompare` for `ops.len() >= 2`, i.e. a real chain).
fn build_chain_cmp_expr(
    operands: &[Expr],
    ops: &[(TokenKind, bool)],
    index: usize,
    left: Expr,
) -> Expr {
    let (op, negated) = ops[index].clone();
    if index == ops.len() - 1 {
        return make_chain_cmp(left, op, operands[index + 1].clone(), negated);
    }

    let tmp_idx = CHAIN_CMP_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let tmp_name = format!("__mutsu_chain_cmp_{tmp_idx}");
    let tmp_var = Expr::Var(tmp_name.clone());
    let cmp = make_chain_cmp(left, op, tmp_var.clone(), negated);
    let rest = build_chain_cmp_expr(operands, ops, index + 1, tmp_var.clone());
    Expr::DoBlock {
        body: vec![
            Stmt::VarDecl {
                name: tmp_name,
                expr: operands[index + 1].clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::Expr(Expr::Binary {
                left: Box::new(cmp),
                op: TokenKind::AndAnd,
                right: Box::new(rest),
            }),
        ],
        label: None,
    }
}

/// Expand an `Expr::ChainedCompare { operands, ops }` marker into its runtime
/// shape. Called from the compiler's `Expr::ChainedCompare` arm, which
/// immediately compiles the result — the expanded tree never re-enters any
/// AST-level walker, exactly like `whatever_curry::build_closure`.
pub(crate) fn expand(operands: &[Expr], ops: &[(TokenKind, bool)]) -> Expr {
    debug_assert_eq!(operands.len(), ops.len() + 1);
    build_chain_cmp_expr(operands, ops, 0, operands[0].clone())
}
