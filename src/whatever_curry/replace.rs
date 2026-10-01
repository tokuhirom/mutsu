//! WhateverCode body construction: replacing `*` placeholders with parameter
//! variables (numbered or single `$_`).
//!
//! A nested, already-planted `Expr::WhateverCurry` operand (e.g. `(* - 1)`
//! inside `(* - 1) - 1`) is inlined by recursing straight into its un-curried
//! body — since that body still has literal `Expr::Whatever` placeholders (not
//! yet turned into `$_`/`__wc_N` variables), no renaming pass is needed. This
//! is simpler than the pre-ADR-0033 code, which had to unwrap an
//! already-built `Lambda`/`AnonSubParams` closure and rename its parameter(s)
//! to fit the enclosing numbering scheme.
//!
//! The replacement clones the expression and rewrites the clone in place
//! through [`VisitMut`] (ADR-10499 §2). It is a walk of the priming scope's
//! operator spine, not of the whole tree: each operator hands on only its
//! currying operands (a method call's target, not its arguments), and any
//! other node is not a priming-scope operator, so a `*` below it is not this
//! closure's placeholder.

use crate::ast::Expr;
use crate::ast_visit::{VisitMut, walk_expr_mut};
use crate::parser::{expand_compound_assign_expr, is_whatever};
use crate::token_kind::TokenKind;

/// Replace Whatever expressions with numbered parameter variables.
/// `counter` tracks the next parameter index to assign.
// Cost: O(n), n = size of `expr`'s subtree.
pub(crate) fn replace_whatever_numbered(expr: &Expr, counter: &mut usize) -> Expr {
    let mut out = expr.clone();
    Replacer {
        counter: Some(counter),
    }
    .visit_expr_mut(&mut out);
    out
}

/// Replace Whatever and nested single-arg WhateverCode with $_ (for single-arg wrapping).
// Cost: O(n), n = size of `expr`'s subtree.
pub(crate) fn replace_whatever_single(expr: &Expr) -> Expr {
    let mut out = expr.clone();
    Replacer { counter: None }.visit_expr_mut(&mut out);
    out
}

/// `counter` numbers the placeholders `__wc_N`; without it every placeholder
/// (and a `**`) becomes `$_`.
struct Replacer<'a> {
    counter: Option<&'a mut usize>,
}

impl Replacer<'_> {
    fn placeholder(&mut self) -> Expr {
        match self.counter.as_deref_mut() {
            Some(counter) => {
                let var_name = format!("__wc_{counter}");
                *counter += 1;
                Expr::Var(var_name)
            }
            None => Expr::Var("_".to_string()),
        }
    }

    fn is_placeholder(&self, e: &Expr) -> bool {
        is_whatever(e) || (self.counter.is_none() && matches!(e, Expr::HyperWhatever))
    }
}

impl VisitMut for Replacer<'_> {
    fn visit_expr_mut(&mut self, e: &mut Expr) {
        // `((*))` is a frozen `Whatever` value, not a placeholder: it stays.
        // A single layer of parentheses is transparent, and `is_whatever`
        // already looks through it.
        if crate::parser::is_frozen_whatever(e) {
            return;
        }
        if self.is_placeholder(e) {
            *e = self.placeholder();
            return;
        }
        match e {
            // The grouping and an inner priming marker dissolve into this
            // closure's body: replace the node by its operand, rewritten.
            Expr::Grouped(inner) | Expr::WhateverCurry(inner) => {
                let operand = std::mem::replace(inner.as_mut(), Expr::Whatever);
                *e = operand;
                self.visit_expr_mut(e);
            }
            // A curried CompoundAssign retains its source marker for RakuAST,
            // but its executable closure body must be the established
            // expansion rebuilt with the substituted RHS. This also covers
            // index and method lvalues, whose stored expansion is a desugared
            // block rather than an AssignExpr.
            Expr::CompoundAssign {
                target, op, rhs, ..
            } => {
                let original = (**rhs).clone();
                self.visit_expr_mut(rhs);
                let expanded = op.strip_suffix('=').and_then(|op| {
                    expand_compound_assign_expr((**target).clone(), op, (**rhs).clone()).ok()
                });
                match expanded {
                    Some(expanded) => *e = expanded,
                    None => **rhs = original,
                }
            }
            Expr::AssignExpr { expr, .. } => self.visit_expr_mut(expr),
            // A thunk barrier is opaque: each of its operands is its own
            // priming scope (already wrapped in a `WhateverCurry` by
            // `super::plant`, which the compiler expands into its own
            // closure), so no placeholder inside it belongs to the enclosing
            // closure's parameter list. ADR-0033 Phase 4.
            e if super::plant::is_thunk_barrier(e) => {}
            // `todo/tickets/chained-compare-ast-node.md`: each operand appears
            // exactly once, so each is replaced in place and the node stays a
            // `ChainedCompare`. The final operand is exempt when the chain's
            // last link is a SmartMatch/BangTilde, mirroring the SmartMatch arm
            // below and `count_whatever` (numbering must agree with it, which
            // visits operands in the same order).
            Expr::ChainedCompare { operands, ops } => {
                let last_is_smartmatch_rhs = ops.last().is_some_and(|(op, _)| {
                    matches!(op, TokenKind::SmartMatch | TokenKind::BangTilde)
                });
                let n = operands.len();
                for (i, o) in operands.iter_mut().enumerate() {
                    if !(i + 1 == n && last_is_smartmatch_rhs && !is_whatever(o)) {
                        self.visit_expr_mut(o);
                    }
                }
            }
            // SmartMatch/BangTilde: a compound RHS Whatever is left untouched
            // (it is handled at runtime, not curried), but the RHS-autoprime
            // forms `X ~~ *` / `X !~~ *` (ADR-0033 Phase 2 section 2.5) have a
            // bare placeholder on the right that is replaced too.
            Expr::Binary {
                left,
                op: TokenKind::SmartMatch | TokenKind::BangTilde,
                right,
            } => {
                self.visit_expr_mut(left);
                if is_whatever(right) {
                    self.visit_expr_mut(right);
                }
            }
            // Every operand of these operators curries.
            Expr::Binary { .. }
            | Expr::Unary { .. }
            | Expr::PostfixOp { .. }
            | Expr::ZenSlice(_)
            | Expr::InfixFunc { .. }
            | Expr::MetaOp { .. } => walk_expr_mut(self, e),
            // Only the *target* of a method call, an invocation or a subscript
            // curries; a Whatever passed as an argument or used as the index
            // stays a value (see `count_whatever`/`contains_whatever`).
            Expr::MethodCall { target, .. }
            | Expr::DynamicMethodCall { target, .. }
            | Expr::HyperMethodCall { target, .. }
            | Expr::HyperMethodCallDynamic { target, .. }
            | Expr::CallOn { target, .. }
            | Expr::Index { target, .. } => self.visit_expr_mut(target),
            // Not a priming-scope operator: a `*` below it is not this
            // closure's placeholder.
            _ => {}
        }
    }
}
