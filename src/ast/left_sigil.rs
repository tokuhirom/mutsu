//! Whether an assignment is an item assignment (`Assignment.new(:item)`).
//!
//! Rakudo 2026.09's grammar (`src/Raku/Grammar.nqp`) decides it while it
//! parses an expression, from `$*LEFTSIGIL`: each `EXPR` starts it empty, the
//! first variable term of the expression sets it to its sigil, every postfix
//! (a method call, a subscript, `++`) sets it to `@`, and `infix:<=>` is an
//! item assignment exactly when it reads `$`. A parenthesized list, an
//! argument list, a ternary's middle, a block or a declarator is an `EXPR`
//! of its own and leaves the enclosing one's sigil alone.
//!
//! The parser builds an assignment once it has parsed the target, which is
//! every term of the expression that precedes the `=`, so the sigil is read
//! off the target as it is built ([`assignment_is_item`]) and stored in the
//! node (`parsed::Assign::item`). A construct that opens an `EXPR` of its own
//! is not walked: only the terms that share the target's `EXPR` can move the
//! sigil.

use super::{ContextKind, Expr};
use crate::ast_visit::Visit;
use std::cell::Cell;

/// The sigil an expression's variable terms have left so far: `$*LEFTSIGIL`.
#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum LeftSigil {
    Unset,
    Scalar,
    Other,
}

thread_local! {
    /// The sigil the operands before a loose operator (`andthen`, `,`) left,
    /// and where the operand that follows it starts: that operand is parsed
    /// as a part of the same `EXPR`, so its first term finds the sigil set.
    static INHERITED: Cell<Option<(usize, LeftSigil)>> = const { Cell::new(None) };
}

/// The sigil after `expr` has been read with `sigil` already left by what
/// precedes it. Only a keeping parse reads the sigil, so any other parse
/// stays `Unset` and skips the walk.
// Cost: O(n), n = size of the part of `expr` that shares its expression.
pub(crate) fn after(sigil: LeftSigil, expr: &Expr) -> LeftSigil {
    if !super::spelled::keeping() {
        return LeftSigil::Unset;
    }
    let mut reader = Reader { sigil };
    reader.visit_expr(expr);
    reader.sigil
}

/// Run `parse_operand`, the parse of the operand that starts at `at` after a
/// loose operator whose earlier operands left `sigil`.
// Cost: O(1) plus `parse_operand`.
pub(crate) fn inherit<R>(at: &str, sigil: LeftSigil, parse_operand: impl FnOnce() -> R) -> R {
    if sigil == LeftSigil::Unset {
        return parse_operand();
    }
    let saved = INHERITED.replace(Some((at.as_ptr() as usize, sigil)));
    let parsed = parse_operand();
    INHERITED.set(saved);
    parsed
}

/// Whether the operand that starts at `at` inherits a sigil: a parse of it
/// is not the same parse of the same text without one, so it is not memoized.
// Cost: O(1).
pub(crate) fn inherits_at(at: &str) -> bool {
    INHERITED
        .get()
        .is_some_and(|(ptr, _)| ptr == at.as_ptr() as usize)
}

/// Whether `TARGET = …` is an item assignment: the sigil is `$` when the `=`
/// is reached. `at` is where the target starts: what the operators before it
/// left in the same expression is already in the sigil.
// Cost: O(n), n = size of the part of the target that shares its expression.
pub(crate) fn assignment_is_item(at: Option<&str>, target: &Expr) -> bool {
    let inherited = INHERITED.get();
    let sigil = match (at, inherited) {
        (Some(at), Some((ptr, sigil))) if ptr == at.as_ptr() as usize => sigil,
        _ => LeftSigil::Unset,
    };
    let mut reader = Reader { sigil };
    reader.visit_expr(target);
    reader.sigil == LeftSigil::Scalar
}

struct Reader {
    sigil: LeftSigil,
}

impl Reader {
    /// A variable term sets the sigil when no earlier term did.
    // Cost: O(1).
    fn term(&mut self, sigil: LeftSigil) {
        if self.sigil == LeftSigil::Unset {
            self.sigil = sigil;
        }
    }
}

impl<'ast> Visit<'ast> for Reader {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Var(_) | Expr::CaptureVar(_) => self.term(LeftSigil::Scalar),
            Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_) => {
                self.term(LeftSigil::Other);
            }
            // `$(…)`, `@(…)`, `%(…)`: the grammar reads the contextualizer as
            // a variable of its sigil; what it holds is an expression of its
            // own.
            Expr::Itemize(_)
            | Expr::Contextualizer {
                kind: ContextKind::Item,
                ..
            } => self.term(LeftSigil::Scalar),
            Expr::Contextualizer { .. } => self.term(LeftSigil::Other),
            // A postfix reads its operand in this expression, then makes the
            // sigil `@`.
            Expr::MethodCall { target, .. }
            | Expr::DynamicMethodCall { target, .. }
            | Expr::HyperMethodCall { target, .. }
            | Expr::HyperMethodCallDynamic { target, .. }
            | Expr::Index { target, .. }
            | Expr::MultiDimIndex { target, .. }
            | Expr::ZenSlice(target)
            | Expr::Exists { target, .. } => {
                self.visit_expr(target);
                self.sigil = LeftSigil::Other;
            }
            Expr::PostfixOp { expr, .. } => {
                self.visit_expr(expr);
                self.sigil = LeftSigil::Other;
            }
            // Operators and markers keep their operands in this expression.
            Expr::Unary { expr: operand, .. } => self.visit_expr(operand),
            Expr::Binary { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::ChainedCompare { operands, .. } => {
                for operand in operands {
                    self.visit_expr(operand);
                }
            }
            // `?? EXPR !!`: the middle is an expression of its own.
            Expr::Ternary {
                cond, else_expr, ..
            } => {
                self.visit_expr(cond);
                self.visit_expr(else_expr);
            }
            // An assignment clears the sigil; its source is read on.
            Expr::Ast(node) => {
                if let super::parsed::ParsedExpr::Assign(assign) = node.as_ref() {
                    self.sigil = LeftSigil::Unset;
                    self.visit_expr(&assign.source);
                }
            }
            Expr::Spelled(spelled) => self.visit_expr(&spelled.expr),
            Expr::WhateverCurry(inner) | Expr::DeitemizeForBind(inner) => self.visit_expr(inner),
            // Anything else opens an expression of its own (a group, a
            // block, a call's arguments, a literal) or is not a variable term.
            _ => {}
        }
    }
}
