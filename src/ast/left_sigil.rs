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
/// precedes it.
// Cost: O(n), n = size of the part of `expr` that shares its expression.
pub(crate) fn after(sigil: LeftSigil, expr: &Expr) -> LeftSigil {
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

/// How an expression moves the sigil.
enum Step<'a> {
    /// A variable term: sets the sigil if nothing did.
    Term(LeftSigil),
    /// A postfix: reads its operand in this expression, then makes the sigil `@`.
    Postfix(&'a Expr),
    /// An operator or marker whose operands share this expression.
    Operands(Operands<'a>),
    /// An assignment: clears the sigil, then reads its source.
    Assign(&'a Expr),
    /// A construct that opens an expression of its own, or is no variable term.
    Skip,
}

enum Operands<'a> {
    One(&'a Expr),
    Two(&'a Expr, &'a Expr),
    All(&'a [Expr]),
}

// Cost: O(1).
fn step(expr: &Expr) -> Step<'_> {
    match expr {
        Expr::Var(_) | Expr::CaptureVar(_) => Step::Term(LeftSigil::Scalar),
        Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_) => Step::Term(LeftSigil::Other),
        // `$(...)`, `@(...)`, `%(...)`: the grammar reads the contextualizer as
        // a variable of its sigil; what it holds is an expression of its own.
        Expr::Itemize(_)
        | Expr::Contextualizer {
            kind: ContextKind::Item,
            ..
        } => Step::Term(LeftSigil::Scalar),
        Expr::Contextualizer { .. } => Step::Term(LeftSigil::Other),
        Expr::MethodCall { target, .. }
        | Expr::DynamicMethodCall { target, .. }
        | Expr::HyperMethodCall { target, .. }
        | Expr::HyperMethodCallDynamic { target, .. }
        | Expr::Index { target, .. }
        | Expr::MultiDimIndex { target, .. }
        | Expr::ZenSlice(target)
        | Expr::Exists { target, .. } => Step::Postfix(target),
        Expr::PostfixOp { expr, .. } => Step::Postfix(expr),
        Expr::Unary { expr, .. } => Step::Operands(Operands::One(expr)),
        Expr::Binary { left, right, .. } => Step::Operands(Operands::Two(left, right)),
        Expr::ChainedCompare { operands, .. } => Step::Operands(Operands::All(operands)),
        // `?? EXPR !!`: the middle is an expression of its own.
        Expr::Ternary {
            cond, else_expr, ..
        } => Step::Operands(Operands::Two(cond, else_expr)),
        Expr::Spelled(spelled) => Step::Operands(Operands::One(&spelled.expr)),
        Expr::WhateverCurry(inner) | Expr::DeitemizeForBind(inner) => {
            Step::Operands(Operands::One(inner))
        }
        Expr::Ast(node) => match node.as_ref() {
            super::parsed::ParsedExpr::Assign(assign) => Step::Assign(&assign.source),
            super::parsed::ParsedExpr::SubscriptAdverbs(adverbs) => Step::Postfix(&adverbs.subscript),
            _ => Step::Skip,
        },
        _ => Step::Skip,
    }
}

impl<'ast> Visit<'ast> for Reader {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match step(expr) {
            Step::Term(sigil) => self.term(sigil),
            Step::Postfix(operand) => {
                self.visit_expr(operand);
                self.sigil = LeftSigil::Other;
            }
            Step::Operands(Operands::One(a)) => self.visit_expr(a),
            Step::Operands(Operands::Two(a, b)) => {
                self.visit_expr(a);
                self.visit_expr(b);
            }
            Step::Operands(Operands::All(all)) => {
                for operand in all {
                    self.visit_expr(operand);
                }
            }
            Step::Assign(source) => {
                self.sigil = LeftSigil::Unset;
                self.visit_expr(source);
            }
            Step::Skip => {}
        }
    }
}
