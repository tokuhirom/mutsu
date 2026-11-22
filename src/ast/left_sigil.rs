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

use super::{ContextKind, Expr, SubstThunk};
use crate::ast_visit::{Visit, VisitMut};
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

/// Mark the `=` of every `s[...] = …` in `expr`'s own expression, which
/// starts at `at`, as an item assignment when the sigil is `$` there (#18446):
/// rakudo's substitution reads `$*LEFTSIGIL` as `infix:<=>` does, so
/// `$s ~~ s[a] = 1` is `Assignment.new(:item)` and a lone `s[a] = 1` is not.
/// A substitution in an expression of its own is marked by that
/// expression's parse.
// Cost: O(n), n = size of the part of `expr` that shares its expression, plus
// the size of `expr` when it holds such a substitution.
pub(crate) fn mark_substitutions(at: &str, expr: &mut Expr) {
    let sigil = match INHERITED.get() {
        Some((ptr, sigil)) if ptr == at.as_ptr() as usize => sigil,
        _ => LeftSigil::Unset,
    };
    let mut reader = Reader {
        sigil,
        marks: Some(Vec::new()),
    };
    reader.visit_expr(expr);
    let marks = reader.marks.unwrap_or_default();
    if !marks.is_empty() {
        Marker { marks }.visit_expr_mut(expr);
    }
}

/// Sets the `item` flag of the thunks [`Reader`] found, by address.
struct Marker {
    marks: Vec<(usize, bool)>,
}

impl VisitMut for Marker {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if self.marks.is_empty() {
            return;
        }
        if let Expr::Subst {
            replacement_thunk: Some(thunk),
            ..
        }
        | Expr::NonDestructiveSubst {
            replacement_thunk: Some(thunk),
            ..
        } = expr
        {
            let addr = &**thunk as *const SubstThunk as usize;
            if let Some(at) = self.marks.iter().position(|(a, _)| *a == addr) {
                thunk.item = self.marks.swap_remove(at).1;
            }
        }
        crate::ast_visit::walk_expr_mut(self, expr);
    }
}

/// The sigil after `expr` has been read with `sigil` already left by what
/// precedes it.
// Cost: O(n), n = size of the part of `expr` that shares its expression.
pub(crate) fn after(sigil: LeftSigil, expr: &Expr) -> LeftSigil {
    let mut reader = Reader { sigil, marks: None };
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
    let mut reader = Reader { sigil, marks: None };
    reader.visit_expr(target);
    reader.sigil == LeftSigil::Scalar
}

struct Reader {
    sigil: LeftSigil,
    /// The `s[...] = …` thunks met, by address, with whether the sigil was
    /// `$` there; `None` when nobody asked.
    marks: Option<Vec<(usize, bool)>>,
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
    /// An `s[...] = …` term: its `=` reads the sigil.
    SubstAssign(&'a SubstThunk),
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
        Expr::WhateverCurry(inner, ..) | Expr::DeitemizeForBind(inner) => {
            Step::Operands(Operands::One(inner))
        }
        Expr::Subst {
            replacement_thunk: Some(thunk),
            ..
        }
        | Expr::NonDestructiveSubst {
            replacement_thunk: Some(thunk),
            ..
        } if thunk.infix.is_none() => Step::SubstAssign(thunk),
        Expr::Ast(node) => match node.as_ref() {
            super::parsed::ParsedExpr::Assign(assign) => Step::Assign(&assign.source),
            super::parsed::ParsedExpr::SubscriptAdverbs(adverbs) => {
                Step::Postfix(&adverbs.subscript)
            }
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
            Step::SubstAssign(thunk) => {
                let item = self.sigil == LeftSigil::Scalar;
                if let Some(marks) = &mut self.marks {
                    marks.push((thunk as *const SubstThunk as usize, item));
                }
            }
            Step::Skip => {}
        }
    }
}
