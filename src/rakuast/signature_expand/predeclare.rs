//! The predeclared targets of an assigning signature declaration
//! (`my ($a, @b) = RHS`).
//!
//! In a signature declaration the new variables are already in scope on the
//! right-hand side and hold their defaults (`my $x = 5; { my ($x, $y) = $x, 2 }`
//! reads the new `$x`, i.e. `(Any)`), so the plain targets are declared before
//! the RHS runs. The element step then *assigns* into those containers rather
//! than declaring the targets again: a closure the RHS builds (`my ($a, $c) =
//! 5, { $a }`) captured the predeclared container, and a second declaration
//! would give the name a fresh one the closure never sees (#18414). This is
//! the same shape the BEGIN-prologue split (`runtime::begin_prologue::
//! destructure`) gives the whole expansion.

use crate::ast::{AssignOp, Expr, SignatureDecl, SignatureInit, Stmt};
use crate::value::Value;

/// The names of the targets [`predeclarations`] declares ahead of the RHS:
/// plain, untyped, unconstrained `my` targets of an assignment.
// Cost: O(v), v = targets of the declaration.
pub(super) fn predeclared_names(decl: &SignatureDecl, init: &SignatureInit) -> Vec<String> {
    if init.is_binding || decl.is_state || decl.is_our || decl.type_constraint.is_some() {
        return Vec::new();
    }
    decl.vars
        .iter()
        .filter(|dvar| {
            dvar.literal_value.is_none()
                && !dvar.sigilless
                && dvar.per_var_type_constraint.is_none()
                && dvar.where_constraint.is_none()
                && !dvar.is_slurpy
                && !dvar.name.starts_with('&')
        })
        .map(|dvar| dvar.name.clone())
        .collect()
}

/// The default-initialized declarations of `names`, run before the RHS.
// Cost: O(v), v = names.
pub(super) fn predeclarations(names: &[String]) -> impl Iterator<Item = Stmt> + '_ {
    names.iter().map(|name| {
        let expr = if name.starts_with('@') {
            Expr::ArrayLiteral(Vec::new())
        } else if name.starts_with('%') {
            Expr::Hash(Vec::new(), crate::ast::HashSpelling::Composer)
        } else {
            Expr::Literal(Value::NIL)
        };
        Stmt::VarDecl {
            marks: Default::default(),
            anonymous: false,
            name: name.clone(),
            expr,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: false,
            constant: None,
            bind: None,
            where_constraint: None,
        }
    })
}

/// The element step of a predeclared target: `decl` (a plain re-declaration
/// of the target from its staged element) turned into an assignment into the
/// predeclared container. A declaration that carries more than the value (a
/// group `is default` trait) is kept as is.
// Cost: O(v), v = predeclared names.
pub(super) fn assign_into_predeclared(decl: Stmt, predeclared: &[String]) -> Stmt {
    match decl {
        Stmt::VarDecl {
            name,
            expr,
            type_constraint: None,
            bind: None,
            where_constraint: None,
            custom_traits,
            ..
        } if custom_traits.is_empty() && predeclared.contains(&name) => Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        },
        other => other,
    }
}
