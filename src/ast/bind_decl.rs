//! The expansion of a binding declaration, `my $x := …` / `my @a := …` /
//! `my %h := …`.
//!
//! The compiler needs a binding declaration wrapped in bookkeeping statements
//! (`MarkBind`, `MarkReadonly`, the bound-array length record) chosen by the
//! sigil and the right-hand side. That wrapping used to be built inline by the
//! parser, so the RakuAST layer met only the wrapped form and refused it.
//! [`expand`] is now the one place the wrapping is built: the parser calls it,
//! and `rakuast::lower` hands it the declaration it lowers from
//! `VarDeclaration::Simple(initializer => Initializer::Bind(…))`
//! (ADR-10723 Stage 1). [`declaration`] goes the other way for the converter:
//! it accepts a statement only when it is exactly what [`expand`] builds for
//! the declaration inside it, so nothing is reverse-engineered from a shape
//! some other desugaring happens to share.

use std::hash::{Hash, Hasher};

use super::{Expr, ReadonlyKind, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// The internal trait marking a `$` declaration as bound (`:=`) rather than
/// assigned; the compiler reads it to bind instead of storing into a Scalar.
pub(crate) const SCALAR_BIND: &str = "__scalar_bind";

/// Whether a binding declaration of `name` (a `$` name carries no sigil) is a
/// scalar bind, the kind that carries [`SCALAR_BIND`].
pub(crate) fn is_scalar_bind_name(name: &str) -> bool {
    !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&')
}

/// The statement the compiler runs for the binding declaration `decl`, a
/// `Stmt::VarDecl` whose `expr` is the bound right-hand side (and which carries
/// [`SCALAR_BIND`] when [`is_scalar_bind_name`] says so).
// Cost: O(1).
pub(crate) fn expand(decl: Stmt) -> Stmt {
    let Stmt::VarDecl { name, expr, .. } = &decl else {
        return decl;
    };
    let bound_name = name.clone();
    let is_array = bound_name.starts_with('@');
    let is_hash = bound_name.starts_with('%');
    if is_array || is_hash {
        let mut stmts = Vec::new();
        if is_hash {
            // Record a dedicated bound-container marker so a later whole
            // reassignment (`%a = (...)`) is allowed (it propagates to the bound
            // source), while a `constant %M` — also readonly — stays immutable.
            stmts.push(Stmt::MarkBoundContainer(bound_name.clone()));
            stmts.push(Stmt::MarkBind);
        }
        stmts.push(decl);
        if is_hash {
            // AFTER the declaration: the declaration resets this bare name's
            // readonly state (so a stale marking from an earlier same-named
            // binding cannot poison it), which would erase a marking emitted
            // before it. See `vm_var_assign_set_local.rs`'s `is_vardecl` block.
            stmts.push(Stmt::MarkReadonly(
                bound_name.clone(),
                ReadonlyKind::ImmutableValue,
            ));
            // A `SyntheticBlock` yields its LAST statement's value, so re-read
            // the now-bound hash to keep `my %h := %src` usable in expression
            // position (mirrors the array branch's trailing read below).
            stmts.push(Stmt::Expr(Expr::Var(bound_name.clone())));
        }
        if is_array {
            stmts.push(Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_record_bound_array_len"),
                args: vec![Expr::Literal(Value::str(bound_name.clone()))],
            }));
            stmts.push(Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_record_shaped_array_dims"),
                args: vec![Expr::Literal(Value::str(bound_name.clone()))],
            }));
            // Return the bound variable so the expression evaluates to the
            // bound value (important for `+my @a := ...` which expects the
            // list count).
            stmts.push(Stmt::Expr(Expr::Var(bound_name)));
        }
        return Stmt::SyntheticBlock(stmts);
    }
    if matches!(expr, Expr::Literal(_)) {
        // Note: a declaration resets the bare name's readonly state (see
        // `vm_var_assign_set_local.rs`'s `is_vardecl` block), so this marking is
        // erased again by the declaration that follows it and is re-applied by
        // that same store's `bind_marks_immutable` arm — which covers exactly
        // the literal kinds accepted here. It is kept here (rather than moved
        // after the declaration) because a `SyntheticBlock` yields its LAST
        // statement's value, and `my $x := 5` must still evaluate to `5` in
        // expression position.
        return Stmt::SyntheticBlock(vec![
            Stmt::MarkReadonly(bound_name, ReadonlyKind::Immutable),
            decl,
        ]);
    }
    // A multi-dimensional subscript RHS (`my $x := @a[0;0;3]`) binds the leaf
    // element's container exactly like the single-dimension form, so it takes
    // the same `MarkBind` route: the compiler then emits `MultiDimIndexBindRef`
    // (via `compile_call_arg`) instead of a plain read, and a later `$x = v`
    // writes through to the real nested slot.
    if matches!(
        expr,
        Expr::Var(_) | Expr::Index { .. } | Expr::MultiDimIndex { .. }
    ) {
        return Stmt::SyntheticBlock(vec![Stmt::MarkBind, decl]);
    }
    decl
}

/// The binding declaration `stmt` is the [`expand`]ed form of, if it is one:
/// a `$` declaration carrying [`SCALAR_BIND`], or an `@`/`%` one, whose
/// expansion is exactly `stmt`.
// Cost: O(n), n = AST nodes under `stmt` (the expansion is rebuilt and hashed).
pub(crate) fn declaration(stmt: &Stmt) -> Option<&Stmt> {
    let decl = match stmt {
        Stmt::VarDecl { .. } => stmt,
        Stmt::SyntheticBlock(stmts) => {
            let mut decls = stmts.iter().filter(|s| matches!(s, Stmt::VarDecl { .. }));
            let decl = decls.next()?;
            if decls.next().is_some() {
                return None;
            }
            decl
        }
        _ => return None,
    };
    let Stmt::VarDecl {
        name,
        custom_traits,
        ..
    } = decl
    else {
        return None;
    };
    let is_bind = if is_scalar_bind_name(name) {
        custom_traits.iter().any(|(t, _)| t == SCALAR_BIND)
    } else {
        name.starts_with('@') || name.starts_with('%')
    };
    (is_bind && structural_hash(&expand(decl.clone())) == structural_hash(stmt)).then_some(decl)
}

/// A structural identity of `stmt` through the derived `Hash` impls, the same
/// identity the AST fingerprints use (#7822).
fn structural_hash(stmt: &Stmt) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    stmt.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn decl(name: &str, expr: Expr, traits: &[&str]) -> Stmt {
        Stmt::VarDecl {
            name: name.to_string(),
            expr,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: traits.iter().map(|t| (t.to_string(), None)).collect(),
            where_constraint: None,
        }
    }

    #[test]
    fn every_expansion_reads_back_as_its_declaration() {
        for d in [
            decl("x", Expr::Literal(Value::int(1)), &[SCALAR_BIND]),
            decl("x", Expr::Var("y".to_string()), &[SCALAR_BIND]),
            decl("x", Expr::BareWord("f".to_string()), &[SCALAR_BIND]),
            decl("@a", Expr::ArrayVar("b".to_string()), &[]),
            decl("%h", Expr::HashVar("s".to_string()), &[]),
        ] {
            let expanded = expand(d.clone());
            assert!(declaration(&expanded).is_some(), "{expanded:?}");
        }
    }

    #[test]
    fn an_assignment_or_a_foreign_block_is_not_a_binding() {
        // `my $x = $y`: no bind trait.
        let assign = decl("x", Expr::Var("y".to_string()), &["__has_initializer"]);
        assert!(declaration(&assign).is_none());
        // `my @a = …` is never wrapped the way a bind is.
        assert!(declaration(&decl("@a", Expr::ArrayVar("b".to_string()), &[])).is_none());
        // A block of the right shape around the wrong statement order.
        let d = decl("x", Expr::Var("y".to_string()), &[SCALAR_BIND]);
        assert!(declaration(&Stmt::SyntheticBlock(vec![d, Stmt::MarkBind])).is_none());
    }
}
