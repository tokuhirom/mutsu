//! The expansion of a binding declaration, `my $x := …` / `my @a := …` /
//! `my %h := …`.
//!
//! The compiler needs a binding declaration wrapped in bookkeeping statements
//! (`MarkBind`, `MarkReadonly`, the bound-array length record) chosen by the
//! sigil and the right-hand side. The parser keeps the declaration as written
//! (`ParsedStmt::BindDecl`, ADR-10723 Stage 2) and [`expand`] is the one place
//! the wrapping is built: the lowering of that node calls it, for the parser's
//! node and for a hand-built
//! `VarDeclaration::Simple(initializer => Initializer::Bind(…))` alike.
//! [`declared`] reads the compiler's input form for passes that need the
//! declaration inside it.

use super::{Expr, ReadonlyKind, Stmt};

/// The internal trait marking a `$` declaration as bound (`:=`) rather than
/// assigned; the compiler reads it to bind instead of storing into a Scalar.
pub(crate) const SCALAR_BIND: &str = "__scalar_bind";

/// Source binding intent for a callable declaration, whose execution needs
/// no scalar-container or aggregate binding bookkeeping.
pub(crate) const CODE_BIND: &str = "__code_bind";

/// Whether a binding declaration of `name` (a `$` name carries no sigil) is a
/// scalar bind, the kind that carries [`SCALAR_BIND`].
pub(crate) fn is_scalar_bind_name(name: &str) -> bool {
    !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&')
}

/// The statement the compiler runs for the binding declaration `decl`, a
/// `Stmt::VarDecl` whose `expr` is the bound right-hand side (and which carries
/// [`SCALAR_BIND`] when [`is_scalar_bind_name`] says so).
// Cost: O(n + t), n = name length, t = custom traits on a callable declaration.
pub(crate) fn expand(mut decl: Stmt) -> Stmt {
    if let Stmt::VarDecl {
        name,
        custom_traits,
        ..
    } = &mut decl
        && name.starts_with('&')
        && !custom_traits.iter().any(|(name, _)| name == CODE_BIND)
    {
        custom_traits.push((CODE_BIND.to_string(), None));
    }
    let Stmt::VarDecl { name, expr, .. } = &decl else {
        return decl;
    };
    let bound_name = name.clone();
    let is_array = bound_name.starts_with('@');
    let is_hash = bound_name.starts_with('%');
    if is_array || is_hash {
        let mut stmts = Vec::new();
        // The bind marker tells the compiler the declaration binds (keeps the
        // bound container's type: a List stays a List).
        stmts.push(Stmt::MarkBind);
        stmts.push(decl);
        if is_hash {
            // A `SyntheticBlock` yields its LAST statement's value, so re-read
            // the now-bound hash to keep `my %h := %src` usable in expression
            // position (mirrors the array branch's trailing read below).
            stmts.push(Stmt::Expr(Expr::Var(bound_name.clone())));
        }
        if is_array {
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
    // A conditional whose every branch is such a container binds the branch
    // it selects (`my $x := $c ?? @a[0] !! @a[1]`).
    if binds_a_container(expr) {
        return Stmt::SyntheticBlock(vec![Stmt::MarkBind, decl]);
    }
    decl
}

/// Whether binding to `expr` aliases an existing container: a variable, an
/// element, or a conditional choosing between such.
// Cost: O(b), b = branches of the conditional.
fn binds_a_container(expr: &Expr) -> bool {
    let mut pending = vec![expr];
    while let Some(branch) = pending.pop() {
        match branch {
            Expr::Var(_) | Expr::Index { .. } | Expr::MultiDimIndex { .. } => {}
            Expr::Ternary {
                then_expr,
                else_expr,
                ..
            } => pending.extend([then_expr.as_ref(), else_expr.as_ref()]),
            _ => return false,
        }
    }
    true
}

/// The `VarDecl` inside the lowered form of a binding declaration (see
/// [`expand`]); a pass over the compiler's input uses it to find what a bind
/// declares. A plain `VarDecl` is its own declaration.
// Cost: O(s), s = statements of the expansion (at most four).
pub(crate) fn declared(stmt: &Stmt) -> Option<&Stmt> {
    match stmt {
        // A bare `$` / `&` bind carries its marker trait; `@` and `%` binds
        // are always wrapped.
        Stmt::VarDecl {
            name,
            custom_traits,
            ..
        } => {
            let marker = if name.starts_with('&') {
                CODE_BIND
            } else {
                SCALAR_BIND
            };
            (is_scalar_bind_name(name) || name.starts_with('&'))
                .then(|| custom_traits.iter().any(|(t, _)| t == marker))
                .filter(|bound| *bound)
                .map(|_| stmt)
        }
        Stmt::SyntheticBlock(stmts) => {
            if !matches!(stmts.first(), Some(Stmt::MarkBind | Stmt::MarkReadonly(..))) {
                return None;
            }
            let mut decls = stmts.iter().filter(|s| matches!(s, Stmt::VarDecl { .. }));
            let decl = decls.next()?;
            decls.next().is_none().then_some(decl)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    fn decl(name: &str, expr: Expr, traits: &[&str]) -> Stmt {
        Stmt::VarDecl {
            anonymous: false,
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
            decl("&f", Expr::CodeVar("source".to_string()), &[CODE_BIND]),
        ] {
            let expanded = expand(d.clone());
            assert!(declared(&expanded).is_some(), "{expanded:?}");
        }
    }

    #[test]
    fn an_assignment_is_not_a_binding() {
        // `my $x = $y`: no bind trait.
        let assign = decl("x", Expr::Var("y".to_string()), &[]);
        assert!(declared(&assign).is_none());
        let code_assign = decl(
            "&f",
            Expr::CodeVar("source".to_string()),
            &[],
        );
        assert!(declared(&code_assign).is_none());
        // `my @a = …` is never wrapped the way a bind is.
        assert!(declared(&decl("@a", Expr::ArrayVar("b".to_string()), &[])).is_none());
    }
}
