//! The expansion of a binding declaration, `my $x := …` / `my @a := …` /
//! `my %h := …`.
//!
//! The compiler needs a binding declaration wrapped in bookkeeping statements
//! (`VarMarks::bound`, `MarkReadonly`, the bound-array length record) chosen by the
//! sigil and the right-hand side. The parser keeps the declaration as written
//! (`ParsedStmt::BindDecl`, ADR-10723 Stage 2) and [`expand`] is the one place
//! the wrapping is built: the lowering of that node calls it, for the parser's
//! node and for a hand-built
//! `VarDeclaration::Simple(initializer => Initializer::Bind(…))` alike.
//! [`declared`] reads the compiler's input form for passes that need the
//! declaration inside it.

use super::{Expr, ReadonlyKind, Stmt};

/// What a binding declaration (`:=`) binds, recorded on `Stmt::VarDecl::bind`
/// (ADR-10723 Stage 2: a field of the declaration, not a marker trait).
/// `@` and `%` binds carry none: their wrapping statements say so.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    serde::Serialize,
    serde::Deserialize,
    bincode::Encode,
    bincode::Decode,
)]
pub(crate) enum BindKind {
    /// `my $x := …`: the compiler binds instead of storing into a Scalar.
    Scalar,
    /// `my &f := …`: a callable declaration, whose execution needs no
    /// scalar-container or aggregate binding bookkeeping.
    Code,
}

/// Whether a binding declaration of `name` (a `$` name carries no sigil) is a
/// scalar bind, the kind recorded as [`BindKind::Scalar`].
pub(crate) fn is_scalar_bind_name(name: &str) -> bool {
    !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&')
}

/// The statement the compiler runs for the binding declaration `decl`, a
/// `Stmt::VarDecl` whose `expr` is the bound right-hand side (and whose `bind` is
/// [`BindKind::Scalar`] when [`is_scalar_bind_name`] says so).
// Cost: O(n), n = name length.
pub(crate) fn expand(mut decl: Stmt) -> Stmt {
    if let Stmt::VarDecl { name, bind, .. } = &mut decl
        && name.starts_with('&')
    {
        *bind = Some(BindKind::Code);
    }
    let Stmt::VarDecl { name, expr, .. } = &decl else {
        return decl;
    };
    let bound_name = name.clone();
    let is_array = bound_name.starts_with('@');
    let is_hash = bound_name.starts_with('%');
    if is_array || is_hash {
        // The bound mark tells the compiler the declaration binds (keeps the
        // bound container's type: a List stays a List).
        let mut stmts = vec![super::var_marks::with_bound_mark(decl)];
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
    // the same bound-declaration route: the compiler then emits `MultiDimIndexBindRef`
    // (via `compile_call_arg`) instead of a plain read, and a later `$x = v`
    // writes through to the real nested slot.
    // A conditional whose every branch is such a container binds the branch
    // it selects (`my $x := $c ?? @a[0] !! @a[1]`).
    if binds_a_container(expr) {
        return Stmt::SyntheticBlock(vec![super::var_marks::with_bound_mark(decl)]);
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
        Stmt::VarDecl { name, bind, .. } => {
            let expected = if name.starts_with('&') {
                BindKind::Code
            } else {
                BindKind::Scalar
            };
            (is_scalar_bind_name(name) || name.starts_with('&'))
                .then_some(())
                .filter(|_| *bind == Some(expected))
                .map(|_| stmt)
        }
        Stmt::SyntheticBlock(stmts) => {
            if !matches!(stmts.first(), Some(Stmt::MarkReadonly(..)))
                && !super::var_marks::is_bind_group(stmts)
            {
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

    fn decl(name: &str, expr: Expr, bind: Option<BindKind>) -> Stmt {
        Stmt::VarDecl {
            marks: Default::default(),
            anonymous: false,
            name: name.to_string(),
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
            bind,
            where_constraint: None,
        }
    }

    #[test]
    fn every_expansion_reads_back_as_its_declaration() {
        for d in [
            decl("x", Expr::Literal(Value::int(1)), Some(BindKind::Scalar)),
            decl("x", Expr::Var("y".to_string()), Some(BindKind::Scalar)),
            decl("x", Expr::BareWord("f".to_string()), Some(BindKind::Scalar)),
            decl("@a", Expr::ArrayVar("b".to_string()), None),
            decl("%h", Expr::HashVar("s".to_string()), None),
            decl(
                "&f",
                Expr::CodeVar("source".to_string()),
                Some(BindKind::Code),
            ),
        ] {
            let expanded = expand(d.clone());
            assert!(declared(&expanded).is_some(), "{expanded:?}");
        }
    }

    #[test]
    fn an_assignment_is_not_a_binding() {
        // `my $x = $y`: no bind trait.
        let assign = decl("x", Expr::Var("y".to_string()), None);
        assert!(declared(&assign).is_none());
        let code_assign = decl("&f", Expr::CodeVar("source".to_string()), None);
        assert!(declared(&code_assign).is_none());
        // `my @a = …` is never wrapped the way a bind is.
        assert!(declared(&decl("@a", Expr::ArrayVar("b".to_string()), None)).is_none());
    }
}
