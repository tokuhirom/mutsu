//! A sigilless declaration, `my \x = 5` / `my Int \x := $s`.
//!
//! The parser builds one expansion for both spellings: the declaration wrapped
//! in the bookkeeping statements the compiler needs (`MarkSigillessReadonly`,
//! or `MarkBind` + `MarkSigilless` when the right-hand side can denote a
//! container). The spelling itself is not part of the expansion, and rakudo
//! renders the two differently (`Initializer::Assign` / `Initializer::Bind`),
//! so the declaration carries it as the [`SIGILLESS_DECL`] trait, whose
//! argument says whether it was written with `=`.
//!
//! [`declaration`] is the converter's way back: it accepts a statement only
//! when it has exactly the shape the parser builds, so a `with`/`given` pointy
//! binding, which uses the same bookkeeping statements, is never mistaken for
//! a declaration written in the source.

use super::{Expr, Stmt};

/// The internal trait marking a declaration built from `my \name = …` /
/// `my \name := …`; its argument is `True` for the `=` spelling.
pub(crate) const SIGILLESS_DECL: &str = "__sigilless_decl";

/// What a sigilless declaration says.
pub(crate) struct SigillessDecl<'a> {
    pub name: &'a str,
    pub expr: &'a Expr,
    pub type_constraint: Option<&'a str>,
    pub is_state: bool,
    pub is_our: bool,
    /// Written `my \x = …` rather than `my \x := …`.
    pub assigned: bool,
    /// The declaration the right-hand side was (`my \x = my $y = 3`): the
    /// parser declares it first and binds `x` to its variable, which `expr`
    /// then names.
    pub declarator: Option<&'a Stmt>,
    /// `my \name .= method` uses an `Initializer::CallAssign` in RakuAST.
    pub call_assign: Option<(&'a crate::symbol::Symbol, &'a [Expr])>,
}

/// The sigilless declaration `stmt` is the parser's expansion of, if it is one.
// Cost: O(t), t = custom traits of the declaration.
pub(crate) fn declaration(stmt: &Stmt) -> Option<SigillessDecl<'_>> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    // `my \x = my $y = 3`: the inner declaration, then the bind to `$y`.
    if let [
        declarator @ Stmt::VarDecl {
            name: inner,
            is_our: false,
            is_dynamic: false,
            ..
        },
        Stmt::SyntheticBlock(bind),
    ] = stmts.as_slice()
    {
        let decl = expansion(bind)?;
        return (matches!(decl.expr, Expr::Var(v) if v == inner)).then_some(SigillessDecl {
            declarator: Some(declarator),
            call_assign: None,
            ..decl
        });
    }
    if let Some(decl) = expansion(stmts) {
        return Some(decl);
    }
    // `my \name .= method` is stored as the initialized variable plus its
    // readonly marker. The parser's call-assignment expansion has no traits.
    let [
        Stmt::VarDecl {
            anonymous: _,
            name,
            expr:
                expr @ Expr::MethodCall {
                    target,
                    name: method,
                    args,
                    modifier: None,
                    quoted: false,
                    sugar: false,
                },
            type_constraint,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags,
            custom_traits,
            where_constraint: None,
        },
        Stmt::MarkSigillessReadonly(marked),
    ] = stmts.as_slice()
    else {
        return None;
    };
    // The invocant is the declared type, or `Mu` for an untyped declaration.
    let invocant = type_constraint.as_deref().unwrap_or("Mu");
    if name != marked
        || !matches!(&**target, Expr::BareWord(value) if value == invocant)
        || !export_tags.is_empty()
        || !custom_traits.is_empty()
    {
        return None;
    }
    Some(SigillessDecl {
        name,
        expr,
        type_constraint: type_constraint.as_deref(),
        is_state: false,
        is_our: false,
        assigned: true,
        declarator: None,
        call_assign: Some((method, args)),
    })
}

/// The statements of the parser's sigilless expansion, read back.
// Cost: O(t), t = custom traits of the declaration.
fn expansion(stmts: &[Stmt]) -> Option<SigillessDecl<'_>> {
    let (decl, marked) = match stmts {
        [
            decl @ Stmt::VarDecl { .. },
            Stmt::MarkSigillessReadonly(marked),
        ] => (decl, marked),
        [
            Stmt::MarkBind,
            decl @ Stmt::VarDecl { .. },
            Stmt::MarkSigilless(marked),
        ] => (decl, marked),
        _ => return None,
    };
    let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic: false,
        is_export: false,
        custom_traits,
        where_constraint: None,
        ..
    } = decl
    else {
        return None;
    };
    if name != marked {
        return None;
    }
    let mut assigned = None;
    for (trait_name, arg) in custom_traits {
        match (trait_name.as_str(), arg) {
            ("__has_initializer", None) => {}
            (SIGILLESS_DECL, Some(Expr::Literal(value))) => assigned = Some(value.truthy()),
            _ => return None,
        }
    }
    Some(SigillessDecl {
        name,
        expr,
        type_constraint: type_constraint.as_deref(),
        is_state: *is_state,
        is_our: *is_our,
        assigned: assigned?,
        declarator: None,
        call_assign: None,
    })
}
