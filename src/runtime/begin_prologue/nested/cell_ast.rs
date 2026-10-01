//! The AST a lifted BEGIN is built from: the declarations of its cells and
//! value slots, and the reads of both (ADR-0134, slice 2).

use crate::ast::{Expr, Stmt};

pub(super) fn sigil_of(name: &str) -> &str {
    match name.as_bytes().first() {
        Some(b'@') => "@",
        Some(b'%') => "%",
        Some(b'&') => "&",
        _ => "",
    }
}

/// The expression reading variable `name` (in `VarDecl` naming).
pub(super) fn read_var(name: &str) -> Expr {
    match name.as_bytes().first() {
        Some(b'@') => Expr::ArrayVar(name[1..].to_string()),
        Some(b'%') => Expr::HashVar(name[1..].to_string()),
        Some(b'&') => Expr::CodeVar(name[1..].to_string()),
        _ => Expr::Var(name.to_string()),
    }
}

pub(super) fn static_scalar(name: &str) -> Stmt {
    Stmt::VarDecl {
        name: name.to_string(),
        expr: Expr::Literal(crate::value::Value::NIL),
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: vec![],
        custom_traits: vec![],
        where_constraint: None,
    }
}

/// A fresh declaration of `name` as an unbound parameter looks at BEGIN time.
pub(super) fn unbound_decl(name: &str) -> Stmt {
    let decl = static_scalar(name);
    crate::runtime::phasers::split_var_decl(&decl)
        .map(|(static_decl, _)| static_decl)
        .unwrap_or(decl)
}

pub(super) fn without_initializer_markers(
    traits: &[(String, Option<Expr>)],
) -> Vec<(String, Option<Expr>)> {
    traits
        .iter()
        .filter(|(t, _)| t != "__has_initializer" && t != "__scalar_bind")
        .cloned()
        .collect()
}

/// The cell's own static declaration: the variable's, under the cell's name.
pub(super) fn renamed_static_decl(static_decl: &Stmt, cell_name: &str) -> Stmt {
    let mut decl = static_decl.clone();
    if let Stmt::VarDecl {
        name,
        is_export,
        export_tags,
        custom_traits,
        ..
    } = &mut decl
    {
        *name = cell_name.to_string();
        *is_export = false;
        export_tags.clear();
        *custom_traits = without_initializer_markers(custom_traits);
    }
    decl
}

/// The variable's declaration, initialized from its cell.
pub(super) fn decl_from_cell(static_decl: &Stmt, cell_name: &str) -> Stmt {
    let mut decl = static_decl.clone();
    if let Stmt::VarDecl {
        expr,
        custom_traits,
        ..
    } = &mut decl
    {
        *expr = read_var(cell_name);
        let mut traits = without_initializer_markers(custom_traits);
        traits.push(("__has_initializer".to_string(), None));
        traits.push((
            crate::runtime::phasers::BEGIN_STATIC_TRAIT.to_string(),
            None,
        ));
        *custom_traits = traits;
    }
    decl
}

/// Reads a value slot the way the BEGIN's own value would be read: the slot is
/// a scalar, so it is decontainerized (`$slot<>`). Otherwise
/// `my str @hex = BEGIN (^256)>>.fmt("%02x")` would assign one itemized list.
pub(super) fn slot_read(slot: String) -> Expr {
    Expr::MethodCall {
        target: Box::new(Expr::Var(slot)),
        name: crate::symbol::Symbol::intern("__mutsu_zen_angle"),
        args: vec![],
        modifier: None,
        quoted: false,
    }
}
