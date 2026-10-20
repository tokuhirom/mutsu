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
        anonymous: false,
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
        type_constraint,
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
        // A `:D` variable's cell holds its type object until the run-time
        // assignment, which the constraint rejects. The cell is storage only;
        // the variable itself keeps the constraint.
        let definite = crate::runtime::phasers::BEGIN_STATIC_DEFINITE_TRAIT;
        if custom_traits.iter().any(|(t, _)| t == definite) {
            custom_traits.retain(|(t, _)| t != definite);
            *type_constraint = None;
        }
    }
    decl
}

/// A cell for a variable whose type constraint names a type of an inner scope
/// (`my K $v`), which does not exist at the unit's level where the cell is
/// declared. The cell drops the constraint (and any `where`). A scalar cell
/// defaults to `Nil` (`is default(Nil)`), so a cell nothing has stored into
/// hands the variable `Nil`, which resets it to its own default, the type
/// object, as the typed cell would have.
pub(super) fn untyped_cell_decl(mut decl: Stmt) -> Stmt {
    if let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        where_constraint,
        custom_traits,
        ..
    } = &mut decl
    {
        *type_constraint = None;
        *where_constraint = None;
        if sigil_of(name).is_empty() {
            *expr = Expr::Literal(crate::value::Value::NIL);
            custom_traits.retain(|(t, _)| t != crate::runtime::phasers::BEGIN_STATIC_DEFINITE_TRAIT);
            custom_traits.push((
                "default".to_string(),
                Some(Expr::Literal(crate::value::Value::NIL)),
            ));
        }
    }
    decl
}

/// The variable's declaration, initialized from its cell. A `:D` variable's
/// static state is its nominal type object, which an untyped cell
/// ([`untyped_cell_decl`]) does not hold until something is stored into it, so
/// it falls back to that.
///
/// With `bind`, the declaration binds the cell's container instead of copying
/// it (`my @a := @cell`). That is what an `@`/`%` lexical gets: rakudo's pad
/// clone hands every frame the static pad's own Array or Hash, so a BEGIN's
/// push and every run's push land in one object (#10333). A scalar is copied,
/// since its pad clone is a fresh Scalar holding the static value.
pub(super) fn decl_from_cell(static_decl: &Stmt, cell_name: &str, bind: bool) -> Stmt {
    let mut decl = static_decl.clone();
    if let Stmt::VarDecl {
        expr,
        custom_traits,
        ..
    } = &mut decl
    {
        let definite = crate::runtime::phasers::BEGIN_STATIC_DEFINITE_TRAIT;
        *expr = if custom_traits.iter().any(|(t, _)| t == definite) {
            Expr::Binary {
                left: Box::new(read_var(cell_name)),
                op: crate::token_kind::TokenKind::SlashSlash,
                right: Box::new(expr.clone()),
                form: crate::ast::BinaryForm::Infix,
            }
        } else {
            read_var(cell_name)
        };
        let mut traits = without_initializer_markers(custom_traits);
        if !bind {
            traits.push(("__has_initializer".to_string(), None));
        }
        traits.push((
            crate::runtime::phasers::BEGIN_STATIC_TRAIT.to_string(),
            None,
        ));
        *custom_traits = traits;
    }
    if bind {
        // The same bookkeeping the parser wraps around `my @a := …`.
        crate::ast::bind_decl::expand(decl)
    } else {
        decl
    }
}

/// Reads a value slot the way the BEGIN's own value would be read: the slot is
/// a scalar, so it is decontainerized (`$slot<>`). Otherwise
/// `my str @hex = BEGIN (^256)>>.fmt("%02x")` would assign one itemized list.
pub(in crate::runtime::begin_prologue) fn slot_read(slot: String) -> Expr {
    Expr::ZenSlice(Box::new(Expr::Var(slot)))
}
