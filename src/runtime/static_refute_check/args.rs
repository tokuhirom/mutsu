//! The arguments the CHECK-time refutation pass can judge: literals, core
//! type objects and variables declared once with a core type.

use std::collections::HashMap;

use crate::ast::{CallArg, Expr, ParamDef, Stmt};
use crate::value::{Value, ValueView};

use super::Survey;

/// The static value of a call argument, when it is one a refutation can be
/// judged on: a literal (`Int`, `Num`, `Rat`, `Str`, `Bool`, `Nil`), a core
/// type object, or a variable `typed_vars` gives a declared core type, which
/// stands for its type object (every value it can hold is of that type).
/// `None` for anything else.
// Cost: O(1).
pub(super) fn static_arg(arg: &Expr, typed_vars: &HashMap<&str, &str>) -> Option<Value> {
    match arg {
        Expr::Literal(value)
            if matches!(
                value.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Rat(..)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Nil
            ) =>
        {
            Some(value.clone())
        }
        // A core type object (`f(Str)`); a smiley (`Int:D`) is a run-time check.
        Expr::BareWord(name) => core_type_object(name),
        Expr::Var(name) => typed_vars.get(name.as_str()).and_then(|ty| core_type_object(ty)),
        _ => None,
    }
}

/// The type object of the core type `name`, unless it is a smiley or native.
// Cost: O(1).
fn core_type_object(name: &str) -> Option<Value> {
    (!name.contains(':')
        && crate::known_types::is_known_type_constraint(name)
        && !crate::native_types::is_native_type_name(name))
    .then(|| Value::package(crate::symbol::Symbol::intern(name)))
}

/// Whether `arg` can be a static argument at all (its value or type is
/// decided once the unit's declarations are known).
// Cost: O(1).
fn static_shaped(arg: &Expr) -> bool {
    matches!(arg, Expr::Literal(_) | Expr::BareWord(_) | Expr::Var(_))
}

/// Whether `arg` is the line marker the parser adds to a call (not an argument).
// Cost: O(1).
fn is_callsite_marker(arg: &Expr) -> bool {
    matches!(arg, Expr::Binary { left, op: crate::token_kind::TokenKind::FatArrow, .. }
        if matches!(left.as_ref(), Expr::Literal(key)
            if matches!(key.view(), ValueView::Str(key)
                if key.as_str() == crate::parser::TEST_CALLSITE_LINE_KEY)))
}

// Cost: O(a), a = arguments.
pub(super) fn static_expr_args(args: &[Expr]) -> Option<Vec<&Expr>> {
    args.iter()
        .filter(|arg| !is_callsite_marker(arg))
        .map(|arg| static_shaped(arg).then_some(arg))
        .collect()
}

// Cost: O(a), a = arguments.
pub(super) fn static_call_args(args: &[CallArg]) -> Option<Vec<&Expr>> {
    args.iter()
        .filter_map(|arg| match arg {
            CallArg::Named { name, .. } if name == crate::parser::TEST_CALLSITE_LINE_KEY => None,
            CallArg::Positional(expr) if is_callsite_marker(expr) => None,
            CallArg::Positional(expr) => Some(static_shaped(expr).then_some(expr)),
            _ => Some(None),
        })
        .collect()
}

/// The type a scalar declaration gives its variable, when the variable is
/// one an argument can be judged by: a named `my`/`our`/`state` scalar
/// (`my Str $s`) at any depth. Its name and type; `Str:D` holds only `Str`s
/// too. A dynamic or twigilled variable, the topic and an anonymous one are
/// not.
// Cost: O(t), t = length of the name and type.
pub(super) fn typed_var_decl(stmt: &Stmt) -> Option<(&str, &str)> {
    match stmt {
        Stmt::VarDecl {
            name,
            type_constraint: Some(ty),
            anonymous: false,
            is_dynamic: false,
            ..
        } => typed_scalar(name, ty),
        _ => None,
    }
}

/// The type a scalar parameter gives its variable inside the routine or
/// block body (`sub g(Str $s)`), as [`typed_var_decl`] does for a variable:
/// a value bound to it, or assigned to it later, is of that type. A
/// sigilless parameter is a term, not a variable.
// Cost: O(t), t = length of the name and type.
pub(super) fn typed_param(param: &ParamDef) -> Option<(&str, &str)> {
    if param.sigilless || param.slurpy || param.double_slurpy {
        return None;
    }
    typed_scalar(&param.name, param.type_constraint.as_deref()?)
}

// Cost: O(t), t = length of the name and type.
fn typed_scalar<'a>(name: &'a str, ty: &'a str) -> Option<(&'a str, &'a str)> {
    let plain = name != "_"
        && name
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_');
    plain.then(|| (name, ty.strip_suffix(":D").unwrap_or(ty)))
}

/// The typed scalars the survey found ([`typed_var_decl`], [`typed_param`]),
/// by name, when the name is declared nowhere else in the unit: every use of
/// it then reads that variable, whose values are all of its type.
// Cost: O(v), v = typed declarations.
pub(super) fn typed_vars<'ast>(survey: &Survey<'ast>) -> HashMap<&'ast str, &'ast str> {
    survey
        .typed
        .iter()
        .filter(|(name, _)| survey.var_decls.get(*name) == Some(&1))
        .copied()
        .collect()
}
