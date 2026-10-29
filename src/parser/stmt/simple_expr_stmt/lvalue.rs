use crate::ast::{Expr, LvaluePeel, LvalueRoot, Stmt};
use crate::value::Value;

/// Extract the writeback variable name from a declaration used in expression
/// position as an lvalue-method target, e.g. `(my $o = $s).substr-rw(...) = ...`.
/// Without this the assignment would target a detached value and the mutation
/// would be lost. Handles both a bare `VarDecl` and the `SyntheticBlock`-wrapped
/// declarations the parser produces for `:=` binds and readonly scalar binds.
// Cost: O(s), s = statements of a SyntheticBlock.
pub(crate) fn decl_target_var_name(stmt: &crate::ast::Stmt) -> Option<String> {
    match stmt {
        crate::ast::Stmt::VarDecl { name, .. } => Some(name.clone()),
        crate::ast::Stmt::SyntheticBlock(inner) => inner.iter().find_map(|s| match s {
            crate::ast::Stmt::VarDecl { name, .. } => Some(name.clone()),
            _ => None,
        }),
        _ => None,
    }
}

/// The variable a method-call lvalue (`$s.substr-rw(0, 1) = "x"`,
/// `x.AT-KEY(k) = v` for a sigil-less `x`) writes the updated invocant back
/// through, or `None` when the invocant is not a variable. The parentheses
/// are transparent and a declaration is its variable: `(my $x = $s).substr-rw(...)
/// = $c` writes back through `$x`. A sigil-less invocant is keyed by its
/// spelling (the parser cannot tell a `constant` from a `my \x`).
// Cost: O(w + |name|), w = wrappers peeled.
pub(crate) fn method_lvalue_target_name(target: &Expr) -> Option<String> {
    target
        .lvalue_root(LvaluePeel::GROUPED | LvaluePeel::DECL | LvaluePeel::SIGILLESS)
        .map(LvalueRoot::into_spelled_key)
}

pub(super) fn bind_source_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) => expr.container_var_key(),
        // For indexed expressions like @a[1], encode as "@a\x00idx\x001"
        // so that binding can track the specific array/hash element.
        Expr::Index { .. } => expr.element_source_key(),
        // Inline declaration on the RHS of a bind, e.g. `@a[1] := my $x` or
        // `$y := my $x`. The `my $x` parses to a `DoStmt(VarDecl { .. })` whose
        // `name` already carries the sigil convention used by bind metadata
        // ("x" for `$x`, "@x" for `@x`, "%y" for `%y`). The declaration itself
        // runs in the enclosing scope when the bind value is evaluated; here we
        // only need to surface the declared variable's name so the bind sets up
        // cell sharing between the target and the freshly-declared variable.
        Expr::DoStmt(stmt) => match stmt.as_ref() {
            Stmt::VarDecl { name, .. } => Some(name.clone()),
            _ => None,
        },
        _ => None,
    }
}

/// The value an indexed bind (`@a[i] := v`, `%h<k> := v`) stores through
/// `IndexAssign`: the RHS marked as a bind value, so the compiler takes bind
/// (not assign) semantics. A compound `*` RHS stays a WhateverCode, as for any
/// finished value.
// Cost: O(n), n = size of the RHS.
pub(crate) fn bind_index_value(rhs: Expr) -> Expr {
    Expr::IndexBindValue(Box::new(crate::parser::expr::wrap_finished_expr(rhs)))
}

/// `TARGET[INDEX] := RHS` (or `{…}` when not `is_positional`), the one shape
/// every indexed-bind parse path builds.
// Cost: O(n), n = size of the RHS.
pub(crate) fn index_bind_expr(
    target: Box<Expr>,
    index: Box<Expr>,
    is_positional: bool,
    spelling: crate::ast::IndexSpelling,
    rhs: Expr,
) -> Expr {
    Expr::IndexAssign {
        target,
        index,
        value: Box::new(bind_index_value(rhs)),
        is_positional,
        spelling,
    }
}

pub(crate) fn bind_source_metadata_expr(rhs: &Expr) -> Expr {
    match rhs {
        Expr::ArrayLiteral(items) => Expr::ArrayLiteral(
            items
                .iter()
                .map(|item| {
                    if let Some(name) = bind_source_name(item) {
                        Expr::Literal(Value::str(name))
                    } else {
                        Expr::Literal(Value::NIL)
                    }
                })
                .collect(),
        ),
        other => {
            if let Some(name) = bind_source_name(other) {
                Expr::ArrayLiteral(vec![Expr::Literal(Value::str(name))])
            } else {
                Expr::ArrayLiteral(vec![Expr::Literal(Value::NIL)])
            }
        }
    }
}
