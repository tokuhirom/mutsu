use crate::ast::{AssignOp, Expr, LvaluePeel, LvalueRoot, Stmt};
use crate::symbol::Symbol;
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

pub(super) fn method_lvalue_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    // `$(EXPR) = value` lowers `$(EXPR)` to `EXPR.item`, but the item
    // contextualizer is transparent as an lvalue: it names the same container as
    // `EXPR`. Assign straight through to `EXPR` instead of dispatching a `.item`
    // method-lvalue (which has no writable slot and threw X::Assignment::RO).
    if method_name == "item" && method_args.is_empty() {
        return crate::parser::expr::precedence::assign_to_target_expr(target, value);
    }
    let mut args = vec![
        target,
        Expr::Literal(Value::str(method_name)),
        Expr::ArrayLiteral(method_args),
        value,
    ];
    args.push(match target_var_name {
        Some(name) => Expr::Literal(Value::str(name)),
        None => Expr::Literal(Value::NIL),
    });
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_method_lvalue"),
        args,
        listop: false,
    }
}

pub(super) fn named_sub_lvalue_assign_expr(
    name: String,
    call_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    crate::parser::stmt::assign::named_sub_lvalue_assign_expr(name, call_args, value)
}

pub(super) fn callable_lvalue_assign_expr(target: Expr, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
        args: vec![target, Expr::ArrayLiteral(call_args), value],
        listop: false,
    }
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
/// `IndexAssign`: the RHS wrapped in the `__mutsu_bind_index_value` marker, so
/// the VM takes bind (not assign) semantics. A compound `*` RHS stays a
/// WhateverCode, as for any finished value.
// Cost: O(n), n = size of the RHS.
pub(crate) fn bind_index_value(rhs: Expr) -> Expr {
    let rhs = crate::parser::expr::wrap_finished_expr(rhs);
    let source_meta = bind_source_metadata_expr(&rhs);
    Expr::Call {
        name: Symbol::intern("__mutsu_bind_index_value"),
        args: vec![rhs, source_meta],
        listop: false,
    }
}

/// `TARGET[INDEX] := RHS` (or `{…}` when not `is_positional`), the one shape
/// every indexed-bind parse path builds.
// Cost: O(n), n = size of the RHS.
pub(crate) fn index_bind_expr(
    target: Box<Expr>,
    index: Box<Expr>,
    is_positional: bool,
    rhs: Expr,
) -> Expr {
    Expr::IndexAssign {
        target,
        index,
        value: Box::new(bind_index_value(rhs)),
        is_positional,
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

pub(super) fn single_target_list_lvalue_stmt(lhs: Expr, rhs: Expr) -> Option<Stmt> {
    let Expr::ArrayLiteral(items) = lhs else {
        return None;
    };
    let mut saw_whatever = false;
    let mut lvalues: Vec<(usize, Expr)> = Vec::new();
    for (i, item) in items.iter().enumerate() {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push((i, item.clone()));
    }
    if !saw_whatever || lvalues.len() != 1 {
        return None;
    }
    let (pos, target) = lvalues.into_iter().next()?;
    // Extract the element at position `pos` from the RHS list for scalar targets
    let extracted_rhs = Expr::Index {
        target: Box::new(rhs.clone()),
        index: Box::new(Expr::Literal(Value::int(pos as i64))),
        is_positional: true,
    };
    Some(match target {
        Expr::Var(name) => Stmt::Assign {
            name,
            expr: extracted_rhs,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        },
        Expr::ArrayVar(name) => {
            // For array targets, pass the full RHS — __mutsu_star_lvalue_rhs
            // handles truncation based on bound array length.
            // If the * is trailing (array comes first), the array gets the full RHS.
            // If the * is leading (array comes last), we need to skip leading elements.
            let array_rhs = if pos > 0 {
                // Skip the first `pos` elements for the array target
                Expr::MethodCall {
                    target: Box::new(rhs),
                    name: Symbol::intern("skip"),
                    args: vec![Expr::Literal(Value::int(pos as i64))],
                    modifier: None,
                    quoted: false,
                }
            } else {
                rhs
            };
            Stmt::Assign {
                name: format!("@{}", name),
                expr: Expr::Call {
                    name: Symbol::intern("__mutsu_star_lvalue_rhs"),
                    args: vec![Expr::Literal(Value::str(format!("@{}", name))), array_rhs],
                    listop: false,
                },
                op: AssignOp::Assign,
                target_is_sigilless: false,
            }
        }
        Expr::HashVar(name) => Stmt::Assign {
            name: format!("%{}", name),
            expr: extracted_rhs,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
        } => Stmt::Expr(Expr::IndexAssign {
            target,
            index,
            value: Box::new(extracted_rhs),
            is_positional,
        }),
        _ => return None,
    })
}

pub(super) fn grouped_assign_lvalue_stmt(target: &Expr, rhs: Expr) -> Option<Stmt> {
    let Expr::Grouped(inner) = target else {
        return None;
    };
    let Expr::AssignExpr { name, .. } = inner.as_ref() else {
        return None;
    };
    Some(Stmt::Block(vec![
        Stmt::Expr((**inner).clone()),
        Stmt::Assign {
            name: name.clone(),
            expr: rhs,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        },
    ]))
}
