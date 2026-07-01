use super::*;

pub(crate) fn method_lvalue_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    // `$(EXPR) = value` lowers `$(EXPR)` to `EXPR.item`, but the item
    // contextualizer is transparent as an lvalue: it names the same container as
    // `EXPR`. Assign straight through to `EXPR` (`$(@a[0]) = ...` writes `@a[0]`)
    // instead of dispatching a `.item` method-lvalue, which has no writable slot
    // and threw X::Assignment::RO.
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
        None => Expr::Literal(Value::Nil),
    });
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_method_lvalue"),
        args,
    }
}

pub(crate) fn named_sub_lvalue_assign_expr(
    name: String,
    call_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
        args: vec![
            Expr::Literal(Value::str(name)),
            Expr::ArrayLiteral(call_args),
            value,
        ],
    }
}

pub(crate) fn callable_lvalue_assign_expr(target: Expr, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
        args: vec![target, Expr::ArrayLiteral(call_args), value],
    }
}

pub(crate) fn subscript_adverb_lvalue_assign_expr(lhs: Expr, rhs: Expr) -> Option<Expr> {
    fn subscript_parts(expr: &Expr) -> Option<(Expr, Expr, String)> {
        let Expr::Call { name, args } = expr else {
            return None;
        };
        if name != "__mutsu_subscript_adverb" || args.len() < 3 {
            return None;
        }
        let Expr::Literal(Value::Str(mode)) = &args[2] else {
            return None;
        };
        Some((args[0].clone(), args[1].clone(), mode.to_string()))
    }

    match lhs {
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } if name == "value" && args.is_empty() => {
            if let Some((base_target, base_index, mode)) = subscript_parts(target.as_ref())
                && (mode == "p" || mode == "not-p")
            {
                return Some(Expr::IndexAssign {
                    target: Box::new(base_target),
                    index: Box::new(base_index),
                    value: Box::new(rhs),
                    is_positional: true,
                });
            }
            None
        }
        Expr::Index { target, index, .. } => {
            let (base_target, base_index, mode) = subscript_parts(target.as_ref())?;
            if mode != "kv" && mode != "not-kv" {
                return None;
            }
            if !matches!(*index, Expr::Literal(Value::Int(1))) {
                return None;
            }
            Some(Expr::IndexAssign {
                target: Box::new(base_target),
                index: Box::new(base_index),
                value: Box::new(rhs),
                is_positional: true,
            })
        }
        _ => None,
    }
}

pub(crate) fn list_lvalue_assign_expr(items: Vec<Expr>, rhs: Expr) -> Option<Expr> {
    let mut saw_whatever = false;
    let mut lvalues: Vec<(usize, Expr)> = Vec::new();
    for (i, item) in items.into_iter().enumerate() {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push((i, item));
    }

    if lvalues.len() != 1 {
        return None;
    }
    if !saw_whatever {
        return None;
    }

    let (pos, target) = lvalues.into_iter().next()?;
    // A single scalar target in a `*`-slurpy list assignment takes exactly ONE
    // item, the one at its position (`($a, *) = 1, 2, 3` gives `$a == 1`), not
    // the whole list. Mirrors the statement-level `single_target_list_lvalue_stmt`.
    let extracted_rhs = Expr::Index {
        target: Box::new(rhs.clone()),
        index: Box::new(Expr::Literal(Value::Int(pos as i64))),
        is_positional: true,
    };
    match target {
        Expr::Var(name) => Some(Expr::AssignExpr {
            name,
            expr: Box::new(extracted_rhs),
            is_bind: false,
        }),
        Expr::ArrayVar(name) => {
            // An array target slurps the tail: skip the leading `pos` items.
            let array_rhs = if pos > 0 {
                Expr::MethodCall {
                    target: Box::new(rhs),
                    name: Symbol::intern("skip"),
                    args: vec![Expr::Literal(Value::Int(pos as i64))],
                    modifier: None,
                    quoted: false,
                }
            } else {
                rhs
            };
            Some(Expr::AssignExpr {
                name: format!("@{}", name),
                expr: Box::new(array_rhs),
                is_bind: false,
            })
        }
        Expr::HashVar(name) => Some(Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(extracted_rhs),
            is_bind: false,
        }),
        Expr::Index {
            target,
            index,
            is_positional,
        } => Some(Expr::IndexAssign {
            target,
            index,
            value: Box::new(extracted_rhs),
            is_positional,
        }),
        _ => None,
    }
}
