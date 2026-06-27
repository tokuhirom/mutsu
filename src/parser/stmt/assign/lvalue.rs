use super::*;

pub(crate) fn method_lvalue_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
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
    let mut lvalues: Vec<Expr> = Vec::new();
    for item in items {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push(item);
    }

    if lvalues.len() != 1 {
        return None;
    }
    if !saw_whatever {
        return None;
    }

    match lvalues.into_iter().next()? {
        Expr::Var(name) => Some(Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
            is_bind: false,
        }),
        Expr::ArrayVar(name) => Some(Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(rhs),
            is_bind: false,
        }),
        Expr::HashVar(name) => Some(Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(rhs),
            is_bind: false,
        }),
        Expr::Index {
            target,
            index,
            is_positional,
        } => Some(Expr::IndexAssign {
            target,
            index,
            value: Box::new(rhs),
            is_positional,
        }),
        _ => None,
    }
}
