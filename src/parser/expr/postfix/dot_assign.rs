use super::call_method::{QuotedMethodName, parse_quoted_method_name};
use crate::ast::{Expr, Stmt};
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};
use crate::parser::primary::{colonpair_expr, parse_call_arg_list, primary};
use crate::symbol::Symbol;

pub(crate) fn atomic_var_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(name) => Some(name.clone()),
        _ => None,
    }
}

/// The writeback variable name (`AssignExpr`-convention: `x` / `@a` / `%h`) for an
/// expression whose value is a simple-variable lvalue, or `None` otherwise. Used to
/// route an outer `.=` through a `do { … }`-block target back to its lvalue.
fn lvalue_assign_name(e: &Expr) -> Option<String> {
    match e {
        Expr::Grouped(inner) => lvalue_assign_name(inner),
        Expr::AssignExpr { name, .. } => Some(name.clone()),
        Expr::Var(n) => Some(n.clone()),
        Expr::ArrayVar(n) => Some(format!("@{}", n)),
        Expr::HashVar(n) => Some(format!("%{}", n)),
        _ => None,
    }
}

/// Wrap a `.=` method call result in the appropriate assignment expression.
/// For simple variables, generates `AssignExpr { name, expr }`.
/// For index expressions, generates an `IndexAssign` wrapped in a `DoBlock`.
/// For non-lvalue targets, returns the method call as-is.
pub(crate) fn wrap_dot_assign(target: Expr, method_call_fn: impl FnOnce(Expr) -> Expr) -> Expr {
    // Unwrap Grouped to get at the inner expression
    let target = match target {
        Expr::Grouped(inner) => *inner,
        other => other,
    };
    match &target {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(method_call_fn(target)),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(method_call_fn(target)),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(method_call_fn(target)),
            is_bind: false,
        },
        Expr::Index {
            target: idx_target,
            index,
            is_positional,
        } => {
            use std::sync::atomic::Ordering;
            let tmp_idx = format!(
                "__mutsu_idx_{}",
                crate::parser::stmt::simple::TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let tmp_idx_expr = Expr::Var(tmp_idx.clone());
            let lhs_expr = Expr::Index {
                target: idx_target.clone(),
                index: Box::new(tmp_idx_expr.clone()),
                is_positional: *is_positional,
            };
            let assigned_value = method_call_fn(lhs_expr);
            Expr::DoBlock {
                body: vec![
                    Stmt::VarDecl {
                        name: tmp_idx.clone(),
                        expr: *index.clone(),
                        type_constraint: None,
                        is_state: false,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                        custom_traits: Vec::new(),
                        where_constraint: None,
                    },
                    Stmt::Expr(Expr::IndexAssign {
                        target: idx_target.clone(),
                        index: Box::new(tmp_idx_expr),
                        value: Box::new(assigned_value),
                        is_positional: *is_positional,
                    }),
                ],
                label: None,
            }
        }
        // ($var = expr).=method => evaluate the assignment, then $var = $var.method
        Expr::AssignExpr { name, .. } => {
            let assign_name = name.clone();
            let var_expr = if let Some(rest) = assign_name.strip_prefix('@') {
                Expr::ArrayVar(rest.to_string())
            } else if let Some(rest) = assign_name.strip_prefix('%') {
                Expr::HashVar(rest.to_string())
            } else {
                Expr::Var(assign_name.clone())
            };
            let method_result = method_call_fn(var_expr);
            Expr::DoBlock {
                body: vec![
                    Stmt::Expr(target),
                    Stmt::Expr(Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(method_result),
                        is_bind: false,
                    }),
                ],
                label: None,
            }
        }
        // An inline declaration target (`(my Int $x .= new).= new: 42`) parses to
        // `DoStmt(VarDecl)`. Run the declaration (which declares and initializes the
        // variable), then assign the method result back to the just-declared
        // variable so the outer `.=` writes through, mirroring the AssignExpr case.
        Expr::DoStmt(stmt) if matches!(stmt.as_ref(), Stmt::VarDecl { .. }) => {
            let decl_name = match stmt.as_ref() {
                Stmt::VarDecl { name, .. } => name.clone(),
                _ => unreachable!(),
            };
            let var_expr = if let Some(rest) = decl_name.strip_prefix('@') {
                Expr::ArrayVar(rest.to_string())
            } else if let Some(rest) = decl_name.strip_prefix('%') {
                Expr::HashVar(rest.to_string())
            } else {
                Expr::Var(decl_name.clone())
            };
            let method_result = method_call_fn(var_expr);
            Expr::DoBlock {
                body: vec![
                    Stmt::Expr(target),
                    Stmt::Expr(Expr::AssignExpr {
                        name: decl_name,
                        expr: Box::new(method_result),
                        is_bind: false,
                    }),
                ],
                label: None,
            }
        }
        // A `do { … }` block whose value is an lvalue (`do { …; ($x .= new) }.= new`)
        // writes the outer `.=` result back to that lvalue. Run the block once (its
        // side effects included), then assign `$x = $x.method`.
        Expr::DoBlock { body, .. }
            if body
                .last()
                .and_then(|s| match s {
                    Stmt::Expr(e) => lvalue_assign_name(e),
                    _ => None,
                })
                .is_some() =>
        {
            let assign_name = match body.last() {
                Some(Stmt::Expr(e)) => lvalue_assign_name(e).unwrap(),
                _ => unreachable!(),
            };
            let var_expr = if let Some(rest) = assign_name.strip_prefix('@') {
                Expr::ArrayVar(rest.to_string())
            } else if let Some(rest) = assign_name.strip_prefix('%') {
                Expr::HashVar(rest.to_string())
            } else {
                Expr::Var(assign_name.clone())
            };
            let method_result = method_call_fn(var_expr);
            Expr::DoBlock {
                body: vec![
                    Stmt::Expr(target),
                    Stmt::Expr(Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(method_result),
                        is_bind: false,
                    }),
                ],
                label: None,
            }
        }
        // A parenthesized list of lvalues (`($x, $y) .= reverse`) applies the
        // method to the WHOLE list and assigns the result back element-wise
        // (`($x, $y) = ($x, $y).reverse`), not per-element.
        Expr::ArrayLiteral(_) => {
            let lhs = target.clone();
            let value = method_call_fn(target);
            Expr::Call {
                name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                args: vec![lhs, Expr::ArrayLiteral(Vec::new()), value],
            }
        }
        _ => method_call_fn(target),
    }
}

/// Parse `.=` mutating method call after the `=` has been identified.
/// `input` starts after the `=` character. `expr` is the target expression.
/// Kept as a separate function to avoid bloating the postfix loop's stack frame.
#[inline(never)]
pub(crate) fn parse_dot_assign<'a>(input: &'a str, expr: Expr) -> PResult<'a, Expr> {
    let (r, _) = ws(input)?;
    // Parse quoted method name
    if let Some((r_after, qname)) = parse_quoted_method_name(r) {
        let (r_after, _) = ws(r_after)?;
        let (r_final, args) = if r_after.starts_with('(') {
            let (r2, _) = parse_char(r_after, '(')?;
            let (r2, _) = ws(r2)?;
            let (r2, a) = parse_call_arg_list(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, _) = parse_char(r2, ')')?;
            (r2, a)
        } else {
            (r_after, vec![])
        };
        let result = match qname {
            QuotedMethodName::Static(mname) => {
                let mname_sym = Symbol::intern(&mname);
                wrap_dot_assign(expr, |target| Expr::MethodCall {
                    target: Box::new(target),
                    name: mname_sym,
                    args: args.clone(),
                    modifier: None,
                    quoted: true,
                })
            }
            QuotedMethodName::Dynamic(name_expr) => {
                wrap_dot_assign(expr, |target| Expr::DynamicMethodCall {
                    target: Box::new(target),
                    name_expr: Box::new(name_expr.clone()),
                    args: args.clone(),
                    modifier: None,
                })
            }
        };
        return Ok((r_final, result));
    }
    // `.= &sub` / `.= $meth` — mutating method-call-via-sub or dynamic method
    // name (`$x .= &f` means `$x = $x.&f`). The name is a sigiled expression.
    if r.starts_with('&') || r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
        let (r_name, name_expr) = primary(r)?;
        let (r_after, _) = ws(r_name)?;
        let (r_final, args) = if r_after.starts_with('(') {
            let (r2, _) = parse_char(r_after, '(')?;
            let (r2, _) = ws(r2)?;
            let (r2, a) = parse_call_arg_list(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, _) = parse_char(r2, ')')?;
            (r2, a)
        } else {
            (r_name, vec![])
        };
        let result = wrap_dot_assign(expr, |target| Expr::DynamicMethodCall {
            target: Box::new(target),
            name_expr: Box::new(name_expr.clone()),
            args: args.clone(),
            modifier: None,
        });
        return Ok((r_final, result));
    }
    // Parse regular method name
    let (r, method_name) = crate::parser::primary::var::parse_ident_with_hyphens(r)?;
    let r_before_ws = r;
    let (r, _) = ws(r)?;
    let (r_final, args) = if r.starts_with('(') {
        let (r2, _) = parse_char(r, '(')?;
        let (r2, _) = ws(r2)?;
        let (r2, a) = parse_call_arg_list(r2)?;
        let (r2, _) = ws(r2)?;
        let (r2, _) = parse_char(r2, ')')?;
        (r2, a)
    } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
        // Colon-arg syntax: .=method: arg (no space before colon)
        let r2 = &r[1..];
        let (r2, _) = ws(r2)?;
        let (r2, first_arg) = expression(r2)?;
        let mut args = vec![first_arg];
        let mut r_inner = r2;
        loop {
            let (r3, _) = ws(r_inner)?;
            if r3.starts_with(':')
                && !r3.starts_with("::")
                && let Ok((r4, arg)) = colonpair_expr(r3)
            {
                args.push(arg);
                r_inner = r4;
                continue;
            }
            if !r3.starts_with(',') {
                r_inner = r3;
                break;
            }
            let r3 = &r3[1..];
            let (r3, _) = ws(r3)?;
            if r3.starts_with(';') || r3.starts_with('}') || r3.is_empty() {
                r_inner = r3;
                break;
            }
            let (r3, next) = expression(r3)?;
            args.push(next);
            r_inner = r3;
        }
        (r_inner, args)
    } else if r.starts_with(':') && !r.starts_with("::") {
        // Fake-infix adverb form (space before ':'): .=method :key<val>
        let mut args = Vec::new();
        let mut r_inner = r;
        while r_inner.starts_with(':') && !r_inner.starts_with("::") {
            if let Ok((r2, arg)) = colonpair_expr(r_inner) {
                args.push(arg);
                let (r3, _) = ws(r2)?;
                r_inner = r3;
            } else {
                break;
            }
        }
        (r_inner, args)
    } else {
        (r, vec![])
    };
    let method_sym = Symbol::intern(method_name);
    let result = wrap_dot_assign(expr, |target| Expr::MethodCall {
        target: Box::new(target),
        name: method_sym,
        args: args.clone(),
        modifier: None,
        quoted: false,
    });
    Ok((r_final, result))
}
