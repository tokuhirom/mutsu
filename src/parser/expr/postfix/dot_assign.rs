use super::call_method::{QuotedMethodName, parse_quoted_method_name};
use crate::ast::{Expr, Stmt};
use crate::parser::expr::listop_arg_expr;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};
use crate::parser::primary::{colonpair_expr, parse_call_arg_list, primary};
use crate::symbol::Symbol;
use crate::value::Value;

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
    // Unwrap Grouped and identity `.self` calls to get at the lvalue. `.self`
    // returns its invocant unchanged, so `(X.self).=meth` is exactly `X.=meth`
    // (the assignment writes back through `X`).
    let mut target = target;
    let target = loop {
        let is_self_call = matches!(
            &target,
            Expr::MethodCall { name, args, modifier: None, .. }
                if name.resolve() == "self" && args.is_empty()
        );
        target = match target {
            Expr::Grouped(inner) => *inner,
            Expr::MethodCall { target: inner, .. } if is_self_call => *inner,
            other => break other,
        };
    };
    match &target {
        // NOTE: an *expression*-position `$_ .= meth` keeps the plain `AssignExpr`
        // form (so a chain-retarget can redirect the implied `$_`). The whole-
        // container topic write-through (`given @a { $_ .= uc }`) is a *statement*
        // and is handled via the `__mutsu_topic_dotassign` marker in the statement
        // assign parser instead.
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
        // A chained `.=` on an index lvalue (`@a[i] .= m1 .= m2`). The first `.=`
        // already lowered `@a[i] .= m1` to `do { my idx = i; @a[idx] = @a[idx].m1 }`
        // (an `IndexAssign` tail), which `lvalue_assign_name` does not recognize.
        // Re-read `@a[idx]` (now holding m1's result, via the already-declared `idx`
        // temp), apply this method, and write it back to the same element so the
        // chain keeps mutating `@a[i]` rather than a detached copy.
        Expr::DoBlock { body, .. }
            if matches!(body.last(), Some(Stmt::Expr(Expr::IndexAssign { .. }))) =>
        {
            let (idx_target, index, is_positional) = match body.last() {
                Some(Stmt::Expr(Expr::IndexAssign {
                    target,
                    index,
                    is_positional,
                    ..
                })) => (target.clone(), index.clone(), *is_positional),
                _ => unreachable!(),
            };
            let read_expr = Expr::Index {
                target: idx_target.clone(),
                index: index.clone(),
                is_positional,
            };
            let new_value = method_call_fn(read_expr);
            let mut new_body = body.clone();
            new_body.push(Stmt::Expr(Expr::IndexAssign {
                target: idx_target,
                index,
                value: Box::new(new_value),
                is_positional,
            }));
            Expr::DoBlock {
                body: new_body,
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
        // A non-lvalue target (`(Foo.new).=meth`, `Foo.new.=meth`, ...). In Raku
        // `X.=meth` is `X = X.meth`; when X (evaluated once) provides a `STORE`
        // method it is a custom container, so this becomes `X.STORE(X.meth)` and
        // the expression value is the container X itself (not STORE's return). An
        // object without STORE keeps mutsu's historical plain-method-call value
        // (Raku errors with "Cannot modify an immutable ..." there, but that path
        // is untested and the lax form is relied on elsewhere). Bind the target to
        // a temp so it is evaluated exactly once, then branch at runtime on
        // `.^can("STORE")`:
        //   do { my $t = TARGET;
        //        $t.^can("STORE") ?? do { $t.STORE($t.meth); $t } !! $t.meth }
        _ => {
            use std::sync::atomic::Ordering;
            let tmp = format!(
                "__mutsu_dotassign_{}",
                crate::parser::stmt::simple::TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let tmp_var = Expr::Var(tmp.clone());
            // `method_call_fn` is `FnOnce`; call it once against the temp and clone
            // the resulting `$t.meth(args)` for both the STORE argument and the
            // no-STORE fallback (both branches share the same target `$t`).
            let meth_result = method_call_fn(tmp_var.clone());
            let can_store = Expr::MethodCall {
                target: Box::new(tmp_var.clone()),
                name: Symbol::intern("can"),
                args: vec![Expr::Literal(Value::str("STORE".to_string()))],
                modifier: Some('^'),
                quoted: false,
            };
            let store_call = Expr::MethodCall {
                target: Box::new(tmp_var.clone()),
                name: Symbol::intern("STORE"),
                args: vec![meth_result.clone()],
                modifier: None,
                quoted: false,
            };
            let then_expr = Expr::DoBlock {
                body: vec![Stmt::Expr(store_call), Stmt::Expr(tmp_var.clone())],
                label: None,
            };
            let ternary = Expr::Ternary {
                cond: Box::new(can_store),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(meth_result),
            };
            Expr::DoBlock {
                body: vec![
                    Stmt::VarDecl {
                        name: tmp,
                        expr: target,
                        type_constraint: None,
                        is_state: false,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                        custom_traits: Vec::new(),
                        where_constraint: None,
                    },
                    Stmt::Expr(ternary),
                ],
                label: None,
            }
        }
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
        // Colon-arg syntax: .=method: arg (no space before colon). The arg list
        // is a comma-list at listop precedence, so each element stops before the
        // loose word-logical ops (`andthen`/`and`/`or`): `$x .=new: 1 andthen $y`
        // is `($x .=new: 1) andthen $y`, not `$x .=new(1 andthen $y)`.
        let r2 = &r[1..];
        let (r2, _) = ws(r2)?;
        let (r2, first_arg) = listop_arg_expr(r2)?;
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
            let (r3, next) = listop_arg_expr(r3)?;
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
