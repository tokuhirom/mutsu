//! WhateverCode-adjacent AST shaping that stays parser-side (ADR-0033):
//! composing `o`/`\u{2218}` operands, and threading a curried `CallOn` target
//! through a trailing method-call chain. These decide *shape*, not closure
//! construction — the un-curried operands are wrapped in `Expr::WhateverCurry`
//! markers; `crate::whatever_curry::build_closure` expands those at compile
//! time.

use super::*;
use crate::whatever_curry::make_wc_param;

pub(crate) fn wrap_composition_operands(expr: Expr) -> Expr {
    match expr {
        Expr::Binary { left, op, right } => {
            let left = wrap_composition_operands(*left);
            let right = wrap_composition_operands(*right);
            if matches!(&op, TokenKind::Ident(name) if name == "o") {
                let mut bare_count = 0usize;
                if is_whatever(&left) {
                    bare_count += 1;
                }
                if is_whatever(&right) {
                    bare_count += 1;
                }
                if bare_count > 0 {
                    let mut params = Vec::new();
                    let mut param_defs = Vec::new();
                    let left_expr = if is_whatever(&left) {
                        let name = format!("__wc_{}", params.len());
                        params.push(name.clone());
                        param_defs.push(make_wc_param(name.clone()));
                        Expr::Var(name)
                    } else if should_wrap_whatevercode(&left) {
                        Expr::WhateverCurry(Box::new(left))
                    } else {
                        left
                    };
                    let right_expr = if is_whatever(&right) {
                        let name = format!("__wc_{}", params.len());
                        params.push(name.clone());
                        param_defs.push(make_wc_param(name.clone()));
                        Expr::Var(name)
                    } else if should_wrap_whatevercode(&right) {
                        Expr::WhateverCurry(Box::new(right))
                    } else {
                        right
                    };
                    let body_expr = Expr::Binary {
                        left: Box::new(left_expr),
                        op,
                        right: Box::new(right_expr),
                    };
                    if params.len() == 1 {
                        return Expr::Lambda {
                            param: params[0].clone(),
                            body: vec![Stmt::Expr(body_expr)],
                            is_whatever_code: false,
                            param_sigilless: false,
                        };
                    }
                    return Expr::AnonSubParams {
                        params,
                        param_defs,
                        return_type: None,
                        body: vec![Stmt::Expr(body_expr)],
                        is_rw: false,
                        is_whatever_code: false,
                    };
                }
                let left_wrapped = if should_wrap_whatevercode(&left) {
                    Expr::WhateverCurry(Box::new(left))
                } else {
                    left
                };
                let right_wrapped = if should_wrap_whatevercode(&right) {
                    Expr::WhateverCurry(Box::new(right))
                } else {
                    right
                };
                Expr::Binary {
                    left: Box::new(left_wrapped),
                    op,
                    right: Box::new(right_wrapped),
                }
            } else {
                Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }
            }
        }
        Expr::Unary { op, expr } => Expr::Unary {
            op,
            expr: Box::new(wrap_composition_operands(*expr)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(wrap_composition_operands(*target)),
            name,
            args: args.into_iter().map(wrap_composition_operands).collect(),
            modifier,
            quoted,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(wrap_composition_operands(*target)),
            args: args.into_iter().map(wrap_composition_operands).collect(),
        },
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => Expr::Index {
            target: Box::new(wrap_composition_operands(*target)),
            index: Box::new(wrap_composition_operands(*index)),
            is_positional,
        },
        other => other,
    }
}

/// Try to detect and fix a chain of MethodCalls leading to a CallOn whose target
/// contains Whatever. If found, wrap only the CallOn target as a WhateverCurry
/// marker, leaving the outer method calls outside the curried scope.
///
/// Handles patterns like: *.foo().(args).bar().baz()
/// where only *.foo() should be wrapped as WhateverCode.
pub(crate) fn try_wrap_whatevercode_call_chain(expr: &Expr) -> Option<Expr> {
    // Check if this is a MethodCall chain ending at a CallOn with Whatever target
    match expr {
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } if !args.iter().any(contains_whatever) => {
            match target.as_ref() {
                // Direct: MethodCall -> CallOn -> Whatever-containing target
                Expr::CallOn {
                    target: inner_target,
                    args: call_args,
                } if should_wrap_whatevercode(inner_target)
                    && !call_args.iter().any(contains_whatever) =>
                {
                    Some(Expr::MethodCall {
                        target: Box::new(Expr::CallOn {
                            target: Box::new(Expr::WhateverCurry(Box::new(
                                (**inner_target).clone(),
                            ))),
                            args: call_args.clone(),
                        }),
                        name: *name,
                        args: args.clone(),
                        modifier: *modifier,
                        quoted: *quoted,
                    })
                }
                // Recursive: MethodCall -> MethodCall -> ... -> CallOn
                inner @ Expr::MethodCall { .. } => {
                    let wrapped_inner = try_wrap_whatevercode_call_chain(inner)?;
                    Some(Expr::MethodCall {
                        target: Box::new(wrapped_inner),
                        name: *name,
                        args: args.clone(),
                        modifier: *modifier,
                        quoted: *quoted,
                    })
                }
                _ => None,
            }
        }
        _ => None,
    }
}
