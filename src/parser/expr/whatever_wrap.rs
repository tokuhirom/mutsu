//! WhateverCode construction: wrapping expressions containing `*` placeholders
//! into the appropriate `Lambda` / `AnonSubParams` closures, and composing
//! `o`-operator operands.

use super::*;

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
                        wrap_whatevercode(&left)
                    } else {
                        left
                    };
                    let right_expr = if is_whatever(&right) {
                        let name = format!("__wc_{}", params.len());
                        params.push(name.clone());
                        param_defs.push(make_wc_param(name.clone()));
                        Expr::Var(name)
                    } else if should_wrap_whatevercode(&right) {
                        wrap_whatevercode(&right)
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
                    wrap_whatevercode(&left)
                } else {
                    left
                };
                let right_wrapped = if should_wrap_whatevercode(&right) {
                    wrap_whatevercode(&right)
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

fn make_wc_param(name: String) -> crate::ast::ParamDef {
    crate::ast::ParamDef {
        name,
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
    }
}

/// Try to detect and fix a chain of MethodCalls leading to a CallOn whose target
/// contains Whatever. If found, wrap only the CallOn target as WhateverCode,
/// leaving the outer method calls outside the lambda.
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
                            target: Box::new(wrap_whatevercode(inner_target)),
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

/// Build a WhateverCode lambda from an expression containing Whatever placeholders.
pub(crate) fn wrap_whatevercode(expr: &Expr) -> Expr {
    if let Expr::CallOn { target, args } = expr
        && should_wrap_whatevercode(target)
        && !args.iter().any(contains_whatever)
    {
        return Expr::CallOn {
            target: Box::new(wrap_whatevercode(target)),
            args: args.clone(),
        };
    }

    let wc_count = count_whatever(expr);

    if wc_count <= 1 && !expr_contains_topic(expr) {
        // Single-arg: use Lambda with param "_" for backward compat
        // Use a special single-arg replacer that maps * and nested single-arg WC to $_
        let body_expr = replace_whatever_single(expr);
        Expr::Lambda {
            param: "_".to_string(),
            body: vec![Stmt::Expr(body_expr)],
            is_whatever_code: true,
        }
    } else if wc_count <= 1 {
        // Single-arg, but expression already contains $_ — use a numbered param
        // to avoid shadowing the outer $_.
        let mut counter = 0;
        let body_expr = replace_whatever_numbered(expr, &mut counter);
        let param_name = "__wc_0".to_string();
        Expr::AnonSubParams {
            params: vec![param_name.clone()],
            param_defs: vec![crate::ast::ParamDef {
                name: param_name,
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy: false,
                sigilless: false,
                type_constraint: None,
                literal_value: None,
                sub_signature: None,
                where_constraint: None,
                traits: Vec::new(),
                double_slurpy: false,
                onearg: false,
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            }],
            return_type: None,
            body: vec![Stmt::Expr(body_expr)],
            is_rw: false,
            is_whatever_code: true,
        }
    } else {
        // Multi-arg: use AnonSubParams with numbered params
        let mut counter = 0;
        let body_expr = replace_whatever_numbered(expr, &mut counter);
        let params: Vec<String> = (0..counter).map(|i| format!("__wc_{}", i)).collect();
        Expr::AnonSubParams {
            params: params.clone(),
            param_defs: params
                .iter()
                .map(|name| crate::ast::ParamDef {
                    name: name.clone(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    double_slurpy: false,
                    onearg: false,
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                })
                .collect(),
            return_type: None,
            body: vec![Stmt::Expr(body_expr)],
            is_rw: false,
            is_whatever_code: true,
        }
    }
}
