use std::sync::atomic::Ordering;

use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, take_while1,
};

use crate::ast::{AssignOp, Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use super::simple::{TMP_INDEX_COUNTER, add_xor_sink_warnings, parse_hyper_assign_op};
use super::{is_stmt_modifier_keyword, keyword, parse_comma_or_expr, parse_statement_modifier};

fn starts_with_term_token(input: &str) -> bool {
    let Some(ch) = input.chars().next() else {
        return false;
    };
    ch.is_ascii_digit()
        || ch.is_alphabetic()
        || matches!(
            ch,
            '$' | '@'
                | '%'
                | '&'
                | '\''
                | '"'
                | '‘'
                | '’'
                | '‚'
                | '“'
                | '”'
                | '„'
                | '('
                | '['
                | '{'
                | ':'
        )
}

/// Returns true if the input starts with a token that is unambiguously a new
/// term (not an infix operator or statement modifier).  More conservative than
/// `starts_with_term_token`: only digits and quote characters, which can never
/// be the start of an operator.
fn starts_with_unambiguous_term(input: &str) -> bool {
    let Some(ch) = input.chars().next() else {
        return false;
    };
    ch.is_ascii_digit()
        || matches!(
            ch,
            '\'' | '"'
                | '\u{2018}'
                | '\u{2019}'
                | '\u{201A}'
                | '\u{201C}'
                | '\u{201D}'
                | '\u{201E}'
        )
}

/// Returns true if the expression is a pure value that cannot take arguments
/// (i.e., a literal or variable, not a function call or bareword that might
/// be a function name).  Used to detect "two terms in a row" parse errors.
fn is_pure_value_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Literal(_)
            | Expr::Var(_)
            | Expr::ArrayVar(_)
            | Expr::HashVar(_)
            | Expr::StringInterpolation(_)
            | Expr::ArrayLiteral(_)
    )
}

fn method_lvalue_assign_expr(
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

fn named_sub_lvalue_assign_expr(name: String, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
        args: vec![
            Expr::Literal(Value::str(name)),
            Expr::ArrayLiteral(call_args),
            value,
        ],
    }
}

fn callable_lvalue_assign_expr(target: Expr, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
        args: vec![target, Expr::ArrayLiteral(call_args), value],
    }
}

fn bind_source_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(name) => Some(name.clone()),
        Expr::ArrayVar(name) => Some(format!("@{}", name)),
        Expr::HashVar(name) => Some(format!("%{}", name)),
        _ => None,
    }
}

fn bind_source_metadata_expr(rhs: &Expr) -> Expr {
    match rhs {
        Expr::ArrayLiteral(items) => Expr::ArrayLiteral(
            items
                .iter()
                .map(|item| {
                    if let Some(name) = bind_source_name(item) {
                        Expr::Literal(Value::str(name))
                    } else {
                        Expr::Literal(Value::Nil)
                    }
                })
                .collect(),
        ),
        other => {
            if let Some(name) = bind_source_name(other) {
                Expr::ArrayLiteral(vec![Expr::Literal(Value::str(name))])
            } else {
                Expr::ArrayLiteral(vec![Expr::Literal(Value::Nil)])
            }
        }
    }
}

fn single_target_list_lvalue_stmt(lhs: Expr, rhs: Expr) -> Option<Stmt> {
    let Expr::ArrayLiteral(items) = lhs else {
        return None;
    };
    let mut saw_whatever = false;
    let mut lvalues: Vec<Expr> = Vec::new();
    for item in items {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push(item);
    }
    if !saw_whatever || lvalues.len() != 1 {
        return None;
    }
    let target = lvalues.into_iter().next()?;
    Some(match target {
        Expr::Var(name) => Stmt::Assign {
            name,
            expr: rhs,
            op: AssignOp::Assign,
        },
        Expr::ArrayVar(name) => Stmt::Assign {
            name: format!("@{}", name),
            expr: Expr::Call {
                name: Symbol::intern("__mutsu_star_lvalue_rhs"),
                args: vec![Expr::Literal(Value::str(format!("@{}", name))), rhs],
            },
            op: AssignOp::Assign,
        },
        Expr::HashVar(name) => Stmt::Assign {
            name: format!("%{}", name),
            expr: rhs,
            op: AssignOp::Assign,
        },
        Expr::Index { target, index } => Stmt::Expr(Expr::IndexAssign {
            target,
            index,
            value: Box::new(rhs),
        }),
        _ => return None,
    })
}

/// Parse an expression statement (fallback).
pub(super) fn expr_stmt(input: &str) -> PResult<'_, Stmt> {
    // Topic mutating method call: .=method(args)
    if let Some(stripped) = input.strip_prefix(".=") {
        let (rest, _) = ws(stripped)?;
        let (rest, method_name) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else {
            (rest, Vec::new())
        };
        let stmt = Stmt::Expr(Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: Symbol::intern(&method_name),
            args,
            modifier: None,
            quoted: false,
        });
        return parse_statement_modifier(rest, stmt);
    }

    if let Ok((rest, expr)) = crate::parser::primary::string::qx_string(input) {
        return parse_statement_modifier(rest, Stmt::Expr(expr));
    }

    let (rest, expr) = match expression(input) {
        Ok(parsed) => parsed,
        Err(err) => {
            if err.messages.iter().any(|m| m.contains("X::Obsolete")) {
                return Err(err);
            }
            if let Ok(parsed_assign) = super::assign::parse_assign_expr_or_comma(input) {
                parsed_assign
            } else if err.is_fatal()
                && (err.exception.is_some()
                    || err
                        .messages
                        .first()
                        .is_some_and(|m| m.contains("X::Comp::Trait::Unknown")))
            {
                return Err(err);
            } else {
                return Err(PError {
                    messages: merge_expected_messages(
                        "expected expression statement",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(input.len())),
                    exception: err.exception,
                });
            }
        }
    };

    if let Expr::BareWord(name) = &expr
        && matches!(name.as_str(), "qx" | "qqx")
        && rest.starts_with('=')
        && let Ok((r_qx, qx_expr)) = crate::parser::primary::string::qx_string(input)
    {
        return parse_statement_modifier(r_qx, Stmt::Expr(qx_expr));
    }

    // Check for index assignment after expression
    let rest_before_ws = rest;
    let (rest, _) = ws(rest)?;
    let separator = &rest_before_ws[..rest_before_ws.len() - rest.len()];
    let separated_by_newline = separator.contains('\n') || separator.contains('\r');
    if !separated_by_newline
        && matches!(&expr, Expr::BareWord(name) if name == "int")
        && !rest.is_empty()
        && !rest.starts_with(';')
        && !rest.starts_with('}')
        && !rest.starts_with(')')
        && !rest.starts_with(']')
        && !rest.starts_with(',')
        && !is_stmt_modifier_keyword(rest)
        && starts_with_term_token(rest)
    {
        return Err(PError::expected("statement end"));
    }
    // Detect "two terms in a row" errors: a literal value followed by another
    // unambiguous term token on the same line without an infix operator.
    // Only flag cases that cannot be valid syntax (e.g., `1 1`, `"a" "b"`).
    if !separated_by_newline
        && is_pure_value_expr(&expr)
        && !rest.is_empty()
        && !rest.starts_with(';')
        && !rest.starts_with('}')
        && !rest.starts_with(')')
        && !rest.starts_with(']')
        && !rest.starts_with(',')
        && !is_stmt_modifier_keyword(rest)
        && starts_with_unambiguous_term(rest)
    {
        return Err(PError::fatal("Confused. Two terms in a row".to_string()));
    }
    if let Some(stripped) = rest
        .strip_prefix("\u{00BB}.=")
        .or_else(|| rest.strip_prefix(">>.="))
    {
        let (stripped, _) = ws(stripped)?;
        let (r, method_name) = take_while1(stripped, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })
        .map_err(|err| PError {
            messages: merge_expected_messages(
                "expected method name after hyper '.='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(stripped.len())),
            exception: None,
        })?;
        let method_name = method_name.to_string();
        let (r, method_args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else {
            (r, Vec::new())
        };
        let make_rhs = |target: Expr| Expr::HyperMethodCall {
            target: Box::new(target),
            name: Symbol::intern(&method_name),
            args: method_args.clone(),
            modifier: None,
            quoted: false,
        };
        match expr {
            Expr::Var(name) => {
                let stmt = Stmt::Assign {
                    name: name.clone(),
                    expr: make_rhs(Expr::Var(name)),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::ArrayVar(name) => {
                let stmt = Stmt::Assign {
                    name: format!("@{}", name),
                    expr: make_rhs(Expr::ArrayVar(name)),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::HashVar(name) => {
                let stmt = Stmt::Assign {
                    name: format!("%{}", name),
                    expr: make_rhs(Expr::HashVar(name)),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::Index { target, index } => {
                let tmp_idx = format!(
                    "__mutsu_idx_{}",
                    TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
                );
                let tmp_idx_expr = Expr::Var(tmp_idx.clone());
                let lhs_expr = Expr::Index {
                    target: target.clone(),
                    index: Box::new(tmp_idx_expr.clone()),
                };
                let assigned_value = make_rhs(lhs_expr);
                let stmt = Stmt::Expr(Expr::DoBlock {
                    body: vec![
                        Stmt::VarDecl {
                            name: tmp_idx.clone(),
                            expr: *index,
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
                            target,
                            index: Box::new(tmp_idx_expr),
                            value: Box::new(assigned_value),
                        }),
                    ],
                    label: None,
                });
                return parse_statement_modifier(r, stmt);
            }
            _ => {
                return Err(PError::expected("assignable expression before hyper '.='"));
            }
        }
    }
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (stripped, _) = ws(stripped)?;
        let (r, method_name) = take_while1(stripped, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })
        .map_err(|err| PError {
            messages: merge_expected_messages("expected method name after '.='", &err.messages),
            remaining_len: err.remaining_len.or(Some(stripped.len())),
            exception: None,
        })?;
        let method_name = method_name.to_string();
        let (r, method_args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else {
            (r, Vec::new())
        };
        let make_rhs = |target: Expr| Expr::MethodCall {
            target: Box::new(target),
            name: Symbol::intern(&method_name),
            args: method_args.clone(),
            modifier: None,
            quoted: false,
        };
        match expr {
            Expr::Var(name) => {
                let stmt = Stmt::Assign {
                    name: name.clone(),
                    expr: make_rhs(Expr::Var(name.clone())),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::ArrayVar(name) => {
                let stmt = Stmt::Assign {
                    name: format!("@{}", name),
                    expr: make_rhs(Expr::ArrayVar(name)),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::HashVar(name) => {
                let stmt = Stmt::Assign {
                    name: format!("%{}", name),
                    expr: make_rhs(Expr::HashVar(name)),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            Expr::Index { target, index } => {
                let tmp_idx = format!(
                    "__mutsu_idx_{}",
                    TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
                );
                let tmp_idx_expr = Expr::Var(tmp_idx.clone());
                let lhs_expr = Expr::Index {
                    target: target.clone(),
                    index: Box::new(tmp_idx_expr.clone()),
                };
                let assigned_value = make_rhs(lhs_expr);
                let stmt = Stmt::Expr(Expr::DoBlock {
                    body: vec![
                        Stmt::VarDecl {
                            name: tmp_idx.clone(),
                            expr: *index,
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
                            target,
                            index: Box::new(tmp_idx_expr),
                            value: Box::new(assigned_value),
                        }),
                    ],
                    label: None,
                });
                return parse_statement_modifier(r, stmt);
            }
            Expr::BareWord(ref name) => {
                // BareWord .= method: e.g. `Foo .= new` tries to assign to a type object,
                // which is immutable. Emit an assignment so the runtime can throw the
                // appropriate "Cannot modify an immutable type object" error.
                let stmt = Stmt::Assign {
                    name: name.clone(),
                    expr: make_rhs(expr.clone()),
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(r, stmt);
            }
            _ => {
                // Non-lvalue targets still support `.=` dispatch in statement position,
                // but must not sink the returned value.
                let stmt = Stmt::Expr(Expr::Call {
                    name: Symbol::intern("sink"),
                    args: vec![make_rhs(expr)],
                });
                return parse_statement_modifier(r, stmt);
            }
        }
    }
    if matches!(expr, Expr::Index { .. } | Expr::MultiDimIndex { .. })
        && rest.starts_with('=')
        && !rest.starts_with("==")
    {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after index assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        if let Expr::MultiDimIndex { target, dimensions } = expr {
            let stmt = Stmt::Expr(Expr::IndexAssign {
                target,
                index: Box::new(Expr::ArrayLiteral(dimensions)),
                value: Box::new(value),
            });
            return parse_statement_modifier(rest, stmt);
        }
        if let Expr::Index { target, index } = expr {
            if let Expr::Call { name, args } = target.as_ref()
                && name == "__mutsu_subscript_adverb"
                && args.len() >= 3
                && matches!(index.as_ref(), Expr::Literal(Value::Int(1)))
                && matches!(&args[2], Expr::Literal(Value::Str(mode)) if mode.as_str() == "kv" || mode.as_str() == "not-kv")
            {
                let stmt = Stmt::Expr(Expr::IndexAssign {
                    target: Box::new(args[0].clone()),
                    index: Box::new(args[1].clone()),
                    value: Box::new(value),
                });
                return parse_statement_modifier(rest, stmt);
            }
            let stmt = Stmt::Expr(Expr::IndexAssign {
                target,
                index,
                value: Box::new(value),
            });
            return parse_statement_modifier(rest, stmt);
        }
    }
    if let Expr::Index { target, index } = expr.clone()
        && let Some(rest) = parse_hyper_assign_op(rest)
    {
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after hyper assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let stmt = Stmt::Expr(Expr::IndexAssign {
            target,
            index,
            value: Box::new(value),
        });
        return parse_statement_modifier(rest, stmt);
    }
    if matches!(expr, Expr::Index { .. } | Expr::MultiDimIndex { .. }) && rest.starts_with(":=") {
        let rest = &rest[2..];
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after index bind",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let source_meta = bind_source_metadata_expr(&value);
        let bind_value = Expr::Call {
            name: Symbol::intern("__mutsu_bind_index_value"),
            args: vec![value, source_meta],
        };
        match expr {
            Expr::Index { target, index } => {
                let stmt = Stmt::Expr(Expr::IndexAssign {
                    target,
                    index,
                    value: Box::new(bind_value),
                });
                return parse_statement_modifier(rest, stmt);
            }
            Expr::MultiDimIndex { target, dimensions } => {
                let stmt = Stmt::Expr(Expr::IndexAssign {
                    target,
                    index: Box::new(Expr::ArrayLiteral(dimensions)),
                    value: Box::new(bind_value),
                });
                return parse_statement_modifier(rest, stmt);
            }
            _ => {}
        }
    }

    // Reverse bind assignment is intentionally unsupported in Raku.
    if !matches!(expr, Expr::AssignExpr { .. })
        && (rest.starts_with("R:=") || rest.starts_with("R::="))
    {
        return Err(PError::fatal(
            "Cannot reverse the args of := because list assignment operators are too fiddly"
                .to_string(),
        ));
    }

    // Reverse assignment: `value R= target` means `target = value`.
    if !matches!(expr, Expr::AssignExpr { .. })
        && rest.starts_with("R=")
        && !rest.starts_with("R==")
    {
        let r = &rest[2..];
        let (r, _) = ws(r)?;
        let (r, target_expr) =
            super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected assignable expression after 'R='",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?;
        if let Expr::DoStmt(stmt) = &target_expr
            && let Stmt::VarDecl {
                name,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                ..
            } = stmt.as_ref()
        {
            let stmt = Stmt::VarDecl {
                name: name.clone(),
                expr,
                type_constraint: type_constraint.clone(),
                is_state: *is_state,
                is_our: *is_our,
                is_dynamic: *is_dynamic,
                is_export: *is_export,
                export_tags: export_tags.clone(),
                custom_traits: custom_traits.clone(),
                where_constraint: None,
            };
            return parse_statement_modifier(r, stmt);
        }
        if let Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } = &target_expr
        {
            let target_var_name = match target.as_ref() {
                Expr::Var(var_name) => Some(var_name.clone()),
                Expr::ArrayVar(var_name) => Some(format!("@{}", var_name)),
                Expr::HashVar(var_name) => Some(format!("%{}", var_name)),
                _ => None,
            };
            let assigned = method_lvalue_assign_expr(
                (**target).clone(),
                target_var_name,
                name.resolve(),
                args.clone(),
                expr,
            );
            let stmt = Stmt::Expr(assigned);
            return parse_statement_modifier(r, stmt);
        }
        if let Expr::Call { name, args } = &target_expr {
            let stmt = Stmt::Expr(named_sub_lvalue_assign_expr(
                name.resolve(),
                args.clone(),
                expr,
            ));
            return parse_statement_modifier(r, stmt);
        }
        if let Expr::CallOn { target, args } = &target_expr {
            let stmt = Stmt::Expr(callable_lvalue_assign_expr(
                (**target).clone(),
                args.clone(),
                expr,
            ));
            return parse_statement_modifier(r, stmt);
        }
        let stmt = match target_expr {
            Expr::Var(name) => Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            },
            Expr::ArrayVar(name) => Stmt::Assign {
                name: format!("@{}", name),
                expr,
                op: AssignOp::Assign,
            },
            Expr::HashVar(name) => Stmt::Assign {
                name: format!("%{}", name),
                expr,
                op: AssignOp::Assign,
            },
            target => {
                if let Some(stmt) = single_target_list_lvalue_stmt(target.clone(), expr.clone()) {
                    stmt
                } else {
                    Stmt::Expr(callable_lvalue_assign_expr(target, Vec::new(), expr))
                }
            }
        };
        return parse_statement_modifier(r, stmt);
    }

    // Generic assignment on non-variable lhs (e.g. `.key = 1`).
    // TODO: Introduce a dedicated assignment AST form for arbitrary lvalues.
    // Current fallback consumes `lhs = rhs` and preserves both sides as expressions.
    if !matches!(expr, Expr::AssignExpr { .. })
        && rest.starts_with('=')
        && !rest.starts_with("==")
        && !rest.starts_with("=>")
    {
        let r = &rest[1..];
        let (r, _) = ws(r)?;
        let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after '='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        if let Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } = &expr
        {
            let assigned = if name == "AT-POS" && args.len() == 1 {
                Expr::IndexAssign {
                    target: target.clone(),
                    index: Box::new(args[0].clone()),
                    value: Box::new(rhs),
                }
            } else {
                let target_var_name = match target.as_ref() {
                    Expr::Var(var_name) => Some(var_name.clone()),
                    Expr::ArrayVar(var_name) => Some(format!("@{}", var_name)),
                    Expr::HashVar(var_name) => Some(format!("%{}", var_name)),
                    _ => None,
                };
                method_lvalue_assign_expr(
                    (**target).clone(),
                    target_var_name,
                    name.resolve(),
                    args.clone(),
                    rhs,
                )
            };
            let stmt = Stmt::Expr(assigned);
            return parse_statement_modifier(r, stmt);
        }
        if let Expr::Call { name, args } = &expr {
            let stmt = Stmt::Expr(named_sub_lvalue_assign_expr(
                name.resolve(),
                args.clone(),
                rhs,
            ));
            return parse_statement_modifier(r, stmt);
        }
        if let Expr::CallOn { target, args } = &expr {
            let stmt = Stmt::Expr(callable_lvalue_assign_expr(
                (**target).clone(),
                args.clone(),
                rhs,
            ));
            return parse_statement_modifier(r, stmt);
        }
        let stmt = match expr {
            Expr::Literal(_) | Expr::BareWord(_) => {
                Stmt::Block(vec![Stmt::Expr(expr), Stmt::Expr(rhs)])
            }
            target => {
                if let Some(stmt) = single_target_list_lvalue_stmt(target.clone(), rhs.clone()) {
                    stmt
                } else {
                    Stmt::Expr(callable_lvalue_assign_expr(target, Vec::new(), rhs))
                }
            }
        };
        return parse_statement_modifier(r, stmt);
    }

    // Generic bind assignment on non-variable lhs (e.g. `($a, $b) := |(f)`).
    // Keep this as a parse fallback so complex bind lvalues don't fail early.
    if !matches!(expr, Expr::AssignExpr { .. }) && rest.starts_with(":=") {
        let r = &rest[2..];
        let (r, _) = ws(r)?;
        let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after ':='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        let stmt = Stmt::Block(vec![Stmt::Expr(expr), Stmt::Expr(rhs)]);
        return parse_statement_modifier(r, stmt);
    }

    // Check for assignment after parenthesized assign expression: ($x = $y) = 5
    if let Expr::AssignExpr { ref name, .. } = expr {
        if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
            let var_name = name.clone();
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after '='",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?;
            let stmts = vec![
                Stmt::Expr(expr),
                Stmt::Assign {
                    name: var_name,
                    expr: rhs,
                    op: AssignOp::Assign,
                },
            ];
            return parse_statement_modifier(r, Stmt::Block(stmts));
        }
        if let Some((stripped, op)) = super::assign::parse_compound_assign_op(rest) {
            let var_name = name.clone();
            let (r, _) = ws(stripped)?;
            let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after compound assignment",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?;
            let var_expr = Expr::Var(var_name.clone());
            let stmts = vec![
                Stmt::Expr(expr),
                Stmt::Assign {
                    name: var_name,
                    expr: Expr::Binary {
                        left: Box::new(var_expr),
                        op: op.token_kind(),
                        right: Box::new(rhs),
                    },
                    op: AssignOp::Assign,
                },
            ];
            return parse_statement_modifier(r, Stmt::Block(stmts));
        }
    }

    // Generic compound assignment on non-variable lhs (e.g. `.key //= ++$i`).
    // TODO: Support proper compound assignment semantics for arbitrary lvalues.
    // Current fallback desugars to a binary op expression so parsing can proceed.
    if !matches!(expr, Expr::AssignExpr { .. })
        && let Some((stripped, op)) = super::assign::parse_compound_assign_op(rest)
    {
        let (r, _) = ws(stripped)?;
        let (r, rhs) = super::assign::parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        if let Expr::Index { target, index } = &expr {
            let tmp_idx = format!(
                "__mutsu_idx_{}",
                TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let tmp_idx_expr = Expr::Var(tmp_idx.clone());
            let lhs_expr = Expr::Index {
                target: target.clone(),
                index: Box::new(tmp_idx_expr.clone()),
            };
            let assigned_value = if matches!(op, super::assign::CompoundAssignOp::DefinedOr) {
                Expr::Ternary {
                    cond: Box::new(Expr::Call {
                        name: Symbol::intern("defined"),
                        args: vec![lhs_expr.clone()],
                    }),
                    then_expr: Box::new(lhs_expr.clone()),
                    else_expr: Box::new(rhs),
                }
            } else {
                Expr::Binary {
                    left: Box::new(lhs_expr.clone()),
                    op: op.token_kind(),
                    right: Box::new(rhs),
                }
            };
            let stmt = Stmt::Expr(Expr::DoBlock {
                body: vec![
                    Stmt::VarDecl {
                        name: tmp_idx.clone(),
                        expr: (*index.clone()),
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
                        target: target.clone(),
                        index: Box::new(tmp_idx_expr),
                        value: Box::new(assigned_value),
                    }),
                ],
                label: None,
            });
            return parse_statement_modifier(r, stmt);
        }
        // Handle method call compound assignment: .key //= val, .key += val, etc.
        if let Expr::MethodCall {
            ref target,
            ref name,
            ref args,
            ..
        } = expr
        {
            let assigned_value = super::assign::compound_assigned_value_expr(expr.clone(), op, rhs);
            // Determine the topic variable name for writeback
            let topic_name = if let Expr::Var(ref v) = **target {
                v.clone()
            } else {
                "_".to_string()
            };
            let stmt = Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_assign_method_lvalue"),
                args: vec![
                    (**target).clone(),
                    Expr::Literal(crate::value::Value::str(name.resolve())),
                    Expr::ArrayLiteral(args.clone()),
                    assigned_value,
                    Expr::Literal(crate::value::Value::str(topic_name)),
                ],
            });
            return parse_statement_modifier(r, stmt);
        }
        let stmt = if matches!(expr, Expr::BracketArray(..))
            && matches!(op, super::assign::CompoundAssignOp::Comma)
        {
            Stmt::Expr(Expr::Binary {
                left: Box::new(expr),
                op: op.token_kind(),
                right: Box::new(rhs),
            })
        } else {
            Stmt::Expr(Expr::DoBlock {
                body: vec![
                    Stmt::Expr(expr),
                    Stmt::Expr(rhs),
                    Stmt::Expr(Expr::Call {
                        name: Symbol::intern("__mutsu_assignment_ro"),
                        args: Vec::new(),
                    }),
                ],
                label: None,
            })
        };
        return parse_statement_modifier(r, stmt);
    }

    // Check for comma-separated expressions (e.g., "1,2, until $++")
    // The statement modifier applies only to the last expression
    if rest.starts_with(',') && !rest.starts_with(",,") {
        let mut exprs = vec![expr];
        let mut r = rest;

        // Collect comma-separated expressions
        while r.starts_with(',') && !r.starts_with(",,") {
            let r2 = &r[1..];
            let (r2, _) = ws(r2)?;

            // Check if we hit a statement modifier - if so, stop parsing exprs
            if is_stmt_modifier_keyword(r2) {
                r = r2;
                break;
            }

            // Stop at semicolon, closing brace, or end of input
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                r = r2;
                break;
            }

            let (r2, next_expr) = expression(r2)?;
            exprs.push(next_expr);
            let (r2, _) = ws(r2)?;
            r = r2;
        }

        // Without a trailing statement modifier, keep this as a plain comma
        // expression statement (do not split into multiple statements).
        if !is_stmt_modifier_keyword(r) {
            let expr = if exprs.len() == 1 {
                exprs.remove(0)
            } else {
                Expr::ArrayLiteral(exprs)
            };
            add_xor_sink_warnings(&expr);
            return Ok((r, Stmt::Expr(expr)));
        }

        // Convert all but last expr to Stmt::Expr for modifier lowering
        let mut stmts = Vec::new();
        let last_expr = exprs.pop().unwrap();
        for e in exprs {
            stmts.push(Stmt::Expr(e));
        }

        // Apply statement modifier to the last expression
        let last_stmt = Stmt::Expr(last_expr);
        let (r, last_stmt_with_modifier) = parse_statement_modifier(r, last_stmt)?;
        stmts.push(last_stmt_with_modifier);

        // Return as a block
        return Ok((r, Stmt::Block(stmts)));
    }

    add_xor_sink_warnings(&expr);
    if let Expr::DoStmt(stmt) = expr.clone()
        && let Stmt::VarDecl { .. } = stmt.as_ref()
    {
        return parse_statement_modifier(rest, *stmt);
    }
    let stmt = Stmt::Expr(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse `let` statement: `let $var = expr`, `let $var`, `let @arr[idx] = expr`.
pub(super) fn let_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("let", input).ok_or_else(|| PError::expected("let statement"))?;
    let (rest, _) = ws1(rest)?;
    // Parse sigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after let"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after let"));
    }
    let rest_after_sigil = &rest[1..];
    let (rest, var_name) = super::ident(rest_after_sigil)?;
    // Env key: scalars strip $, arrays/hashes keep sigil
    let full_name = if sigil == '$' {
        var_name.clone()
    } else {
        format!("{}{}", sigil, var_name)
    };
    let (rest, _) = ws(rest)?;

    // Check for index: @arr[idx]
    if let Some(idx_rest) = rest.strip_prefix('[') {
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, idx_expr) = expression(idx_rest)?;
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, _) = parse_char(idx_rest, ']')?;
        let (idx_rest, _) = ws(idx_rest)?;
        if idx_rest.starts_with('=') && !idx_rest.starts_with("==") {
            let val_rest = &idx_rest[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: Some(Box::new(idx_expr)),
                    value: Some(Box::new(val_expr)),
                    is_temp: false,
                },
            );
        }
        return parse_statement_modifier(
            idx_rest,
            Stmt::Let {
                name: full_name,
                index: Some(Box::new(idx_expr)),
                value: None,
                is_temp: false,
            },
        );
    }

    // Check for assignment: let $var = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
                is_temp: false,
            },
        );
    }

    // Bare let: let $var / let @arr / let %hash
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
            is_temp: false,
        },
    )
}

/// Parse `temp` statement — same semantics as `let` (save/restore at scope exit).
pub(super) fn temp_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("temp", input).ok_or_else(|| PError::expected("temp statement"))?;
    let (rest, _) = ws1(rest)?;
    // temp on lvalue method call: `temp $obj.method = value`
    if let Ok((expr_rest, expr)) = expression(rest) {
        let (expr_rest_ws, _) = ws(expr_rest)?;
        if expr_rest_ws.starts_with('=')
            && !expr_rest_ws.starts_with("==")
            && let Expr::MethodCall {
                target,
                name,
                args,
                modifier: _,
                quoted: _,
            } = expr
            && let Expr::Var(var_name) = target.as_ref()
        {
            let rhs_rest = &expr_rest_ws[1..];
            let (rhs_rest, _) = ws(rhs_rest)?;
            let (rhs_rest, rhs_expr) = expression(rhs_rest)?;
            return parse_statement_modifier(
                rhs_rest,
                Stmt::TempMethodAssign {
                    var_name: var_name.clone(),
                    method_name: name.resolve(),
                    method_args: args,
                    value: rhs_expr,
                },
            );
        }
    }
    // Parse sigil + optional twigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after temp"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after temp"));
    }
    let after_sigil = &rest[1..];
    // Handle twigils: $*CWD, $?FILE, etc.
    let (after_twigil, twigil) = if after_sigil.starts_with('*')
        || after_sigil.starts_with('?')
        || after_sigil.starts_with('!')
    {
        (&after_sigil[1..], &after_sigil[..1])
    } else {
        (after_sigil, "")
    };
    let (rest, var_name) = super::ident(after_twigil)?;
    // Build full env key including twigil
    let full_name = if sigil == '$' {
        if twigil.is_empty() {
            var_name.clone()
        } else {
            format!("{}{}", twigil, var_name)
        }
    } else {
        format!("{}{}{}", sigil, twigil, var_name)
    };
    let (rest, _) = ws(rest)?;
    // Check for assignment: temp $*CWD = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
                is_temp: true,
            },
        );
    }
    // Bare temp: temp $var
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
            is_temp: true,
        },
    )
}
