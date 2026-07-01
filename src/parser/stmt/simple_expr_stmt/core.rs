use std::sync::atomic::Ordering;

use crate::ast::{AssignOp, Expr, Stmt};
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, take_while1,
};
use crate::parser::primary::parse_call_arg_list;
use crate::parser::stmt::assign::{
    CompoundAssignOp, compound_assigned_value_expr, parse_assign_expr_or_comma,
    parse_comma_or_expr, parse_compound_assign_op,
};
use crate::parser::stmt::modifier::{is_stmt_modifier_keyword, parse_statement_modifier};
use crate::parser::stmt::simple::{
    TMP_INDEX_COUNTER, add_xor_sink_warnings, parse_hyper_assign_op,
};
use crate::symbol::Symbol;
use crate::value::Value;

use super::lvalue::{
    bind_source_metadata_expr, callable_lvalue_assign_expr, decl_target_var_name,
    grouped_assign_lvalue_stmt, method_lvalue_assign_expr, named_sub_lvalue_assign_expr,
    single_target_list_lvalue_stmt,
};
use super::predicates::{
    index_bind_target_is_immutable, is_literal_expr, is_pseudo_package, is_pure_value_expr,
    starts_with_postfix_ambiguous_term, starts_with_term_token, starts_with_unambiguous_term,
};
use super::sig_info::{SigParamInfo, extract_signature_param_infos, extract_static_named_map};

/// Desugar `LVALUE = RHS` for a method-call / sub-call / callable lvalue into the
/// corresponding `__mutsu_assign_*` call expression. Mirrors the inline handling
/// of the main `=` assignment path; factored out so a statement-prefix-wrapped
/// lvalue (`try $obj.x = v`) can push the assignment *inside* the prefix.
fn lvalue_assign_to_expr(lvalue: Expr, rhs: Expr) -> Expr {
    match lvalue {
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted: _,
        } => {
            if name == "AT-POS" && args.len() == 1 {
                Expr::IndexAssign {
                    target: target.clone(),
                    index: Box::new(args[0].clone()),
                    value: Box::new(rhs),
                    is_positional: true,
                }
            } else {
                let target_var_name = match target.as_ref() {
                    Expr::Var(v) => Some(v.clone()),
                    Expr::ArrayVar(v) => Some(format!("@{}", v)),
                    Expr::HashVar(v) => Some(format!("%{}", v)),
                    Expr::DoStmt(s) => decl_target_var_name(s),
                    _ => None,
                };
                let method_name = if modifier == Some('!') {
                    format!("!{}", name.resolve())
                } else {
                    name.resolve()
                };
                method_lvalue_assign_expr(*target, target_var_name, method_name, args, rhs)
            }
        }
        Expr::Call { name, args } => named_sub_lvalue_assign_expr(name.resolve(), args, rhs),
        Expr::CallOn { target, args } => callable_lvalue_assign_expr(*target, args, rhs),
        other => callable_lvalue_assign_expr(other, Vec::new(), rhs),
    }
}

/// Parse an expression statement (fallback).
pub(crate) fn expr_stmt(input: &str) -> PResult<'_, Stmt> {
    // Topic mutating method call: .=method(args)
    // This assigns the result back to $_, equivalent to $_ = $_.method(args)
    if let Some(stripped) = input.strip_prefix(".=") {
        let (rest, _) = ws(stripped)?;
        let (rest, method_name) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else {
            (rest, Vec::new())
        };
        // The `.=` metaop on the topic. Route through the `__mutsu_topic_dotassign`
        // marker (compiled to `TopicDotAssign`) so it can reassign a read-only
        // whole-container topic (`given @a { .=uc }`, writing through to `@a`)
        // while a plain `$_ = ...` keeps throwing X::Assignment::RO.
        let stmt = Stmt::Expr(Expr::Call {
            name: Symbol::intern("__mutsu_topic_dotassign"),
            args: vec![Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name: Symbol::intern(&method_name),
                args,
                modifier: None,
                quoted: false,
            }],
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
            if err.is_fatal() {
                return Err(err);
            } else if let Ok(parsed_assign) = parse_assign_expr_or_comma(input) {
                parsed_assign
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

    // When the whole statement expression is a bare numeric literal whose source
    // format diverges from its canonical value (`0xFF`, `6.02e23`, `∞`), record
    // the original text so a sink-context "Useless use" warning preserves it.
    // Confined to this statement position so the wrapper never leaks into
    // signatures / type checks / ranges (see `number::wrap_divergent_literal`).
    let expr = if matches!(expr, Expr::Literal(_)) {
        let consumed = &input[..input.len() - rest.len()];
        crate::parser::primary::wrap_divergent_literal(expr, consumed)
    } else {
        // A bare colonpair statement (`:foo(42)`) parses to the same
        // `Binary { FatArrow }` as a fatarrow; record its source so a
        // sink-context warning echoes the colonpair form, not `foo => 42`.
        let consumed = &input[..input.len() - rest.len()];
        crate::parser::primary::wrap_colonpair_sink_source(expr, consumed)
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
    // Detect "two terms in a row" after a postfix increment/decrement: a postfix
    // `++`/`--` (whether written as `$n++`, `$n.++`, or via an unspace such as
    // `$n\ ++`) followed on the same line by an unambiguous new term (a sigilled
    // variable, number, or string literal) is a syntax error in Raku. This is the
    // postfix-vs-infix ambiguity rule: e.g. `$n++$m`, `$n.++ $m`, `$n\ ++ $m`.
    // We only flag continuations that cannot begin an infix operator (sigils,
    // digits, quotes) so word infixes like `and`/`or`/`xx` are not misflagged.
    if !separated_by_newline
        && matches!(
            &expr,
            Expr::PostfixOp {
                op: crate::token_kind::TokenKind::PlusPlus
                    | crate::token_kind::TokenKind::MinusMinus,
                ..
            }
        )
        && !is_stmt_modifier_keyword(rest)
        && starts_with_postfix_ambiguous_term(rest)
    {
        return Err(PError::fatal("Confused. Two terms in a row".to_string()));
    }
    // Detect "unexpected block in infix position": a completed expression followed
    // by `{` on the same line without an infix operator is an error in Raku.
    // e.g., `(1) { $foo = 2 }` — parens do not eat spaces after them.
    if !separated_by_newline && is_pure_value_expr(&expr) && rest.starts_with('{') {
        return Err(PError::fatal(
            "Unexpected block in infix position (missing statement control word before the expression?)".to_string(),
        ));
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
            let (r, args) = parse_call_arg_list(r)?;
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
            Expr::Index {
                target,
                index,
                is_positional,
                ..
            } => {
                let tmp_idx = format!(
                    "__mutsu_idx_{}",
                    TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
                );
                let tmp_idx_expr = Expr::Var(tmp_idx.clone());
                let lhs_expr = Expr::Index {
                    target: target.clone(),
                    index: Box::new(tmp_idx_expr.clone()),
                    is_positional,
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
                            is_positional: true,
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
        let r_before_ws = r;
        let (r, _) = ws(r)?;
        let (r, method_args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
            // Colon-arg syntax: .=method: arg
            let r2 = &r_before_ws[1..];
            let (r2, _) = ws(r2)?;
            let (r2, first_arg) = crate::parser::expr::expression(r2)?;
            let mut args = vec![first_arg];
            let mut r_inner = r2;
            loop {
                let (r3, _) = ws(r_inner)?;
                if r3.starts_with(':')
                    && !r3.starts_with("::")
                    && let Ok((r4, arg)) = crate::parser::primary::misc::colonpair_expr(r3)
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
                let (r3, next) = crate::parser::expr::expression(r3)?;
                args.push(next);
                r_inner = r3;
            }
            (r_inner, args)
        } else if r.starts_with(':') && !r.starts_with("::") {
            // Fake-infix adverb form: .=method :key<val>
            let mut args = Vec::new();
            let mut r_inner = r;
            while r_inner.starts_with(':') && !r_inner.starts_with("::") {
                if let Ok((r2, arg)) = crate::parser::primary::misc::colonpair_expr(r_inner) {
                    args.push(arg);
                    let (r3, _) = ws(r2)?;
                    r_inner = r3;
                } else {
                    break;
                }
            }
            (r_inner, args)
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
            // `$_ .= meth`: route the topic metaop through `__mutsu_topic_dotassign`
            // (see the leading-dot case above).
            Expr::Var(name) if name == "_" => {
                let stmt = Stmt::Expr(Expr::Call {
                    name: Symbol::intern("__mutsu_topic_dotassign"),
                    args: vec![make_rhs(Expr::Var("_".to_string()))],
                });
                return parse_statement_modifier(r, stmt);
            }
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
            Expr::Index {
                target,
                index,
                is_positional,
                ..
            } => {
                let tmp_idx = format!(
                    "__mutsu_idx_{}",
                    TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
                );
                let tmp_idx_expr = Expr::Var(tmp_idx.clone());
                let lhs_expr = Expr::Index {
                    target: target.clone(),
                    index: Box::new(tmp_idx_expr.clone()),
                    is_positional,
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
                            is_positional: true,
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
            let stmt = Stmt::Expr(Expr::MultiDimIndexAssign {
                target,
                dimensions,
                value: Box::new(value),
            });
            return parse_statement_modifier(rest, stmt);
        }
        if let Expr::Index {
            target,
            index,
            is_positional,
        } = expr
        {
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
                    is_positional,
                });
                return parse_statement_modifier(rest, stmt);
            }
            let stmt = Stmt::Expr(Expr::IndexAssign {
                target,
                index,
                value: Box::new(value),
                is_positional,
            });
            return parse_statement_modifier(rest, stmt);
        }
    }
    if let Expr::Index {
        target,
        index,
        is_positional,
    } = expr.clone()
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
            is_positional,
        });
        return parse_statement_modifier(rest, stmt);
    }
    if matches!(expr, Expr::Index { .. } | Expr::MultiDimIndex { .. }) && rest.starts_with(":=") {
        // Binding into an immutable subscript target (`(1,2)[0] := 3`,
        // `10[0] := 1`, `"Hi"[0] := 1`) is illegal — Raku raises X::Bind.
        if let Expr::Index { target, .. } = &expr
            && index_bind_target_is_immutable(target)
        {
            let message = "Cannot use bind operator with this left-hand side".to_string();
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Bind"),
                std::collections::HashMap::from([(
                    "message".to_string(),
                    crate::value::Value::str(message.clone()),
                )]),
            );
            return Err(PError::fatal_with_exception(message, Box::new(ex)));
        }
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
            Expr::Index {
                target,
                index,
                is_positional,
            } => {
                let stmt = Stmt::Expr(Expr::IndexAssign {
                    target,
                    index,
                    value: Box::new(bind_value),
                    is_positional,
                });
                return parse_statement_modifier(rest, stmt);
            }
            Expr::MultiDimIndex { target, dimensions } => {
                // For bind (:=), use IndexAssign with flattened dimensions
                // because bind semantics are handled by the IndexAssign VM path
                let stmt = Stmt::Expr(Expr::IndexAssign {
                    target,
                    index: Box::new(Expr::ArrayLiteral(dimensions)),
                    value: Box::new(bind_value),
                    is_positional: true,
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
        let (r, target_expr) = parse_assign_expr_or_comma(r).map_err(|err| PError {
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
            modifier,
            quoted: _,
        } = &target_expr
        {
            let target_var_name = match target.as_ref() {
                Expr::Var(var_name) => Some(var_name.clone()),
                Expr::ArrayVar(var_name) => Some(format!("@{}", var_name)),
                Expr::HashVar(var_name) => Some(format!("%{}", var_name)),
                Expr::DoStmt(s) => decl_target_var_name(s),
                _ => None,
            };
            let method_name = if *modifier == Some('!') {
                format!("!{}", name.resolve())
            } else {
                name.resolve()
            };
            let assigned = method_lvalue_assign_expr(
                (**target).clone(),
                target_var_name,
                method_name,
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
            Expr::SymbolicDeref { sigil, expr: inner } => Stmt::Expr(Expr::SymbolicDerefAssign {
                sigil,
                expr: inner,
                value: Box::new(expr),
            }),
            Expr::IndirectTypeLookup(inner) => Stmt::Expr(Expr::IndirectTypeLookupAssign {
                expr: inner,
                value: Box::new(expr),
            }),
            target => {
                if let Some(stmt) = grouped_assign_lvalue_stmt(&target, expr.clone()) {
                    stmt
                } else if let Some(stmt) =
                    single_target_list_lvalue_stmt(target.clone(), expr.clone())
                {
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
    // `$(EXPR) = a, b` forces ITEM assignment: `$(...)` names the same container
    // as EXPR but in item context, so the comma is NOT part of the RHS
    // (`($(EXPR) = a), b`) -- unlike a bare `@a[0] = a, b` list assignment. Parse
    // the RHS comma-blind and leave any trailing comma list for the enclosing
    // (statement-level) comma expression.
    if let Expr::MethodCall {
        target, name, args, ..
    } = &expr
        && name == "item"
        && args.is_empty()
        && rest.starts_with('=')
        && !rest.starts_with("==")
        && !rest.starts_with("=>")
    {
        let inner_target = (**target).clone();
        let r = &rest[1..];
        let (r, _) = ws(r)?;
        let (mut r, value) = expression(r)?;
        let assign = crate::parser::expr::precedence::assign_to_target_expr(inner_target, value);
        let (r_ws, _) = ws(r)?;
        r = r_ws;
        let result = if r.starts_with(',') && !r.starts_with(",,") {
            let mut items = vec![assign];
            while r.starts_with(',') && !r.starts_with(",,") {
                let (c, _) = parse_char(r, ',')?;
                let (c, _) = ws(c)?;
                if c.is_empty() || c.starts_with(';') || c.starts_with('}') {
                    r = c;
                    break;
                }
                let (c, item) = expression(c)?;
                items.push(item);
                let (c, _) = ws(c)?;
                r = c;
            }
            Expr::ArrayLiteral(items)
        } else {
            assign
        };
        return parse_statement_modifier(r, Stmt::Expr(result));
    }
    if !matches!(expr, Expr::AssignExpr { .. })
        && rest.starts_with('=')
        && !rest.starts_with("==")
        && !rest.starts_with("=>")
    {
        let r = &rest[1..];
        let (r, _) = ws(r)?;
        let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| PError {
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
            modifier,
            quoted: _,
        } = &expr
        {
            let assigned = if name == "AT-POS" && args.len() == 1 {
                Expr::IndexAssign {
                    target: target.clone(),
                    index: Box::new(args[0].clone()),
                    value: Box::new(rhs),
                    is_positional: true,
                }
            } else {
                let target_var_name = match target.as_ref() {
                    Expr::Var(var_name) => Some(var_name.clone()),
                    Expr::ArrayVar(var_name) => Some(format!("@{}", var_name)),
                    Expr::HashVar(var_name) => Some(format!("%{}", var_name)),
                    Expr::DoStmt(s) => decl_target_var_name(s),
                    _ => None,
                };
                let method_name = if *modifier == Some('!') {
                    format!("!{}", name.resolve())
                } else {
                    name.resolve()
                };
                method_lvalue_assign_expr(
                    (**target).clone(),
                    target_var_name,
                    method_name,
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
            // Assigning to an immutable literal (`120 = 3`, `"a" = 3`, `1.0 = 3`,
            // `1e0 = 3`) is illegal: Raku throws X::Assignment::RO. Pass the
            // literal to `__mutsu_assignment_ro` so it can report the value's type
            // and repr. A source-preserving `LiteralSrc` is unwrapped to its plain
            // literal first (the sink-warn source text is irrelevant here).
            Expr::Literal(_) | Expr::LiteralSrc(..) => {
                let lit = match expr {
                    Expr::LiteralSrc(v, _) => Expr::Literal(v),
                    other => other,
                };
                Stmt::Expr(Expr::DoBlock {
                    body: vec![
                        Stmt::Expr(rhs),
                        Stmt::Expr(Expr::Call {
                            name: Symbol::intern("__mutsu_assignment_ro"),
                            args: vec![lit],
                        }),
                    ],
                    label: None,
                })
            }
            Expr::BareWord(_) => Stmt::Block(vec![Stmt::Expr(expr), Stmt::Expr(rhs)]),
            Expr::SymbolicDeref { sigil, expr: inner } => Stmt::Expr(Expr::SymbolicDerefAssign {
                sigil,
                expr: inner,
                value: Box::new(rhs),
            }),
            Expr::IndirectTypeLookup(inner) => Stmt::Expr(Expr::IndirectTypeLookupAssign {
                expr: inner,
                value: Box::new(rhs),
            }),
            // `(try LVALUE) = RHS` / `(do LVALUE) = RHS`: a statement prefix binds
            // looser than `=`, so the assignment belongs *inside* the prefix:
            // `try (LVALUE = RHS)`. Without this the Try node itself becomes the
            // lvalue ("cannot assign through non-callable value").
            Expr::Try { body, catch } if matches!(body.as_slice(), [Stmt::Expr(_)]) => {
                let inner = match body.into_iter().next() {
                    Some(Stmt::Expr(e)) => e,
                    _ => unreachable!(),
                };
                Stmt::Expr(Expr::Try {
                    body: vec![Stmt::Expr(lvalue_assign_to_expr(inner, rhs))],
                    catch,
                })
            }
            target => {
                if let Some(stmt) = grouped_assign_lvalue_stmt(&target, rhs.clone()) {
                    stmt
                } else if let Some(stmt) =
                    single_target_list_lvalue_stmt(target.clone(), rhs.clone())
                {
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
    if !matches!(expr, Expr::AssignExpr { .. })
        && (rest.starts_with(":=") || rest.starts_with("::="))
    {
        // Signature binding: `:($f, $o, $) := @a` or `:(:type($t)) := (...)`
        // Extract variable info from the Signature literal and generate assignments.
        if let Some(param_infos) = extract_signature_param_infos(&expr) {
            let r = if let Some(stripped) = rest.strip_prefix("::=") {
                stripped
            } else {
                &rest[2..]
            };
            let (r, _) = ws(r)?;
            let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after ':='",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?;
            // Try static lookup for named params: if rhs is a CaptureLiteral
            // or comma-list of FatArrow pairs, we can bind each named param
            // directly to its source expression (preserving identity).
            let static_named = extract_static_named_map(&rhs);
            let has_named = param_infos.iter().any(|p: &SigParamInfo| p.is_named);
            let mut stmts = Vec::new();
            // Declare and assign the temp variable (used for positional params
            // and for named params when no static map is available).
            let tmp_name = format!(
                "__sig_bind_tmp_{}",
                TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let need_tmp = param_infos.iter().any(|p: &SigParamInfo| !p.is_named)
                || (has_named && static_named.is_none());
            if need_tmp {
                stmts.push(Stmt::VarDecl {
                    name: tmp_name.clone(),
                    expr: rhs.clone(),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                });
            } else {
                // Still evaluate rhs for side effects.
                stmts.push(Stmt::Expr(rhs.clone()));
            }
            let mut pos_index: usize = 0;
            for info in param_infos.iter() {
                if info.sigiled_name.is_empty() {
                    if !info.is_named {
                        pos_index += 1;
                    }
                    continue;
                }
                let assign_name = if let Some(stripped) = info.sigiled_name.strip_prefix('$') {
                    stripped.to_string()
                } else {
                    info.sigiled_name.clone()
                };
                let value_expr = if info.is_named {
                    // Find a key match in static_named, or fall back to .hash<key>
                    let key = info
                        .named_keys
                        .first()
                        .cloned()
                        .unwrap_or_else(|| assign_name.clone());
                    if let Some(map) = static_named.as_ref() {
                        match map.get(&key) {
                            Some(e) => e.clone(),
                            None => Expr::Literal(Value::Nil),
                        }
                    } else {
                        // Runtime: $tmp.hash{key}
                        Expr::Index {
                            target: Box::new(Expr::MethodCall {
                                target: Box::new(Expr::Var(tmp_name.clone())),
                                name: Symbol::intern("hash"),
                                args: Vec::new(),
                                modifier: None,
                                quoted: false,
                            }),
                            index: Box::new(Expr::Literal(Value::str(key))),
                            is_positional: false,
                        }
                    }
                } else {
                    let i = pos_index;
                    pos_index += 1;
                    Expr::Index {
                        target: Box::new(Expr::Var(tmp_name.clone())),
                        index: Box::new(Expr::Literal(Value::Int(i as i64))),
                        is_positional: true,
                    }
                };
                stmts.push(Stmt::Assign {
                    name: assign_name,
                    expr: value_expr,
                    op: crate::ast::AssignOp::Bind,
                });
            }
            let stmt = Stmt::SyntheticBlock(stmts);
            return parse_statement_modifier(r, stmt);
        }
        // Reject binding to a zen slice: `@a[] := ...` / `%a{} := ...` → X::Bind::ZenSlice
        if let Expr::ZenSlice(inner) = &expr {
            let type_name = match inner.as_ref() {
                Expr::HashVar(_) => "Hash",
                _ => "Array",
            };
            let message = format!("Cannot bind to {} zen slice", type_name);
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Bind::ZenSlice"),
                std::collections::HashMap::from([
                    (
                        "type".to_string(),
                        crate::value::Value::Package(crate::symbol::Symbol::intern(type_name)),
                    ),
                    (
                        "message".to_string(),
                        crate::value::Value::str(message.clone()),
                    ),
                ]),
            );
            return Err(PError::fatal_with_exception(message, Box::new(ex)));
        }
        // Reject binding to a pseudo-package (`OUTER := 5`) → X::Bind with target.
        if let Expr::BareWord(name) = &expr
            && is_pseudo_package(name)
        {
            let message = format!("Cannot bind to pseudo-package {}", name);
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Bind"),
                std::collections::HashMap::from([
                    (
                        "message".to_string(),
                        crate::value::Value::str(message.clone()),
                    ),
                    (
                        "target".to_string(),
                        crate::value::Value::str(name.to_string()),
                    ),
                ]),
            );
            return Err(PError::fatal_with_exception(message, Box::new(ex)));
        }
        // Reject binding to a literal (`0 := 1`) or a function call (`f() := 2`)
        // — neither is a bindable container. → X::Bind
        if is_literal_expr(&expr) || matches!(expr, Expr::Call { .. }) {
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Bind"),
                std::collections::HashMap::from([(
                    "message".to_string(),
                    crate::value::Value::str(
                        "Cannot use bind operator with this left-hand side".to_string(),
                    ),
                )]),
            );
            return Err(PError::fatal_with_exception(
                "Cannot use bind operator with this left-hand side".to_string(),
                Box::new(ex),
            ));
        }
        let r = if let Some(stripped) = rest.strip_prefix("::=") {
            stripped
        } else {
            &rest[2..]
        };
        let (r, _) = ws(r)?;
        let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after ':='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        // A sigilless bareword on the LHS of `:=` binds to that named symbol
        // (e.g. a `constant`). Emit a real bind so it raises for a readonly
        // constant ("terms cannot be rebound") instead of silently evaluating
        // both sides as a no-op block.
        let stmt = if let Expr::BareWord(name) = expr {
            Stmt::Expr(Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
                is_bind: true,
            })
        } else {
            Stmt::Block(vec![Stmt::Expr(expr), Stmt::Expr(rhs)])
        };
        return parse_statement_modifier(r, stmt);
    }

    // Check for assignment after parenthesized assign expression: ($x = $y) = 5
    if let Expr::AssignExpr { ref name, .. } = expr {
        if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
            let var_name = name.clone();
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| PError {
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
        if let Some((stripped, op)) = parse_compound_assign_op(rest) {
            let var_name = name.clone();
            let (r, _) = ws(stripped)?;
            let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| PError {
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
        && let Some((stripped, op)) = parse_compound_assign_op(rest)
    {
        let (r, _) = ws(stripped)?;
        let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        if let Expr::Index {
            target,
            index,
            is_positional,
            ..
        } = &expr
        {
            let tmp_idx = format!(
                "__mutsu_idx_{}",
                TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
            );
            let tmp_idx_expr = Expr::Var(tmp_idx.clone());
            let lhs_expr = Expr::Index {
                target: target.clone(),
                index: Box::new(tmp_idx_expr.clone()),
                is_positional: *is_positional,
            };
            let assigned_value = if matches!(op, CompoundAssignOp::DefinedOr) {
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
                        is_positional: true,
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
            ref modifier,
            ..
        } = expr
        {
            let assigned_value = compound_assigned_value_expr(expr.clone(), op, rhs);
            // Determine the topic variable name for writeback
            let topic_name = if let Expr::Var(ref v) = **target {
                v.clone()
            } else {
                "_".to_string()
            };
            let method_name = if *modifier == Some('!') {
                format!("!{}", name.resolve())
            } else {
                name.resolve()
            };
            let stmt = Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_assign_method_lvalue"),
                args: vec![
                    (**target).clone(),
                    Expr::Literal(crate::value::Value::str(method_name)),
                    Expr::ArrayLiteral(args.clone()),
                    assigned_value,
                    Expr::Literal(crate::value::Value::str(topic_name)),
                ],
            });
            return parse_statement_modifier(r, stmt);
        }
        let stmt =
            if matches!(expr, Expr::BracketArray(..)) && matches!(op, CompoundAssignOp::Comma) {
                Stmt::Expr(Expr::Binary {
                    left: Box::new(expr),
                    op: op.token_kind(),
                    right: Box::new(rhs),
                })
            } else if matches!(
                op,
                CompoundAssignOp::KeywordOr
                    | CompoundAssignOp::KeywordAnd
                    | CompoundAssignOp::LogicalOr
                    | CompoundAssignOp::LogicalAnd
                    | CompoundAssignOp::DefinedOr
                    | CompoundAssignOp::Orelse
                    | CompoundAssignOp::Andthen
            ) {
                // Short-circuit compound assignment on non-lvalue: preserve
                // short-circuit semantics so the RHS is not evaluated when
                // the LHS triggers short-circuit.
                Stmt::Expr(Expr::Binary {
                    left: Box::new(expr),
                    op: op.token_kind(),
                    right: Box::new(Expr::DoBlock {
                        body: vec![
                            Stmt::Expr(rhs),
                            Stmt::Expr(Expr::Call {
                                name: Symbol::intern("__mutsu_assignment_ro"),
                                args: Vec::new(),
                            }),
                        ],
                        label: None,
                    }),
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
        let mut trailing_comma = false;

        // Collect comma-separated expressions
        while r.starts_with(',') && !r.starts_with(",,") {
            let r2 = &r[1..];
            let (r2, _) = ws(r2)?;

            // Check if we hit a statement modifier - if so, stop parsing exprs
            if is_stmt_modifier_keyword(r2) {
                r = r2;
                break;
            }

            // Stop at semicolon, closing brace, or end of input.
            // A trailing comma before `}` creates a list (e.g. `42,`).
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                trailing_comma = r2.starts_with('}');
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
        // A trailing comma before `}` always creates a list, even with
        // a single element (e.g. `{ 42, }` gives `(42,)`).
        if !is_stmt_modifier_keyword(r) {
            let expr = if trailing_comma || exprs.len() > 1 {
                Expr::ArrayLiteral(exprs)
            } else {
                exprs.remove(0)
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
    let stmt = Stmt::Expr(expr.clone());
    // For block-valued expressions (try { ... }, gather { ... }),
    // pass pre-whitespace rest so parse_statement_modifier can detect
    // newline separation and avoid treating the next line's `if`/`for`
    // as a statement modifier.
    if separated_by_newline && matches!(expr, Expr::Try { .. } | Expr::Gather(_)) {
        return parse_statement_modifier(rest_before_ws, stmt);
    }
    parse_statement_modifier(rest, stmt)
}
