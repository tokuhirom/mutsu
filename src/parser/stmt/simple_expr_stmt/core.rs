use crate::ast::{Expr, Stmt};
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, take_while1,
};
use crate::parser::primary::parse_call_arg_list;
use crate::parser::stmt::assign::{parse_assign_expr_or_comma, parse_colon_args};
use crate::parser::stmt::modifier::{is_stmt_modifier_keyword, parse_statement_modifier};
use crate::parser::stmt::simple::add_xor_sink_warnings;
use crate::parser::term_boundary::{is_pure_value_expr, starts_with_unambiguous_term};
use crate::symbol::Symbol;

use super::lvalue::callable_lvalue_assign_expr;
use super::predicates::{
    is_literal_expr, is_pseudo_package, starts_with_postfix_ambiguous_term, starts_with_term_token,
};
/// The statement `.=` on the topic (`$_ .= meth`, `.= meth`), tagged with its
/// source-level shape. It is an [`Expr::TopicDotAssign`]
/// (compiled to `TopicDotAssign`) so it can reassign a read-only
/// whole-container topic (`given @a { .=uc }`, writing through to `@a`) while a
/// plain `$_ = ...` keeps throwing X::Assignment::RO.
pub(crate) fn topic_dot_assign(method_call: Expr) -> Expr {
    let expanded = Expr::TopicDotAssign(Box::new(method_call.clone()));
    crate::parser::stmt::assign::dotty_assign_marker(
        Expr::Var("_".to_string()),
        method_call,
        expanded,
    )
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
            let (r, mut args) = parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, ()) = crate::parser::expr::parse_trailing_call_adverbs(r, &mut args)?;
            (r, args)
        } else {
            // `.=method: args` is the topic form of the same colon-argument
            // syntax supported by `$var .= method: args`. Keep the original
            // remainder when there is no argument list so statement modifiers
            // continue to see their leading whitespace.
            let (r, _) = ws(rest)?;
            if r.starts_with(':') && !r.starts_with("::") {
                parse_colon_args(r)?
            } else {
                (rest, Vec::new())
            }
        };
        // The `.=` metaop on the topic. Build an `Expr::TopicDotAssign`
        // marker (compiled to `TopicDotAssign`) so it can reassign a read-only
        // whole-container topic (`given @a { .=uc }`, writing through to `@a`)
        // while a plain `$_ = ...` keeps throwing X::Assignment::RO.
        let call = Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: Symbol::intern(&method_name),
            args,
            modifier: None,
            quoted: false,
            sugar: false,
        };
        let stmt = Stmt::Expr(crate::parser::stmt::assign::written_topic_dotty_assignment(
            call,
        ));
        return parse_statement_modifier(rest, stmt);
    }

    if let Ok((rest, expr)) = crate::parser::primary::string::qx_string(input)
        // A postfix chain on the qx result (`qx`pwd`.chomp.IO`) must parse as
        // one expression — without this guard the shortcut consumed only the
        // qx call and `.chomp` became a separate $_-method statement.
        && !rest.starts_with('.')
    {
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
    //
    // `consumed_span` rather than an `input.len() - rest.len()` subtraction: a
    // heredoc whose marker line carries trailing code (`Foo.new(:n(qq:to/E/)).throw
    // unless $ok`) resumes the parse on a freshly built buffer, so `rest` is not a
    // tail slice of `input` and the subtraction lands at an arbitrary offset — a
    // mid-character one panics (Collection, #7954). No contiguous source span then
    // exists to record, and neither wrapper has anything to preserve without one.
    let consumed = crate::parser::expr::consumed_span(input, rest);
    let expr = match (matches!(expr, Expr::Literal(_)), consumed) {
        (_, None) => expr,
        (true, Some(consumed)) => crate::parser::primary::wrap_divergent_literal(expr, consumed),
        // A bare colonpair statement (`:foo(42)`) parses to the same
        // `Binary { FatArrow }` as a fatarrow; record its source so a
        // sink-context warning echoes the colonpair form, not `foo => 42`.
        (false, Some(consumed)) => {
            crate::parser::primary::wrap_colonpair_sink_source(expr, consumed)
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
        return Err(PError::fatal_at(
            "Confused. Two terms in a row".to_string(),
            rest,
        ));
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
        return Err(PError::fatal_at(
            "Confused. Two terms in a row".to_string(),
            rest,
        ));
    }
    if !separated_by_newline
        && let Some(result) = super::block_arg_call::bareword_block_call(input, &expr, rest)
    {
        return result;
    }
    // Detect "unexpected block in infix position": a completed expression followed
    // by `{` on the same line without an infix operator is an error in Raku.
    // e.g., `(1) { $foo = 2 }` — parens do not eat spaces after them.
    if !separated_by_newline && is_pure_value_expr(&expr) && rest.starts_with('{') {
        return Err(PError::fatal_at(
            "Unexpected block in infix position (missing statement control word before the expression?)".to_string(),
            rest,
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
        // The typed `.=` node over the hyper call; its lowering writes each
        // result back through a variable or (index bound once) an element.
        match expr {
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::Index { .. } => {
                let call = make_rhs(expr.clone());
                let stmt = Stmt::Expr(crate::parser::stmt::assign::written_dotty_assignment(
                    expr, call,
                ));
                return parse_statement_modifier(r, stmt);
            }
            _ => {
                return Err(PError::expected("assignable expression before hyper '.='"));
            }
        }
    }
    if matches!(expr, Expr::Index { .. } | Expr::MultiDimIndex { .. }) && rest.starts_with(":=") {
        let rest = &rest[2..];
        let (rest, _) = ws(rest)?;
        // Assign-aware RHS parser so a chained bind whose next lvalue is also
        // indexed — `%h<a> := %h<b> := 7` — parses (the trailing `%h<b> := 7`
        // indexed bind is recognized instead of being left behind).
        let (rest, value) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected assigned expression after index bind",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        // Indexed bind RHSs are values too: a compound `*` expression must be
        // retained as WhateverCode instead of being evaluated as Numeric(Whatever).
        match expr {
            Expr::Index {
                target,
                index,
                is_positional,
                spelling,
            } => {
                let stmt = Stmt::Expr(super::lvalue::index_bind_expr(
                    target,
                    index,
                    is_positional,
                    spelling,
                    value,
                ));
                return parse_statement_modifier(rest, stmt);
            }
            Expr::MultiDimIndex {
                target,
                dimensions,
                is_positional,
            } => {
                // For bind (:=), use IndexAssign with flattened dimensions
                // because bind semantics are handled by the IndexAssign VM path
                let stmt = Stmt::Expr(super::lvalue::index_bind_expr(
                    target,
                    Box::new(Expr::ArrayLiteral(dimensions)),
                    is_positional,
                    crate::ast::IndexSpelling::SemicolonDimensions,
                    value,
                ));
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

    // Generic assignment on non-variable lhs (e.g. `.key = 1`).
    // TODO: Introduce a dedicated assignment AST form for arbitrary lvalues.
    // Current fallback consumes `lhs = rhs` and preserves both sides as expressions.
    // `$(EXPR) = a, b` forces ITEM assignment: `$(...)` names the same container
    // as EXPR but in item context, so the comma is NOT part of the RHS
    // (`($(EXPR) = a), b`) -- unlike a bare `@a[0] = a, b` list assignment. Parse
    // the RHS comma-blind and leave any trailing comma list for the enclosing
    // (statement-level) comma expression.
    let item_form =
        matches!(expr, Expr::Contextualizer { .. }).then(|| expr.clone().contextualizer_call());
    if let Expr::MethodCall { name, args, .. } = item_form.as_ref().unwrap_or(&expr)
        && name == "item"
        && args.is_empty()
        && rest.starts_with('=')
        && !rest.starts_with("==")
        && !rest.starts_with("=>")
    {
        let r = &rest[1..];
        let (r, _) = ws(r)?;
        let (mut r, value) = expression(r)?;
        let assign = crate::parser::written_assignment(expr.clone(), value);
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
        if matches!(
            expr,
            Expr::MethodCall { .. } | Expr::Call { .. } | Expr::CallOn { .. }
        ) {
            let stmt = crate::parser::stmt::assign::written_assignment_stmt(expr, rhs);
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
                Stmt::Expr(crate::parser::literal_assign_ro_expr(lit, rhs))
            }
            Expr::SymbolicDeref { sigil, expr: inner } => Stmt::Expr(Expr::SymbolicDerefAssign {
                sigil,
                expr: inner,
                value: Box::new(rhs),
            }),
            Expr::IndirectTypeLookup(inner) => Stmt::Expr(Expr::IndirectTypeLookupAssign {
                expr: inner,
                value: Box::new(rhs),
            }),
            // `-$x = 5`, `$<c> = $0`, `($a || $b) = 1`, `(my $v) = 1, 2`: the
            // assignment node, written through the target's result.
            target if crate::rakuast::is_callable_lvalue_assign_target(&target) => {
                Stmt::Expr(crate::parser::stmt::assign::written_assignment(target, rhs))
            }
            target if crate::rakuast::is_expression_assign_target(&target) => {
                crate::parser::stmt::assign::written_assignment_stmt(target, rhs)
            }
            // TODO: #15012 -- an operator result (`("a" R~ "b") = 1`) is written
            // through its container here instead of by the typed node.
            target => Stmt::Expr(callable_lvalue_assign_expr(target, Vec::new(), rhs)),
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
        if crate::ast::signature_bind::supports(&expr) {
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
            // `supports` held, so the lowering cannot fail.
            let stmt = crate::parser::stmt::control::parsed_block::finish_total(
                crate::ast::parsed::ParsedStmt::SignatureBind {
                    signature: expr.clone(),
                    rhs,
                },
            );
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
                        crate::value::Value::package(crate::symbol::Symbol::intern(type_name)),
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
        //
        // The target is spelled as a term key (`runtime::term_names`,
        // #9962) so the compiler can tell it from the sigil-stripped `$name`
        // an ordinary `$name := ...` carries.
        let stmt = if let Expr::BareWord(name) = expr {
            Stmt::Expr(Expr::AssignExpr {
                name: crate::term_names::term_key(&name),
                expr: Box::new(rhs),
                is_bind: true,
            })
        } else {
            Stmt::Block(vec![Stmt::Expr(expr), Stmt::Expr(rhs)])
        };
        return parse_statement_modifier(r, stmt);
    }

    // Check for comma-separated expressions (e.g., "1,2, until $++")
    // A trailing statement modifier gates the WHOLE comma list, not just its
    // last element (`$a++, $b++ if COND` runs neither or both — verified
    // against `raku`; HTTP::Server::Async's `$index--, last if
    // $data[$index] == ...` relies on the decrement and the `last` firing
    // together).
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

            let sigil = exprs
                .iter()
                .fold(crate::ast::left_sigil::LeftSigil::Unset, |sigil, e| {
                    crate::ast::left_sigil::after(sigil, e)
                });
            let (r2, next_expr) = crate::ast::left_sigil::inherit(r2, sigil, || expression(r2))?;
            exprs.push(next_expr);
            let (r2, _) = ws(r2)?;
            r = r2;
        }

        // Without a trailing statement modifier, keep this as a plain comma
        // expression statement (do not split into multiple statements).
        // A trailing comma before `}` always creates a list, even with
        // a single element (e.g. `{ 42, }` gives `(42,)`).
        if !is_stmt_modifier_keyword(r) {
            // This split the comma list by hand, so the operators that are looser than the
            // comma still need their real precedence: without this, `$[1], -> @p {...} ... *`
            // seeds the sequence with the pointy block alone, dropping `$[1]`.
            let mut exprs = crate::parser::stmt::assign::normalize_comma_list_items(exprs);
            let expr = if trailing_comma || exprs.len() > 1 {
                Expr::ArrayLiteral(exprs)
            } else {
                exprs.remove(0)
            };
            add_xor_sink_warnings(&expr, crate::parser::primary::current_line_number(input));
            return Ok((r, Stmt::Expr(expr)));
        }

        // Build one list expression for the entire comma-separated statement
        // and let the modifier wrap that whole list, same as the operators
        // that are looser than the comma (see the `normalize_comma_list_items`
        // call in the no-modifier branch above).
        let mut exprs = crate::parser::stmt::assign::normalize_comma_list_items(exprs);
        let list_expr = if exprs.len() > 1 {
            Expr::ArrayLiteral(exprs)
        } else {
            exprs.remove(0)
        };
        return parse_statement_modifier(r, Stmt::Expr(list_expr));
    }

    add_xor_sink_warnings(&expr, crate::parser::primary::current_line_number(input));
    if let Expr::DoStmt(stmt) = expr.clone()
        && let Stmt::VarDecl { .. } = stmt.as_ref()
    {
        return parse_statement_modifier(rest, *stmt);
    }
    // A block-final statement (`$lock.protect: { ... }` NL `if ...`) declines
    // the next line's modifier inside `parse_statement_modifier`, from the
    // position the block's `}` recorded — so `try foo($x)` NL `if COND;` still
    // takes its modifier ([#7993]).
    //
    // [#7993]: https://github.com/tokuhirom/mutsu/issues/7993
    parse_statement_modifier(rest, Stmt::Expr(expr))
}
