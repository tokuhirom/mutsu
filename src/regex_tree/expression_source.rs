//! Source reconstruction for main-slang expressions embedded in regex trees.

/// Render a parenthesized argument expression, preserving a top-level comma
/// list as the comma list rather than an array composer.
// Cost: O(n), n = total expression size.
pub(crate) fn expression_argument_source(expr: &crate::ast::Expr) -> Option<String> {
    match expr {
        crate::ast::Expr::ArrayLiteral(items) => items
            .iter()
            .map(expression_source)
            .collect::<Option<Vec<_>>>()
            .map(|parts| parts.join(", ")),
        other => expression_source(other),
    }
}

// Cost: O(n), n = size of the expression tree.
pub(crate) fn expression_source(expr: &crate::ast::Expr) -> Option<String> {
    fn quote_string(value: &str) -> String {
        let mut out = String::with_capacity(value.len() + 2);
        out.push('\'');
        for ch in value.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '\'' => out.push_str("\\'"),
                _ => out.push(ch),
            }
        }
        out.push('\'');
        out
    }

    fn join_args(args: &[crate::ast::Expr]) -> Option<String> {
        args.iter()
            .map(expression_source)
            .collect::<Option<Vec<_>>>()
            .map(|parts| parts.join(", "))
    }

    match expr {
        crate::ast::Expr::Literal(value) => match value.view() {
            crate::value::ValueView::Str(value) => Some(quote_string(&value)),
            crate::value::ValueView::Int(_)
            | crate::value::ValueView::BigInt(_)
            | crate::value::ValueView::Num(_)
            | crate::value::ValueView::Bool(_)
            | crate::value::ValueView::Rat(..)
            | crate::value::ValueView::FatRat(..)
            | crate::value::ValueView::BigRat(..)
            | crate::value::ValueView::Complex(..)
            | crate::value::ValueView::Nil => Some(value.to_string_value()),
            _ => None,
        },
        crate::ast::Expr::LiteralSrc(_, source) => Some(source.to_string()),
        // `<v(/\d+/)>`: a regex argument, spelled from its tree. A `/` in the
        // pattern would end the spelled literal early.
        crate::ast::Expr::RegexLiteral { tree, .. }
            if tree.adverbs.is_empty() && tree.declaration_kind.is_none() =>
        {
            let source = tree.to_source();
            (!source.contains('/')).then(|| format!("/{source}/"))
        }
        // `"a$b"`: each computed part as an interpolated `{…}` block, which
        // reads the same value as the variable spelling.
        crate::ast::Expr::StringInterpolation(parts) => {
            let mut out = String::from("\"");
            for part in parts {
                match part {
                    crate::ast::Expr::Literal(value)
                        if let crate::value::ValueView::Str(text) = value.view() =>
                    {
                        for ch in text.chars() {
                            if matches!(ch, '\\' | '"' | '$' | '@' | '%' | '&' | '{' | '}') {
                                out.push('\\');
                            }
                            out.push(ch);
                        }
                    }
                    other => {
                        out.push('{');
                        out.push_str(&expression_source(other)?);
                        out.push('}');
                    }
                }
            }
            out.push('"');
            Some(out)
        }
        crate::ast::Expr::AssignExpr {
            name,
            expr,
            is_bind: false,
        } if !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || matches!(c, '_' | '-')) => {
            Some(format!("${name} = {}", expression_source(expr)?))
        }
        crate::ast::Expr::AnonSub {
            body,
            is_rw: false,
            is_raw: false,
            is_block: true,
            ..
        } => closure_body_source(body),
        crate::ast::Expr::PostfixOp { op, expr } => Some(format!(
            "{}{}",
            expression_source(expr)?,
            crate::compiler::helpers_ops::token_kind_to_op_name(op)
        )),
        // `-> { ... }`: a pointy block with an empty signature.
        crate::ast::Expr::AnonSubParams {
            params,
            param_defs,
            body,
            return_type: None,
            is_rw: false,
            is_raw: false,
            is_whatever_code: false,
            declarator: crate::ast::RoutineDeclarator::Block | crate::ast::RoutineDeclarator::PointyBlock,
            ..
        } if params.is_empty() && param_defs.is_empty() => {
            Some(format!("-> {}", closure_body_source(body)?))
        }
        // A placeholder block (`{ $^n * 2 }`, `{ @_ }`, `{ %_ }`): its source is
        // the braces over the body, which declares the placeholders again.
        crate::ast::Expr::AnonSubParams { body, .. }
            if crate::ast::placeholders::is_implicit_placeholder_block(expr) =>
        {
            closure_body_source(body)
        }
        // A WhateverCode (`* + 1`) is spelled as the expression over its `*`s.
        crate::ast::Expr::WhateverCurry(inner) => expression_source(inner),
        crate::ast::Expr::Whatever | crate::ast::Expr::WhateverArg => Some("*".to_string()),
        crate::ast::Expr::Grouped(inner) => Some(format!("({})", expression_source(inner)?)),
        crate::ast::Expr::CaptureVar(name) => Some(format!("$<{name}>")),
        crate::ast::Expr::Var(_) | crate::ast::Expr::ArrayVar(_) | crate::ast::Expr::HashVar(_) => {
            expr.sigiled_var_name()
        }
        crate::ast::Expr::CodeVar(name) => Some(format!("&{name}")),
        crate::ast::Expr::BareWord(name) => Some(name.clone()),
        crate::ast::Expr::Unary { op, expr, .. } => Some(format!(
            "{}{}",
            crate::compiler::helpers_ops::token_kind_to_op_name(op),
            expression_source(expr)?
        )),
        // Whatever-derived match indexes such as `*-1` need the operator
        // attached to `*`; whitespace would turn the parser's compact nth
        // spelling into an invalid index token.
        crate::ast::Expr::Binary {
            left,
            op,
            right,
            ..
        } if matches!(left.as_ref(), crate::ast::Expr::Whatever)
            && crate::compiler::helpers_ops::token_kind_to_op_name(op) == "-" =>
        {
            Some(format!("*-{}", expression_source(right)?))
        }
        crate::ast::Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
            ..
        } if matches!(left.as_ref(), crate::ast::Expr::Literal(v)
            if matches!(v.view(), crate::value::ValueView::Str(key)
                if crate::parser::is_identifier_spelling(&key))) =>
        {
            let crate::ast::Expr::Literal(key) = left.as_ref() else { unreachable!() };
            Some(format!("{} => {}", key.to_string_value(), expression_source(right)?))
        }
        crate::ast::Expr::Binary {
            left, op, right, ..
        } => Some(format!(
            "{} {} {}",
            expression_source(left)?,
            crate::compiler::helpers_ops::token_kind_to_op_name(op),
            expression_source(right)?
        )),
        // Keep a dynamic selector grouped when a constructed RakuAST regex is
        // lowered through the established string parser.  Its branches remain
        // expressions for the existing match-time subrule-argument evaluator.
        crate::ast::Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Some(format!(
            "({} ?? {} !! {})",
            expression_source(cond)?,
            expression_source(then_expr)?,
            expression_source(else_expr)?
        )),
        crate::ast::Expr::Call { name, args, .. }
        | crate::ast::Expr::UserRoutineCall { name, args, .. } => {
            Some(format!("{}({})", name.resolve(), join_args(args)?))
        }
        // A lexical callable invocation (`&name(args)` / `$callable(args)`)
        // is represented as `CallOn` because the callee is an expression
        // rather than a statically resolved routine name. Preserve that
        // expression boundary so a constructed RakuAST regex returns through
        // the existing match-time argument evaluator.
        crate::ast::Expr::CallOn { target, args } => Some(format!(
            "{}({})",
            expression_source(target)?,
            join_args(args)?
        )),
        // A pointy block used as a named regex argument must be rendered back
        // as a pointy block rather than as an opaque closure value. The body
        // remains unevaluated here; the existing regex argument evaluator
        // invokes it at match time.
        crate::ast::Expr::Lambda {
            param,
            body,
            is_whatever_code: false,
            ..
        } => Some(format!("-> ${param} {}", closure_body_source(body)?)),
        crate::ast::Expr::MethodCall {
            target,
            name,
            args,
            modifier: None,
            quoted: false,
            ..
        } => Some(format!(
            "{}.{}({})",
            expression_source(target)?,
            name.resolve(),
            join_args(args)?
        )),
        // Dynamic quoted names must retain their quoted interpolation spelling
        // when a constructed RakuAST regex returns through the established
        // string parser. The matcher continues to resolve the name at match
        // time; this renderer merely reconstructs the bounded source form.
        crate::ast::Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier: None,
            quoted: true,
        } => Some(format!(
            "{}.{}({})",
            expression_source(target)?,
            dynamic_quoted_method_name_source(name_expr)?,
            join_args(args)?
        )),
        // Preserve the ordinary dispatch modifiers of a method call when a
        // constructed RakuAST regex returns through the established string
        // parser.  The matcher continues to evaluate the call at match time;
        // rendering must not turn `.?`, `.+`, or `.*` into an unsupported
        // argument expression.
        crate::ast::Expr::MethodCall {
            target,
            name,
            args,
            modifier: Some(modifier @ ('?' | '+' | '*')),
            quoted: false,
            ..
        } => Some(format!(
            "{}.{}{}({})",
            expression_source(target)?,
            modifier,
            name.resolve(),
            join_args(args)?
        )),
        // A quoted method name deliberately bypasses pseudo-method macros.
        // Keep its quoted spelling when a constructed RakuAST regex returns
        // through the established string parser so the match-time argument
        // evaluator retains that distinction.
        crate::ast::Expr::MethodCall {
            target,
            name,
            args,
            modifier: None,
            quoted: true,
            ..
        } => Some(format!(
            "{}.\"{}\"({})",
            expression_source(target)?,
            name.resolve(),
            join_args(args)?
        )),
        // Argumented subrules retain their expressions structurally.  When a
        // constructed RakuAST tree returns through the existing regex parser,
        // preserve an ordinary subscript rather than rejecting the whole
        // assertion at the source-rendering boundary.
        crate::ast::Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => Some(format!(
            "{}{}{}{}",
            expression_source(target)?,
            if *is_positional { '[' } else { '{' },
            expression_source(index)?,
            if *is_positional { ']' } else { '}' },
        )),
        // A list of plain words is the `<a b c>` word list (a List); any other
        // list keeps the array spelling.
        crate::ast::Expr::ArrayLiteral(items)
            if !items.is_empty()
                && items.iter().all(|item| {
                    matches!(item, crate::ast::Expr::Literal(v)
                        if matches!(v.view(), crate::value::ValueView::Str(w)
                            if !w.is_empty() && !w.contains(|c: char| c.is_whitespace() || matches!(c, '<' | '>'))))
                }) =>
        {
            let words = items.iter().map(|item| match item {
                crate::ast::Expr::Literal(v) => v.to_string_value(),
                _ => String::new(),
            });
            Some(format!("<{}>", words.collect::<Vec<_>>().join(" ")))
        }
        crate::ast::Expr::ArrayLiteral(items) => Some(format!("[{}]", join_args(items)?)),
        crate::ast::Expr::BracketArray(items, _) => Some(format!("[{}]", join_args(items)?)),
        crate::ast::Expr::Hash(pairs, crate::ast::HashSpelling::Composer) => {
            let entries = pairs
                .iter()
                .map(|(key, value)| {
                    let pair = crate::ast::Expr::Binary {
                        left: Box::new(crate::ast::Expr::Literal(
                            crate::value::Value::str(key.clone()),
                        )),
                        op: crate::token_kind::TokenKind::FatArrow,
                        right: Box::new(value.clone()?),
                        form: Default::default(),
                    };
                    expression_source(&pair)
                })
                .collect::<Option<Vec<_>>>()?;
            Some(format!("{{ {} }}", entries.join(", ")))
        }
        crate::ast::Expr::PositionalPair(inner) => Some(format!("({})", expression_source(inner)?)),
        _ => None,
    }
}

/// Render the expression-only statement subset used by an explicit pointy
/// block in a constructed regex argument. Compiler line markers are metadata,
/// not source, and all other statements remain outside this bounded renderer.
fn closure_body_source(body: &[crate::ast::Stmt]) -> Option<String> {
    let mut expressions = Vec::new();
    for stmt in body {
        match stmt {
            crate::ast::Stmt::SetLine(_) => {}
            crate::ast::Stmt::Expr(expr) => expressions.push(expression_source(expr)?),
            crate::ast::Stmt::Assign {
                name,
                expr,
                op: crate::ast::AssignOp::Assign,
                target_is_sigilless: false,
            } if !name.is_empty()
                && name
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-')) =>
            {
                expressions.push(format!("${name} = {}", expression_source(expr)?))
            }
            _ => return None,
        }
    }
    Some(format!("{{ {} }}", expressions.join("; ")))
}

/// Render the interpolated `QuotedString` expression used by a dynamic quoted
/// method name. This intentionally accepts the source subset parser-created
/// regex arguments retain: literal runs and lexical sigil interpolation.
fn dynamic_quoted_method_name_source(expr: &crate::ast::Expr) -> Option<String> {
    let crate::ast::Expr::StringInterpolation(parts) = expr else {
        return None;
    };
    let mut body = String::new();
    for part in parts {
        match part {
            crate::ast::Expr::Literal(value) => match value.view() {
                crate::value::ValueView::Str(text) => {
                    body.push_str(
                        &text
                            .replace('\\', "\\\\")
                            .replace('"', "\\\"")
                            .replace('$', "\\$"),
                    );
                }
                _ => return None,
            },
            crate::ast::Expr::Var(_)
            | crate::ast::Expr::ArrayVar(_)
            | crate::ast::Expr::HashVar(_) => body.push_str(&expression_source(part)?),
            _ => return None,
        }
    }
    Some(format!("\"{body}\""))
}
