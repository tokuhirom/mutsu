use super::super::helpers::{is_ident_char, is_non_breaking_space, split_angle_words, ws};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};
use super::super::primary::{colonpair_expr, parse_block_body, parse_call_arg_list, primary};

use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::expression;
use super::operators::{parse_postfix_update_op, parse_prefix_unary_op};

fn supports_postfix_call_adverbs(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Call { .. }
            | Expr::MethodCall { .. }
            | Expr::CallOn { .. }
            | Expr::HyperMethodCall { .. }
    )
}

fn append_call_arg(expr: &mut Expr, arg: Expr) -> bool {
    match expr {
        Expr::Call { args, .. } => {
            args.push(arg);
            true
        }
        Expr::MethodCall { args, .. } => {
            args.push(arg);
            true
        }
        Expr::CallOn { args, .. } => {
            args.push(arg);
            true
        }
        Expr::HyperMethodCall { args, .. } => {
            args.push(arg);
            true
        }
        _ => false,
    }
}

fn parse_quoted_method_name(input: &str) -> Option<(&str, String)> {
    let (open, close) = if input.starts_with('"') {
        ('"', '"')
    } else if input.starts_with('“') {
        ('“', '”')
    } else {
        return None;
    };
    let mut escaped = false;
    let mut end = None;
    for (i, c) in input[open.len_utf8()..].char_indices() {
        if escaped {
            escaped = false;
            continue;
        }
        if c == '\\' {
            escaped = true;
            continue;
        }
        if c == close {
            end = Some(i);
            break;
        }
    }
    let end = end?;
    let start = open.len_utf8();
    let content = &input[start..start + end];
    let rest = &input[start + end + close.len_utf8()..];
    Some((rest, content.to_string()))
}

fn parse_private_method_name(input: &str) -> Option<(&str, String)> {
    let mut rest = input;
    let mut name = String::new();
    let mut first = true;
    loop {
        let (r, part) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-').ok()?;
        if !first {
            name.push_str("::");
        }
        name.push_str(part);
        first = false;
        rest = r;
        if let Some(r2) = rest.strip_prefix("::") {
            rest = r2;
            continue;
        }
        break;
    }
    Some((rest, name))
}

pub(super) fn prefix_expr(input: &str) -> PResult<'_, Expr> {
    if let Some((op, len)) = parse_prefix_unary_op(input) {
        let mut rest = &input[len..];
        if op.consumes_ws() {
            let (r, _) = ws(rest)?;
            rest = r;
        }
        let (rest, expr) = if op.parses_postfix_target() {
            postfix_expr(rest)?
        } else {
            prefix_expr(rest)?
        };
        return Ok((
            rest,
            Expr::Unary {
                op: op.token_kind(),
                expr: Box::new(expr),
            },
        ));
    }
    // not(expr) — tight-binding form: not followed by ( without space
    if input.starts_with("not(") {
        let r = &input[3..];
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    // so(expr) — tight-binding form: so followed by ( without space
    if input.starts_with("so(") {
        let r = &input[2..];
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            },
        ));
    }
    // lazy prefix (treated as no-op): lazy expr
    if input.starts_with("lazy") && !is_ident_char(input.as_bytes().get(4).copied()) {
        let r = &input[4..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((r, expr));
    }
    // eager prefix (treated as no-op): eager expr
    if input.starts_with("eager") && !is_ident_char(input.as_bytes().get(5).copied()) {
        let r = &input[5..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((r, expr));
    }
    // ^expr — upto operator: ^5 means 0..^5
    // Use postfix_expr_tight so that `^10 .batch(3)` parses as `(^10).batch(3)`,
    // not `^(10.batch(3))`.  Only no-space method calls bind tighter than `^`.
    if input.starts_with('^')
        && !input.starts_with("^..")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$' || c == b'(' || c.is_ascii_digit() || c.is_ascii_alphabetic() || c == b'_')
    {
        let rest = &input[1..];
        let (rest, expr) = postfix_expr_tight(rest)?;
        return postfix_expr_continue(
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Int(0))),
                op: TokenKind::DotDotCaret,
                right: Box::new(expr),
            },
        );
    }
    // |@array or |%hash or |$scalar or |ident — slip/flatten prefix
    if input.starts_with('|')
        && !input.starts_with("||")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'@'
            || c == b'%'
            || c == b'$'
            || c == b'('
            || c == b'['
            || c.is_ascii_alphabetic()
            || c == b'_')
    {
        let rest = &input[1..];
        let (rest, expr) = postfix_expr(rest)?;
        return Ok((
            rest,
            Expr::Unary {
                op: TokenKind::Pipe,
                expr: Box::new(expr),
            },
        ));
    }
    postfix_expr(input)
}

/// Like `postfix_expr` but does NOT allow whitespace-separated dotty method
/// calls.  Used for operands of tight prefix operators like `^` so that
/// `^10 .batch(3)` parses as `(^10).batch(3)` rather than `^(10.batch(3))`.
fn postfix_expr_tight(input: &str) -> PResult<'_, Expr> {
    postfix_expr_inner(input, false)
}

/// Continue applying postfix operations (including whitespace-dotty) to an
/// already-parsed expression.  Used after prefix operators like `^` to allow
/// `^10 .method` to call `.method` on the range result.
fn postfix_expr_continue(input: &str, expr: Expr) -> PResult<'_, Expr> {
    postfix_expr_loop(input, expr, true)
}

/// Postfix: method calls (.method), indexing ([]), ++, --
fn postfix_expr(input: &str) -> PResult<'_, Expr> {
    postfix_expr_inner(input, true)
}

fn postfix_expr_inner(input: &str, allow_ws_dot: bool) -> PResult<'_, Expr> {
    let (rest, expr) = primary(input)?;

    // Type smiley: Any:U, Int:D, Str:D etc.
    // Consume and ignore — runtime doesn't use definedness constraints yet.
    let rest = if matches!(expr, Expr::Literal(Value::Package(_)))
        && (rest.starts_with(":U") || rest.starts_with(":D") || rest.starts_with(":_"))
    {
        let after = &rest[2..];
        if after.is_empty()
            || !after
                .chars()
                .next()
                .is_some_and(|c| c.is_alphanumeric() || c == '_')
        {
            after
        } else {
            rest
        }
    } else {
        rest
    };

    postfix_expr_loop(rest, expr, allow_ws_dot)
}

fn postfix_expr_loop(mut rest: &str, mut expr: Expr, allow_ws_dot: bool) -> PResult<'_, Expr> {
    loop {
        // Allow whitespace before dotty postfix call in expression context:
        // e.g. `^3 .map: { ... }`.
        if allow_ws_dot {
            let (r_ws, _) = ws(rest)?;
            if r_ws.starts_with('.') && !r_ws.starts_with("..") {
                rest = r_ws;
            }
        }

        // Unspace: backslash + whitespace collapses to nothing, allowing
        // `foo\ .method` to parse as `foo.method`.
        if rest.starts_with('\\') {
            let after_bs = &rest[1..];
            if let Some(c) = after_bs.chars().next()
                && c.is_whitespace()
            {
                // Peek ahead: consume unspace only if followed by a postfix operator
                let mut scan = &after_bs[c.len_utf8()..];
                while let Some(c2) = scan.chars().next() {
                    if c2.is_whitespace() {
                        scan = &scan[c2.len_utf8()..];
                    } else {
                        break;
                    }
                }
                if scan.starts_with('.') && !scan.starts_with("..") {
                    rest = scan;
                }
            }
        }
        // Method call: .method or .method(args) or .method: args
        // Also handles modifiers: .?method, .!method
        // Also handles: .^method (meta-method)
        // Also handles call-on: .(args)
        if rest.starts_with('.') && !rest.starts_with("..") {
            let r = &rest[1..];
            // Check for .[index] syntax: object.[expr]
            if let Some(r) = r.strip_prefix('[') {
                let (r, _) = ws(r)?;
                let (r, index) = expression(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ']')?;
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                };
                rest = r;
                continue;
            }
            // Check for call-on syntax: .(args)
            if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                expr = Expr::CallOn {
                    target: Box::new(expr),
                    args,
                };
                rest = r;
                continue;
            }
            // Check for modifier: .?method, .^method, .+method, .*method
            let (r, modifier) = if let Some(stripped) = r.strip_prefix('?') {
                (stripped, Some('?'))
            } else if let Some(stripped) = r.strip_prefix('^') {
                (stripped, Some('^'))
            } else if let Some(stripped) = r.strip_prefix('+') {
                (stripped, Some('+'))
            } else if r.starts_with('*') && !r.starts_with("**") {
                (&r[1..], Some('*'))
            } else {
                (r, None)
            };
            // Parse method name
            if let Ok((r, name)) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                let name = name.to_string();
                // Check for args in parens
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, args) = parse_call_arg_list(r)?;
                    let (r, _) = ws(r)?;
                    let (r, _) = parse_char(r, ')')?;
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                        modifier,
                    };
                    rest = r;
                    continue;
                }
                // Check for colon-arg syntax: .method: arg, arg2
                let (r2, _) = ws(r)?;
                if r2.starts_with(':') && !r2.starts_with("::") {
                    let r3 = &r2[1..];
                    let (r3, _) = ws(r3)?;
                    let (r3, first_arg) = expression(r3)?;
                    let mut args = vec![first_arg];
                    let mut r_inner = r3;
                    loop {
                        let (r4, _) = ws(r_inner)?;
                        if !r4.starts_with(',') {
                            break;
                        }
                        let r4 = &r4[1..];
                        let (r4, _) = ws(r4)?;
                        let (r4, next) = expression(r4)?;
                        args.push(next);
                        r_inner = r4;
                    }
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                        modifier,
                    };
                    rest = r_inner;
                    continue;
                }
                // No-arg method call
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier,
                };
                rest = r;
                continue;
            }
            // Dynamic method call with quoted method name: ."$name"()
            // TODO: Evaluate interpolated method-name expressions dynamically.
            // For now we keep parsed source text as method name to unblock parsing.
            if let Some((r, name)) = parse_quoted_method_name(r) {
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, args) = parse_call_arg_list(r)?;
                    let (r, _) = ws(r)?;
                    let (r, _) = parse_char(r, ')')?;
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                        modifier,
                    };
                    rest = r;
                    continue;
                }
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier,
                };
                rest = r;
                continue;
            }
            return Err(PError::expected_at("method name", r));
        }

        // Private method call: target!Type::method(args) / target!method(args)
        if rest.starts_with('!') && !rest.starts_with("!=") {
            let after_bang = &rest[1..];
            if let Some((r, name)) = parse_private_method_name(after_bang) {
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, args) = parse_call_arg_list(r)?;
                    let (r, _) = ws(r)?;
                    let (r, _) = parse_char(r, ')')?;
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                        modifier: Some('!'),
                    };
                    rest = r;
                    continue;
                }
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier: Some('!'),
                };
                rest = r;
                continue;
            }
        }

        // CallOn: $var(args) — invoke a callable stored in a variable
        if rest.starts_with('(')
            && matches!(
                &expr,
                Expr::Var(_)
                    | Expr::CodeVar(_)
                    | Expr::IndirectCodeLookup { .. }
                    | Expr::MethodCall { .. }
                    | Expr::AnonSub(_)
                    | Expr::AnonSubParams { .. }
                    | Expr::Lambda { .. }
                    | Expr::Index { .. }
                    | Expr::CallOn { .. }
            )
        {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            expr = Expr::CallOn {
                target: Box::new(expr),
                args,
            };
            rest = r;
            continue;
        }

        // Array indexing: [expr] or zen slice []
        if rest.starts_with('[') {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            // Zen slice: expr[] — returns expr unchanged (identity on lists)
            if let Some(after) = r.strip_prefix(']') {
                rest = after;
                continue;
            }
            let (r, index) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(index),
            };
            rest = r;
            continue;
        }

        // Hash indexing with angle brackets: %hash<key>, $hash<key>, @a[0]<key>, etc.
        // Only match when content is a simple word key (alphanumeric, no operators)
        if rest.starts_with('<')
            && !rest.starts_with("<=")
            && !rest.starts_with("<<")
            && !rest.starts_with("<=>")
        {
            let r = &rest[1..];
            let Some(end) = r.find('>') else {
                return Err(PError::expected_at("closing '>'", r));
            };
            let content = &r[..end];
            let keys = split_angle_words(content);
            let is_zen_angle = keys.is_empty();
            if !is_zen_angle
                && keys.iter().any(|key| {
                    key.is_empty()
                        || !key.chars().all(|c| {
                            c.is_alphanumeric()
                                || c == '_'
                                || c == '-'
                                || c == '!'
                                || c == '.'
                                || c == ':'
                                || c == '?'
                                || c == '+'
                                || c == '/'
                                || is_non_breaking_space(c)
                        })
                })
            {
                return Err(PError::expected_at("angle index key", r));
            }
            let r = &r[end + 1..];
            let index_expr = if keys.len() == 1 {
                Expr::Literal(Value::Str(keys[0].to_string()))
            } else {
                Expr::ArrayLiteral(
                    keys.into_iter()
                        .map(|k| Expr::Literal(Value::Str(k.to_string())))
                        .collect(),
                )
            };
            let indexed_expr = if is_zen_angle {
                expr.clone()
            } else {
                Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index_expr),
                }
            };
            if r.starts_with(":v") && !is_ident_char(r.as_bytes().get(2).copied()) {
                let r = &r[2..];
                expr = if is_zen_angle {
                    Expr::MethodCall {
                        target: Box::new(indexed_expr),
                        name: "values".to_string(),
                        args: Vec::new(),
                        modifier: None,
                    }
                } else {
                    indexed_expr
                };
                rest = r;
                continue;
            }
            // Check for :exists / :!exists / :delete adverbs
            if r.starts_with(":exists") && !is_ident_char(r.as_bytes().get(7).copied()) {
                let r = &r[7..];
                expr = Expr::Exists(Box::new(indexed_expr));
                rest = r;
                continue;
            }
            if r.starts_with(":!exists") && !is_ident_char(r.as_bytes().get(8).copied()) {
                let r = &r[8..];
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Exists(Box::new(indexed_expr))),
                };
                rest = r;
                continue;
            }
            expr = indexed_expr;
            rest = r;
            continue;
        }

        // Hash indexing with braces: %hash{"key"}, %hash{$var}, @a[0]{"key"}, etc.
        if rest.starts_with('{')
            && matches!(
                &expr,
                Expr::HashVar(_)
                    | Expr::Var(_)
                    | Expr::Index { .. }
                    | Expr::MethodCall { .. }
                    | Expr::Call { .. }
            )
        {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, index) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, '}')?;
            // Allow whitespace before adverbs
            let (r_adv, _) = ws(r)?;
            // Check for :exists / :!exists / :delete adverbs on curly-brace subscript
            if r_adv.starts_with(":exists") && !is_ident_char(r_adv.as_bytes().get(7).copied()) {
                let r = &r_adv[7..];
                expr = Expr::Exists(Box::new(Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                }));
                rest = r;
                continue;
            }
            if r_adv.starts_with(":!exists") && !is_ident_char(r_adv.as_bytes().get(8).copied()) {
                let r = &r_adv[8..];
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Exists(Box::new(Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(index),
                    }))),
                };
                rest = r;
                continue;
            }
            if r_adv.starts_with(":delete") && !is_ident_char(r_adv.as_bytes().get(7).copied()) {
                let r = &r_adv[7..];
                // Use MethodCall as a proxy for delete operation
                expr = Expr::MethodCall {
                    target: Box::new(Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(index),
                    }),
                    name: "DELETE-KEY".to_string(),
                    args: vec![],
                    modifier: None,
                };
                rest = r;
                continue;
            }
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(index),
            };
            rest = r;
            continue;
        }

        // Adverbs on subscript expressions: :exists / :!exists / :delete
        // These can appear with whitespace after ] or } subscripts
        if matches!(&expr, Expr::Index { .. }) {
            let (r_adv2, _) = ws(rest)?;
            if r_adv2.starts_with(":exists") && !is_ident_char(r_adv2.as_bytes().get(7).copied()) {
                rest = &r_adv2[7..];
                expr = Expr::Exists(Box::new(expr));
                continue;
            }
            if r_adv2.starts_with(":!exists") && !is_ident_char(r_adv2.as_bytes().get(8).copied()) {
                rest = &r_adv2[8..];
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Exists(Box::new(expr))),
                };
                continue;
            }
        }

        // Hyper method call: »/>> followed by .method or method name
        // Also hyper postfix: » followed by superscript digits (e.g. »²)
        if rest.starts_with('\u{00BB}') || rest.starts_with(">>") {
            let after_hyper = if rest.starts_with('\u{00BB}') {
                &rest['\u{00BB}'.len_utf8()..]
            } else {
                &rest[2..]
            };
            // Check for hyper + superscript exponent: »²
            if let Some((exp, len)) = parse_superscript_exp(after_hyper) {
                rest = &after_hyper[len..];
                expr = Expr::HyperOp {
                    op: "**".to_string(),
                    left: Box::new(expr),
                    right: Box::new(Expr::Literal(Value::Int(exp))),
                    dwim_left: false,
                    dwim_right: true,
                };
                continue;
            }
            // Check for ».method or >>.method
            let r = after_hyper.strip_prefix('.').unwrap_or(after_hyper);
            if let Ok((r, name)) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                let name = name.to_string();
                // Check for args in parens
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, args) = parse_call_arg_list(r)?;
                    let (r, _) = ws(r)?;
                    let (r, _) = parse_char(r, ')')?;
                    expr = Expr::HyperMethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                    };
                    rest = r;
                    continue;
                }
                // No-arg hyper method call
                expr = Expr::HyperMethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                };
                rest = r;
                continue;
            }
        }

        // Postfix ++ and --
        if let Some((op, len)) = parse_postfix_update_op(rest) {
            rest = &rest[len..];
            expr = Expr::PostfixOp {
                op: op.token_kind(),
                expr: Box::new(expr),
            };
            continue;
        }

        // Custom postfix operator call: $x! -> postfix:<!>($x)
        if rest.starts_with('!') && !rest.starts_with("!=") {
            let after = &rest[1..];
            // Keep `!` as postfix only at expression boundary.
            if after.is_empty()
                || after.starts_with(|c: char| {
                    c.is_whitespace() || c == ')' || c == '}' || c == ']' || c == ',' || c == ';'
                })
            {
                expr = Expr::Call {
                    name: "postfix:<!>".to_string(),
                    args: vec![expr],
                };
                rest = after;
                continue;
            }
        }

        // Postfix call adverbs: call():k, call():x(42), call():{ ... }
        // Parse as additional arguments on the preceding call/method-call expression.
        if supports_postfix_call_adverbs(&expr) {
            let (r_adv, _) = ws(rest)?;
            if r_adv.starts_with(':') && !r_adv.starts_with("::") {
                let after_colon = &r_adv[1..];
                let (after_colon, _) = ws(after_colon)?;
                if after_colon.starts_with('{') {
                    let (r_next, body) = parse_block_body(after_colon)?;
                    if append_call_arg(&mut expr, Expr::AnonSub(body)) {
                        rest = r_next;
                        continue;
                    }
                } else if let Ok((r_next, adverb_expr)) = colonpair_expr(r_adv)
                    && append_call_arg(&mut expr, adverb_expr)
                {
                    rest = r_next;
                    continue;
                }
            }
        }

        // Postfix i (imaginary number): (expr)i → Complex(0, expr)
        if rest.starts_with('i') && !is_ident_char(rest.as_bytes().get(1).copied()) {
            rest = &rest[1..];
            expr = Expr::MethodCall {
                target: Box::new(expr),
                name: "i".to_string(),
                args: vec![],
                modifier: None,
            };
            continue;
        }

        // Superscript digits as postfix exponentiation: x² → x ** 2, x³ → x ** 3
        if let Some((exp, len)) = parse_superscript_exp(rest) {
            rest = &rest[len..];
            expr = Expr::Binary {
                left: Box::new(expr),
                op: crate::token_kind::TokenKind::StarStar,
                right: Box::new(Expr::Literal(Value::Int(exp))),
            };
            continue;
        }

        break;
    }

    Ok((rest, expr))
}

/// Parse superscript digits as an exponent. Returns (exponent_value, byte_length).
fn parse_superscript_exp(input: &str) -> Option<(i64, usize)> {
    fn superscript_digit(c: char) -> Option<i64> {
        match c {
            '\u{2070}' => Some(0), // ⁰
            '\u{00B9}' => Some(1), // ¹
            '\u{00B2}' => Some(2), // ²
            '\u{00B3}' => Some(3), // ³
            '\u{2074}' => Some(4), // ⁴
            '\u{2075}' => Some(5), // ⁵
            '\u{2076}' => Some(6), // ⁶
            '\u{2077}' => Some(7), // ⁷
            '\u{2078}' => Some(8), // ⁸
            '\u{2079}' => Some(9), // ⁹
            _ => None,
        }
    }
    let mut chars = input.chars();
    let first = chars.next()?;
    let mut value = superscript_digit(first)?;
    let mut len = first.len_utf8();
    for c in chars {
        if let Some(d) = superscript_digit(c) {
            value = value * 10 + d;
            len += c.len_utf8();
        } else {
            break;
        }
    }
    Some((value, len))
}
