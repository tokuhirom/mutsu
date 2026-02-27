use super::super::helpers::{
    consume_unspace, is_ident_char, is_non_breaking_space, split_angle_words, ws,
};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};
use super::super::primary::{colonpair_expr, parse_block_body, parse_call_arg_list, primary};

use crate::ast::{ExistsAdverb, Expr, HyperSliceAdverb};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::expression;
use super::operators::{parse_postfix_update_op, parse_prefix_unary_op};

/// Try to parse a secondary adverb after :exists/:!exists.
/// Returns (remaining_input, adverb).
fn parse_exists_secondary_adverb(input: &str) -> (&str, ExistsAdverb) {
    if input.starts_with(":!kv") && !is_ident_char(input.as_bytes().get(4).copied()) {
        return (&input[4..], ExistsAdverb::NotKv);
    }
    if input.starts_with(":kv") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::Kv);
    }
    if input.starts_with(":!p") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::NotP);
    }
    if input.starts_with(":p") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return (&input[2..], ExistsAdverb::P);
    }
    if input.starts_with(":!v") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::NotV);
    }
    if input.starts_with(":!k") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::InvalidNotK);
    }
    if input.starts_with(":k") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return (&input[2..], ExistsAdverb::InvalidK);
    }
    if input.starts_with(":v") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return (&input[2..], ExistsAdverb::InvalidV);
    }
    (input, ExistsAdverb::None)
}

/// Try to parse :exists or :!exists adverb on a subscript expression.
/// Returns (remaining_input, exists_expr) or None if no adverb found.
fn try_parse_exists_adverb(input: &str, target: Expr) -> Option<(&str, Expr)> {
    let r = input;
    let (r, negated) = if r.starts_with(":!exists") && !is_ident_char(r.as_bytes().get(8).copied())
    {
        (&r[8..], true)
    } else if r.starts_with(":exists") && !is_ident_char(r.as_bytes().get(7).copied()) {
        (&r[7..], false)
    } else {
        return None;
    };
    // Check for parameterized argument: :exists(expr)
    let (r, arg) = if let Some(r_stripped) = r.strip_prefix('(') {
        let r2 = r_stripped;
        if let Ok((r2, _)) = ws(r2) {
            if let Ok((r2, arg_expr)) = expression(r2) {
                if let Ok((r2, _)) = ws(r2) {
                    if let Ok((r2, _)) = parse_char(r2, ')') {
                        (r2, Some(Box::new(arg_expr)))
                    } else {
                        (r, None)
                    }
                } else {
                    (r, None)
                }
            } else {
                (r, None)
            }
        } else {
            (r, None)
        }
    } else {
        (r, None)
    };
    // Check for secondary adverb
    let (r, adverb) = parse_exists_secondary_adverb(r);
    Some((
        r,
        Expr::Exists {
            target: Box::new(target),
            negated,
            arg,
            adverb,
        },
    ))
}

fn supports_postfix_call_adverbs(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Call { .. }
            | Expr::MethodCall { .. }
            | Expr::CallOn { .. }
            | Expr::HyperMethodCall { .. }
            | Expr::HyperMethodCallDynamic { .. }
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
        Expr::HyperMethodCallDynamic { args, .. } => {
            args.push(arg);
            true
        }
        _ => false,
    }
}

fn parse_bracket_indices(input: &str) -> PResult<'_, Expr> {
    let (r, first) = expression(input)?;
    let mut indices = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if r2.starts_with(',') || (r2.starts_with(';') && !r2.starts_with(";;")) {
            let sep = if r2.starts_with(',') { ',' } else { ';' };
            let (r3, _) = parse_char(r2, sep)?;
            let (r3, _) = ws(r3)?;
            let (r3, next) = expression(r3)?;
            indices.push(next);
            r = r3;
            continue;
        }
        return Ok((
            r2,
            if indices.len() == 1 {
                indices.remove(0)
            } else {
                Expr::ArrayLiteral(indices)
            },
        ));
    }
}

/// Result of parsing a quoted method name.
enum QuotedMethodName {
    /// Static method name (single-quoted or double-quoted without interpolation)
    Static(String),
    /// Dynamic method name (double-quoted with interpolation)
    Dynamic(Expr),
}

fn parse_quoted_method_name(input: &str) -> Option<(&str, QuotedMethodName)> {
    if input.starts_with('\'') {
        // Single-quoted: no interpolation
        let start = 1;
        let mut end = None;
        for (i, c) in input[start..].char_indices() {
            if c == '\'' {
                end = Some(i);
                break;
            }
        }
        let end = end?;
        let content = &input[start..start + end];
        let rest = &input[start + end + 1..];
        return Some((rest, QuotedMethodName::Static(content.to_string())));
    }
    if input.starts_with('"') {
        // Double-quoted: may have interpolation
        let start = 1;
        let mut escaped = false;
        let mut end = None;
        for (i, c) in input[start..].char_indices() {
            if escaped {
                escaped = false;
                continue;
            }
            if c == '\\' {
                escaped = true;
                continue;
            }
            if c == '"' {
                end = Some(i);
                break;
            }
        }
        let end = end?;
        let content = &input[start..start + end];
        let rest = &input[start + end + 1..];
        // Check if content has interpolation
        if content.contains('$') || content.contains('@') || content.contains('{') {
            use super::super::primary::string::interpolate_string_content;
            let name_expr = interpolate_string_content(content);
            return Some((rest, QuotedMethodName::Dynamic(name_expr)));
        }
        return Some((rest, QuotedMethodName::Static(content.to_string())));
    }
    None
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

fn is_postfix_operator_char(c: char) -> bool {
    if c.is_whitespace() || c.is_alphanumeric() || c == '_' {
        return false;
    }
    !matches!(
        c,
        '.' | ','
            | ';'
            | ':'
            | '('
            | ')'
            | '['
            | ']'
            | '{'
            | '}'
            | '"'
            | '\''
            | '\\'
            | '$'
            | '@'
            | '%'
            | '&'
            | '#'
    )
}

fn is_postfix_operator_boundary(rest: &str) -> bool {
    rest.is_empty()
        || rest.starts_with(|c: char| {
            c.is_whitespace() || matches!(c, ')' | '}' | ']' | ',' | ';' | ':')
        })
}

fn has_ternary_else_after(input: &str) -> bool {
    let mut rest = input;
    while let Ok((next, _)) = ws(rest) {
        if next.len() == rest.len() {
            break;
        }
        rest = next;
    }
    rest.starts_with("!!")
}

fn parse_custom_postfix_operator(input: &str) -> Option<(String, usize)> {
    // Don't consume characters that are closing delimiters of circumfix operators
    if crate::parser::stmt::simple::is_circumfix_close_delimiter(input) {
        return None;
    }

    if input.starts_with('!') {
        return Some(("!".to_string(), '!'.len_utf8()));
    }

    let mut chars = input.chars();
    let first = chars.next()?;
    if first.is_ascii() || !is_postfix_operator_char(first) {
        return None;
    }

    let mut len = first.len_utf8();
    for c in chars {
        if c.is_ascii() || !is_postfix_operator_char(c) {
            break;
        }
        len += c.len_utf8();
    }
    Some((input[..len].to_string(), len))
}

fn atomic_var_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(name) => Some(name.clone()),
        _ => None,
    }
}

pub(super) fn prefix_expr(input: &str) -> PResult<'_, Expr> {
    if let Some(rest) = input.strip_prefix("++⚛") {
        let (rest, expr) = postfix_expr(rest)?;
        if let Some(name) = atomic_var_name(&expr) {
            return Ok((
                rest,
                Expr::Call {
                    name: "__mutsu_atomic_pre_inc_var".to_string(),
                    args: vec![Expr::Literal(Value::Str(name))],
                },
            ));
        }
        return Ok((
            rest,
            Expr::Unary {
                op: TokenKind::PlusPlus,
                expr: Box::new(expr),
            },
        ));
    }
    if let Some(rest) = input.strip_prefix("--⚛") {
        let (rest, expr) = postfix_expr(rest)?;
        if let Some(name) = atomic_var_name(&expr) {
            return Ok((
                rest,
                Expr::Call {
                    name: "__mutsu_atomic_pre_dec_var".to_string(),
                    args: vec![Expr::Literal(Value::Str(name))],
                },
            ));
        }
        return Ok((
            rest,
            Expr::Unary {
                op: TokenKind::MinusMinus,
                expr: Box::new(expr),
            },
        ));
    }
    if let Some(rest) = input.strip_prefix('⚛') {
        let (rest, expr) = postfix_expr(rest)?;
        if let Some(name) = atomic_var_name(&expr) {
            return Ok((
                rest,
                Expr::Call {
                    name: "__mutsu_atomic_fetch_var".to_string(),
                    args: vec![Expr::Literal(Value::Str(name))],
                },
            ));
        }
        return Ok((rest, expr));
    }

    if let Some(after_open) = input.strip_prefix('(')
        && let Some(end) = after_open.find(')')
    {
        let raw_op = &after_open[..end];
        let op = raw_op.trim();
        let after = &after_open[end + 1..];
        if !op.is_empty()
            && op == raw_op
            && op
                .chars()
                .all(|c| !c.is_whitespace() && !c.is_alphanumeric() && c != '_' && c != '\'')
            && !op
                .chars()
                .any(|c| matches!(c, '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>'))
            && after.chars().next().is_some_and(char::is_whitespace)
        {
            let (after, _) = ws(after)?;
            let (after, arg) = prefix_expr(after)?;
            return Ok((
                after,
                Expr::Call {
                    name: format!("prefix:<({})>", op),
                    args: vec![arg],
                },
            ));
        }
    }

    if let Some((name, len)) = crate::parser::stmt::simple::match_user_declared_prefix_op(input) {
        let rest = &input[len..];
        let (rest, _) = ws(rest)?;
        let (rest, arg) = prefix_expr(rest)?;
        return Ok((
            rest,
            Expr::Call {
                name,
                args: vec![arg],
            },
        ));
    }

    // Hyper prefix metaop: -« expr / -<< expr / +« expr / ?« expr ...
    if let Some((op, len)) = parse_prefix_unary_op(input) {
        let after_op = &input[len..];
        let marker_len = if after_op.starts_with('\u{00AB}') {
            Some('\u{00AB}'.len_utf8())
        } else if after_op.starts_with("<<") {
            Some(2)
        } else {
            None
        };
        if let Some(marker_len) = marker_len
            && let Some(symbol) = op.prefix_symbol()
        {
            let rest = &after_op[marker_len..];
            let (rest, _) = ws(rest)?;
            let (rest, arg) = prefix_expr(rest)?;
            return Ok((
                rest,
                Expr::Call {
                    name: "__mutsu_hyper_prefix".to_string(),
                    args: vec![Expr::Literal(Value::Str(symbol.to_string())), arg],
                },
            ));
        }
    }

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
            || c == b'.'
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
pub(in crate::parser) fn postfix_expr_continue(input: &str, expr: Expr) -> PResult<'_, Expr> {
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
        // `foo\ .method` to parse as `foo.method` and
        // `foo\ (args)` to parse as `foo(args)`.
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
                if (scan.starts_with('.') && !scan.starts_with(".."))
                    || scan.starts_with('(')
                    || scan.starts_with('[')
                {
                    rest = scan;
                }
            }
        }
        if rest.starts_with("->") {
            return Err(PError::fatal(
                "X::Obsolete: Perl -> is dead. Please use '.' instead.".to_string(),
            ));
        }
        // Method call: .method or .method(args) or .method: args
        // Also handles modifiers: .?method, .!method
        // Also handles: .^method (meta-method)
        // Also handles call-on: .(args)
        if rest.starts_with('.') && !rest.starts_with("..") {
            let r = &rest[1..];
            // `.=` is handled by statement-level assignment parsing, not postfix
            // method-call parsing. Stop postfix parsing and let the caller consume it.
            if r.starts_with('=') {
                break;
            }
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
            // Check for .<key> angle-bracket hash access: %h.<foo>, $obj.<bar>
            if r.starts_with('<')
                && !r.starts_with("<=")
                && !r.starts_with("<<")
                && !r.starts_with("<=>")
            {
                let r2 = &r[1..];
                if let Some(end) = r2.find('>') {
                    let content = &r2[..end];
                    let keys = split_angle_words(content);
                    if !keys.is_empty()
                        && keys.iter().all(|key| {
                            !key.is_empty()
                                && key.chars().all(|c| {
                                    c.is_alphanumeric()
                                        || c == '_'
                                        || c == '-'
                                        || c == '!'
                                        || c == '.'
                                        || c == ':'
                                        || c == '?'
                                        || c == '+'
                                        || c == '/'
                                        || c == '$'
                                        || c == '@'
                                        || c == '%'
                                        || c == '&'
                                })
                        })
                    {
                        let r2 = &r2[end + 1..];
                        let index_expr = if keys.len() == 1 {
                            Expr::Literal(Value::Str(keys[0].to_string()))
                        } else {
                            Expr::ArrayLiteral(
                                keys.into_iter()
                                    .map(|k| Expr::Literal(Value::Str(k.to_string())))
                                    .collect(),
                            )
                        };
                        expr = Expr::Index {
                            target: Box::new(expr),
                            index: Box::new(index_expr),
                        };
                        rest = r2;
                        continue;
                    }
                }
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
            // Dot postfix update shorthand: .++ / .--
            // Equivalent to applying postfix update to the current expression.
            if let Some((op, len)) = parse_postfix_update_op(r) {
                expr = Expr::PostfixOp {
                    op: op.token_kind(),
                    expr: Box::new(expr),
                };
                rest = &r[len..];
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
                // Detect illegal space between method name and parens
                if (r.starts_with(' ') || r.starts_with('\t')) && !r.starts_with('\\') {
                    let after_ws = r.trim_start_matches([' ', '\t']);
                    if after_ws.starts_with('(') {
                        return Err(PError::expected_at(
                            "Confused. no space allowed between method name and the left parenthesis",
                            r,
                        ));
                    }
                }
                // Handle unspace between method name and parens: .method\ (args)
                let r = consume_unspace(r);
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
                        quoted: false,
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
                        quoted: false,
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
                    quoted: false,
                };
                rest = r;
                continue;
            }
            // Quoted method name: .'method'() or ."$name"()
            // Quoted method names require parenthesized arguments.
            if let Some((r, qname)) = parse_quoted_method_name(r) {
                if !r.starts_with('(') {
                    return Err(PError::expected_at(
                        "parenthesized arguments after quoted method name",
                        r,
                    ));
                }
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                match qname {
                    QuotedMethodName::Static(name) => {
                        expr = Expr::MethodCall {
                            target: Box::new(expr),
                            name,
                            args,
                            modifier,
                            quoted: true,
                        };
                    }
                    QuotedMethodName::Dynamic(name_expr) => {
                        expr = Expr::DynamicMethodCall {
                            target: Box::new(expr),
                            name_expr: Box::new(name_expr),
                            args,
                        };
                    }
                }
                rest = r;
                continue;
            }
            // Dynamic method name via sigiled expression: .$meth or .$meth(...)
            if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') || r.starts_with('&')
            {
                if r.starts_with("$(") || r.starts_with("&(") {
                    let r_name = &r[2..];
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, name_expr) = expression(r_name)?;
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, _) = parse_char(r_name, ')')?;
                    let (r_name, _) = ws(r_name)?;
                    if r_name.starts_with('(') {
                        let (r_name, _) = parse_char(r_name, '(')?;
                        let (r_name, _) = ws(r_name)?;
                        let (r_name, args) = parse_call_arg_list(r_name)?;
                        let (r_name, _) = ws(r_name)?;
                        let (r_name, _) = parse_char(r_name, ')')?;
                        expr = Expr::DynamicMethodCall {
                            target: Box::new(expr),
                            name_expr: Box::new(name_expr),
                            args,
                        };
                        rest = r_name;
                        continue;
                    }
                    expr = Expr::DynamicMethodCall {
                        target: Box::new(expr),
                        name_expr: Box::new(name_expr),
                        args: Vec::new(),
                    };
                    rest = r_name;
                    continue;
                }
                let (r_name, name_expr) = super::super::primary::primary(r)?;
                let (r_name, _) = ws(r_name)?;
                if r_name.starts_with('(') {
                    let (r_name, _) = parse_char(r_name, '(')?;
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, args) = parse_call_arg_list(r_name)?;
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, _) = parse_char(r_name, ')')?;
                    expr = Expr::DynamicMethodCall {
                        target: Box::new(expr),
                        name_expr: Box::new(name_expr),
                        args,
                    };
                    rest = r_name;
                    continue;
                }
                expr = Expr::DynamicMethodCall {
                    target: Box::new(expr),
                    name_expr: Box::new(name_expr),
                    args: Vec::new(),
                };
                rest = r_name;
                continue;
            }
            if let Some((op, len)) = parse_custom_postfix_operator(r) {
                let after = &r[len..];
                if is_postfix_operator_boundary(after) {
                    expr = Expr::Call {
                        name: format!("postfix:<{op}>"),
                        args: vec![expr],
                    };
                    rest = after;
                    continue;
                }
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
                        quoted: false,
                    };
                    rest = r;
                    continue;
                }
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier: Some('!'),
                    quoted: false,
                };
                rest = r;
                continue;
            }
        }

        // CallOn: expr(args) — invoke any callable expression.
        if rest.starts_with('(') {
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
            // But check for :exists adverb first since @a[]:exists needs ZenSlice
            if let Some(after) = r.strip_prefix(']') {
                let (r_adv, _) = ws(after)?;
                if r_adv.starts_with(":exists") || r_adv.starts_with(":!exists") {
                    // Will be handled by adverb check below
                    expr = Expr::ZenSlice(Box::new(expr));
                    rest = after;
                    continue;
                }
                rest = after;
                continue;
            }
            let (r, index) = parse_bracket_indices(r)?;
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
                                || c == '$'
                                || c == '@'
                                || c == '%'
                                || c == '&'
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
                        quoted: false,
                    }
                } else {
                    indexed_expr
                };
                rest = r;
                continue;
            }
            // Check for :exists / :!exists / :delete adverbs
            if let Some((r_after, exists_expr)) = try_parse_exists_adverb(r, indexed_expr.clone()) {
                expr = exists_expr;
                rest = r_after;
                continue;
            }
            expr = indexed_expr;
            rest = r;
            continue;
        }

        // Object constructor shorthand: Type{ :named(...) } / Type{ key => value, ... }.
        // Treat this as Type.new(...) for package/type barewords.
        if rest.starts_with('{') && matches!(&expr, Expr::BareWord(_)) {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, args) = if r.starts_with('}') {
                (r, Vec::new())
            } else {
                parse_call_arg_list(r)?
            };
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, '}')?;
            expr = Expr::MethodCall {
                target: Box::new(expr),
                name: "new".to_string(),
                args,
                modifier: None,
                quoted: false,
            };
            rest = r;
            continue;
        }

        // Hash hyperslice: %hash{**}:adverb
        if rest.starts_with("{**}")
            && matches!(&expr, Expr::HashVar(_) | Expr::Var(_) | Expr::Index { .. })
        {
            let r = &rest[4..];
            let (r_adv, _) = ws(r)?;
            // Parse required adverb
            let (adverb, r) = if r_adv.starts_with(":deepkv")
                && !is_ident_char(r_adv.as_bytes().get(7).copied())
            {
                (HyperSliceAdverb::DeepKv, &r_adv[7..])
            } else if r_adv.starts_with(":deepk")
                && !is_ident_char(r_adv.as_bytes().get(6).copied())
            {
                (HyperSliceAdverb::DeepK, &r_adv[6..])
            } else if r_adv.starts_with(":tree") && !is_ident_char(r_adv.as_bytes().get(5).copied())
            {
                (HyperSliceAdverb::Tree, &r_adv[5..])
            } else if r_adv.starts_with(":kv") && !is_ident_char(r_adv.as_bytes().get(3).copied()) {
                (HyperSliceAdverb::Kv, &r_adv[3..])
            } else if r_adv.starts_with(":k") && !is_ident_char(r_adv.as_bytes().get(2).copied()) {
                (HyperSliceAdverb::K, &r_adv[2..])
            } else if r_adv.starts_with(":v") && !is_ident_char(r_adv.as_bytes().get(2).copied()) {
                (HyperSliceAdverb::V, &r_adv[2..])
            } else {
                // Default to :kv if no adverb
                (HyperSliceAdverb::Kv, r)
            };
            expr = Expr::HyperSlice {
                target: Box::new(expr),
                adverb,
            };
            rest = r;
            continue;
        }

        // Hash hyperindex: %hash{||@keys}
        if rest.starts_with("{||")
            && matches!(&expr, Expr::HashVar(_) | Expr::Var(_) | Expr::Index { .. })
        {
            let r = &rest[3..];
            let (r, _) = ws(r)?;
            let (r, keys_expr) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, '}')?;
            expr = Expr::HyperIndex {
                target: Box::new(expr),
                keys: Box::new(keys_expr),
            };
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
            let (r, index) = parse_bracket_indices(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, '}')?;
            // Allow whitespace before adverbs
            let (r_adv, _) = ws(r)?;
            // Check for :exists / :!exists / :delete adverbs on curly-brace subscript
            let indexed = Expr::Index {
                target: Box::new(expr.clone()),
                index: Box::new(index.clone()),
            };
            if let Some((r_after, exists_expr)) = try_parse_exists_adverb(r_adv, indexed) {
                expr = exists_expr;
                rest = r_after;
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
                    quoted: false,
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
        if matches!(&expr, Expr::Index { .. } | Expr::ZenSlice(_)) {
            let (r_adv2, _) = ws(rest)?;
            if r_adv2.starts_with(":v")
                && !is_ident_char(r_adv2.as_bytes().get(2).copied())
                && !has_ternary_else_after(&r_adv2[2..])
            {
                rest = &r_adv2[2..];
                continue;
            }
            if let Some((r_after, exists_expr)) = try_parse_exists_adverb(r_adv2, expr.clone()) {
                expr = exists_expr;
                rest = r_after;
                continue;
            }
            if r_adv2.starts_with(":delete") && !is_ident_char(r_adv2.as_bytes().get(7).copied()) {
                rest = &r_adv2[7..];
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name: "DELETE-KEY".to_string(),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                };
                continue;
            }
        }

        // General :delete adverb. Index targets are lowered to dedicated delete
        // opcodes by the compiler; non-index targets die at runtime.
        let (r_delete, _) = ws(rest)?;
        if r_delete.starts_with(":delete") && !is_ident_char(r_delete.as_bytes().get(7).copied()) {
            rest = &r_delete[7..];
            expr = Expr::MethodCall {
                target: Box::new(expr),
                name: "DELETE-KEY".to_string(),
                args: vec![],
                modifier: None,
                quoted: false,
            };
            continue;
        }

        // Hyper method call: »/>> followed by .method or method name
        // Also hyper postfix: » followed by superscript digits (e.g. »²)
        if rest.starts_with('\u{00BB}') || rest.starts_with(">>") {
            let after_hyper = if rest.starts_with('\u{00BB}') {
                &rest['\u{00BB}'.len_utf8()..]
            } else {
                &rest[2..]
            };
            // Hyper `.=` assignment is handled by statement-level assignment parsing.
            if after_hyper.starts_with(".=") {
                break;
            }
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
            // Hyper indexing: expr»[idx] (or expr>>[idx]) => expr».AT-POS(idx)
            if let Some(r) = after_hyper.strip_prefix('[') {
                let (r, _) = ws(r)?;
                if let Some(after) = r.strip_prefix(']') {
                    rest = after;
                    continue;
                }
                let (r, index) = parse_bracket_indices(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ']')?;
                expr = Expr::HyperMethodCall {
                    target: Box::new(expr),
                    name: "AT-POS".to_string(),
                    args: vec![index],
                    modifier: None,
                    quoted: false,
                };
                rest = r;
                continue;
            }
            // Check for ».method / >>.method and modifier forms like ».?method, ».+method, »!Type::meth.
            let r = after_hyper.strip_prefix('.').unwrap_or(after_hyper);
            let (r, modifier) = if let Some(stripped) = r.strip_prefix('?') {
                (stripped, Some('?'))
            } else if let Some(stripped) = r.strip_prefix('^') {
                (stripped, Some('^'))
            } else if let Some(stripped) = r.strip_prefix('+') {
                (stripped, Some('+'))
            } else if r.starts_with('*') && !r.starts_with("**") {
                (&r[1..], Some('*'))
            } else if let Some(stripped) = r.strip_prefix('!') {
                (stripped, Some('!'))
            } else {
                (r, None)
            };

            // Hyper postcircumfix key lookup: >>.<a>
            if r.starts_with('<')
                && !r.starts_with("<=")
                && !r.starts_with("<<")
                && !r.starts_with("<=>")
            {
                let r2 = &r[1..];
                if let Some(end) = r2.find('>') {
                    let content = &r2[..end];
                    let keys = split_angle_words(content);
                    if !keys.is_empty()
                        && keys.iter().all(|key| {
                            !key.is_empty()
                                && key.chars().all(|c| {
                                    c.is_alphanumeric() || c == '_' || c == '-' || c == ':'
                                })
                        })
                    {
                        let args = if keys.len() == 1 {
                            vec![Expr::Literal(Value::Str(keys[0].to_string()))]
                        } else {
                            vec![Expr::ArrayLiteral(
                                keys.into_iter()
                                    .map(|k| Expr::Literal(Value::Str(k.to_string())))
                                    .collect(),
                            )]
                        };
                        expr = Expr::HyperMethodCall {
                            target: Box::new(expr),
                            name: "AT-KEY".to_string(),
                            args,
                            modifier,
                            quoted: false,
                        };
                        rest = &r2[end + 1..];
                        continue;
                    }
                }
            }

            // Static method names (includes private owner qualification in `!Type::method`)
            let parsed_static_name = if modifier == Some('!') {
                parse_private_method_name(r)
            } else {
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
                    .ok()
                    .map(|(rr, name)| (rr, name.to_string()))
            };
            if let Some((r, name)) = parsed_static_name {
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
                        modifier,
                        quoted: false,
                    };
                    rest = r;
                    continue;
                }
                expr = Expr::HyperMethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier,
                    quoted: false,
                };
                rest = r;
                continue;
            }

            // Quoted method names: »."foo"(...), ».+"$method"(...)
            if let Some((r, qname)) = parse_quoted_method_name(r) {
                if !r.starts_with('(') {
                    return Err(PError::expected_at(
                        "parenthesized arguments after quoted method name",
                        r,
                    ));
                }
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                match qname {
                    QuotedMethodName::Static(name) => {
                        expr = Expr::HyperMethodCall {
                            target: Box::new(expr),
                            name,
                            args,
                            modifier,
                            quoted: true,
                        };
                    }
                    QuotedMethodName::Dynamic(name_expr) => {
                        expr = Expr::HyperMethodCallDynamic {
                            target: Box::new(expr),
                            name_expr: Box::new(name_expr),
                            args,
                            modifier,
                        };
                    }
                }
                rest = r;
                continue;
            }

            // Dynamic method name via sigiled expression: ».$meth, ».&code(...)
            if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') || r.starts_with('&')
            {
                if r.starts_with("$(") || r.starts_with("&(") {
                    let r_name = &r[2..];
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, name_expr) = expression(r_name)?;
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, _) = parse_char(r_name, ')')?;
                    let (r_name, _) = ws(r_name)?;
                    if r_name.starts_with('(') {
                        let (r_name, _) = parse_char(r_name, '(')?;
                        let (r_name, _) = ws(r_name)?;
                        let (r_name, args) = parse_call_arg_list(r_name)?;
                        let (r_name, _) = ws(r_name)?;
                        let (r_name, _) = parse_char(r_name, ')')?;
                        expr = Expr::HyperMethodCallDynamic {
                            target: Box::new(expr),
                            name_expr: Box::new(name_expr),
                            args,
                            modifier,
                        };
                        rest = r_name;
                        continue;
                    }
                    expr = Expr::HyperMethodCallDynamic {
                        target: Box::new(expr),
                        name_expr: Box::new(name_expr),
                        args: Vec::new(),
                        modifier,
                    };
                    rest = r_name;
                    continue;
                }
                let (r_name, name_expr) = super::super::primary::primary(r)?;
                let (r_name, _) = ws(r_name)?;
                if r_name.starts_with('(') {
                    let (r_name, _) = parse_char(r_name, '(')?;
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, args) = parse_call_arg_list(r_name)?;
                    let (r_name, _) = ws(r_name)?;
                    let (r_name, _) = parse_char(r_name, ')')?;
                    expr = Expr::HyperMethodCallDynamic {
                        target: Box::new(expr),
                        name_expr: Box::new(name_expr),
                        args,
                        modifier,
                    };
                    rest = r_name;
                    continue;
                }
                expr = Expr::HyperMethodCallDynamic {
                    target: Box::new(expr),
                    name_expr: Box::new(name_expr),
                    args: Vec::new(),
                    modifier,
                };
                rest = r_name;
                continue;
            }
        }

        // User-declared postcircumfix operators: expr⌊arg⌋ → postcircumfix:<⌊ ⌋>(expr, arg)
        if let Some((name, open_len, close_delim)) =
            crate::parser::stmt::simple::match_user_declared_postcircumfix_op(rest)
        {
            let r = &rest[open_len..];
            let (r, _) = ws(r)?;
            let (r, arg) = expression(r)?;
            let (r, _) = ws(r)?;
            if r.starts_with(close_delim.as_str()) {
                let r = &r[close_delim.len()..];
                expr = Expr::Call {
                    name,
                    args: vec![expr, arg],
                };
                rest = r;
                continue;
            }
        }

        // Atomic postfix updates: $x⚛++ / $x⚛--
        if let Some(after_atomic) = rest.strip_prefix("⚛++") {
            rest = after_atomic;
            if let Some(name) = atomic_var_name(&expr) {
                expr = Expr::Call {
                    name: "__mutsu_atomic_post_inc_var".to_string(),
                    args: vec![Expr::Literal(Value::Str(name))],
                };
            } else {
                expr = Expr::PostfixOp {
                    op: TokenKind::PlusPlus,
                    expr: Box::new(expr),
                };
            }
            continue;
        }
        if let Some(after_atomic) = rest.strip_prefix("⚛--") {
            rest = after_atomic;
            if let Some(name) = atomic_var_name(&expr) {
                expr = Expr::Call {
                    name: "__mutsu_atomic_post_dec_var".to_string(),
                    args: vec![Expr::Literal(Value::Str(name))],
                };
            } else {
                expr = Expr::PostfixOp {
                    op: TokenKind::MinusMinus,
                    expr: Box::new(expr),
                };
            }
            continue;
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

        // Custom postfix operator call: $x§ -> postfix:<§>($x)
        if let Some((name, len)) = crate::parser::stmt::simple::match_user_declared_postfix_op(rest)
        {
            let after = &rest[len..];
            if is_postfix_operator_boundary(after) {
                expr = Expr::Call {
                    name,
                    args: vec![expr],
                };
                rest = after;
                continue;
            }
        }

        if let Some((op, len)) = parse_custom_postfix_operator(rest) {
            let after = &rest[len..];
            if is_postfix_operator_boundary(after) {
                expr = Expr::Call {
                    name: format!("postfix:<{op}>"),
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
                    if append_call_arg(&mut expr, Expr::AnonSub { body, is_rw: false }) {
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

        // Postfix i (imaginary number): (expr)i or (expr)\i → Complex(0, expr)
        // The \i form uses unspace to separate the postfix from the preceding token
        // (e.g. Inf\i avoids being parsed as the identifier "Infi").
        if rest.starts_with("\\i") && !is_ident_char(rest.as_bytes().get(2).copied()) {
            rest = &rest[2..];
            expr = Expr::MethodCall {
                target: Box::new(expr),
                name: "i".to_string(),
                args: vec![],
                modifier: None,
                quoted: false,
            };
            continue;
        }
        if rest.starts_with('i') && !is_ident_char(rest.as_bytes().get(1).copied()) {
            rest = &rest[1..];
            expr = Expr::MethodCall {
                target: Box::new(expr),
                name: "i".to_string(),
                args: vec![],
                modifier: None,
                quoted: false,
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
