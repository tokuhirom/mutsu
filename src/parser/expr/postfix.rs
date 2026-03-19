use super::super::helpers::{
    consume_unspace, is_ident_char, is_non_breaking_space, split_angle_words, ws,
};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};
use super::super::primary::{colonpair_expr, parse_block_body, parse_call_arg_list, primary};

use crate::ast::{ExistsAdverb, Expr, HyperSliceAdverb, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::operators::{PrefixUnaryOp, parse_postfix_update_op, parse_prefix_unary_op};
use super::{expression, expression_no_sequence};

/// Check if a character is valid inside an angle-bracket word key (`<key>`).
/// Raku allows any non-whitespace character that isn't `>` in angle bracket words.
/// We allow: alphanumeric, common ASCII punctuation used in identifiers/keys,
/// non-breaking spaces, and any non-ASCII Unicode character that isn't whitespace.
fn is_angle_key_char(c: char) -> bool {
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
        || (!c.is_ascii() && !c.is_whitespace())
}

/// When a prefix operator is applied to a WhateverCode (Lambda or AnonSubParams),
/// compose the prefix into the body so that `+(* + 1)` becomes `-> $_ { +($_ + 1) }`
/// instead of trying to numify the closure itself.
fn compose_prefix_into_whatevercode(op: TokenKind, expr: Expr) -> Expr {
    match expr {
        Expr::Lambda { param, mut body } => {
            wrap_last_stmt_with_unary(&mut body, op.clone());
            Expr::Lambda { param, body }
        }
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            mut body,
            is_rw,
        } => {
            wrap_last_stmt_with_unary(&mut body, op.clone());
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw,
            }
        }
        other => Expr::Unary {
            op,
            expr: Box::new(other),
        },
    }
}

/// Wrap the last expression statement in a list of statements with a unary operator.
fn wrap_last_stmt_with_unary(stmts: &mut [Stmt], op: TokenKind) {
    if let Some(Stmt::Expr(expr)) = stmts.last_mut() {
        let inner = std::mem::replace(expr, Expr::Literal(Value::Nil));
        *expr = Expr::Unary {
            op,
            expr: Box::new(inner),
        };
    }
}

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

/// Parse dynamic subscript adverb: :$delete, :$exists — the variable value
/// determines whether the adverb is active at runtime.
fn parse_dynamic_subscript_adverb(input: &str) -> Option<&str> {
    if !input.starts_with(":$") {
        return None;
    }
    let r = &input[2..];
    // Read identifier
    let end = r
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(r.len());
    if end == 0 {
        return None;
    }
    let name = &r[..end];
    // Only recognize known subscript adverb names
    match name {
        "delete" | "exists" => Some(&r[end..]),
        _ => None,
    }
}

fn parse_subscript_adverb(input: &str) -> Option<(&str, &'static str)> {
    if input.starts_with(":!kv") && !is_ident_char(input.as_bytes().get(4).copied()) {
        return Some((&input[4..], "not-kv"));
    }
    if input.starts_with(":!p") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "not-p"));
    }
    if input.starts_with(":!k") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "not-k"));
    }
    if input.starts_with(":!v") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "not-v"));
    }
    if let Some(rest) = input.strip_prefix(":kv(0)") {
        return Some((rest, "kv0"));
    }
    if let Some(rest) = input.strip_prefix(":kv(1)") {
        return Some((rest, "kv"));
    }
    if input.starts_with(":kv") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "kv"));
    }
    if let Some(rest) = input.strip_prefix(":p(0)") {
        return Some((rest, "p0"));
    }
    if let Some(rest) = input.strip_prefix(":p(1)") {
        return Some((rest, "p"));
    }
    if input.starts_with(":p") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return Some((&input[2..], "p"));
    }
    if let Some(rest) = input.strip_prefix(":k(0)") {
        return Some((rest, "k0"));
    }
    if let Some(rest) = input.strip_prefix(":k(1)") {
        return Some((rest, "k"));
    }
    if input.starts_with(":k") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return Some((&input[2..], "k"));
    }
    if let Some(rest) = input.strip_prefix(":v(0)") {
        return Some((rest, "v0"));
    }
    if let Some(rest) = input.strip_prefix(":v(1)") {
        return Some((rest, "v"));
    }
    if input.starts_with(":v") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return Some((&input[2..], "v"));
    }
    None
}

/// Extract the variable name from a MultiDimIndex target expression.
fn multidim_target_var_name(target: &Expr) -> String {
    match target {
        Expr::ArrayVar(name) => format!("@{}", name),
        Expr::HashVar(name) => format!("%{}", name),
        Expr::Var(name) => name.clone(),
        _ => String::new(),
    }
}

enum DeleteAdverb {
    NoDelete,
    Delete(Option<Expr>),
}

fn parse_delete_adverb(input: &str) -> Option<(&str, DeleteAdverb)> {
    if input.starts_with(":!delete") && !is_ident_char(input.as_bytes().get(8).copied()) {
        return Some((&input[8..], DeleteAdverb::NoDelete));
    }
    if input.starts_with(":delete") && !is_ident_char(input.as_bytes().get(7).copied()) {
        let mut r = &input[7..];
        if let Some(r_stripped) = r.strip_prefix('(')
            && let Ok((r2, _)) = ws(r_stripped)
            && let Ok((r2, cond)) = expression(r2)
            && let Ok((r2, _)) = ws(r2)
            && let Ok((r2, _)) = parse_char(r2, ')')
        {
            return Some((r2, DeleteAdverb::Delete(Some(cond))));
        }
        // :delete with no argument, or malformed parens (leave for outer parser).
        if r.starts_with('(') {
            return None;
        }
        r = &input[7..];
        return Some((r, DeleteAdverb::Delete(None)));
    }
    None
}

fn subscript_adverb_expr(expr: Expr, adverb: &'static str) -> Expr {
    // Handle MultiDimIndex: @a[0;0;0]:kv etc.
    if let Expr::MultiDimIndex { target, dimensions } = expr {
        let mut args = vec![*target, Expr::Literal(Value::str(adverb.to_string()))];
        args.extend(dimensions);
        return Expr::Call {
            name: Symbol::intern("__mutsu_multidim_subscript_adverb"),
            args,
        };
    }
    let Expr::Index { target, index } = expr else {
        return expr;
    };
    let var_name = match target.as_ref() {
        Expr::ArrayVar(name) => Expr::Literal(Value::str(format!("@{}", name))),
        Expr::HashVar(name) => Expr::Literal(Value::str(format!("%{}", name))),
        _ => Expr::Literal(Value::Nil),
    };
    Expr::Call {
        name: Symbol::intern("__mutsu_subscript_adverb"),
        args: vec![
            *target,
            *index,
            Expr::Literal(Value::str(adverb.to_string())),
            var_name,
        ],
    }
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
            delete: false,
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

fn auto_invoke_bareword_method_target(expr: Expr) -> Expr {
    let Expr::BareWord(name) = expr else {
        return expr;
    };
    if crate::parser::stmt::simple::is_user_declared_sub(&name)
        || crate::parser::stmt::simple::is_imported_function(&name)
    {
        return Expr::Call {
            name: Symbol::intern(&name),
            args: Vec::new(),
        };
    }
    Expr::BareWord(name)
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

/// Result of parsing bracket indices — either a single-dimension index or
/// a multi-dimensional (semicolon-separated) index.
enum ParsedBracketIndex {
    /// Normal single-dimension index (possibly comma-separated list).
    Single(Expr),
    /// Multi-dimensional index: dimensions separated by semicolons.
    /// Each dimension can itself be a comma-separated list.
    MultiDim(Vec<Expr>),
}

fn parse_bracket_indices(input: &str) -> PResult<'_, Expr> {
    let (rest, parsed) = parse_bracket_indices_inner(input)?;
    match parsed {
        ParsedBracketIndex::Single(expr) => Ok((rest, expr)),
        // For callers that don't handle MultiDim, flatten to ArrayLiteral.
        // The postfix parser will use parse_bracket_indices_inner directly.
        ParsedBracketIndex::MultiDim(dims) => Ok((rest, Expr::ArrayLiteral(dims))),
    }
}

fn parse_bracket_indices_inner(input: &str) -> PResult<'_, ParsedBracketIndex> {
    let (r, first) = expression(input)?;
    let mut current_dim = vec![first];
    let mut dimensions: Vec<Expr> = Vec::new();
    let mut has_semicolons = false;
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if r2.starts_with(',') {
            let (r3, _) = parse_char(r2, ',')?;
            let (r3, _) = ws(r3)?;
            // Handle trailing comma before ']' or ';'
            if r3.starts_with(']') || r3.starts_with(';') {
                r = r3;
                continue;
            }
            let (r3, next) = expression(r3)?;
            current_dim.push(next);
            r = r3;
            continue;
        }
        if r2.starts_with(';') && !r2.starts_with(";;") {
            has_semicolons = true;
            // Finish current dimension
            let dim_expr = if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(std::mem::take(&mut current_dim))
            };
            dimensions.push(dim_expr);
            current_dim = Vec::new();
            let (r3, _) = parse_char(r2, ';')?;
            let (r3, _) = ws(r3)?;
            let (r3, next) = expression(r3)?;
            current_dim.push(next);
            r = r3;
            continue;
        }
        if has_semicolons {
            // Finish last dimension
            let dim_expr = if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(current_dim)
            };
            dimensions.push(dim_expr);
            return Ok((r2, ParsedBracketIndex::MultiDim(dimensions)));
        }
        return Ok((
            r2,
            ParsedBracketIndex::Single(if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(current_dim)
            }),
        ));
    }
}

/// Result of parsing a quoted method name.
pub(crate) enum QuotedMethodName {
    /// Static method name (single-quoted or double-quoted without interpolation)
    Static(String),
    /// Dynamic method name (double-quoted with interpolation)
    Dynamic(Expr),
}

pub(crate) fn parse_quoted_method_name(input: &str) -> Option<(&str, QuotedMethodName)> {
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
            use super::super::primary::string::interpolate_string_content_with_modes;
            let name_expr = interpolate_string_content_with_modes(content, true, true);
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

/// Parse the operator name from a prefix-as-postfix construct like `<->`, `«~»`, `<<~>>`, `["~"]`.
/// Input starts after the `:` in `.:<->`.
fn parse_prefix_as_postfix(input: &str) -> Option<(&str, String)> {
    // <<op>> (must be checked before <op>)
    if let Some(rest) = input.strip_prefix("<<") {
        // Handle both <<op>> and <<'op'>>
        let end = rest.find(">>")?;
        let mut op = &rest[..end];
        // Strip quotes if present: <<'op'>>
        if let Some(inner) = op.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
            op = inner;
        }
        if op.is_empty() {
            return None;
        }
        return Some((&rest[end + 2..], op.to_string()));
    }
    // <op>
    if let Some(rest) = input.strip_prefix('<') {
        let end = rest.find('>')?;
        let op = &rest[..end];
        if op.is_empty() {
            return None;
        }
        return Some((&rest[end + 1..], op.to_string()));
    }
    // «op» (U+00AB / U+00BB)
    if let Some(rest) = input.strip_prefix('\u{00AB}') {
        let end = rest.find('\u{00BB}')?;
        let op = &rest[..end];
        if op.is_empty() {
            return None;
        }
        return Some((&rest[end + '\u{00BB}'.len_utf8()..], op.to_string()));
    }
    // ["op"]
    if let Some(rest) = input.strip_prefix('[')
        && let Some(rest) = rest.strip_prefix('"')
    {
        let end = rest.find('"')?;
        let op = &rest[..end];
        let rest = &rest[end + 1..];
        let rest = rest.strip_prefix(']')?;
        if op.is_empty() {
            return None;
        }
        return Some((rest, op.to_string()));
    }
    None
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
            c.is_whitespace() || matches!(c, '.' | ')' | '}' | ']' | ',' | ';' | ':')
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
                    name: Symbol::intern("__mutsu_atomic_pre_inc_var"),
                    args: vec![Expr::Literal(Value::str(name))],
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
                    name: Symbol::intern("__mutsu_atomic_pre_dec_var"),
                    args: vec![Expr::Literal(Value::str(name))],
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
                    name: Symbol::intern("__mutsu_atomic_fetch_var"),
                    args: vec![Expr::Literal(Value::str(name))],
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
            && !op.chars().any(|c| matches!(c, '$' | '@' | '%' | '&'))
            && !op
                .chars()
                .any(|c| matches!(c, '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>' | '"'))
            && after.chars().next().is_some_and(char::is_whitespace)
        {
            let (after, _) = ws(after)?;
            let (after, arg) = prefix_expr(after)?;
            return Ok((
                after,
                Expr::Call {
                    name: Symbol::intern(&format!("prefix:<({})>", op)),
                    args: vec![arg],
                },
            ));
        }
    }

    if let Some((name, len)) = crate::parser::stmt::simple::match_user_declared_prefix_op(input) {
        let after_op = &input[len..];
        // If the text immediately after the operator is a hyper marker (<< or «),
        // fall through to the hyper prefix handler instead of treating as a regular call.
        let is_hyper = after_op.starts_with("<<") || after_op.starts_with('\u{00AB}');
        if is_hyper {
            // Fall through to hyper prefix metaop handling below
        } else {
            let rest = after_op;
            let (rest, _) = ws(rest)?;
            // Check if this prefix has a custom precedence level
            let prec_level = crate::parser::stmt::simple::lookup_prefix_precedence(&name);
            let (mut rest, arg) = match prec_level {
                Some(level) if level <= crate::parser::stmt::simple::PREC_ADDITIVE => {
                    // Looser than additive: grab everything up to additive level
                    super::precedence::loose_prefix_operand(rest, level)?
                }
                Some(level) if level <= crate::parser::stmt::simple::PREC_MULTIPLICATIVE => {
                    // Between additive and multiplicative
                    super::precedence::multiplicative_operand(rest)?
                }
                Some(level) if level <= crate::parser::stmt::simple::PREC_POWER => {
                    // Between multiplicative and power
                    super::precedence::power_operand(rest)?
                }
                _ => prefix_expr(rest)?,
            };
            let mut result = Expr::Call {
                name: Symbol::intern(&name),
                args: vec![arg],
            };
            // Apply loose postfix operators (declared `is looser(&prefix:<...>)`)
            loop {
                if let Some((post_name, post_len)) =
                    crate::parser::stmt::simple::match_user_declared_postfix_op(rest)
                {
                    let post_prec =
                        crate::parser::stmt::simple::lookup_postfix_precedence(&post_name);
                    if post_prec.is_some_and(|p| p < crate::parser::stmt::simple::PREC_PREFIX) {
                        let after = &rest[post_len..];
                        result = Expr::Call {
                            name: Symbol::intern(&post_name),
                            args: vec![result],
                        };
                        rest = after;
                        continue;
                    }
                }
                break;
            }
            return Ok((rest, result));
        } // end of !is_hyper else block
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
                    name: Symbol::intern("__mutsu_hyper_prefix"),
                    args: vec![Expr::Literal(Value::str(symbol.to_string())), arg],
                },
            ));
        }
    }

    if let Some((op, len)) = parse_prefix_unary_op(input) {
        // Skip prefix ++ and -- here: they are handled at the autoincrement
        // level inside power_expr, which binds tighter than ** but looser than
        // postfix. This ensures `++$i ** 2` parses as `(++$i) ** 2`.
        if !matches!(op, PrefixUnaryOp::PreInc | PrefixUnaryOp::PreDec) {
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
            // Detect precedence confusion: `!%h<a>:exists` should be `:!exists`
            // Only trigger when the operand was NOT parenthesized (i.e., not `!(%h<a>:exists)`)
            if op == PrefixUnaryOp::Not
                && matches!(&expr, Expr::Exists { .. })
                && !input[len..].trim_start().starts_with('(')
            {
                return Ok((
                    rest,
                    Expr::Call {
                        name: Symbol::intern("die"),
                        args: vec![Expr::Literal(Value::str(
                            "Precedence issue with ! and :exists, perhaps you meant :!exists?"
                                .to_string(),
                        ))],
                    },
                ));
            }
            // If the operand is a WhateverCode (Lambda or AnonSubParams), compose
            // the prefix operator into its body instead of wrapping it.
            let result = compose_prefix_into_whatevercode(op.token_kind(), expr);
            return Ok((rest, result));
        }
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
    // lazy prefix: wrap inner expression with .lazy method call
    if input.starts_with("lazy") && !is_ident_char(input.as_bytes().get(4).copied()) {
        let r = &input[4..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::MethodCall {
                target: Box::new(expr),
                name: Symbol::intern("lazy"),
                args: vec![],
                modifier: None,
                quoted: false,
            },
        ));
    }
    // hyper prefix: eager materialization variant (same surface semantics as `.hyper`)
    if input.starts_with("hyper") && !is_ident_char(input.as_bytes().get(5).copied()) {
        let r = &input[5..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::MethodCall {
                target: Box::new(expr),
                name: Symbol::intern("hyper"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            },
        ));
    }
    // eager prefix: force lazy evaluation
    if input.starts_with("eager") && !is_ident_char(input.as_bytes().get(5).copied()) {
        let r = &input[5..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((r, Expr::Eager(Box::new(expr))));
    }
    // ^expr — upto operator: ^5 means 0..^5
    // Use power_expr_tight so that `^2**64` parses as `^(2**64)` (** binds
    // tighter than ^), while `^10 .batch(3)` still parses as `(^10).batch(3)`
    // because whitespace-dotty is excluded from the tight variant.
    if input.starts_with('^') && !input.starts_with("^..") {
        let rest = &input[1..];
        let parsed_operand =
            super::precedence_meta_ops::power_expr_tight(rest).or_else(|_| prefix_expr(rest));
        if let Ok((rest, expr)) = parsed_operand {
            return postfix_expr_continue(
                rest,
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::Int(0))),
                    op: TokenKind::DotDotCaret,
                    right: Box::new(expr),
                },
            );
        }
    }
    // Hyper-prefix slip forms: |<< expr / |>> expr.
    // Lower to the same unary Pipe AST used by plain `|expr`.
    if input.starts_with("|<<") || input.starts_with("|>>") {
        let (rest, _) = ws(&input[3..])?;
        let (rest, expr) = prefix_expr(rest)?;
        return Ok((
            rest,
            Expr::Unary {
                op: TokenKind::Pipe,
                expr: Box::new(expr),
            },
        ));
    }
    // |expr — slip/flatten prefix
    if input.starts_with('|') && !input.starts_with("||") {
        let (rest, _) = ws(&input[1..])?;
        let (rest, expr) = prefix_expr(rest)?;
        return Ok((
            rest,
            Expr::Unary {
                op: TokenKind::Pipe,
                expr: Box::new(expr),
            },
        ));
    }
    super::precedence_meta_ops::power_expr(input)
}

/// Like `postfix_expr` but does NOT allow whitespace-separated dotty method
/// calls.  Used for operands of tight prefix operators like `^` so that
/// `^10 .batch(3)` parses as `(^10).batch(3)` rather than `^(10.batch(3))`.
fn postfix_expr_tight(input: &str) -> PResult<'_, Expr> {
    postfix_expr_inner(input, false)
}

/// Public wrapper for `postfix_expr_tight`, used by `power_expr_tight`.
pub(super) fn postfix_expr_tight_pub(input: &str) -> PResult<'_, Expr> {
    postfix_expr_tight(input)
}

/// Continue applying postfix operations (including whitespace-dotty) to an
/// already-parsed expression.  Used after prefix operators like `^` to allow
/// `^10 .method` to call `.method` on the range result.
pub(in crate::parser) fn postfix_expr_continue(input: &str, expr: Expr) -> PResult<'_, Expr> {
    postfix_expr_loop(input, expr, true)
}

/// Postfix: method calls (.method), indexing ([]), ++, --
pub(super) fn postfix_expr(input: &str) -> PResult<'_, Expr> {
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
            expr = auto_invoke_bareword_method_target(expr);
            let r = &rest[1..];
            // `.=` mutating method call.  When the target is a simple variable
            // (`$x`, `@a`, `%h`) the statement-level parser can turn `$x.=m` into
            // `$x = $x.m`.  Break out of postfix parsing so the statement parser
            // handles it.  For all other targets (parenthesised exprs, method-call
            // chains, etc.) we parse `.=method(args)` here and produce the method
            // call expression inline so that it works inside argument lists.
            if let Some(r_after_eq) = r.strip_prefix('=') {
                let handled_by_stmt_parser = matches!(
                    &expr,
                    Expr::Var(_)
                        | Expr::ArrayVar(_)
                        | Expr::HashVar(_)
                        | Expr::Index { .. }
                        | Expr::MultiDimIndex { .. }
                        | Expr::BareWord(_)
                );
                if handled_by_stmt_parser {
                    break;
                }
                // Non-variable target: parse .=method inline as a method call
                let r = r_after_eq;
                let (r, _) = ws(r)?;
                // Try quoted method name
                if let Some((r2, qname)) = parse_quoted_method_name(r) {
                    if !r2.starts_with('(') {
                        return Err(PError::expected_at(
                            "parenthesized arguments after quoted method name with '.='",
                            r2,
                        ));
                    }
                    let (r2, _) = parse_char(r2, '(')?;
                    let (r2, _) = ws(r2)?;
                    let (r2, args) = parse_call_arg_list(r2)?;
                    let (r2, _) = ws(r2)?;
                    let (r2, _) = parse_char(r2, ')')?;
                    match qname {
                        QuotedMethodName::Static(name) => {
                            expr = Expr::MethodCall {
                                target: Box::new(expr),
                                name: Symbol::intern(&name),
                                args,
                                modifier: None,
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
                    rest = r2;
                    continue;
                }
                // Regular identifier method name
                if let Ok((r2, method_name)) =
                    take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
                {
                    let (r2, args) = if r2.starts_with('(') {
                        let (r3, _) = parse_char(r2, '(')?;
                        let (r3, _) = ws(r3)?;
                        let (r3, args) = parse_call_arg_list(r3)?;
                        let (r3, _) = ws(r3)?;
                        let (r3, _) = parse_char(r3, ')')?;
                        (r3, args)
                    } else {
                        (r2, Vec::new())
                    };
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name: Symbol::intern(method_name),
                        args,
                        modifier: None,
                        quoted: false,
                    };
                    rest = r2;
                    continue;
                }
                // Can't parse method name; break and let statement parser handle
                break;
            }
            // Allow `.>>...` / `.»...` chains by deferring to the dedicated hyper
            // postfix parser below.
            if r.starts_with(">>") || r.starts_with('\u{00BB}') {
                rest = r;
                continue;
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
            // Check for .{index} syntax: object.{$expr}
            if let Some(r) = r.strip_prefix('{') {
                let (r, _) = ws(r)?;
                let (r, index) = parse_bracket_indices(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, '}')?;
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
                        && keys
                            .iter()
                            .all(|key| !key.is_empty() && key.chars().all(is_angle_key_char))
                    {
                        let r2 = &r2[end + 1..];
                        let index_expr = if keys.len() == 1 {
                            Expr::Literal(Value::str(keys[0].to_string()))
                        } else {
                            Expr::ArrayLiteral(
                                keys.into_iter()
                                    .map(|k| Expr::Literal(Value::str(k.to_string())))
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
            if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') || r.starts_with('&')
            {
                let (r, name_expr) = primary(r)?;
                let r = consume_unspace(r);
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, args) = parse_call_arg_list(r)?;
                    let (r, _) = ws(r)?;
                    let (r, _) = parse_char(r, ')')?;
                    expr = Expr::DynamicMethodCall {
                        target: Box::new(expr),
                        name_expr: Box::new(name_expr),
                        args,
                    };
                    rest = r;
                    continue;
                }
                let (r2, _) = ws(r)?;
                if r2.starts_with(':') && !r2.starts_with("::") {
                    let r3 = &r2[1..];
                    let (r3, _) = ws(r3)?;
                    let (r3, first_arg) = expression(r3)?;
                    let mut args = vec![first_arg];
                    let mut r_inner = r3;
                    loop {
                        let (r4, _) = ws(r_inner)?;
                        if r4.starts_with(':')
                            && !r4.starts_with("::")
                            && let Ok((r5, next)) = colonpair_expr(r4)
                        {
                            args.push(next);
                            r_inner = r5;
                            continue;
                        }
                        if !r4.starts_with(',') {
                            break;
                        }
                        let r4 = &r4[1..];
                        let (r4, _) = ws(r4)?;
                        let (r4, next) = expression(r4)?;
                        args.push(next);
                        r_inner = r4;
                    }
                    expr = Expr::DynamicMethodCall {
                        target: Box::new(expr),
                        name_expr: Box::new(name_expr),
                        args,
                    };
                    rest = r_inner;
                    continue;
                }
                expr = Expr::DynamicMethodCall {
                    target: Box::new(expr),
                    name_expr: Box::new(name_expr),
                    args: Vec::new(),
                };
                rest = r;
                continue;
            }
            // Prefix-as-postfix: .:<op> / .:«op» / .:<<op>> / .:["op"]
            // Applies prefix operator as postfix: 42.:<-> == -42
            if let Some(r_colon) = r.strip_prefix(':')
                && let Some((r_after, op_name)) = parse_prefix_as_postfix(r_colon)
            {
                expr = Expr::Call {
                    name: Symbol::intern(&format!("prefix:<{op_name}>")),
                    args: vec![expr],
                };
                rest = r_after;
                continue;
            }
            // Parse qualified method name with leading `::`, e.g. `.::Int::abs`
            // `.::` with no name is a syntax error (X::Syntax::Malformed)
            if let Some(r_after_colons) = r.strip_prefix("::") {
                if let Ok((r2, first_part)) = take_while1(r_after_colons, |c: char| {
                    c.is_alphanumeric() || c == '_' || c == '-'
                }) {
                    let mut qualified = first_part.to_string();
                    let mut r2 = r2;
                    while let Some(after_colons) = r2.strip_prefix("::") {
                        if let Ok((r3, part)) = take_while1(after_colons, |c: char| {
                            c.is_alphanumeric() || c == '_' || c == '-'
                        }) {
                            qualified.push_str("::");
                            qualified.push_str(part);
                            r2 = r3;
                        } else {
                            break;
                        }
                    }
                    let name = Symbol::intern(&qualified);
                    let r2_unspace = consume_unspace(r2);
                    if r2_unspace.starts_with('(') {
                        let (r3, _) = parse_char(r2_unspace, '(')?;
                        let (r3, _) = ws(r3)?;
                        let (r3, args) = parse_call_arg_list(r3)?;
                        let (r3, _) = ws(r3)?;
                        let (r3, _) = parse_char(r3, ')')?;
                        expr = Expr::MethodCall {
                            target: Box::new(expr),
                            name,
                            args,
                            modifier,
                            quoted: false,
                        };
                        rest = r3;
                        continue;
                    }
                    // No-arg qualified method call
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args: Vec::new(),
                        modifier,
                        quoted: false,
                    };
                    rest = r2;
                    continue;
                } else {
                    return Err(PError::fatal(
                        "X::Syntax::Malformed: Malformed qualified method name".to_string(),
                    ));
                }
            }
            // Parse method name
            // Allow whitespace between dot and method name: `$x . abs` or `$x. abs`
            let r = ws(r).map_or(r, |(r_ws, _)| r_ws);
            if let Ok((r, parsed_name)) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                let mut method_name = parsed_name.to_string();
                let mut trailing_postfix: Option<TokenKind> = None;
                if !r.starts_with('(') {
                    if method_name.ends_with("++") && method_name.len() > 2 {
                        method_name.truncate(method_name.len() - 2);
                        trailing_postfix = Some(TokenKind::PlusPlus);
                    } else if method_name.ends_with("--") && method_name.len() > 2 {
                        method_name.truncate(method_name.len() - 2);
                        trailing_postfix = Some(TokenKind::MinusMinus);
                    }
                }
                // Check for qualified method name: C::x (e.g., .Parent::x)
                let (r, name) = if r.starts_with("::") {
                    let mut qualified = method_name;
                    let mut r = r;
                    while let Some(after_colons) = r.strip_prefix("::") {
                        if let Ok((r2, part)) = take_while1(after_colons, |c: char| {
                            c.is_alphanumeric() || c == '_' || c == '-'
                        }) {
                            qualified.push_str("::");
                            qualified.push_str(part);
                            r = r2;
                        } else {
                            break;
                        }
                    }
                    (r, Symbol::intern(&qualified))
                } else {
                    (r, Symbol::intern(&method_name))
                };
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
                        if r4.starts_with(':')
                            && !r4.starts_with("::")
                            && let Ok((r5, next)) = colonpair_expr(r4)
                        {
                            args.push(next);
                            r_inner = r5;
                            continue;
                        }
                        if !r4.starts_with(',') {
                            break;
                        }
                        let r4 = &r4[1..];
                        let (r4, _) = ws(r4)?;
                        // Handle trailing comma before ';' or '}'
                        if r4.starts_with(';') || r4.starts_with('}') || r4.is_empty() {
                            r_inner = r4;
                            break;
                        }
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
                let mut method_expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier,
                    quoted: false,
                };
                if let Some(op) = trailing_postfix {
                    method_expr = Expr::PostfixOp {
                        op,
                        expr: Box::new(method_expr),
                    };
                }
                expr = method_expr;
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
                            name: Symbol::intern(&name),
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
                        name: Symbol::intern(&format!("postfix:<{op}>")),
                        args: vec![expr],
                    };
                    rest = after;
                    continue;
                }
            }
            return Err(PError::expected_at("method name", r));
        }

        // Private method call: target!Type::method(args) / target!method(args)
        // Also supports quoted names: target!"method"() / target!'method'()
        if rest.starts_with('!') && !rest.starts_with("!=") {
            let after_bang = &rest[1..];
            if let Some((r, name)) = parse_private_method_name(after_bang) {
                let name = Symbol::intern(&name);
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
                // Check for colon-arg syntax: !method: arg, arg2
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
                        // Handle trailing comma before ';' or '}'
                        if r4.starts_with(';') || r4.starts_with('}') || r4.is_empty() {
                            r_inner = r4;
                            break;
                        }
                        let (r4, next) = expression(r4)?;
                        args.push(next);
                        r_inner = r4;
                    }
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                        modifier: Some('!'),
                        quoted: false,
                    };
                    rest = r_inner;
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
            // Try quoted method name: self!"method"() / self!'method'()
            if let Some((r, qname)) = parse_quoted_method_name(after_bang)
                && r.starts_with('(')
            {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                match qname {
                    QuotedMethodName::Static(name) => {
                        expr = Expr::MethodCall {
                            target: Box::new(expr),
                            name: Symbol::intern(&name),
                            args,
                            modifier: Some('!'),
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
        }

        // Postfix symbolic lookup: expr::(key) -> expr.WHO{key}
        if let Some(after_scope) = rest.strip_prefix("::(") {
            let (r, _) = ws(after_scope)?;
            let (r, key_expr) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let who = Expr::MethodCall {
                target: Box::new(expr),
                name: Symbol::intern("WHO"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
            expr = Expr::Index {
                target: Box::new(who),
                index: Box::new(key_expr),
            };
            rest = r;
            continue;
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

        // Array indexing: [expr] or zen slice [] or multislice [a;b;c]
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
            // Check for || syntax: @a[|| @indices]
            // || takes a list and treats each element as a dimension separator.
            if r.starts_with("||") && !r.starts_with("|||") {
                let r2 = &r[2..];
                let (r2, _) = ws(r2)?;
                // Parse comma-separated list of dimension expressions
                let (r2, first) = expression(r2)?;
                let (mut r2, _) = ws(r2)?;
                let mut items = vec![first];
                while r2.starts_with(',') && !r2.starts_with(",,") {
                    let r3 = &r2[1..];
                    let (r3, _) = ws(r3)?;
                    if r3.starts_with(']') {
                        r2 = r3;
                        break;
                    }
                    let (r3, item) = expression(r3)?;
                    items.push(item);
                    let (r3, _) = ws(r3)?;
                    r2 = r3;
                }
                let (r2, _) = parse_char(r2, ']')?;
                let list_expr = if items.len() == 1 {
                    items.remove(0)
                } else {
                    Expr::ArrayLiteral(items)
                };
                expr = Expr::MultiDimIndex {
                    target: Box::new(expr),
                    dimensions: vec![list_expr],
                };
                rest = r2;
                continue;
            }
            let (r, parsed) = parse_bracket_indices_inner(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            match parsed {
                ParsedBracketIndex::Single(index) => {
                    expr = Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                ParsedBracketIndex::MultiDim(dimensions) => {
                    expr = Expr::MultiDimIndex {
                        target: Box::new(expr),
                        dimensions,
                    };
                }
            }
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
                && keys
                    .iter()
                    .any(|key| key.is_empty() || !key.chars().all(is_angle_key_char))
            {
                return Err(PError::expected_at("angle index key", r));
            }
            let r = &r[end + 1..];
            let index_expr = if keys.len() == 1 {
                Expr::Literal(Value::str(keys[0].to_string()))
            } else {
                Expr::ArrayLiteral(
                    keys.into_iter()
                        .map(|k| Expr::Literal(Value::str(k.to_string())))
                        .collect(),
                )
            };
            let indexed_expr = if is_zen_angle {
                match &expr {
                    Expr::HashVar(_) => expr.clone(),
                    _ => Expr::MethodCall {
                        target: Box::new(expr.clone()),
                        name: Symbol::intern("__mutsu_zen_angle"),
                        args: Vec::new(),
                        modifier: None,
                        quoted: false,
                    },
                }
            } else {
                Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index_expr),
                }
            };
            if is_zen_angle && r.starts_with(":v") && !is_ident_char(r.as_bytes().get(2).copied()) {
                let r = &r[2..];
                expr = Expr::MethodCall {
                    target: Box::new(indexed_expr),
                    name: Symbol::intern("values"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
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
                name: Symbol::intern("new"),
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

        // Hash hyperindex: %hash{||@keys} or %hash{|| <a b>, "d"}
        if rest.starts_with("{||")
            && matches!(&expr, Expr::HashVar(_) | Expr::Var(_) | Expr::Index { .. })
        {
            let r = &rest[3..];
            let (r, _) = ws(r)?;
            let (r, first) = expression_no_sequence(r)?;
            let (r, _) = ws(r)?;
            // Check for comma-separated list of dimension keys
            let keys_expr = if r.starts_with(',') {
                let mut items = vec![first];
                let mut r = r;
                while r.starts_with(',') {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with('}') {
                        r = r2;
                        break;
                    }
                    let (r2, item) = expression_no_sequence(r2)?;
                    items.push(item);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                let (r2, _) = parse_char(r, '}')?;
                expr = Expr::HyperIndex {
                    target: Box::new(expr),
                    keys: Box::new(Expr::ArrayLiteral(items)),
                };
                rest = r2;
                continue;
            } else {
                first
            };
            let (r, _) = parse_char(r, '}')?;
            expr = Expr::HyperIndex {
                target: Box::new(expr),
                keys: Box::new(keys_expr),
            };
            rest = r;
            continue;
        }

        // Hash indexing with braces: %hash{"key"}, %hash{$var}, @a[0]{"key"}, etc.
        // Also handle postcircumfix on literals/expressions: 5{"c"} should throw at runtime.
        if rest.starts_with('{')
            && matches!(
                &expr,
                Expr::HashVar(_)
                    | Expr::Var(_)
                    | Expr::Index { .. }
                    | Expr::MethodCall { .. }
                    | Expr::Call { .. }
                    | Expr::Literal(_)
                    | Expr::ArrayLiteral(_)
                    | Expr::Hash(_)
            )
        {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            // Empty hash subscript (%h{}) is a zen slice — returns the hash itself
            // (which iterates as pairs), unlike %h{*} which returns values.
            // However, %h{}:exists and %h{}:delete need Index{Whatever} for adverb handling.
            if let Some(r) = r.strip_prefix('}') {
                let (r_adv, _) = ws(r)?;
                if r_adv.starts_with(":exists")
                    || r_adv.starts_with(":!exists")
                    || r_adv.starts_with(":delete")
                {
                    // Keep as Index with Whatever so adverb processing works
                    expr = Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(Expr::Whatever),
                    };
                    rest = r;
                    continue;
                }
                expr = Expr::ZenSlice(Box::new(expr));
                rest = r;
                continue;
            }
            let (r, index) = {
                let (r, index) = parse_bracket_indices(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, '}')?;
                (r, index)
            };
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
            // Try :delete:exists combination first
            if let Some((r_after_delete, delete_adv)) = parse_delete_adverb(r_adv) {
                let indexed_expr = Expr::Index {
                    target: Box::new(expr.clone()),
                    index: Box::new(index.clone()),
                };
                if let Some((r_after, mut exists_expr)) =
                    try_parse_exists_adverb(r_after_delete, indexed_expr.clone())
                {
                    if let Expr::Exists { delete, .. } = &mut exists_expr {
                        *delete = matches!(delete_adv, DeleteAdverb::Delete(_));
                    }
                    expr = exists_expr;
                    rest = r_after;
                    continue;
                }
                rest = r_after_delete;
                match delete_adv {
                    DeleteAdverb::NoDelete => {
                        expr = indexed_expr;
                    }
                    DeleteAdverb::Delete(None) => {
                        expr = Expr::MethodCall {
                            target: Box::new(indexed_expr),
                            name: Symbol::intern("DELETE-KEY"),
                            args: vec![],
                            modifier: None,
                            quoted: false,
                        };
                    }
                    DeleteAdverb::Delete(Some(cond)) => {
                        let delete_expr = Expr::MethodCall {
                            target: Box::new(indexed_expr.clone()),
                            name: Symbol::intern("DELETE-KEY"),
                            args: vec![],
                            modifier: None,
                            quoted: false,
                        };
                        expr = Expr::Ternary {
                            cond: Box::new(cond),
                            then_expr: Box::new(delete_expr),
                            else_expr: Box::new(indexed_expr),
                        };
                    }
                }
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
        // Also on expressions already wrapped with adverbs (Exists, __mutsu_multidim_adverb)
        if matches!(
            &expr,
            Expr::Index { .. }
                | Expr::ZenSlice(_)
                | Expr::MultiDimIndex { .. }
                | Expr::Exists { .. }
        ) || matches!(&expr, Expr::Call { name, .. } if name == "__mutsu_subscript_adverb" || name == "__mutsu_multidim_adverb" || name == "__mutsu_multidim_subscript_adverb" || name == "__mutsu_multidim_delete" || name == "__mutsu_multidim_dynamic_adverb")
        {
            let (r_adv2, _) = ws(rest)?;
            if let Some((r_after_delete, delete_adv)) = parse_delete_adverb(r_adv2)
                && let Some((r_after, mut exists_expr)) =
                    try_parse_exists_adverb(r_after_delete, expr.clone())
            {
                if let Expr::Exists { delete, .. } = &mut exists_expr {
                    *delete = matches!(delete_adv, DeleteAdverb::Delete(_));
                }
                expr = exists_expr;
                rest = r_after;
                continue;
            }
            if let Some((r_after_adv, adv_name)) = parse_subscript_adverb(r_adv2) {
                // Avoid consuming ternary `:v` separator in `?? !!` expressions.
                if !has_ternary_else_after(r_after_adv) {
                    expr = subscript_adverb_expr(expr, adv_name);
                    rest = r_after_adv;
                    continue;
                }
            }
            if let Some((r_after, exists_expr)) = try_parse_exists_adverb(r_adv2, expr.clone()) {
                expr = exists_expr;
                rest = r_after;
                continue;
            }
            if let Some((r_after_delete, delete_adv)) = parse_delete_adverb(r_adv2) {
                rest = r_after_delete;
                match delete_adv {
                    DeleteAdverb::NoDelete => {
                        // Explicitly non-deleting adverb; expression is unchanged.
                    }
                    DeleteAdverb::Delete(None) => {
                        if let Expr::MultiDimIndex {
                            target: mdt,
                            dimensions: dims,
                        } = expr
                        {
                            let var_name = multidim_target_var_name(&mdt);
                            let mut args = vec![Expr::Literal(Value::str(var_name))];
                            args.extend(dims);
                            expr = Expr::Call {
                                name: Symbol::intern("__mutsu_multidim_delete"),
                                args,
                            };
                        } else {
                            expr = Expr::MethodCall {
                                target: Box::new(expr),
                                name: Symbol::intern("DELETE-KEY"),
                                args: vec![],
                                modifier: None,
                                quoted: false,
                            };
                        }
                    }
                    DeleteAdverb::Delete(Some(cond)) => {
                        let original_expr = expr.clone();
                        if let Expr::MultiDimIndex {
                            target: mdt,
                            dimensions: dims,
                        } = original_expr.clone()
                        {
                            let var_name = multidim_target_var_name(&mdt);
                            let mut args = vec![Expr::Literal(Value::str(var_name))];
                            args.extend(dims);
                            let delete_expr = Expr::Call {
                                name: Symbol::intern("__mutsu_multidim_delete"),
                                args,
                            };
                            expr = Expr::Ternary {
                                cond: Box::new(cond),
                                then_expr: Box::new(delete_expr),
                                else_expr: Box::new(original_expr),
                            };
                        } else {
                            let delete_expr = Expr::MethodCall {
                                target: Box::new(original_expr.clone()),
                                name: Symbol::intern("DELETE-KEY"),
                                args: vec![],
                                modifier: None,
                                quoted: false,
                            };
                            expr = Expr::Ternary {
                                cond: Box::new(cond),
                                then_expr: Box::new(delete_expr),
                                else_expr: Box::new(original_expr),
                            };
                        }
                    }
                }
                continue;
            }
            // Dynamic adverb: :$delete — runtime-decided delete
            if let Some(r_after) = parse_dynamic_subscript_adverb(r_adv2) {
                let adverb_var = &r_adv2[2..r_adv2.len() - r_after.len()];
                if let Expr::MultiDimIndex {
                    target: mdt,
                    dimensions: dims,
                } = expr
                {
                    // For MultiDimIndex, pass var_name + adverb info + indices
                    let var_name = multidim_target_var_name(&mdt);
                    let mut args = vec![
                        Expr::Literal(Value::str(var_name)),
                        Expr::Literal(Value::str(adverb_var.to_string())),
                        Expr::Var(adverb_var.to_string()),
                    ];
                    args.extend(dims);
                    expr = Expr::Call {
                        name: Symbol::intern("__mutsu_multidim_dynamic_adverb"),
                        args,
                    };
                } else if matches!(&expr, Expr::Call { name, .. }
                    if name == "__mutsu_multidim_subscript_adverb"
                    || name == "__mutsu_multidim_exists_adverb")
                {
                    // Wrap adverb calls: inject delete flag into the call
                    // by converting to a combined handler
                    if let Expr::Call {
                        name: inner_name,
                        mut args,
                    } = expr
                    {
                        // Insert dynamic delete flag after adverb name
                        // Original args: [target_expr, adverb_name, dim0, dim1, ...]
                        // New args: [var_name_str, adverb_name, delete_var, dim0, dim1, ...]
                        let target = args.remove(0);
                        let var_name_str = multidim_target_var_name(&target);
                        let adverb_name = args.remove(0);
                        let dims = args;
                        let mut new_args = vec![
                            Expr::Literal(Value::str(var_name_str)),
                            adverb_name,
                            Expr::Var(adverb_var.to_string()),
                        ];
                        new_args.extend(dims);
                        let fn_name = if inner_name == "__mutsu_multidim_subscript_adverb" {
                            "__mutsu_multidim_subscript_adverb_dyn"
                        } else {
                            "__mutsu_multidim_exists_adverb_dyn"
                        };
                        expr = Expr::Call {
                            name: Symbol::intern(fn_name),
                            args: new_args,
                        };
                    } else {
                        unreachable!()
                    }
                } else if let Expr::Exists {
                    target,
                    negated,
                    adverb: exists_adv,
                    ..
                } = &expr
                {
                    // :$delete on Exists { target: MultiDimIndex }
                    if let Expr::MultiDimIndex {
                        target: mdt,
                        dimensions: dims,
                    } = target.as_ref()
                    {
                        let var_name = multidim_target_var_name(mdt);
                        let adverb_str = match exists_adv {
                            ExistsAdverb::Kv => "kv",
                            ExistsAdverb::P => "p",
                            ExistsAdverb::InvalidK => "k",
                            ExistsAdverb::InvalidV => "v",
                            _ => "none",
                        };
                        let negated_val = *negated;
                        let dims = dims.clone();
                        let mut new_args = vec![
                            Expr::Literal(Value::str(var_name)),
                            Expr::Literal(Value::Bool(negated_val)),
                            Expr::Var(adverb_var.to_string()),
                            Expr::Literal(Value::str(adverb_str.to_string())),
                        ];
                        new_args.extend(dims);
                        expr = Expr::Call {
                            name: Symbol::intern("__mutsu_multidim_exists_adverb_dyn"),
                            args: new_args,
                        };
                    } else {
                        expr = Expr::Call {
                            name: Symbol::intern("__mutsu_multidim_adverb"),
                            args: vec![
                                expr,
                                Expr::Literal(Value::str(adverb_var.to_string())),
                                Expr::Var(adverb_var.to_string()),
                            ],
                        };
                    }
                } else {
                    expr = Expr::Call {
                        name: Symbol::intern("__mutsu_multidim_adverb"),
                        args: vec![
                            expr,
                            Expr::Literal(Value::str(adverb_var.to_string())),
                            Expr::Var(adverb_var.to_string()),
                        ],
                    };
                }
                rest = r_after;
                continue;
            }
        }

        // General :delete adverb. Index targets are lowered to dedicated delete
        // opcodes by the compiler; non-index targets die at runtime.
        let (r_delete, _) = ws(rest)?;
        if let Some((r_after_delete, delete_adv)) = parse_delete_adverb(r_delete) {
            rest = r_after_delete;
            match delete_adv {
                DeleteAdverb::NoDelete => {}
                DeleteAdverb::Delete(None) => {
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name: Symbol::intern("DELETE-KEY"),
                        args: vec![],
                        modifier: None,
                        quoted: false,
                    };
                }
                DeleteAdverb::Delete(Some(cond)) => {
                    let delete_expr = Expr::MethodCall {
                        target: Box::new(expr.clone()),
                        name: Symbol::intern("DELETE-KEY"),
                        args: vec![],
                        modifier: None,
                        quoted: false,
                    };
                    expr = Expr::Ternary {
                        cond: Box::new(cond),
                        then_expr: Box::new(delete_expr),
                        else_expr: Box::new(expr),
                    };
                }
            }
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
            let after_hyper = consume_unspace(after_hyper);
            // Hyper `.=` assignment is handled by statement-level assignment parsing.
            if after_hyper.starts_with(".=") {
                break;
            }
            // Hyper postfix update: expr>>++ / expr>>--
            if let Some((op, len)) = parse_postfix_update_op(after_hyper) {
                let name = match op.token_kind() {
                    TokenKind::PlusPlus => "postfix:<++>",
                    TokenKind::MinusMinus => "postfix:<-->",
                    _ => unreachable!("postfix update parser returned unexpected token"),
                };
                expr = Expr::HyperMethodCall {
                    target: Box::new(expr),
                    name: Symbol::intern(name),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                };
                rest = &after_hyper[len..];
                continue;
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
            // Hyper indexing: expr»[idx] / expr».[idx] (or >> forms) => expr».AT-POS(idx)
            let hyper_index_input = after_hyper
                .strip_prefix(".[")
                .map(|r| (r, true))
                .or_else(|| after_hyper.strip_prefix('[').map(|r| (r, false)));
            if let Some((r, _dotted)) = hyper_index_input {
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
                    name: Symbol::intern("AT-POS"),
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

            // Hyper postcircumfix call-on: >>.(args) — invoke each element as a callable
            if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                expr = Expr::HyperMethodCall {
                    target: Box::new(expr),
                    name: Symbol::intern("CALL-ME"),
                    args,
                    modifier,
                    quoted: false,
                };
                rest = r;
                continue;
            }

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
                        && keys
                            .iter()
                            .all(|key| !key.is_empty() && key.chars().all(is_angle_key_char))
                    {
                        let args = if keys.len() == 1 {
                            vec![Expr::Literal(Value::str(keys[0].to_string()))]
                        } else {
                            vec![Expr::ArrayLiteral(
                                keys.into_iter()
                                    .map(|k| Expr::Literal(Value::str(k.to_string())))
                                    .collect(),
                            )]
                        };
                        expr = Expr::HyperMethodCall {
                            target: Box::new(expr),
                            name: Symbol::intern("AT-KEY"),
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
                let name = Symbol::intern(&name);
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
                            name: Symbol::intern(&name),
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
                    name: Symbol::intern(&name),
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
                    name: Symbol::intern("__mutsu_atomic_post_inc_var"),
                    args: vec![Expr::Literal(Value::str(name))],
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
                    name: Symbol::intern("__mutsu_atomic_post_dec_var"),
                    args: vec![Expr::Literal(Value::str(name))],
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
            let ident_continuation = after
                .as_bytes()
                .first()
                .copied()
                .is_some_and(|b| is_ident_char(Some(b)));
            if !ident_continuation {
                // Skip postfix ops that have a precedence level lower than PREFIX
                // (declared `is looser(&prefix:<...>)`). These will be handled
                // at the prefix level after the prefix operand is parsed.
                let post_prec = crate::parser::stmt::simple::lookup_postfix_precedence(&name);
                if post_prec.is_some_and(|p| p < crate::parser::stmt::simple::PREC_PREFIX) {
                    // Don't consume this postfix here; let it be consumed at prefix level
                } else {
                    expr = Expr::Call {
                        name: Symbol::intern(&name),
                        args: vec![expr],
                    };
                    rest = after;
                    continue;
                }
            }
        }

        if let Some((op, len)) = parse_custom_postfix_operator(rest) {
            let after = &rest[len..];
            if is_postfix_operator_boundary(after) {
                expr = Expr::Call {
                    name: Symbol::intern(&format!("postfix:<{op}>")),
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
                name: Symbol::intern("i"),
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
                name: Symbol::intern("i"),
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
    let mut len = 0usize;
    let mut sign = 1i64;
    if let Some(first) = chars.clone().next() {
        match first {
            '\u{207A}' => {
                // ⁺
                len += first.len_utf8();
                chars.next();
            }
            '\u{207B}' | '\u{00AF}' => {
                // ⁻ or ¯
                sign = -1;
                len += first.len_utf8();
                chars.next();
            }
            _ => {}
        }
    }
    let first = chars.next()?;
    let mut value = superscript_digit(first)?;
    len += first.len_utf8();
    for c in chars {
        if let Some(d) = superscript_digit(c) {
            value = value * 10 + d;
            len += c.len_utf8();
        } else {
            break;
        }
    }
    Some((sign * value, len))
}
