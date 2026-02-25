use super::super::parse_result::{PError, PResult, parse_char, take_while1};

use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::value::Value;
use crate::value::signature::{make_signature_value, param_defs_to_sig_info};

use super::super::expr::expression;
use super::super::helpers::{split_angle_words, ws};
use super::super::stmt::keyword;
use super::string::{double_quoted_string, single_quoted_string};
use super::var::parse_ident_with_hyphens;

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

fn skip_pointy_return_type(mut r: &str) -> PResult<'_, Option<String>> {
    let (r2, _) = ws(r)?;
    r = r2;
    if let Some(after_arrow) = r.strip_prefix("-->") {
        let (after_arrow, _) = ws(after_arrow)?;
        // Keep this permissive for now: simple type-like names only.
        let (after_arrow, type_name) = take_while1(after_arrow, |c: char| {
            c.is_alphanumeric() || c == '_' || c == ':'
        })?;
        let (after_arrow, _) = ws(after_arrow)?;
        Ok((after_arrow, Some(type_name.to_string())))
    } else {
        Ok((r, None))
    }
}

/// Known reduction operators (must be listed to distinguish from array literals).
const REDUCTION_OPS: &[&str] = &[
    "+", "-", "*", "/", "%", "~", "||", "&&", "//", "%%", "**", "^^", "+&", "+|", "+^", "+<", "+>",
    "~&", "~|", "~^", "~<", "~>", "?&", "?|", "?^", "==", "!=", "<", ">", "<=", ">=", "<=>", "===",
    "=:=", "=>", "eqv", "eq", "ne", "lt", "gt", "le", "ge", "leg", "cmp", "~~", "min", "max",
    "gcd", "lcm", "and", "or", "not", ",", "after", "before", "X", "Z", "x", "xx", "&", "|", "^",
    "o", "∘",
];

/// Find the matching `]` for a `[` at position 0, respecting nesting.
fn find_matching_bracket(input: &str) -> Option<usize> {
    let mut depth = 0;
    for (i, c) in input.char_indices() {
        match c {
            '[' => depth += 1,
            ']' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Flatten nested bracket operator notation into a simple string.
/// e.g., "+" → "+", "R-" → "R-", "R[+]" → "R+", "[+]" → "+", "R[R[R-]]" → "RRR-"
fn flatten_bracket_op(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let first = s.as_bytes()[0];
    // Handle plain bracket: [op] → flatten(op)
    if first == b'['
        && let Some(end) = find_matching_bracket(s)
    {
        let inner = &s[1..end];
        return flatten_bracket_op(inner);
    }
    // Handle meta + bracket: R[op] → R + flatten(op)
    if (first == b'R' || first == b'Z' || first == b'X')
        && s.len() > 1
        && s.as_bytes()[1] == b'['
        && let Some(end) = find_matching_bracket(&s[1..])
    {
        let inner = &s[2..1 + end];
        let flattened_inner = flatten_bracket_op(inner);
        let rest = &s[1 + end + 1..];
        return format!("{}{}{}", first as char, flattened_inner, rest);
    }
    s.to_string()
}

/// Check if the given op (after flattening) is a valid reduction operator.
/// Handles R/Z/X meta-prefix chains by stripping them to find the base op.
fn is_valid_reduction_op(op: &str) -> bool {
    let mut s = op;
    // Strip meta prefixes while keeping bare operators like `X` intact.
    while s.len() > 1 {
        if let Some(rest) = s.strip_prefix('R') {
            s = rest;
        } else if let Some(rest) = s.strip_prefix('Z') {
            s = rest;
        } else if let Some(rest) = s.strip_prefix('X') {
            s = rest;
        } else {
            break;
        }
    }
    // Also support scan form (\op) and negated operators (!after),
    // while keeping operators that genuinely start with '!' (e.g. '!=').
    let s = s.strip_prefix('\\').unwrap_or(s);
    let s = if s == "∘" { "o" } else { s };
    if REDUCTION_OPS.contains(&s) {
        return true;
    }
    if let Some(stripped) = s.strip_prefix('!')
        && REDUCTION_OPS.contains(&stripped)
    {
        return true;
    }
    is_custom_reduction_op(s)
}

fn is_custom_reduction_op(op: &str) -> bool {
    if let Some(name) = op.strip_prefix('&') {
        return is_callable_reduction_name(name);
    }
    if let Ok((rest, _)) = parse_ident_with_hyphens(op) {
        return rest.is_empty();
    }
    false
}

fn is_callable_reduction_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    if (name.starts_with("infix:<")
        || name.starts_with("prefix:<")
        || name.starts_with("postfix:<")
        || name.starts_with("term:<"))
        && name.ends_with('>')
    {
        return true;
    }
    if let Ok((rest, _)) = parse_ident_with_hyphens(name) {
        return rest.is_empty();
    }
    false
}

/// Parse a reduction operator: [+], [*], [~], [min], [[+]], [R[+]], etc.
pub(super) fn reduction_op(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('[') {
        return Err(PError::expected("reduction operator"));
    }
    // Use bracket-matching to find the closing ] (supports nesting)
    let end =
        find_matching_bracket(input).ok_or_else(|| PError::expected("']' closing reduction"))?;
    let inner = &input[1..end];
    if inner.is_empty() {
        return Err(PError::expected("operator in reduction"));
    }
    // Flatten nested bracket notation: [[+]] → "+", [R[+]] → "R+"
    let op = match flatten_bracket_op(inner).as_str() {
        "∘" => "o".to_string(),
        other => other.to_string(),
    };
    // Only accept known operators (after flattening) to avoid confusion with array literals.
    if !is_valid_reduction_op(&op) {
        return Err(PError::expected("known reduction operator"));
    }
    let r = &input[end + 1..];
    let call_style_operand = r.starts_with('(') && is_custom_reduction_op(&op);
    // Must be followed by whitespace and an expression (not just `]`)
    if r.is_empty() || r.starts_with(';') || r.starts_with('}') || r.starts_with(')') {
        return Err(PError::expected("expression after reduction operator"));
    }
    let (r, _) = ws(r)?;
    // Parse comma-separated list as the operand
    let (r, first) = parse_reduction_operand(r)?;
    let mut items = vec![first];
    let mut rest = r;
    if !call_style_operand {
        loop {
            let (r, _) = ws_inner(rest);
            if !r.starts_with(',') {
                break;
            }
            let r = &r[1..];
            let (r, _) = ws_inner(r);
            // Stop at end-of-input, semicolon, closing brackets, or statement modifiers
            if r.is_empty()
                || r.starts_with(';')
                || r.starts_with('}')
                || r.starts_with(')')
                || r.starts_with(']')
            {
                rest = r;
                break;
            }
            if let Ok((r, next)) = parse_reduction_operand(r) {
                items.push(next);
                rest = r;
            } else {
                return Err(PError::expected_at(
                    "expression after ',' in reduction list",
                    r,
                ));
            }
        }
    }
    let expr = if items.len() == 1 {
        items.remove(0)
    } else {
        Expr::ArrayLiteral(items)
    };
    Ok((
        rest,
        Expr::Reduction {
            op,
            expr: Box::new(expr),
        },
    ))
}

fn parse_reduction_operand(input: &str) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    if let Some(r) = r.strip_prefix('|') {
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        return Ok((
            r,
            Expr::Call {
                name: "slip".to_string(),
                args: vec![expr],
            },
        ));
    }
    expression(r)
}

/// Parse colonpair expressions: :$var, :@var, :%var, :name(expr), :name, :!name
pub(in crate::parser) fn colonpair_expr(input: &str) -> PResult<'_, Expr> {
    let r = input
        .strip_prefix(':')
        .filter(|r| !r.starts_with(':'))
        .ok_or_else(|| PError::expected("colonpair"))?;
    // :(...): signature literal.
    if let Some(mut r) = r.strip_prefix('(') {
        let (r2, _) = ws(r)?;
        // Preferred path: parse using the full parameter list parser.
        if let Ok((r3, (param_defs, return_type))) =
            super::super::stmt::parse_param_list_with_return_pub(r2)
            && let Ok((r3, _)) = ws(r3)
            && let Ok((r3, _)) = parse_char(r3, ')')
        {
            let sig_info = param_defs_to_sig_info(&param_defs, return_type);
            return Ok((r3, Expr::Literal(make_signature_value(sig_info))));
        }

        // Fallback path: permissive parsing for legacy forms like :(:$a = True)
        // used in roast tests around Test::Assuming.
        r = r2;
        let mut items = Vec::new();
        if r.starts_with(')') {
            let (r, _) = parse_char(r, ')')?;
            let mut attrs = HashMap::new();
            attrs.insert("raku".to_string(), Value::Str(":()".to_string()));
            attrs.insert("perl".to_string(), Value::Str(":()".to_string()));
            attrs.insert("Str".to_string(), Value::Str(":()".to_string()));
            attrs.insert("gist".to_string(), Value::Str(":()".to_string()));
            return Ok((
                r,
                Expr::Literal(Value::make_instance("Signature".to_string(), attrs)),
            ));
        }
        loop {
            let mut item_input = r;
            if let Ok((after_type, _type_name)) = parse_ident_with_hyphens(r) {
                let (after_ws, _) = ws(after_type)?;
                if after_ws.starts_with(':') {
                    item_input = after_ws;
                }
            }
            let (r_item, mut item) = match colonpair_expr(item_input) {
                Ok(parsed) => parsed,
                Err(_) => match expression(item_input) {
                    Ok((r_expr, expr_item)) => {
                        let (r_chk, _) = ws(r_expr)?;
                        let valid_tail = r_chk.starts_with(',')
                            || r_chk.starts_with(')')
                            || keyword("is", r_chk).is_some()
                            || keyword("where", r_chk).is_some()
                            || r_chk.starts_with('=');
                        if valid_tail {
                            (r_expr, expr_item)
                        } else {
                            let (r_frag, frag) = parse_signature_fragment(item_input)?;
                            (r_frag, Expr::BareWord(frag))
                        }
                    }
                    Err(_) => {
                        let (r_frag, frag) = parse_signature_fragment(item_input)?;
                        (r_frag, Expr::BareWord(frag))
                    }
                },
            };
            let (mut r_next, _) = ws(r_item)?;
            while let Some(after_is) = keyword("is", r_next) {
                let (after_is, _) = ws(after_is)?;
                let (after_is, _trait_name) = parse_ident_with_hyphens(after_is)?;
                let (after_is, _) = ws(after_is)?;
                r_next = after_is;
            }
            if let Some(after_where) = keyword("where", r_next) {
                let (after_where, _) = ws(after_where)?;
                let (after_where, _constraint) = expression(after_where)?;
                let (after_where, _) = ws(after_where)?;
                r_next = after_where;
            }
            if let Some(after_eq) = r_next.strip_prefix('=')
                && let Expr::Binary { left, op, .. } = &item
                && *op == crate::token_kind::TokenKind::FatArrow
            {
                let (after_eq, _) = ws(after_eq)?;
                let (after_eq, value_expr) = expression(after_eq)?;
                item = Expr::Binary {
                    left: left.clone(),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(value_expr),
                };
                let (after_eq, _) = ws(after_eq)?;
                r_next = after_eq;
            }
            items.push(item);
            if r_next.starts_with(',') {
                let (r_more, _) = parse_char(r_next, ',')?;
                let (r_more, _) = ws(r_more)?;
                r = r_more;
                continue;
            }
            let (r_end, _) = parse_char(r_next, ')')?;
            let rendered = items
                .iter()
                .map(render_signature_item)
                .collect::<Vec<_>>()
                .join(", ");
            let sig = format!(":({})", rendered);
            let mut attrs = HashMap::new();
            attrs.insert("raku".to_string(), Value::Str(sig.clone()));
            attrs.insert("perl".to_string(), Value::Str(sig.clone()));
            attrs.insert("Str".to_string(), Value::Str(sig.clone()));
            attrs.insert("gist".to_string(), Value::Str(sig));
            return Ok((
                r_end,
                Expr::Literal(Value::make_instance("Signature".to_string(), attrs)),
            ));
        }
    }
    // :123name (numeric leading-value pair) => :name(123)
    let digit_end = r.find(|c: char| !c.is_ascii_digit()).unwrap_or(r.len());
    if digit_end > 0 && digit_end < r.len() {
        let digits = &r[..digit_end];
        let after_digits = &r[digit_end..];
        if let Ok((rest, name)) = parse_ident_with_hyphens(after_digits)
            && let Ok(n) = digits.parse::<i64>()
        {
            return Ok((
                rest,
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(Expr::Literal(Value::Int(n))),
                },
            ));
        }
    }
    // :!name (negated boolean pair)
    if let Some(after_bang) = r.strip_prefix('!') {
        let (rest, name) = parse_ident_with_hyphens(after_bang)?;
        return Ok((
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::Literal(Value::Bool(false))),
            },
        ));
    }
    // :$var / :@var / :%var (autopair from variable)
    if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
        let sigil = &r[..1];
        let after_sigil = &r[1..];
        let (mut rest, var_name) = parse_ident_with_hyphens(after_sigil)?;
        if rest.starts_with('?') || rest.starts_with('!') {
            rest = &rest[1..];
        }
        let var_expr = match sigil {
            "$" => Expr::Var(var_name.to_string()),
            "@" => Expr::ArrayVar(var_name.to_string()),
            "%" => Expr::HashVar(var_name.to_string()),
            _ => unreachable!(),
        };
        return Ok((
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(var_name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(var_expr),
            },
        ));
    }
    // :name(expr) or :name (boolean true pair)
    let (mut rest, name) = parse_ident_with_hyphens(r)?;
    if rest.starts_with('?') || rest.starts_with('!') {
        rest = &rest[1..];
    }
    // Don't parse statement-modifier keywords as colonpairs
    if matches!(
        name,
        "if" | "unless" | "for" | "while" | "until" | "given" | "when"
    ) {
        return Err(PError::expected("colonpair name"));
    }
    if let Some(after_paren) = rest.strip_prefix('(') {
        let (r, _) = ws(after_paren)?;
        let (r, first) = expression(r)?;
        let (r, _) = ws(r)?;
        // Check for separated list: :name(a, b, ...) or :name(a; b; ...)
        if r.starts_with(',') || (r.starts_with(';') && !r.starts_with(";;")) {
            let mut items = vec![first];
            let mut r = r;
            while r.starts_with(',') || (r.starts_with(';') && !r.starts_with(";;")) {
                let sep = if r.starts_with(',') { ',' } else { ';' };
                let (r2, _) = parse_char(r, sep)?;
                let (r2, _) = ws(r2)?;
                let (r2, next) = expression(r2)?;
                let (r2, _) = ws(r2)?;
                items.push(next);
                r = r2;
            }
            let (r, _) = parse_char(r, ')')?;
            return Ok((
                r,
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(Expr::ArrayLiteral(items)),
                },
            ));
        }
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(first),
            },
        ));
    }
    // :name[items] (array-valued colonpair)
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let mut items = Vec::new();
        let mut r = r;
        while !r.starts_with(']') {
            let (r2, item) = expression(r)?;
            items.push(item);
            let (r2, _) = ws(r2)?;
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
        }
        let (r, _) = parse_char(r, ']')?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::ArrayLiteral(items)),
            },
        ));
    }
    // :name{ ... } (block-valued colonpair)
    if rest.starts_with('{') {
        let (r, body) = parse_block_body(rest)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::AnonSub(body)),
            },
        ));
    }
    // :name<value> (angle-bracket colonpair, equivalent to :name("value"))
    if rest.starts_with('<') && !rest.starts_with("<<") && !rest.starts_with("<=") {
        let inner = &rest[1..];
        if let Some(close) = inner.find('>') {
            let content = &inner[..close];
            let r = &inner[close + 1..];
            let words = split_angle_words(content);
            if !words.is_empty() {
                let val_expr = if words.len() == 1 {
                    Expr::Literal(Value::Str(words[0].to_string()))
                } else {
                    Expr::ArrayLiteral(
                        words
                            .into_iter()
                            .map(|w| Expr::Literal(Value::Str(w.to_string())))
                            .collect(),
                    )
                };
                return Ok((
                    r,
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                        op: crate::token_kind::TokenKind::FatArrow,
                        right: Box::new(val_expr),
                    },
                ));
            }
        }
    }
    // :name (boolean true)
    Ok((
        rest,
        Expr::Binary {
            left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::Bool(true))),
        },
    ))
}

fn render_signature_item(expr: &Expr) -> String {
    match expr {
        Expr::BareWord(name) => name.clone(),
        Expr::Var(name) => format!("${}", name),
        Expr::ArrayVar(name) => format!("@{}", name),
        Expr::HashVar(name) => format!("%{}", name),
        Expr::Binary { left, op, right } if *op == crate::token_kind::TokenKind::FatArrow => {
            if let Expr::Literal(Value::Str(name)) = left.as_ref() {
                match right.as_ref() {
                    Expr::Var(v) if v == name => format!(":${}", name),
                    Expr::Literal(Value::Bool(true)) => format!(":${}", name),
                    other => format!(":${} = {}", name, render_signature_item(other)),
                }
            } else {
                "...".to_string()
            }
        }
        Expr::Literal(v) => match v {
            Value::Str(s) => format!("\"{}\"", s.replace('"', "\\\"")),
            _ => v.to_string_value(),
        },
        Expr::AnonSub(_) => "{ ... }".to_string(),
        _ => "...".to_string(),
    }
}

fn parse_signature_fragment(input: &str) -> PResult<'_, String> {
    let mut depth_paren = 0usize;
    let mut depth_bracket = 0usize;
    let mut depth_brace = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;
    for (idx, ch) in input.char_indices() {
        if in_single {
            if ch == '\'' && !escape {
                in_single = false;
            }
            escape = ch == '\\' && !escape;
            continue;
        }
        if in_double {
            if ch == '"' && !escape {
                in_double = false;
            }
            escape = ch == '\\' && !escape;
            continue;
        }
        match ch {
            '\'' => in_single = true,
            '"' => in_double = true,
            '(' => depth_paren += 1,
            ')' => {
                if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 {
                    let frag = input[..idx].trim();
                    if frag.is_empty() {
                        return Err(PError::expected("signature item"));
                    }
                    return Ok((&input[idx..], frag.to_string()));
                }
                depth_paren = depth_paren.saturating_sub(1);
            }
            '[' => depth_bracket += 1,
            ']' => depth_bracket = depth_bracket.saturating_sub(1),
            '{' => depth_brace += 1,
            '}' => depth_brace = depth_brace.saturating_sub(1),
            ',' if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 => {
                let frag = input[..idx].trim();
                if frag.is_empty() {
                    return Err(PError::expected("signature item"));
                }
                return Ok((&input[idx..], frag.to_string()));
            }
            _ => {}
        }
    }
    let frag = input.trim();
    if frag.is_empty() {
        return Err(PError::expected("signature item"));
    }
    Ok(("", frag.to_string()))
}

/// Parse capture literals: `\(...)` and `\expr`.
pub(super) fn capture_literal(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('\\') {
        return Err(PError::expected("capture literal"));
    }
    let r = &input[1..]; // skip backslash
    if !r.starts_with('(') {
        let (r, item) = super::super::expr::expression(r)?;
        return Ok((r, Expr::CaptureLiteral(vec![item])));
    }
    // Parenthesized capture literal: \(a, b, :c)
    let (r, _) = parse_char(r, '(')?;
    let (r, _) = ws(r)?;
    let (r, items) = super::parse_call_arg_list(r)?;
    let (r, _) = ws(r)?;
    let (r, _) = parse_char(r, ')')?;
    Ok((r, Expr::CaptureLiteral(items)))
}

/// Parse `-> $param { body }` or `-> $a, $b { body }` arrow lambda.
pub(super) fn arrow_lambda(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("->") {
        return Err(PError::expected("arrow lambda"));
    }
    let r = &input[2..];
    let (r, _) = ws(r)?;
    // Zero-param pointed block: -> { body }
    if r.starts_with('{') {
        let (r, body) = parse_block_body(r)?;
        return Ok((r, Expr::AnonSub(body)));
    }
    // Sub-signature destructuring: -> (:key($var), :value($var2)) { body }
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::super::stmt::parse_param_list_pub(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, return_type) = skip_pointy_return_type(r)?;
        let (r, body) = parse_block_body(r)?;
        let params: Vec<String> = sub_params.iter().map(|p| p.name.clone()).collect();
        return Ok((
            r,
            Expr::AnonSubParams {
                params,
                param_defs: sub_params,
                return_type,
                body,
            },
        ));
    }
    // Parse params
    let (r, first) = super::super::stmt::parse_pointy_param_pub(r)?;
    let (r, _) = ws(r)?;
    if r.starts_with(',') {
        // Multi-param: -> $a, $b { body }
        let mut param_defs = vec![first];
        let mut r = r;
        loop {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            let (r2, next) = super::super::stmt::parse_pointy_param_pub(r2)?;
            param_defs.push(next);
            let (r2, _) = ws(r2)?;
            if !r2.starts_with(',') {
                r = r2;
                break;
            }
            r = r2;
        }
        let (r, return_type) = skip_pointy_return_type(r)?;
        let (r, body) = parse_block_body(r)?;
        let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
        Ok((
            r,
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
            },
        ))
    } else {
        // Single param: -> $n { body }
        let (r, return_type) = skip_pointy_return_type(r)?;
        let (r, body) = parse_block_body(r)?;
        let simple_single = first.traits.is_empty();
        if simple_single {
            Ok((
                r,
                Expr::Lambda {
                    param: first.name,
                    body,
                },
            ))
        } else {
            Ok((
                r,
                Expr::AnonSubParams {
                    params: vec![first.name.clone()],
                    param_defs: vec![first],
                    return_type,
                    body,
                },
            ))
        }
    }
}

/// Parse a block `{ stmts }` as AnonSub or `{}` / `{ key => val, ... }` as Hash.
pub(super) fn block_or_hash_expr(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('{') {
        return Err(PError::expected("block or hash"));
    }
    let r = &input[1..];
    let (r, _) = ws_inner(r);

    // Empty hash: {}
    if let Some(rest) = r.strip_prefix('}') {
        return Ok((rest, Expr::Hash(Vec::new())));
    }

    // Try to detect if this is a hash literal: { key => val, ... }
    // Heuristic: if after ws we see `ident =>` or `"str" =>` or `'str' =>`, it's a hash
    if is_hash_literal_start(r) {
        return parse_hash_literal_body(r);
    }

    // Otherwise parse as a block (anonymous sub)
    crate::parser::stmt::simple::push_scope();
    let result = (|| -> PResult<'_, Expr> {
        let (r, stmts) = super::super::stmt::stmt_list_pub(r)?;
        let (r, _) = ws_inner(r);
        if !r.starts_with('}') {
            return Err(PError::expected("'}'"));
        }
        let r = &r[1..];
        Ok((r, make_anon_sub(stmts)))
    })();
    crate::parser::stmt::simple::pop_scope();
    result
}

/// Simple whitespace consumer that doesn't use PResult (infallible).
pub(super) fn ws_inner(input: &str) -> (&str, ()) {
    match super::super::helpers::ws(input) {
        Ok((r, _)) => (r, ()),
        Err(_) => (input, ()),
    }
}

/// Parse a block body: { stmts }
pub(in crate::parser) fn parse_block_body(input: &str) -> PResult<'_, Vec<crate::ast::Stmt>> {
    let (r, _) = parse_char(input, '{')?;
    crate::parser::stmt::simple::push_scope();
    let result = (|| -> PResult<'_, Vec<crate::ast::Stmt>> {
        let (r, stmts) = super::super::stmt::stmt_list_pub(r)?;
        let (r, _) = ws_inner(r);
        let (r, _) = parse_char(r, '}')?;
        Ok((r, stmts))
    })();
    crate::parser::stmt::simple::pop_scope();
    result
}

/// Check if the input looks like a hash literal start.
fn is_hash_literal_start(input: &str) -> bool {
    // ident => or "str" => or 'str' =>
    if let Ok((r, _)) = super::super::stmt::ident_pub(input) {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    // Quoted key => val
    if (input.starts_with('"') || input.starts_with('\''))
        && let Ok((r, _)) = single_quoted_string(input).or_else(|_| double_quoted_string(input))
    {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    // Colon pair: :name(expr) or :name or :!name or :Nname — indicates a hash literal
    if input.starts_with(':') && !input.starts_with("::") {
        let r = &input[1..];
        // :!name
        if r.starts_with('!') && super::super::stmt::ident_pub(&r[1..]).is_ok() {
            return true;
        }
        // :Nname — numeric colon pair (e.g., :1status)
        if let Some(first) = r.chars().next()
            && first.is_ascii_digit()
        {
            let digit_end = r.find(|c: char| !c.is_ascii_digit()).unwrap_or(r.len());
            if digit_end < r.len() && super::super::stmt::ident_pub(&r[digit_end..]).is_ok() {
                return true;
            }
        }
        // :$var / :@var / :%var
        if (r.starts_with('$') || r.starts_with('@') || r.starts_with('%'))
            && super::super::stmt::ident_pub(&r[1..]).is_ok()
        {
            return true;
        }
        // :name or :name(expr) or :name[expr]
        if let Ok((_r, name)) = super::super::stmt::ident_pub(r)
            && !matches!(
                name.as_str(),
                "my" | "our"
                    | "has"
                    | "if"
                    | "unless"
                    | "for"
                    | "while"
                    | "until"
                    | "loop"
                    | "given"
                    | "when"
                    | "return"
            )
            && !name.starts_with(|c: char| c.is_ascii_digit())
        {
            return true;
        }
    }
    false
}

static ANON_CLASS_COUNTER: AtomicU64 = AtomicU64::new(0);
static ANON_ROLE_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Parse an anonymous class expression: `class { ... }`
pub(super) fn anon_class_expr(input: &str) -> PResult<'_, Expr> {
    let rest = keyword("class", input).ok_or_else(|| PError::expected("anonymous class"))?;
    let (rest, _) = ws(rest)?;
    // Must be followed by '{' (no name) to be an anonymous class
    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous class"));
    }
    let id = ANON_CLASS_COUNTER.fetch_add(1, Ordering::Relaxed);
    let name = format!("__ANON_CLASS_{id}__");
    let (rest, body) = parse_block_body(rest)?;
    Ok((
        rest,
        Expr::DoStmt(Box::new(Stmt::ClassDecl {
            name,
            name_expr: None,
            parents: Vec::new(),
            body,
        })),
    ))
}

/// Parse an anonymous role expression: `role { ... }`
pub(super) fn anon_role_expr(input: &str) -> PResult<'_, Expr> {
    let rest = keyword("role", input).ok_or_else(|| PError::expected("anonymous role"))?;
    let (rest, _) = ws(rest)?;
    // Must be followed by '{' (no name) to be an anonymous role
    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous role"));
    }
    let id = ANON_ROLE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let name = format!("__ANON_ROLE_{id}__");
    let (rest, body) = parse_block_body(rest)?;
    Ok((
        rest,
        Expr::DoStmt(Box::new(Stmt::RoleDecl {
            name,
            type_params: Vec::new(),
            body,
        })),
    ))
}

/// Parse hash literal body: key => val, :name(val), ... }
fn parse_hash_literal_body(input: &str) -> PResult<'_, Expr> {
    let mut pairs = Vec::new();
    let mut rest = input;
    loop {
        let (r, _) = ws_inner(rest);
        if let Some(rest) = r.strip_prefix('}') {
            return Ok((rest, Expr::Hash(pairs)));
        }

        // Try colon pair syntax: :name(expr), :name, :!name, :$var, etc.
        if r.starts_with(':') && !r.starts_with("::") {
            let (r, (key, val)) = parse_colon_pair_entry(r)?;
            pairs.push((key, val));
            let (r, _) = ws_inner(r);
            if let Some(stripped) = r.strip_prefix(',') {
                rest = stripped;
            } else if let Some(stripped) = r.strip_prefix(';') {
                rest = stripped;
            } else {
                rest = r;
            }
            continue;
        }

        // Parse key as identifier or string
        let (r, key) = if let Ok((r, name)) = super::super::stmt::ident_pub(r) {
            (r, name)
        } else if let Ok((r, Expr::Literal(Value::Str(s)))) =
            single_quoted_string(r).or_else(|_| double_quoted_string(r))
        {
            (r, s)
        } else {
            return Err(PError::expected("hash key"));
        };
        let (r, _) = ws_inner(r);
        // Expect =>
        if !r.starts_with("=>") {
            return Err(PError::expected("'=>' in hash literal"));
        }
        let r = &r[2..];
        let (r, _) = ws_inner(r);
        let (r, val) = super::super::expr::expression(r)?;
        pairs.push((key, Some(val)));
        let (r, _) = ws_inner(r);
        if let Some(stripped) = r.strip_prefix(',') {
            rest = stripped;
        } else if let Some(stripped) = r.strip_prefix(';') {
            rest = stripped;
        } else {
            rest = r;
        }
    }
}

/// Parse a colon pair entry inside a hash literal: :name(expr), :name, :!name, :Nname
fn parse_colon_pair_entry(input: &str) -> PResult<'_, (String, Option<Expr>)> {
    let r = input
        .strip_prefix(':')
        .ok_or_else(|| PError::expected("':'"))?;

    // :!name
    if let Some(r) = r.strip_prefix('!') {
        let (r, name) = super::super::stmt::ident_pub(r)?;
        return Ok((r, (name, Some(Expr::Literal(Value::Bool(false))))));
    }

    // :Nname — numeric colon pair, e.g., :1status means status => 1
    if let Some(first) = r.chars().next()
        && first.is_ascii_digit()
    {
        let digit_end = r.find(|c: char| !c.is_ascii_digit()).unwrap_or(r.len());
        let digits = &r[..digit_end];
        let after_digits = &r[digit_end..];
        if let Ok((r, name)) = super::super::stmt::ident_pub(after_digits) {
            let num: i64 = digits.parse().unwrap_or(0);
            return Ok((r, (name, Some(Expr::Literal(Value::Int(num))))));
        }
    }

    // :$var / :@var / :%var
    if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
        let sigil = &r[..1];
        let (r, name) = super::super::stmt::ident_pub(&r[1..])?;
        let expr = match sigil {
            "$" => Expr::Var(name.clone()),
            "@" => Expr::ArrayVar(name.clone()),
            "%" => Expr::HashVar(name.clone()),
            _ => unreachable!(),
        };
        return Ok((r, (name, Some(expr))));
    }

    // :name or :name(expr) or :name[items]
    let (r, name) = super::super::stmt::ident_pub(r)?;

    // :name(expr) or :name(expr, expr, ...)
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws_inner(r);
        let (r, first) = super::super::expr::expression(r)?;
        let (r, _) = ws_inner(r);
        if r.starts_with(',') {
            let mut items = vec![first];
            let mut r = r;
            while r.starts_with(',') {
                let (r2, _) = parse_char(r, ',')?;
                let (r2, _) = ws_inner(r2);
                let (r2, next) = super::super::expr::expression(r2)?;
                let (r2, _) = ws_inner(r2);
                items.push(next);
                r = r2;
            }
            let (r, _) = parse_char(r, ')')?;
            return Ok((r, (name, Some(Expr::ArrayLiteral(items)))));
        }
        let (r, _) = parse_char(r, ')')?;
        return Ok((r, (name, Some(first))));
    }

    // :name[items]
    if r.starts_with('[') {
        let (r, _) = parse_char(r, '[')?;
        let (r, _) = ws_inner(r);
        let mut items = Vec::new();
        let mut r = r;
        while !r.starts_with(']') {
            let (r2, item) = super::super::expr::expression(r)?;
            items.push(item);
            let (r2, _) = ws_inner(r2);
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws_inner(r2);
                r = r2;
            } else {
                r = r2;
            }
        }
        let (r, _) = parse_char(r, ']')?;
        return Ok((r, (name, Some(Expr::ArrayLiteral(items)))));
    }

    // :name{block} (block-valued colonpair)
    if r.starts_with('{') {
        let (r, body) = parse_block_body(r)?;
        return Ok((r, (name, Some(Expr::AnonSub(body)))));
    }

    // :name<word> or :name<words> (angle bracket form)
    if r.starts_with('<') && !r.starts_with("<<") {
        let (r, _) = parse_char(r, '<')?;
        let end = r
            .find('>')
            .ok_or_else(|| PError::expected("'>' closing angle bracket"))?;
        let content = &r[..end];
        let r = &r[end + 1..];
        let words = split_angle_words(content);
        if words.len() == 1 {
            return Ok((
                r,
                (name, Some(Expr::Literal(Value::Str(words[0].to_string())))),
            ));
        }
        let items = words
            .iter()
            .map(|w| Expr::Literal(Value::Str(w.to_string())))
            .collect();
        return Ok((r, (name, Some(Expr::ArrayLiteral(items)))));
    }

    // :name (boolean true)
    Ok((r, (name, None)))
}
