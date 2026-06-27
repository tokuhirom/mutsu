use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::parse_result::PError;
use crate::value::Value;

/// Assemble interpolation parts into a final expression.
pub(crate) fn finalize_interpolation(parts: Vec<Expr>, current: String) -> Expr {
    if parts.is_empty() {
        Expr::Literal(Value::str(current))
    } else {
        let mut parts = parts;
        if !current.is_empty() {
            parts.push(Expr::Literal(Value::str(current)));
        }
        if parts.len() == 1 && matches!(&parts[0], Expr::Literal(Value::Str(_))) {
            return parts.into_iter().next().unwrap();
        }
        Expr::StringInterpolation(parts)
    }
}

/// Interpolate variables in string content (used by qq// etc.)
pub(crate) fn interpolate_string_content(content: &str) -> Expr {
    interpolate_string_content_with_modes(content, true, false)
}

pub(crate) fn interpolate_string_content_with_modes(
    content: &str,
    interpolate_vars: bool,
    interpolate_closures: bool,
) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
            match process_escape_sequence(rest, &mut current, &[]) {
                Ok(Some((r, needs_continue))) => {
                    rest = r;
                    if needs_continue {
                        continue;
                    }
                }
                Ok(None) | Err(_) => {
                    let c = rest.as_bytes()[1] as char;
                    current.push('\\');
                    current.push(c);
                    rest = &rest[2..];
                }
            }
            continue;
        }
        if interpolate_closures
            && rest.starts_with('{')
            && let Some((after, inner)) = parse_braced_interpolation(rest)
            && let Ok((remaining, expr)) = expression(inner.trim())
            && remaining.trim().is_empty()
        {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(&mut current))));
            }
            parts.push(expr);
            rest = after;
            continue;
        }
        if interpolate_vars && let Some(r) = try_interpolate_var(rest, &mut parts, &mut current) {
            rest = r;
            continue;
        }
        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    finalize_interpolation(parts, current)
}

pub(crate) fn parse_braced_interpolation(input: &str) -> Option<(&str, &str)> {
    if !input.starts_with('{') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, ch) in input.char_indices() {
        if ch == '{' {
            depth += 1;
        } else if ch == '}' {
            depth -= 1;
            if depth == 0 {
                let inner = &input[1..idx];
                let after = &input[idx + 1..];
                return Some((after, inner));
            }
        }
    }
    None
}

pub(crate) fn parse_single_quote_qq(content: &str) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        if let Some(after_qq) = rest.strip_prefix("\\qq")
            && let Some(open) = after_qq.chars().next()
            && !open.is_alphanumeric()
            && !open.is_whitespace()
        {
            let parsed = if let Some(close) = unicode_bracket_close(open) {
                read_bracketed(after_qq, open, close, true)
                    .map(|(after, inner)| (after, interpolate_string_content(inner)))
            } else {
                let body = &after_qq[open.len_utf8()..];
                body.find(open)
                    .map(|end| {
                        let inner = &body[..end];
                        let after = &body[end + open.len_utf8()..];
                        (after, interpolate_string_content(inner))
                    })
                    .ok_or_else(|| PError::expected("closing qq delimiter"))
            };
            if let Ok((after, interpolated)) = parsed {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(&mut current))));
                }
                parts.push(interpolated);
                rest = after;
                continue;
            }
        }

        if let Some(after_backslash) = rest.strip_prefix('\\')
            && let Some(next) = after_backslash.chars().next()
        {
            if next == '\'' || next == '\\' {
                current.push(next);
            } else {
                current.push('\\');
                current.push(next);
            }
            rest = &after_backslash[next.len_utf8()..];
            continue;
        }

        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    finalize_interpolation(parts, current)
}
