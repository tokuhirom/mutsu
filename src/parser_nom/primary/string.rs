use super::super::parse_result::{PError, PResult, parse_char};

use crate::ast::Expr;
use crate::token_kind::lookup_unicode_char_by_name;
use crate::value::Value;

use super::super::expr::expression;
use super::var::parse_var_name_from_str;

/// Read a bracketed string with nesting support (e.g., `{...{...}...}`)
pub(super) fn read_bracketed(input: &str, open: char, close: char) -> PResult<'_, &str> {
    if !input.starts_with(open) {
        return Err(PError::expected(&format!("'{}'", open)));
    }
    let mut rest = &input[open.len_utf8()..];
    let start = rest;
    let mut depth = 1u32;
    loop {
        if rest.is_empty() {
            return Err(PError::expected(&format!("closing '{}'", close)));
        }
        let ch = rest.chars().next().unwrap();
        if ch == '\\' && rest.len() > 1 {
            rest = &rest[2..]; // skip escape
            continue;
        }
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                let content = &start[..start.len() - rest.len()];
                return Ok((&rest[close.len_utf8()..], content));
            }
        }
        rest = &rest[ch.len_utf8()..];
    }
}

/// Parse q{...}, q[...], q(...), q<...>, q/.../ quoting forms.
pub(super) fn q_string(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('q') {
        return Err(PError::expected("q string"));
    }
    let after_q = &input[1..];

    // q:to/DELIM/ or qq:to/DELIM/ heredoc
    if after_q.starts_with(":to/") || after_q.starts_with("q:to/") {
        let (r, _is_qq) = if let Some(stripped) = after_q.strip_prefix("q:to/") {
            (stripped, true)
        } else if let Some(stripped) = after_q.strip_prefix(":to/") {
            (stripped, false)
        } else {
            unreachable!()
        };
        // Find the delimiter name (e.g. END)
        let delim_end = r
            .find('/')
            .ok_or_else(|| PError::expected("closing / for heredoc delimiter"))?;
        let delimiter = &r[..delim_end];
        let r = &r[delim_end + 1..];
        // In Raku, heredoc content starts on the NEXT line.
        // The rest of the current line (after q:to/DELIM/) continues as normal code.
        // Find the next newline to split current-line remainder from heredoc body.
        let (rest_of_line, heredoc_start) = if let Some(nl) = r.find('\n') {
            (&r[..nl], &r[nl + 1..])
        } else {
            // No newline after heredoc declaration — no heredoc body
            return Err(PError::expected("heredoc body after newline"));
        };
        // Find the terminator line in the heredoc body
        let mut content_end = None;
        let mut search_pos = 0;
        while search_pos <= heredoc_start.len() {
            if heredoc_start[search_pos..].starts_with(delimiter) {
                let after_delim = &heredoc_start[search_pos + delimiter.len()..];
                if after_delim.is_empty()
                    || after_delim.starts_with('\n')
                    || after_delim.starts_with('\r')
                    || after_delim.starts_with(';')
                {
                    content_end = Some(search_pos);
                    break;
                }
            }
            if let Some(nl) = heredoc_start[search_pos..].find('\n') {
                search_pos += nl + 1;
            } else {
                break;
            }
        }
        if let Some(end) = content_end {
            let content = &heredoc_start[..end];
            let after_terminator = &heredoc_start[end + delimiter.len()..];
            // Skip optional newline after terminator
            let after_terminator = after_terminator
                .strip_prefix('\n')
                .unwrap_or(after_terminator);
            // Return rest_of_line + after_terminator as remaining input
            // so the caller can continue parsing the current line's arguments.
            // We need to concatenate, but since we can't create new string slices
            // from two disjoint parts, we check if rest_of_line is empty first.
            if rest_of_line.trim().is_empty() || rest_of_line.trim() == ";" {
                return Ok((
                    after_terminator,
                    Expr::Literal(Value::Str(content.to_string())),
                ));
            }
            // rest_of_line has content (e.g. ", 'description';") — we need to
            // return it as remaining input. Since we can't concatenate slices,
            // we use the fact that rest_of_line and after_terminator are part of
            // the same original string. Build a combined view by finding the
            // rest_of_line followed by after_terminator.
            // Strategy: create a new string and leak it to get a &'static str.
            // This is acceptable since parser runs once per program.
            let combined = format!("{}\n{}", rest_of_line, after_terminator);
            let leaked: &'static str = Box::leak(combined.into_boxed_str());
            return Ok((leaked, Expr::Literal(Value::Str(content.to_string()))));
        }
        return Err(PError::expected("heredoc terminator"));
    }

    // Check for qq forms
    let (after_prefix, is_qq) = if let Some(after_qq) = after_q.strip_prefix('q') {
        if after_qq.starts_with('{')
            || after_qq.starts_with('[')
            || after_qq.starts_with('(')
            || after_qq.starts_with('<')
            || after_qq.starts_with('/')
        {
            (after_qq, true)
        } else {
            (after_q, false)
        }
    } else {
        (after_q, false)
    };
    // Must be followed by a delimiter
    let (open, close) = match after_prefix.chars().next() {
        Some('{') => ('{', '}'),
        Some('[') => ('[', ']'),
        Some('(') => ('(', ')'),
        Some('<') => ('<', '>'),
        Some('/') => {
            // q/.../ — find closing /
            let rest = &after_prefix[1..];
            let end = rest
                .find('/')
                .ok_or_else(|| PError::expected("closing /"))?;
            let content = &rest[..end];
            let rest = &rest[end + 1..];
            if is_qq {
                return Ok((rest, interpolate_string_content(content)));
            }
            let s = content.replace("\\'", "'").replace("\\\\", "\\");
            return Ok((rest, Expr::Literal(Value::Str(s))));
        }
        _ => return Err(PError::expected("q string delimiter")),
    };
    let (rest, content) = read_bracketed(after_prefix, open, close)?;
    if is_qq {
        return Ok((rest, interpolate_string_content(content)));
    }
    let s = content.replace("\\'", "'").replace("\\\\", "\\");
    Ok((rest, Expr::Literal(Value::Str(s))))
}

/// Process an escape sequence starting at `rest` (which begins with `\`).
/// `extra_escapes` lists additional simple single-char escapes (e.g., `'"'`, `'{'`).
/// Returns `Some((remaining_input, true))` if a continuation-style escape was handled
/// (caller should `continue`), or `Some((remaining_input, false))` for simple escapes
/// (caller should advance past `\c`), or `None` if the escape char is unknown (caller
/// pushes `\` + char and advances).
pub(super) fn process_escape_sequence<'a>(
    rest: &'a str,
    current: &mut String,
    extra_escapes: &[char],
) -> Option<(&'a str, bool)> {
    let c = rest.as_bytes()[1] as char;
    match c {
        'n' => current.push('\n'),
        't' => current.push('\t'),
        'r' => current.push('\r'),
        '0' => current.push('\0'),
        '\\' => current.push('\\'),
        '$' => current.push('$'),
        '@' => current.push('@'),
        'x' => {
            let r = &rest[2..];
            if r.starts_with('[') {
                if let Some(end) = r.find(']') {
                    let hex = &r[1..end];
                    if let Ok(n) = u32::from_str_radix(hex, 16)
                        && let Some(ch) = char::from_u32(n)
                    {
                        current.push(ch);
                    }
                    return Some((&r[end + 1..], true));
                }
            } else {
                let hex_chars: String = r.chars().take_while(|ch| ch.is_ascii_hexdigit()).collect();
                let len = hex_chars.len();
                if let Ok(n) = u32::from_str_radix(&hex_chars, 16)
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
                }
                return Some((&r[len..], true));
            }
            return Some((r, true));
        }
        'o' => {
            let r = &rest[2..];
            if r.starts_with('[') {
                if let Some(end) = r.find(']') {
                    let oct = &r[1..end];
                    if let Ok(n) = u32::from_str_radix(oct, 8)
                        && let Some(ch) = char::from_u32(n)
                    {
                        current.push(ch);
                    }
                    return Some((&r[end + 1..], true));
                }
            } else {
                let oct_chars: String =
                    r.chars().take_while(|ch| matches!(ch, '0'..='7')).collect();
                let len = oct_chars.len();
                if let Ok(n) = u32::from_str_radix(&oct_chars, 8)
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
                }
                return Some((&r[len..], true));
            }
            return Some((r, true));
        }
        'c' => {
            let r = &rest[2..];
            if r.starts_with('[')
                && let Some((s, after)) = parse_backslash_c_bracket(&r[1..])
            {
                current.push_str(&s);
                return Some((after, true));
            }
            return Some((r, true));
        }
        _ => {
            if extra_escapes.contains(&c) {
                current.push(c);
            } else {
                return None;
            }
        }
    }
    Some((&rest[2..], false))
}

/// Try to interpolate a `$var` or `@var` at the current position.
/// Returns `Some(remaining_input)` if interpolation was performed, `None` otherwise.
pub(super) fn try_interpolate_var<'a>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
) -> Option<&'a str> {
    if rest.starts_with('$') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        if next.is_alphabetic() || next == '_' || next == '*' || next == '?' || next == '!' {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::Str(std::mem::take(current))));
            }
            let var_rest = &rest[1..];
            let (var_rest, var_name) = parse_var_name_from_str(var_rest);
            parts.push(Expr::Var(var_name));
            return Some(var_rest);
        }
    }
    if rest.starts_with('@') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        if next.is_alphabetic() || next == '_' {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::Str(std::mem::take(current))));
            }
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(var_rest.len());
            let name = &var_rest[..end];
            parts.push(Expr::ArrayVar(name.to_string()));
            return Some(&var_rest[end..]);
        }
    }
    None
}

/// Assemble interpolation parts into a final expression.
pub(super) fn finalize_interpolation(parts: Vec<Expr>, current: String) -> Expr {
    if parts.is_empty() {
        Expr::Literal(Value::Str(current))
    } else {
        let mut parts = parts;
        if !current.is_empty() {
            parts.push(Expr::Literal(Value::Str(current)));
        }
        if parts.len() == 1 && matches!(&parts[0], Expr::Literal(Value::Str(_))) {
            return parts.into_iter().next().unwrap();
        }
        Expr::StringInterpolation(parts)
    }
}

/// Interpolate variables in string content (used by qq// etc.)
pub(super) fn interpolate_string_content(content: &str) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
            if let Some((r, needs_continue)) = process_escape_sequence(rest, &mut current, &[]) {
                rest = r;
                if needs_continue {
                    continue;
                }
            } else {
                let c = rest.as_bytes()[1] as char;
                current.push('\\');
                current.push(c);
                rest = &rest[2..];
            }
            continue;
        }
        if let Some(r) = try_interpolate_var(rest, &mut parts, &mut current) {
            rest = r;
            continue;
        }
        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    finalize_interpolation(parts, current)
}

pub(super) fn single_quoted_string(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '\'')?;
    let start = input;
    let mut rest = input;
    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing '"));
        }
        if let Some(after_quote) = rest.strip_prefix('\'') {
            let content = &start[..start.len() - rest.len()];
            let s = content.replace("\\'", "'").replace("\\\\", "\\");
            return Ok((after_quote, Expr::Literal(Value::Str(s))));
        }
        if rest.starts_with('\\') && rest.len() > 1 {
            // Skip backslash + next character (which may be multi-byte)
            let after_backslash = &rest[1..];
            let next_ch = after_backslash.chars().next().unwrap();
            rest = &after_backslash[next_ch.len_utf8()..];
        } else {
            let ch = rest.chars().next().unwrap();
            rest = &rest[ch.len_utf8()..];
        }
    }
}

/// Parse corner bracket string literal: ｢...｣ (no interpolation)
pub(super) fn corner_bracket_string(input: &str) -> PResult<'_, Expr> {
    let rest = input
        .strip_prefix('｢')
        .ok_or_else(|| PError::expected("corner bracket string"))?;
    if let Some(end) = rest.find('｣') {
        let content = &rest[..end];
        let after = &rest[end + '｣'.len_utf8()..];
        Ok((after, Expr::Literal(Value::Str(content.to_string()))))
    } else {
        Err(PError::expected("closing ｣"))
    }
}

/// Parse `\c[NAME, NAME, ...]` Unicode character name escape.
/// Returns the resulting string and the remaining input after the `]`.
/// `rest` should point right after `\c[`.
pub(super) fn parse_backslash_c_bracket(rest: &str) -> Option<(String, &str)> {
    let end = rest.find(']')?;
    let names_str = &rest[..end];
    let mut result = String::new();
    for part in names_str.split(',') {
        let name = part.trim();
        if name.is_empty() {
            continue;
        }
        // Try as Unicode character name
        if let Some(c) = lookup_unicode_char_by_name(name) {
            result.push(c);
        } else {
            // Try as hex codepoint (e.g. 0x0041)
            let hex = name.strip_prefix("0x").or_else(|| name.strip_prefix("0X"));
            if let Some(hex) = hex
                && let Ok(n) = u32::from_str_radix(hex, 16)
                && let Some(c) = char::from_u32(n)
            {
                result.push(c);
                continue;
            }
            // Try as decimal codepoint
            if let Ok(n) = name.parse::<u32>()
                && let Some(c) = char::from_u32(n)
            {
                result.push(c);
                continue;
            }
            // Unknown name - skip
            return None;
        }
    }
    Some((result, &rest[end + 1..]))
}

/// Parse a double-quoted string with interpolation support.
pub(super) fn double_quoted_string(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '"')?;
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = input;

    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing \""));
        }
        if rest.starts_with('"') {
            rest = &rest[1..];
            break;
        }
        if rest.starts_with('\\') && rest.len() > 1 {
            if let Some((r, needs_continue)) =
                process_escape_sequence(rest, &mut current, &['"', '{'])
            {
                rest = r;
                if needs_continue {
                    continue;
                }
            } else {
                let c = rest.as_bytes()[1] as char;
                current.push('\\');
                current.push(c);
                rest = &rest[2..];
            }
            continue;
        }
        if let Some(r) = try_interpolate_var(rest, &mut parts, &mut current) {
            rest = r;
            continue;
        }
        // Block interpolation: { expr }
        if rest.starts_with('{') {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
            }
            // Find matching close brace (tracking nesting)
            let mut depth = 0;
            let mut end = 0;
            for (i, c) in rest.char_indices() {
                match c {
                    '{' => depth += 1,
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            end = i;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            if end > 0 {
                let block_src = &rest[1..end];
                if let Ok((_rest, expr)) = expression(block_src) {
                    parts.push(expr);
                }
                rest = &rest[end + 1..];
                continue;
            }
        }
        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    Ok((rest, finalize_interpolation(parts, current)))
}
