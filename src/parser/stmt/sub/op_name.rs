use super::*;

/// Parse the content inside `:[...]` for operator sub names.
/// Handles: `["op"]`, `['op']`, `["op1", "op2"]`, `[sym]`, `[sym1, sym2]`
/// Returns `(remaining_input, full_name)` on success.
pub(crate) fn parse_bracket_op_name<'a>(input: &'a str, base: &str) -> Option<(&'a str, String)> {
    let bracket_end = find_closing_bracket(input)?;
    let content = &input[..bracket_end];
    let after_close = &input[bracket_end + 1..];

    let parts = parse_bracket_parts(content)?;
    if parts.is_empty() {
        return None;
    }

    let op_symbol = parts.join(" ");
    let full_name = format!("{}:<{}>", base, op_symbol);
    Some((after_close, full_name))
}

/// Find the position of the closing `]`, skipping over quoted strings.
pub(crate) fn find_closing_bracket(input: &str) -> Option<usize> {
    let bytes = input.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b']' => return Some(i),
            b'"' => {
                i += 1;
                while i < bytes.len() {
                    if bytes[i] == b'\\' {
                        i += 2;
                    } else if bytes[i] == b'"' {
                        i += 1;
                        break;
                    } else {
                        i += 1;
                    }
                }
            }
            b'\'' => {
                i += 1;
                while i < bytes.len() {
                    if bytes[i] == b'\'' {
                        i += 1;
                        break;
                    } else {
                        i += 1;
                    }
                }
            }
            _ => i += 1,
        }
    }
    None
}

/// Parse comma-separated parts inside brackets.
/// Each part can be a double-quoted string, single-quoted string, or bare identifier.
pub(crate) fn parse_bracket_parts(content: &str) -> Option<Vec<String>> {
    let mut parts = Vec::new();
    let mut rest = content.trim();

    while !rest.is_empty() {
        if !parts.is_empty() {
            rest = rest.strip_prefix(',')?.trim();
            if rest.is_empty() {
                break;
            }
        }
        if let Some(after_quote) = rest.strip_prefix('"') {
            let mut end = 0;
            let mut escaped = false;
            for (i, c) in after_quote.char_indices() {
                if escaped {
                    escaped = false;
                    continue;
                }
                if c == '\\' {
                    escaped = true;
                    continue;
                }
                if c == '"' {
                    end = i;
                    break;
                }
            }
            let raw = &after_quote[..end];
            parts.push(unescape_operator_double_quoted(raw));
            rest = after_quote[end + 1..].trim();
        } else if let Some(after_quote) = rest.strip_prefix('\'') {
            if let Some(end) = after_quote.find('\'') {
                let raw = &after_quote[..end];
                parts.push(unescape_operator_single_quoted(raw));
                rest = after_quote[end + 1..].trim();
            } else {
                return None;
            }
        } else {
            let ident_end = rest
                .find(|c: char| c == ',' || c == ']' || c.is_whitespace())
                .unwrap_or(rest.len());
            let ident = rest[..ident_end].trim();
            if let Some(value) = super::super::simple::lookup_compile_time_constant(ident) {
                parts.push(value);
            } else if let Some(bare) = ident.strip_prefix('$') {
                if let Some(value) = super::super::simple::lookup_compile_time_constant(bare) {
                    parts.push(value);
                } else if let Some(value) =
                    super::super::simple::lookup_compile_time_constant(ident)
                {
                    parts.push(value);
                } else {
                    return None;
                }
            } else {
                return None;
            }
            rest = rest[ident_end..].trim();
        }
    }
    Some(parts)
}

pub(crate) fn unescape_operator_single_quoted(s: &str) -> String {
    s.replace("\\'", "'").replace("\\\\", "\\")
}

pub(crate) fn unescape_operator_double_quoted(s: &str) -> String {
    let mut out = String::new();
    let mut rest = s;
    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
            let c = rest.as_bytes()[1] as char;
            match c {
                'n' => {
                    out.push('\n');
                    rest = &rest[2..];
                    continue;
                }
                't' => {
                    out.push('\t');
                    rest = &rest[2..];
                    continue;
                }
                'r' => {
                    out.push('\r');
                    rest = &rest[2..];
                    continue;
                }
                '0' => {
                    out.push('\0');
                    rest = &rest[2..];
                    continue;
                }
                '"' => {
                    out.push('"');
                    rest = &rest[2..];
                    continue;
                }
                '\\' => {
                    out.push('\\');
                    rest = &rest[2..];
                    continue;
                }
                'x' => {
                    let r = &rest[2..];
                    if let Some(r2) = r.strip_prefix('[')
                        && let Some(end) = r2.find(']')
                    {
                        for part in r2[..end].split(',') {
                            if let Ok(n) = u32::from_str_radix(part.trim(), 16)
                                && let Some(ch) = char::from_u32(n)
                            {
                                out.push(ch);
                            }
                        }
                        rest = &r2[end + 1..];
                        continue;
                    }
                }
                'c' => {
                    let r = &rest[2..];
                    if let Some(r2) = r.strip_prefix('[')
                        && let Some(end) = r2.find(']')
                    {
                        let names = &r2[..end];
                        let mut ok = true;
                        for part in names.split(',') {
                            let name = part.trim();
                            if name.is_empty() {
                                continue;
                            }
                            if let Some(ch) = lookup_unicode_char_by_name(name) {
                                out.push(ch);
                            } else {
                                ok = false;
                                break;
                            }
                        }
                        if ok {
                            rest = &r2[end + 1..];
                            continue;
                        }
                    }
                }
                _ => {}
            }
            out.push('\\');
            out.push(c);
            rest = &rest[2..];
            continue;
        }
        let ch = rest
            .chars()
            .next()
            .expect("rest is non-empty when decoding operator name");
        out.push(ch);
        rest = &rest[ch.len_utf8()..];
    }
    out
}
