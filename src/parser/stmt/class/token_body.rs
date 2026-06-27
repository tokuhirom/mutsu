use crate::ast::Stmt;
use crate::parser::parse_result::{PError, PResult, take_while1};
use crate::parser::primary::regex::scan_to_delim;
use crate::parser::stmt::ident;

pub(crate) fn consume_raw_braced_body(input: &str) -> PResult<'_, Vec<Stmt>> {
    if !input.starts_with('{') {
        return Err(PError::expected("raw braced body"));
    }
    let mut depth = 0u32;
    let mut i = 0usize;
    while i < input.len() {
        let ch = input[i..]
            .chars()
            .next()
            .ok_or_else(|| PError::expected("closing '}'"))?;
        let len = ch.len_utf8();
        match ch {
            '{' => depth += 1,
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    let rest = &input[i + len..];
                    return Ok((rest, Vec::new()));
                }
            }
            '\\' => {
                i += len;
                if i < input.len() {
                    let next_len = input[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(0);
                    i += next_len;
                    continue;
                }
            }
            '\'' | '"' => {
                let quote = ch;
                i += len;
                while i < input.len() {
                    let c = input[i..]
                        .chars()
                        .next()
                        .ok_or_else(|| PError::expected("string close"))?;
                    let c_len = c.len_utf8();
                    if c == '\\' {
                        i += c_len;
                        if i < input.len() {
                            let n_len =
                                input[i..].chars().next().map(|n| n.len_utf8()).unwrap_or(0);
                            i += n_len;
                            continue;
                        }
                    }
                    i += c_len;
                    if c == quote {
                        break;
                    }
                }
                continue;
            }
            _ => {}
        }
        i += len;
    }
    Err(PError::expected("closing '}'"))
}

pub(crate) fn parse_token_like_name(input: &str) -> PResult<'_, String> {
    let (mut rest, mut name) = ident(input)?;
    loop {
        // `::`-qualified name segments (e.g. `does DBDish::ErrorHandling`).
        // Handle these before the single-`:` adverb logic so a qualified role
        // name parses as one token rather than failing on the second segment.
        if rest.starts_with("::") && !rest[2..].starts_with('(') {
            if let Ok((r, seg)) = ident(&rest[2..]) {
                name.push_str("::");
                name.push_str(&seg);
                rest = r;
                continue;
            }
            break;
        }
        if !rest.starts_with(':') {
            break;
        }
        let r = &rest[1..];
        // `:sym<value>` has an identifier part; `:<value>` (shorthand for
        // `:sym<value>`) has the colon immediately followed by `<...>` with no
        // part name. Allow an empty part in that case.
        let (r, part) = if r.starts_with('<') || r.starts_with("<<") || r.starts_with('\u{ab}') {
            (r, "")
        } else {
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?
        };
        name.push(':');
        name.push_str(part);
        let mut r2 = r;
        if r2.starts_with("<<") {
            // <<...>> delimiter (ASCII double-angle quotes)
            let after_open = &r2[2..];
            if let Some(end) = after_open.find(">>") {
                // Store as «...» internally for consistency
                name.push('\u{ab}');
                name.push_str(&after_open[..end]);
                name.push('\u{bb}');
                r2 = &after_open[end + 2..];
            }
        } else if r2.starts_with('<')
            && let Some(end) = r2.find('>')
        {
            name.push_str(&r2[..=end]);
            r2 = &r2[end + 1..];
        } else if r2.starts_with('\u{ab}') {
            // «» (French quotes) — keep as «» internally to avoid
            // ambiguity when the value contains '>'
            let after_open = &r2['\u{ab}'.len_utf8()..];
            if let Some(end) = after_open.find('\u{bb}') {
                name.push('\u{ab}');
                name.push_str(&after_open[..end]);
                name.push('\u{bb}');
                r2 = &after_open[end + '\u{bb}'.len_utf8()..];
            }
        }
        rest = r2;
    }
    Ok((rest, name))
}

pub(crate) fn parse_raw_braced_regex_body(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('{')
        .ok_or_else(|| PError::expected("regex body"))?;
    if let Some((body, rest)) = scan_to_delim(after_open, '{', '}', true) {
        return Ok((rest, body.trim().to_string()));
    }
    Err(PError::expected("regex closing delimiter"))
}

pub(crate) fn inject_implicit_rule_ws(pattern: &str) -> String {
    fn should_insert(prev: char, next: char) -> bool {
        !matches!(
            (prev, next),
            ('|', _)
                | (_, '|')
                | ('(', _)
                | (_, ')')
                | ('[', _)
                | (_, ']')
                | ('{', _)
                | (_, '}')
                | ('^', _)
                | (_, '$')
                | ('<', _)
                | (_, '>')
                | (_, '*')
                | (_, '+')
                | (_, '?')
                | (_, '%')
                | ('%', _)
        )
    }

    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::new();
    let mut i = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;
    let mut brace_depth = 0usize;
    while i < chars.len() {
        let c = chars[i];
        if escaped {
            out.push(c);
            escaped = false;
            i += 1;
            continue;
        }
        if c == '\\' {
            out.push(c);
            escaped = true;
            i += 1;
            continue;
        }
        if c == '\'' && !in_double {
            in_single = !in_single;
            out.push(c);
            i += 1;
            continue;
        }
        if c == '"' && !in_single {
            in_double = !in_double;
            out.push(c);
            i += 1;
            continue;
        }
        // Track brace depth to skip ws injection inside code blocks { ... }
        if !in_single && !in_double {
            if c == '{' {
                brace_depth += 1;
                out.push(c);
                i += 1;
                continue;
            }
            if c == '}' && brace_depth > 0 {
                brace_depth -= 1;
                out.push(c);
                i += 1;
                continue;
            }
        }
        // Inside a code block — pass through without ws injection
        if brace_depth > 0 {
            out.push(c);
            i += 1;
            continue;
        }
        if !in_single && !in_double && c.is_whitespace() {
            let mut j = i;
            while j < chars.len() && chars[j].is_whitespace() {
                j += 1;
            }
            let prev = out.chars().rev().find(|ch| !ch.is_whitespace());
            let next = chars[j..].iter().copied().find(|ch| !ch.is_whitespace());
            if let (Some(p), Some(n)) = (prev, next) {
                if p == '^' {
                    if !out.ends_with(' ') && !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str("<.ws>?");
                    out.push(' ');
                } else if should_insert(p, n) {
                    if !out.ends_with(' ') && !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str("<.ws>");
                    out.push(' ');
                } else if !out.ends_with(' ') && !out.is_empty() {
                    out.push(' ');
                }
            }
            i = j;
            continue;
        }
        out.push(c);
        i += 1;
    }
    out.trim().to_string()
}

/// In a `rule`, the `%` separator quantifier should allow optional whitespace
/// around the separator. This function finds `% SEPARATOR` patterns and wraps
/// the separator with `[ <.ws>? SEPARATOR <.ws>? ]`.
pub(crate) fn inject_separator_ws(pattern: &str) -> String {
    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::new();
    let mut i = 0usize;
    let mut escaped = false;
    let mut in_single = false;
    let mut in_double = false;
    while i < chars.len() {
        let c = chars[i];
        if escaped {
            out.push(c);
            escaped = false;
            i += 1;
            continue;
        }
        if c == '\\' {
            out.push(c);
            escaped = true;
            i += 1;
            continue;
        }
        if c == '\'' && !in_double {
            in_single = !in_single;
            out.push(c);
            i += 1;
            continue;
        }
        if c == '"' && !in_single {
            in_double = !in_double;
            out.push(c);
            i += 1;
            continue;
        }
        if in_single || in_double {
            out.push(c);
            i += 1;
            continue;
        }
        // Look for `%` (or `%%`) followed by a separator expression
        if c == '%' {
            out.push('%');
            i += 1;
            // `%%` (trailing-separator-allowed) is a single operator: emit its
            // second `%` too, otherwise the second `%` is mistaken for the
            // separator atom and the operator is destroyed (e.g.
            // `\d+ %% "+"` would become `\d+ % [<.ws>? % <.ws>?] "+"`).
            if i < chars.len() && chars[i] == '%' {
                out.push('%');
                i += 1;
            }
            // Skip whitespace after % / %%
            while i < chars.len() && chars[i].is_whitespace() {
                out.push(chars[i]);
                i += 1;
            }
            if i >= chars.len() {
                continue;
            }
            // Extract the separator expression
            let sep_start = i;
            if chars[i] == '[' {
                // Bracketed separator: % [ ... ]
                // Scan to matching ]
                let mut depth = 1usize;
                i += 1;
                while i < chars.len() && depth > 0 {
                    if chars[i] == '[' {
                        depth += 1;
                    } else if chars[i] == ']' {
                        depth -= 1;
                    } else if chars[i] == '\\' {
                        i += 1; // skip escaped char
                    }
                    i += 1;
                }
                let sep: String = chars[sep_start..i].iter().collect();
                out.push_str(&format!("[ <.ws>? {} <.ws>? ]", sep.trim()));
            } else {
                // Non-bracketed separator: % \, or % ","
                // Collect the separator atom (could be \X, 'str', "str", or a single char)
                let atom_start = i;
                if chars[i] == '\'' {
                    // Single-quoted string
                    i += 1;
                    while i < chars.len() && chars[i] != '\'' {
                        if chars[i] == '\\' {
                            i += 1;
                        }
                        i += 1;
                    }
                    if i < chars.len() {
                        i += 1;
                    } // skip closing '
                } else if chars[i] == '"' {
                    // Double-quoted string
                    i += 1;
                    while i < chars.len() && chars[i] != '"' {
                        if chars[i] == '\\' {
                            i += 1;
                        }
                        i += 1;
                    }
                    if i < chars.len() {
                        i += 1;
                    } // skip closing "
                } else if chars[i] == '\\' {
                    // Backslash-escaped char
                    i += 2;
                } else if chars[i] == '<' {
                    // Angle-bracket assertion
                    let mut depth = 1usize;
                    i += 1;
                    while i < chars.len() && depth > 0 {
                        if chars[i] == '<' {
                            depth += 1;
                        } else if chars[i] == '>' {
                            depth -= 1;
                        }
                        i += 1;
                    }
                } else {
                    // Single character separator
                    i += 1;
                }
                let sep: String = chars[atom_start..i].iter().collect();
                out.push_str(&format!("[ <.ws>? {} <.ws>? ]", sep.trim()));
            }
            continue;
        }
        out.push(c);
        i += 1;
    }
    out
}

pub(crate) fn normalize_token_pattern(pattern: &str) -> String {
    let trimmed = pattern.trim();
    if trimmed.len() >= 2 && trimmed.starts_with('/') && trimmed.ends_with('/') {
        trimmed[1..trimmed.len() - 1].to_string()
    } else {
        trimmed.to_string()
    }
}
