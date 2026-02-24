use super::super::parse_result::{PError, PResult, parse_char};

use crate::ast::Expr;
use crate::token_kind::{lookup_emoji_sequence, lookup_unicode_char_by_name};
use crate::value::Value;

use super::super::expr::expression;
use super::var::parse_var_name_from_str;

/// Map a Unicode opening bracket to its closing counterpart.
/// Only includes pairs that Raku recognizes for quoting.
fn unicode_bracket_close(open: char) -> Option<char> {
    match open {
        '\u{0028}' => Some('\u{0029}'), // ( )
        '\u{003C}' => Some('\u{003E}'), // < >
        '\u{005B}' => Some('\u{005D}'), // [ ]
        '\u{007B}' => Some('\u{007D}'), // { }
        '\u{00AB}' => Some('\u{00BB}'), // « »
        '\u{2018}' => Some('\u{2019}'), // ' '
        '\u{201A}' => Some('\u{2019}'), // ‚ '
        '\u{201C}' => Some('\u{201D}'), // " "
        '\u{201E}' => Some('\u{201D}'), // „ "
        '\u{2039}' => Some('\u{203A}'), // ‹ ›
        '\u{2045}' => Some('\u{2046}'), // ⁅ ⁆
        '\u{207D}' => Some('\u{207E}'), // ⁽ ⁾
        '\u{208D}' => Some('\u{208E}'), // ₍ ₎
        '\u{2308}' => Some('\u{2309}'), // ⌈ ⌉
        '\u{230A}' => Some('\u{230B}'), // ⌊ ⌋
        '\u{2329}' => Some('\u{232A}'), // 〈 〉
        '\u{27C5}' => Some('\u{27C6}'), // ⟅ ⟆
        '\u{27E6}' => Some('\u{27E7}'), // ⟦ ⟧
        '\u{27E8}' => Some('\u{27E9}'), // ⟨ ⟩
        '\u{27EA}' => Some('\u{27EB}'), // ⟪ ⟫
        '\u{27EC}' => Some('\u{27ED}'), // ⟬ ⟭
        '\u{27EE}' => Some('\u{27EF}'), // ⟮ ⟯
        '\u{2983}' => Some('\u{2984}'), // ⦃ ⦄
        '\u{2985}' => Some('\u{2986}'), // ⦅ ⦆
        '\u{2987}' => Some('\u{2988}'), // ⦇ ⦈
        '\u{2989}' => Some('\u{298A}'), // ⦉ ⦊
        '\u{298B}' => Some('\u{298C}'), // ⦋ ⦌
        '\u{298D}' => Some('\u{2990}'), // ⦍ ⦐
        '\u{298F}' => Some('\u{298E}'), // ⦏ ⦎
        '\u{2991}' => Some('\u{2992}'), // ⦑ ⦒
        '\u{2993}' => Some('\u{2994}'), // ⦓ ⦔
        '\u{2995}' => Some('\u{2996}'), // ⦕ ⦖
        '\u{2997}' => Some('\u{2998}'), // ⦗ ⦘
        '\u{29FC}' => Some('\u{29FD}'), // ⧼ ⧽
        '\u{2E22}' => Some('\u{2E23}'), // ⸢ ⸣
        '\u{2E24}' => Some('\u{2E25}'), // ⸤ ⸥
        '\u{2E26}' => Some('\u{2E27}'), // ⸦ ⸧
        '\u{2E28}' => Some('\u{2E29}'), // ⸨ ⸩
        '\u{3008}' => Some('\u{3009}'), // 〈 〉
        '\u{300A}' => Some('\u{300B}'), // 《 》
        '\u{300C}' => Some('\u{300D}'), // 「 」
        '\u{300E}' => Some('\u{300F}'), // 『 』
        '\u{3010}' => Some('\u{3011}'), // 【 】
        '\u{3014}' => Some('\u{3015}'), // 〔 〕
        '\u{3016}' => Some('\u{3017}'), // 〖 〗
        '\u{3018}' => Some('\u{3019}'), // 〘 〙
        '\u{301A}' => Some('\u{301B}'), // 〚 〛
        '\u{301D}' => Some('\u{301E}'), // 〝 〞
        '\u{FD3E}' => Some('\u{FD3F}'), // ﴾ ﴿
        '\u{FE17}' => Some('\u{FE18}'), // ︗ ︘
        '\u{FE59}' => Some('\u{FE5A}'), // ﹙ ﹚
        '\u{FE5B}' => Some('\u{FE5C}'), // ﹛ ﹜
        '\u{FE5D}' => Some('\u{FE5E}'), // ﹝ ﹞
        '\u{FF08}' => Some('\u{FF09}'), // （ ）
        '\u{FF3B}' => Some('\u{FF3D}'), // ［ ］
        '\u{FF5B}' => Some('\u{FF5D}'), // ｛ ｝
        '\u{FF5F}' => Some('\u{FF60}'), // ｟ ｠
        '\u{FF62}' => Some('\u{FF63}'), // ｢ ｣
        _ => None,
    }
}

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

/// Parse Q quoting with arbitrary delimiters: Q!...!, Q{...}, Q/.../, etc.
/// Q means no interpolation and no escape processing — content is taken verbatim.
pub(super) fn big_q_string(input: &str) -> PResult<'_, Expr> {
    let rest = input
        .strip_prefix('Q')
        .ok_or_else(|| PError::expected("Q string"))?;
    // Q:to/DELIM/ or Q:to<DELIM> heredoc
    if let Some(r) = rest.strip_prefix(":to") {
        return parse_to_heredoc(r);
    }
    // Q:s form (interpolating) — e.g. Q:s|...|
    let (rest, is_scalar_interp) = if let Some(r) = rest.strip_prefix(":s") {
        (r, true)
    } else {
        (rest, false)
    };
    let delim_char = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("Q string delimiter"))?;
    // Must not be alphanumeric or whitespace
    if delim_char.is_alphanumeric() || delim_char.is_whitespace() {
        return Err(PError::expected("Q string delimiter"));
    }
    // Check for bracket-style delimiter
    if let Some(close_char) = unicode_bracket_close(delim_char) {
        let (after, content) = read_bracketed(rest, delim_char, close_char)?;
        if is_scalar_interp {
            return Ok((after, interpolate_string_content(content)));
        }
        return Ok((after, Expr::Literal(Value::Str(content.to_string()))));
    }
    // Non-bracket delimiter — same char opens and closes, no nesting
    let after_open = &rest[delim_char.len_utf8()..];
    let end = after_open
        .find(delim_char)
        .ok_or_else(|| PError::expected("closing Q delimiter"))?;
    let content = &after_open[..end];
    let after = &after_open[end + delim_char.len_utf8()..];
    if is_scalar_interp {
        return Ok((after, interpolate_string_content(content)));
    }
    Ok((after, Expr::Literal(Value::Str(content.to_string()))))
}

fn parse_to_heredoc(input: &str) -> PResult<'_, Expr> {
    let (r, delimiter) = parse_to_heredoc_delimiter(input)?;
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
        // Return rest_of_line + after_terminator as remaining input.
        if rest_of_line.trim().is_empty() || rest_of_line.trim() == ";" {
            return Ok((
                after_terminator,
                Expr::Literal(Value::Str(content.to_string())),
            ));
        }
        // We cannot return a disjoint slice pair, so concatenate.
        let combined = format!("{}\n{}", rest_of_line, after_terminator);
        let leaked: &'static str = Box::leak(combined.into_boxed_str());
        return Ok((leaked, Expr::Literal(Value::Str(content.to_string()))));
    }
    Err(PError::expected("heredoc terminator"))
}

fn parse_to_heredoc_delimiter(input: &str) -> PResult<'_, &'_ str> {
    let open = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("heredoc delimiter"))?;
    if open.is_alphanumeric() || open.is_whitespace() {
        return Err(PError::expected("heredoc delimiter"));
    }

    if let Some(close) = unicode_bracket_close(open) {
        let (rest, delimiter) = read_bracketed(input, open, close)?;
        return Ok((rest, delimiter));
    }

    // Symmetric non-bracket delimiter such as /.../
    let body = &input[open.len_utf8()..];
    let end = body
        .find(open)
        .ok_or_else(|| PError::expected("closing heredoc delimiter"))?;
    let delimiter = &body[..end];
    let rest = &body[end + open.len_utf8()..];
    Ok((rest, delimiter))
}

/// Parse q{...}, q[...], q(...), q<...>, q/.../ quoting forms.
pub(super) fn q_string(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('q') {
        return Err(PError::expected("q string"));
    }
    let after_q = &input[1..];

    // q:to/DELIM/, q:to<DELIM>, qq:to/.../, qq:to<...> heredoc
    if after_q.starts_with(":to") || after_q.starts_with("q:to") {
        let r = if let Some(stripped) = after_q.strip_prefix("q:to") {
            stripped
        } else if let Some(stripped) = after_q.strip_prefix(":to") {
            stripped
        } else {
            unreachable!()
        };
        return parse_to_heredoc(r);
    }

    // Check for qq forms
    let (after_prefix, is_qq) = if let Some(after_qq) = after_q.strip_prefix('q') {
        // Accept any non-alphanumeric, non-whitespace character as qq delimiter
        let is_qq_delim = after_qq
            .chars()
            .next()
            .is_some_and(|c| !c.is_alphanumeric() && !c.is_whitespace());
        if is_qq_delim {
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
        Some(c) => {
            if let Some(close_char) = unicode_bracket_close(c) {
                let rest = &after_prefix[c.len_utf8()..];
                let end = rest
                    .find(close_char)
                    .ok_or_else(|| PError::expected("closing Unicode bracket"))?;
                let content = &rest[..end];
                let rest = &rest[end + close_char.len_utf8()..];
                if is_qq {
                    return Ok((rest, interpolate_string_content(content)));
                }
                let s = content.replace("\\'", "'").replace("\\\\", "\\");
                return Ok((rest, Expr::Literal(Value::Str(s))));
            }
            // Non-bracket, non-/ delimiter (e.g. q|...|, q!...!) — symmetric delimiter
            if !c.is_alphanumeric() && !c.is_whitespace() {
                let rest = &after_prefix[c.len_utf8()..];
                let end = rest
                    .find(c)
                    .ok_or_else(|| PError::expected(&format!("closing '{c}'")))?;
                let content = &rest[..end];
                let rest = &rest[end + c.len_utf8()..];
                if is_qq {
                    return Ok((rest, interpolate_string_content(content)));
                }
                let s = content.replace("\\'", "'").replace("\\\\", "\\");
                return Ok((rest, Expr::Literal(Value::Str(s))));
            }
            return Err(PError::expected("q string delimiter"));
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
                    let content = &r[1..end];
                    // Handle comma-separated hex values: \x[0041,0300]
                    for part in content.split(',') {
                        let hex = part.trim();
                        if let Ok(n) = u32::from_str_radix(hex, 16)
                            && let Some(ch) = char::from_u32(n)
                        {
                            current.push(ch);
                        }
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
            // \c followed by decimal digits → character by codepoint (e.g. \c10 = LF)
            let digits: String = r.chars().take_while(|ch| ch.is_ascii_digit()).collect();
            if !digits.is_empty() {
                let len = digits.len();
                if let Ok(n) = digits.parse::<u32>()
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
                }
                return Some((&r[len..], true));
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
        // Special variable $/ (match variable)
        if next == '/' {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::Str(std::mem::take(current))));
            }
            parts.push(Expr::Var("/".to_string()));
            return Some(&rest[2..]);
        }
        if next.is_alphabetic()
            || next == '_'
            || next == '*'
            || next == '?'
            || next == '!'
            || next == '^'
        {
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

/// Parse smart single-quoted string literal: ‘...’ (no interpolation)
pub(super) fn smart_single_quoted_string(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '‘')?;
    let mut rest = input;
    let start = input;
    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing ’"));
        }
        if let Some(after_quote) = rest.strip_prefix('’') {
            let content = &start[..start.len() - rest.len()];
            return Ok((after_quote, Expr::Literal(Value::Str(content.to_string()))));
        }
        let ch = rest.chars().next().unwrap();
        rest = &rest[ch.len_utf8()..];
    }
}

/// Parse corner bracket string literal: ｢...｣ (no interpolation, supports nesting)
pub(super) fn corner_bracket_string(input: &str) -> PResult<'_, Expr> {
    let rest = input
        .strip_prefix('｢')
        .ok_or_else(|| PError::expected("corner bracket string"))?;
    let mut depth: usize = 1;
    let mut pos = 0;
    for (i, ch) in rest.char_indices() {
        if ch == '｢' {
            depth += 1;
        } else if ch == '｣' {
            depth -= 1;
            if depth == 0 {
                pos = i;
                break;
            }
        }
    }
    if depth == 0 {
        let content = &rest[..pos];
        let after = &rest[pos + '｣'.len_utf8()..];
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
    let full_name = names_str.trim();

    // First try the full content as a single name (handles emoji sequences)
    if !full_name.contains(',') {
        if let Some(c) = lookup_unicode_char_by_name(full_name) {
            return Some((c.to_string(), &rest[end + 1..]));
        }
        // Try as emoji sequence name
        if let Some(s) = lookup_emoji_sequence(full_name) {
            return Some((s, &rest[end + 1..]));
        }
    }

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
                process_escape_sequence(rest, &mut current, &['"', '{', '}'])
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

/// Parse a Unicode smart-quoted string `“...”` with interpolation support.
pub(super) fn smart_double_quoted_string(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '“')?;
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = input;

    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing ”"));
        }
        if rest.starts_with('”') {
            rest = &rest['”'.len_utf8()..];
            break;
        }
        if rest.starts_with('\\') && rest.len() > 1 {
            if let Some((r, needs_continue)) =
                process_escape_sequence(rest, &mut current, &['”', '{', '}'])
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
        if rest.starts_with('{') {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
            }
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
