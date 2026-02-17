use super::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, parse_tag, take_while_opt, take_while1,
    update_best_error,
};
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::Expr;
use crate::token_kind::lookup_unicode_char_by_name;
use crate::value::Value;

use super::expr::{expression, expression_no_sequence};
use super::helpers::ws;

#[derive(Debug, Clone)]
enum PrimaryMemoEntry {
    Ok { consumed: usize, expr: Box<Expr> },
    Err(PError),
}

#[derive(Debug, Default, Clone, Copy)]
struct PrimaryMemoStats {
    hits: usize,
    misses: usize,
    stores: usize,
}

thread_local! {
    static PRIMARY_MEMO: RefCell<HashMap<(usize, usize), PrimaryMemoEntry>> = RefCell::new(HashMap::new());
    static PRIMARY_MEMO_STATS: RefCell<PrimaryMemoStats> = RefCell::new(PrimaryMemoStats::default());
    /// Original source pointer and length, set at parse_program start for $?LINE computation.
    static ORIGINAL_SOURCE: RefCell<(usize, usize)> = const { RefCell::new((0, 0)) };
}

fn primary_memo_key(input: &str) -> (usize, usize) {
    (input.as_ptr() as usize, input.len())
}

fn primary_memo_get(input: &str) -> Option<PResult<'_, Expr>> {
    if !super::parse_memo_enabled() {
        return None;
    }
    let key = primary_memo_key(input);
    let entry = PRIMARY_MEMO.with(|memo| memo.borrow().get(&key).cloned());
    if let Some(entry) = entry {
        PRIMARY_MEMO_STATS.with(|stats| stats.borrow_mut().hits += 1);
        return Some(match entry {
            PrimaryMemoEntry::Ok { consumed, expr } => Ok((&input[consumed..], *expr)),
            PrimaryMemoEntry::Err(err) => Err(err),
        });
    }
    PRIMARY_MEMO_STATS.with(|stats| stats.borrow_mut().misses += 1);
    None
}

fn primary_memo_store(input: &str, result: &PResult<'_, Expr>) {
    if !super::parse_memo_enabled() {
        return;
    }
    let key = primary_memo_key(input);
    let entry = match result {
        Ok((rest, expr)) => PrimaryMemoEntry::Ok {
            consumed: input.len().saturating_sub(rest.len()),
            expr: Box::new(expr.clone()),
        },
        Err(err) => PrimaryMemoEntry::Err(err.clone()),
    };
    PRIMARY_MEMO.with(|memo| {
        memo.borrow_mut().insert(key, entry);
    });
    PRIMARY_MEMO_STATS.with(|stats| stats.borrow_mut().stores += 1);
}

/// Set the original source for $?LINE computation.
pub(super) fn set_original_source(source: &str) {
    ORIGINAL_SOURCE.with(|s| {
        *s.borrow_mut() = (source.as_ptr() as usize, source.len());
    });
}

/// Compute the 1-based line number of `input` within the original source.
fn current_line_number(input: &str) -> i64 {
    ORIGINAL_SOURCE.with(|s| {
        let (src_ptr, src_len) = *s.borrow();
        if src_ptr == 0 {
            return 1;
        }
        let input_ptr = input.as_ptr() as usize;
        if input_ptr < src_ptr || input_ptr > src_ptr + src_len {
            return 1;
        }
        let offset = input_ptr - src_ptr;
        // SAFETY: offset is within the original source bounds
        let src_slice = unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(src_ptr as *const u8, offset))
        };
        (src_slice.matches('\n').count() + 1) as i64
    })
}

pub(super) fn reset_primary_memo() {
    if !super::parse_memo_enabled() {
        return;
    }
    PRIMARY_MEMO.with(|memo| memo.borrow_mut().clear());
    PRIMARY_MEMO_STATS.with(|stats| *stats.borrow_mut() = PrimaryMemoStats::default());
}

pub(super) fn primary_memo_stats() -> (usize, usize, usize) {
    PRIMARY_MEMO_STATS.with(|stats| {
        let s = *stats.borrow();
        (s.hits, s.misses, s.stores)
    })
}

/// Parse an integer string with given radix, using BigInt for overflow.
fn parse_int_radix(clean: &str, radix: u32) -> Expr {
    if let Ok(n) = i64::from_str_radix(clean, radix) {
        Expr::Literal(Value::Int(n))
    } else if let Some(n) = num_bigint::BigInt::parse_bytes(clean.as_bytes(), radix) {
        Expr::Literal(Value::BigInt(n))
    } else {
        Expr::Literal(Value::Int(0))
    }
}

/// Parse an integer literal (including underscore separators).
fn integer(input: &str) -> PResult<'_, Expr> {
    // Hex: 0x...
    if let Ok((rest, _)) = parse_tag(input, "0x") {
        let (rest, digits) = take_while1(rest, |c: char| c.is_ascii_hexdigit() || c == '_')?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        return Ok((rest, parse_int_radix(&clean, 16)));
    }
    // Octal: 0o...
    if let Ok((rest, _)) = parse_tag(input, "0o") {
        let (rest, digits) = take_while1(rest, |c: char| matches!(c, '0'..='7' | '_'))?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        return Ok((rest, parse_int_radix(&clean, 8)));
    }
    // Binary: 0b...
    if let Ok((rest, _)) = parse_tag(input, "0b") {
        let (rest, digits) = take_while1(rest, |c: char| c == '0' || c == '1' || c == '_')?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        return Ok((rest, parse_int_radix(&clean, 2)));
    }
    let (rest, digits) = take_while1(input, |c: char| c.is_ascii_digit() || c == '_')?;
    // Don't consume if next char is '.' followed by digit (that's a decimal)
    if rest.starts_with('.') && rest.len() > 1 && rest.as_bytes()[1].is_ascii_digit() {
        return Err(PError::expected("integer (not decimal)"));
    }
    let clean: String = digits.chars().filter(|c| *c != '_').collect();
    // Check for imaginary suffix: 4i
    if rest.starts_with('i') && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_') {
        let n: i64 = clean.parse().unwrap_or(0);
        return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n as f64))));
    }
    // Try i64 first, fall back to BigInt for large integers
    if let Ok(n) = clean.parse::<i64>() {
        Ok((rest, Expr::Literal(Value::Int(n))))
    } else if let Ok(n) = clean.parse::<num_bigint::BigInt>() {
        Ok((rest, Expr::Literal(Value::BigInt(n))))
    } else {
        Ok((rest, Expr::Literal(Value::Int(0))))
    }
}

/// Parse a decimal number literal.
fn decimal(input: &str) -> PResult<'_, Expr> {
    let start = input;
    let (rest, _) = take_while1(input, |c: char| c.is_ascii_digit() || c == '_')?;
    let (rest, _) = parse_char(rest, '.')?;
    let (rest, _) = take_while1(rest, |c: char| c.is_ascii_digit() || c == '_')?;
    let num_str = &start[..start.len() - rest.len()];

    // Check for scientific notation
    let (rest, exp_part) = if rest.starts_with('e') || rest.starts_with('E') {
        let exp_start = rest;
        let r = &rest[1..];
        let r = if r.starts_with('+') || r.starts_with('-') {
            &r[1..]
        } else {
            r
        };
        if let Ok((r, _)) = take_while1(r, |c: char| c.is_ascii_digit()) {
            (r, Some(&exp_start[..exp_start.len() - r.len()]))
        } else {
            (rest, None)
        }
    } else {
        (rest, None)
    };

    let full = if let Some(exp) = exp_part {
        format!("{}{}", num_str, exp)
    } else {
        num_str.to_string()
    };
    let clean: String = full.chars().filter(|c| *c != '_').collect();
    let n: f64 = clean.parse().unwrap_or(0.0);
    // Check for imaginary suffix
    if rest.starts_with('i') && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_') {
        return Ok((&rest[1..], Expr::Literal(Value::Complex(0.0, n))));
    }
    Ok((rest, Expr::Literal(Value::Num(n))))
}

/// Read a bracketed string with nesting support (e.g., `{...{...}...}`)
fn read_bracketed(input: &str, open: char, close: char) -> PResult<'_, &str> {
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
fn q_string(input: &str) -> PResult<'_, Expr> {
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

/// Parse a single-quoted string literal.
/// Interpolate variables in string content (used by qq// etc.)
fn interpolate_string_content(content: &str) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
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
                    rest = &rest[2..];
                    if rest.starts_with('[') {
                        if let Some(end) = rest.find(']') {
                            let hex = &rest[1..end];
                            if let Ok(n) = u32::from_str_radix(hex, 16)
                                && let Some(ch) = char::from_u32(n)
                            {
                                current.push(ch);
                            }
                            rest = &rest[end + 1..];
                        }
                    } else {
                        let hex_chars: String = rest
                            .chars()
                            .take_while(|ch| ch.is_ascii_hexdigit())
                            .collect();
                        let len = hex_chars.len();
                        if let Ok(n) = u32::from_str_radix(&hex_chars, 16)
                            && let Some(ch) = char::from_u32(n)
                        {
                            current.push(ch);
                        }
                        rest = &rest[len..];
                    }
                    continue;
                }
                'o' => {
                    rest = &rest[2..];
                    if rest.starts_with('[') {
                        if let Some(end) = rest.find(']') {
                            let oct = &rest[1..end];
                            if let Ok(n) = u32::from_str_radix(oct, 8)
                                && let Some(ch) = char::from_u32(n)
                            {
                                current.push(ch);
                            }
                            rest = &rest[end + 1..];
                        }
                    } else {
                        let oct_chars: String = rest
                            .chars()
                            .take_while(|ch| matches!(ch, '0'..='7'))
                            .collect();
                        let len = oct_chars.len();
                        if let Ok(n) = u32::from_str_radix(&oct_chars, 8)
                            && let Some(ch) = char::from_u32(n)
                        {
                            current.push(ch);
                        }
                        rest = &rest[len..];
                    }
                    continue;
                }
                'c' => {
                    rest = &rest[2..];
                    if rest.starts_with('[')
                        && let Some((s, after)) = parse_backslash_c_bracket(&rest[1..])
                    {
                        current.push_str(&s);
                        rest = after;
                    }
                    continue;
                }
                _ => {
                    current.push('\\');
                    current.push(c);
                }
            }
            rest = &rest[2..];
            continue;
        }
        if rest.starts_with('$') && rest.len() > 1 {
            let next = rest.as_bytes()[1] as char;
            if next.is_alphabetic() || next == '_' || next == '*' || next == '?' || next == '!' {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
                }
                let var_rest = &rest[1..];
                let (var_rest, var_name) = parse_var_name_from_str(var_rest);
                parts.push(Expr::Var(var_name));
                rest = var_rest;
                continue;
            }
        }
        if rest.starts_with('@') && rest.len() > 1 {
            let next = rest.as_bytes()[1] as char;
            if next.is_alphabetic() || next == '_' {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
                }
                let var_rest = &rest[1..];
                let end = var_rest
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(var_rest.len());
                let name = &var_rest[..end];
                parts.push(Expr::ArrayVar(name.to_string()));
                rest = &var_rest[end..];
                continue;
            }
        }
        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    if parts.is_empty() {
        Expr::Literal(Value::Str(current))
    } else {
        if !current.is_empty() {
            parts.push(Expr::Literal(Value::Str(current)));
        }
        if parts.len() == 1 && matches!(&parts[0], Expr::Literal(Value::Str(_))) {
            return parts.into_iter().next().unwrap();
        }
        Expr::StringInterpolation(parts)
    }
}

fn single_quoted_string(input: &str) -> PResult<'_, Expr> {
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
            rest = &rest[2..];
        } else {
            let ch = rest.chars().next().unwrap();
            rest = &rest[ch.len_utf8()..];
        }
    }
}

/// Parse `\c[NAME, NAME, ...]` Unicode character name escape.
/// Returns the resulting string and the remaining input after the `]`.
/// `rest` should point right after `\c[`.
fn parse_backslash_c_bracket(rest: &str) -> Option<(String, &str)> {
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
fn double_quoted_string(input: &str) -> PResult<'_, Expr> {
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
            let c = rest.as_bytes()[1] as char;
            match c {
                'n' => current.push('\n'),
                't' => current.push('\t'),
                'r' => current.push('\r'),
                '0' => current.push('\0'),
                '\\' => current.push('\\'),
                '"' => current.push('"'),
                '$' => current.push('$'),
                '@' => current.push('@'),
                '{' => current.push('{'),
                'x' => {
                    // \x[HH] or \xHH
                    rest = &rest[2..];
                    if rest.starts_with('[') {
                        if let Some(end) = rest.find(']') {
                            let hex = &rest[1..end];
                            if let Ok(n) = u32::from_str_radix(hex, 16)
                                && let Some(c) = char::from_u32(n)
                            {
                                current.push(c);
                            }
                            rest = &rest[end + 1..];
                        }
                    } else {
                        let hex_chars: String =
                            rest.chars().take_while(|c| c.is_ascii_hexdigit()).collect();
                        let len = hex_chars.len();
                        if let Ok(n) = u32::from_str_radix(&hex_chars, 16)
                            && let Some(c) = char::from_u32(n)
                        {
                            current.push(c);
                        }
                        rest = &rest[len..];
                    }
                    continue;
                }
                'o' => {
                    // \o[OOO] or \oOOO
                    rest = &rest[2..];
                    if rest.starts_with('[') {
                        if let Some(end) = rest.find(']') {
                            let oct = &rest[1..end];
                            if let Ok(n) = u32::from_str_radix(oct, 8)
                                && let Some(c) = char::from_u32(n)
                            {
                                current.push(c);
                            }
                            rest = &rest[end + 1..];
                        }
                    } else {
                        let oct_chars: String = rest
                            .chars()
                            .take_while(|c| matches!(c, '0'..='7'))
                            .collect();
                        let len = oct_chars.len();
                        if let Ok(n) = u32::from_str_radix(&oct_chars, 8)
                            && let Some(c) = char::from_u32(n)
                        {
                            current.push(c);
                        }
                        rest = &rest[len..];
                    }
                    continue;
                }
                'c' => {
                    // \c[NAME, NAME, ...] Unicode character name escape
                    rest = &rest[2..];
                    if rest.starts_with('[')
                        && let Some((s, after)) = parse_backslash_c_bracket(&rest[1..])
                    {
                        current.push_str(&s);
                        rest = after;
                    }
                    continue;
                }
                _ => {
                    current.push('\\');
                    current.push(c);
                }
            }
            rest = &rest[2..];
            continue;
        }
        // Variable interpolation: $var
        if rest.starts_with('$') && rest.len() > 1 {
            let next = rest.as_bytes()[1] as char;
            if next.is_alphabetic() || next == '_' || next == '*' || next == '?' || next == '!' {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
                }
                // Parse variable name
                let var_rest = &rest[1..];
                let (var_rest, var_name) = parse_var_name_from_str(var_rest);
                parts.push(Expr::Var(var_name));
                rest = var_rest;
                continue;
            }
        }
        // Array interpolation: @var
        if rest.starts_with('@') && rest.len() > 1 {
            let next = rest.as_bytes()[1] as char;
            if next.is_alphabetic() || next == '_' {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
                }
                let var_rest = &rest[1..];
                let end = var_rest
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(var_rest.len());
                let name = &var_rest[..end];
                parts.push(Expr::ArrayVar(name.to_string()));
                rest = &var_rest[end..];
                continue;
            }
        }
        // Block interpolation: { expr }
        if rest.starts_with('{') {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
            }
            // Find matching close brace (simple, no nesting)
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
                // Parse the block as an expression
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

    if parts.is_empty() {
        Ok((rest, Expr::Literal(Value::Str(current))))
    } else {
        if !current.is_empty() {
            parts.push(Expr::Literal(Value::Str(current)));
        }
        if parts.len() == 1
            && let Expr::Literal(Value::Str(_)) = &parts[0]
        {
            return Ok((rest, parts.into_iter().next().unwrap()));
        }
        Ok((rest, Expr::StringInterpolation(parts)))
    }
}

/// Parse a variable name from raw string (used in interpolation).
fn parse_var_name_from_str(input: &str) -> (&str, String) {
    // Handle twigils: $*, $?, $!
    let (rest, twigil) =
        if input.starts_with('*') || input.starts_with('?') || input.starts_with('!') {
            (&input[1..], &input[..1])
        } else {
            (input, "")
        };
    let end = rest
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(rest.len());
    let name = &rest[..end];
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    (&rest[end..], full_name)
}

/// Parse a $variable reference.
fn scalar_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '$')?;
    // Handle $(expr) — scalar context / itemization
    if input.starts_with('(') {
        return paren_expr(input);
    }
    // Handle $_ special variable
    if input.starts_with('_') && (input.len() == 1 || !input.as_bytes()[1].is_ascii_alphanumeric())
    {
        return Ok((&input[1..], Expr::Var("_".to_string())));
    }
    // Handle $/ (match variable)
    if let Some(stripped) = input.strip_prefix('/') {
        return Ok((stripped, Expr::Var("/".to_string())));
    }
    // Handle $! (exception variable) — bare $! without name after it
    if let Some(after) = input.strip_prefix('!') {
        // If next char is alphanumeric or _, it's a twigil (e.g. $!attr)
        // If not, it's the bare $! variable
        let is_twigil = !after.is_empty()
            && (after.as_bytes()[0].is_ascii_alphanumeric() || after.as_bytes()[0] == b'_');
        if !is_twigil {
            return Ok((after, Expr::Var("!".to_string())));
        }
    }
    // Handle $<name> (named capture variable)
    if let Some(after_lt) = input.strip_prefix('<')
        && let Ok((after_name, name)) = take_while1(after_lt, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })
        && let Some(after_gt) = after_name.strip_prefix('>')
    {
        return Ok((after_gt, Expr::CaptureVar(name.to_string())));
    }
    // Handle $=finish and other Pod variables ($=pod, $=data, etc.)
    if let Some(after_eq) = input.strip_prefix('=') {
        let (rest, name) = parse_ident_with_hyphens(after_eq)?;
        let full_name = format!("={}", name);
        return Ok((rest, Expr::Var(full_name)));
    }
    // Handle twigils: $*FOO, $?FILE, $!attr, $.attr
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
        || (input.starts_with('.') && input.len() > 1 && input.as_bytes()[1].is_ascii_alphabetic())
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest, name) = parse_ident_with_hyphens(rest)?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    // $?LINE is a compile-time constant: replace with the current line number
    if full_name == "?LINE" {
        let line = current_line_number(input);
        return Ok((rest, Expr::Literal(Value::Int(line))));
    }
    Ok((rest, Expr::Var(full_name)))
}

/// Parse identifier allowing kebab-case hyphens (e.g., `my-var`).
/// A hyphen is only part of the name if followed by an alphanumeric character,
/// so `$pd--` parses as `$pd` + postfix `--`.
fn parse_ident_with_hyphens<'a>(input: &'a str) -> PResult<'a, &'a str> {
    let (rest, _first) = take_while1(input, |c: char| c.is_alphanumeric() || c == '_')?;
    let mut end = input.len() - rest.len();
    let bytes = input.as_bytes();
    loop {
        if end < bytes.len()
            && bytes[end] == b'-'
            && end + 1 < bytes.len()
            && (bytes[end + 1].is_ascii_alphabetic() || bytes[end + 1] == b'_')
        {
            end += 1;
            while end < bytes.len() && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_') {
                end += 1;
            }
        } else {
            break;
        }
    }
    Ok((&input[end..], &input[..end]))
}

/// Parse an @array variable reference.
fn array_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '@')?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest, name) = parse_ident_with_hyphens(rest)?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::ArrayVar(full_name)))
}

/// Parse a %hash variable reference.
fn hash_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '%')?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Special: %*ENV
    let (rest, name) = parse_ident_with_hyphens(rest)?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::HashVar(full_name)))
}

/// Parse a &code variable reference.
fn code_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '&')?;
    // Handle twigils: &?BLOCK, &?ROUTINE
    let (rest, twigil) = if let Some(stripped) = input.strip_prefix('?') {
        (stripped, "?")
    } else {
        (input, "")
    };
    let (rest, name) = parse_ident_with_hyphens(rest)?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::CodeVar(full_name)))
}

/// Parse a parenthesized expression or list.
fn paren_expr(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '(')?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = parse_char(input, ')') {
        // Empty parens = empty list
        return Ok((input, Expr::ArrayLiteral(Vec::new())));
    }
    // Try assignment expression: ($var = expr)
    let (input, first) = if let Ok((r, var_expr)) = expression_no_sequence(input) {
        let (r2, _) = ws(r)?;
        if matches!(&var_expr, Expr::Var(_))
            && r2.starts_with('=')
            && !r2.starts_with("==")
            && !r2.starts_with("=>")
        {
            let r2 = &r2[1..];
            let (r2, _) = ws(r2)?;
            let (r2, rhs) = expression(r2)?;
            if let Expr::Var(name) = &var_expr {
                (
                    r2,
                    Expr::AssignExpr {
                        name: name.clone(),
                        expr: Box::new(rhs),
                    },
                )
            } else {
                unreachable!()
            }
        } else {
            (r, var_expr)
        }
    } else {
        expression_no_sequence(input)?
    };
    let (input, _) = ws(input)?;
    // Check for sequence operator after single item: (1 ... 5)
    if let Some(seq) = try_parse_sequence_in_paren(input, std::slice::from_ref(&first)) {
        return seq;
    }
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((input, first));
    }
    // Comma-separated list with sequence operator detection
    // Use expression_no_sequence so that `...` is not consumed as part of an item
    let (input, _) = parse_char(input, ',')?;
    let (input, _) = ws(input)?;
    let mut items = vec![first];
    // Handle trailing comma before close paren
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((input, Expr::ArrayLiteral(items)));
    }
    // Check for sequence operator right after first comma
    if let Some(seq) = try_parse_sequence_in_paren(input, &items) {
        return seq;
    }
    let (mut input_rest, second) = expression_no_sequence(input)?;
    items.push(second);
    loop {
        let (input, _) = ws(input_rest)?;
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        // Check for sequence operator before comma
        if let Some(seq) = try_parse_sequence_in_paren(input, &items) {
            return seq;
        }
        let (input, _) = parse_char(input, ',')?;
        let (input, _) = ws(input)?;
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        // Check for sequence operator after comma
        if let Some(seq) = try_parse_sequence_in_paren(input, &items) {
            return seq;
        }
        let (input, next) = expression_no_sequence(input)?;
        items.push(next);
        input_rest = input;
    }
}

/// Try to parse a sequence operator (...) inside a paren expression.
/// If the input starts with ... or ...^, treat all collected items as seeds.
fn try_parse_sequence_in_paren<'a>(input: &'a str, seeds: &[Expr]) -> Option<PResult<'a, Expr>> {
    let (is_excl, rest) = if let Some(stripped) = input.strip_prefix("...^") {
        (true, stripped)
    } else if input.starts_with("...") && !input.starts_with("....") {
        (false, &input[3..])
    } else {
        return None;
    };
    // Parse the endpoint expression
    let result = (|| {
        let (rest, _) = ws(rest)?;
        // Special case: bare * means infinite sequence (Whatever/Inf)
        let (rest, endpoint) = if rest.starts_with('*')
            && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
        {
            (&rest[1..], Expr::Literal(Value::Num(f64::INFINITY)))
        } else {
            super::expr::expression_no_sequence(rest)?
        };
        // There may be more comma items after the endpoint
        let (rest, _) = ws(rest)?;
        let mut extra_items = Vec::new();
        let mut r = rest;
        while r.starts_with(',') {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(')') {
                r = r2;
                break;
            }
            let (r2, item) = super::expr::expression(r2)?;
            extra_items.push(item);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let (r, _) = parse_char(r, ')')?;

        let op = if is_excl {
            crate::token_kind::TokenKind::DotDotDotCaret
        } else {
            crate::token_kind::TokenKind::DotDotDot
        };
        let left = if seeds.len() == 1 {
            seeds[0].clone()
        } else {
            Expr::ArrayLiteral(seeds.to_vec())
        };
        // If there are extra items after the endpoint, pack endpoint + extras as Array right
        let right_expr = if extra_items.is_empty() {
            endpoint
        } else {
            let mut items = vec![endpoint];
            items.extend(extra_items);
            Expr::ArrayLiteral(items)
        };
        let seq = Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right_expr),
        };
        Ok((r, seq))
    })();
    Some(result)
}

/// Parse an array literal [...].
fn array_literal(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '[')?;
    let (input, _) = ws(input)?;
    let mut items = Vec::new();
    if let Ok((input, _)) = parse_char(input, ']') {
        return Ok((input, Expr::ArrayLiteral(items)));
    }
    let (mut rest, first) = expression(input)?;
    items.push(first);
    loop {
        let (r, _) = ws(rest)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((r, Expr::ArrayLiteral(items)));
            }
            let (r, next) = expression(r)?;
            items.push(next);
            rest = r;
        } else {
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            return Ok((r, Expr::ArrayLiteral(items)));
        }
    }
}

/// Parse a < > quote-word list.
fn angle_list(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '<')?;
    // Make sure it's not <= or <=> etc.
    if input.starts_with('=') || input.starts_with('-') {
        return Err(PError::expected("angle list"));
    }
    let mut words = Vec::new();
    let mut rest = input;
    loop {
        // Skip whitespace
        let (r, _) = take_while_opt(rest, |c: char| c == ' ' || c == '\t');
        rest = r;
        if rest.starts_with('>') {
            rest = &rest[1..];
            break;
        }
        if rest.is_empty() {
            return Err(PError::expected("closing >"));
        }
        let (r, word) = take_while1(rest, |c: char| {
            c != '>' && c != ' ' && c != '\t' && c != '\n'
        })?;
        words.push(word.to_string());
        rest = r;
    }
    if words.len() == 1 {
        Ok((
            rest,
            Expr::Literal(Value::Str(words.into_iter().next().unwrap())),
        ))
    } else {
        let exprs: Vec<Expr> = words
            .into_iter()
            .map(|w| Expr::Literal(Value::Str(w)))
            .collect();
        Ok((rest, Expr::ArrayLiteral(exprs)))
    }
}

/// Parse `Whatever` or `*` as Whatever.
/// Parse `::Foo` class literal (type object reference).
fn class_literal(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("::") {
        return Err(PError::expected("class literal"));
    }
    let rest = &input[2..];
    let (rest, name) = super::stmt::ident_pub(rest)?;
    // Handle qualified names: ::Foo::Bar
    let mut full_name = name;
    let mut r = rest;
    while r.starts_with("::") {
        let r2 = &r[2..];
        if let Ok((r2, part)) = super::stmt::ident_pub(r2) {
            full_name = format!("{}::{}", full_name, part);
            r = r2;
        } else {
            break;
        }
    }
    Ok((r, Expr::BareWord(full_name)))
}

fn whatever(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '*')?;
    // Make sure it's not ** (power op)
    if input.starts_with('*') {
        return Err(PError::expected("whatever (not **)"));
    }
    Ok((input, Expr::Literal(Value::Num(f64::INFINITY))))
}

/// Parse keywords that are values: True, False, Nil, Any, Inf, NaN, etc.
fn keyword_literal(input: &str) -> PResult<'_, Expr> {
    // Try each keyword, ensuring it's not followed by alphanumeric (word boundary)
    // Also reject if followed by `(` to prevent treating e() as a constant
    let try_kw = |kw: &str, val: Value| -> PResult<'_, Expr> {
        let (rest, _) = parse_tag(input, kw)?;
        // Check word boundary
        if let Some(c) = rest.chars().next()
            && (c.is_alphanumeric() || c == '_' || c == '-')
        {
            return Err(PError::expected("word boundary"));
        }
        // Reject if followed by `(` - that's a function call, not a constant
        if rest.starts_with('(') {
            return Err(PError::expected("not a function call"));
        }
        Ok((rest, Expr::Literal(val)))
    };

    if let Ok(r) = try_kw("True", Value::Bool(true)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("False", Value::Bool(false)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Nil", Value::Nil) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Any", Value::Nil) {
        return Ok(r);
    }
    // Unicode: ∅ (U+2205 EMPTY SET)
    if input.starts_with('\u{2205}') {
        return Ok((
            &input['\u{2205}'.len_utf8()..],
            Expr::Literal(Value::Set(std::collections::HashSet::new())),
        ));
    }
    if let Ok(r) = try_kw("Inf", Value::Num(f64::INFINITY)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("-Inf", Value::Num(f64::NEG_INFINITY)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("NaN", Value::Num(f64::NAN)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("pi", Value::Num(std::f64::consts::PI)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("tau", Value::Num(std::f64::consts::TAU)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("e", Value::Num(std::f64::consts::E)) {
        return Ok(r);
    }
    Err(PError::expected("keyword literal"))
}

/// Parse a bare identifier that could be a type name or function call.
/// Returns Expr::Call for function calls, Expr::BareWord for type names.
/// Check if a name is a Raku keyword (not a function call).
fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "loop"
            | "repeat"
            | "try"
            | "do"
            | "gather"
            | "sub"
            | "my"
            | "our"
            | "has"
            | "class"
            | "role"
            | "module"
            | "use"
            | "need"
            | "import"
            | "require"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "say"
            | "print"
            | "put"
            | "note"
            | "with"
            | "without"
            | "supply"
            | "react"
            | "whenever"
            | "start"
            | "quietly"
            | "sink"
    )
}

/// Check if a name is a listop (can take args without parens).
fn is_listop(name: &str) -> bool {
    matches!(
        name,
        "shift"
            | "unshift"
            | "push"
            | "pop"
            | "grep"
            | "map"
            | "sort"
            | "first"
            | "any"
            | "all"
            | "none"
            | "one"
            | "print"
            | "say"
            | "put"
            | "note"
            | "return"
            | "die"
            | "fail"
            | "warn"
            | "take"
            | "emit"
            | "split"
            | "join"
            | "reverse"
            | "min"
            | "max"
            | "sum"
            | "pick"
            | "roll"
    ) || is_expr_listop(name)
}

/// Functions that take multiple comma-separated expression arguments in listop style.
/// These are parsed with full expression arguments (not just primaries).
fn is_expr_listop(name: &str) -> bool {
    matches!(
        name,
        "ok" | "nok"
            | "is"
            | "isnt"
            | "is-deeply"
            | "is-approx"
            | "cmp-ok"
            | "like"
            | "unlike"
            | "isa-ok"
            | "does-ok"
            | "can-ok"
            | "lives-ok"
            | "dies-ok"
            | "eval-lives-ok"
            | "eval-dies-ok"
            | "throws-like"
            | "pass"
            | "flunk"
            | "skip"
            | "todo"
            | "diag"
            | "plan"
            | "done-testing"
            | "bail-out"
            | "subtest"
            | "use-ok"
    )
}

/// Check if input starts with a statement modifier keyword.
fn is_stmt_modifier_ahead(input: &str) -> bool {
    for kw in &["if", "unless", "for", "while", "until", "given", "when"] {
        if input.starts_with(kw)
            && !input
                .as_bytes()
                .get(kw.len())
                .is_some_and(|&c| c.is_ascii_alphanumeric() || c == b'_' || c == b'-')
        {
            return true;
        }
    }
    false
}

/// Parse expression listop arguments: comma-separated full expressions.
/// Stops at statement modifiers, semicolons, and closing brackets.
fn parse_expr_listop_args(input: &str, name: String) -> PResult<'_, Expr> {
    let (r, first) = expression(input).map_err(|err| PError {
        message: merge_expected_messages("expected listop argument expression", &err.message),
        remaining_len: err.remaining_len.or(Some(input.len())),
    })?;
    let mut args = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if !r2.starts_with(',') || r2.starts_with(",,") {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        // Stop at terminators
        if r2.is_empty()
            || r2.starts_with(';')
            || r2.starts_with('}')
            || r2.starts_with(')')
            || is_stmt_modifier_ahead(r2)
        {
            break;
        }
        let (r2, arg) = expression(r2).map_err(|err| PError {
            message: merge_expected_messages(
                "expected listop argument expression after ','",
                &err.message,
            ),
            remaining_len: err.remaining_len.or(Some(r2.len())),
        })?;
        args.push(arg);
        r = r2;
    }
    Ok((r, Expr::Call { name, args }))
}

/// Parse a single listop argument (stops before statement modifiers, semicolon, closing brackets).
/// Listops that take a block/closure as the first argument followed by a list.
fn is_block_first_listop(name: &str) -> bool {
    matches!(name, "map" | "grep" | "sort" | "first")
}

/// Check if an expression is a block-like form (closure, pointed block, etc.)
fn is_block_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::AnonSubParams { .. } | Expr::Lambda { .. } | Expr::Block { .. }
    )
}

fn parse_listop_arg(input: &str) -> PResult<'_, Expr> {
    // Try to parse a primary expression (variable, literal, call, etc.)
    // but stop if we hit a statement modifier
    if is_stmt_modifier_ahead(input) {
        return Err(PError::expected("listop argument"));
    }

    // Parse a single term (no binary operators to avoid consuming too much)
    // We use primary instead of expression to avoid consuming binary operators
    // This means shift @a + 1 will be parsed as (shift @a) + 1, not shift(@a + 1)
    primary(input)
}

pub(super) fn identifier_or_call(input: &str) -> PResult<'_, Expr> {
    let (rest, name) = take_while1(input, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();

    // Handle special expression keywords before qualified name resolution
    match name.as_str() {
        "try" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Try { body, catch: None }));
            }
        }
        "do" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::DoBlock { body, label: None }));
            }
            // do if/unless/given/for/while — wrap the control flow statement
            {
                let is_ctrl = |s: &str| {
                    for kw in &["if", "unless", "given", "for", "while", "until"] {
                        if s.starts_with(kw)
                            && !s.as_bytes().get(kw.len()).is_some_and(|&c| {
                                c.is_ascii_alphanumeric() || c == b'_' || c == b'-'
                            })
                        {
                            return true;
                        }
                    }
                    false
                };
                if is_ctrl(r)
                    && let Ok((r, stmt)) = super::stmt::statement_pub(r)
                {
                    return Ok((r, Expr::DoStmt(Box::new(stmt))));
                }
            }
            // do STMT — wrap an assignment or other statement
            if let Ok((r, stmt)) = super::stmt::statement_pub(r) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
            // do EXPR — just evaluate the expression
            let (r, expr) = expression(r)?;
            return Ok((r, expr));
        }
        "sub" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::AnonSub(body)));
            }
            // sub with params: sub ($x, $y) { ... }
            if r.starts_with('(') {
                let (r2, params_body) = parse_anon_sub_with_params(r).map_err(|err| PError {
                    message: merge_expected_messages(
                        "expected anonymous sub parameter list/body",
                        &err.message,
                    ),
                    remaining_len: err.remaining_len.or(Some(r.len())),
                })?;
                return Ok((r2, params_body));
            }
        }
        "gather" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Gather(body)));
            }
        }
        "die" | "fail" => {
            let (r, _) = ws(rest)?;
            // die/fail with no argument
            if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
                return Ok((r, Expr::Call { name, args: vec![] }));
            }
            let (r, arg) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name,
                    args: vec![arg],
                },
            ));
        }
        "quietly" | "sink" => {
            let (r, _) = ws(rest)?;
            // quietly/sink expr — wrap in a Call
            let (r, expr) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name: name.clone(),
                    args: vec![expr],
                },
            ));
        }
        "start" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((
                    r,
                    Expr::Call {
                        name: "start".to_string(),
                        args: vec![Expr::AnonSub(body)],
                    },
                ));
            }
        }
        "last" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Last,
                    label: None,
                },
            ));
        }
        "next" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Next,
                    label: None,
                },
            ));
        }
        "redo" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Redo,
                    label: None,
                },
            ));
        }
        _ => {}
    }

    // Check for :: qualified name (e.g. Foo::Bar)
    let (rest, name) = {
        let mut full_name = name;
        let mut r = rest;
        while r.starts_with("::") {
            let after = &r[2..];
            if let Ok((rest2, part)) =
                take_while1(after, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                full_name.push_str("::");
                full_name.push_str(part);
                r = rest2;
            } else {
                return Err(PError::expected_at("identifier after '::'", after));
            }
        }
        (r, full_name)
    };

    // Check if followed by `(` for function call
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        let (rest, _) = ws(rest)?;
        let (rest, args) = parse_call_arg_list(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, Expr::Call { name, args }));
    }

    // Bareword followed by block { ... } and comma — function call with block arg
    // e.g., map { $_ * 2 }, @arr  or  grep { $_ > 0 }, @arr
    let (r, _) = ws(rest)?;
    if r.starts_with('{')
        && !is_keyword(&name)
        && let Ok((r2, block_body)) = parse_block_body(r)
    {
        let (r3, _) = ws(r2)?;
        if let Some(r3) = r3.strip_prefix(',') {
            // Consume comma and remaining args
            let (r3, _) = ws(r3)?;
            let mut args = vec![Expr::AnonSub(block_body)];
            let (mut r3, first_arg) = expression(r3)?;
            args.push(first_arg);
            loop {
                let (r4, _) = ws(r3)?;
                if !r4.starts_with(',') {
                    return Ok((r4, Expr::Call { name, args }));
                }
                let r4 = &r4[1..];
                let (r4, _) = ws(r4)?;
                if r4.starts_with(';') || r4.is_empty() || r4.starts_with('}') {
                    return Ok((r4, Expr::Call { name, args }));
                }
                let (r4, next_arg) = expression(r4)?;
                args.push(next_arg);
                r3 = r4;
            }
        }
        // Block without trailing comma — return as separate expressions
        // Fall through to BareWord
    }

    // Check for listop: bareword followed by space and argument (but not statement modifier)
    // e.g., shift @a, push @a, 42, etc.
    if is_listop(&name)
        && !r.is_empty()
        && !r.starts_with(';')
        && !r.starts_with('}')
        && !r.starts_with(')')
        && !r.starts_with(',')
    {
        // Check if next token is a statement modifier keyword
        if !is_stmt_modifier_ahead(r) {
            // Expression listops (ok, is, diag, etc.) parse full expressions as args
            if is_expr_listop(&name) {
                return parse_expr_listop_args(r, name);
            }
            // Try to parse an argument
            let (r2, arg) = parse_listop_arg(r).map_err(|err| PError {
                message: merge_expected_messages(
                    "expected listop argument expression",
                    &err.message,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?;
            // For block-first listops (map, grep, sort, first), if the first arg is
            // a block/pointed-block and followed by comma, parse remaining args
            let mut args = vec![arg.clone()];
            let mut rest_after = r2;
            if is_block_first_listop(&name) && is_block_expr(&arg) {
                let (r3, _) = ws(rest_after)?;
                if let Some(r3) = r3.strip_prefix(',') {
                    let (r3, _) = ws(r3)?;
                    if !r3.starts_with(';') && !r3.starts_with('}') && !r3.starts_with(')') {
                        let (r3, rest_arg) = parse_listop_arg(r3)?;
                        args.push(rest_arg);
                        rest_after = r3;
                    }
                }
            }
            return Ok((rest_after, Expr::Call { name, args }));
        }
    }

    // Method-like: .new, .elems etc. is handled at expression level
    Ok((rest, Expr::BareWord(name)))
}

/// Parse a block body: { stmts }
fn parse_block_body(input: &str) -> PResult<'_, Vec<crate::ast::Stmt>> {
    let (r, _) = parse_char(input, '{')?;
    let (r, stmts) = super::stmt::stmt_list_pub(r)?;
    let (r, _) = ws_inner(r);
    let (r, _) = parse_char(r, '}')?;
    Ok((r, stmts))
}

/// Parse anonymous sub with params: sub ($x, $y) { ... }
fn parse_anon_sub_with_params(input: &str) -> PResult<'_, Expr> {
    let (mut r, _) = parse_char(input, '(')?;
    let mut params = Vec::new();
    loop {
        let (r2, _) = ws(r)?;
        if r2.starts_with(')') {
            r = r2;
            break;
        }
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
            let (r3, name) = super::stmt::var_name_pub(r2)?;
            params.push(name);
            let (r3, _) = ws(r3)?;
            if let Some(stripped) = r3.strip_prefix(',') {
                r = stripped;
            } else {
                r = r3;
            }
        } else {
            r = r2;
            break;
        }
    }
    parse_anon_sub_rest(r, params)
}

fn parse_anon_sub_rest(input: &str, params: Vec<String>) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    let (r, _) = parse_char(r, ')')?;
    let (r, _) = ws(r)?;
    let (r, body) = parse_block_body(r)?;
    if params.is_empty() {
        Ok((r, Expr::AnonSub(body)))
    } else {
        Ok((r, Expr::AnonSubParams { params, body }))
    }
}

/// Parse comma-separated call arguments inside parens.
pub(super) fn parse_call_arg_list(input: &str) -> PResult<'_, Vec<Expr>> {
    if input.starts_with(')') {
        return Ok((input, Vec::new()));
    }
    let (input, first) = expression(input)?;
    let mut args = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            return Ok((r, args));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, args));
        }
        let (r, arg) = expression(r)?;
        args.push(arg);
        rest = r;
    }
}

/// Parse a regex literal: /pattern/ or rx/pattern/ or m/pattern/
fn regex_lit(input: &str) -> PResult<'_, Expr> {
    // rx/pattern/ or rx{pattern}
    if let Ok((rest, _)) = parse_tag(input, "rx") {
        let close_delim = if rest.starts_with('/') {
            '/'
        } else if rest.starts_with('{') {
            '}'
        } else {
            return Err(PError::expected("regex delimiter"));
        };
        let r = &rest[1..];
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == close_delim as u8 {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let pattern = &r[..end];
        let rest = if end < r.len() {
            &r[end + 1..]
        } else {
            &r[end..]
        };
        return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
    }

    // !!! — fatal stub operator
    if let Some(r) = input.strip_prefix("!!!") {
        let (r, _) = ws(r)?;
        let (r, msg) = if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            (
                r,
                Expr::Literal(Value::Str("Stub code executed".to_string())),
            )
        } else {
            expression(r)?
        };
        return Ok((
            r,
            Expr::Call {
                name: "die".to_string(),
                args: vec![msg],
            },
        ));
    }

    // ??? — admonitory stub operator
    if let Some(r) = input.strip_prefix("???") {
        let (r, _) = ws(r)?;
        let (r, msg) = if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            (
                r,
                Expr::Literal(Value::Str("Stub code executed".to_string())),
            )
        } else {
            expression(r)?
        };
        return Ok((
            r,
            Expr::Call {
                name: "warn".to_string(),
                args: vec![msg],
            },
        ));
    }

    // s/pattern/replacement/
    if let Some(r) = input.strip_prefix("s/") {
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == b'/' {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let pattern = &r[..end];
        if end < bytes.len() {
            let r = &r[end + 1..]; // skip closing /
            // Now parse replacement until next /
            let mut rend = 0;
            let rbytes = r.as_bytes();
            while rend < rbytes.len() {
                if rbytes[rend] == b'/' {
                    break;
                }
                if rbytes[rend] == b'\\' && rend + 1 < rbytes.len() {
                    rend += 2;
                } else {
                    rend += 1;
                }
            }
            let replacement = &r[..rend];
            let rest = if rend < rbytes.len() {
                &r[rend + 1..]
            } else {
                &r[rend..]
            };
            return Ok((
                rest,
                Expr::Subst {
                    pattern: pattern.to_string(),
                    replacement: replacement.to_string(),
                },
            ));
        }
    }

    // S/pattern/replacement/ — non-destructive substitution
    if let Some(r) = input.strip_prefix("S/") {
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == b'/' {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let pattern = &r[..end];
        if end < bytes.len() {
            let r = &r[end + 1..];
            let mut rend = 0;
            let rbytes = r.as_bytes();
            while rend < rbytes.len() {
                if rbytes[rend] == b'/' {
                    break;
                }
                if rbytes[rend] == b'\\' && rend + 1 < rbytes.len() {
                    rend += 2;
                } else {
                    rend += 1;
                }
            }
            let replacement = &r[..rend];
            let rest = if rend < rbytes.len() {
                &r[rend + 1..]
            } else {
                &r[rend..]
            };
            return Ok((
                rest,
                Expr::NonDestructiveSubst {
                    pattern: pattern.to_string(),
                    replacement: replacement.to_string(),
                },
            ));
        }
    }

    // tr/from/to/ or TR/from/to/
    if let Some(r) = input
        .strip_prefix("tr/")
        .or_else(|| input.strip_prefix("TR/"))
    {
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == b'/' {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let from = &r[..end];
        if end < bytes.len() {
            let r = &r[end + 1..]; // skip middle /
            let mut rend = 0;
            let rbytes = r.as_bytes();
            while rend < rbytes.len() {
                if rbytes[rend] == b'/' {
                    break;
                }
                if rbytes[rend] == b'\\' && rend + 1 < rbytes.len() {
                    rend += 2;
                } else {
                    rend += 1;
                }
            }
            let to = &r[..rend];
            let rest = if rend < rbytes.len() {
                &r[rend + 1..]
            } else {
                &r[rend..]
            };
            return Ok((
                rest,
                Expr::Transliterate {
                    from: from.to_string(),
                    to: to.to_string(),
                },
            ));
        }
    }

    // m/pattern/ or m{pattern} or m[pattern]
    if input.starts_with("m/") || input.starts_with("m{") || input.starts_with("m[") {
        let close_delim = match input.as_bytes()[1] {
            b'/' => b'/',
            b'{' => b'}',
            b'[' => b']',
            _ => unreachable!(),
        };
        let r = &input[2..];
        let mut end = 0;
        let bytes = r.as_bytes();
        let mut depth = 1u32;
        while end < bytes.len() {
            if bytes[end] == close_delim {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            if close_delim != b'/' && bytes[end] == input.as_bytes()[1] {
                depth += 1;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let pattern = &r[..end];
        let rest = if end < r.len() {
            &r[end + 1..]
        } else {
            &r[end..]
        };
        return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
    }

    // Bare /pattern/
    if input.starts_with('/') && !input.starts_with("//") {
        let r = &input[1..];
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == b'/' {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        if end > 0 && end < bytes.len() {
            let pattern = &r[..end];
            let rest = &r[end + 1..];
            return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
        }
    }

    Err(PError::expected("regex literal"))
}

/// Parse a version literal: v5.26.1
fn version_lit(input: &str) -> PResult<'_, Expr> {
    use crate::value::VersionPart;
    let (rest, _) = parse_char(input, 'v')?;
    // Must start with a digit
    if rest.is_empty() || !rest.as_bytes()[0].is_ascii_digit() {
        return Err(PError::expected("version number"));
    }
    let (rest, version) = take_while1(rest, |c: char| {
        c.is_ascii_digit() || c == '.' || c == '*' || c == '+' || c == '-'
    })?;
    // Don't consume trailing '.' — it's likely a method call (e.g. v1.2.3.WHAT)
    let (version, rest) = if let Some(stripped) = version.strip_suffix('.') {
        (stripped, &input[1 + version.len() - 1..])
    } else {
        (version, rest)
    };
    // Check for + or - suffix
    let version_str = version.trim_end_matches(['+', '-']);
    let plus = version.ends_with('+');
    let minus = version.ends_with('-');
    let parts: Vec<VersionPart> = version_str
        .split('.')
        .map(|s| {
            if s == "*" {
                VersionPart::Whatever
            } else {
                VersionPart::Num(s.parse::<i64>().unwrap_or(0))
            }
        })
        .collect();
    Ok((rest, Expr::Literal(Value::Version { parts, plus, minus })))
}

/// Parse a topicalized method call: .say, .uc, .defined, etc.
fn topic_method_call(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('.') || input.starts_with("..") {
        return Err(PError::expected("topic method call"));
    }
    let r = &input[1..];
    // .() — invoke topic as callable
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, args) = parse_call_arg_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::CallOn {
                target: Box::new(Expr::Var("_".to_string())),
                args,
            },
        ));
    }
    let (r, modifier) = if let Some(stripped) = r.strip_prefix('^') {
        (stripped, Some('^'))
    } else if let Some(stripped) = r.strip_prefix('?') {
        (stripped, Some('?'))
    } else {
        (r, None)
    };
    let (rest, name) = take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        let (rest, _) = ws(rest)?;
        let (rest, args) = parse_call_arg_list(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((
            rest,
            Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name,
                args,
                modifier,
            },
        ));
    }
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name,
            args: Vec::new(),
            modifier,
        },
    ))
}

/// Parse a primary expression (atomic value).
/// Known reduction operators (must be listed to distinguish from array literals).
const REDUCTION_OPS: &[&str] = &[
    "+", "-", "*", "/", "~", "||", "&&", "//", "%%", "**", "+&", "+|", "+^", "?&", "?|", "?^",
    "==", "!=", "<", ">", "<=", ">=", "<=>", "===", "eq", "ne", "lt", "gt", "le", "ge", "leg",
    "cmp", "~~", "min", "max", "gcd", "lcm", "and", "or", "not", ",",
];

/// Parse a reduction operator: [+], [*], [~], [min], [max], [gcd], [lcm], [||], [&&], etc.
fn reduction_op(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('[') {
        return Err(PError::expected("reduction operator"));
    }
    let r = &input[1..];
    // Find the closing ]
    let end = r
        .find(']')
        .ok_or_else(|| PError::expected("']' closing reduction"))?;
    let op = &r[..end];
    if op.is_empty() {
        return Err(PError::expected("operator in reduction"));
    }
    // Only accept known operators to avoid confusion with array literals
    if !REDUCTION_OPS.contains(&op) {
        return Err(PError::expected("known reduction operator"));
    }
    let r = &r[end + 1..];
    // Must be followed by whitespace and an expression (not just `]`)
    if r.is_empty() || r.starts_with(';') || r.starts_with('}') || r.starts_with(')') {
        return Err(PError::expected("expression after reduction operator"));
    }
    let (r, _) = ws(r)?;
    // Parse comma-separated list as the operand
    let (r, first) = expression(r)?;
    let mut items = vec![first];
    let mut rest = r;
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
        if let Ok((r, next)) = expression(r) {
            items.push(next);
            rest = r;
        } else {
            return Err(PError::expected_at(
                "expression after ',' in reduction list",
                r,
            ));
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
            op: op.to_string(),
            expr: Box::new(expr),
        },
    ))
}

pub(super) fn primary(input: &str) -> PResult<'_, Expr> {
    if let Some(cached) = primary_memo_get(input) {
        return cached;
    }
    let result = (|| {
        let input_len = input.len();
        let mut best_error: Option<(usize, PError)> = None;
        macro_rules! try_primary {
            ($expr:expr) => {
                match $expr {
                    Ok(r) => return Ok(r),
                    Err(err) => update_best_error(&mut best_error, err, input_len),
                }
            };
        }

        try_primary!(decimal(input));
        try_primary!(integer(input));
        try_primary!(single_quoted_string(input));
        try_primary!(double_quoted_string(input));
        try_primary!(q_string(input));
        try_primary!(regex_lit(input));
        try_primary!(version_lit(input));
        try_primary!(keyword_literal(input));
        try_primary!(topic_method_call(input));
        try_primary!(scalar_var(input));
        try_primary!(array_var(input));
        try_primary!(hash_var(input));
        try_primary!(code_var(input));
        try_primary!(paren_expr(input));
        try_primary!(reduction_op(input));
        try_primary!(array_literal(input));
        try_primary!(angle_list(input));
        try_primary!(whatever(input));
        try_primary!(arrow_lambda(input));
        try_primary!(block_or_hash_expr(input));
        // ::Foo class literal (type object reference)
        try_primary!(class_literal(input));

        match identifier_or_call(input) {
            Ok(r) => Ok(r),
            Err(err) => {
                update_best_error(&mut best_error, err, input_len);
                Err(best_error
                    .map(|(_, err)| err)
                    .unwrap_or_else(|| PError::expected_at("primary expression", input)))
            }
        }
    })();
    primary_memo_store(input, &result);
    result
}

/// Parse `-> $param { body }` or `-> $a, $b { body }` arrow lambda.
fn arrow_lambda(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("->") {
        return Err(PError::expected("arrow lambda"));
    }
    let r = &input[2..];
    let (r, _) = ws(r)?;
    // Parse params
    let (r, first) = super::stmt::parse_pointy_param_pub(r)?;
    let (r, _) = ws(r)?;
    if r.starts_with(',') {
        // Multi-param: -> $a, $b { body }
        let mut params = vec![first];
        let mut r = r;
        loop {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            let (r2, next) = super::stmt::parse_pointy_param_pub(r2)?;
            params.push(next);
            let (r2, _) = ws(r2)?;
            if !r2.starts_with(',') {
                r = r2;
                break;
            }
            r = r2;
        }
        let (r, body) = parse_block_body(r)?;
        Ok((r, Expr::AnonSubParams { params, body }))
    } else {
        // Single param: -> $n { body }
        let (r, body) = parse_block_body(r)?;
        Ok((r, Expr::Lambda { param: first, body }))
    }
}

/// Parse a block `{ stmts }` as AnonSub or `{}` / `{ key => val, ... }` as Hash.
fn block_or_hash_expr(input: &str) -> PResult<'_, Expr> {
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
    let (r, stmts) = super::stmt::stmt_list_pub(r)?;
    let (r, _) = ws_inner(r);
    if !r.starts_with('}') {
        return Err(PError::expected("'}'"));
    }
    let r = &r[1..];
    Ok((r, Expr::AnonSub(stmts)))
}

/// Simple whitespace consumer that doesn't use PResult (infallible).
fn ws_inner(input: &str) -> (&str, ()) {
    match super::helpers::ws(input) {
        Ok((r, _)) => (r, ()),
        Err(_) => (input, ()),
    }
}

/// Check if the input looks like a hash literal start.
fn is_hash_literal_start(input: &str) -> bool {
    // ident => or "str" => or 'str' =>
    if let Ok((r, _)) = super::stmt::ident_pub(input) {
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
    false
}

/// Parse hash literal body: key => val, key => val, ... }
fn parse_hash_literal_body(input: &str) -> PResult<'_, Expr> {
    let mut pairs = Vec::new();
    let mut rest = input;
    loop {
        let (r, _) = ws_inner(rest);
        if let Some(rest) = r.strip_prefix('}') {
            return Ok((rest, Expr::Hash(pairs)));
        }
        // Parse key as identifier or string
        let (r, key) = if let Ok((r, name)) = super::stmt::ident_pub(r) {
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
        let (r, val) = super::expr::expression(r)?;
        pairs.push((key, Some(val)));
        let (r, _) = ws_inner(r);
        if let Some(stripped) = r.strip_prefix(',') {
            rest = stripped;
        } else {
            rest = r;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_integer() {
        let (rest, expr) = primary("42").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(42))));
    }

    #[test]
    fn parse_hex() {
        let (rest, expr) = primary("0xFF").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(255))));
    }

    #[test]
    fn parse_scalar() {
        let (rest, expr) = primary("$x").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Var(ref n) if n == "x"));
    }

    #[test]
    fn parse_twigil_var() {
        let (rest, expr) = primary("$*OUT").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Var(ref n) if n == "*OUT"));
    }

    #[test]
    fn parse_angle_single() {
        let (rest, expr) = primary("<hello>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn parse_angle_list() {
        let (rest, expr) = primary("<a b c>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::ArrayLiteral(ref items) if items.len() == 3));
    }

    #[test]
    fn parse_dq_interpolation() {
        let (rest, expr) = primary("\"hello $x world\"").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::StringInterpolation(_)));
    }

    #[test]
    fn primary_memo_reuses_result() {
        reset_primary_memo();
        let (rest, expr) = primary("42").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(42))));
        let (rest2, expr2) = primary("42").unwrap();
        assert_eq!(rest2, "");
        assert!(matches!(expr2, Expr::Literal(Value::Int(42))));
    }

    #[test]
    fn primary_aggregates_furthest_expected_messages() {
        let err = primary("@").unwrap_err();
        assert_eq!(err.message, "expected at least one matching character");
    }

    #[test]
    fn primary_reports_invalid_qualified_identifier_tail() {
        let err = primary("Foo::").unwrap_err();
        assert!(err.message.contains("identifier after '::'"));
    }

    #[test]
    fn primary_reports_missing_listop_argument() {
        let err = primary("shift :").unwrap_err();
        assert!(err.message.contains("listop argument expression"));
    }

    #[test]
    fn primary_reports_invalid_reduction_list_item() {
        let err = primary("[+] 1, :").unwrap_err();
        assert!(err.message.contains("reduction list"));
    }

    #[test]
    fn primary_reports_invalid_anon_sub_params() {
        let err = primary("sub ($x,)").unwrap_err();
        assert!(err.message.contains("anonymous sub parameter list/body"));
    }
}
