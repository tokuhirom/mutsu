use super::super::parse_result::{PError, PResult, parse_char};

use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::token_kind::{lookup_emoji_sequence, lookup_unicode_char_by_name};
use crate::value::Value;

use super::super::expr::expression;
use super::var::parse_var_name_from_str;

fn make_list_expr(items: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern("list"),
        args: items,
    }
}

pub(super) fn make_word_result_expr(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        Expr::Call {
            name: Symbol::intern("__mutsu_qw_result"),
            args: items,
        }
    } else {
        make_list_expr(items)
    }
}

fn quotewords_literal_marker(s: String) -> Expr {
    Expr::Literal(Value::Scalar(Box::new(Value::Pair(
        "__mutsu_qw_literal".to_string(),
        Box::new(Value::str(s)),
    ))))
}

pub(super) fn quotewords_atom_expr(atom_expr: Expr) -> Expr {
    quotewords_atom_expr_allomorphic(atom_expr, true)
}

pub(super) fn quotewords_atom_expr_allomorphic(atom_expr: Expr, allomorphic: bool) -> Expr {
    let args = match atom_expr {
        Expr::StringInterpolation(parts) => parts
            .into_iter()
            .map(|part| match part {
                Expr::Literal(Value::Str(s)) => quotewords_literal_marker(s.to_string()),
                other => other,
            })
            .collect(),
        other => vec![other],
    };
    let func_name = if allomorphic {
        "__mutsu_quotewords_atom"
    } else {
        "__mutsu_words_atom"
    };
    Expr::Call {
        name: Symbol::intern(func_name),
        args,
    }
}

/// Map a Unicode opening bracket to its closing counterpart (public for sibling modules).
pub(super) fn unicode_bracket_close_pub(open: char) -> Option<char> {
    unicode_bracket_close(open)
}

/// Map a Unicode opening bracket to its closing counterpart.
/// Only includes pairs that Raku recognizes for quoting.
fn unicode_bracket_close(open: char) -> Option<char> {
    match open {
        '\u{0028}' => Some('\u{0029}'), // ( )
        '\u{003C}' => Some('\u{003E}'), // < >
        '\u{005B}' => Some('\u{005D}'), // [ ]
        '\u{007B}' => Some('\u{007D}'), // { }
        '\u{00AB}' => Some('\u{00BB}'), // « »
        '\u{0F3A}' => Some('\u{0F3B}'), // ༺ ༻
        '\u{0F3C}' => Some('\u{0F3D}'), // ༼ ༽
        '\u{169B}' => Some('\u{169C}'), // ᚛ ᚜
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
        '\u{2768}' => Some('\u{2769}'), // ❨ ❩
        '\u{276A}' => Some('\u{276B}'), // ❪ ❫
        '\u{276C}' => Some('\u{276D}'), // ❬ ❭
        '\u{276E}' => Some('\u{276F}'), // ❮ ❯
        '\u{2770}' => Some('\u{2771}'), // ❰ ❱
        '\u{2772}' => Some('\u{2773}'), // ❲ ❳
        '\u{2774}' => Some('\u{2775}'), // ❴ ❵
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
        '\u{29D8}' => Some('\u{29D9}'), // ⧘ ⧙
        '\u{29DA}' => Some('\u{29DB}'), // ⧚ ⧛
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
        '\u{FE35}' => Some('\u{FE36}'), // ︵ ︶
        '\u{FE37}' => Some('\u{FE38}'), // ︷ ︸
        '\u{FE39}' => Some('\u{FE3A}'), // ︹ ︺
        '\u{FE3B}' => Some('\u{FE3C}'), // ︻ ︼
        '\u{FE3D}' => Some('\u{FE3E}'), // ︽ ︾
        '\u{FE3F}' => Some('\u{FE40}'), // ︿ ﹀
        '\u{FE41}' => Some('\u{FE42}'), // ﹁ ﹂
        '\u{FE43}' => Some('\u{FE44}'), // ﹃ ﹄
        '\u{FE47}' => Some('\u{FE48}'), // ﹇ ﹈
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

/// Read a bracketed string with nesting support (e.g., `{...{...}...}`).
/// When `allow_escape` is true, `\x` consumes both chars so escaped delimiters
/// do not affect nesting.
pub(super) fn read_bracketed(
    input: &str,
    open: char,
    close: char,
    allow_escape: bool,
) -> PResult<'_, &str> {
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
        if allow_escape && ch == '\\' && rest.len() > 1 {
            let next_ch = rest[1..].chars().next().unwrap();
            rest = &rest[1 + next_ch.len_utf8()..];
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

/// Read a multi-character bracketed string (e.g. `{{ ... }}`, `[[ ... ]]`).
/// `open_str` and `close_str` are the multi-character delimiters (e.g. "{{" and "}}").
/// Nested occurrences of `open_str` and `close_str` are tracked for proper balancing.
/// When `allow_escape` is true, `\x` consumes both chars so escaped delimiters
/// do not affect nesting.
fn read_multi_bracketed<'a>(
    input: &'a str,
    open_str: &str,
    close_str: &str,
    allow_escape: bool,
) -> PResult<'a, &'a str> {
    if !input.starts_with(open_str) {
        return Err(PError::expected(&format!("'{}'", open_str)));
    }
    let mut rest = &input[open_str.len()..];
    let start = rest;
    let mut depth = 1u32;
    loop {
        if rest.is_empty() {
            return Err(PError::expected(&format!("closing '{}'", close_str)));
        }
        if allow_escape && rest.starts_with('\\') && rest.len() > 1 {
            // Skip escape sequence (backslash + next char)
            let next_ch = rest[1..].chars().next().unwrap();
            rest = &rest[1 + next_ch.len_utf8()..];
            continue;
        }
        if rest.starts_with(open_str) {
            depth += 1;
            rest = &rest[open_str.len()..];
            continue;
        }
        if rest.starts_with(close_str) {
            depth -= 1;
            if depth == 0 {
                let content = &start[..start.len() - rest.len()];
                return Ok((&rest[close_str.len()..], content));
            }
            rest = &rest[close_str.len()..];
            continue;
        }
        let ch = rest.chars().next().unwrap();
        rest = &rest[ch.len_utf8()..];
    }
}

/// Count how many times the given bracket char is repeated at the start of `input`.
fn count_repeated_bracket(input: &str, ch: char) -> usize {
    let mut count = 0;
    for c in input.chars() {
        if c == ch {
            count += 1;
        } else {
            break;
        }
    }
    count
}

/// Parse Q quoting with arbitrary delimiters: Q!...!, Q{...}, Q/.../, etc.
/// Q means no interpolation and no escape processing — content is taken verbatim.
pub(super) fn big_q_string(input: &str) -> PResult<'_, Expr> {
    use super::quote_adverbs::{
        QuoteFlags, parse_colon_adverbs, parse_fused_adverbs_big_q, process_content_with_flags,
    };

    let rest = input
        .strip_prefix('Q')
        .ok_or_else(|| PError::expected("Q string"))?;

    // Parse fused adverbs (Qs, Qa, Qb, etc.)
    let mut flags = QuoteFlags::bare_q_big();
    let rest = parse_fused_adverbs_big_q(rest, &mut flags);

    // Parse colon-separated adverbs (:b, :s, :q, :qq, :to, etc.)
    let rest = parse_colon_adverbs(rest, &mut flags);

    // Handle :to (heredoc)
    if flags.heredoc {
        return parse_to_heredoc(rest, flags.has_interpolation() || flags.qq_mode);
    }

    if let Some((open, close)) = quote_delimiters(rest) {
        flags.quote_open = Some(open);
        flags.quote_close = Some(close);
    }

    // Handle :q/:qq through existing paths when no other adverbs are set
    if flags.qq_mode
        && !flags.backslash
        && !flags.scalar
        && !flags.array
        && !flags.hash
        && !flags.function
        && !flags.closure
        && !flags.execute
    {
        return parse_q_quoted_content(rest, true, false);
    }
    if flags.q_mode
        && !flags.backslash
        && !flags.scalar
        && !flags.array
        && !flags.hash
        && !flags.function
        && !flags.closure
        && !flags.execute
    {
        return parse_q_quoted_content(rest, false, false);
    }

    // Read delimited content
    let escape = flags.q_mode || flags.qq_mode || flags.backslash;
    let (after, content) = read_delimited_content(rest, escape)?;

    // Process content with flags
    if flags.quotewords {
        let items = parse_quotewords_items(content, &flags)?;
        return Ok((after, make_word_result_expr(items)));
    }
    let expr = process_content_with_flags(content, &flags);

    // Apply post-processing (execute, word-split, format)
    apply_post_processing(after, expr, &flags)
}

/// Apply execute, word-split, and format post-processing to a quote expression.
fn apply_post_processing<'a>(
    after: &'a str,
    expr: Expr,
    flags: &super::quote_adverbs::QuoteFlags,
) -> PResult<'a, Expr> {
    // Wrap in execute if :x
    if flags.execute {
        return Ok((
            after,
            Expr::Call {
                name: Symbol::intern("QX"),
                args: vec![expr],
            },
        ));
    }

    // Handle :w/:ww word splitting
    if flags.quotewords
        && let Expr::Literal(Value::Str(ref s)) = expr
    {
        let items = parse_quotewords_items(s, flags)?;
        return Ok((after, make_word_result_expr(items)));
    }

    if flags.words || flags.quotewords {
        if let Expr::Literal(Value::Str(ref s)) = expr {
            let words: Vec<Expr> = if flags.val {
                s.split_whitespace()
                    .map(|w| Expr::Literal(super::container::angle_word_value_full_allomorphic(w)))
                    .collect()
            } else {
                s.split_whitespace()
                    .map(|w| Expr::Literal(Value::str(w.to_string())))
                    .collect()
            };
            return Ok((after, make_word_result_expr(words)));
        }
        let func_name = if flags.val {
            "__mutsu_quotewords_atom"
        } else {
            "__mutsu_words_atom"
        };
        return Ok((
            after,
            Expr::Call {
                name: Symbol::intern(func_name),
                args: vec![expr],
            },
        ));
    }

    if flags.format {
        return Ok((
            after,
            Expr::Call {
                name: Symbol::intern("__mutsu_make_format"),
                args: vec![expr],
            },
        ));
    }

    Ok((after, expr))
}

fn parse_quotewords_items(
    content: &str,
    flags: &super::quote_adverbs::QuoteFlags,
) -> Result<Vec<Expr>, PError> {
    let mut items = Vec::new();
    let mut rest = content;
    loop {
        rest = trim_quotewords_ws_and_comments(rest)?;
        if rest.is_empty() {
            break;
        }
        if let Some((next, quoted)) = parse_quotewords_quoted_atom(rest)? {
            items.push(quoted);
            rest = next;
            continue;
        }

        let atom_len = find_quotewords_atom_end(rest);
        let (atom, next) = rest.split_at(atom_len);
        let atom_expr = super::quote_adverbs::process_content_with_flags(atom, flags);
        items.push(quotewords_atom_expr_allomorphic(atom_expr, flags.val));
        rest = next;
    }
    Ok(items)
}

fn trim_quotewords_ws_and_comments(mut input: &str) -> Result<&str, PError> {
    loop {
        let trimmed = input.trim_start_matches(|c: char| c.is_whitespace());
        if trimmed.len() != input.len() {
            input = trimmed;
            continue;
        }
        if let Some(after) = input.strip_prefix("#`") {
            input = skip_quotewords_embedded_comment(after)
                .ok_or_else(|| PError::expected("Opening bracket required for #` comment"))?;
            continue;
        }
        if let Some(after) = input.strip_prefix('#') {
            let end = after.find('\n').unwrap_or(after.len());
            input = &after[end..];
            continue;
        }
        return Ok(input);
    }
}

fn skip_quotewords_embedded_comment(input: &str) -> Option<&str> {
    let mut chars = input.chars();
    let open = chars.next()?;
    let close = unicode_bracket_close(open)?;
    let mut count = 1usize;
    let mut rest = chars.as_str();
    while rest.starts_with(open) {
        count += 1;
        rest = &rest[open.len_utf8()..];
    }
    let close_seq: String = std::iter::repeat_n(close, count).collect();
    let open_seq: String = std::iter::repeat_n(open, count).collect();
    if count == 1 {
        let mut depth = 1i32;
        let mut scan = rest;
        while !scan.is_empty() {
            let ch = scan.chars().next().unwrap();
            if ch == open {
                depth += 1;
            } else if ch == close {
                depth -= 1;
                if depth == 0 {
                    return Some(&scan[ch.len_utf8()..]);
                }
            }
            scan = &scan[ch.len_utf8()..];
        }
        None
    } else {
        let mut depth = 1i32;
        let mut scan = rest;
        while !scan.is_empty() {
            if scan.starts_with(&close_seq) {
                depth -= 1;
                if depth == 0 {
                    return Some(&scan[close_seq.len()..]);
                }
                scan = &scan[close_seq.len()..];
            } else if scan.starts_with(&open_seq) {
                depth += 1;
                scan = &scan[open_seq.len()..];
            } else {
                let ch = scan.chars().next().unwrap();
                scan = &scan[ch.len_utf8()..];
            }
        }
        None
    }
}

pub(super) fn parse_quotewords_quoted_atom(input: &str) -> Result<Option<(&str, Expr)>, PError> {
    if let Ok((rest, expr)) = single_quoted_string(input) {
        return Ok(Some((rest, expr)));
    }
    if let Some((rest, expr)) = parse_quotewords_unicode_quoted_atom(input) {
        return Ok(Some((rest, expr)));
    }
    if let Ok((rest, expr)) = double_quoted_string(input) {
        return Ok(Some((rest, expr)));
    }
    Ok(None)
}

fn find_quotewords_atom_end(input: &str) -> usize {
    for (idx, ch) in input.char_indices() {
        if ch.is_whitespace()
            || matches!(
                ch,
                '#' | '"' | '\'' | '“' | '”' | '„' | '‘' | '’' | '‚' | '｢'
            )
        {
            return idx;
        }
    }
    input.len()
}

fn parse_quotewords_unicode_quoted_atom(input: &str) -> Option<(&str, Expr)> {
    let first = input.chars().next()?;
    match first {
        '‘' | '’' | '‚' => parse_quotewords_single_quote_atom(input),
        '“' | '”' | '„' => {
            let (rest, content) = parse_quotewords_quote_span(input, &['“', '”'])?;
            Some((rest, interpolate_string_content(content)))
        }
        '｢' => corner_bracket_string(input).ok(),
        _ => None,
    }
}

fn parse_quotewords_single_quote_atom(input: &str) -> Option<(&str, Expr)> {
    let (rest, content) = parse_quotewords_quote_span(input, &['‘', '’'])?;
    Some((rest, Expr::Literal(Value::str(content.to_string()))))
}

fn parse_quotewords_quote_span<'a>(input: &'a str, closers: &[char]) -> Option<(&'a str, &'a str)> {
    let first = input.chars().next()?;
    let body = &input[first.len_utf8()..];
    for (idx, ch) in body.char_indices() {
        if closers.contains(&ch) {
            let rest = &body[idx + ch.len_utf8()..];
            return Some((rest, &body[..idx]));
        }
    }
    None
}

/// Read delimited content from input. Handles bracket-style and symmetric delimiters.
fn read_delimited_content<'a>(input: &'a str, escape_backslash: bool) -> PResult<'a, &'a str> {
    let rest = input.trim_start_matches(' ');
    let delim_char = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("Q string delimiter"))?;
    if delim_char.is_alphanumeric() || delim_char.is_whitespace() || delim_char == '#' {
        return Err(PError::expected("Q string delimiter"));
    }
    let bracket_close = match delim_char {
        '{' => Some('}'),
        '[' => Some(']'),
        '(' => Some(')'),
        '<' => Some('>'),
        _ => unicode_bracket_close(delim_char),
    };
    if let Some(close_char) = bracket_close {
        let repeat_count = count_repeated_bracket(rest, delim_char);
        if repeat_count > 1 {
            let open_str: String = std::iter::repeat_n(delim_char, repeat_count).collect();
            let close_str: String = std::iter::repeat_n(close_char, repeat_count).collect();
            return read_multi_bracketed(rest, &open_str, &close_str, escape_backslash);
        }
        return read_bracketed(rest, delim_char, close_char, escape_backslash);
    }
    // Non-bracket delimiter
    let after_open = &rest[delim_char.len_utf8()..];
    let end = if escape_backslash {
        find_unescaped(after_open, delim_char)
    } else {
        after_open.find(delim_char)
    }
    .ok_or_else(|| PError::expected("closing Q delimiter"))?;
    let content = &after_open[..end];
    let after = &after_open[end + delim_char.len_utf8()..];
    Ok((after, content))
}

fn quote_delimiters(input: &str) -> Option<(char, char)> {
    let rest = input.trim_start_matches(' ');
    let open = rest.chars().next()?;
    let close = match open {
        '{' => '}',
        '[' => ']',
        '(' => ')',
        '<' => '>',
        _ => unicode_bracket_close(open).unwrap_or(open),
    };
    Some((open, close))
}

/// Find the position of the first unescaped occurrence of `delim` in `s`.
/// A `\` before the delimiter prevents it from matching.
fn find_unescaped(s: &str, delim: char) -> Option<usize> {
    let mut chars = s.char_indices();
    while let Some((i, ch)) = chars.next() {
        if ch == '\\' {
            chars.next(); // skip escaped character
            continue;
        }
        if ch == delim {
            return Some(i);
        }
    }
    None
}

/// Process escape sequences in q-string content for a given delimiter.
/// Only `\\` → `\` and `\<delim>` → `<delim>` are recognized.
fn process_q_escapes(content: &str, delim: char) -> String {
    let mut result = String::new();
    let mut chars = content.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                if next == '\\' || next == delim {
                    result.push(next);
                } else {
                    result.push('\\');
                    result.push(next);
                }
            } else {
                result.push('\\');
            }
        } else {
            result.push(ch);
        }
    }
    result
}

fn parse_to_heredoc(input: &str, interpolate: bool) -> PResult<'_, Expr> {
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
    let mut terminator_end = None;
    let mut terminator_indent = 0usize;
    let mut search_pos = 0;
    while search_pos <= heredoc_start.len() {
        // Raku allows indentation before heredoc terminators.
        let line = &heredoc_start[search_pos..];
        let leading_ws_bytes: usize = line
            .chars()
            .take_while(|c| matches!(c, ' ' | '\t'))
            .map(char::len_utf8)
            .sum();
        let term_pos = search_pos + leading_ws_bytes;
        if heredoc_start[term_pos..].starts_with(delimiter) {
            let after_delim = &heredoc_start[term_pos + delimiter.len()..];
            if after_delim.is_empty()
                || after_delim.starts_with('\n')
                || after_delim.starts_with('\r')
                || after_delim.starts_with(';')
            {
                content_end = Some(search_pos);
                terminator_end = Some(term_pos + delimiter.len());
                terminator_indent = ws_column_width(line);
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
        let content = if terminator_indent == 0 {
            content.to_string()
        } else {
            dedent_heredoc(content, terminator_indent)
        };
        let after_terminator = &heredoc_start[terminator_end.expect("terminator end")..];
        // Skip optional newline after terminator
        let after_terminator = after_terminator
            .strip_prefix('\n')
            .unwrap_or(after_terminator);
        // qq:to interpolates like double-quoted strings.
        // Use HeredocInterpolation to defer variable resolution to compile time,
        // so variables are resolved in the scope where the terminator appears
        // (not where qq:to is declared).
        let expr = if interpolate {
            Expr::HeredocInterpolation(content)
        // q:to keeps content literal; only process explicit \qq escapes.
        } else if content.contains("\\qq") {
            parse_single_quote_qq(&content)
        } else {
            // q:heredoc processes \\ → \ (same as single-quoted strings)
            let processed = process_q_escapes(&content, '\0');
            Expr::Literal(Value::str(processed))
        };
        // Return rest_of_line + after_terminator as remaining input.
        if rest_of_line.trim().is_empty() {
            return Ok((after_terminator, expr));
        }
        // We cannot return a disjoint slice pair, so concatenate.
        let combined = format!("{}\n{}", rest_of_line, after_terminator);
        let leaked: &'static str = Box::leak(combined.into_boxed_str());
        return Ok((leaked, expr));
    }
    Err(PError::expected("heredoc terminator"))
}

fn parse_to_heredoc_delimiter(input: &str) -> PResult<'_, &'_ str> {
    // Raku allows optional whitespace before the delimiter in q:to/Q:to
    // forms, e.g. q:to /END/;
    let input = input.trim_start_matches(' ');
    let open = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("heredoc delimiter"))?;
    if open.is_alphanumeric() || open.is_whitespace() {
        return Err(PError::expected("heredoc delimiter"));
    }

    if let Some(close) = unicode_bracket_close(open) {
        let (rest, delimiter) = read_bracketed(input, open, close, false)?;
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
    use super::quote_adverbs::{
        QuoteFlags, parse_colon_adverbs, parse_fused_adverbs_small_q, process_content_with_flags,
    };

    if !input.starts_with('q') {
        return Err(PError::expected("q string"));
    }
    let mut after_q = &input[1..];

    if let Some(after_paren) = after_q.strip_prefix('(')
        && !after_paren.starts_with('(')
    {
        return Err(PError::expected("q string"));
    }

    // q:nfc, q:nfd, q:nfkc, q:nfkd — Unicode normalization adverbs
    for nf_form in &[":nfkc", ":nfkd", ":nfc", ":nfd"] {
        if let Some(rest_after_nf) = after_q.strip_prefix(nf_form) {
            return parse_nf_form(rest_after_nf, &nf_form[1..]);
        }
    }

    // Start with q defaults (single-quote semantics)
    let mut flags = QuoteFlags::q_single();

    // Check for qq first (before fused adverbs, since qqww = qq + ww)
    if after_q.starts_with('q') {
        let after_qq = &after_q[1..];
        // qq is valid if followed by delimiter, colon adverb, or known fused adverb
        let qq_valid = after_qq
            .chars()
            .next()
            .is_some_and(|c| !c.is_alphanumeric() && !c.is_whitespace())
            || after_qq.starts_with("ww")
                && after_qq[2..]
                    .chars()
                    .next()
                    .is_some_and(|c| !c.is_alphanumeric())
            || after_qq.starts_with('w')
                && !after_qq.starts_with("ww")
                && after_qq[1..]
                    .chars()
                    .next()
                    .is_some_and(|c| !c.is_alphanumeric());
        if qq_valid {
            flags.q_mode = false;
            flags.qq_mode = true;
            after_q = after_qq;
        }
    }

    // Parse fused adverbs (qw, qww, qb — also qqww, qqw via the qq detection above)
    after_q = parse_fused_adverbs_small_q(after_q, &mut flags);

    // Parse colon-separated adverbs (:b, :s, :a, :h, :f, :c, :x, :w, :ww, :to, etc.)
    after_q = parse_colon_adverbs(after_q, &mut flags);

    // Handle :to (heredoc)
    if flags.heredoc {
        let interpolate = flags.has_interpolation() || flags.qq_mode;
        return parse_to_heredoc_with_flags(after_q, &flags, interpolate);
    }

    // `#` cannot be used as a quoting delimiter (it starts a comment).
    let trimmed_after_q = after_q.trim_start_matches(' ');
    if trimmed_after_q.starts_with('#') {
        return Err(PError::fatal(
            "# cannot be used as a quote delimiter".to_string(),
        ));
    }

    if let Some((open, close)) = quote_delimiters(after_q) {
        flags.quote_open = Some(open);
        flags.quote_close = Some(close);
    }

    // Read delimited content
    let escape = flags.q_mode || flags.qq_mode || flags.backslash;
    let (rest, content) = read_delimited_content(after_q, escape)?;

    // For q-mode with symmetric delimiters, pre-process \<delim> → <delim>
    let delim_char = after_q.trim_start().chars().next().unwrap_or('/');
    let is_symmetric =
        !matches!(delim_char, '{' | '[' | '(' | '<') && unicode_bracket_close(delim_char).is_none();
    if is_symmetric
        && flags.q_mode
        && !flags.qq_mode
        && delim_char != '\''
        && !flags.has_interpolation()
        && !flags.full_backslash()
    {
        let processed = process_q_escapes(content, delim_char);
        let expr = Expr::Literal(Value::str(processed));
        return apply_post_processing(rest, expr, &flags);
    }

    // When delimiter is {}, disable closure interpolation (Raku spec)
    if delim_char == '{' && flags.closure {
        flags.closure = false;
    }
    if delim_char == '{' && flags.qq_mode {
        // qq{...} — disable closure interp by switching to explicit flags
        flags.qq_mode = false;
        flags.scalar = true;
        flags.array = true;
        flags.hash = true;
        flags.function = true;
        flags.backslash = true;
        // closure stays false
    }

    // Process content with flags
    if flags.quotewords {
        let items = parse_quotewords_items(content, &flags)?;
        return Ok((rest, make_word_result_expr(items)));
    }
    let expr = process_content_with_flags(content, &flags);

    // Apply post-processing (execute, word-split, format)
    apply_post_processing(rest, expr, &flags)
}

/// Parse q:nfc/q:nfd/q:nfkc/q:nfkd forms.
fn parse_nf_form<'a>(rest_after_nf: &'a str, nf_name: &str) -> PResult<'a, Expr> {
    let form_upper = nf_name.to_uppercase();
    let (rest, content) = read_delimited_content(rest_after_nf, false)?;
    use unicode_normalization::UnicodeNormalization;
    let normalized: String = match form_upper.as_str() {
        "NFC" => content.nfc().collect(),
        "NFD" => content.nfd().collect(),
        "NFKC" => content.nfkc().collect(),
        _ => content.nfkd().collect(),
    };
    Ok((
        rest,
        Expr::Literal(Value::Uni {
            form: form_upper,
            text: normalized,
        }),
    ))
}

/// Parse heredoc with flags (supports :c, :w adverbs on heredoc).
fn parse_to_heredoc_with_flags<'a>(
    input: &'a str,
    flags: &super::quote_adverbs::QuoteFlags,
    interpolate: bool,
) -> PResult<'a, Expr> {
    use super::quote_adverbs::process_content_with_flags;

    let (r, delimiter) = parse_to_heredoc_delimiter(input)?;
    let (rest_of_line, heredoc_start) = if let Some(nl) = r.find('\n') {
        (&r[..nl], &r[nl + 1..])
    } else {
        return Err(PError::expected("heredoc body after newline"));
    };
    // Find the terminator line
    let mut content_end = None;
    let mut terminator_end = None;
    let mut terminator_indent = 0usize;
    let mut search_pos = 0;
    while search_pos <= heredoc_start.len() {
        let line = &heredoc_start[search_pos..];
        let leading_ws_bytes: usize = line
            .chars()
            .take_while(|c| matches!(c, ' ' | '\t'))
            .map(char::len_utf8)
            .sum();
        let term_pos = search_pos + leading_ws_bytes;
        if heredoc_start[term_pos..].starts_with(delimiter) {
            let after_delim = &heredoc_start[term_pos + delimiter.len()..];
            if after_delim.is_empty()
                || after_delim.starts_with('\n')
                || after_delim.starts_with('\r')
                || after_delim.starts_with(';')
            {
                content_end = Some(search_pos);
                terminator_end = Some(term_pos + delimiter.len());
                terminator_indent = ws_column_width(line);
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
        let content = if terminator_indent == 0 {
            content.to_string()
        } else {
            dedent_heredoc(content, terminator_indent)
        };
        let after_terminator = &heredoc_start[terminator_end.expect("terminator end")..];
        let after_terminator = after_terminator
            .strip_prefix('\n')
            .unwrap_or(after_terminator);

        // Process content based on flags
        let expr = if interpolate {
            Expr::HeredocInterpolation(content)
        } else if flags.closure || flags.words || flags.backslash {
            // Use flags-based processing for heredoc with adverbs
            process_content_with_flags(&content, flags)
        } else if content.contains("\\qq") {
            parse_single_quote_qq(&content)
        } else {
            // q:heredoc processes \\ → \ (same as single-quoted strings)
            let processed = process_q_escapes(&content, '\0');
            Expr::Literal(Value::str(processed))
        };

        // Apply word splitting if :w
        let expr = if flags.words {
            if let Expr::Literal(Value::Str(ref s)) = expr {
                let words: Vec<Expr> = s
                    .split_whitespace()
                    .map(|w| Expr::Literal(Value::str(w.to_string())))
                    .collect();
                Expr::ArrayLiteral(words)
            } else {
                Expr::MethodCall {
                    target: Box::new(expr),
                    name: Symbol::intern("words"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                }
            }
        } else {
            expr
        };

        if rest_of_line.trim().is_empty() {
            return Ok((after_terminator, expr));
        }
        let combined = format!("{}\n{}", rest_of_line, after_terminator);
        let leaked: &'static str = Box::leak(combined.into_boxed_str());
        return Ok((leaked, expr));
    }
    Err(PError::expected("heredoc terminator"))
}

/// Compute the visual column width of leading whitespace, treating tabs as
/// stops at every 8 columns.
fn ws_column_width(s: &str) -> usize {
    let mut col = 0usize;
    for ch in s.chars() {
        match ch {
            ' ' => col += 1,
            '\t' => col = (col / 8 + 1) * 8,
            _ => break,
        }
    }
    col
}

/// Dedent heredoc content by removing leading whitespace matching the terminator's
/// visual column width. Tabs are treated as stops at every 8 columns so that
/// mixed tab/space indentation is handled correctly.
fn dedent_heredoc(content: &str, terminator_indent_bytes: usize) -> String {
    // First, compute the terminator indent as a column width.
    // We receive the raw byte length of the terminator's leading whitespace,
    // so reconstruct the whitespace string to measure its column width.
    // However, the caller only passes byte length, so we need the actual
    // whitespace chars. Instead, accept the byte-based indent and re-derive
    // the column width from the terminator line.
    // Since we can't access the terminator line here, we use an alternative:
    // compute column width from the content's context.
    //
    // Actually, the cleanest approach: take column-based indent directly.
    // For now, we'll use the byte-based indent for the simple (spaces-only
    // or tabs-only) case and fall through to column-based for mixed.
    dedent_heredoc_by_columns(content, terminator_indent_bytes)
}

fn dedent_heredoc_by_columns(content: &str, target_cols: usize) -> String {
    let mut dedented = String::new();
    for segment in content.split_inclusive('\n') {
        let mut col = 0usize;
        let mut strip_pos = 0usize;
        for ch in segment.chars() {
            if col >= target_cols {
                break;
            }
            match ch {
                ' ' => {
                    col += 1;
                    strip_pos += 1;
                }
                '\t' => {
                    let next_col = (col / 8 + 1) * 8;
                    if next_col > target_cols {
                        // Tab overshoots the target — stop here without
                        // consuming it (the tab will remain in the output).
                        break;
                    }
                    col = next_col;
                    strip_pos += 1;
                }
                _ => break,
            }
        }
        dedented.push_str(&segment[strip_pos..]);
    }
    dedented
}

/// Helper to build the final expression from quoted content.
fn make_q_content_expr(content: &str, is_qq: bool, q_closure_interp: bool) -> Expr {
    if is_qq {
        interpolate_string_content(content)
    } else if q_closure_interp {
        interpolate_string_content_with_modes(content, false, true)
    } else {
        let s = content.replace("\\'", "'").replace("\\\\", "\\");
        Expr::Literal(Value::str(s))
    }
}

fn parse_q_quoted_content(input: &str, is_qq: bool, q_closure_interp: bool) -> PResult<'_, Expr> {
    // Allow optional whitespace between q/qq and the delimiter
    let input = input.trim_start_matches(' ');

    // Must be followed by a delimiter
    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("q string delimiter"))?;

    // Check for bracket-style delimiter (possibly doubled)
    let bracket_close = match first {
        '{' => Some('}'),
        '[' => Some(']'),
        '(' => Some(')'),
        '<' => Some('>'),
        _ => unicode_bracket_close(first),
    };

    if let Some(close_ch) = bracket_close {
        // Count how many times the open bracket is repeated (e.g. {{ = 2, {{{ = 3)
        let repeat_count = count_repeated_bracket(input, first);
        if repeat_count > 1 {
            // Multi-bracket delimiter (e.g. q{{ ... }}, q[[ ... ]])
            let open_str: String = std::iter::repeat_n(first, repeat_count).collect();
            let close_str: String = std::iter::repeat_n(close_ch, repeat_count).collect();
            let (rest, content) = read_multi_bracketed(input, &open_str, &close_str, true)?;
            return Ok((rest, make_q_content_expr(content, is_qq, q_closure_interp)));
        }
        // Single bracket — use read_bracketed for proper nesting
        let (rest, content) = read_bracketed(input, first, close_ch, true)?;
        return Ok((rest, make_q_content_expr(content, is_qq, q_closure_interp)));
    }

    // Non-bracket delimiter (e.g. q/.../, q|...|)
    if !first.is_alphanumeric() && !first.is_whitespace() {
        let rest = &input[first.len_utf8()..];
        let end = rest
            .find(first)
            .ok_or_else(|| PError::expected(&format!("closing '{first}'")))?;
        let content = &rest[..end];
        let rest = &rest[end + first.len_utf8()..];
        return Ok((rest, make_q_content_expr(content, is_qq, q_closure_interp)));
    }

    Err(PError::expected("q string delimiter"))
}

/// Parse qx{...}, qqx{...} forms.
/// qx executes with single-quote (backslash) interpolation.
/// qqx executes with full interpolation.
pub(in crate::parser) fn qx_string(input: &str) -> PResult<'_, Expr> {
    // Try qqx first, then qx
    let (after_qx, is_qq) = if let Some(r) = input.strip_prefix("qqx") {
        (r, true)
    } else if let Some(r) = input.strip_prefix("qx") {
        (r, false)
    } else {
        return Err(PError::expected("qx string"));
    };
    let delim = after_qx
        .chars()
        .next()
        .ok_or_else(|| PError::expected("qx string delimiter"))?;
    if delim.is_alphanumeric() || delim.is_whitespace() {
        return Err(PError::expected("qx string delimiter"));
    }

    let (rest, content) = if let Some(close_char) = unicode_bracket_close(delim) {
        read_bracketed(after_qx, delim, close_char, true)?
    } else {
        let body = &after_qx[delim.len_utf8()..];
        let end = body
            .find(delim)
            .ok_or_else(|| PError::expected(&format!("closing '{delim}'")))?;
        (&body[end + delim.len_utf8()..], &body[..end])
    };

    let command_expr = if is_qq {
        interpolate_string_content(content)
    } else {
        // qx uses q-style backslash (only \\ → \)
        let s = content.replace("\\\\", "\\");
        Expr::Literal(Value::str(s))
    };

    Ok((
        rest,
        Expr::Call {
            name: Symbol::intern("QX"),
            args: vec![command_expr],
        },
    ))
}

/// Parse backtick command form: `...`.
/// This is equivalent to `qx` with interpolation enabled.
pub(super) fn backtick_qx_string(input: &str) -> PResult<'_, Expr> {
    if crate::parser::stmt::simple::match_user_declared_circumfix_op(input).is_some() {
        return Err(PError::expected("backtick qx string"));
    }
    let body = input
        .strip_prefix('`')
        .ok_or_else(|| PError::expected("backtick qx string"))?;
    let end = body
        .find('`')
        .ok_or_else(|| PError::expected("closing '`'"))?;
    let content = &body[..end];
    let rest = &body[end + 1..];
    let command_expr = interpolate_string_content(content);
    Ok((
        rest,
        Expr::Call {
            name: Symbol::intern("QX"),
            args: vec![command_expr],
        },
    ))
}

/// Process an escape sequence starting at `rest` (which begins with `\`).
/// `extra_escapes` lists additional simple single-char escapes (e.g., `'"'`, `'{'`).
/// Returns `Ok(Some((remaining_input, true)))` if a continuation-style escape was handled
/// (caller should `continue`), or `Ok(Some((remaining_input, false)))` for simple escapes
/// (caller should advance past `\c`), or `Ok(None)` if the escape char is unknown (caller
/// pushes `\` + char and advances).
/// Returns `Err(message)` for malformed escape sequences that should be compile-time errors.
pub(super) fn process_escape_sequence<'a>(
    rest: &'a str,
    current: &mut String,
    extra_escapes: &[char],
) -> Result<Option<(&'a str, bool)>, String> {
    let Some(after_backslash) = rest.strip_prefix('\\') else {
        return Ok(None);
    };
    let Some(c) = after_backslash.chars().next() else {
        return Ok(None);
    };
    let after_escape = &after_backslash[c.len_utf8()..];
    match c {
        'n' => current.push('\n'),
        't' => current.push('\t'),
        'r' => current.push('\r'),
        'b' => current.push('\u{0008}'),
        'a' => current.push('\u{0007}'),
        'e' => current.push('\u{001B}'),
        'f' => current.push('\u{000C}'),
        '0' => current.push('\0'),
        '\\' => current.push('\\'),
        '$' => current.push('$'),
        '@' => current.push('@'),
        '%' => current.push('%'),
        '&' => current.push('&'),
        '"' => current.push('"'),
        '\'' => current.push('\''),
        ' ' => current.push(' '),
        'x' => {
            let r = after_escape;
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
                    // NFC-normalize: combining characters from \x[...] should
                    // compose with the preceding character
                    use unicode_normalization::UnicodeNormalization;
                    let normalized: String = current.nfc().collect();
                    current.clear();
                    current.push_str(&normalized);
                    return Ok(Some((&r[end + 1..], true)));
                }
                // \x[ without closing ] is a compile error
                return Err("Missing closing bracket for \\x[".to_string());
            } else {
                let hex_chars: String = r.chars().take_while(|ch| ch.is_ascii_hexdigit()).collect();
                let len = hex_chars.len();
                if len == 0 {
                    // \x not followed by hex digits or [ is unrecognized
                    return Err(
                        "X::Backslash::UnrecognizedSequence: Unrecognized backslash sequence: '\\x'"
                            .to_string(),
                    );
                }
                if let Ok(n) = u32::from_str_radix(&hex_chars, 16)
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
                }
                // NFC-normalize: \xHHHH escapes may introduce characters that
                // should compose with surrounding text
                use unicode_normalization::UnicodeNormalization;
                let normalized: String = current.nfc().collect();
                current.clear();
                current.push_str(&normalized);
                return Ok(Some((&r[len..], true)));
            }
        }
        'o' => {
            let r = after_escape;
            if r.starts_with('[') {
                if let Some(end) = r.find(']') {
                    let content = &r[1..end];
                    // Handle comma-separated octal values: \o[101,102,103]
                    for part in content.split(',') {
                        let oct = part.trim();
                        if let Ok(n) = u32::from_str_radix(oct, 8)
                            && let Some(ch) = char::from_u32(n)
                        {
                            current.push(ch);
                        }
                    }
                    return Ok(Some((&r[end + 1..], true)));
                }
                // \o[ without closing ] is a compile error
                return Err("Missing closing bracket for \\o[".to_string());
            } else {
                let oct_chars: String =
                    r.chars().take_while(|ch| matches!(ch, '0'..='7')).collect();
                let len = oct_chars.len();
                if len == 0 {
                    // \o not followed by octal digits or [ is unrecognized
                    return Err(
                        "X::Backslash::UnrecognizedSequence: Unrecognized backslash sequence: '\\o'"
                            .to_string(),
                    );
                }
                if let Ok(n) = u32::from_str_radix(&oct_chars, 8)
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
                }
                return Ok(Some((&r[len..], true)));
            }
        }
        'c' => {
            let r = after_escape;
            if let Some(r_inner) = r.strip_prefix('[') {
                if let Some((s, after)) = parse_backslash_c_bracket(r_inner) {
                    current.push_str(&s);
                    // NFC-normalize: combining characters from \c[...] should
                    // compose with the preceding character (e.g. "a\c[COMBINING DIAERESIS]" → "ä")
                    use unicode_normalization::UnicodeNormalization;
                    let normalized: String = current.nfc().collect();
                    current.clear();
                    current.push_str(&normalized);
                    return Ok(Some((after, true)));
                }
                // Distinguish missing ] from unknown name
                if r_inner.find(']').is_none() {
                    return Err("Missing closing bracket for \\c[".to_string());
                }
                // Unknown character name with valid brackets — skip content
                let end = r_inner.find(']').unwrap();
                return Ok(Some((&r_inner[end + 1..], true)));
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
                return Ok(Some((&r[len..], true)));
            }
            // \c followed by a single character → control character
            // \c@ = NUL (0), \cA = SOH (1), ..., \cZ = SUB (26)
            // \c? = DEL (127), \cI = TAB (9)
            if let Some(ch) = r.chars().next() {
                let ctrl = match ch {
                    '@' => Some(0u32),
                    'A'..='Z' => Some(ch as u32 - 'A' as u32 + 1),
                    'a'..='z' => Some(ch as u32 - 'a' as u32 + 1),
                    '?' => Some(127),
                    _ => None,
                };
                if let Some(code) = ctrl {
                    if let Some(ctrl_ch) = char::from_u32(code) {
                        current.push(ctrl_ch);
                    }
                    return Ok(Some((&r[ch.len_utf8()..], true)));
                }
                // \c followed by space is an error
                if ch == ' ' {
                    return Err("Unrecognized backslash sequence: '\\c'".to_string());
                }
            }
            return Ok(Some((r, true)));
        }
        // \1 through \9 followed by digits is no longer valid in Raku
        '1'..='9' => {
            return Err(format!("Unrecognized backslash sequence: '\\{}'", c));
        }
        _ => {
            if extra_escapes.contains(&c) {
                current.push(c);
            } else if !c.is_alphanumeric() {
                // Non-alphanumeric chars after \ produce themselves in Raku
                current.push(c);
            } else {
                return Ok(None);
            }
        }
    }
    Ok(Some((after_escape, false)))
}

/// Try to parse a method call chain on an interpolated variable: "$var.method()" or "$var.method".
/// Only recognizes simple identifier method names followed by `()` (no args).
/// Try to parse a method call chain on an interpolated variable: "$var.method()" or "$var.method".
/// Supports chained methods: "$var.flip.chars()" — intermediate methods need no parens,
/// but the chain is only interpolated if the LAST method has parens.
/// Also supports methods with arguments: "$var.substr(0,1)".
/// Also supports indirect (quoted) method names: "$var.'method'()" or "$var."method"()".
fn try_parse_interp_method_call(input: &str, target: Expr) -> (Expr, &str) {
    let mut expr = target;
    let mut rest = input;
    // Collect chain of .method parts; only commit if chain ends with parens
    // Each entry is (method_name, is_quoted, modifier) where modifier is
    // the meta-method prefix character (^, ?, !) if present.
    let mut chain: Vec<(String, bool, Option<char>)> = Vec::new();
    let mut chain_rest = rest;
    while let Some(after_dot) = chain_rest.strip_prefix('.') {
        if after_dot.is_empty() {
            break;
        }
        // Check for meta-method prefix: .^name() .?method() .!method()
        let (method_prefix, after_prefix) = if let Some(stripped) = after_dot.strip_prefix('^') {
            (Some('^'), stripped)
        } else if let Some(stripped) = after_dot.strip_prefix('?') {
            (Some('?'), stripped)
        } else if let Some(stripped) = after_dot.strip_prefix('!') {
            (Some('!'), stripped)
        } else {
            (None, after_dot)
        };
        let after_dot = after_prefix;
        if after_dot.is_empty() {
            break;
        }
        // Check for quoted method name: .'method'() or ."method"()
        let (method_name, after_name, is_quoted) =
            if let Some(after_q) = after_dot.strip_prefix('\'') {
                if let Some(end) = after_q.find('\'') {
                    let name = &after_q[..end];
                    // Reject quoted method names containing whitespace
                    if name.contains(char::is_whitespace) {
                        break;
                    }
                    let rest = &after_q[end + 1..];
                    (name, rest, true)
                } else {
                    break;
                }
            } else if let Some(after_q) = after_dot.strip_prefix('\u{201C}') {
                // Unicode left double quote
                if let Some(end) = after_q.find('\u{201D}') {
                    let name = &after_q[..end];
                    let rest = &after_q[end + '\u{201D}'.len_utf8()..];
                    (name, rest, true)
                } else {
                    break;
                }
            } else if let Some(after_q) = after_dot.strip_prefix('"') {
                if let Some(end) = after_q.find('"') {
                    let name = &after_q[..end];
                    // Reject quoted method names containing whitespace
                    if name.contains(char::is_whitespace) {
                        break;
                    }
                    let rest = &after_q[end + 1..];
                    (name, rest, true)
                } else {
                    break;
                }
            } else {
                let first = after_dot.as_bytes()[0];
                if !(first.is_ascii_alphabetic() || first == b'_') {
                    break;
                }
                let end = after_dot
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(after_dot.len());
                (&after_dot[..end], &after_dot[end..], false)
            };
        chain.push((method_name.to_string(), is_quoted, method_prefix));
        // Check if followed by (...) — parse args
        if after_name.starts_with('(') {
            // Find matching closing paren
            let mut depth = 0usize;
            let mut paren_end = None;
            for (idx, ch) in after_name.char_indices() {
                if ch == '(' {
                    depth += 1;
                } else if ch == ')' {
                    depth -= 1;
                    if depth == 0 {
                        paren_end = Some(idx);
                        break;
                    }
                }
            }
            if let Some(pe) = paren_end {
                let args_str = &after_name[1..pe];
                let after_parens = &after_name[pe + 1..];
                // Build the full chain
                for (i, (name, quoted, modifier)) in chain.iter().enumerate() {
                    let args = if i == chain.len() - 1 {
                        // Last method gets the args
                        if args_str.trim().is_empty() {
                            vec![]
                        } else {
                            let mut args = vec![];
                            for arg in args_str.split(',') {
                                let arg = arg.trim();
                                if let Ok((_, e)) = crate::parser::expr::expression(arg) {
                                    args.push(e);
                                }
                            }
                            args
                        }
                    } else {
                        vec![]
                    };
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name: Symbol::intern(name),
                        args,
                        modifier: *modifier,
                        quoted: *quoted,
                    };
                }
                rest = after_parens;
                chain.clear();
            }
            break;
        }
        chain_rest = after_name;
    }
    (expr, rest)
}

/// Parse shell-word content from `<<...>>` or `«...»` subscripts.
/// Handles variable interpolation ($var) and bare words.
fn parse_shell_words_index(content: &str) -> Expr {
    let trimmed = content.trim();
    // Check for variable interpolation ($var)
    if let Some(var_name) = trimmed.strip_prefix('$')
        && !var_name.is_empty()
        && var_name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
    {
        return Expr::Var(var_name.to_string());
    }
    // Otherwise treat as literal string key
    Expr::Literal(Value::str(trimmed.to_string()))
}

/// Try to interpolate a `$var` or `@var` at the current position.
/// Returns `Some(remaining_input)` if interpolation was performed, `None` otherwise.
pub(super) fn try_interpolate_var<'a>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
) -> Option<&'a str> {
    let parse_postcircumfix_index = |input: &'a str, target: Expr| -> (Expr, &'a str) {
        // Double angle bracket indexing: $var<<key>> (must be checked before single <)
        if let Some(after_dlt) = input.strip_prefix("<<")
            && let Some(end) = after_dlt.find(">>")
        {
            let content = &after_dlt[..end];
            let index = parse_shell_words_index(content);
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                },
                &after_dlt[end + 2..],
            );
        }
        // French guillemet indexing: $var«key»
        if let Some(after_guillemet) = input.strip_prefix('\u{00AB}')
            && let Some(end) = after_guillemet.find('\u{00BB}')
        {
            let content = &after_guillemet[..end];
            let index = parse_shell_words_index(content);
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                },
                &after_guillemet[end + '\u{00BB}'.len_utf8()..],
            );
        }
        // Angle bracket indexing: $var<key>
        if let Some(after_lt) = input.strip_prefix('<')
            && let Some(end) = after_lt.find('>')
        {
            let content = &after_lt[..end];
            let words: Vec<&str> = content.split_whitespace().collect();
            let index = if words.len() <= 1 {
                Expr::Literal(Value::str(words.first().copied().unwrap_or("").to_string()))
            } else {
                Expr::ArrayLiteral(
                    words
                        .into_iter()
                        .map(|w| Expr::Literal(Value::str(w.to_string())))
                        .collect(),
                )
            };
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                },
                &after_lt[end + 1..],
            );
        }
        // Square bracket indexing: $var[expr]
        if let Some(after_bracket) = input.strip_prefix('[')
            && let Some(end) = after_bracket.find(']')
        {
            let content = after_bracket[..end].trim();
            // Parse the index expression (integer literal or simple expression)
            let index = if let Ok(n) = content.parse::<i64>() {
                Expr::Literal(Value::Int(n))
            } else {
                // Try parsing as an expression
                if let Ok((_, expr)) = crate::parser::expr::expression(content) {
                    expr
                } else {
                    Expr::Literal(Value::str(content.to_string()))
                }
            };
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                },
                &after_bracket[end + 1..],
            );
        }
        // Curly brace indexing: %hash{expr}
        if let Some(after_brace) = input.strip_prefix('{') {
            // Find matching closing brace respecting nesting
            let mut depth = 1usize;
            let mut brace_end = None;
            for (idx, ch) in after_brace.char_indices() {
                if ch == '{' {
                    depth += 1;
                } else if ch == '}' {
                    depth -= 1;
                    if depth == 0 {
                        brace_end = Some(idx);
                        break;
                    }
                }
            }
            if let Some(end) = brace_end {
                let content = after_brace[..end].trim();
                if !content.is_empty()
                    && let Ok((_, expr)) = crate::parser::expr::expression(content)
                {
                    return (
                        Expr::Index {
                            target: Box::new(target),
                            index: Box::new(expr),
                        },
                        &after_brace[end + 1..],
                    );
                }
            }
        }
        (target, input)
    };

    if rest.starts_with('$') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        // Special variable $/ (match variable)
        if next == '/' {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let var_expr = Expr::Var("/".to_string());
            let (expr, var_rest) = parse_postcircumfix_index(&rest[2..], var_expr);
            let (expr, var_rest) = try_parse_interp_method_call(var_rest, expr);
            parts.push(expr);
            return Some(var_rest);
        }
        // $<key> is sugar for $/<key> (named capture shorthand)
        if next == '<'
            && let Some(after_lt) = rest.strip_prefix("$<")
            && let Some(end) = after_lt.find('>')
        {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let key = &after_lt[..end];
            let var_expr = Expr::Var("/".to_string());
            let index = Expr::Literal(Value::str(key.to_string()));
            let expr = Expr::Index {
                target: Box::new(var_expr),
                index: Box::new(index),
            };
            let var_rest = &after_lt[end + 1..];
            let (expr, var_rest) = try_parse_interp_method_call(var_rest, expr);
            parts.push(expr);
            return Some(var_rest);
        }
        // Numeric capture variables: $0, $1, $2, ...
        if next.is_ascii_digit() {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(var_rest.len());
            let digits = &var_rest[..end];
            // $0, $1 etc. are positional captures — Index into $/
            let index_val: i64 = digits.parse().unwrap_or(0);
            let expr = Expr::Index {
                target: Box::new(Expr::Var("/".to_string())),
                index: Box::new(Expr::Literal(Value::Int(index_val))),
            };
            let (expr, var_rest) = try_parse_interp_method_call(&var_rest[end..], expr);
            parts.push(expr);
            return Some(var_rest);
        }
        // $(...) expression interpolation
        if next == '(' {
            let after_dollar = &rest[1..];
            // Find matching closing paren
            let mut depth = 0usize;
            let mut paren_end = None;
            for (idx, ch) in after_dollar.char_indices() {
                if ch == '(' {
                    depth += 1;
                } else if ch == ')' {
                    depth -= 1;
                    if depth == 0 {
                        paren_end = Some(idx);
                        break;
                    }
                }
            }
            if let Some(pe) = paren_end {
                let inner = &after_dollar[1..pe];
                let remainder = &after_dollar[pe + 1..];
                // Try parsing as statement list first (supports if/for modifiers)
                let parsed = if let Ok((leftover, stmts)) =
                    crate::parser::stmt::stmt_list_pub(inner.trim())
                    && leftover.trim().is_empty()
                    && !stmts.is_empty()
                {
                    // When there's a single statement, use DoStmt so that
                    // For/If/etc. get expression-level compilation that
                    // collects results (e.g. list comprehensions).
                    if stmts.len() == 1 {
                        Some(Expr::DoStmt(Box::new(stmts.into_iter().next().unwrap())))
                    } else {
                        Some(Expr::DoBlock {
                            body: stmts,
                            label: None,
                        })
                    }
                } else if let Ok((leftover, expr)) = expression(inner.trim())
                    && leftover.trim().is_empty()
                {
                    Some(expr)
                } else {
                    None
                };
                if let Some(expr) = parsed {
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    parts.push(expr);
                    return Some(remainder);
                }
            }
        }
        if next.is_alphabetic()
            || next == '_'
            || next == '*'
            || next == '?'
            || next == '!'
            || next == '^'
        {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let var_rest = &rest[1..];
            let (var_rest, var_name) = parse_var_name_from_str(var_rest);
            let (expr, var_rest) = parse_postcircumfix_index(var_rest, Expr::Var(var_name));
            // Handle method call interpolation: "$var.method()" or "$var.method"
            let (expr, var_rest) = try_parse_interp_method_call(var_rest, expr);
            parts.push(expr);
            return Some(var_rest);
        }
        if next == '.' {
            let after_dot = &rest[2..];
            if !after_dot.is_empty() {
                let first = after_dot.as_bytes()[0];
                if first.is_ascii_alphabetic() || first == b'_' {
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    let end = after_dot
                        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                        .unwrap_or(after_dot.len());
                    let var_name = format!(".{}", &after_dot[..end]);
                    let (expr, remainder) =
                        parse_postcircumfix_index(&after_dot[end..], Expr::Var(var_name));
                    let (expr, remainder) = try_parse_interp_method_call(remainder, expr);
                    parts.push(expr);
                    return Some(remainder);
                }
            }
        }
    }
    if rest.starts_with('@') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        if next.is_alphabetic() || next == '_' {
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(var_rest.len());
            let name = &var_rest[..end];
            let expr = Expr::ArrayVar(name.to_string());
            let after_name = &var_rest[end..];
            let mut remainder = after_name;
            // Zen-slice interpolation: "@arr[]" and "@arr.[]" should interpolate the array value
            let mut consumed = false;
            if let Some(r) = remainder.strip_prefix(".[]") {
                remainder = r;
                consumed = true;
            } else if let Some(r) = remainder.strip_prefix("[]") {
                remainder = r;
                consumed = true;
            }
            let (expr, remainder) = parse_postcircumfix_index(remainder, expr);
            if !consumed && remainder.len() == after_name.len() {
                // @array without postcircumfix ([], [n], <key>) is literal
                return None;
            }
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            parts.push(expr);
            return Some(remainder);
        }
    }
    if rest.starts_with('%') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        if next.is_alphabetic() || next == '_' {
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(var_rest.len());
            let name = &var_rest[..end];
            let expr = Expr::HashVar(name.to_string());
            let tail = &var_rest[end..];
            // Zen-slice: %hash{} should stringify the whole hash
            if let Some(r) = tail.strip_prefix("{}") {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(r);
            }
            // Zen-slice: %hash<> should stringify the whole hash
            if let Some(after_zen) = tail.strip_prefix("<>") {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(after_zen);
            }
            // Zen-slice: %hash[] should stringify the whole hash
            if let Some(r) = tail.strip_prefix("[]") {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(r);
            }
            let (expr, remainder) = parse_postcircumfix_index(tail, expr);
            // In qq-like strings, `%hash` without a postcircumfix remains literal.
            // Only interpolate hash variables for explicit access forms like
            // `%hash<key>` / `%hash{...}`.
            if remainder.len() == tail.len() {
                return None;
            }
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            parts.push(expr);
            return Some(remainder);
        }
    }
    // &func() interpolation — only with parentheses
    if rest.starts_with('&') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        if next.is_alphabetic() || next == '_' {
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(var_rest.len());
            let name = &var_rest[..end];
            let after_name = &var_rest[end..];
            // Must be followed by (...) to interpolate
            if after_name.starts_with('(') {
                // Find matching closing paren
                let mut depth = 0usize;
                let mut paren_end = None;
                for (idx, ch) in after_name.char_indices() {
                    if ch == '(' {
                        depth += 1;
                    } else if ch == ')' {
                        depth -= 1;
                        if depth == 0 {
                            paren_end = Some(idx);
                            break;
                        }
                    }
                }
                if let Some(pe) = paren_end {
                    let args_str = &after_name[1..pe];
                    let remainder = &after_name[pe + 1..];
                    // Parse arguments
                    let args = if args_str.trim().is_empty() {
                        vec![]
                    } else {
                        let mut args = vec![];
                        for arg in args_str.split(',') {
                            let arg = arg.trim();
                            if let Ok((_, expr)) = crate::parser::expr::expression(arg) {
                                args.push(expr);
                            }
                        }
                        args
                    };
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    parts.push(Expr::Call {
                        name: Symbol::intern(name),
                        args,
                    });
                    return Some(remainder);
                }
            }
        }
    }
    None
}

fn has_malformed_angle_interpolation(rest: &str) -> bool {
    let mut chars = rest.chars();
    let Some(sigil) = chars.next() else {
        return false;
    };
    if !matches!(sigil, '$' | '@' | '%') {
        return false;
    }
    let mut after_name = chars.as_str();
    let mut saw_name = false;
    while let Some(ch) = after_name.chars().next() {
        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
            saw_name = true;
            after_name = &after_name[ch.len_utf8()..];
        } else {
            break;
        }
    }
    if !saw_name {
        return false;
    }
    if let Some(after_open) = after_name.strip_prefix("<<") {
        return !after_open.contains(">>");
    }
    if let Some(after_open) = after_name.strip_prefix('«') {
        return !after_open.contains('»');
    }
    if let Some(after_open) = after_name.strip_prefix('<') {
        return !after_open.contains('>');
    }
    false
}

/// Assemble interpolation parts into a final expression.
pub(super) fn finalize_interpolation(parts: Vec<Expr>, current: String) -> Expr {
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
pub(in crate::parser) fn interpolate_string_content(content: &str) -> Expr {
    interpolate_string_content_with_modes(content, true, false)
}

pub(in crate::parser) fn interpolate_string_content_with_modes(
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

fn parse_braced_interpolation(input: &str) -> Option<(&str, &str)> {
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

fn parse_single_quote_qq(content: &str) -> Expr {
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

pub(super) fn single_quoted_string(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '\'')?;
    let start = input;
    let mut rest = input;
    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing '"));
        }
        if let Some(after_quote) = rest.strip_prefix('\'') {
            // A quote followed by a combining mark forms a single grapheme cluster,
            // so it is string content, not a closing delimiter.
            if let Some(next_ch) = after_quote.chars().next()
                && unicode_normalization::char::is_combining_mark(next_ch)
            {
                // Skip over the quote + combining mark(s) as content
                rest = &after_quote[next_ch.len_utf8()..];
                // Skip any additional combining marks
                while let Some(ch) = rest.chars().next() {
                    if unicode_normalization::char::is_combining_mark(ch) {
                        rest = &rest[ch.len_utf8()..];
                    } else {
                        break;
                    }
                }
                continue;
            }
            let content = &start[..start.len() - rest.len()];
            return Ok((after_quote, parse_single_quote_qq(content)));
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

/// Parse smart single-quoted string literal (no interpolation).
/// Accepts \u{2018}...\u{2019}, \u{201A}...\u{2019}, \u{201A}...\u{2018}, \u{2019}...\u{2019}, \u{2019}...\u{2018}
pub(super) fn smart_single_quoted_string(input: &str) -> PResult<'_, Expr> {
    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("smart single quote"))?;
    // Determine the set of valid closing characters based on opener
    let closers: &[char] = match first {
        '\u{2018}' => &['\u{2019}'],
        '\u{201A}' => &['\u{2019}', '\u{2018}'],
        '\u{2019}' => &['\u{2019}', '\u{2018}'],
        _ => return Err(PError::expected("smart single quote")),
    };
    let input = &input[first.len_utf8()..];
    let mut rest = input;
    let start = input;
    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing smart single quote"));
        }
        let ch = rest.chars().next().unwrap();
        if closers.contains(&ch) {
            let content = &start[..start.len() - rest.len()];
            return Ok((
                &rest[ch.len_utf8()..],
                Expr::Literal(Value::str(content.to_string())),
            ));
        }
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
        Ok((after, Expr::Literal(Value::str(content.to_string()))))
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
            // A quote followed by a combining mark forms a single grapheme cluster,
            // so it is string content, not a closing delimiter.
            let after_quote = &rest[1..];
            if let Some(next_ch) = after_quote.chars().next()
                && unicode_normalization::char::is_combining_mark(next_ch)
            {
                current.push('"');
                current.push(next_ch);
                rest = &after_quote[next_ch.len_utf8()..];
                while let Some(ch) = rest.chars().next() {
                    if unicode_normalization::char::is_combining_mark(ch) {
                        current.push(ch);
                        rest = &rest[ch.len_utf8()..];
                    } else {
                        break;
                    }
                }
                continue;
            }
            rest = after_quote;
            break;
        }
        if rest.starts_with('\\') && rest.len() > 1 {
            match process_escape_sequence(rest, &mut current, &['"', '{', '}']) {
                Ok(Some((r, needs_continue))) => {
                    rest = r;
                    if needs_continue {
                        continue;
                    }
                }
                Ok(None) => {
                    let c = rest.as_bytes()[1] as char;
                    current.push('\\');
                    current.push(c);
                    rest = &rest[2..];
                }
                Err(msg) => {
                    return Err(PError::fatal(msg));
                }
            }
            continue;
        }
        if has_malformed_angle_interpolation(rest) {
            return Err(PError::expected("closing '>' in interpolated index"));
        }
        if let Some(r) = try_interpolate_var(rest, &mut parts, &mut current) {
            rest = r;
            continue;
        }
        // Block interpolation: { expr }
        if rest.starts_with('{') {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(&mut current))));
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
                let block_src = rest[1..end].trim();
                if !block_src.is_empty() {
                    // Use DoStmt(Block) for scope isolation — block-local `my` doesn't leak
                    if let Ok((sr, stmts)) = crate::parser::stmt::stmt_list_pub(block_src)
                        && sr.trim().is_empty()
                    {
                        parts.push(Expr::DoStmt(Box::new(crate::ast::Stmt::Block(stmts))));
                    } else if let Ok((expr_rest, expr)) = expression(block_src)
                        && expr_rest.trim().is_empty()
                    {
                        parts.push(Expr::DoStmt(Box::new(crate::ast::Stmt::Block(vec![
                            crate::ast::Stmt::Expr(expr),
                        ]))));
                    }
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

/// Parse a Unicode smart-quoted string with interpolation support.
/// Accepts \u{201C}...\u{201D}, \u{201E}...\u{201D}, \u{201E}...\u{201C},
/// \u{201D}...\u{201D}, \u{201D}...\u{201C}
pub(super) fn smart_double_quoted_string(input: &str) -> PResult<'_, Expr> {
    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("smart double quote"))?;
    let closers: &[char] = match first {
        '\u{201C}' => &['\u{201D}'],
        '\u{201E}' => &['\u{201D}', '\u{201C}'],
        '\u{201D}' => &['\u{201D}', '\u{201C}'],
        _ => return Err(PError::expected("smart double quote")),
    };
    let input = &input[first.len_utf8()..];
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = input;

    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing smart double quote"));
        }
        let next_ch = rest.chars().next().unwrap();
        if closers.contains(&next_ch) {
            rest = &rest[next_ch.len_utf8()..];
            break;
        }
        if rest.starts_with('\\') && rest.len() > 1 {
            match process_escape_sequence(rest, &mut current, &['\u{201D}', '{', '}']) {
                Ok(Some((r, needs_continue))) => {
                    rest = r;
                    if needs_continue {
                        continue;
                    }
                }
                Ok(None) => {
                    let c = rest.as_bytes()[1] as char;
                    current.push('\\');
                    current.push(c);
                    rest = &rest[2..];
                }
                Err(msg) => {
                    return Err(PError::fatal(msg));
                }
            }
            continue;
        }
        if has_malformed_angle_interpolation(rest) {
            return Err(PError::expected("closing '>' in interpolated index"));
        }
        if let Some(r) = try_interpolate_var(rest, &mut parts, &mut current) {
            rest = r;
            continue;
        }
        if rest.starts_with('{') {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(&mut current))));
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
                let block_src = rest[1..end].trim();
                if !block_src.is_empty() {
                    if let Ok((sr, stmts)) = crate::parser::stmt::stmt_list_pub(block_src)
                        && sr.trim().is_empty()
                    {
                        parts.push(Expr::DoStmt(Box::new(crate::ast::Stmt::Block(stmts))));
                    } else if let Ok((expr_rest, expr)) = expression(block_src)
                        && expr_rest.trim().is_empty()
                    {
                        parts.push(Expr::DoStmt(Box::new(crate::ast::Stmt::Block(vec![
                            crate::ast::Stmt::Expr(expr),
                        ]))));
                    }
                }
                rest = &rest[end + 1..];
                continue;
            }
        }
        current.push(next_ch);
        rest = &rest[next_ch.len_utf8()..];
    }

    Ok((rest, finalize_interpolation(parts, current)))
}
