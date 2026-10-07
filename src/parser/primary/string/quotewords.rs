use super::*;
use crate::ast::Expr;
use crate::parser::parse_result::PError;

use super::helpers::literal_str;
pub(crate) fn parse_quotewords_items(
    content: &str,
    flags: &crate::parser::primary::quote_adverbs::QuoteFlags,
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
        let atom_expr =
            crate::parser::primary::quote_adverbs::process_content_with_flags(atom, flags);
        items.push(quotewords_atom_expr_allomorphic(atom_expr, flags.val));
        rest = next;
    }
    Ok(items)
}

pub(crate) fn trim_quotewords_ws_and_comments(mut input: &str) -> Result<&str, PError> {
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

pub(crate) fn skip_quotewords_embedded_comment(input: &str) -> Option<&str> {
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

pub(crate) fn parse_quotewords_quoted_atom(input: &str) -> Result<Option<(&str, Expr)>, PError> {
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

pub(crate) fn find_quotewords_atom_end(input: &str) -> usize {
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

pub(crate) fn parse_quotewords_unicode_quoted_atom(input: &str) -> Option<(&str, Expr)> {
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

pub(crate) fn parse_quotewords_single_quote_atom(input: &str) -> Option<(&str, Expr)> {
    let (rest, content) = parse_quotewords_quote_span(input, &['‘', '’'])?;
    Some((rest, Expr::Literal(literal_str(content.to_string()))))
}

pub(crate) fn parse_quotewords_quote_span<'a>(
    input: &'a str,
    closers: &[char],
) -> Option<(&'a str, &'a str)> {
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

/// One piece of an interpolating `quotewords` text, as RakuAST segments it.
pub(crate) enum QuoteWordsPart {
    /// An unquoted run (words and the whitespace between them), interpolated
    /// as one `qq` string.
    Text(Expr),
    /// A quoted word (`"b c"`), a `QuoteWordsAtom` over its own string.
    Atom(Expr),
}

/// Split the raw text of an interpolating `quotewords` quote into the pieces
/// rakudo makes segments of: each quoted word on its own, every run between
/// them interpolated as one string.
// Cost: O(n), n = length of `content`.
pub(crate) fn quotewords_spelled_parts(content: &str) -> Option<Vec<QuoteWordsPart>> {
    let mut parts = Vec::new();
    let mut run_start = 0;
    let mut pos = 0;
    let flush = |parts: &mut Vec<QuoteWordsPart>, from: usize, to: usize| {
        if from < to {
            parts.push(QuoteWordsPart::Text(crate::parser::interpolate_qq_content(
                &content[from..to],
            )));
        }
    };
    while pos < content.len() {
        let rest = &content[pos..];
        if let Some((next, quoted)) = parse_quotewords_quoted_atom(rest).ok()? {
            flush(&mut parts, run_start, pos);
            parts.push(QuoteWordsPart::Atom(quoted));
            pos = content.len() - next.len();
            run_start = pos;
            continue;
        }
        let atom_len = find_quotewords_atom_end(rest);
        // Whitespace, or a quote character that opens no quoted word.
        let step = match atom_len {
            0 => rest.chars().next().map_or(1, char::len_utf8),
            n => n,
        };
        pos += step;
    }
    flush(&mut parts, run_start, content.len());
    Some(parts)
}
