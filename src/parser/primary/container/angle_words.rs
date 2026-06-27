use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::{is_non_breaking_space, split_angle_words};
use crate::parser::parse_result::{PError, PResult};
use crate::parser::primary::quote_adverbs::QuoteFlags;
use crate::parser::primary::string::{parse_quotewords_quoted_atom, quotewords_atom_expr};
use crate::symbol::Symbol;

use super::allomorph::{angle_word_expr, angle_word_value_full_allomorphic};

/// Parse a < > quote-word list.
pub(crate) fn angle_list(input: &str) -> PResult<'_, Expr> {
    parse_quote_word_list(input, "<", ">", true, false)
}

/// Parse a « » quote-word list.
pub(crate) fn french_quote_list(input: &str) -> PResult<'_, Expr> {
    parse_quote_word_list(input, "«", "»", false, true)
}

/// Parse a << >> quote-word list.
pub(crate) fn double_angle_list(input: &str) -> PResult<'_, Expr> {
    parse_quote_word_list(input, "<<", ">>", false, true)
}

fn parse_quote_word_list<'a>(
    input: &'a str,
    open: &str,
    close: &str,
    reject_lt_operators: bool,
    quoted_words: bool,
) -> PResult<'a, Expr> {
    let Some(input) = input.strip_prefix(open) else {
        return Err(PError::expected("quote-word list"));
    };
    // For `<...>`, reject leading operator forms like <= and <=>.
    // Allow negative words/numerics such as <-1/0> and word lists starting
    // with a bare hyphen like <- a - b ->.
    if reject_lt_operators
        && (input.starts_with('=')
            || (input.starts_with('-')
                && !input.as_bytes().get(1).copied().is_some_and(|b| {
                    b.is_ascii_alphanumeric()
                        || b == b' '
                        || matches!(b, b'_' | b'/' | b'.' | b'+' | b'-')
                })))
    {
        return Err(PError::expected("angle list"));
    }
    let end = if quoted_words {
        find_quote_word_close(input, close)
    } else if close == ">" {
        find_nested_angle_close(input)
    } else {
        input.find(close)
    };
    let Some(end) = end else {
        return Err(PError::expected("closing quote-word delimiter"));
    };
    let content = &input[..end];
    let rest = &input[end + close.len()..];
    // A bare *empty* `<>` is the obsolete Perl diamond. `< >` (with whitespace)
    // is a legal empty `List`, so only the truly-empty form errors.
    if reject_lt_operators && content.is_empty() {
        return Err(crate::parser::stmt::control::make_obsolete_error(
            "<>",
            None,
            "Unsupported use of <>. In Raku please use: lines() to read input, \
             ('') to represent a null string or () to represent an empty list.",
        ));
    }
    if quoted_words {
        let exprs = split_quotish_words(content)?;
        return Ok((
            rest,
            crate::parser::primary::string::make_word_result_expr(exprs),
        ));
    }
    let words = split_angle_words(content);
    if words.len() == 1 {
        let expr = angle_word_expr(words[0]);
        // Single-word angle brackets: <7+8i> produces plain Complex, not ComplexStr
        // and <2/3> produces plain Rat, not RatStr.
        // However, if the original content had surrounding whitespace (e.g. <01.0+42i >),
        // keep the allomorphic ComplexStr form.
        let has_whitespace = content.trim() != content;
        let expr = match expr {
            Expr::Literal(crate::value::Value::Mixin(inner, _))
                if matches!(inner.as_ref(), crate::value::Value::Complex(..))
                    && !has_whitespace =>
            {
                Expr::Literal(inner.as_ref().clone())
            }
            other => other,
        };
        Ok((rest, expr))
    } else {
        // Multi-element lists: fractions also become allomorphic (RatStr)
        let exprs = words
            .iter()
            .map(|w| Expr::Literal(angle_word_value_full_allomorphic(w)))
            .collect();
        Ok((rest, Expr::ArrayLiteral(exprs)))
    }
}

fn find_quote_word_close(input: &str, close: &str) -> Option<usize> {
    let mut i = 0usize;
    let mut quoted_by: Option<char> = None;
    let mut escaped = false;
    let mut angle_depth = 0usize;
    while i < input.len() {
        let rest = &input[i..];
        if quoted_by.is_none()
            && angle_depth == 0
            && rest.starts_with(close)
            && !rest
                .strip_prefix(close)
                .is_some_and(|after| close == ">>" && after.starts_with('>'))
        {
            return Some(i);
        }
        if quoted_by.is_none() && rest.starts_with("#`") {
            let after = skip_quotish_embedded_comment(&rest[2..])?;
            i = input.len() - after.len();
            continue;
        }
        if quoted_by.is_none() && rest.starts_with('#') {
            let end = rest.find('\n').unwrap_or(rest.len());
            i += end;
            continue;
        }
        let mut chars = rest.chars();
        let ch = chars.next()?;
        let ch_len = ch.len_utf8();
        if let Some(quote) = quoted_by {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == quote {
                quoted_by = None;
            }
        } else if ch == '"'
            || ch == '\''
            || ch == '\u{201C}'
            || ch == '\u{201D}'
            || ch == '\u{201E}'
            || ch == '\u{2018}'
            || ch == '\u{2019}'
            || ch == '\u{201A}'
        {
            quoted_by = Some(match ch {
                '"' => '"',
                '\'' => '\'',
                '\u{201C}' => '\u{201D}',
                '\u{201D}' => '\u{201D}',
                '\u{201E}' => '\u{201D}',
                '\u{2018}' => '\u{2019}',
                '\u{2019}' => '\u{2019}',
                '\u{201A}' => '\u{2019}',
                _ => unreachable!(),
            });
        } else if close == ">>" {
            if ch == '<' {
                angle_depth += 1;
            } else if ch == '>' && angle_depth > 0 {
                angle_depth -= 1;
            }
        }
        i += ch_len;
    }
    None
}

/// Find the closing `>` for `<...>`, handling nested `<>` pairs
/// (e.g. `<:13<01>/:13<07>>`).
pub(crate) fn find_nested_angle_close_pub(input: &str) -> Option<usize> {
    find_nested_angle_close(input)
}

fn find_nested_angle_close(input: &str) -> Option<usize> {
    let mut depth: usize = 0;
    let mut i = 0usize;
    let bytes = input.as_bytes();
    while i < bytes.len() {
        let b = bytes[i];
        // Backslash escapes the next byte (allows `\<`, `\>`, `\\` etc.)
        if b == b'\\' && i + 1 < bytes.len() {
            i += 2;
            continue;
        }
        if b == b'<' {
            depth += 1;
        } else if b == b'>' {
            if depth == 0 {
                return Some(i);
            }
            depth -= 1;
        }
        i += 1;
    }
    None
}

fn split_quotish_words(content: &str) -> Result<Vec<Expr>, PError> {
    let mut words = Vec::new();
    let mut rest = content;
    let flags = QuoteFlags::qq_double();
    loop {
        rest = trim_quotish_ws_and_comments(rest)?;
        if rest.is_empty() {
            break;
        }
        if let Some((r, quoted)) = parse_quoted_word(rest)? {
            words.push(quoted);
            rest = r;
            continue;
        }
        let word_len = find_quotish_word_end(rest);
        let (word, r) = rest.split_at(word_len);
        if word.starts_with(':')
            && let Ok((remaining, expr)) = expression(word)
            && remaining.is_empty()
        {
            words.push(Expr::MethodCall {
                target: Box::new(expr),
                name: Symbol::intern("item"),
                args: vec![],
                modifier: None,
                quoted: false,
            });
        } else if word.contains('$')
            || word.contains('@')
            || word.contains('%')
            || word.contains('&')
            || word.contains('{')
            || word.contains('\\')
        {
            let expr =
                crate::parser::primary::quote_adverbs::process_content_with_flags(word, &flags);
            words.push(quotewords_atom_expr(expr));
        } else {
            // «...» and <<...>> are equivalent to qqww:v, so fractions
            // also produce allomorphic types (RatStr).
            words.push(Expr::Literal(angle_word_value_full_allomorphic(word)));
        }
        rest = r;
    }
    Ok(words)
}

fn trim_quotish_ws_and_comments(mut input: &str) -> Result<&str, PError> {
    loop {
        let trimmed = trim_breaking_ws(input);
        if trimmed.len() != input.len() {
            input = trimmed;
            continue;
        }
        if let Some(after) = input.strip_prefix("#`") {
            input = skip_quotish_embedded_comment(after)
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

fn skip_quotish_embedded_comment(input: &str) -> Option<&str> {
    let mut chars = input.chars();
    let open = chars.next()?;
    let close = match open {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        '\u{00AB}' => '\u{00BB}',
        '\u{201C}' => '\u{201D}',
        '\u{201D}' => '\u{201D}',
        '\u{201E}' => '\u{201D}',
        '\u{2018}' => '\u{2019}',
        '\u{2019}' => '\u{2019}',
        '\u{201A}' => '\u{2019}',
        _ => return None,
    };
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

fn find_quotish_word_end(input: &str) -> usize {
    for (idx, c) in input.char_indices() {
        if (c.is_whitespace() && !is_non_breaking_space(c))
            || c == '#'
            || c == '"'
            || c == '\''
            || c == '\u{201C}'
            || c == '\u{2018}'
            || c == '\u{201D}'
            || c == '\u{2019}'
            || c == '\u{201E}'
            || c == '\u{201A}'
            || c == '\u{FF62}'
        {
            return idx;
        }
    }
    input.len()
}

fn trim_breaking_ws(input: &str) -> &str {
    let mut idx = 0usize;
    for (i, c) in input.char_indices() {
        if !c.is_whitespace() || is_non_breaking_space(c) {
            idx = i;
            break;
        }
        idx = i + c.len_utf8();
    }
    &input[idx..]
}

fn parse_quoted_word(input: &str) -> Result<Option<(&str, Expr)>, PError> {
    parse_quotewords_quoted_atom(input)
}
