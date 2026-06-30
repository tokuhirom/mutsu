use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::token_kind::{lookup_emoji_sequence, lookup_unicode_char_by_name};
use crate::value::Value;

pub(crate) fn single_quoted_string(input: &str) -> PResult<'_, Expr> {
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
pub(crate) fn smart_single_quoted_string(input: &str) -> PResult<'_, Expr> {
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
    // Describe-by-adverb / goal text for an unterminated string, matching
    // Rakudo's X::Comp::FailGoal for these smart-quote forms.
    let (dba, goal): (&str, &str) = match first {
        '\u{201A}' => ("low curly single quotes", "<[\u{2019}\u{2018}]>"),
        // `\u{2018}`/`\u{2019}` both report as plain curly single quotes.
        _ => ("curly single quotes", "'\u{2019}'"),
    };
    let input = &input[first.len_utf8()..];
    let mut rest = input;
    let start = input;
    loop {
        if rest.is_empty() {
            return Err(crate::parser::primary::container::fail_goal_error_at(
                dba,
                goal,
                Some(rest),
            ));
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
pub(crate) fn corner_bracket_string(input: &str) -> PResult<'_, Expr> {
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
pub(crate) fn parse_backslash_c_bracket(rest: &str) -> Option<(String, &str)> {
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
pub(crate) fn double_quoted_string(input: &str) -> PResult<'_, Expr> {
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
                    let c = rest[1..].chars().next().unwrap();
                    return Err(unrecognized_backslash_perror(c));
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
            // A `{` with no matching `}` anywhere in the remaining source is an
            // unterminated closure interpolation: the closing `"` is swallowed by
            // the never-closed block, so the string is itself unterminated.
            // Matches rakudo's X::Comp::FailGoal ("couldn't find final '\"'").
            return Err(crate::parser::primary::container::fail_goal_error_at(
                "double quotes",
                "'\"'",
                Some(&rest[rest.len()..]),
            ));
        }
        // A `$` that reached here did not introduce a variable or `${...}`/
        // `{...}` interpolation, so it is a non-variable dollar and must be
        // backslashed (Raku: "Non-variable $ must be backslashed"). A `$`
        // immediately followed by another sigil (`$@arr`, `$$aref`, `$%hash`,
        // `$&code`) is a contextualizer prefix on the following sigil-variable,
        // not a bare dollar — leave it so the sigil-variable interpolates on
        // the next iteration.
        if rest.starts_with('$') && !rest[1..].starts_with(['$', '@', '%', '&']) {
            return Err(non_variable_dollar_perror());
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
pub(crate) fn smart_double_quoted_string(input: &str) -> PResult<'_, Expr> {
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
    // Describe-by-adverb / goal text for an unterminated string, matching
    // Rakudo's X::Comp::FailGoal for these smart-quote forms.
    let (dba, goal): (&str, &str) = match first {
        '\u{201E}' => ("low curly double quotes", "<[\u{201D}\u{201C}]>"),
        // `\u{201C}`/`\u{201D}` both report as plain curly double quotes.
        _ => ("curly double quotes", "'\u{201D}'"),
    };
    let input = &input[first.len_utf8()..];
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = input;

    loop {
        if rest.is_empty() {
            return Err(crate::parser::primary::container::fail_goal_error_at(
                dba,
                goal,
                Some(rest),
            ));
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
                    let c = rest[1..].chars().next().unwrap();
                    return Err(unrecognized_backslash_perror(c));
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
