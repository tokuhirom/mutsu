//! Delimiter scanning: scan_to_delim and its inner implementation.
//!
//! scan_to_delim_inner is a single indivisible ~250-line function that handles
//! all Raku-specific delimiter nesting (character classes, angle bracket assertions,
//! embedded code, quoted strings inside regexes, comment skipping, etc.).

fn is_regex_quote_open(ch: char) -> bool {
    matches!(
        ch,
        '\'' | '"' | '\u{2018}' | '\u{201A}' | '\u{201C}' | '\u{201E}' | '\u{FF62}'
    )
}

fn is_regex_quote_terminator(open: char, ch: char) -> bool {
    match open {
        '\'' => ch == '\'',
        '"' => ch == '"',
        '\u{2018}' => ch == '\u{2019}',                     // '...'
        '\u{201A}' => ch == '\u{2019}' || ch == '\u{2018}', // ‚...' and ‚...'
        '\u{201C}' => ch == '\u{201D}',                     // "..."
        '\u{201E}' => ch == '\u{201D}',                     // „..."
        '\u{FF62}' => ch == '\u{FF63}',                     // ｢...｣
        _ => false,
    }
}

/// Scan `input` for content delimited by `close_ch`, handling backslash escapes,
/// single-quoted strings, and paired-delimiter nesting.
/// Returns `(content, rest_after_close)` or None.
pub(in crate::parser) fn scan_to_delim(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
) -> Option<(&str, &str)> {
    scan_to_delim_inner(input, open_ch, close_ch, is_paired, false, false)
}

/// Scan the *replacement* half of a substitution (`s/pat/REPL/`).
///
/// The replacement is a qq-like string, NOT a regex, so the regex-specific
/// constructs that `scan_to_delim` recognizes — character classes `<[...]>`,
/// `<...>` assertions, `<<`/`>>` word boundaries, `#` comments, and bare
/// single/double-quoted atoms — are all literal text here. Only these affect
/// where the closing delimiter is found:
/// - `\` escapes the next char,
/// - `{...}` interpolated closures skip a balanced (string-aware) brace block,
/// - `$(...)`/`@(...)` interpolation skips a balanced paren block,
/// - for paired delimiters, a nested open delimiter raises the depth.
///
/// Without this, `s:g/ '[' /<[/` (replacement is the literal text `<[`) would
/// have `scan_to_delim` treat `<[` as the start of a `<[...]>` character class
/// and scan past the closing `/`, breaking the whole expression.
pub(in crate::parser) fn scan_to_delim_replacement(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
) -> Option<(&str, &str)> {
    let mut depth = 1u32;
    let mut chars = input.char_indices();
    while let Some((i, c)) = chars.next() {
        if c == close_ch {
            // Skip '.' when it's part of '..' (range operator).
            if close_ch == '.' && input[i + 1..].starts_with('.') {
                chars.next();
                continue;
            }
            depth -= 1;
            if depth == 0 {
                return Some((&input[..i], &input[i + c.len_utf8()..]));
            }
        } else if is_paired && c == open_ch {
            depth += 1;
        } else if c == '{' {
            // Interpolated closure `{ ... }`: skip a balanced brace block so a
            // delimiter char inside it (including inside a nested string) does
            // not end the replacement early (e.g. `s/x/{ "a/b" }/`). Only
            // reached when '{' is not itself the paired delimiter (that case is
            // handled by the depth bump above).
            skip_interp_block(&mut chars)?;
        } else if (c == '$' || c == '@') && input[i + c.len_utf8()..].starts_with('(') {
            // `$(...)` / `@(...)` interpolation: skip the balanced paren block.
            chars.next(); // consume '('
            let mut paren_depth = 1u32;
            loop {
                match chars.next() {
                    Some((_, '(')) => paren_depth += 1,
                    Some((_, ')')) => {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            break;
                        }
                    }
                    Some((_, '\\')) => {
                        chars.next();
                    }
                    Some(_) => {}
                    None => return None,
                }
            }
        } else if c == '$' && !is_paired && input[i + 1..].starts_with(close_ch) {
            // `$/` — the match variable — followed by the close delimiter. When
            // it is immediately followed by a postfix `.`/`[`/`<` the delimiter
            // is part of `$/` (e.g. `$/.chars()`, `$/[0]`, `$/<k>`), not the end
            // of the replacement, so skip it. Mirrors the same disambiguation in
            // `scan_to_delim`.
            let after = &input[i + 1..];
            let after_delim = &after[close_ch.len_utf8()..];
            if after_delim.starts_with('[')
                || after_delim.starts_with('.')
                || after_delim.starts_with('<')
            {
                chars.next(); // skip the delimiter char (it is part of $/)
            }
        } else if c == '\\' {
            chars.next();
        }
    }
    None
}

/// Skip a balanced `{ ... }` block, having already consumed the opening `{`.
/// String literals inside are skipped whole so a `}` within a string does not
/// close the block early. Returns None if the braces never balance.
fn skip_interp_block(chars: &mut std::str::CharIndices<'_>) -> Option<()> {
    let mut brace_depth = 1u32;
    while let Some((_, ch)) = chars.next() {
        match ch {
            '{' => brace_depth += 1,
            '}' => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    return Some(());
                }
            }
            '\\' => {
                chars.next();
            }
            '\'' | '"' => {
                let quote = ch;
                loop {
                    match chars.next() {
                        Some((_, '\\')) => {
                            chars.next();
                        }
                        Some((_, c2)) if c2 == quote => break,
                        Some(_) => {}
                        None => return None,
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Like `scan_to_delim` but for the *pattern* half of a substitution
/// (`s/pattern/replacement/`). There the closing delimiter is a mandatory
/// separator, so a trailing `$` is always the end-of-string anchor — never the
/// `$/` match variable. Disables the `$/` disambiguation heuristic that
/// `scan_to_delim` applies for match regexes, which otherwise swallows the
/// separator in `s/foo$/.bar/` (a `$` anchor followed by a replacement starting
/// with `.`/`[`/`<`).
pub(in crate::parser) fn scan_to_delim_subst_pattern(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
) -> Option<(&str, &str)> {
    scan_to_delim_inner(input, open_ch, close_ch, is_paired, false, true)
}

/// Like `scan_to_delim` but with an option to skip Raku-specific handling
/// (angle brackets, single-quoted strings, `$` variable detection).
/// In P5 mode, only backslash escapes and the close delimiter are significant.
pub(in crate::parser) fn scan_to_delim_p5(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
) -> Option<(&str, &str)> {
    scan_to_delim_inner(input, open_ch, close_ch, is_paired, true, false)
}

fn scan_to_delim_inner(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
    p5_mode: bool,
    subst_pattern: bool,
) -> Option<(&str, &str)> {
    let mut depth = 1u32;
    let mut chars = input.char_indices();
    while let Some((i, c)) = chars.next() {
        if c == close_ch {
            // Skip '.' when it's part of '..' (range operator)
            if close_ch == '.' && input[i + 1..].starts_with('.') {
                chars.next(); // skip the second '.'
                continue;
            }
            depth -= 1;
            if depth == 0 {
                return Some((&input[..i], &input[i + c.len_utf8()..]));
            }
        } else if is_paired && c == open_ch {
            depth += 1;
        } else if !p5_mode && c == '#' {
            // # starts a comment in Raku regex.
            // #`[...] is an embedded comment (bracket-delimited).
            // Plain # is a line comment (until end of line).
            if let Some((_, '`')) = chars.clone().next() {
                chars.next(); // skip `
                if let Some((_, bracket)) = chars.next() {
                    // `#`«...»`, `#`[...]`, etc. — any bracket pair Raku accepts.
                    // Falls back to the same char for a non-bracket delimiter.
                    let close =
                        crate::parser::helpers::matching_bracket(bracket).unwrap_or(bracket);
                    let mut embed_depth = 1u32;
                    for (_, ch) in chars.by_ref() {
                        if ch == bracket && bracket != close {
                            embed_depth += 1;
                        } else if ch == close {
                            embed_depth -= 1;
                            if embed_depth == 0 {
                                break;
                            }
                        }
                    }
                }
            } else {
                for (_, ch) in chars.by_ref() {
                    if ch == '\n' {
                        break;
                    }
                }
            }
        } else if !p5_mode && c == '<' && input[i + 1..].starts_with('<') {
            // << is a left word boundary assertion — skip both chars
            chars.next(); // consume second <
        } else if !p5_mode && c == '>' && input[i + 1..].starts_with('>') {
            // >> is a right word boundary assertion — skip both chars
            chars.next(); // consume second >
        } else if !p5_mode
            && c == '<'
            && (input[i + 1..].starts_with('[')
                || input[i + 1..].starts_with("-[")
                || input[i + 1..].starts_with("+[")
                || input[i + 1..].starts_with("!["))
        {
            // Skip character class <[...]>, <-[...]>, <+[...]>, <![...]> content
            // without interpreting quotes. Handles <['"]>, <-["\\\t]>, etc.
            // Advance past any prefix chars before '['
            loop {
                if let Some((_, ch)) = chars.next() {
                    if ch == '[' {
                        break;
                    }
                } else {
                    return None;
                }
            }
            // Inside a Raku character class like <[...]>, <-[...]+[...]>, etc.
            // We scan bracket groups sequentially. Inside each [...], an
            // unescaped '[' is a literal character (only '\]' escapes ']').
            // After ']', we check for compound class operators (+[, -[) or
            // the closing '>'.
            'char_class: loop {
                // Scan inside a [...] group until unescaped ']'
                loop {
                    match chars.next() {
                        Some((_, '\\')) => {
                            chars.next(); // skip escaped char
                        }
                        Some((_, ']')) => {
                            break; // end of this bracket group
                        }
                        Some(_) => {}
                        None => return None,
                    }
                }
                // After ']', check for compound class or closing '>'
                let saved = chars.clone();
                match chars.next() {
                    Some((_, '>')) => break, // done
                    Some((_, '+' | '-')) => {
                        if let Some((_, '[')) = chars.next() {
                            continue 'char_class;
                        }
                        // Not a compound group; try consuming '>'
                        chars = saved;
                        if let Some((_, '>')) = chars.next() {
                            break;
                        }
                        break;
                    }
                    Some((_, '[')) => continue 'char_class,
                    _ => {
                        chars = saved;
                        if let Some((_, '>')) = chars.next() {
                            break;
                        }
                        break;
                    }
                }
            }
        } else if !p5_mode
            && c == '<'
            && !input[i + 1..].starts_with('[')
            && !input[i + 1..].starts_with('(')
        {
            // Track angle bracket nesting for regex constructs.
            // Prevents # inside <...> from being treated as a comment,
            // and { } inside <?{...}> from affecting brace depth.
            // This prevents / inside <:name(/:s .../)> from closing the regex.
            let remaining = &input[i + 1..];
            if remaining.starts_with("?{")
                || remaining.starts_with("!{")
                || remaining.starts_with('{')
            {
                // Code assertion/interpolation: <?{...}>, <!{...}>, or <{...}>
                // Skip the '?' or '!' prefix if present, then the brace-delimited block
                if remaining.starts_with("?{") || remaining.starts_with("!{") {
                    chars.next(); // skip ? or !
                }
                chars.next(); // skip {
                let mut brace_depth = 1u32;
                loop {
                    match chars.next() {
                        Some((_, '{')) => brace_depth += 1,
                        Some((_, '}')) => {
                            brace_depth -= 1;
                            if brace_depth == 0 {
                                break;
                            }
                        }
                        Some((_, '\\')) => {
                            chars.next();
                        }
                        Some(_) => {}
                        None => return None,
                    }
                }
                // Consume the closing >
                if let Some((_, '>')) = chars.next() {
                    // done
                }
            } else {
                // Named assertions, Unicode props, etc.: track <> depth
                // Also track (...) so that > inside parens (e.g. => in args)
                // doesn't prematurely close the angle brackets.
                let mut angle_depth = 1u32;
                let mut paren_depth = 0u32;
                loop {
                    match chars.next() {
                        Some((_, '(')) => paren_depth += 1,
                        Some((_, ')')) => paren_depth = paren_depth.saturating_sub(1),
                        Some((_, '<')) if paren_depth == 0 => angle_depth += 1,
                        Some((_, '>')) if paren_depth == 0 => {
                            angle_depth -= 1;
                            if angle_depth == 0 {
                                break;
                            }
                        }
                        Some((_, '\\')) => {
                            chars.next();
                        }
                        Some(_) => {}
                        None => return None,
                    }
                }
            }
        } else if !p5_mode && is_regex_quote_open(c) {
            // Skip quoted string content in regex (e.g., '/' or '\\').
            // This prevents delimiters inside string atoms like m/ "/" ** 2 /
            // from prematurely ending the regex literal.
            loop {
                match chars.next() {
                    Some((_, '\\')) => {
                        chars.next(); // skip escaped char
                    }
                    Some((_, ch)) if is_regex_quote_terminator(c, ch) => break,
                    Some(_) => {}
                    None => return None,
                }
            }
        } else if !p5_mode && !subst_pattern && c == '$' && !is_paired {
            // In non-paired delimiters (like /), $ followed by the close
            // delimiter MIGHT be a variable reference ($/ is the match variable)
            // or it might be the end-of-string anchor followed by the closing
            // delimiter. Disambiguate: if $/ is followed by [ or . or < it's
            // the variable; otherwise it's anchor + close. Skipped for a
            // substitution pattern, where the delimiter always separates and a
            // trailing `$` is unambiguously the anchor (`s/foo$/.bar/`).
            let after = &input[i + 1..];
            if after.starts_with(close_ch) {
                let after_delim = &after[close_ch.len_utf8()..];
                if after_delim.starts_with('[')
                    || after_delim.starts_with('.')
                    || after_delim.starts_with('<')
                {
                    chars.next(); // skip the delimiter char (it's part of $/)
                }
            }
        } else if !p5_mode && (c == '@' || c == '$') && !is_paired {
            // @(...) or $(...) parenthesized expressions inside regex.
            // Track parenthesis depth so that delimiters (like /) inside
            // the expression don't prematurely close the regex.
            let after = &input[i + c.len_utf8()..];
            if after.starts_with('(') {
                chars.next(); // skip '('
                let mut paren_depth = 1u32;
                loop {
                    match chars.next() {
                        Some((_, '(')) => paren_depth += 1,
                        Some((_, ')')) => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
                                break;
                            }
                        }
                        Some((_, '\\')) => {
                            chars.next();
                        }
                        Some(_) => {}
                        None => return None,
                    }
                }
            }
        } else if c == '\\' {
            // skip next char
            chars.next();
        }
    }
    None
}
