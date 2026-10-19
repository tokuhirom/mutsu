//! A regex's embedded code block (`{ … }`, `<{ … }>`, `<?{ … }>`): where it
//! ends, and a regex literal nested inside it.

/// The index of the `}` closing a code block whose body `rest` starts right
/// after the opening `{`, or `None` when the block never closes. Every regex
/// scanner that has to find the end of a code block asks this one, so they
/// cannot disagree about where it ends.
///
/// Inside the body a brace counts only as code: braces inside a quoted
/// string (`{ say "a}b" }`), a `#` line comment, or a nested regex literal
/// (`{ $s ~~ / '}' / }`) do not. An apostrophe opens no string inside a
/// comment (`# Don't …`) or an identifier (`don't`, which Raku allows
/// between two letters) (#13928). A `#` followed by a backtick opens an
/// embedded comment, whose bracket is left to the count.
// Cost: O(n), n = chars scanned.
pub(crate) fn code_block_body_len(rest: &[char]) -> Option<usize> {
    let mut depth = 1usize;
    // The last non-whitespace code character, to tell a nested regex
    // literal's opening `/` from a division.
    let mut prev_sig = '{';
    let mut i = 0usize;
    while i < rest.len() {
        let ch = rest[i];
        match ch {
            '\\' => {
                prev_sig = ch;
                i += 2;
                continue;
            }
            '#' if rest.get(i + 1) != Some(&'`') => {
                while i < rest.len() && rest[i] != '\n' {
                    i += 1;
                }
                continue;
            }
            '/' if slash_opens_regex_after(prev_sig) => {
                if let Some(len) = nested_regex_literal_len(&rest[i + 1..]) {
                    prev_sig = '/';
                    i += 1 + len;
                    continue;
                }
            }
            '\'' if i > 0
                && rest[i - 1].is_alphanumeric()
                && rest.get(i + 1).is_some_and(|next| next.is_alphabetic()) => {}
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {
                if let Some(closer) = crate::regex_qq_atoms::quote_closer(ch) {
                    i += 1;
                    while i < rest.len() && rest[i] != closer {
                        if rest[i] == '\\' {
                            i += 1;
                        }
                        i += 1;
                    }
                    prev_sig = closer;
                    i += 1;
                    continue;
                }
            }
        }
        if !ch.is_whitespace() {
            prev_sig = ch;
        }
        i += 1;
    }
    None
}

/// Does a `/` right after code character `prev` open a regex literal rather
/// than divide? It does in term position: after an infix or prefix operator
/// character or an opening bracket or separator. (`$/` is not reached: its
/// `$` is not in the set.)
// Cost: O(1).
pub(crate) fn slash_opens_regex_after(prev: char) -> bool {
    matches!(
        prev,
        '~' | '=' | '(' | ',' | '{' | '[' | '|' | '&' | '!' | '?' | ':' | ';'
    )
}

/// The length, in chars, of a nested `/ … /` regex literal's body plus its
/// closing `/`, where `rest` starts right after the opening `/`. A character
/// class (`<[ … ]>`) is literal text, so a quote in it (`<["']>`) does not open
/// a string; outside a class a quoted string is skipped whole, and `\` escapes
/// the next character. `None` when the literal never closes.
// Cost: O(n), n = chars scanned.
pub(crate) fn nested_regex_literal_len(rest: &[char]) -> Option<usize> {
    let mut i = 0usize;
    while i < rest.len() {
        match rest[i] {
            '\\' => i += 2,
            '/' => return Some(i + 1),
            '<' if rest.get(i + 1) == Some(&'[')
                || (matches!(rest.get(i + 1), Some('-' | '+' | '!'))
                    && rest.get(i + 2) == Some(&'[')) =>
            {
                // Skip to the class's closing `]>`.
                i += 2;
                while i < rest.len() {
                    if rest[i] == '\\' {
                        i += 2;
                        continue;
                    }
                    if rest[i] == ']' && rest.get(i + 1) == Some(&'>') {
                        i += 2;
                        break;
                    }
                    i += 1;
                }
            }
            q @ ('\'' | '"') => {
                i += 1;
                while i < rest.len() && rest[i] != q {
                    if rest[i] == '\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
            }
            _ => i += 1,
        }
    }
    None
}
