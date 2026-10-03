//! A regex literal nested inside a regex's embedded code block.

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
