//! `$foo=ATOM` in a regex: an alias to a variable.
//!
//! Rakudo's grammar (`metachar:sym<var>`) parses a variable interpolation
//! optionally followed by `=` and a quantified atom, and its actions keep only
//! the interpolation: the atom is dropped. S05 intends the atom's match to be
//! assigned to the variable, but neither Rakudo nor mutsu does that. Without
//! this the interpolated value was followed by a literal `=` and the atom
//! (#14628).

/// The index just past a `= ATOM QUANT?` that follows an interpolated variable
/// whose text ends at `j`, or `None` when no alias follows (a `==`, or an atom
/// whose end this scan cannot find, keeps the plain interpolation).
// Cost: O(n), n = source consumed by the alias.
pub(super) fn variable_alias_end(chars: &[char], j: usize) -> Option<usize> {
    let mut k = skip_ws(chars, j);
    if chars.get(k) != Some(&'=') || matches!(chars.get(k + 1), Some('=' | '~' | '>')) {
        return None;
    }
    k = skip_ws(chars, k + 1);
    let atom_end = atom_end(chars, k)?;
    Some(quantifier_end(chars, atom_end))
}

fn skip_ws(chars: &[char], mut k: usize) -> usize {
    while chars.get(k).is_some_and(|c| c.is_whitespace()) {
        k += 1;
    }
    k
}

/// The end of the single atom at `k`: a bracketed group, an assertion, a
/// quoted literal, an escape, a variable, or one literal character.
fn atom_end(chars: &[char], k: usize) -> Option<usize> {
    let first = *chars.get(k)?;
    match first {
        '[' => matching_close(chars, k, '[', ']'),
        '(' => matching_close(chars, k, '(', ')'),
        '{' => matching_close(chars, k, '{', '}'),
        '<' => matching_close(chars, k, '<', '>'),
        '\'' | '"' => {
            let mut e = k + 1;
            while let Some(&c) = chars.get(e) {
                if c == '\\' {
                    e += 2;
                } else if c == first {
                    return Some(e + 1);
                } else {
                    e += 1;
                }
            }
            None
        }
        '\\' => {
            let escape = *chars.get(k + 1)?;
            if matches!(escape, 'x' | 'X' | 'c' | 'C' | 'o' | 'O') && chars.get(k + 2) == Some(&'[')
            {
                matching_close(chars, k + 2, '[', ']')
            } else {
                Some(k + 2)
            }
        }
        '$' | '@' => {
            let mut e = k + 1;
            if chars.get(e) == Some(&'<') {
                return matching_close(chars, e, '<', '>');
            }
            while chars
                .get(e)
                .is_some_and(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '*' | '.'))
            {
                e += 1;
            }
            (e > k + 1).then_some(e)
        }
        c if c.is_whitespace() || matches!(c, '|' | '&' | ')' | ']' | '}' | '>' | ';' | '=') => None,
        _ => Some(k + 1),
    }
}

/// One past the `close` matching the `open` at `start`, nested pairs counted,
/// backslash escapes and quoted text skipped.
fn matching_close(chars: &[char], start: usize, open: char, close: char) -> Option<usize> {
    let mut depth = 0usize;
    let mut e = start;
    while let Some(&c) = chars.get(e) {
        if c == '\\' {
            e += 2;
            continue;
        }
        if c == '\'' || c == '"' {
            let mut q = e + 1;
            while let Some(&d) = chars.get(q) {
                if d == '\\' {
                    q += 2;
                } else if d == c {
                    break;
                } else {
                    q += 1;
                }
            }
            e = q + 1;
            continue;
        }
        if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return Some(e + 1);
            }
        }
        e += 1;
    }
    None
}

/// The end of an optional quantifier (`*`, `+`, `?`, `**N`, `**N..M`, `**{...}`)
/// and its `?` / `:` modifier after the atom ending at `e`.
fn quantifier_end(chars: &[char], e: usize) -> usize {
    let mut k = e;
    match chars.get(k) {
        Some('*') if chars.get(k + 1) == Some(&'*') => {
            k += 2;
            if chars.get(k) == Some(&'{') {
                return matching_close(chars, k, '{', '}').unwrap_or(e);
            }
            while chars
                .get(k)
                .is_some_and(|c| c.is_ascii_digit() || matches!(c, '.' | '*'))
            {
                k += 1;
            }
        }
        Some('*' | '+' | '?') => k += 1,
        _ => return e,
    }
    if matches!(chars.get(k), Some('?' | ':')) {
        k += 1;
    }
    k
}

#[cfg(test)]
mod tests {
    use super::variable_alias_end;

    fn end(text: &str, at: usize) -> Option<usize> {
        let chars: Vec<char> = text.chars().collect();
        variable_alias_end(&chars, at)
    }

    #[test]
    fn an_alias_drops_its_atom() {
        assert_eq!(end("$foo=[<two>] d", 4), Some(12));
        assert_eq!(end("$foo = [<two>] d", 4), Some(14));
        assert_eq!(end("$foo=x d", 4), Some(6));
        assert_eq!(end("$foo=<two>+ d", 4), Some(11));
        assert_eq!(end("$foo=. d", 4), Some(6));
        assert_eq!(end("$foo='a b'* d", 4), Some(11));
    }

    #[test]
    fn no_alias_keeps_the_interpolation() {
        assert_eq!(end("$foo d", 4), None);
        assert_eq!(end("$foo == 1", 4), None);
        assert_eq!(end("$foo=", 4), None);
        assert_eq!(end("$foo=|x", 4), None);
    }
}
