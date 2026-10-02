//! Splitting a compacted quantified-atom pattern (`atom**2..3 % sep`) into its
//! parts for the string-based LTM expansion in `regex_parse_ltm`.
//!
//! These scanners replace two `regex` crate patterns (#10439); they reproduce
//! that crate's leftmost, lazy-atom answers exactly, which the tests at the
//! bottom pin against the original patterns.

/// The parts of `^(.+?)\*\*(\??)(COUNT)(?:(%%|%)(.+))?$`, where `COUNT` is
/// `\^?[0-9_]+(?:\^?\.\.(?:\^?[0-9_]+|\*))?`.
#[derive(Debug, PartialEq, Eq)]
pub(super) struct CountedAtom<'a> {
    pub atom: &'a str,
    pub frugal: bool,
    pub count: &'a str,
    /// `%` or `%%` and the separator text after it.
    pub sep: Option<(&'a str, &'a str)>,
}

/// `\^?[0-9_]+` at the start of `s`: its length.
fn count_bound_len(s: &str) -> Option<usize> {
    let caret = usize::from(s.starts_with('^'));
    let digits = s[caret..]
        .bytes()
        .take_while(|b| b.is_ascii_digit() || *b == b'_')
        .count();
    (digits > 0).then_some(caret + digits)
}

/// `COUNT` at the start of `s`: its length. The optional range part is taken
/// whenever it matches, as the greedy regex did.
fn count_spec_len(s: &str) -> Option<usize> {
    let lo = count_bound_len(s)?;
    let rest = &s[lo..];
    let caret = usize::from(rest.starts_with('^'));
    if let Some(after_dots) = rest[caret..].strip_prefix("..") {
        let hi = if after_dots.starts_with('*') {
            Some(1)
        } else {
            count_bound_len(after_dots)
        };
        if let Some(hi) = hi {
            return Some(lo + caret + 2 + hi);
        }
    }
    Some(lo)
}

/// `(?:(%%|%)(.+))?$` on `s`: `Some(None)` for an empty tail, `Some(Some(..))`
/// for a separator, `None` when neither fits.
fn separator_tail(s: &str) -> Option<Option<(&str, &str)>> {
    if s.is_empty() {
        return Some(None);
    }
    for mode in ["%%", "%"] {
        if let Some(sep) = s.strip_prefix(mode)
            && !sep.is_empty()
            && !sep.contains('\n')
        {
            return Some(Some((mode, sep)));
        }
    }
    None
}

/// Match `s` against the counted-atom shape. The atom is the shortest
/// non-empty, newline-free prefix the rest of the shape accepts.
// Cost: O(n^2) worst case, n = pattern length (n is one regex atom's text).
pub(super) fn split_counted_atom(s: &str) -> Option<CountedAtom<'_>> {
    let newline = s.find('\n').unwrap_or(s.len());
    let mut from = 1;
    while let Some(off) = s.get(from..)?.find("**") {
        let at = from + off;
        if at > newline {
            return None;
        }
        from = at + 1;
        let rest = &s[at + 2..];
        let frugal = rest.starts_with('?');
        let rest = &rest[usize::from(frugal)..];
        let Some(count_len) = count_spec_len(rest) else {
            continue;
        };
        if let Some(sep) = separator_tail(&rest[count_len..]) {
            return Some(CountedAtom {
                atom: &s[..at],
                frugal,
                count: &rest[..count_len],
                sep,
            });
        }
    }
    None
}

/// Match `s` against `^(.+?)(%%|%)(.+)$`: the shortest non-empty,
/// newline-free atom, the separator mode and the separator text.
// Cost: O(n), n = pattern length.
pub(super) fn split_bare_separator(s: &str) -> Option<(&str, &str, &str)> {
    let newline = s.find('\n').unwrap_or(s.len());
    for (at, _) in s.match_indices('%') {
        if at == 0 {
            continue;
        }
        if at > newline {
            return None;
        }
        if let Some(Some((mode, sep))) = separator_tail(&s[at..]) {
            return Some((&s[..at], mode, sep));
        }
    }
    None
}

/// Does `s` contain `<\w+\(.*\)>`: a named rule call whose arguments (on the
/// same line) close with `)>`?
// Cost: O(n^2) worst case, n = pattern length.
pub(super) fn contains_named_rule_call_with_args(s: &str) -> bool {
    s.match_indices('<').any(|(at, _)| {
        let rest = &s[at + 1..];
        let word = rest
            .char_indices()
            .find(|&(_, c)| !is_word(c))
            .map_or(rest.len(), |(i, _)| i);
        word > 0
            && rest[word..].starts_with('(')
            && rest[word + 1..]
                .split('\n')
                .next()
                .is_some_and(|line| line.contains(")>"))
    })
}

/// Regex `\w`: alphanumerics, marks, connector punctuation and the joiners.
fn is_word(c: char) -> bool {
    use crate::builtins::unicode_gc::{GeneralCategory as Gc, general_category};
    c.is_alphanumeric()
        || matches!(general_category(c), Gc::Mn | Gc::Mc | Gc::Me | Gc::Pc)
        || c == '\u{200C}'
        || c == '\u{200D}'
}

#[cfg(test)]
mod tests {
    use super::*;

    const WITH_COUNT: &str =
        r"^(.+?)\*\*(\??)(\^?[0-9_]+(?:\^?\.\.(?:\^?[0-9_]+|\*))?)(?:(%%|%)(.+))?$";

    /// Every string over `alphabet` up to `max_len` characters.
    fn all_strings(alphabet: &[char], max_len: usize) -> Vec<String> {
        let mut out = vec![String::new()];
        let mut layer = vec![String::new()];
        for _ in 0..max_len {
            layer = layer
                .iter()
                .flat_map(|s| {
                    alphabet.iter().map(move |c| {
                        let mut t = s.clone();
                        t.push(*c);
                        t
                    })
                })
                .collect();
            out.extend(layer.iter().cloned());
        }
        out
    }

    #[test]
    fn split_counted_atom_agrees_with_the_regex_it_replaced() {
        let re = regex::Regex::new(WITH_COUNT).unwrap();
        let alphabet = ['a', '*', '%', '?', '^', '.', '1', '\n'];
        for s in all_strings(&alphabet, 7) {
            let expected = re.captures(&s).map(|c| CountedAtom {
                atom: c.get(1).unwrap().as_str(),
                frugal: !c.get(2).unwrap().as_str().is_empty(),
                count: c.get(3).unwrap().as_str(),
                sep: c.get(4).map(|m| (m.as_str(), c.get(5).unwrap().as_str())),
            });
            assert_eq!(split_counted_atom(&s), expected, "{s:?}");
        }
    }

    #[test]
    fn split_bare_separator_agrees_with_the_regex_it_replaced() {
        let re = regex::Regex::new(r"^(.+?)(%%|%)(.+)$").unwrap();
        for s in all_strings(&['a', '%', '\n', 'b'], 8) {
            let expected = re.captures(&s).map(|c| {
                (
                    c.get(1).unwrap().as_str(),
                    c.get(2).unwrap().as_str(),
                    c.get(3).unwrap().as_str(),
                )
            });
            assert_eq!(split_bare_separator(&s), expected, "{s:?}");
        }
    }

    #[test]
    fn named_rule_call_scan_agrees_with_the_regex_it_replaced() {
        let re = regex::Regex::new(r"<\w+\(.*\)>").unwrap();
        for s in all_strings(&['<', 'a', '(', ')', '>', '\n', '\u{E9}', '-'], 7) {
            assert_eq!(
                contains_named_rule_call_with_args(&s),
                re.is_match(&s),
                "{s:?}"
            );
        }
    }
}
