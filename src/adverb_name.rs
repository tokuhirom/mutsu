//! The spelling of adverb values inside an *extended identifier*
//! (`$a:foo<42>`, `$a:foo«$c»`, `$a:foo(1+1)`).
//!
//! Raku canonicalizes every bracket form to `<word word>`: the brackets
//! themselves are not part of the name, so `$today:foo<a b>`,
//! `$today:foo«a b»`, `$today:foo['a','b']` and `$today:foo('a','b')` all
//! name the same variable (S02, `Language/syntax.rakudoc`).
//!
//! Two of those forms are *evaluated* before they are canonicalized:
//!
//! - `«...»` interpolates like `qqw`, so `$a:foo«$c»` names `$a:foo<42>` when
//!   `constant $c = 42` is in scope. `<...>` deliberately does not — the
//!   documentation is explicit that angle brackets "mimic single quote
//!   interpolation characteristics" and "cannot be used for the interpolation
//!   of constant names".
//! - `(...)` / `[...]` hold an arbitrary expression list, so `$a:foo(1+1)`
//!   names `$a:foo<2>`.
//!
//! Evaluation is strictly BEGIN-time: the value has to come from the
//! compile-time constant environment, which the parser does not have (it is a
//! pure `&str -> AST` pass whose results are memoized, so giving it a mutable
//! "constants seen so far" table would make memo hits depend on state the key
//! does not capture).
//!
//! So the parser canonicalizes only the spellings it can decide on its own and
//! leaves the rest wrapped in [`INTERP_MARK`] sentinels, preserving the source
//! spelling in the name; the compiler resolves those against its `constant`
//! environment before the name is used (`compiler::adverb_interp`). This module
//! owns the convention both sides speak.

/// Sentinel wrapping an adverb value the parser could not canonicalize on its
/// own — `…:foo\u{1}«$c»\u{1}`. U+0001 cannot appear in Raku source, so a name
/// containing it is unambiguously one awaiting BEGIN-time evaluation.
pub(crate) const INTERP_MARK: char = '\u{1}';

/// True when `name` still carries an unevaluated adverb value.
pub(crate) fn needs_interp(name: &str) -> bool {
    name.contains(INTERP_MARK)
}

/// Canonicalize the words of an already-literal adverb value: the brackets are
/// not part of the name and neither is the exact whitespace, so `< a  b >` and
/// `«a b»` both canonicalize to `a b`.
pub(crate) fn normalize_words(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    for word in content.split_whitespace() {
        if !out.is_empty() {
            out.push(' ');
        }
        out.push_str(word);
    }
    out
}

/// Split a `(...)`/`[...]` adverb value into comma-separated items, stripping
/// one layer of surrounding quotes. This is the *literal* reading, used both by
/// the parser's fast path and as the compiler's fallback when an item is not a
/// compile-time constant.
pub(crate) fn literal_items(content: &str) -> Vec<String> {
    content
        .split(',')
        .map(|s| {
            let s = s.trim();
            if s.len() >= 2
                && ((s.starts_with('\'') && s.ends_with('\''))
                    || (s.starts_with('"') && s.ends_with('"')))
            {
                s[1..s.len() - 1].to_string()
            } else {
                s.to_string()
            }
        })
        .collect()
}

/// The canonical `<...>` body for a literal `(...)`/`[...]` adverb value.
pub(crate) fn literal_paren_words(content: &str) -> String {
    if content.trim().is_empty() {
        return String::new();
    }
    normalize_words(&literal_items(content).join(" "))
}

/// Whether every comma item of a `(...)`/`[...]` adverb value is a plain
/// single- or double-quoted word, i.e. the value is spelled entirely in
/// literals and needs no BEGIN-time evaluation. This is what
/// `roast/S02-names-vars/varnames.t` exercises (`$today:foo('a','b')`), and
/// keeping it on the parser's fast path means the common case never depends on
/// the compiler pass at all.
pub(crate) fn is_all_quoted_items(content: &str) -> bool {
    if content.trim().is_empty() {
        return false;
    }
    content.split(',').all(|s| {
        let s = s.trim();
        s.len() >= 2
            && ((s.starts_with('\'') && s.ends_with('\''))
                || (s.starts_with('"') && s.ends_with('"')))
            // No escapes or embedded quotes: `'a\'b'` and `"a$b"` are not the
            // plain literals this fast path assumes.
            && !s[1..s.len() - 1].contains(['\\', '\'', '"'])
    })
}

/// Whether a `«...»` adverb value mentions a variable, and so has to be
/// interpolated at BEGIN time rather than taken literally. A `«...»` without a
/// sigil is just `qw`, which the parser canonicalizes itself.
pub(crate) fn guillemet_interpolates(content: &str) -> bool {
    content.contains(['$', '@', '%', '&'])
}

/// Wrap an adverb value's source spelling for later BEGIN-time evaluation.
/// `bracket` is `'«'` for the interpolating-quote form and `'('` for the
/// expression-list form (`[...]` normalizes to the same thing — the brackets
/// carry no meaning of their own).
pub(crate) fn mark_unevaluated(bracket: char, content: &str) -> String {
    let close = if bracket == '«' { '»' } else { ')' };
    format!("{INTERP_MARK}{bracket}{content}{close}{INTERP_MARK}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalizes_whitespace() {
        assert_eq!(normalize_words(" a  b "), "a b");
        assert_eq!(normalize_words(""), "");
        assert_eq!(normalize_words("+"), "+");
    }

    #[test]
    fn literal_paren_reading() {
        assert_eq!(literal_paren_words("'a','b'"), "a b");
        assert_eq!(literal_paren_words(" 'a' , 'b' "), "a b");
        assert_eq!(literal_paren_words(""), "");
    }

    #[test]
    fn quoted_item_detection() {
        assert!(is_all_quoted_items("'a','b'"));
        assert!(is_all_quoted_items("\"\u{1F602}\""));
        assert!(!is_all_quoted_items("1+1"));
        assert!(!is_all_quoted_items("C"));
        assert!(!is_all_quoted_items(""));
        assert!(!is_all_quoted_items("'a\\'b'"));
    }

    #[test]
    fn guillemet_sigil_detection() {
        assert!(guillemet_interpolates("$c"));
        assert!(guillemet_interpolates("a $c b"));
        assert!(!guillemet_interpolates("a b"));
        assert!(!guillemet_interpolates("\u{1F602}"));
    }

    #[test]
    fn marking_round_trips() {
        let marked = mark_unevaluated('«', "$c");
        assert!(needs_interp(&marked));
        assert_eq!(marked, "\u{1}«$c»\u{1}");
        assert_eq!(mark_unevaluated('(', "1+1"), "\u{1}(1+1)\u{1}");
        assert!(!needs_interp("a:foo<42>"));
    }
}
