//! Declared-symbol shadowing of the named quote languages.
//!
//! Raku's quote languages are spelled as ordinary identifiers (`Q`, `q`, `qq`,
//! `qw`, `qx`, `m`, `s`, `S`, `tr`, `TR`, `rx`, ...). Rakudo decides between
//! "this is a quote construct" and "this is a term" by consulting what has been
//! *declared*: once a symbol of that name is in scope, the quote language
//! spelled that way is gone. `enum E <P Q>; say Q, "x", 2;` prints `Qx2`
//! because `Q` is the enum value, and `enum E <P Q>; say Q/2/;` is a *division*
//! (rakudo: "Missing required term after infix"), not a `Q`-quote.
//!
//! mutsu used to decide this lexically per quote name, with a handful of
//! one-off guards bolted on afterwards (`is_user_declared_type("S")`,
//! `is_user_declared_sub("m")`, `is_user_declared_sub("s")`, ...). Each guard
//! consulted a *different* subset of the declared-symbol registries and most
//! quote names had no guard at all, so a declared `Q`/`q`/`qq`/`tr`/`TR`/`rx`
//! silently lost to the quote language — sometimes producing a parse error,
//! sometimes silently swallowing the rest of the statement.
//!
//! This module is the single, name-agnostic implementation of the rule: extract
//! the identifier the quote construct is spelled with, and let a declaration of
//! that identifier win.
//!
//! The one exception, which rakudo shares, is an explicit **adverb**: `s:g/…/…/`
//! is a substitution even when `sub s` is in scope, and so are `m:i/…/`,
//! `q:w/…/` and friends — an adverb makes the construct unambiguously the quote
//! language, so no declaration can shadow it.

use crate::parser::stmt::simple::is_declared_symbol_name;

/// Byte length of the leading Raku identifier in `input`, or `0` when `input`
/// does not start with one.
///
/// Mirrors the ordinary identifier rule (`-`/`'` continue an identifier only
/// when a letter/underscore follows), so a declared `q-and-a` shadows the
/// spelling `q-and-a` rather than the quote name `q`.
fn leading_ident_len(input: &str) -> usize {
    let first = match input.chars().next() {
        Some(c) if c.is_alphabetic() || c == '_' => c,
        _ => return 0,
    };
    let mut end = first.len_utf8();
    while let Some(c) = input[end..].chars().next() {
        if c.is_alphanumeric() || c == '_' {
            end += c.len_utf8();
            continue;
        }
        // `-`/`'` continue an identifier only when a name character follows.
        if c == '-' || c == '\'' {
            let after = &input[end + c.len_utf8()..];
            if after
                .chars()
                .next()
                .is_some_and(|n| n.is_alphabetic() || n == '_')
            {
                end += c.len_utf8();
                continue;
            }
        }
        break;
    }
    end
}

/// Whether `rest` opens with an explicit quote adverb (`:g`, `:i`, `:!ratchet`,
/// `:2nd`, ...), optionally after horizontal whitespace (`s :g/…/…/`).
///
/// `::` is a package qualification, never an adverb.
fn starts_with_quote_adverb(rest: &str) -> bool {
    let trimmed = rest.trim_start_matches([' ', '\t']);
    let Some(after_colon) = trimmed.strip_prefix(':') else {
        return false;
    };
    after_colon
        .chars()
        .next()
        .is_some_and(|c| c.is_alphanumeric() || c == '_' || c == '!')
}

/// Whether the quote-language construct that `input` opens with is shadowed by
/// a declared symbol of the same name, and so must be parsed as a term instead.
///
/// `input` must start at the quote construct's *name* (`Q…`, `q…`, `s…`, ...).
pub(crate) fn quote_lang_shadowed(input: &str) -> bool {
    let len = leading_ident_len(input);
    if len == 0 {
        return false;
    }
    // An adverb makes this unambiguously the quote language, declaration or not.
    if starts_with_quote_adverb(&input[len..]) {
        return false;
    }
    is_declared_symbol_name(&input[..len])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident_len_basics() {
        assert_eq!(leading_ident_len("Q/a/"), 1);
        assert_eq!(leading_ident_len("qq{x}"), 2);
        assert_eq!(leading_ident_len("q-and-a(1)"), 7);
        assert_eq!(leading_ident_len("q-/a/"), 1);
        assert_eq!(leading_ident_len("/a/"), 0);
        assert_eq!(leading_ident_len(""), 0);
        assert_eq!(leading_ident_len("_x1;"), 3);
    }

    #[test]
    fn adverb_detection() {
        assert!(starts_with_quote_adverb(":g/a/b/"));
        assert!(starts_with_quote_adverb(" :g/a/b/"));
        assert!(starts_with_quote_adverb(":!ratchet/a/"));
        assert!(starts_with_quote_adverb(":2nd/a/"));
        assert!(!starts_with_quote_adverb("::Foo"));
        assert!(!starts_with_quote_adverb("/a/b/"));
        assert!(!starts_with_quote_adverb("\n:g/a/"));
    }
}
