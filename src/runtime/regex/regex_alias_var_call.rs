//! A sigil alias on a match-time `$var` atom (`$<a>=$var`, `@<a>=$var`,
//! `$0=$var`) as a call of the variable's value (#14510, ADR-12917
//! direction 2).
//!
//! ```raku
//! my $re = /<digit>+/;
//! say ("12" ~~ /$<rx>=$re/)<rx><digit>.elems;   # 2
//! my $s = "1";
//! say ("111" ~~ /$<a>=$s+/)<a>.elems;            # 3
//! ```
//!
//! Rakudo's `metachar:sym<var>` runs `subrule_alias` on the interpolation
//! node, so the alias names the interpolated pattern's own Match: a `Regex`
//! value keeps its nested captures under the alias, and a quantifier repeats
//! the call, giving one Match per iteration whatever the value's type. The
//! plain `VarInterp` atom enters a `Regex` value as a capture-isolated group
//! instead, which leaves the alias only the matched span. So the structural
//! parser lowers an aliased match-time `$var` to a call atom spelled with
//! [`INTERP_CALL_MARK`]: its lookup spec reads the variable when the call is
//! made and matches a `Regex` value as itself and any other value as the
//! literal the interpolation would have matched
//! ([`Interpreter::interp_call_value`]).

use super::super::*;

/// Starts the text of a call atom that interpolates a variable under an alias:
/// the mark, one case flag ([`INTERP_CASE_PLAIN`], [`INTERP_CASE_I`],
/// [`INTERP_CASE_M`] or [`INTERP_CASE_IM`]), then `$name`. A reserved control
/// character like `NON_DECLARATIVE_INTERP_MARK`: written pattern source never
/// contains it.
pub(crate) const INTERP_CALL_MARK: char = '\u{4}';

const INTERP_CASE_PLAIN: char = '-';
const INTERP_CASE_I: char = 'i';
const INTERP_CASE_M: char = 'm';
const INTERP_CASE_IM: char = 'b';

/// How a non-`Regex` value of an interpolation call matches: the enclosing
/// pattern's `:i` / `:m` at the atom, which a literal splice would have
/// inherited.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct InterpCallCase {
    pub(crate) ignore_case: bool,
    pub(crate) ignore_mark: bool,
}

/// The call atom text for `$name` under an alias (see the module doc).
// Cost: O(n), n = the name's length.
pub(crate) fn interp_call_text(name: &str, case: InterpCallCase) -> String {
    let flag = match (case.ignore_case, case.ignore_mark) {
        (false, false) => INTERP_CASE_PLAIN,
        (true, false) => INTERP_CASE_I,
        (false, true) => INTERP_CASE_M,
        (true, true) => INTERP_CASE_IM,
    };
    let mut text = String::with_capacity(name.len() + 3);
    text.push(INTERP_CALL_MARK);
    text.push(flag);
    text.push('$');
    text.push_str(name);
    text
}

/// Split an interpolation call's atom text into its case flags and the
/// variable's sigiled name (`$name`); `None` for any other call text.
// Cost: O(1).
pub(crate) fn parse_interp_call_text(text: &str) -> Option<(InterpCallCase, &str)> {
    let rest = text.strip_prefix(INTERP_CALL_MARK)?;
    let mut chars = rest.chars();
    let case = match chars.next()? {
        INTERP_CASE_PLAIN => InterpCallCase::default(),
        INTERP_CASE_I => InterpCallCase {
            ignore_case: true,
            ignore_mark: false,
        },
        INTERP_CASE_M => InterpCallCase {
            ignore_case: false,
            ignore_mark: true,
        },
        INTERP_CASE_IM => InterpCallCase {
            ignore_case: true,
            ignore_mark: true,
        },
        _ => return None,
    };
    let name = chars.as_str();
    name.starts_with('$').then_some((case, name))
}

impl Interpreter {
    /// The Regex an interpolation call matches for the variable's `value`: a
    /// `Regex` value itself (it keeps its own adverbs, as an interpolated
    /// regex does), anything else the pattern the interpolation pre-pass
    /// would have spliced for it (a literal, a junction's alternation, `<!>`
    /// for Nil) under the atom's `:i` / `:m`.
    // Cost: O(r), r = the value's rendered length.
    pub(in crate::runtime) fn interp_call_value(value: Value, case: InterpCallCase) -> Value {
        if matches!(
            value.view(),
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_)
        ) {
            return value;
        }
        let mut source = String::new();
        if case.ignore_case {
            source.push_str(":i ");
        }
        if case.ignore_mark {
            source.push_str(":m ");
        }
        let body_start = source.len();
        Self::push_value_as_regex_pattern(&value, &mut source);
        if source.len() == body_start {
            // The empty string matches at the cursor, as its splice did.
            source.push_str("<?>");
        }
        Value::regex(source)
    }
}

/// Whether the interpolation pre-pass output `out` ends in a sigil alias
/// (`$<a>=`, `@<a>=`, `$0=`), so the value about to be spliced is that
/// alias's atom. A spliced literal is several atoms; the pre-pass brackets it
/// so the alias names the whole value, as it names the interpolation node in
/// rakudo, and a quantifier after it repeats the whole value.
// Cost: O(k), k = the trailing whitespace and alias name's length.
pub(crate) fn ends_with_sigil_alias(out: &str) -> bool {
    let trimmed = out.trim_end_matches(|c: char| {
        c.is_whitespace()
            || c == crate::runtime::Interpreter::NON_DECLARATIVE_INTERP_MARK
    });
    let Some(before_eq) = trimmed.strip_suffix('=') else {
        return false;
    };
    let before_eq = before_eq.trim_end();
    if before_eq.ends_with('>') {
        // `$<a>=` / `@<a>=`, not a `<…>` assertion followed by `=`: the
        // `<` that opens the name follows a sigil.
        return before_eq
            .rfind('<')
            .is_some_and(|lt| before_eq[..lt].ends_with(['$', '@']));
    }
    let digits = before_eq.trim_end_matches(|c: char| c.is_ascii_digit());
    digits.len() < before_eq.len() && digits.ends_with('$')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interp_call_text_round_trips() {
        for case in [
            InterpCallCase::default(),
            InterpCallCase {
                ignore_case: true,
                ignore_mark: false,
            },
            InterpCallCase {
                ignore_case: false,
                ignore_mark: true,
            },
            InterpCallCase {
                ignore_case: true,
                ignore_mark: true,
            },
        ] {
            let text = interp_call_text("re", case);
            assert_eq!(parse_interp_call_text(&text), Some((case, "$re")));
        }
        assert_eq!(parse_interp_call_text("&$re"), None);
    }

    #[test]
    fn sigil_alias_suffixes() {
        assert!(ends_with_sigil_alias("x $<a>="));
        assert!(ends_with_sigil_alias("@<a> = "));
        assert!(ends_with_sigil_alias("$0="));
        assert!(!ends_with_sigil_alias("<foo>="));
        assert!(!ends_with_sigil_alias("a="));
        assert!(!ends_with_sigil_alias("x"));
    }
}
