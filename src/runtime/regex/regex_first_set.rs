//! ADR-0099 Stage 1, second derived fact: a **first-character set** over an
//! unanchored scan's candidate start positions.
//!
//! [`regex_prefilter`](super::regex_prefilter) already narrows a scan whose
//! pattern begins with a plain literal run down to a substring search. That
//! leaves every pattern with *no* literal prefix entering the full
//! backtracking engine at every character position — ADR-0099 §2.4 measured
//! ~983 instructions to establish a single rejection, and
//! [#8248](https://github.com/tokuhirom/mutsu/issues/8248) measured the
//! consequence: a failing four-literal alternation over a 640 KB subject costs
//! 635 ms against rakudo's ~508 ms, the one shape where mutsu loses outright.
//!
//! This module derives the other fact ADR-0099 §4 names for Stage 1: the set
//! of characters a match *can begin with*. A position whose character is not
//! in that set cannot start a match, so it is rejected by one table lookup
//! rather than by an engine entry. `'zzq' | 'yyq' | 'xxq' | 'wwq'` yields
//! `{w, x, y, z}`; `:i 'ZZZQ'` yields `{Z, z}` plus the wide escape hatch;
//! `\d+` yields the ten ASCII digits.
//!
//! # Soundness
//!
//! The only way this can be *wrong* is by being too narrow: a set that omits a
//! character some match really could start with silently drops that match.
//! Every rule here is therefore an over-approximation, and anything not
//! provably over-approximating declines outright (`None`), which restores
//! exactly today's unfiltered scan. Three specific over-approximations are
//! load-bearing:
//!
//! - **Character classes are probed, not re-derived.** The set is built by
//!   asking [`char_class_matches`] — the matcher's
//!   own predicate — about every character in `0..=0xFF`, so the two cannot
//!   drift the way a second reading of `ClassItem` would. Anything the class
//!   might match above U+00FF is covered by [`FirstCharSet::wide`].
//! - **`\r` is included whenever `\n` is.** The `CharClass` atom matches a
//!   `\r\n` pair as the single grapheme `\n` (`regex_match_atom_simple.rs`),
//!   so a class containing `\n` really can start at a `\r`.
//! - **`:i` always sets `wide`.** U+212A KELVIN SIGN lowercases to `k`, so an
//!   ASCII `:i` literal is reachable from outside Latin-1. The Latin-1 half is
//!   probed with the matcher's own `to_lowercase()`-string comparison.
//!
//! A pattern that can match the empty string has no first character at all, so
//! every nullable shape declines rather than guessing.

use super::super::*;
use super::regex_eval_class::char_class_matches;

/// Characters a match of some pattern can begin with — a conservative
/// over-approximation (see the module docs).
pub(crate) struct FirstCharSet {
    /// Membership for `U+0000..=U+00FF`, the range that covers essentially all
    /// of the text mutsu scans at size — as a 256-bit set, so that testing a
    /// position is a shift and a mask, and a union is four `|`s.
    latin1: [u64; 4],
    /// Whether *some* character at or above `U+0100` can begin a match. Set
    /// whenever the derivation cannot enumerate the possibilities, which keeps
    /// the set an over-approximation without a second, unbounded table.
    wide: bool,
}

/// A set this dense rejects too little to pay for the lookup, so the caller
/// declines instead of filtering. `\w` sits at ~125 with `wide` set and stays
/// worth applying; `.` reaches 256 and does not.
const TOO_DENSE: u32 = 224;

impl FirstCharSet {
    fn empty() -> Self {
        FirstCharSet {
            latin1: [0; 4],
            wide: false,
        }
    }

    #[inline]
    pub(crate) fn contains(&self, c: char) -> bool {
        let u = c as u32;
        if u < 256 {
            self.latin1[(u >> 6) as usize] & (1u64 << (u & 63)) != 0
        } else {
            self.wide
        }
    }

    fn insert(&mut self, c: char) {
        let u = c as u32;
        if u < 256 {
            self.latin1[(u >> 6) as usize] |= 1u64 << (u & 63);
        } else {
            self.wide = true;
        }
    }

    fn union_with(&mut self, other: &FirstCharSet) {
        for (slot, bits) in self.latin1.iter_mut().zip(other.latin1.iter()) {
            *slot |= *bits;
        }
        self.wide |= other.wide;
    }

    fn latin1_len(&self) -> u32 {
        self.latin1.iter().map(|w| w.count_ones()).sum()
    }

    /// `true` when this set is dense enough that filtering with it would cost
    /// more than the engine entries it saves.
    fn too_dense(&self) -> bool {
        self.latin1_len() >= TOO_DENSE
    }

    /// Close the set under the grapheme rule that lets a `\r\n` pair match a
    /// class written as `\n` — the pair is one grapheme, so such a match
    /// really does start at the `\r`.
    fn close_over_crlf(&mut self) {
        if self.contains('\n') {
            self.insert('\r');
        }
    }
}

/// The first-character set of `pattern`, or `None` when no sound one can be
/// derived (the caller then scans every position, exactly as before).
///
/// Memoized on the pattern: `.comb`, `:g` and `split` restart their scan after
/// every match, so deriving per scan would charge the whole analysis once per
/// MATCH rather than once per pattern — measurably worse than not filtering at
/// all on a subject with tens of thousands of matches (#8248).
pub(crate) fn required_first_chars(pattern: &RegexPattern) -> &Option<FirstCharSet> {
    pattern
        .first_chars
        .get_or_init(|| derive_first_chars(pattern))
}

fn derive_first_chars(pattern: &RegexPattern) -> Option<FirstCharSet> {
    // `:m` matches against a mark-stripped image of the subject, so a set
    // derived from the pattern does not describe the characters the scan
    // loop actually indexes. (The already-stripped pattern the `:m` scan
    // loop passes in has the flag cleared, and is filtered normally.)
    if pattern.ignore_mark {
        return None;
    }
    let set = pattern_first_chars(pattern, pattern.ignore_case)?;
    if set.too_dense() {
        return None;
    }
    Some(set)
}

fn pattern_first_chars(pattern: &RegexPattern, ignore_case: bool) -> Option<FirstCharSet> {
    // A nested pattern (a group, an alternation branch) carries its own
    // scoped modifiers; `:i` from an enclosing level still applies, and an
    // over-wide set is the safe direction, so the two are OR'd.
    let ignore_case = ignore_case || pattern.ignore_case;
    if pattern.ignore_mark {
        return None;
    }
    let token = pattern.tokens.first()?;
    token_first_chars(token, ignore_case)
}

fn token_first_chars(token: &RegexToken, ignore_case: bool) -> Option<FirstCharSet> {
    // A literal spliced in from a runtime variable is not a static fact about
    // the pattern at all (ADR-0022 slice 5 treats it as a declarative-prefix
    // stopper for the same reason).
    if token.from_runtime_interpolation {
        return None;
    }
    // A separator quantifier (`atom +% sep`) runs through its own candidate
    // chain; the atom still comes first, but the shape is rare enough that
    // declining costs nothing and removes a case to reason about.
    if token.separator.is_some() {
        return None;
    }
    // Only a quantifier with a positive lower bound guarantees that the atom
    // is what the match begins with. A nullable one would need the set to
    // continue into the following token; declining is the honest answer.
    match token.quant {
        RegexQuant::One | RegexQuant::OneOrMore => {}
        RegexQuant::Repeat(min, _) if min > 0 => {}
        _ => return None,
    }
    atom_first_chars(&token.atom, ignore_case)
}

fn atom_first_chars(atom: &RegexAtom, ignore_case: bool) -> Option<FirstCharSet> {
    let mut set = FirstCharSet::empty();
    match atom {
        RegexAtom::Literal(ch) => {
            if ignore_case {
                // Probed with the matcher's own comparison
                // (`regex_match_atom_simple.rs`'s `Literal` arm) rather than a
                // second case-folding rule, and left open above Latin-1
                // because U+212A lowercases into ASCII.
                let want = ch.to_lowercase().to_string();
                for u in 0u32..256 {
                    let c = char::from_u32(u).expect("u < 256 is always a char");
                    if c.to_lowercase().to_string() == want {
                        set.insert(c);
                    }
                }
                set.wide = true;
            } else {
                set.insert(*ch);
            }
        }
        RegexAtom::CharClass(class) => {
            for u in 0u32..256 {
                let c = char::from_u32(u).expect("u < 256 is always a char");
                let hit = if ignore_case {
                    // Mirrors `regex_match_class_ignorecase`: a positive class
                    // matches when any case variant does, and a negated one is
                    // only widened by considering variants, so testing the
                    // variants is an over-approximation either way.
                    class_matches_any_case(class, c)
                } else {
                    char_class_matches(class, c)
                };
                if hit {
                    set.insert(c);
                }
            }
            // A class is written over characters but matched over graphemes,
            // and the items that can name a whole cluster (`Grapheme`) or a
            // property that reaches beyond Latin-1 cannot be enumerated here.
            if class.negated || !class_is_latin1_bounded(class) {
                set.wide = true;
            }
            set.close_over_crlf();
        }
        RegexAtom::Group(inner)
        | RegexAtom::CaptureGroup(inner)
        | RegexAtom::CaptureIsolatedGroup(inner) => {
            return pattern_first_chars(inner, ignore_case);
        }
        RegexAtom::Alternation(branches) | RegexAtom::SequentialAlternation(branches) => {
            // Any branch may be the one that matches, so the set is their
            // union — and one branch that cannot be described sinks the lot.
            if branches.is_empty() {
                return None;
            }
            for branch in branches {
                set.union_with(&pattern_first_chars(branch, ignore_case)?);
            }
        }
        // Everything else — `.`, subrules, assertions, code blocks, goal
        // matching, backreferences — either matches (nearly) any character or
        // depends on state this pass cannot see. Declining is free and safe.
        _ => return None,
    }
    Some(set)
}

/// Whether `class` matches `c` under `:i`, by the same "any case variant
/// matches" rule the matcher uses.
fn class_matches_any_case(class: &CharClass, c: char) -> bool {
    if char_class_matches(class, c) {
        return true;
    }
    for variant in c.to_lowercase().chain(c.to_uppercase()) {
        if char_class_matches(class, variant) {
            return true;
        }
    }
    false
}

/// Whether every character `class` can match is guaranteed to be below
/// `U+0100`, so that probing Latin-1 enumerates the class exhaustively.
fn class_is_latin1_bounded(class: &CharClass) -> bool {
    class.items.iter().all(|item| match item {
        ClassItem::Char(c) => (*c as u32) < 256,
        ClassItem::Range(a, b) => (*a as u32) < 256 && (*b as u32) < 256,
        // `\d` is `is_ascii_digit`, and the horizontal/vertical space sets are
        // closed lists — but the latter reach past Latin-1, so only the digit
        // items qualify.
        ClassItem::Digit => true,
        _ => false,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(pattern: &str) -> std::sync::Arc<RegexPattern> {
        let interp = crate::runtime::Interpreter::new();
        interp
            .parse_regex(pattern)
            .expect("pattern should parse for this test")
    }

    /// The derived set, computed directly rather than through the pattern's
    /// memo so that each case reads as a fact about the pattern text.
    fn set_of(pattern: &str) -> Option<FirstCharSet> {
        derive_first_chars(&parse(pattern))
    }

    /// Every character of `subject` that the set admits.
    fn admitted(set: &FirstCharSet, subject: &str) -> String {
        subject.chars().filter(|c| set.contains(*c)).collect()
    }

    #[test]
    fn a_literal_alternation_yields_its_branch_heads() {
        let set = set_of("[ 'zzq' | 'yyq' | 'xxq' | 'wwq' ]").expect("derivable");
        assert_eq!(admitted(&set, "abcwxyz0123"), "wxyz");
        assert!(!set.wide);
    }

    #[test]
    fn a_plain_literal_yields_exactly_its_first_character() {
        let set = set_of("'hello'").expect("derivable");
        assert_eq!(admitted(&set, "hHelo"), "h");
    }

    #[test]
    fn case_insensitive_covers_both_cases_and_stays_open_above_latin1() {
        let set = set_of(":i 'ZZZQ'").expect("derivable");
        assert!(set.contains('z'));
        assert!(set.contains('Z'));
        assert!(!set.contains('q'));
        // U+212A KELVIN SIGN lowercases to 'k', so `:i` can never close the
        // set above Latin-1.
        assert!(set.wide);
    }

    #[test]
    fn a_digit_class_is_closed_and_ascii_only() {
        let set = set_of(r"\d+").expect("derivable");
        assert_eq!(admitted(&set, "a1b2c3 _"), "123");
        assert!(!set.wide);
    }

    #[test]
    fn a_word_class_stays_open_above_latin1() {
        let set = set_of(r"\w+ 'QQQ'").expect("derivable");
        assert!(set.contains('a'));
        assert!(set.contains('_'));
        assert!(!set.contains(' '));
        assert!(set.wide);
    }

    #[test]
    fn a_nullable_leading_token_declines() {
        assert!(set_of("a? bc").is_none());
        assert!(set_of(r"\s* 'x'").is_none());
    }

    #[test]
    fn any_is_too_dense_to_be_worth_filtering() {
        assert!(set_of(". 'x'").is_none());
        assert!(set_of(r"<-[q]> 'x'").is_none());
    }

    #[test]
    fn a_class_containing_newline_also_admits_carriage_return() {
        // `\r\n` is one grapheme and matches a class written as `\n`, so a
        // match really can start at the `\r`.
        let set = set_of(r"<[\n]> 'x'").expect("derivable");
        assert!(set.contains('\r'));
    }

    #[test]
    fn an_assertion_or_a_real_subrule_declines() {
        assert!(set_of(r"<!after \d> 'QQ'").is_none());
        assert!(set_of("<ws> 'x'").is_none());
        assert!(set_of("<some-user-token> 'x'").is_none());
    }

    #[test]
    fn a_builtin_class_subrule_is_probed_like_any_other_class() {
        // `<alpha>` parses to a `CharClass`, not to a subrule call, so probing
        // the matcher's own predicate derives it exactly — no special case.
        // Raku's `<alpha>` is `<+alpha +[_]>`, so the underscore belongs in
        // the set — which is exactly why probing the matcher beats writing a
        // second rule here.
        let set = set_of("<alpha> 'x'").expect("derivable");
        assert_eq!(admitted(&set, "aZ9 _"), "aZ_");
    }

    #[test]
    fn ignoremark_declines() {
        assert!(set_of(":m 'cafe'").is_none());
    }

    #[test]
    fn an_alternation_with_one_underivable_branch_declines() {
        // A branch nobody can describe sinks the union: the set has to admit
        // every character ANY branch could start with.
        assert!(set_of("[ 'zz' | . ]").is_none());
        assert!(set_of("[ 'zz' | <some-user-token> ]").is_none());
    }

    #[test]
    fn an_alternation_of_describable_branches_unions_them() {
        let set = set_of(r"[ 'zz' | \d ]").expect("derivable");
        assert_eq!(admitted(&set, "az9Z0"), "z90");
    }
}
