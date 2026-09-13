//! How far back a look-behind has to search.
//!
//! `<?after X>` / `<!after X>` ask whether `X` matches ending exactly at the
//! current position. The engine answers by running `X` forward from a candidate
//! start and checking whether it ends at `pos`, which means it has to try
//! starts — and it used to try **every** start from 0, making one look-behind
//! O(pos) and any pattern that evaluates one per input line O(n^2).
//!
//! That was the whole superlinear term in a YAMLish parse
//! ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)): `token
//! block-ws($indent)` contains `<!after <.alnum>>` and is evaluated once per
//! block entry, so its per-evaluation cost grew with the document while its
//! evaluation count grew with it too. Measured by attributing every single-atom
//! match to its innermost enclosing rule: `block-ws` went from 2,168 atom
//! matches per evaluation at 60 rows to 8,648 at 240 (4x the document, 4x the
//! per-evaluation work, 16x in total), and accounted for 4,151,040 of the
//! 4,150,080 `matches_named_builtin` calls the whole parse made.
//!
//! A start earlier than `pos` minus the most `X` can consume cannot end at
//! `pos`, so those starts are provably unreachable and skipping them changes no
//! answer. [`lookbehind_start_floor`] computes that bound and returns the first
//! start worth trying; when it cannot bound the pattern it returns 0 and the
//! search is exactly what it was.
//!
//! The bound counts **grapheme clusters**, not `char`s, because that is what
//! the matcher consumes: one `<.alnum>` can span a base character plus any
//! number of combining marks, and `\r\n` is one cluster
//! ([`super::regex_helpers::grapheme_end`]). So the floor is found by stepping
//! back that many clusters with the same rule `grapheme_end` steps forward by,
//! rather than by subtracting a character count.

use unicode_normalization::char::is_combining_mark;

use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexQuant};

/// The first start position a look-behind of `pattern` at `pos` can match from.
///
/// `0` when the pattern's length cannot be bounded, which is the conservative
/// answer: the caller then searches every start, as it did before this existed.
pub(super) fn lookbehind_start_floor(pattern: &RegexPattern, chars: &[char], pos: usize) -> usize {
    match pattern_max_graphemes(pattern) {
        Some(max) => step_back_graphemes(chars, pos, max),
        None => 0,
    }
}

/// Walk `count` grapheme clusters back from `pos`, mirroring
/// [`super::regex_helpers::grapheme_end`]'s notion of a cluster: `\r\n` is one,
/// and a cluster extends over the combining marks that follow its base.
fn step_back_graphemes(chars: &[char], pos: usize, count: usize) -> usize {
    let mut i = pos;
    for _ in 0..count {
        if i == 0 {
            return 0;
        }
        if i >= 2 && chars[i - 2] == '\r' && chars[i - 1] == '\n' {
            i -= 2;
            continue;
        }
        i -= 1;
        // `chars[i]` may be a combining mark, in which case the cluster starts
        // at the base character before it.
        while i > 0 && is_combining_mark(chars[i]) {
            i -= 1;
        }
    }
    i
}

/// An upper bound on the grapheme clusters `pattern` can consume, or `None`
/// when it cannot be bounded.
///
/// Deliberately incomplete: every shape this does not model returns `None`, so
/// a new atom or quantifier kind costs a missed optimization rather than a
/// wrong answer. The shapes it does model are the ones look-behinds are written
/// with in practice — a character class, a literal, a short bounded group.
fn pattern_max_graphemes(pattern: &RegexPattern) -> Option<usize> {
    let mut total: usize = 0;
    for token in &pattern.tokens {
        // A separated quantifier's separator contributes length too; not worth
        // modelling for a look-behind, so it is simply not bounded.
        if token.separator.is_some() {
            return None;
        }
        let atom = atom_max_graphemes(&token.atom)?;
        let reps = quant_max_reps(&token.quant)?;
        total = total.checked_add(atom.checked_mul(reps)?)?;
    }
    Some(total)
}

/// The most times a quantifier can repeat its atom, or `None` when unbounded.
fn quant_max_reps(quant: &RegexQuant) -> Option<usize> {
    match quant {
        RegexQuant::One => Some(1),
        RegexQuant::ZeroOrOne => Some(1),
        RegexQuant::Repeat(_, Some(max)) => Some(*max),
        // `*`, `+`, `** n..*` and a runtime-computed count are all unbounded.
        RegexQuant::ZeroOrMore
        | RegexQuant::OneOrMore
        | RegexQuant::Repeat(_, None)
        | RegexQuant::RepeatCode(_) => None,
    }
}

/// An upper bound on the grapheme clusters one atom can consume.
fn atom_max_graphemes(atom: &RegexAtom) -> Option<usize> {
    match atom {
        // Exactly one cluster.
        RegexAtom::Literal(_)
        | RegexAtom::Any
        | RegexAtom::CharClass(_)
        | RegexAtom::CompositeClass { .. }
        | RegexAtom::UnicodeProp { .. } => Some(1),

        // Zero-width: assertions, markers and declarations consume nothing.
        RegexAtom::ZeroWidth
        | RegexAtom::CaptureStartMarker
        | RegexAtom::CaptureEndMarker
        | RegexAtom::StartOfLine
        | RegexAtom::EndOfLine
        | RegexAtom::AtPosition(_)
        | RegexAtom::LeftWordBoundary
        | RegexAtom::RightWordBoundary
        | RegexAtom::WordBoundary { .. }
        | RegexAtom::WithinWord { .. }
        | RegexAtom::UnicodePropAssert { .. }
        | RegexAtom::Lookaround { .. }
        | RegexAtom::CodeAssertion { .. }
        | RegexAtom::VarDecl { .. } => Some(0),

        // Transparent wrappers: the bound is the inner pattern's.
        RegexAtom::Group(inner)
        | RegexAtom::CaptureGroup(inner)
        | RegexAtom::CaptureIsolatedGroup(inner) => pattern_max_graphemes(inner),

        // A branch set consumes at most its longest branch. A conjunction's
        // branches all match the same span, so the same bound is sound.
        RegexAtom::Alternation(branches)
        | RegexAtom::SequentialAlternation(branches)
        | RegexAtom::Conjunction(branches) => {
            let mut worst = 0;
            for branch in branches {
                worst = worst.max(pattern_max_graphemes(branch)?);
            }
            Some(worst)
        }

        // Everything else — a subrule call above all, but also `\n`/`\N`/`\s`
        // (whose CRLF handling is not the cluster rule this counts in) and a
        // pattern interpolated at match time — is not bounded here.
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::regex_types::{CharClass, ClassItem, RegexToken};

    fn pattern(tokens: Vec<RegexToken>) -> RegexPattern {
        RegexPattern {
            tokens,
            anchor_start: false,
            anchor_end: false,
            ignore_case: false,
            ignore_mark: false,
        }
    }

    fn token(atom: RegexAtom, quant: RegexQuant) -> RegexToken {
        RegexToken {
            atom,
            quant,
            named_capture: None,
            secondary_named_capture: None,
            hash_capture: None,
            force_list_capture: false,
            ratchet: false,
            frugal: false,
            separator: None,
            from_runtime_interpolation: false,
        }
    }

    fn one_class() -> RegexAtom {
        RegexAtom::CharClass(CharClass {
            negated: false,
            items: vec![ClassItem::Range('a', 'z')],
        })
    }

    #[test]
    fn a_single_class_is_one_grapheme() {
        let p = pattern(vec![token(one_class(), RegexQuant::One)]);
        assert_eq!(pattern_max_graphemes(&p), Some(1));
    }

    #[test]
    fn zero_width_atoms_cost_nothing() {
        let p = pattern(vec![
            token(RegexAtom::StartOfLine, RegexQuant::One),
            token(one_class(), RegexQuant::One),
        ]);
        assert_eq!(pattern_max_graphemes(&p), Some(1));
    }

    #[test]
    fn a_bounded_repeat_multiplies_and_an_unbounded_one_poisons() {
        let bounded = pattern(vec![token(one_class(), RegexQuant::Repeat(1, Some(3)))]);
        assert_eq!(pattern_max_graphemes(&bounded), Some(3));
        let unbounded = pattern(vec![token(one_class(), RegexQuant::OneOrMore)]);
        assert_eq!(pattern_max_graphemes(&unbounded), None);
    }

    #[test]
    fn an_alternation_takes_its_longest_branch() {
        let short = pattern(vec![token(one_class(), RegexQuant::One)]);
        let long = pattern(vec![
            token(one_class(), RegexQuant::One),
            token(RegexAtom::Literal('x'), RegexQuant::One),
        ]);
        let p = pattern(vec![token(
            RegexAtom::Alternation(vec![short, long]),
            RegexQuant::One,
        )]);
        assert_eq!(pattern_max_graphemes(&p), Some(2));
    }

    #[test]
    fn a_subrule_call_is_not_bounded() {
        let p = pattern(vec![token(
            RegexAtom::Named(Default::default()),
            RegexQuant::One,
        )]);
        assert_eq!(pattern_max_graphemes(&p), None);
    }

    #[test]
    fn the_floor_walks_back_whole_clusters() {
        // A base character followed by two combining marks is one cluster, so
        // stepping back one cluster from its end lands on the base.
        let chars: Vec<char> = "ae\u{301}\u{302}".chars().collect();
        assert_eq!(chars.len(), 4);
        assert_eq!(step_back_graphemes(&chars, 4, 1), 1);
        assert_eq!(step_back_graphemes(&chars, 4, 2), 0);
        // CRLF is one cluster, matching `grapheme_end`.
        let crlf: Vec<char> = "a\r\n".chars().collect();
        assert_eq!(step_back_graphemes(&crlf, 3, 1), 1);
        // Walking past the start clamps rather than wrapping.
        assert_eq!(step_back_graphemes(&crlf, 3, 99), 0);
    }

    #[test]
    fn an_unbounded_pattern_searches_from_zero() {
        let chars: Vec<char> = "abcdef".chars().collect();
        let unbounded = pattern(vec![token(one_class(), RegexQuant::ZeroOrMore)]);
        assert_eq!(lookbehind_start_floor(&unbounded, &chars, 6), 0);
        let bounded = pattern(vec![token(one_class(), RegexQuant::One)]);
        assert_eq!(lookbehind_start_floor(&bounded, &chars, 6), 5);
    }
}
