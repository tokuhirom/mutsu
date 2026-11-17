//! A look-behind matched the way rakudo matches it: the pattern reversed and
//! run forward over the reversed subject (#16031).
//!
//! ```raku
//! say so "\nset \$x" ~~ rx:r/<?after ^^ \s* 'set' \s+> '$'/;   # False
//! say so "ab\n" ~~ /ab \n <?after \n $$>/;                    # True
//! ```
//!
//! Rakudo's `after` flips the target string and runs a flipped copy of the
//! look-behind's AST (`flip_ast`: a concatenation's children reversed, each
//! anchor swapped for its mirror) forward from the mirrored position. Two
//! observable consequences follow, and a forward search for a start that
//! ends exactly at the position (the engine's general look-behind) has
//! neither:
//!
//! - quantifiers are greedy and ratcheting *leftwards*: under `:ratchet`,
//!   `\s*` eats every space before `'set'` and never gives one back, so
//!   `^^` is then tested where the run stopped;
//! - `^^` and `$$` are tested as their mirrors on the reversed subject, so
//!   their end-of-string exceptions swap sides: inside a look-behind `^^`
//!   fails at the start of a subject that begins with a newline and holds at
//!   the end of one that ends with a newline, and `$$` the other way round.
//!
//! Only a pattern made of atoms whose meaning is direction-free (literals,
//! classes, groups, alternations) or that have a mirror (anchors, word
//! boundaries) is flipped. Anything else — a rule call, a backreference,
//! code, an interpolation, a nested look-around — keeps the forward search:
//! rakudo runs a called rule forward over the reversed subject, which is not
//! a semantics worth copying (`<?after <foo>>` with `token foo { ab }` then
//! matches after "ba").

use std::sync::Arc;

use super::super::*;
use super::regex_helpers::grapheme_end;
use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexToken};

/// `pattern` reversed for a look-behind, or `None` when it holds an atom that
/// has no direction-free meaning (see the module doc).
// Cost: O(t), t = the pattern's tokens, nested patterns included.
pub(super) fn flip_pattern(pattern: &RegexPattern) -> Option<RegexPattern> {
    let mut tokens = Vec::with_capacity(pattern.tokens.len());
    for token in pattern.tokens.iter().rev() {
        if token.separator.is_some() {
            return None;
        }
        tokens.push(RegexToken {
            atom: flip_atom(&token.atom)?,
            ..token.clone()
        });
    }
    Some(RegexPattern {
        tokens,
        anchor_start: pattern.anchor_end,
        anchor_end: pattern.anchor_start,
        ignore_case: pattern.ignore_case,
        ignore_mark: pattern.ignore_mark,
        derived: Default::default(),
    })
}

fn flip_atom(atom: &RegexAtom) -> Option<RegexAtom> {
    Some(match atom {
        RegexAtom::Literal(_)
        | RegexAtom::LiteralGrapheme(_)
        | RegexAtom::Any
        | RegexAtom::CharClass(_)
        | RegexAtom::CompositeClass { .. }
        | RegexAtom::UnicodeProp { .. }
        | RegexAtom::Newline
        | RegexAtom::NotNewline
        | RegexAtom::ZeroWidth
        | RegexAtom::WordBoundary { .. }
        | RegexAtom::WithinWord { .. }
        | RegexAtom::CaptureStartMarker
        | RegexAtom::CaptureEndMarker => atom.clone(),
        RegexAtom::StartOfLine => RegexAtom::EndOfLine,
        RegexAtom::EndOfLine => RegexAtom::StartOfLine,
        RegexAtom::StartOfString => RegexAtom::EndOfString,
        RegexAtom::EndOfString => RegexAtom::StartOfString,
        RegexAtom::LeftWordBoundary => RegexAtom::RightWordBoundary,
        RegexAtom::RightWordBoundary => RegexAtom::LeftWordBoundary,
        RegexAtom::Group(inner) => RegexAtom::Group(flip_pattern(inner)?),
        RegexAtom::CaptureGroup(inner) => RegexAtom::CaptureGroup(flip_pattern(inner)?),
        RegexAtom::Alternation(branches) => {
            RegexAtom::Alternation(branches.iter().map(flip_pattern).collect::<Option<_>>()?)
        }
        RegexAtom::SequentialAlternation(branches) => RegexAtom::SequentialAlternation(
            branches.iter().map(flip_pattern).collect::<Option<_>>()?,
        ),
        _ => return None,
    })
}

/// `chars[lo..hi]` with its grapheme clusters in reverse order, each
/// cluster's own characters kept in order (rakudo flips graphemes, and a
/// `\r\n` or a base with its marks is one of them). A cluster boundary at `p`
/// lands at `hi - p`.
// Cost: O(hi - lo).
fn reversed_clusters(chars: &[char], lo: usize, hi: usize) -> Vec<char> {
    let mut bounds = Vec::new();
    let mut i = lo;
    while i < hi {
        bounds.push(i);
        i = grapheme_end(chars, i).min(hi).max(i + 1);
    }
    let mut out = Vec::with_capacity(hi - lo);
    let mut end = hi;
    for &start in bounds.iter().rev() {
        out.extend_from_slice(&chars[start..end]);
        end = start;
    }
    out
}

impl Interpreter {
    /// Whether the look-behind `pattern` matches ending at `pos`, matched as
    /// rakudo does (see the module doc); `None` when the pattern is not one
    /// this flips, and the caller searches forward instead.
    ///
    /// Only the part of the subject the flipped match can reach is reversed:
    /// from one cluster before the earliest start the pattern can match from
    /// (`floor`, so an anchor there still sees its neighbour) to one cluster
    /// past `pos` (so a mirrored `$$` at `pos` sees the character after it).
    // Cost: O(w) to reverse the window, w = pos - floor plus two clusters,
    // plus one match of the flipped pattern; the flip is built once per
    // pattern.
    pub(super) fn lookbehind_flipped(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        floor: usize,
        pkg: Symbol,
    ) -> Option<bool> {
        let flipped: Arc<RegexPattern> = pattern
            .derived
            .flipped
            .get_or_init(|| flip_pattern(pattern).map(Arc::new))
            .clone()?;
        let lo = super::regex_lookbehind::step_back_one_grapheme(chars, floor.min(pos));
        let hi = if pos < chars.len() {
            grapheme_end(chars, pos).max(pos + 1).min(chars.len())
        } else {
            chars.len()
        };
        let reversed = reversed_clusters(chars, lo, hi);
        Some(
            self.regex_match_end_from_caps_in_pkg(&flipped, &reversed, hi - pos, pkg)
                .is_some(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clusters_reverse_as_units() {
        let chars: Vec<char> = "ae\u{301}\r\nb".chars().collect();
        let rev: String = reversed_clusters(&chars, 0, chars.len()).into_iter().collect();
        assert_eq!(rev, "b\r\ne\u{301}a");
        let rev: String = reversed_clusters(&chars, 1, 3).into_iter().collect();
        assert_eq!(rev, "e\u{301}");
    }
}
