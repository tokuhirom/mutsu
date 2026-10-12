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
//! answer. [`Interpreter::lookbehind_start_floor_in`] computes that bound and
//! returns the first start worth trying; when it cannot bound the pattern it
//! returns 0 and the search is exactly what it was.
//!
//! The bound counts **grapheme clusters**, not `char`s, because that is what
//! the matcher consumes: one `<.alnum>` can span a base character plus any
//! number of combining marks, and `\r\n` is one cluster
//! ([`super::regex_helpers::grapheme_end`]). So the floor is found by stepping
//! back that many clusters with the same rule `grapheme_end` steps forward by,
//! rather than by subtracting a character count.

//!
//! A look-behind that calls a rule (`<!after <.space>>`, where the grammar
//! declares `token space { <[\ \t]> }`) is bounded through the call: the
//! engine runs the rule's body, so the most that body can consume bounds the
//! call ([`Interpreter::lookbehind_start_floor_in`]). Before that, every such
//! look-behind searched from 0, and YAMLish's `plain` scalars evaluate one per
//! character (#12917).

use unicode_normalization::char::is_combining_mark;

use super::super::*;
use crate::runtime::regex_types::{NamedAtom, RegexAtom, RegexPattern, RegexQuant};

/// How many rule calls deep a look-behind's bound follows: a body calling a
/// rule calling a rule … is bounded through this many levels, and anything
/// deeper (a recursive rule above all) is not bounded.
const MAX_RULE_DEPTH: usize = 8;

/// The first start position a look-behind of `pattern` at `pos` can match from,
/// without following rule calls (a call is unbounded).
///
/// `0` when the pattern's length cannot be bounded, which is the conservative
/// answer: the caller then searches every start, as it did before this existed.
#[cfg(test)]
fn lookbehind_start_floor(pattern: &RegexPattern, chars: &[char], pos: usize) -> usize {
    match pattern_max_graphemes(pattern, &mut |_| None) {
        Some(max) => step_back_graphemes(chars, pos, max),
        None => 0,
    }
}

impl Interpreter {
    /// The first start position a look-behind of `pattern`, matched in `pkg`,
    /// can match from at `pos`: `pos` minus the most the pattern can consume,
    /// following its rule calls into the bodies the engine would run.
    ///
    /// `0` when the pattern's length cannot be bounded, which is the
    /// conservative answer: the caller then searches every start.
    // Cost: O(t), t = the tokens of the pattern and of the rule bodies it
    // reaches (each call resolved through the memoized candidate lookup), plus
    // O(m), m = the graphemes stepped back.
    pub(super) fn lookbehind_start_floor_in(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> usize {
        let max = pattern_max_graphemes(pattern, &mut |name| {
            self.rule_call_max_graphemes(name, pkg, MAX_RULE_DEPTH)
        });
        match max {
            Some(max) => step_back_graphemes(chars, pos, max),
            None => 0,
        }
    }

    /// An upper bound on the grapheme clusters the call `name` made from `pkg`
    /// consumes, or `None`. A call the engine answers with user code (a
    /// grammar method, a wrapped token, a custom HOW), with arguments, or to a
    /// name it cannot resolve statically is not bounded; a builtin character
    /// class is one character; a rule is the longest of its candidates' bodies.
    // Cost: O(1) expected to resolve the call (memoized), plus the bodies'
    // walk, O(t), t = their tokens, at most `depth` calls deep.
    fn rule_call_max_graphemes(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        depth: usize,
    ) -> Option<usize> {
        let spec = name.spec();
        if depth == 0
            || !spec.arg_exprs.is_empty()
            || spec.lookup_name == "::"
            || Self::may_name_lexical_regex(spec)
            || !self.registry().grammar_custom_how.is_empty()
            || self.token_method_has_wrap_chain(pkg.as_str(), &spec.lookup_name)
        {
            return None;
        }
        let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, &[]);
        if raw_empty {
            if self.subrule_names_user_method(spec, pkg) {
                return None;
            }
            // `regex_builtin_named`'s character classes match one character.
            return matches!(
                spec.lookup_name.as_str(),
                "alpha"
                    | "upper"
                    | "lower"
                    | "digit"
                    | "xdigit"
                    | "space"
                    | "alnum"
                    | "blank"
                    | "cntrl"
                    | "punct"
                    | "graph"
                    | "print"
            )
            .then_some(1);
        }
        if candidates.dispatch_failed() || candidates.is_empty() {
            return None;
        }
        let mut worst = 0;
        for (parsed, sub_pkg, _) in candidates.iter() {
            let sub_pkg = *sub_pkg;
            let bound = pattern_max_graphemes(parsed, &mut |inner| {
                self.rule_call_max_graphemes(inner, sub_pkg, depth - 1)
            })?;
            worst = worst.max(bound);
        }
        Some(worst)
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
fn pattern_max_graphemes(
    pattern: &RegexPattern,
    call: &mut dyn FnMut(&NamedAtom) -> Option<usize>,
) -> Option<usize> {
    let mut total: usize = 0;
    for token in &pattern.tokens {
        // A separated quantifier's separator contributes length too; not worth
        // modelling for a look-behind, so it is simply not bounded.
        if token.separator.is_some() {
            return None;
        }
        let atom = atom_max_graphemes(&token.atom, call)?;
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

/// An upper bound on the grapheme clusters one atom can consume. `call`
/// bounds a rule call.
fn atom_max_graphemes(
    atom: &RegexAtom,
    call: &mut dyn FnMut(&NamedAtom) -> Option<usize>,
) -> Option<usize> {
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
        | RegexAtom::CaptureIsolatedGroup(inner) => pattern_max_graphemes(inner, call),
        RegexAtom::CaptureIsolatedGroupScoped(inner, _) => pattern_max_graphemes(inner, call),

        // A branch set consumes at most its longest branch. A conjunction's
        // branches all match the same span, so the same bound is sound.
        RegexAtom::Alternation(branches)
        | RegexAtom::SequentialAlternation(branches)
        | RegexAtom::Conjunction(branches) => {
            let mut worst = 0;
            for branch in branches {
                worst = worst.max(pattern_max_graphemes(branch, call)?);
            }
            Some(worst)
        }

        // A rule call: as long as the body it runs.
        RegexAtom::Named(name) => call(name),

        // Everything else — `\n`/`\N`/`\s`
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
            derived: Default::default(),
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
            subrule_call_capture: false,
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
        assert_eq!(pattern_max_graphemes(&p, &mut |_| None), Some(1));
    }

    #[test]
    fn zero_width_atoms_cost_nothing() {
        let p = pattern(vec![
            token(RegexAtom::StartOfLine, RegexQuant::One),
            token(one_class(), RegexQuant::One),
        ]);
        assert_eq!(pattern_max_graphemes(&p, &mut |_| None), Some(1));
    }

    #[test]
    fn a_bounded_repeat_multiplies_and_an_unbounded_one_poisons() {
        let bounded = pattern(vec![token(one_class(), RegexQuant::Repeat(1, Some(3)))]);
        assert_eq!(pattern_max_graphemes(&bounded, &mut |_| None), Some(3));
        let unbounded = pattern(vec![token(one_class(), RegexQuant::OneOrMore)]);
        assert_eq!(pattern_max_graphemes(&unbounded, &mut |_| None), None);
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
        assert_eq!(pattern_max_graphemes(&p, &mut |_| None), Some(2));
    }

    #[test]
    fn a_subrule_call_is_not_bounded() {
        let p = pattern(vec![token(
            RegexAtom::Named(Default::default()),
            RegexQuant::One,
        )]);
        assert_eq!(pattern_max_graphemes(&p, &mut |_| None), None);
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
