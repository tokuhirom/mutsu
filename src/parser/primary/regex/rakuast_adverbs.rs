//! The outer adverbs of a regex literal, as RakuAST sees them.
//!
//! Rakudo keeps `rx:i/a/`'s adverbs on the `QuotedRegex` node rather than in
//! its body, so the source tree records them beside the body it parses.

use super::adverbs::MatchAdverbs;
use crate::regex_tree::RegexAdverb;

/// The outer adverbs of an `rx//` (or, with `match_time`, an `m//`) as the
/// `QuotedRegex` adverbs the RakuAST lowering can turn back into the same
/// execution value, spelled as written (`g`, `2x`, `x(2)`, `nth(1,3)`). `None`
/// when any adverb is outside that set, or a position argument is an
/// expression only evaluated at match time; the literal then keeps its
/// value-only path.
// Cost: O(a), a = number of adverbs.
pub(super) fn rakuast_regex_adverbs(
    adverbs: &MatchAdverbs,
    match_time: bool,
) -> Option<Vec<RegexAdverb>> {
    if adverbs.pos_expr.is_some() || adverbs.continue_expr.is_some() {
        return None;
    }
    adverbs
        .source
        .iter()
        .map(|(name, argument)| {
            let base = name.trim_start_matches(|c: char| c.is_ascii_digit());
            let modifier = argument.is_none()
                && name == base
                && matches!(
                    base,
                    "i" | "ignorecase" | "m" | "ignoremark" | "s" | "sigspace" | "r" | "ratchet"
                );
            let match_adverb = match_time
                && matches!(
                    base,
                    "g" | "global"
                        | "ex"
                        | "exhaustive"
                        | "ov"
                        | "overlap"
                        | "x"
                        | "nth"
                        | "st"
                        | "nd"
                        | "rd"
                        | "th"
                        | "p"
                        | "pos"
                        | "c"
                        | "continue"
                );
            (modifier || match_adverb).then(|| RegexAdverb {
                name: name.clone(),
                argument: argument.clone(),
            })
        })
        .collect()
}
