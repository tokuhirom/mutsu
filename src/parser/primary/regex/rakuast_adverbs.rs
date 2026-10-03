//! The outer adverbs of a regex literal, as RakuAST sees them.
//!
//! Rakudo keeps `rx:i/a/`'s adverbs on the `QuotedRegex` node rather than in
//! its body, so the source tree records them beside the body it parses.

use super::adverbs::MatchAdverbs;
use crate::regex_tree::RegexAdverb;

/// The outer adverbs of an `rx//` (or, with `match_time`, an `m//`) as the
/// `QuotedRegex` adverbs the RakuAST lowering can turn back into the same
/// execution value. `None` when any adverb is outside that set or takes an
/// argument; the literal then keeps its value-only path.
// Cost: O(a), a = number of adverbs.
pub(super) fn rakuast_regex_adverbs(
    adverbs: &MatchAdverbs,
    match_time: bool,
) -> Option<Vec<RegexAdverb>> {
    adverbs
        .source
        .iter()
        .map(|(name, argument)| {
            let modifier = matches!(
                name.as_str(),
                "i" | "ignorecase" | "m" | "ignoremark" | "s" | "sigspace" | "r" | "ratchet"
            );
            let representable = modifier || (match_time && matches!(name.as_str(), "g" | "global"));
            (argument.is_none() && representable).then(|| RegexAdverb {
                name: name.clone(),
                argument: None,
            })
        })
        .collect()
}
