//! The deferred `Seq` sources that need no interpreter to pull
//! ([`crate::value::SeqSource::Pure`]): a `Str.comb` / `.lines` / `.words`
//! cursor ([`crate::value::StrIterSpec`]) or a native list iterator
//! ([`crate::value::ListGen`]). `SeqBody` cuts one on its first read, and a
//! consuming `.head(n)` / `.first` or a subscript pulls only the prefix it
//! needs.

use crate::value::Value;

/// A deferred source that needs no interpreter to pull ([`crate::value::SeqSource::Pure`]):
/// a `Str` cursor or a native list iterator.
#[derive(Clone)]
pub(crate) enum PureCursor {
    Str(crate::value::StrIterSpec),
    List(crate::value::ListGen),
}

impl PureCursor {
    /// Append up to `n` more elements to `out` (`usize::MAX` pulls them all).
    // Cost: O(n) pulls; see `StrIterSpec::push_up_to` / `ListGen::pull_one`
    // for the cost of one.
    pub(crate) fn push_up_to(&mut self, out: &mut Vec<Value>, n: usize) {
        match self {
            PureCursor::Str(spec) => spec.push_up_to(out, n),
            PureCursor::List(list_gen) => list_gen.push_up_to(out, n),
        }
    }

    /// Whether every element this cursor yields is defined (a `Str`, an
    /// `Int`, a `Pair` or an `Array`), so a reader asking only "is this a
    /// one-element Seq of a Nil" can answer without pulling.
    // Cost: O(1).
    pub(crate) fn never_nilish(&self) -> bool {
        match self {
            PureCursor::Str(_) => true,
            PureCursor::List(list_gen) => list_gen.never_nilish(),
        }
    }
}
