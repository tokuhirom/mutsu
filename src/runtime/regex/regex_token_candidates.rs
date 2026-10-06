//! The candidates a `<subrule>` reference resolves to, with the one thing
//! that is derived from the whole list: a proto's NFA.
//!
//! A proto call ranks its candidates by measuring their declarative prefixes
//! (ADR-0046), and Rakudo does that with one NFA for the whole proto. mutsu
//! does the same: [`TokenCandidates::proto_nfa`] holds that NFA, built from
//! exactly this list the first time a call ranks it. It lives next to the list
//! and not in a table keyed by the list's address, so it cannot outlive the
//! candidates it was built from, and a list that is resolved afresh (a rule
//! with arguments, a rule redefined) starts without one.

use super::regex_ltm_nfa::LtmNfa;
use super::regex_token_resolve::ParsedTokenCandidate;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

/// The NFA of a proto and the `TOKEN_DEFS_GEN` it was built under.
type ProtoNfaSlot = (u64, Arc<LtmNfa>);

/// A resolved list of subrule candidates, in declaration order.
pub(in crate::runtime::regex) struct TokenCandidates {
    list: Vec<ParsedTokenCandidate>,
    proto_nfa: Mutex<Option<ProtoNfaSlot>>,
    /// The call is a protoless multi call whose signature dispatch died
    /// (`regex_multi_dispatch`). `list` is then every candidate, for an
    /// analysis to read; the call that runs raises the error.
    dispatch_failed: bool,
}

impl TokenCandidates {
    // Cost: O(1).
    pub(in crate::runtime::regex) fn new(list: Vec<ParsedTokenCandidate>) -> Self {
        TokenCandidates {
            list,
            proto_nfa: Mutex::new(None),
            dispatch_failed: false,
        }
    }

    /// The candidates of a call whose multi dispatch died; see
    /// [`Self::dispatch_failed`].
    // Cost: O(1).
    pub(in crate::runtime::regex) fn with_failed_dispatch(list: Vec<ParsedTokenCandidate>) -> Self {
        TokenCandidates {
            dispatch_failed: true,
            ..Self::new(list)
        }
    }

    /// Whether the call's protoless multi dispatch died: the call raises an
    /// error instead of running any of [`Self::deref`]'s candidates.
    // Cost: O(1).
    pub(in crate::runtime::regex) fn dispatch_failed(&self) -> bool {
        self.dispatch_failed
    }

    /// The proto NFA built under `generation`, if there is one.
    // Cost: O(1).
    pub(super) fn cached_proto_nfa(&self, generation: u64) -> Option<Arc<LtmNfa>> {
        let slot = self.proto_nfa.lock().ok()?;
        slot.as_ref()
            .filter(|(built, _)| *built == generation)
            .map(|(_, nfa)| Arc::clone(nfa))
    }

    /// Keep `nfa`, built under `generation`, for the next ranking.
    // Cost: O(1).
    pub(super) fn store_proto_nfa(&self, generation: u64, nfa: &Arc<LtmNfa>) {
        if let Ok(mut slot) = self.proto_nfa.lock() {
            *slot = Some((generation, Arc::clone(nfa)));
        }
    }
}

impl Deref for TokenCandidates {
    type Target = Vec<ParsedTokenCandidate>;

    fn deref(&self) -> &Vec<ParsedTokenCandidate> {
        &self.list
    }
}
