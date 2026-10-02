//! `MUTSU_VM_STATS` counters for the lazy `Match` representation. They live
//! beside the capture types they count so `Value` does not reach up into
//! `vm::vm_stats` (#10779); `vm_stats::dump` reads them through [`snapshot`].

use crate::stats_gate::enabled;
use std::sync::atomic::{AtomicU64, Ordering};

// ADR-0016 P3: how many leaf captures reached the Match builder WITHOUT a
// span carrier (text-axis only — their offsets are reported as 0..len of the
// captured text, the position search having been retired), vs. leaves whose
// span came from a recorded carrier node. Non-zero `searches` means an
// exploded-builder caller still passes bare text.
static REGEX_MATCH_LEAF_SEARCHES: AtomicU64 = AtomicU64::new(0);
static REGEX_MATCH_LEAF_SPANS: AtomicU64 = AtomicU64::new(0);
// ADR-0016 P5 guard: every first `view()` of a lazy Match forces its
// Instance-shaped attribute map. This makes accidental `view()`-based tag
// probes visible in instrumented grammar/regex runs instead of silently
// eroding the lazy representation.
static REGEX_MATCH_MATERIALIZATIONS: AtomicU64 = AtomicU64::new(0);
// #8247 guard: how many times a whole subject was materialized as a
// `MatchTarget` (an `Arc<String>` copy plus an `Arc<[char]>`, ~5 bytes per
// character). One per regex *operation* is correct; one per match makes an
// operation that scans repeatedly -- `.split(rx)`, `.subst(rx, :g)`, `s:g///`
// -- quadratic in subject length, which is what this counts.
static REGEX_MATCH_TARGETS_BUILT: AtomicU64 = AtomicU64::new(0);

/// A Match-builder leaf capture arrived without a span carrier (`searched ==
/// true` — the legacy text-only shape) or read a recorded span (`false`).
pub(crate) fn record_regex_match_leaf(searched: bool) {
    if enabled() {
        if searched {
            REGEX_MATCH_LEAF_SEARCHES.fetch_add(1, Ordering::Relaxed);
        } else {
            REGEX_MATCH_LEAF_SPANS.fetch_add(1, Ordering::Relaxed);
        }
    }
}

/// Record the first materialization of one lazy `Match` node.
#[inline]
pub(crate) fn record_regex_match_materialization() {
    if enabled() {
        REGEX_MATCH_MATERIALIZATIONS.fetch_add(1, Ordering::Relaxed);
    }
}

/// Record one whole-subject `MatchTarget` construction (see
/// `REGEX_MATCH_TARGETS_BUILT`).
#[inline]
pub(crate) fn record_regex_match_target_built() {
    if enabled() {
        REGEX_MATCH_TARGETS_BUILT.fetch_add(1, Ordering::Relaxed);
    }
}

/// The counters above, as `vm_stats::dump` prints them.
pub(crate) struct MatchCounts {
    pub(crate) leaf_searches: u64,
    pub(crate) leaf_spans: u64,
    pub(crate) match_materializations: u64,
    pub(crate) match_targets: u64,
}

// Cost: O(1).
pub(crate) fn snapshot() -> MatchCounts {
    MatchCounts {
        leaf_searches: REGEX_MATCH_LEAF_SEARCHES.load(Ordering::Relaxed),
        leaf_spans: REGEX_MATCH_LEAF_SPANS.load(Ordering::Relaxed),
        match_materializations: REGEX_MATCH_MATERIALIZATIONS.load(Ordering::Relaxed),
        match_targets: REGEX_MATCH_TARGETS_BUILT.load(Ordering::Relaxed),
    }
}
