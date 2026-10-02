//! `MUTSU_VM_STATS` counters for the compiled regex engine (ADR-0135), kept
//! out of `vm_stats.rs` so that file does not grow. Reported as one line:
//!
//! `regex-vm: compiled=N declined=M runs=R reasons=(reason=count …)`
//!
//! `compiled` / `declined` count *patterns* (each is compiled at most once);
//! `runs` counts engine entries the compiled program answered. The reasons
//! are ADR-0135 D5's migration ratchet: what keeps a pattern on the walk.
//!
//! A second line counts every *use* of the walk's code, declined pattern or
//! not (`WalkUse`): the whole residue §4 E must bring to zero.
//!
//! `regex-walk: walked=N (reason=count …) bridged=M (…) leaf=L (…)`

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicU64, Ordering};

use super::vm_stats::enabled;

static COMPILED: AtomicU64 = AtomicU64::new(0);
static RUNS: AtomicU64 = AtomicU64::new(0);

fn declined_by_reason() -> &'static Mutex<HashMap<&'static str, u64>> {
    static MAP: std::sync::OnceLock<Mutex<HashMap<&'static str, u64>>> = std::sync::OnceLock::new();
    MAP.get_or_init(Default::default)
}

/// One pattern's compile outcome: `None` compiled, `Some(reason)` declined.
pub(crate) fn record_regex_vm_compile(declined: Option<&'static str>) {
    if !enabled() {
        return;
    }
    match declined {
        None => {
            COMPILED.fetch_add(1, Ordering::Relaxed);
        }
        Some(reason) => {
            let mut map = declined_by_reason()
                .lock()
                .unwrap_or_else(|e| e.into_inner());
            *map.entry(reason).or_insert(0) += 1;
        }
    }
}

/// One engine entry answered by a compiled program.
#[inline]
pub(crate) fn record_regex_vm_run() {
    if enabled() {
        RUNS.fetch_add(1, Ordering::Relaxed);
    }
}

/// How a match used the tree walk's code instead of the compiled engine — the
/// residue ADR-0135 §4 E has to remove before the walk can be deleted. Unlike
/// `declined` above, these count *events*, not patterns.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum WalkUse {
    /// A whole match the walk answered: the pattern has no program, the
    /// dynamic context keeps the compiled engine out, or the entry point
    /// (every end at a position) has no compiled form.
    Walked,
    /// A `<subrule>` call inside a compiled run that the walk's producer
    /// answered (D5's bridge), or an interpolated pattern whose ends the walk
    /// computed.
    Bridged,
    /// One atom of a compiled program the walk's single-atom arm matched.
    Leaf,
}

impl WalkUse {
    fn label(self) -> &'static str {
        match self {
            WalkUse::Walked => "walked",
            WalkUse::Bridged => "bridged",
            WalkUse::Leaf => "leaf",
        }
    }
}

type WalkUses = HashMap<(WalkUse, &'static str), u64>;

fn walk_uses() -> &'static Mutex<WalkUses> {
    static MAP: std::sync::OnceLock<Mutex<WalkUses>> = std::sync::OnceLock::new();
    MAP.get_or_init(Default::default)
}

/// One use of the walk's code, with the reason the compiled engine did not
/// answer it itself.
#[inline]
pub(crate) fn record_regex_walk(kind: WalkUse, reason: &'static str) {
    if enabled() {
        let mut map = walk_uses().lock().unwrap_or_else(|e| e.into_inner());
        *map.entry((kind, reason)).or_insert(0) += 1;
    }
}

/// `n=… (reason=count …)` for the counts in `counts`, highest first.
fn reason_list(counts: &mut [(&'static str, u64)]) -> String {
    counts.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(b.0)));
    counts
        .iter()
        .map(|(k, n)| format!("{k}={n}"))
        .collect::<Vec<_>>()
        .join(" ")
}

pub(super) fn dump() {
    let map = declined_by_reason()
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let mut reasons: Vec<(&'static str, u64)> = map.iter().map(|(k, n)| (*k, *n)).collect();
    let declined: u64 = reasons.iter().map(|(_, n)| *n).sum();
    let reasons = reason_list(&mut reasons);
    eprintln!(
        "[mutsu vm-stats] regex-vm: compiled={} declined={declined} runs={} reasons=({reasons})",
        COMPILED.load(Ordering::Relaxed),
        RUNS.load(Ordering::Relaxed)
    );
    let uses = walk_uses().lock().unwrap_or_else(|e| e.into_inner());
    let groups = [WalkUse::Walked, WalkUse::Bridged, WalkUse::Leaf].map(|kind| {
        let mut counts: Vec<(&'static str, u64)> = uses
            .iter()
            .filter(|((k, _), _)| *k == kind)
            .map(|((_, reason), n)| (*reason, *n))
            .collect();
        let total: u64 = counts.iter().map(|(_, n)| *n).sum();
        format!("{}={total} ({})", kind.label(), reason_list(&mut counts))
    });
    eprintln!("[mutsu vm-stats] regex-walk: {}", groups.join(" "));
}
