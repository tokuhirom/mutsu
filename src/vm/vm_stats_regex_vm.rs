//! `MUTSU_VM_STATS` counters for the compiled regex engine (ADR-0135), kept
//! out of `vm_stats.rs` so that file does not grow. Reported as one line:
//!
//! `regex-vm: compiled=N declined=M runs=R reasons=(reason=count …)`
//!
//! `compiled` / `declined` count *patterns* (each is compiled at most once);
//! `runs` counts engine entries the compiled program answered. The reasons
//! are ADR-0135 D5's migration ratchet: what keeps a pattern on the walk.

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

pub(super) fn dump() {
    let map = declined_by_reason()
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let mut reasons: Vec<(&&str, &u64)> = map.iter().collect();
    reasons.sort_by(|a, b| b.1.cmp(a.1).then(a.0.cmp(b.0)));
    let declined: u64 = reasons.iter().map(|(_, n)| **n).sum();
    let reasons = reasons
        .iter()
        .map(|(k, n)| format!("{k}={n}"))
        .collect::<Vec<_>>()
        .join(" ");
    eprintln!(
        "[mutsu vm-stats] regex-vm: compiled={} declined={declined} runs={} reasons=({reasons})",
        COMPILED.load(Ordering::Relaxed),
        RUNS.load(Ordering::Relaxed)
    );
}
