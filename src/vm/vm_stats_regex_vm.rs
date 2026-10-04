//! `MUTSU_VM_STATS` counters for the compiled regex engine (ADR-0135), kept
//! out of `vm_stats.rs` so that file does not grow. Reported as one line:
//!
//! `regex-vm: compiled=N declined=M runs=R reasons=(reason=count …)`
//!
//! `compiled` / `declined` count *patterns* (each is compiled at most once);
//! `runs` counts engine entries the compiled program answered. The reasons
//! name the constructs the engine does not implement (such a match raises).
//!
//! A second line counts the compiled engine's *eager* calls: a `<subrule>`
//! call whose ends the growing-seed loop computes up front (`regex_lr_seed`)
//! instead of a frame resuming them. They run compiled programs too; the
//! reasons say why the call is not a frame.
//!
//! `regex-eager: calls=N (reason=count …)`

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

fn eager_calls() -> &'static Mutex<HashMap<&'static str, u64>> {
    static MAP: std::sync::OnceLock<Mutex<HashMap<&'static str, u64>>> = std::sync::OnceLock::new();
    MAP.get_or_init(Default::default)
}

/// One eager `<subrule>` call of the compiled engine, with why it is not a
/// frame.
#[inline]
pub(crate) fn record_regex_eager(reason: &'static str) {
    if enabled() {
        let mut map = eager_calls().lock().unwrap_or_else(|e| e.into_inner());
        *map.entry(reason).or_insert(0) += 1;
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
    let eager = eager_calls().lock().unwrap_or_else(|e| e.into_inner());
    let mut counts: Vec<(&'static str, u64)> = eager.iter().map(|(k, n)| (*k, *n)).collect();
    let total: u64 = counts.iter().map(|(_, n)| *n).sum();
    eprintln!(
        "[mutsu vm-stats] regex-eager: calls={total} ({})",
        reason_list(&mut counts)
    );
}
