//! The `MUTSU_VM_STATS` switch, below every layer so a lower-layer counter
//! (`value::regex_caps::stats`) and `vm::vm_stats` read the same gate
//! (#10779).

use std::sync::OnceLock;

/// Whether instrumentation is active. Resolved once from the environment so the
/// hot path is a single cached boolean load when the feature is off.
// Cost: O(1).
#[inline]
pub(crate) fn enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("MUTSU_VM_STATS").is_some())
}
