//! `MUTSU_TRIR_DUMP`: report each routine's eligibility decision, and with
//! `MUTSU_TRIR_DUMP=ops` list the accepted chunk one op a line, followed by
//! its constants (what a `LoadBareWord(i)` or `ConstObj(i)` names).

use super::TrChunk;
use crate::symbol::Symbol;

/// The dump mode, read once: this runs per routine declaration.
fn mode() -> Option<&'static str> {
    use std::sync::OnceLock;
    static MODE: OnceLock<Option<String>> = OnceLock::new();
    MODE.get_or_init(|| std::env::var("MUTSU_TRIR_DUMP").ok())
        .as_deref()
}

pub(super) fn report(name: Symbol, chunk: Option<&TrChunk>) {
    let Some(mode) = mode() else {
        return;
    };
    let Some(c) = chunk else {
        eprintln!("trir: {} declined", name.as_str());
        return;
    };
    eprintln!(
        "trir: {} accepted ({} ops, {} native slots, {} obj slots, {} outers, {} calls)",
        name.as_str(),
        c.ops.len(),
        c.n_native,
        c.n_obj,
        c.outers.len(),
        c.calls.len(),
    );
    if mode == "ops" {
        for (i, op) in c.ops.iter().enumerate() {
            eprintln!("  {i:4} {op:?}");
        }
        for (i, k) in c.constants.iter().enumerate() {
            eprintln!("  const {i}: {}", k.to_string_value());
        }
    }
}
