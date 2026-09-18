//! One identity per file, for the report's tables.
//!
//! mutsu names a source file two ways, and a profile reads both. A chunk's
//! `CompiledCode::source_file` is the **canonicalized** path (`run.rs` runs the
//! program path through `fs::canonicalize` before publishing the unit), while a
//! `RoutineFrame`'s call-site `file` is `$?FILE` — the path *as the user
//! spelled it on the command line*. Run `mutsu prof.raku` from its own
//! directory and the same file appears as both `/abs/path/prof.raku` and
//! `prof.raku`, so a table keyed on the raw symbols splits one file's time in
//! two and a caller row cannot be matched to the line row above it.
//!
//! The report therefore picks one identity: the canonical path when the name
//! resolves to a real file, and the name unchanged when it does not — which is
//! what keeps synthetic unit names (`EVAL_1`, `<unknown>`) intact instead of
//! turning them into a path under the current directory.
//!
//! This is a *report-side* reconciliation, deliberately: making the runtime's
//! two identities agree means changing what `$?FILE`, backtraces and
//! `CallFrame.file` report, which several `t/` tests pin and which is not this
//! slice's decision to make. [#8719] tracks settling it at the source.
//!
//! [#8719]: https://github.com/tokuhirom/mutsu/issues/8719

use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::sync::{Mutex, OnceLock};

fn memo() -> &'static Mutex<FxHashMap<Symbol, Symbol>> {
    static MEMO: OnceLock<Mutex<FxHashMap<Symbol, Symbol>>> = OnceLock::new();
    MEMO.get_or_init(|| Mutex::new(FxHashMap::default()))
}

/// The identity the report uses for `file`.
///
/// Called only while a snapshot is being built — once per distinct symbol per
/// process, and never from the sample path, so the `canonicalize` syscall it
/// may perform costs a handful of calls per run.
pub(crate) fn canonical(file: Symbol) -> Symbol {
    let mut memo = memo()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    if let Some(known) = memo.get(&file) {
        return *known;
    }
    let resolved = match std::fs::canonicalize(file.as_str()) {
        Ok(path) => Symbol::intern(&path.to_string_lossy()),
        // Not a path on this filesystem: an `EVAL_<n>` unit name, `<unknown>`,
        // or a file that has since been removed. Its own name is its identity.
        Err(_) => file,
    };
    memo.insert(file, resolved);
    resolved
}

pub(crate) fn canonical_opt(file: Option<Symbol>) -> Option<Symbol> {
    file.map(canonical)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_name_that_is_not_a_path_keeps_its_own_identity() {
        let eval_unit = Symbol::intern("EVAL_7");
        assert_eq!(canonical(eval_unit), eval_unit);
        // Memoized, so the second call cannot disagree with the first.
        assert_eq!(canonical(eval_unit), eval_unit);
    }

    #[test]
    fn two_spellings_of_one_file_resolve_to_one_identity() {
        let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let absolute = Symbol::intern(&dir.join("Cargo.toml").to_string_lossy());
        let indirect = Symbol::intern(&dir.join("src/../Cargo.toml").to_string_lossy());
        assert_ne!(absolute, indirect, "the fixture must start out disagreeing");
        assert_eq!(canonical(absolute), canonical(indirect));
    }
}
