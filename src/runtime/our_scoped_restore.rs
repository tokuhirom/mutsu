//! Which `our`-scoped subs a routine-registry restore puts back
//! (`restore_routine_registry_impl`).
//!
//! An `our sub` outlives the block that declared it, so a block-scope restore
//! re-inserts every `our_scoped_functions` entry the restored functions map
//! lacks: those of the current package, and those the block itself added. That
//! check walks the whole our-scoped table, O(o) per restore, o = `our` subs
//! registered — paid on every exit of a block that declares a routine, so
//! `for ^N { { sub g {} } }` in a program with many `our` subs cost O(N·o)
//! (#9170).
//!
//! When the block did not touch the our-scoped table, the answer depends only
//! on that table, the restored map's content and the current package, and a
//! loop body that declares a routine restores the same map under the same
//! table on every iteration. So the answer for that case is memoized: one
//! entry, keyed by the table itself (an `Arc` the memo holds, so its address
//! cannot be reused for another table while the entry lives), the restored
//! map's version stamp (a version names one content, `runtime::function_table`)
//! and the package.

use super::*;
use crate::runtime::function_table::FunctionTable;
use std::sync::Arc;

type OurTable = rustc_hash::FxHashMap<Symbol, Arc<FunctionDef>>;

/// The last answer [`Interpreter::our_scoped_to_reinstate`] computed for an
/// untouched our-scoped table.
#[derive(Clone, Default)]
pub(crate) struct OurScopedRestoreMemo {
    table: Option<Arc<OurTable>>,
    functions_version: u64,
    package: Option<Symbol>,
    keep: Arc<[(Symbol, Arc<FunctionDef>)]>,
}

impl Interpreter {
    /// The our-scoped subs a restore to `functions` re-inserts: each entry of
    /// `our_scoped_functions` missing from `functions` whose package is the
    /// current one, or that is absent from `snapshot` (the table the scope
    /// started with) — registered by the scope itself.
    // Cost: O(1) when `snapshot` is still the installed table and the same
    // restore was answered before (the steady state of a loop body that
    // declares a routine); otherwise O(o), o = our-scoped subs registered.
    pub(crate) fn our_scoped_to_reinstate(
        &mut self,
        functions: &FunctionTable,
        snapshot: &Arc<OurTable>,
    ) -> Arc<[(Symbol, Arc<FunctionDef>)]> {
        let package = self.current_package_sym();
        let registry = self.registry();
        let table = &registry.our_scoped_functions;
        let untouched = Arc::ptr_eq(snapshot, table);
        if untouched {
            let memo = &registry.our_scoped_restore_memo;
            if memo.table.as_ref().is_some_and(|t| Arc::ptr_eq(t, table))
                && memo.functions_version == functions.version()
                && memo.package == Some(package)
            {
                return memo.keep.clone();
            }
        }
        let keep: Arc<[(Symbol, Arc<FunctionDef>)]> = table
            .iter()
            .filter(|(key, def)| {
                !functions.contains_key(key)
                    && (def.package == package || !snapshot.contains_key(*key))
            })
            .map(|(key, def)| (*key, def.clone()))
            .collect();
        let table = table.clone();
        drop(registry);
        if untouched {
            self.registry_mut().our_scoped_restore_memo = OurScopedRestoreMemo {
                table: Some(table),
                functions_version: functions.version(),
                package: Some(package),
                keep: keep.clone(),
            };
        }
        keep
    }
}
