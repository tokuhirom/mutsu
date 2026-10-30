//! The import scope an `EVAL` opens around its compunit (#11069), and the
//! env region every import scope runs over.
//!
//! What a `use` imports is lexical to the unit that holds it. A `use`-holding
//! block rolls back the routine/class registries with
//! `push_import_scope`/`pop_import_scope`, and runs over an env *import
//! region* ([`ImportRegion`]): the names its imports bind live in a layer the
//! region drops whole on exit (ADR-14140). An `EVAL` already rolls back its
//! registries itself (`eval_eval_string_unit`'s snapshots, which know to keep
//! an EVAL's own `our sub` and the classes it declares), so it needs only the
//! env half: the aliases `import_module` wrote (a `sub EXPORT` hook's
//! terms, an exported class name) must not outlive the EVAL.

use super::*;

/// An env import region opened by [`Interpreter::open_import_region`].
pub(crate) struct ImportRegion {
    id: u32,
}

impl Interpreter {
    /// Open the env import region of the import scope just pushed: the scope
    /// records the layer's id, and its imports bind there.
    // Cost: O(1), or O(v) once when the env chain is at the depth cap (one
    // flatten, v = visible names).
    pub(crate) fn open_import_region(&mut self) -> ImportRegion {
        if self.env().needs_flatten_for_import_region() {
            let flat = self.env().flattened();
            *self.env_mut() = flat;
        }
        let id = crate::env::next_import_layer_id();
        let entry = std::mem::replace(self.env_mut(), Env::empty_placeholder());
        match Env::open_import_region(entry, id) {
            Ok(tier) => *self.env_mut() = tier,
            Err(_) => unreachable!("the chain was flattened below the depth cap"),
        }
        if let Some(top) = self.module.import_scope_stack.last_mut() {
            top.import_layer = id;
        }
        ImportRegion { id }
    }

    /// Leave an import region: the region's by-name writes merge into the
    /// enclosing env (the block-level module-merge grants stay behind), and
    /// the import layer is dropped.
    // Cost: O(w), w = the region's by-name writes.
    pub(crate) fn close_import_region(&mut self, region: ImportRegion) {
        let current = std::mem::replace(self.env_mut(), Env::empty_placeholder());
        match current.close_import_region(region.id) {
            Ok(closed) => {
                let mut restored = closed.outer;
                let grant = crate::meta_ns::MetaNs::ModuleMerge.prefix();
                for (key, value) in closed.overlay.iter() {
                    if key.with_str(|k| k.starts_with(grant)) {
                        continue;
                    }
                    restored.insert_sym(*key, value.clone());
                }
                if let Some(removed) = closed.tombstones {
                    for key in removed {
                        restored.remove_sym(key);
                    }
                }
                *self.env_mut() = restored;
            }
            Err(replaced) => {
                // The env was replaced wholesale while the region ran: no
                // layer to drop. Keep what is there; the mismatch is a bug
                // elsewhere (no test or whitelisted roast file reaches it).
                debug_assert!(false, "an import region's env was replaced wholesale");
                *self.env_mut() = replaced;
            }
        }
    }

    /// Run `body` (an `EVAL`'s compunit) with its imported `env` aliases
    /// dropped again afterwards, on every exit path, along with the alias
    /// bookkeeping (`unit_env_imports`) they were recorded in.
    // Cost: O(w) plus the body, w = by-name writes the body made (the
    // import-scope snapshot holds the copy-on-write registry tables, O(1)).
    pub(crate) fn with_eval_import_scope<T>(
        &mut self,
        body: impl FnOnce(&mut Self) -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        self.push_import_scope();
        let result = body(self);
        if let Some(snapshot) = self.module.import_scope_stack.pop() {
            let ImportScopeSnapshot {
                unit_env_imports,
                importer_type_aliases,
                ..
            } = snapshot;
            self.restore_importer_type_aliases(importer_type_aliases);
            self.module.unit_env_imports = unit_env_imports;
        }
        result
    }
}
