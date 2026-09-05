//! BEGIN-time name visibility for hoisted sub declarations (ADR-0041 §9).
//!
//! mutsu registers every top-level `sub` of a block through a hoist pre-pass
//! emitted at the head of that block (`Compiler::hoist_sub_decls`), so a name
//! is callable from anywhere in its enclosing scope regardless of textual
//! order. Rakudo gets the same effect by installing the routine's pad entry at
//! *compile* time, which means an ordinary **runtime** reference sees the whole
//! scope — but a reference evaluated at **BEGIN** time (a `BEGIN`/`CHECK`
//! phaser body, or a `constant` initializer) only sees what compilation has
//! reached so far.
//!
//! This module carries that difference. Every hoist-pass registration records
//! what it installed and what it displaced; the declaration's own in-sequence
//! registration clears the record, because the program has now genuinely
//! reached it. While a BEGIN-time region is open, every still-recorded
//! declaration is rolled back to what it displaced (removed outright when it
//! displaced nothing), and reinstated when the region closes.
//!
//! Deriving "has this declaration been reached?" from *registry writes* does
//! not work — a declaration whose hoisted twin is byte-identical takes the
//! idempotent `SubRegisterOutcome::Unchanged` path and writes nothing (ADR-0041
//! §8). The signal used here is the `RegisterDecl` execution itself, which
//! knows whether it is the hoist copy (`__hoisted`) or the in-sequence one,
//! whatever the registration outcome turns out to be.

use crate::ast::FunctionDef;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use std::sync::Arc;

/// One registry entry a hoist-pass registration touched.
#[derive(Debug, Clone)]
pub(crate) struct HoistedEntry {
    pub(crate) key: Symbol,
    /// What the registry held under `key` immediately after the hoist-pass
    /// registration. Used as a staleness guard: if the live entry is no longer
    /// this exact `Arc`, a later declaration or a scope restore has taken the
    /// key over and the record no longer describes reality.
    pub(crate) installed: Option<Arc<FunctionDef>>,
    /// What the registry held under `key` immediately before it.
    pub(crate) displaced: Option<Arc<FunctionDef>>,
}

/// Every registry entry one hoisted declaration touched.
pub(crate) type HoistedDeclRecord = Vec<HoistedEntry>;

/// A registry key and the def to put back under it (`None` = remove).
type RegistryUndo = Vec<(Symbol, Option<Arc<FunctionDef>>)>;

impl Interpreter {
    fn hoisted_decl_key(&self, name: &str) -> Symbol {
        Symbol::intern(&format!("{}::{}", self.current_package(), name))
    }

    /// Registry keys owned by the routine `name` in the current package. A
    /// plain (non-`multi`) declaration only ever owns the single key, so it
    /// costs one intern; a `multi` family additionally spans candidate keys
    /// (`Pkg::name/2`, `Pkg::name/2:Int`, …) and pays a key scan.
    fn routine_registry_keys(&self, name: &str, multi: bool) -> Vec<Symbol> {
        let single = format!("{}::{}", self.current_package(), name);
        let single_sym = Symbol::intern(&single);
        if !multi {
            return vec![single_sym];
        }
        let multi_prefix = format!("{}/", single);
        let mut keys: Vec<Symbol> = self
            .registry()
            .functions
            .keys()
            .filter(|k| **k != single_sym && k.resolve().starts_with(&multi_prefix))
            .copied()
            .collect();
        keys.push(single_sym);
        keys
    }

    /// Snapshot the registry entries a hoist-pass registration of `name` is
    /// about to overwrite. Paired with [`Self::note_hoisted_decl`].
    pub(crate) fn capture_pre_hoist_defs(
        &self,
        name: &str,
        multi: bool,
    ) -> Vec<(Symbol, Arc<FunctionDef>)> {
        let keys = self.routine_registry_keys(name, multi);
        let registry = self.registry();
        keys.into_iter()
            .filter_map(|k| registry.functions.get(&k).map(|def| (k, def.clone())))
            .collect()
    }

    /// Record what a hoist-pass registration of `name` installed and displaced,
    /// so a BEGIN-time region opening before the declaration's own in-sequence
    /// registration can roll it back.
    pub(crate) fn note_hoisted_decl(
        &mut self,
        name: &str,
        multi: bool,
        before: Vec<(Symbol, Arc<FunctionDef>)>,
    ) {
        let mut entries: HoistedDeclRecord = Vec::new();
        for key in self.routine_registry_keys(name, multi) {
            let installed = self.registry().functions.get(&key).cloned();
            let displaced = before
                .iter()
                .find(|(k, _)| *k == key)
                .map(|(_, def)| def.clone());
            // The hoist did not change this key (same `Arc` before and after,
            // or nothing on either side): nothing to roll back.
            match (&installed, &displaced) {
                (None, None) => continue,
                (Some(a), Some(b)) if Arc::ptr_eq(a, b) => continue,
                _ => {}
            }
            entries.push(HoistedEntry {
                key,
                installed,
                displaced,
            });
        }
        // A key present before but gone after is a displacement too (a plain
        // `sub` lexically shadowing a `multi` family clears its candidate keys).
        for (key, def) in before {
            if entries.iter().any(|e| e.key == key) || self.registry().functions.contains_key(&key)
            {
                continue;
            }
            entries.push(HoistedEntry {
                key,
                installed: None,
                displaced: Some(def),
            });
        }
        let record_key = self.hoisted_decl_key(name);
        if entries.is_empty() {
            self.hoisted_unreached_decls.remove(&record_key);
        } else {
            self.hoisted_unreached_decls.insert(record_key, entries);
        }
    }

    /// The declaration has now been reached in source order: its in-sequence
    /// `RegisterDecl` is executing, so it is visible to any later BEGIN-time
    /// evaluation exactly as rakudo's compile-time pad install would be.
    pub(crate) fn mark_hoisted_decl_reached(&mut self, name: &str) {
        if self.hoisted_unreached_decls.is_empty() {
            return;
        }
        let key = self.hoisted_decl_key(name);
        self.hoisted_unreached_decls.remove(&key);
    }

    /// Enter a BEGIN-time region (`CheckPhaserStart`). At the outermost level
    /// this hides every hoisted-but-not-yet-reached declaration; a nested
    /// region only keeps the depth aligned.
    pub(crate) fn begin_time_enter(&mut self) {
        if !self.begin_time_hidden.is_empty() || self.hoisted_unreached_decls.is_empty() {
            self.begin_time_hidden.push(Vec::new());
            return;
        }
        let records: Vec<HoistedDeclRecord> =
            self.hoisted_unreached_decls.values().cloned().collect();
        let mut undo: RegistryUndo = Vec::new();
        for entry in records.into_iter().flatten() {
            let live = self.registry().functions.get(&entry.key).cloned();
            // Staleness guard: only roll back a key the hoist still owns.
            let still_owned = match (&live, &entry.installed) {
                (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                (None, None) => true,
                _ => false,
            };
            if !still_owned {
                continue;
            }
            undo.push((entry.key, live));
            self.apply_registry_entry(entry.key, entry.displaced);
        }
        if !undo.is_empty() {
            self.fn_resolve_gen += 1;
        }
        self.begin_time_hidden.push(undo);
    }

    /// Leave a BEGIN-time region (`CheckPhaserEnd`), putting back everything
    /// [`Self::begin_time_enter`] hid.
    pub(crate) fn begin_time_leave(&mut self) {
        let Some(undo) = self.begin_time_hidden.pop() else {
            return;
        };
        if undo.is_empty() {
            return;
        }
        for (key, def) in undo {
            self.apply_registry_entry(key, def);
        }
        self.fn_resolve_gen += 1;
    }

    fn apply_registry_entry(&mut self, key: Symbol, def: Option<Arc<FunctionDef>>) {
        match def {
            Some(def) => {
                self.registry_mut().functions.insert(key, def);
            }
            None => {
                self.registry_mut().functions.remove(&key);
            }
        }
    }

    /// Unwind BEGIN-time regions left open by a throw, back to `depth` — the
    /// `begin_time_hidden` length the enclosing scope was entered with. A
    /// region whose closing opcode is skipped must not leave declarations
    /// rolled out of the registry for the rest of the program.
    pub(crate) fn begin_time_unwind_to(&mut self, depth: u32) {
        while self.begin_time_hidden.len() as u32 > depth {
            self.begin_time_leave();
        }
    }
}
