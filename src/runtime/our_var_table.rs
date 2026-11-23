//! The `our` variable store as one world table (ADR-17216 slice 4b).
//!
//! `our` variables are package state: rakudo keeps one stash per package per
//! process, so an `our` declared, assigned or rebound on one thread is seen by
//! every other. mutsu's store
//! ([`OurVarTable`](crate::runtime::our_var_table::OurVarTable)) is a
//! [`WorldCell`](crate::runtime::world_cell::WorldCell) shared by every
//! interpreter of the process: reads work on this thread's snapshot, writes
//! publish under the table's lock.
//!
//! No `&mut Value` into the table ever escapes its lock. The in-place
//! mutation chokepoint (`env_root_descended_mut`) pins the stored value in the
//! mutating thread's pin slot
//! ([`Interpreter::our_var_pinned_root`](crate::runtime::Interpreter::our_var_pinned_root)) and mutates
//! through the pin: a container (`Array`/`Hash` node, a `ContainerRef` cell)
//! is shared by the table and the pin, so the mutation lands on the very node
//! every thread reads, and the pin keeps it alive while another thread may
//! rebind the key.

use std::sync::Arc;

use rustc_hash::FxHashSet;

use crate::runtime::registry_sharded_map::ShardedMap;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

use super::Interpreter;

/// The `our` variables of the program. See the module docs.
///
/// Copying the table (a write while another thread holds a snapshot) is a
/// reference-count bump per shard of `vars` plus one per index set; the write
/// then copies the one shard it touches (and an index set only when it adds a
/// name to it).
#[derive(Clone, Default, Debug)]
pub(crate) struct OurVarTable {
    /// The variables, keyed by their (package-qualified) store spelling.
    /// Append-only: a key is never removed.
    vars: ShardedMap<String, Value>,
    /// The UNQUALIFIED spelling (`@words`, `$x`) of every key ever stored in
    /// `vars` — the sigil plus the segment after the last `::`.
    ///
    /// `our_package_var_key` reconstructs a package-qualified candidate for a
    /// bare name by walking up to four candidate packages' `::` chains, one
    /// `format!` + store probe per step, on EVERY `@`/`%` container read once a
    /// program declares any `our` variable at all. Every candidate it builds
    /// ends in `<sigil><bare>`, so a name absent from this set cannot match any
    /// stored key and the whole walk can be skipped — which is the common case
    /// even in a module that does declare `our` variables (#7571).
    ///
    /// Append-only, exactly like `vars`, so a membership test can never be
    /// stale.
    unqualified: Arc<FxHashSet<Symbol>>,
    /// The bare keys (`a`, `@arr`, `%h`) of `vars`: the file-scope `our`
    /// variables, whose package-qualified spelling collapses to the bare name.
    /// A `gather` body's captured env keeps only the body's own free
    /// variables, so a routine it calls would not find these; the gather
    /// creation copies them in (`exec_make_gather_op`).
    global_names: Arc<FxHashSet<Symbol>>,
    /// The keys (`Pkg::answer`) that hold a sigil-less `our constant`. The
    /// store spells such a key like a scalar's, so this is the only record
    /// that `answer` is a term and not `$answer` (#11877).
    term_keys: Arc<FxHashSet<Symbol>>,
    /// The keys (`Pkg::Baz`) that hold a `$` scalar. The store spells a
    /// scalar's key without its sigil, and an uppercase-leading tail otherwise
    /// reads as a type or term, so this is the only record that `Baz` is
    /// `$Baz` (#17364).
    scalar_keys: Arc<FxHashSet<Symbol>>,
    /// The `our`-scoped routines (`our sub`, a package's `sub`), keyed by
    /// their package-qualified name, as published to the package stash. A
    /// thread's own routine tables are lexical (ADR-17216 §3.2) and keep
    /// their per-thread `our_scoped_functions`; this is the world copy an
    /// `our sub` declared at run time on one thread (`EVAL`, a `require`) is
    /// found through on every other.
    routines: ShardedMap<Symbol, Arc<crate::ast::FunctionDef>>,
    /// The bare names a program bound a routine for into the setting's
    /// stash (`&CORE::none := &f`, `CORE::<&none> := &f`, stored under
    /// `&CORE::none`). A call of such a name that no user declaration
    /// shadows calls the bound routine (#18779). Append-only, like `vars`.
    core_bound_routines: Arc<FxHashSet<Symbol>>,
}

impl OurVarTable {
    // Cost: O(|key|), one hash and one shard probe.
    #[inline]
    pub(crate) fn get(&self, key: &str) -> Option<&Value> {
        self.vars.get(key)
    }

    // Cost: O(|key|).
    #[inline]
    pub(crate) fn contains_key(&self, key: &str) -> bool {
        self.vars.contains_key(key)
    }

    // Cost: O(1).
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.vars.is_empty()
    }

    // Cost: O(n), n = stored variables.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.vars.iter()
    }

    // Cost: O(n), n = stored variables.
    pub(crate) fn keys(&self) -> impl Iterator<Item = &String> {
        self.vars.keys()
    }

    // Cost: O(n), n = stored variables.
    pub(crate) fn values(&self) -> impl Iterator<Item = &Value> {
        self.vars.values()
    }

    /// Whether some stored key's unqualified part is `unqualified`.
    // Cost: O(1).
    #[inline]
    pub(crate) fn has_unqualified(&self, unqualified: Symbol) -> bool {
        self.unqualified.contains(&unqualified)
    }

    /// The file-scope `our` variables' bare keys.
    pub(crate) fn global_names(&self) -> &FxHashSet<Symbol> {
        &self.global_names
    }

    /// Whether a routine was bound into the setting's stash under `name`.
    // Cost: O(1).
    #[inline]
    pub(crate) fn core_routine_bound(&self, name: Symbol) -> bool {
        !self.core_bound_routines.is_empty() && self.core_bound_routines.contains(&name)
    }

    // Cost: O(1).
    pub(crate) fn is_term_key(&self, key: Symbol) -> bool {
        self.term_keys.contains(&key)
    }

    // Cost: O(1).
    pub(crate) fn is_scalar_key(&self, key: Symbol) -> bool {
        self.scalar_keys.contains(&key)
    }

    /// Store `value` under `key` exactly as spelled, keeping the indexes.
    // Cost: O(|key|) plus, when another thread's snapshot shares it, a copy
    // of the touched shard (and of an index set the key is new to).
    fn insert(&mut self, key: String, value: Value) {
        let key_sym = Symbol::intern(&key);
        let unqualified = crate::qualified::unqualified_part(key_sym);
        if !self.unqualified.contains(&unqualified) {
            Arc::make_mut(&mut self.unqualified).insert(unqualified);
        }
        if !crate::qualified::is_qualified(key_sym)
            && !key.contains('\u{0}')
            && !self.global_names.contains(&key_sym)
        {
            Arc::make_mut(&mut self.global_names).insert(key_sym);
        }
        if let Some(bare) = key.strip_prefix("&CORE::")
            && !bare.is_empty()
        {
            let bare = Symbol::intern(bare);
            if !self.core_bound_routines.contains(&bare) {
                Arc::make_mut(&mut self.core_bound_routines).insert(bare);
            }
        }
        match self.vars.get_mut(key.as_str()) {
            Some(slot) => *slot = value,
            None => {
                self.vars.insert(key, value);
            }
        }
    }

    /// Mark `key` as a sigil-less term (`true`) or a `$` scalar (`false`).
    // Cost: O(1) plus a copy of a shared index set.
    fn mark_term_or_scalar(&mut self, key: Symbol, term: bool) {
        let (add, drop) = if term {
            (&mut self.term_keys, &mut self.scalar_keys)
        } else {
            (&mut self.scalar_keys, &mut self.term_keys)
        };
        if !add.contains(&key) {
            Arc::make_mut(add).insert(key);
        }
        if drop.contains(&key) {
            Arc::make_mut(drop).remove(&key);
        }
    }
}

impl Interpreter {
    /// Publish the `our`-scoped routine `key` (package-qualified) to every
    /// thread.
    // Cost: O(1) plus the table write (a copy of one shared shard).
    pub(crate) fn publish_our_routine(&self, key: Symbol, def: Arc<crate::ast::FunctionDef>) {
        if self
            .our_table()
            .routines
            .get(&key)
            .is_some_and(|stored| Arc::ptr_eq(stored, &def))
        {
            return;
        }
        self.lexicals.our_vars.write(|table| {
            table.routines.insert(key, def);
        });
    }

    /// The `our`-scoped routine `key` (package-qualified): this thread's own
    /// (`our_scoped_functions`) first, else the one another thread published.
    // Cost: O(1), two hash probes.
    pub(crate) fn our_scoped_routine(&self, key: Symbol) -> Option<Arc<crate::ast::FunctionDef>> {
        if let Some(def) = self.routines().our_scoped_functions.get(&key) {
            return Some(def.clone());
        }
        let table = self.our_table();
        if table.routines.is_empty() {
            return None;
        }
        table.routines.get(&key).cloned()
    }

    /// The `our`-scoped routines another thread published that this thread's
    /// own routine table does not hold: for a package stash (`Pkg::<&f>`,
    /// `OUR::<&f>`), which lists the routines of the thread that builds it
    /// (ADR-17216 §3.5). `candidates` narrows the read to those keys.
    // Cost: O(r), r = published routines (or the candidates given).
    pub(crate) fn published_routines_not_local(
        &self,
        candidates: Option<&[Symbol]>,
    ) -> Vec<(Symbol, Arc<crate::ast::FunctionDef>)> {
        let table = self.our_table();
        if table.routines.is_empty() {
            return Vec::new();
        }
        let local = self.routines();
        let unseen = |key: &Symbol| !local.functions.contains_key(key);
        match candidates {
            Some(names) => names
                .iter()
                .filter(|name| unseen(name))
                .filter_map(|name| table.routines.get(name).map(|def| (*name, def.clone())))
                .collect(),
            None => table
                .routines
                .iter()
                .filter(|(key, _)| unseen(key))
                .map(|(key, def)| (*key, def.clone()))
                .collect(),
        }
    }

    /// This thread's snapshot of the `our` variable store.
    // Cost: O(1), see `WorldCell::read`.
    #[inline]
    pub(crate) fn our_table(&self) -> std::cell::Ref<'_, OurVarTable> {
        self.lexicals.our_vars.read()
    }

    /// Store `value` under `key` exactly as spelled and publish it to every
    /// thread. A re-store of the cell already stored there is not a write.
    // Cost: O(|key|) plus the table write (`OurVarTable::insert`).
    pub(crate) fn our_table_insert(&self, key: String, value: Value) {
        if let ValueView::ContainerRef(new_cell) = value.view()
            && let Some(ValueView::ContainerRef(old_cell)) =
                self.our_table().get(&key).map(Value::view)
            && crate::gc::Gc::ptr_eq(&old_cell, &new_cell)
        {
            return;
        }
        // A routine newly bound into the setting's stash changes what a call
        // of its name reaches: retire the call sites that learned the builtin
        // route (#18779).
        let core_bind = key
            .strip_prefix("&CORE::")
            .is_some_and(|bare| !self.our_table().core_routine_bound(Symbol::intern(bare)));
        self.lexicals
            .our_vars
            .write(|table| table.insert(key, value));
        if core_bind {
            self.types.registry_write_gen.store(
                crate::runtime::registry_gen::next_generation(),
                std::sync::atomic::Ordering::Relaxed,
            );
        }
    }

    /// Record that the store key `key` is a sigil-less term (`true`) or a `$`
    /// scalar (`false`). See [`OurVarTable`]'s `term_keys` / `scalar_keys`.
    // Cost: O(1) when already recorded so; otherwise one table write.
    pub(crate) fn our_table_mark_term(&self, key: Symbol, term: bool) {
        {
            let table = self.our_table();
            let (is, is_other) = if term {
                (table.is_term_key(key), table.is_scalar_key(key))
            } else {
                (table.is_scalar_key(key), table.is_term_key(key))
            };
            if is && !is_other {
                return;
            }
        }
        self.lexicals
            .our_vars
            .write(|table| table.mark_term_or_scalar(key, term));
    }

    /// Record a package-qualified store key written by a `$` assignment
    /// (`SetGlobal` of `Pkg::name`) as a scalar, unless it is already a term:
    /// a sigil-less qualified env name is a `$` variable (a term's key carries
    /// the `\\` prefix, ADR-12441), but the stash cannot tell an
    /// uppercase-leading one from a type without this mark (#18127).
    // Cost: O(|name|) for the qualified test and the symbol interning; O(1)
    // table probes when already recorded.
    pub(crate) fn note_qualified_scalar_store(&self, name: &str) {
        if name.starts_with(['@', '%', '&', '*', '\\', '$'])
            || !crate::qualified::is_qualified_str(name)
        {
            return;
        }
        let sym = Symbol::intern(name);
        if self.our_table().is_term_key(sym) {
            return;
        }
        self.our_table_mark_term(sym, false);
    }

    /// A mutable root for the stored `our` variable `key` (as spelled), for
    /// the in-place mutation chokepoint (`env_root_descended_mut`). The stored
    /// value is pinned in this thread's pin slot (`LexicalState::our_var_pin`)
    /// and the reference points at the pin: a container is shared by the
    /// table and the pin, so the mutation lands on the node every thread
    /// reads, and the pin keeps it alive while the reference is held. A
    /// non-container value is boxed into a cell in the table first, so a store
    /// through the root reaches the table too.
    // Cost: O(|key|); one table write for a value not yet in a container.
    pub(crate) fn our_var_pinned_root(&mut self, key: &str) -> Option<&mut Value> {
        let stored = self.our_table().get(key).cloned()?;
        let stored = if stored.is_container_ref() || is_shared_container(&stored) {
            stored
        } else {
            self.lexicals.our_vars.write(|table| {
                let slot = table.vars.get_mut(key)?;
                if !slot.is_container_ref() && !is_shared_container(slot) {
                    *slot = slot.clone().into_container_ref();
                }
                Some(slot.clone())
            })?
        };
        self.lexicals.our_var_pin = stored;
        Some(&mut self.lexicals.our_var_pin)
    }

    /// Store `replacement(old)` under `key` (as spelled) when the key exists,
    /// as one table write; `None` from `replacement` leaves the entry.
    // Cost: O(|key|) plus `replacement` plus the table write.
    pub(crate) fn our_table_update(
        &self,
        key: &str,
        replacement: impl FnOnce(&Value) -> Option<Value>,
    ) -> bool {
        let new_value = {
            let table = self.our_table();
            let Some(old) = table.get(key) else {
                return false;
            };
            replacement(old)
        };
        if let Some(new_value) = new_value {
            self.lexicals.our_vars.write(|table| {
                if let Some(slot) = table.vars.get_mut(key) {
                    *slot = new_value;
                }
            });
        }
        true
    }
}

/// Whether `value` is a container node whose in-place mutation is seen by
/// every holder of the value (an `Array` or `Hash`).
// Cost: O(1).
fn is_shared_container(value: &Value) -> bool {
    matches!(value.view(), ValueView::Array(..) | ValueView::Hash(..))
}
