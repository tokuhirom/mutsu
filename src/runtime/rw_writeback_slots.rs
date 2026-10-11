//! The rw-arg writeback slot table (`Interpreter::pending_rw_writeback_slots`).
//!
//! Each entry maps a writeback source name to the caller local slot baked at
//! arg-binding time, together with the call-frame depth that owns the slot.
//! The table is keyed by `(name, owner depth)`, not by name alone: a nested
//! call forwarding the same-named variable (`sub mk($offset is rw) {
//! Cur.new($offset) }` called as `mk($offset)`) bakes its own entry, and a
//! name-only key let it replace the outer frame's still-pending slot, which
//! the outer drain then never found (#11927).

use std::collections::HashMap;

#[derive(Default)]
pub(crate) struct RwWritebackSlots {
    by_name: HashMap<String, Vec<(u32, usize)>>,
}

impl RwWritebackSlots {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Record `slot` for `name` owned by the frame at `owner_depth`, replacing
    /// that frame's earlier entry for the name.
    // Cost: O(d), d = pending entries for this name (nested frames, small).
    pub(crate) fn insert(&mut self, name: String, slot: u32, owner_depth: usize) {
        let entries = self.by_name.entry(name).or_default();
        match entries.iter_mut().find(|(_, d)| *d == owner_depth) {
            Some(entry) => entry.0 = slot,
            None => entries.push((slot, owner_depth)),
        }
    }

    /// Like [`Self::insert`], but keeps the frame's first entry for the name.
    // Cost: O(d), d = pending entries for this name.
    pub(crate) fn insert_if_absent(&mut self, name: String, slot: u32, owner_depth: usize) {
        let entries = self.by_name.entry(name).or_default();
        if !entries.iter().any(|(_, d)| *d == owner_depth) {
            entries.push((slot, owner_depth));
        }
    }

    /// The entry for `name` owned by `depth` when there is one, else any
    /// entry another frame owns (so the caller can tell "retain for the
    /// owning frame" from "never baked").
    // Cost: O(d), d = pending entries for this name.
    pub(crate) fn get(&self, name: &str, depth: usize) -> Option<(u32, usize)> {
        let entries = self.by_name.get(name)?;
        entries
            .iter()
            .find(|(_, d)| *d == depth)
            .or_else(|| entries.first())
            .copied()
    }

    /// The names that have an entry owned by `depth`.
    // Cost: O(n), n = names with pending entries.
    pub(crate) fn names_at_depth(&self, depth: usize) -> std::collections::HashSet<String> {
        self.by_name
            .iter()
            .filter(|(_, entries)| entries.iter().any(|(_, d)| *d == depth))
            .map(|(name, _)| name.clone())
            .collect()
    }

    /// Move the entries `from` owns for the `names` to depth `to`. An
    /// interpreter-path call (`call_sub_value`) binds its arguments before any
    /// call frame is pushed, so the binder's `len - 1` owner depth names the
    /// caller's parent; the call site's drain looks the slot up under the
    /// caller's own depth.
    // Cost: O(k * d), k = names, d = pending entries per name.
    pub(crate) fn rekey_depth<'a>(
        &mut self,
        names: impl IntoIterator<Item = &'a String>,
        from: usize,
        to: usize,
    ) {
        for name in names {
            let Some(entries) = self.by_name.get_mut(name) else {
                continue;
            };
            if entries.iter().any(|(_, d)| *d == to) {
                continue;
            }
            if let Some(entry) = entries.iter_mut().find(|(_, d)| *d == from) {
                entry.1 = to;
            }
        }
    }

    /// Drop the entry for `name` owned by `depth`.
    // Cost: O(d), d = pending entries for this name.
    pub(crate) fn remove(&mut self, name: &str, depth: usize) {
        if let Some(entries) = self.by_name.get_mut(name) {
            entries.retain(|(_, d)| *d != depth);
            if entries.is_empty() {
                self.by_name.remove(name);
            }
        }
    }
}
