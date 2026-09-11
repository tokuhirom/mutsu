use super::*;

impl<'a> AttrReadGuard<'a> {
    pub(super) fn new(guard: std::sync::RwLockReadGuard<'a, AttrMap>, addr: usize) -> Self {
        HELD_READ_CELLS.with(|c| c.borrow_mut().push(addr));
        Self {
            guard: Some(guard),
            addr,
        }
    }
}

impl std::ops::Deref for AttrReadGuard<'_> {
    type Target = AttrMap;
    fn deref(&self) -> &Self::Target {
        self.guard.as_ref().expect("attr read guard live")
    }
}

impl Drop for AttrReadGuard<'_> {
    fn drop(&mut self) {
        let still_held = HELD_READ_CELLS.with(|c| {
            let mut v = c.borrow_mut();
            if let Some(pos) = v.iter().rposition(|&a| a == self.addr) {
                v.swap_remove(pos);
            }
            v.contains(&self.addr)
        });
        // Release this read lock before flushing, so a deferred blocking write
        // does not deadlock against it.
        self.guard = None;
        if still_held {
            return;
        }
        // Deferred writes are a rare self-deadlock escape hatch; the counter
        // answers "none anywhere" without a second thread-local round trip.
        if !super::pending_cell_writes_possible() {
            return;
        }
        // Partition rather than `retain` + clone: a `PendingWrite` owns its map
        // (or its delta ops), and the queue is drained in push order.
        let flush = PENDING_CELL_WRITES.with(|p| {
            let mut v = p.borrow_mut();
            let mut mine: Vec<(AttrCell, PendingWrite)> = Vec::new();
            let mut keep: Vec<PendingCellWrite> = Vec::new();
            for (addr, cell, write) in std::mem::take(&mut *v) {
                if addr == self.addr {
                    mine.push((cell, write));
                } else {
                    keep.push((addr, cell, write));
                }
            }
            *v = keep;
            mine
        });
        super::note_pending_cell_writes_drained(flush.len());
        for (cell, write) in flush {
            write.apply(&mut write_attrs(&cell));
        }
    }
}

impl Clone for InstanceAttrs {
    /// Deep, independent copy: a fresh cell with a snapshot of the map. Used for
    /// `.clone`-style independent copies and `temp`/`let` snapshots; it must NOT
    /// share the cell — sharing flows through `crate::gc::Gc<InstanceAttrs>`. The copy does
    /// not participate in DESTROY refcounting (`queue_destroy = false`).
    fn clone(&self) -> Self {
        let mut map = read_attrs(&self.attributes).clone();
        // Snapshot any `ContainerRef`-promoted slot (a `:=`-bound attribute):
        // an independent copy must not alias the original's attribute cell.
        // Likewise detach Array/Hash attribute values: element mutations write
        // through the shared backing node (container identity §3), so a copy
        // sharing the `Gc` would observe the original's `@!attr.push` — an
        // independent copy must own distinct containers (rakudo `.clone`
        // clones each `has` container too).
        for v in map.values_mut() {
            if let ValueView::ContainerRef(cell) = v.view() {
                let inner = cell.lock().unwrap().clone();
                *v = inner;
            }
            *v = std::mem::replace(v, Value::NIL).detach_shared_container();
        }
        Self {
            class_name: std::sync::atomic::AtomicU32::new(self.class_name().raw()),
            attributes: Arc::new(RwLock::new(map)),
            // An independent copy carries the same attribute values, so its
            // user-`WHICH` identity is the same string — but in its own cell,
            // since the copy is a separate object whose later mutations must
            // not retag the original.
            which_memo: Arc::new(RwLock::new(self.which_memo())),
            id: self.id,
            queue_destroy: false,
            finalized: std::sync::atomic::AtomicBool::new(false),
        }
    }
}

impl InstanceAttrs {
    pub(crate) fn new(
        class_name: Symbol,
        attributes: AttrMap,
        id: u64,
        queue_destroy: bool,
    ) -> Self {
        if queue_destroy && let Ok(mut counts) = live_instance_refcounts(id).lock() {
            *counts.entry(id).or_insert(0) += 1;
        }
        let cell: AttrCell = Arc::new(RwLock::new(attributes));
        Self {
            class_name: std::sync::atomic::AtomicU32::new(class_name.raw()),
            attributes: cell,
            which_memo: Arc::new(RwLock::new(None)),
            id,
            queue_destroy,
            finalized: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// Build an `InstanceAttrs` that shares an existing cell (used by the cell
    /// reuse path in `make_instance_with_id`).
    fn from_cell(
        class_name: Symbol,
        cell: AttrCell,
        which_memo: Arc<RwLock<Option<Arc<str>>>>,
        id: u64,
        queue_destroy: bool,
    ) -> Self {
        if queue_destroy && let Ok(mut counts) = live_instance_refcounts(id).lock() {
            *counts.entry(id).or_insert(0) += 1;
        }
        Self {
            class_name: std::sync::atomic::AtomicU32::new(class_name.raw()),
            attributes: cell,
            which_memo,
            id,
            queue_destroy,
            finalized: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// The cached user-`WHICH` identity of this object, if the interpreter has
    /// deposited one (see the `which_memo` field doc).
    pub(crate) fn which_memo(&self) -> Option<Arc<str>> {
        self.which_memo.read().ok().and_then(|m| m.clone())
    }

    /// Deposit (or refresh) this object's user-`WHICH` identity. Called only by
    /// the interpreter, which is the only layer that can run the user's method.
    pub(crate) fn set_which_memo(&self, which: Arc<str>) {
        if let Ok(mut slot) = self.which_memo.write() {
            *slot = Some(which);
        }
    }

    /// The object's current type. Reads the interior-mutable slot that
    /// [`InstanceAttrs::rebless`] writes.
    pub(crate) fn class_name(&self) -> Symbol {
        Symbol::from_raw(self.class_name.load(std::sync::atomic::Ordering::Relaxed))
    }

    /// Retag this object's type in place. Every alias shares this node, so the
    /// new type is immediately visible everywhere — that is what makes Raku's
    /// `does` a mutation of the object rather than of one variable.
    pub(crate) fn rebless(&self, class_name: Symbol) {
        self.class_name
            .store(class_name.raw(), std::sync::atomic::Ordering::Relaxed);
    }

    // --- Attribute access API (Phase 3 — encapsulation boundary) ---
    //
    // The storage is a shared mutable cell (`Arc<RwLock<HashMap>>`). A locked
    // cell cannot `Deref` to `&HashMap` (guard lifetime), so all access goes
    // through these inherent methods. Reads take a read lock; mutations take a
    // write lock and happen in place (visible to every alias).

    /// Take a read lock over the attribute map. The guard derefs to `&HashMap`.
    pub(crate) fn as_map(&self) -> AttrReadGuard<'_> {
        read_attrs(&self.attributes)
    }

    /// An owned clone of the backing map.
    pub(crate) fn to_map(&self) -> AttrMap {
        self.as_map().clone()
    }

    /// This instance's stable identity id.
    pub(crate) fn instance_id(&self) -> u64 {
        self.id
    }

    pub(crate) fn contains_key<K: AttrKey>(&self, key: K) -> bool {
        self.as_map().contains_key(key)
    }

    /// In-place insert through the shared cell (visible to all aliases).
    pub(crate) fn insert<K: AttrKey>(&self, key: K, value: Value) -> Option<Value> {
        write_attrs(&self.attributes).insert(key, value)
    }

    /// Drop every attribute (breaking any `Gc` edge out of this object) — the
    /// GC collector's cycle-sever for a proven-garbage `Instance` node (§11
    /// step 8/9). The attribute cell is interior-mutable, so this is a plain
    /// (Stacked-Borrows-sound) write.
    pub(crate) fn clear_gc_edges(&self) {
        write_attrs(&self.attributes).clear();
    }

    /// Mutate one attribute in place under the write lock, returning the
    /// closure's result. Returns `None` if the key is absent. Replaces the old
    /// `get_mut` (which cannot hand out a `&mut` past the guard).
    pub(crate) fn with_attr_mut<K: AttrKey, R>(
        &self,
        key: K,
        f: impl FnOnce(&mut Value) -> R,
    ) -> Option<R> {
        let mut guard = write_attrs(&self.attributes);
        guard.get_mut(key).map(f)
    }

    /// Insert `value` only if `key` is absent (the `entry(..).or_insert(..)`
    /// idiom), in place under the write lock.
    pub(crate) fn insert_if_absent<K: AttrKey>(&self, key: K, value: Value) {
        write_attrs(&self.attributes).entry(key).or_insert(value);
    }

    /// Store `value` at `key`, writing *through* a promoted `ContainerRef` cell
    /// when the slot holds one instead of replacing it.
    ///
    /// A slot promoted by [`Self::promote_attr_to_container`] is the attribute's
    /// Scalar: `$!x = v` assigns into it, so every alias handed out
    /// (a `:=`-bound name, an `is rw` method result, an `is rw` argument) keeps
    /// observing the attribute. Replacing the slot instead would silently
    /// disconnect every one of them at the first internal write.
    pub(crate) fn store_through_container<K: AttrKey + Copy>(&self, key: K, value: Value) {
        let mut guard = write_attrs(&self.attributes);
        match guard.get_mut(key) {
            Some(slot) => {
                if let ValueView::ContainerRef(cell) = slot.view() {
                    *cell.lock().unwrap() = value;
                } else {
                    *slot = value;
                }
            }
            None => {
                guard.insert(key, value);
            }
        }
    }

    /// Promote the attribute at `key` to a shared `ContainerRef` cell (in place,
    /// under a single write lock) and return the cell value. If the slot already
    /// holds a `ContainerRef`, the existing cell is returned — repeated `:=`
    /// binds / `.VAR` chains on the same attribute alias one container. An
    /// absent key materializes as a fresh cell holding `Nil`. This gives an
    /// accessor result container identity: writes through the accessor and reads
    /// through the bound alias observe the same slot.
    pub(crate) fn promote_attr_to_container<K: AttrKey + Copy>(&self, key: K) -> Value {
        let mut guard = write_attrs(&self.attributes);
        match guard.get_mut(key) {
            Some(slot) => {
                if let ValueView::ContainerRef(cell) = slot.view() {
                    return Value::ContainerRef(cell.clone());
                }
                let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(std::mem::replace(
                    slot,
                    Value::Nil,
                )));
                *slot = Value::ContainerRef(cell.clone());
                Value::ContainerRef(cell)
            }
            None => {
                let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(Value::Nil));
                guard.insert(key, Value::ContainerRef(cell.clone()));
                Value::ContainerRef(cell)
            }
        }
    }

    /// Phase 3 registry-removal: replace the whole attribute map in place through
    /// this instance's shared cell, deadlock-safe with respect to a same-thread
    /// read guard (see [`write_cell_respecting_reads`]). Because every alias of the
    /// instance shares this `crate::gc::Gc<InstanceAttrs>` cell, the new map is visible
    /// everywhere — in this frame, any caller frame, a `ContainerRef`-boxed
    /// capture, a role `Mixin`, or a nested attribute of another instance. This is
    /// the in-place replacement for the legacy id→cell registry writeback
    /// (`overwrite_instance_bindings_by_identity` / `update_instance_cell`), which
    /// computed an updated `HashMap` and looked the cell up by id.
    pub(crate) fn commit_attrs(&self, map: AttrMap) {
        write_cell_respecting_reads(&self.attributes, PendingWrite::Replace(map));
    }

    /// Commit only what actually changed, instead of replacing the whole map.
    ///
    /// `before` is the [`AttrMap::bits_image`] of the map as it was read, and
    /// `updated` is the map the caller produced from it. Keys whose boxed word is
    /// unchanged are left alone; keys that were added or rewritten are stored;
    /// keys that were in `before` and are gone from `updated` are removed. The
    /// whole delta goes in under **one** write lock.
    ///
    /// This is what makes a read-modify-write over the attribute map safe to run
    /// concurrently on one instance. [`Self::commit_attrs`] replaces the map, so
    /// the snapshot-dispatch-commit shape every mutable *native* method goes
    /// through (`Interpreter::call_native_instance_method_mut_in_place`) silently threw
    /// away any key another thread committed in between: two threads on one
    /// `Proc::Async` would lose `.start`'s `started`/`pid` to a concurrent
    /// `.ready`, and `.kill` would then throw `X::Proc::Async::MustBeStarted`
    /// (tokuhirom/mutsu#7923). A key *neither* side touched is not in the delta at
    /// all, so the other thread's write survives.
    ///
    /// Tie-break when both threads rewrote the same key: last commit wins. There
    /// is no happens-before between them to prefer, and it matches what a single
    /// write lock per key would have given.
    pub(crate) fn commit_attrs_delta(&self, before: &AttrBits, updated: &AttrMap) {
        let mut ops: Vec<(Symbol, Option<Value>)> = Vec::new();
        // How many of `updated`'s keys were also in `before`. Removals exist iff
        // that overlap is smaller than `before` — so the usual case (a method that
        // only inserts or rewrites) never scans `before` at all.
        let mut overlap = 0usize;
        for (key, value) in updated.iter() {
            match before.bits(*key) {
                Some(bits) => {
                    overlap += 1;
                    if bits != value.nanbox_bits() {
                        ops.push((*key, Some(value.clone())));
                    }
                }
                None => ops.push((*key, Some(value.clone()))),
            }
        }
        if overlap < before.len() {
            for key in before.keys() {
                if !updated.contains_key(*key) {
                    ops.push((*key, None));
                }
            }
        }
        if ops.is_empty() {
            return;
        }
        write_cell_respecting_reads(&self.attributes, PendingWrite::Delta(ops));
    }

    /// Phase 3 cell-CAS: atomically compare-and-swap one attribute under a
    /// single write lock. When `matches(current)` returns true the new value is
    /// stored; returns `(current, swapped)`. The cell's write lock is the
    /// atomic primitive for cross-thread `cas`/atomic ops on instance
    /// attributes — every alias of the instance shares this cell, so the swap
    /// is immediately visible everywhere (no shared_vars side channel).
    pub(crate) fn compare_and_swap<K: AttrKey + Copy>(
        &self,
        key: K,
        matches: impl FnOnce(&Value) -> bool,
        new: Value,
    ) -> (Value, bool) {
        let mut guard = write_attrs(&self.attributes);
        let current = guard.get(key).cloned().unwrap_or(Value::Nil);
        let swapped = matches(&current);
        if swapped {
            guard.insert(key, new);
        }
        (current, swapped)
    }

    /// Phase 3 cell-CAS: atomically read-modify-write one attribute under a
    /// single write lock (atomic add / increment / decrement). Returns
    /// `(old, new)`; an error from `f` leaves the attribute unchanged.
    pub(crate) fn fetch_update<K: AttrKey + Copy>(
        &self,
        key: K,
        f: impl FnOnce(&Value) -> Result<Value, RuntimeError>,
    ) -> Result<(Value, Value), RuntimeError> {
        let mut guard = write_attrs(&self.attributes);
        let current = guard.get(key).cloned().unwrap_or(Value::Nil);
        let next = f(&current)?;
        guard.insert(key, next.clone());
        Ok((current, next))
    }

    /// Build an `InstanceAttrs` that SHARES this cell but carries a different
    /// `class_name` (rebless / role mixin). The mutation visibility comes from the
    /// shared cell; only the class tag differs.
    pub(super) fn with_class(&self, class_name: Symbol) -> Self {
        Self::from_cell(
            class_name,
            Arc::clone(&self.attributes),
            Arc::clone(&self.which_memo),
            self.id,
            self.queue_destroy,
        )
    }
}

impl PartialEq for InstanceAttrs {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.attributes, &other.attributes) {
            return true;
        }
        *read_attrs(&self.attributes) == *read_attrs(&other.attributes)
    }
}

impl InstanceAttrs {
    /// Queue this instance's Raku `DESTROY` (once). Shared by Rust `Drop`
    /// (GC-off: fires at refcount death; GC-on: fires at the node's eventual
    /// memory drop) and `Trace::finalize` (GC-on: fires at last-live-handle
    /// drop or at cycle reclaim, while the attributes are still intact) —
    /// whichever runs first wins via the `finalized` once-flag, which also
    /// guards the `live_instance_refcounts` bookkeeping from double-decrement.
    pub(crate) fn finalize_destroy(&self) {
        if !self.queue_destroy {
            return;
        }
        if self
            .finalized
            .swap(true, std::sync::atomic::Ordering::SeqCst)
        {
            return;
        }
        // Suppress recursive DESTROY queuing when we're already inside a DESTROY handler
        if is_in_destroy_handler() {
            return;
        }
        let should_queue = if let Ok(mut counts) = live_instance_refcounts(self.id).lock() {
            match counts.get_mut(&self.id) {
                Some(count) if *count > 1 => {
                    *count -= 1;
                    false
                }
                Some(_) => {
                    counts.remove(&self.id);
                    true
                }
                None => true,
            }
        } else {
            true
        };
        if !should_queue {
            return;
        }
        // Nothing in this program declares a user `DESTROY`, so the queued item
        // could only be walked and thrown away. Checked AFTER the refcount
        // bookkeeping above (skipping that would leak `live_instance_refcounts`
        // entries) and at DROP time, so a `DESTROY` registered later still
        // fires for every instance that dies after it.
        if !super::any_destroy_method_declared() {
            return;
        }
        let _ = PENDING_INSTANCE_DESTROYS.try_with(|pending| {
            pending.borrow_mut().push(PendingInstanceDestroy {
                class_name: self.class_name(),
                attributes: read_attrs(&self.attributes).clone(),
            });
        });
    }
}

impl Drop for InstanceAttrs {
    fn drop(&mut self) {
        self.finalize_destroy();
    }
}

#[cfg(test)]
mod tests;
