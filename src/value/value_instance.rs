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
        let mut map = read_attrs(self.cell()).clone();
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
            side: Arc::new(RwLock::new(super::lazy_attrs::InstanceSide {
                which: self.which_memo(),
                lazy: None,
            })),
            id: self.id,
            queue_destroy: false,
            lazy_pending: std::sync::atomic::AtomicBool::new(false),
            finalized: std::sync::atomic::AtomicBool::new(false),
        }
    }
}

impl InstanceAttrs {
    /// Build a private attribute store for a role mixin. It reuses the
    /// interior-mutability and GC tracing machinery of ordinary instances but
    /// is not a Raku object: it has no user identity or DESTROY lifecycle.
    pub(crate) fn role_storage(attributes: AttrMap) -> Self {
        Self::new(Symbol::intern("Any"), attributes, 0, false)
    }

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
            side: Arc::new(RwLock::new(Default::default())),
            id,
            queue_destroy,
            lazy_pending: std::sync::atomic::AtomicBool::new(false),
            finalized: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// [`Self::new`] for an object whose `source` attributes are computed on
    /// first access (see `lazy_attrs.rs`).
    pub(crate) fn new_lazy(
        class_name: Symbol,
        attributes: AttrMap,
        id: u64,
        source: Arc<dyn super::lazy_attrs::LazyAttrSource>,
    ) -> Self {
        let mut attrs = Self::new(class_name, attributes, id, true);
        attrs.side = Arc::new(RwLock::new(super::lazy_attrs::InstanceSide {
            which: None,
            lazy: Some(source),
        }));
        attrs.lazy_pending = std::sync::atomic::AtomicBool::new(true);
        attrs
    }

    /// The attribute cell, with any lazy attributes materialized into it. Every
    /// access to the attribute map goes through here, so the deferred
    /// attributes are indistinguishable from eagerly built ones.
    // Cost: O(1) once materialized; the first access costs the source's
    // `materialize`.
    #[inline]
    fn cell(&self) -> &AttrCell {
        if self.lazy_pending.load(std::sync::atomic::Ordering::Acquire) {
            self.materialize_lazy();
        }
        &self.attributes
    }

    /// Move the lazy source's attributes into the cell. The side lock is held
    /// across the insertion, so a concurrent reader that finds the flag still
    /// set waits here instead of reading the map half-filled.
    #[cold]
    fn materialize_lazy(&self) {
        let Ok(mut side) = self.side.write() else {
            return;
        };
        if let Some(source) = side.lazy.take() {
            let entries = source.materialize();
            let mut map = write_attrs(&self.attributes);
            for (key, value) in entries {
                map.entry(key).or_insert(value);
            }
        }
        self.lazy_pending
            .store(false, std::sync::atomic::Ordering::Release);
    }

    /// The attribute map as it stands, WITHOUT materializing lazy attributes:
    /// for the GC, which must not allocate while it traces. A pending source
    /// holds no `Value`, so the map is the object's whole edge set.
    pub(crate) fn as_map_raw(&self) -> AttrReadGuard<'_> {
        read_attrs(&self.attributes)
    }

    /// Build an `InstanceAttrs` that shares an existing cell (used by the cell
    /// reuse path in `make_instance_with_id`).
    fn from_cell(
        class_name: Symbol,
        cell: AttrCell,
        side: Arc<RwLock<super::lazy_attrs::InstanceSide>>,
        id: u64,
        queue_destroy: bool,
    ) -> Self {
        if queue_destroy && let Ok(mut counts) = live_instance_refcounts(id).lock() {
            *counts.entry(id).or_insert(0) += 1;
        }
        Self {
            class_name: std::sync::atomic::AtomicU32::new(class_name.raw()),
            attributes: cell,
            side,
            id,
            queue_destroy,
            // An alias built after its source materialized has nothing to do.
            lazy_pending: std::sync::atomic::AtomicBool::new(false),
            finalized: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// The cached user-`WHICH` identity of this object, if the interpreter has
    /// deposited one (see the `which_memo` field doc).
    pub(crate) fn which_memo(&self) -> Option<Arc<str>> {
        self.side.read().ok().and_then(|m| m.which.clone())
    }

    /// Deposit (or refresh) this object's user-`WHICH` identity. Called only by
    /// the interpreter, which is the only layer that can run the user's method.
    pub(crate) fn set_which_memo(&self, which: Arc<str>) {
        if let Ok(mut slot) = self.side.write() {
            slot.which = Some(which);
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
        read_attrs(self.cell())
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
        write_attrs(self.cell()).insert(key, value)
    }

    /// Store (`Some`) or remove (`None`) several keys under one write lock.
    /// When this thread holds a read guard on the cell the write is queued, as
    /// [`Self::commit_attrs`] queues one, rather than self-deadlocking.
    // Cost: O(k), k = keys written.
    pub(crate) fn write_keys(&self, ops: Vec<(Symbol, Option<Value>)>) {
        write_cell_respecting_reads(self.cell(), PendingWrite::Delta(ops));
    }

    /// Drop every attribute (breaking any `Gc` edge out of this object) — the
    /// GC collector's cycle-sever for a proven-garbage `Instance` node (§11
    /// step 8/9). The attribute cell is interior-mutable, so this is a plain
    /// (Stacked-Borrows-sound) write.
    pub(crate) fn clear_gc_edges(&self) {
        write_attrs(&self.attributes).clear();
        // A pending source holds no edges; dropping it keeps a severed object
        // from growing attributes back.
        if let Ok(mut side) = self.side.write() {
            side.lazy = None;
        }
        self.lazy_pending
            .store(false, std::sync::atomic::Ordering::Release);
    }

    /// Mutate one attribute in place under the write lock, returning the
    /// closure's result. Returns `None` if the key is absent. Replaces the old
    /// `get_mut` (which cannot hand out a `&mut` past the guard).
    pub(crate) fn with_attr_mut<K: AttrKey, R>(
        &self,
        key: K,
        f: impl FnOnce(&mut Value) -> R,
    ) -> Option<R> {
        let mut guard = write_attrs(self.cell());
        guard.get_mut(key).map(f)
    }

    /// Insert `value` only if `key` is absent (the `entry(..).or_insert(..)`
    /// idiom), in place under the write lock.
    pub(crate) fn insert_if_absent<K: AttrKey>(&self, key: K, value: Value) {
        write_attrs(self.cell()).entry(key).or_insert(value);
    }

    /// Assign `value` at `key` the way [`AttrMap::insert_through`] does (through
    /// a `:=`-bound `ContainerRef` slot), in place under one write lock. When
    /// this thread holds a read guard on the cell, the write is queued exactly
    /// as [`Self::commit_attrs`] queues one, rather than self-deadlocking.
    ///
    /// `nqp::bindattr` used to clone the whole map, insert, and commit the clone
    /// back -- O(attributes) per bind for a one-key write (#9134).
    // Cost: O(1).
    pub(crate) fn bind_attr_through<K: AttrKey + Copy>(&self, key: K, value: Value) {
        let addr = cell_addr(self.cell());
        if HELD_READ_CELLS.with(|c| c.borrow().contains(&addr)) {
            let mut map = self.to_map();
            map.insert_through(key, value);
            self.commit_attrs(map);
            return;
        }
        write_attrs(self.cell()).insert_through(key, value);
    }

    /// Store `value` in declared slot `slot` of an instance laid out by layout
    /// `layout_id`, the way [`Self::store_through_container`] stores by key --
    /// the write half of a per-site attribute cache hit (ADR-0121 D3). Returns
    /// the slot's storage key, or hands `value` back when the instance is not
    /// (or no longer) in that shape: another layout, an undeclared attribute
    /// that could outrank the slot, or an absent slot.
    // Cost: O(1).
    pub(crate) fn store_slot_through(
        &self,
        layout_id: u32,
        slot: usize,
        value: Value,
    ) -> Result<Symbol, Value> {
        let mut guard = write_attrs(self.cell());
        let Some(key) = guard
            .layout()
            .filter(|l| l.id() == layout_id)
            .map(|l| l.key_at(slot))
        else {
            return Err(value);
        };
        if guard.has_undeclared() {
            return Err(value);
        }
        match guard.slot_mut(slot) {
            Some(held) => {
                if let ValueView::ContainerRef(cell) = held.view()
                    && !held.container_ref_is_itemized()
                {
                    *cell.lock().unwrap_or_else(|e| e.into_inner()) = value;
                } else {
                    *held = value;
                }
                Ok(key)
            }
            None => Err(value),
        }
    }

    /// Store `value` at `key`, writing *through* a promoted `ContainerRef` cell
    /// when the slot holds one instead of replacing it.
    ///
    /// An ITEMIZED cell is not such a promotion: it is a `$obj.w = @src` value
    /// share (Slice 2e, `vm/vm_attr_share.rs`) whose cell the source variable
    /// also holds, so `$!w = v` rebinds the Scalar and leaves `@src` alone.
    ///
    /// A slot promoted by [`Self::promote_attr_to_container`] is the attribute's
    /// Scalar: `$!x = v` assigns into it, so every alias handed out
    /// (a `:=`-bound name, an `is rw` method result, an `is rw` argument) keeps
    /// observing the attribute. Replacing the slot instead would silently
    /// disconnect every one of them at the first internal write.
    pub(crate) fn store_through_container<K: AttrKey + Copy>(&self, key: K, value: Value) {
        let mut guard = write_attrs(self.cell());
        match guard.get_mut(key) {
            Some(slot) => {
                if let ValueView::ContainerRef(cell) = slot.view()
                    && !slot.container_ref_is_itemized()
                {
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
        let mut guard = write_attrs(self.cell());
        match guard.get_mut(key) {
            Some(slot) => {
                // An itemized cell is a `$obj.w = @src` VALUE share (Slice 2e,
                // `vm/vm_attr_share.rs`) that the source variable holds too, not
                // the attribute's own Scalar. Give the attribute a Scalar cell
                // of its own around it, so a bound alias (`my $x := $o.w; $x =
                // 5`) rebinds the attribute instead of overwriting `@src`, while
                // reads still collapse through to the shared, itemized value.
                if slot.container_ref_is_itemized() {
                    let share = std::mem::replace(slot, Value::Nil);
                    let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(share));
                    *slot = Value::ContainerRef(cell.clone());
                    return Value::ContainerRef(cell);
                }
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
        write_cell_respecting_reads(self.cell(), PendingWrite::Replace(map));
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
        write_cell_respecting_reads(self.cell(), PendingWrite::Delta(ops));
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
        let mut guard = write_attrs(self.cell());
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
        let mut guard = write_attrs(self.cell());
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
            Arc::clone(self.cell()),
            Arc::clone(&self.side),
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
        *read_attrs(self.cell()) == *read_attrs(other.cell())
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
