use super::*;

impl<'a> AttrReadGuard<'a> {
    pub(super) fn new(
        guard: std::sync::RwLockReadGuard<'a, HashMap<String, Value>>,
        addr: usize,
    ) -> Self {
        HELD_READ_CELLS.with(|c| c.borrow_mut().push(addr));
        Self {
            guard: Some(guard),
            addr,
        }
    }
}

impl std::ops::Deref for AttrReadGuard<'_> {
    type Target = HashMap<String, Value>;
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
        let flush = PENDING_CELL_WRITES.with(|p| {
            let mut v = p.borrow_mut();
            let mut mine: Vec<(AttrCell, HashMap<String, Value>)> = Vec::new();
            v.retain(|(a, cell, map)| {
                if *a == self.addr {
                    mine.push((cell.clone(), map.clone()));
                    false
                } else {
                    true
                }
            });
            mine
        });
        for (cell, map) in flush {
            *write_attrs(&cell) = map;
        }
    }
}

impl Clone for InstanceAttrs {
    /// Deep, independent copy: a fresh cell with a snapshot of the map. Used for
    /// `.clone`-style independent copies and `temp`/`let` snapshots; it must NOT
    /// share the cell — sharing flows through `Arc<InstanceAttrs>`. The copy does
    /// not participate in DESTROY refcounting (`queue_destroy = false`).
    fn clone(&self) -> Self {
        Self {
            class_name: self.class_name,
            attributes: Arc::new(RwLock::new(read_attrs(&self.attributes).clone())),
            id: self.id,
            queue_destroy: false,
        }
    }
}

impl InstanceAttrs {
    pub(crate) fn new(
        class_name: Symbol,
        attributes: HashMap<String, Value>,
        id: u64,
        queue_destroy: bool,
    ) -> Self {
        if queue_destroy && let Ok(mut counts) = live_instance_refcounts().lock() {
            *counts.entry(id).or_insert(0) += 1;
        }
        let cell: AttrCell = Arc::new(RwLock::new(attributes));
        Self {
            class_name,
            attributes: cell,
            id,
            queue_destroy,
        }
    }

    /// Build an `InstanceAttrs` that shares an existing cell (used by the cell
    /// reuse path in `make_instance_with_id`).
    fn from_cell(class_name: Symbol, cell: AttrCell, id: u64, queue_destroy: bool) -> Self {
        if queue_destroy && let Ok(mut counts) = live_instance_refcounts().lock() {
            *counts.entry(id).or_insert(0) += 1;
        }
        Self {
            class_name,
            attributes: cell,
            id,
            queue_destroy,
        }
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
    pub(crate) fn to_map(&self) -> HashMap<String, Value> {
        self.as_map().clone()
    }

    /// This instance's stable identity id.
    pub(crate) fn instance_id(&self) -> u64 {
        self.id
    }

    pub(crate) fn contains_key(&self, key: &str) -> bool {
        self.as_map().contains_key(key)
    }

    /// In-place insert through the shared cell (visible to all aliases).
    pub(crate) fn insert(&self, key: String, value: Value) -> Option<Value> {
        write_attrs(&self.attributes).insert(key, value)
    }

    /// Mutate one attribute in place under the write lock, returning the
    /// closure's result. Returns `None` if the key is absent. Replaces the old
    /// `get_mut` (which cannot hand out a `&mut` past the guard).
    pub(crate) fn with_attr_mut<R>(&self, key: &str, f: impl FnOnce(&mut Value) -> R) -> Option<R> {
        let mut guard = write_attrs(&self.attributes);
        guard.get_mut(key).map(f)
    }

    /// Insert `value` only if `key` is absent (the `entry(..).or_insert(..)`
    /// idiom), in place under the write lock.
    pub(crate) fn insert_if_absent(&self, key: String, value: Value) {
        write_attrs(&self.attributes).entry(key).or_insert(value);
    }

    /// Phase 3 registry-removal: replace the whole attribute map in place through
    /// this instance's shared cell, deadlock-safe with respect to a same-thread
    /// read guard (see [`write_cell_respecting_reads`]). Because every alias of the
    /// instance shares this `Arc<InstanceAttrs>` cell, the new map is visible
    /// everywhere — in this frame, any caller frame, a `ContainerRef`-boxed
    /// capture, a role `Mixin`, or a nested attribute of another instance. This is
    /// the in-place replacement for the legacy id→cell registry writeback
    /// (`overwrite_instance_bindings_by_identity` / `update_instance_cell`), which
    /// computed an updated `HashMap` and looked the cell up by id.
    pub(crate) fn commit_attrs(&self, map: HashMap<String, Value>) {
        write_cell_respecting_reads(&self.attributes, map);
    }

    /// Phase 3 cell-CAS: atomically compare-and-swap one attribute under a
    /// single write lock. When `matches(current)` returns true the new value is
    /// stored; returns `(current, swapped)`. The cell's write lock is the
    /// atomic primitive for cross-thread `cas`/atomic ops on instance
    /// attributes — every alias of the instance shares this cell, so the swap
    /// is immediately visible everywhere (no shared_vars side channel).
    pub(crate) fn compare_and_swap(
        &self,
        key: &str,
        matches: impl FnOnce(&Value) -> bool,
        new: Value,
    ) -> (Value, bool) {
        let mut guard = write_attrs(&self.attributes);
        let current = guard.get(key).cloned().unwrap_or(Value::Nil);
        let swapped = matches(&current);
        if swapped {
            guard.insert(key.to_string(), new);
        }
        (current, swapped)
    }

    /// Phase 3 cell-CAS: atomically read-modify-write one attribute under a
    /// single write lock (atomic add / increment / decrement). Returns
    /// `(old, new)`; an error from `f` leaves the attribute unchanged.
    #[allow(clippy::result_large_err)]
    pub(crate) fn fetch_update(
        &self,
        key: &str,
        f: impl FnOnce(&Value) -> Result<Value, RuntimeError>,
    ) -> Result<(Value, Value), RuntimeError> {
        let mut guard = write_attrs(&self.attributes);
        let current = guard.get(key).cloned().unwrap_or(Value::Nil);
        let next = f(&current)?;
        guard.insert(key.to_string(), next.clone());
        Ok((current, next))
    }

    /// Build an `InstanceAttrs` that SHARES this cell but carries a different
    /// `class_name` (rebless / role mixin). The mutation visibility comes from the
    /// shared cell; only the class tag differs.
    pub(super) fn with_class(&self, class_name: Symbol) -> Self {
        Self::from_cell(
            class_name,
            Arc::clone(&self.attributes),
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

impl Drop for InstanceAttrs {
    fn drop(&mut self) {
        if !self.queue_destroy {
            return;
        }
        // Suppress recursive DESTROY queuing when we're already inside a DESTROY handler
        if is_in_destroy_handler() {
            return;
        }
        let should_queue = if let Ok(mut counts) = live_instance_refcounts().lock() {
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
        let _ = PENDING_INSTANCE_DESTROYS.try_with(|pending| {
            pending.borrow_mut().push(PendingInstanceDestroy {
                class_name: self.class_name,
                attributes: read_attrs(&self.attributes).clone(),
            });
        });
    }
}
