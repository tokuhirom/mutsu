use super::*;

impl Value {
    /// Create a decontainerized Proxy value (result of .VAR on a Proxy).
    /// This Proxy won't be auto-FETCHed by method dispatch.
    pub(crate) fn proxy_var_object(proxy: Value, _target_var: String) -> Self {
        match proxy {
            Value::Proxy {
                fetcher,
                storer,
                subclass,
                ..
            } => Value::Proxy {
                fetcher,
                storer,
                subclass,
                decontainerized: true,
            },
            other => other,
        }
    }

    // ---- Arc-wrapping convenience constructors ----
    pub fn bigint(n: NumBigInt) -> Self {
        Value::BigInt(Arc::new(n))
    }
    pub fn str(s: String) -> Self {
        Value::Str(Arc::new(s))
    }
    pub fn str_from(s: &str) -> Self {
        Value::Str(Arc::new(s.to_string()))
    }
    pub fn regex(s: String) -> Self {
        Value::Regex(Arc::new(s))
    }
    pub fn mixin(inner: Value, overrides: HashMap<String, Value>) -> Self {
        Value::Mixin(Arc::new(inner), Arc::new(overrides))
    }
    pub fn generic_range(start: Value, end: Value, excl_start: bool, excl_end: bool) -> Self {
        Value::GenericRange {
            start: Arc::new(start),
            end: Arc::new(end),
            excl_start,
            excl_end,
        }
    }
    pub fn array(items: Vec<Value>) -> Self {
        Value::Array(crate::gc::Gc::new(ArrayData::new(items)), ArrayKind::List)
    }
    /// Create a Capture value. Boxes the positional/named payloads (the variant
    /// stores them behind `Box` to keep `Value` small).
    pub fn capture(positional: Vec<Value>, named: HashMap<String, Value>) -> Self {
        Value::Capture {
            positional: Box::new(positional),
            named: Box::new(named),
        }
    }
    /// Create a BigRat value. The numerator/denominator are boxed (the variant
    /// stores them behind `Box` to keep `Value` small).
    pub fn bigrat(num: NumBigInt, den: NumBigInt) -> Self {
        Value::BigRat(Box::new(num), Box::new(den))
    }
    /// The HOW (meta-object) of a `CustomType` or `CustomTypeInstance`, if this
    /// is one. Centralizes access now that both are boxed payloads.
    pub fn custom_how(&self) -> Option<&Value> {
        match self {
            Value::CustomType(d) => Some(&d.how),
            Value::CustomTypeInstance(d) => Some(&d.how),
            _ => None,
        }
    }
    /// The REPR name of a `CustomType` or `CustomTypeInstance`, if this is one.
    pub fn custom_repr(&self) -> Option<&str> {
        match self {
            Value::CustomType(d) => Some(&d.repr),
            Value::CustomTypeInstance(d) => Some(&d.repr),
            _ => None,
        }
    }
    /// Create a CustomType value (boxed payload).
    pub fn custom_type(how: Box<Value>, repr: String, name: Symbol, id: u64) -> Self {
        Value::CustomType(Box::new(CustomTypeData {
            how,
            repr,
            name,
            id,
        }))
    }
    /// Create a Uni value (boxed payload).
    pub fn uni(form: String, text: String) -> Self {
        Value::Uni(Box::new(UniData { form, text }))
    }
    /// Create a CustomTypeInstance value (boxed payload).
    pub fn custom_type_instance(
        type_id: u64,
        how: Box<Value>,
        repr: String,
        type_name: Symbol,
        attributes: Arc<HashMap<String, Value>>,
        id: u64,
    ) -> Self {
        Value::CustomTypeInstance(Box::new(CustomTypeInstanceData {
            type_id,
            how,
            repr,
            type_name,
            attributes,
            id,
        }))
    }
    /// Create a true Array value (from [...] literals).
    pub fn real_array(items: Vec<Value>) -> Self {
        Value::Array(crate::gc::Gc::new(ArrayData::new(items)), ArrayKind::Array)
    }
    /// Create a true Array value with a single explicitly-assigned index
    /// recorded in the embedded `initialized` set (used when autovivifying a
    /// missing variable via `@a[i] = …`, so the autovivification gaps below `i`
    /// are recognized as holes by `:exists`/`:k`/`:p`).
    pub fn real_array_initialized_at(items: Vec<Value>, idx: usize) -> Self {
        let mut data = ArrayData::new(items);
        let mut set = std::collections::HashSet::new();
        set.insert(idx);
        data.initialized = Some(set);
        Value::Array(crate::gc::Gc::new(data), ArrayKind::Array)
    }
    /// Create a shaped (multidimensional) Array value.
    pub fn shaped_array(items: Vec<Value>) -> Self {
        Value::Array(crate::gc::Gc::new(ArrayData::new(items)), ArrayKind::Shaped)
    }
    /// Build an `crate::gc::Gc<ArrayData>` from a plain element vector.
    pub(crate) fn array_arc(items: Vec<Value>) -> crate::gc::Gc<ArrayData> {
        crate::gc::Gc::new(ArrayData::new(items))
    }
    /// Rebuild an array's backing data with new elements, preserving the
    /// embedded container type metadata of `like` (used by mutators that
    /// reconstruct the vector, so a typed `Array[Int]` stays typed).
    pub(crate) fn array_data_like(like: &ArrayData, items: Vec<Value>) -> crate::gc::Gc<ArrayData> {
        crate::gc::Gc::new(ArrayData {
            items,
            value_type: like.value_type.clone(),
            key_type: like.key_type.clone(),
            declared_type: like.declared_type.clone(),
            default: like.default.clone(),
            shape: like.shape.clone(),
            initialized: like.initialized.clone(),
        })
    }
    /// Construct a `Value::Hash`. Accepts either a bare `HashMap` (fresh hash)
    /// or a `HashData` (a cloned/rebuilt hash whose container metadata is then
    /// preserved) via `Into<HashData>`.
    pub fn hash(map: impl Into<HashData>) -> Self {
        Value::Hash(Gc::new(map.into()))
    }

    /// Build a `Gc<HashData>` from a map or `HashData`. Lets call sites that
    /// constructed `Value::Hash(crate::gc::Gc::new(x))` keep their shape as
    /// `Value::Hash(Value::hash_arc(x))` while the variant moved to `HashData`.
    pub(crate) fn hash_arc(map: impl Into<HashData>) -> Gc<HashData> {
        Gc::new(map.into())
    }

    /// Coerce a value into item context (`.item` method).
    /// Arrays get their kind itemized, hashes get their itemization flag set
    /// (mirroring `ArrayKind` — the value stays a `Value::Hash` so it never
    /// leaks a wrapper to value operations), other values get wrapped in Scalar.
    pub fn item(self) -> Self {
        match self {
            Value::Array(items, kind) => Value::Array(items, kind.itemize()),
            Value::Hash(h) => Value::Hash(Self::hash_arc_itemized(h)),
            other => other,
        }
    }

    /// Return a `Value::Hash` Arc with its itemization flag set (copy-on-write
    /// only when the flag actually changes, so `.WHICH` identity is preserved
    /// for an already-itemized hash). Used by the `Itemize`/`ItemizeVar` opcodes
    /// and `.item` to mark a `$`-sourced hash as a single list-context element.
    pub(crate) fn hash_arc_itemized(h: Gc<HashData>) -> Gc<HashData> {
        if h.itemized {
            h
        } else {
            let mut data = (*h).clone();
            data.itemized = true;
            Gc::new(data)
        }
    }

    pub(crate) fn hash_arc_deitemized(h: Gc<HashData>) -> Gc<HashData> {
        if h.itemized {
            let mut data = (*h).clone();
            data.itemized = false;
            Gc::new(data)
        } else {
            h
        }
    }

    /// Read through a `ContainerRef` and apply `f` to the inner value WITHOUT
    /// cloning it. Non-ContainerRef values are passed to `f` as-is. This is the
    /// canonical non-cloning ContainerRef-read chokepoint (the ContainerRef axis of
    /// the decont family); prefer it over hand-rolled `arc.lock().unwrap()` reads.
    pub fn with_deref<R>(&self, f: impl FnOnce(&Value) -> R) -> R {
        match self {
            Value::ContainerRef(arc) => f(&arc.lock().unwrap()),
            other => f(other),
        }
    }

    /// Read through a `ContainerRef`, returning an owned clone of the inner value.
    /// Non-ContainerRef values are cloned as-is. Use [`Value::with_deref`] instead
    /// when you only need to read the inner value (it avoids the clone).
    pub fn deref_container(&self) -> Value {
        self.with_deref(Value::clone)
    }

    /// Owned counterpart of [`Value::deref_container`]: read through a
    /// `ContainerRef` BY VALUE, cloning ONLY the inner value when `self` is a
    /// ContainerRef; non-ContainerRef values move through with no clone. This is
    /// the canonical move-friendly read chokepoint for hot read opcodes
    /// (`GetLocal`/`GetGlobal`), mirroring how [`Value::into_descalarized`] is the
    /// owned variant of [`Value::descalarize`]. Single-level only (a nested
    /// `ContainerRef` is unwrapped one cell), and it does NOT force an inner
    /// `LazyThunk` nor strip an inner `Scalar` — matching the prior hand-rolled
    /// `arc.lock().unwrap().clone()` reads it replaces.
    pub fn into_deref(self) -> Value {
        match self {
            Value::ContainerRef(arc) => arc.lock().unwrap().clone(),
            other => other,
        }
    }

    /// Store `val` through a `ContainerRef` cell. If the cell currently holds a
    /// `HashEntryRef` deferred token (a boxed `\target` bound to a
    /// not-yet-existent hash key — e.g. the escape analysis boxed a captured
    /// sigilless param before its first write), first materialize the binding:
    /// this cell ITSELF is installed at the token's path (walk-creating any
    /// intermediate hashes), so the hash entry and every holder of the cell
    /// alias the same container from then on. A plain `clone_from` would
    /// overwrite the token and silently drop the hash alias.
    pub(crate) fn store_through_cell(arc: &crate::gc::Gc<Mutex<Value>>, val: &Value) {
        let mut inner = arc.lock().unwrap();
        if matches!(&*inner, Value::HashEntryRef { .. })
            && let Some((hash_arc, key)) = inner.hash_entry_terminal()
        {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `arc_contents_mut`. No borrow into the map is live across the write.
            let hd = unsafe { crate::value::gc_contents_mut(&hash_arc) };
            Value::hash_insert_through(&mut hd.map, key, Value::ContainerRef(arc.clone()));
        }
        inner.clone_from(val);
    }

    /// Assign a value into a `ContainerRef`.
    /// Returns `true` if the value was a ContainerRef and the assignment happened.
    pub fn assign_into_container(&self, new_val: Value) -> bool {
        if let Value::ContainerRef(arc) = self {
            let mut inner = arc.lock().unwrap();
            *inner = new_val;
            true
        } else {
            false
        }
    }

    /// Create a new shared container holding this value.
    pub fn into_container_ref(self) -> Value {
        Value::ContainerRef(crate::gc::Gc::new(Mutex::new(self)))
    }

    /// Assign `val` into an array/hash element `slot`. When the slot already
    /// holds a `ContainerRef` cell (a Phase 2 `:=`-bound element), write
    /// *through* the cell so every alias of that element observes the new
    /// value; otherwise replace the slot in place. This is the single element
    /// write chokepoint that keeps a bound element's alias live across writes.
    pub fn assign_element_slot(slot: &mut Value, val: Value) {
        if let Value::ContainerRef(cell) = slot {
            *cell.lock().unwrap() = val;
        } else {
            *slot = val;
        }
    }

    /// Hash element write chokepoint (Phase 2 Stage 0). The hash analogue of
    /// [`assign_element_slot`]: if the existing entry at `key` is a
    /// `ContainerRef` cell, write *through* it (preserving any `:=` binding);
    /// otherwise insert or replace the entry as a bare value.
    ///
    /// This is behavior-invariant until hash elements are promoted to cells
    /// (Phase 2 Stage 1), because no hash currently stores `ContainerRef`
    /// entries, so every call collapses to a plain insert/replace. Routing all
    /// hash-element writes through this single chokepoint is the prerequisite
    /// for that promotion: a naive promotion without it broke array-through-hash
    /// traversal (nested.t 30->7), see `docs/container-identity.md`.
    pub fn hash_insert_through(map: &mut HashMap<String, Value>, key: String, val: Value) {
        match map.get_mut(&key) {
            Some(slot) => Value::assign_element_slot(slot, val),
            None => {
                map.insert(key, val);
            }
        }
    }

    pub fn is_container_ref(&self) -> bool {
        matches!(self, Value::ContainerRef(_))
    }

    /// Autovivify a hash entry: if the key doesn't exist, insert an empty Hash.
    /// Returns a `HashEntryRef` pointing to the entry in the parent hash.
    /// Uses interior mutation of the `Arc<HashMap>` so that **all** clones of
    /// the same `Arc` observe the change.  This relies on no `&HashMap` borrow
    /// being live across the call (the aliased-mutation contract in
    /// `aliased_mut.rs`); cross-thread sharing of the same `Arc` is the tracked,
    /// pre-existing gap documented there.
    ///
    /// Look up a key in a Hash value by string key.
    pub fn hash_get_str(&self, key: &str) -> Option<Value> {
        match self {
            Value::Hash(arc) => arc.get(key).cloned(),
            Value::Mixin(inner, _) => inner.hash_get_str(key),
            Value::Scalar(inner) => inner.hash_get_str(key),
            _ => None,
        }
    }

    /// Returns `None` if `self` is not a `Value::Hash`.
    pub fn hash_autovivify(&self, key: &str) -> Option<Value> {
        if let Value::Hash(arc) = self {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `arc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(arc) };
            if !data.map.contains_key(key) {
                let new_hash = Value::hash(HashMap::new());
                data.map.insert(key.to_string(), new_hash);
            }
            // The entry exists (created just above if missing): an EAGER token,
            // whose reads see through to the plain entry value (`is raw`
            // reduce lvalue descent).
            Some(Value::HashEntryRef {
                hash: arc.clone(),
                path: vec![key.to_string()],
                eager: true,
            })
        } else {
            None
        }
    }

    /// Autovivifying hash element access for bind descent — the hash analogue of
    /// [`array_slot_ref`] (Phase 2). Instead of the stale `HashEntryRef`
    /// back-reference, it returns a first-class value that survives COW:
    /// - an existing `ContainerRef` cell is returned as-is (already aliased);
    /// - an existing container leaf (Array/Hash) is returned by value — it shares
    ///   the inner Arc, so descent and the eventual leaf write land in the same
    ///   physical container (no back-reference needed, like the lazy op);
    /// - an existing scalar leaf is promoted in place to a shared `ContainerRef`
    ///   cell and that cell is returned (the bind aliases it by cell identity);
    /// - a missing key is autovivified to an empty Hash (the old descent
    ///   behavior) and returned by value (shared Arc).
    pub fn hash_autovivify_cell(&self, key: &str) -> Option<Value> {
        if let Value::Hash(arc) = self {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `arc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(arc) };
            match data.map.get_mut(key) {
                Some(Value::ContainerRef(cell)) => Some(Value::ContainerRef(cell.clone())),
                Some(elem @ (Value::Array(..) | Value::Hash(..))) => Some(elem.clone()),
                Some(elem) => {
                    let cell = crate::gc::Gc::new(Mutex::new(std::mem::replace(elem, Value::Nil)));
                    *elem = Value::ContainerRef(cell.clone());
                    Some(Value::ContainerRef(cell))
                }
                None => {
                    let new_hash = Value::hash(HashMap::new());
                    data.map.insert(key.to_string(), new_hash.clone());
                    Some(new_hash)
                }
            }
        } else {
            None
        }
    }

    /// Bind to hash element `key`, promoting it to a first-class container
    /// (Phase 2 Stage 1) — the hash analogue of [`array_slot_ref`]. An existing
    /// *scalar* leaf is replaced in place with a shared `ContainerRef` cell
    /// (reusing one if already present), and that same cell is returned so the
    /// binding aliases the element by **cell identity**, surviving COW clones of
    /// any enclosing container on a later write (the staleness that the old
    /// `HashEntryRef` back-reference suffers for deep `%h<a><b>` paths).
    ///
    /// An existing *container* leaf (Array/Hash) is an intermediate level of a
    /// deeper path (`%h<a><b>`, `%h<a>[1]`); it keeps the old `HashEntryRef` so
    /// the deeper traversal resolves through the shared inner Arc and the
    /// eventual leaf promotion lands in the physical map the entry points to.
    /// A *missing* key stays lazy (no entry created) — promotion is deferred to
    /// the first write (a `HashEntryRef` token carries the path until then).
    ///
    /// Reads decontainerize at the single chokepoint (`resolve_hash_entry`);
    /// writes go through `hash_insert_through` (Stage 0).
    pub fn hash_slot_ref(&self, key: &str, terminal: bool) -> Option<Value> {
        if let Value::Hash(arc) = self {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `arc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(arc) };
            match data.map.get_mut(key) {
                Some(Value::ContainerRef(cell)) => Some(Value::ContainerRef(cell.clone())),
                Some(elem @ (Value::Array(..) | Value::Hash(..))) if !terminal => {
                    // Intermediate container: return the element value
                    // itself — it shares the inner Arc, so the eventual
                    // leaf promotion by the next index op lands in the
                    // physical map the entry points to (Stage 2: no
                    // `HashEntryRef` back-reference needed).
                    Some(elem.clone())
                }
                Some(elem) => {
                    let cell = crate::gc::Gc::new(Mutex::new(std::mem::replace(elem, Value::Nil)));
                    *elem = Value::ContainerRef(cell.clone());
                    Some(Value::ContainerRef(cell))
                }
                None => Some(Value::HashEntryRef {
                    hash: arc.clone(),
                    path: vec![key.to_string()],
                    eager: false,
                }),
            }
        } else {
            None
        }
    }

    /// Autovivify a hash entry with a scalar value (for binding/assignment).
    /// Inserts the given value at the key if missing, or replaces the existing value.
    /// Returns the value stored at the key after the operation.
    /// Uses the same interior-mutation approach as `hash_autovivify`.
    pub fn hash_assign_at(&self, key: &str, val: Value) -> Option<Value> {
        if let Value::Hash(arc) = self {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `arc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(arc) };
            Value::hash_insert_through(&mut data.map, key.to_string(), val.clone());
            Some(val)
        } else {
            None
        }
    }

    /// Read the current value a `HashEntryRef` points to, walking its `path`
    /// READ-ONLY (no autovivification). Returns `Any` if any intermediate level
    /// is missing or not a hash, or if the terminal key is absent — so a bind to
    /// a not-yet-existent entry reads as `Any` without polluting `:exists`.
    ///
    /// A deferred missing-key bind connects ONLY when written THROUGH the
    /// bound var (`store_through_cell` installs a `ContainerRef` cell at the
    /// path on first write). A terminal holding a plain value was written
    /// independently through the hash path AFTER the bind — rakudo does not
    /// retro-bind that (t/phantom-entry-bind.t), so it reads as `Any` here.
    /// (Pre-§3, the independent write COW-detached the root and the token's
    /// captured `Gc` stayed empty, which masked this; in-place hash writes
    /// now reach the captured root, so the connect condition must be the
    /// cell identity, not mere path existence.)
    /// An EAGER token (`hash_autovivify`, `is raw` reduce descent) reads
    /// through to the plain entry value — its entry was created with the
    /// token, so path existence IS the connection.
    pub fn hash_entry_read(&self) -> Value {
        let Value::HashEntryRef { hash, path, eager } = self else {
            return self.clone();
        };
        let any = || Value::Package(crate::symbol::Symbol::intern("Any"));
        let mut cur = hash.clone();
        for k in &path[..path.len() - 1] {
            let ptr = crate::gc::Gc::as_ptr(&cur);
            match unsafe { (*ptr).get(k.as_str()) } {
                Some(Value::Hash(inner)) => cur = inner.clone(),
                _ => return any(),
            }
        }
        let last = path.last().unwrap();
        let ptr = crate::gc::Gc::as_ptr(&cur);
        match unsafe { (*ptr).get(last.as_str()) } {
            Some(Value::ContainerRef(cell)) => {
                cell.lock().unwrap_or_else(|e| e.into_inner()).clone()
            }
            Some(v) if *eager => v.clone(),
            _ => any(),
        }
    }

    /// Walk-CREATE the intermediate hashes of a `HashEntryRef`'s `path` and
    /// return the `(terminal hash Arc, terminal key)` so the caller can insert.
    /// Missing/non-hash intermediate levels are replaced with fresh empty hashes
    /// (interior mutation, so all holders of the shared Arc observe them).
    pub(crate) fn hash_entry_terminal(&self) -> Option<(Gc<HashData>, String)> {
        let Value::HashEntryRef { hash, path, .. } = self else {
            return None;
        };
        let mut cur = hash.clone();
        for k in &path[..path.len() - 1] {
            // SAFETY: aliased in-place mutation of a shared hash; see
            // `gc_contents_mut`. No borrow into the map is live across the write.
            let next = {
                let data = unsafe { gc_contents_mut(&cur) };
                match data.map.get(k) {
                    Some(Value::Hash(inner)) => inner.clone(),
                    _ => {
                        let new_hash = Value::hash(HashMap::new());
                        let arc = match &new_hash {
                            Value::Hash(a) => a.clone(),
                            _ => unreachable!(),
                        };
                        Value::hash_insert_through(&mut data.map, k.clone(), new_hash);
                        arc
                    }
                }
            };
            cur = next;
        }
        Some((cur, path.last().unwrap().clone()))
    }
}
