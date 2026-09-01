use super::*;

impl Value {
    /// Write a value through a `HashEntryRef`, walk-creating the intermediate
    /// containers and inserting at the terminal slot (interior mutation).
    pub fn hash_entry_write(&self, val: Value) {
        if let Some(terminal) = self.hash_entry_terminal() {
            terminal.insert(val);
        }
    }

    /// Push a value to an Array in-place using interior mutation.
    /// This allows shared references (Arc refcount > 1) to see the mutation,
    /// matching Raku's container semantics where all references share state.
    /// Safety: same assumptions as hash_autovivify — callers ensure no
    /// concurrent reads/writes to the same Arc.
    pub fn array_push_in_place(&self, val: Value) -> bool {
        if let ValueView::Array(arc, _) = self.view() {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the items is live across the push.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
            data.items.push(val);
            true
        } else {
            false
        }
    }

    /// Write `val` into array element `idx` in place, through the shared `Gc`.
    ///
    /// The counterpart of [`array_push_in_place`](Self::array_push_in_place) for
    /// an *existing* slot. Every holder of the same container — an instance
    /// attribute slot, a closure capture, a `.clone`d object that shares the
    /// attribute's container — observes the write, because the container is the
    /// one canonical cell rather than a value that has to be rebound by name.
    /// When the slot already holds a `ContainerRef` cell the write goes
    /// *through* the cell (via [`assign_element_slot`](Self::assign_element_slot)),
    /// so a `:=` binding to that element sees it too.
    ///
    /// Returns `false` (writing nothing) when `self` is not an Array or `idx`
    /// is out of range.
    pub fn array_set_in_place(&self, idx: usize, val: Value) -> bool {
        if let ValueView::Array(arc, _) = self.view() {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the items is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
            let items = data.items_mut();
            if idx >= items.len() {
                return false;
            }
            Value::assign_element_slot(&mut items[idx], val);
            true
        } else {
            false
        }
    }

    /// A copy of this `Array`/`Hash` that shares nothing with the original: a
    /// fresh `Gc` holding cloned data (elements, and the container's own
    /// metadata). `None` for anything that is not a real `Array`/`Hash`.
    ///
    /// Used where a value must become a container the receiver *owns* — an
    /// `@`/`%` attribute initialized from a supplied named argument, which in
    /// Raku gets its own container so later mutation through the attribute
    /// cannot reach the caller's array (`C.new(x => @src); $o.x.push(9)` leaves
    /// `@src` alone).
    pub fn detached_container_copy(&self) -> Option<Value> {
        match self.view() {
            ValueView::Array(arc, kind) => Some(Value::array_with_kind(
                crate::gc::Gc::new((**arc).clone()),
                kind,
            )),
            ValueView::Hash(arc) => Some(Value::hash_with_data_itemized(
                crate::gc::Gc::new((**arc).clone()),
                self.hash_is_itemized(),
            )),
            _ => None,
        }
    }

    /// Replace the *contents* of an `Array`/`Hash` container in place with
    /// those of `src`, keeping this container's identity.
    ///
    /// This is what Raku's `@a = (…)` / `%h = (…)` means: a list assignment
    /// clears and refills the **existing** container rather than rebinding the
    /// name to a fresh one. mutsu's public-accessor assignment
    /// (`$obj.array-attr = (…)`) used to store a brand-new container in the
    /// attribute slot, which silently severed every share of the old one — most
    /// visibly the `Array`/`Hash` attributes that `Mu.clone` deliberately shares
    /// between the original and the clone.
    ///
    /// Container *metadata* (element/key type constraints, `is default`,
    /// declared type, descriptor name) belongs to the container being assigned
    /// into and is deliberately preserved; only the elements come from `src`.
    /// Returns `false` when the two values are not the same container kind, in
    /// which case the caller must fall back to replacing the slot.
    pub fn replace_container_contents(&self, src: &Value) -> bool {
        match (self.view(), src.view()) {
            (ValueView::Array(dst_arc, _), ValueView::Array(src_items, _)) => {
                if crate::gc::Gc::ptr_eq(&dst_arc, &src_items) {
                    return true;
                }
                let new_items = src_items.items().clone();
                // SAFETY: aliased in-place mutation of a shared container; see
                // `gc_contents_mut`. No borrow into the items is live across
                // the replacement.
                let data = unsafe { crate::value::gc_contents_mut(&dst_arc) };
                // The old element vector is authoritative for the reconstruction;
                // a stale native payload would decode back over the new elements.
                data.clear_native_storage();
                *data.items_mut() = new_items;
                true
            }
            (ValueView::Hash(dst_arc), ValueView::Hash(src_map)) => {
                if crate::gc::Gc::ptr_eq(&dst_arc, &src_map) {
                    return true;
                }
                let new_map = src_map.map.clone();
                let new_original_keys = src_map.original_keys.clone();
                // SAFETY: aliased in-place mutation of a shared container; see
                // `gc_contents_mut`. No borrow into the map is live across the
                // replacement.
                let data = unsafe { crate::value::gc_contents_mut(&dst_arc) };
                data.map = new_map;
                data.original_keys = new_original_keys;
                true
            }
            _ => false,
        }
    }

    /// Ensure array element `idx` exists and is a descendable Array, creating
    /// it (and filling any gap with the element type object) when missing or a
    /// scalar hole. Returns the child Array value (sharing the inner `Arc`, so a
    /// later leaf promotion lands in the real container) — the array analogue of
    /// `hash_autovivify` for an *intermediate* multi-dim descent level. Returns
    /// `None` if `self` is not an Array or the existing element is a non-array
    /// container (a Hash — a shape the caller cannot descend as an array index).
    pub fn ensure_array_child(&self, idx: usize) -> Option<Value> {
        let ValueView::Array(arc, _kind) = self.view() else {
            return None;
        };
        // SAFETY: aliased in-place mutation of a shared container; see
        // `gc_contents_mut`. No borrow into the items is live across the write.
        let data = unsafe { crate::value::gc_contents_mut(&arc) };
        let hole = data
            .default
            .as_ref()
            .map(|d| (**d).clone())
            .unwrap_or_else(|| {
                Value::Package(Symbol::intern(data.value_type.as_deref().unwrap_or("Any")))
            });
        while data.len() <= idx {
            data.push(hole.clone());
        }
        // Only vivify an empty slot (`Nil`/type-object hole). A real scalar leaf
        // (`Int`/`Str`/…) or a `Hash` is NOT overwritten — returning `None` there
        // makes the caller fall back to a plain read, so a read-only use of the
        // subscript cannot corrupt existing data.
        let is_hole = |v: &Value| matches!(v.view(), ValueView::Nil | ValueView::Package(..));
        // Deref an existing ContainerRef cell so we inspect/replace its inner value.
        if let ValueView::ContainerRef(cell) = data[idx].view() {
            let inner = cell.lock().unwrap().clone();
            if matches!(inner.view(), ValueView::Array(..)) {
                return Some(inner);
            }
            if is_hole(&inner) {
                let fresh = Value::real_array(Vec::new());
                *cell.lock().unwrap() = fresh.clone();
                return Some(fresh);
            }
            return None;
        }
        if matches!(data[idx].view(), ValueView::Array(..)) {
            Some(data[idx].clone())
        } else if is_hole(&data[idx]) {
            let fresh = Value::real_array(Vec::new());
            data[idx] = fresh.clone();
            Some(fresh)
        } else {
            None
        }
    }

    /// Grow the array so `idx` is in range, filling the gap with the element
    /// hole value (the declared element type object, or the `is default(...)`
    /// value) — the eager growth `array_slot_ref` used to do unconditionally.
    /// A no-op when `idx` is already in range or `self` is not an Array.
    ///
    /// Only for callers that genuinely need the slot to exist *now*; the `:=`
    /// bind path deliberately does not, and hands out a deferred token instead.
    pub fn array_grow_to(&self, idx: usize) {
        if let ValueView::Array(arc, _kind) = self.view() {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the items is live across the
            // growth.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
            if idx < data.len() {
                return;
            }
            let hole = data
                .default
                .as_ref()
                .map(|d| (**d).clone())
                .unwrap_or_else(|| {
                    Value::Package(Symbol::intern(data.value_type.as_deref().unwrap_or("Any")))
                });
            while data.len() <= idx {
                data.push(hole.clone());
            }
        }
    }

    /// Bind to array element `idx`, promoting it to a first-class container
    /// (Phase 2). The element is replaced in place with a shared
    /// `ContainerRef` cell (reusing an existing one), and that same cell is
    /// returned so the binding aliases the element by **cell identity**. Unlike
    /// the old array element back-reference (an array-Arc + index back-reference, which goes
    /// stale when an enclosing container is COW-cloned on a later write), the
    /// `Arc<Mutex>` cell is shared on every clone, so the alias survives
    /// arbitrarily deep `$struct[..]<..>[..]` paths. Reads decontainerize the
    /// element at the single read chokepoint (`resolve_array_entry`).
    ///
    /// An index PAST THE END has nothing to promote. When `terminal` (the
    /// outermost bind subscript) it stays lazy, exactly like `hash_slot_ref`'s
    /// missing-key arm: a deferred `HashEntryRef` token rooted on this array
    /// carries the index until the first write walk-creates it, so
    /// `my @a = 1, 2; my $r := @a[5]` does not grow `@a`. A NON-terminal step
    /// still grows eagerly — the intermediate level must exist for the next
    /// subscript to descend it (the analogue of `hash_autovivify_cell`).
    pub fn array_slot_ref(&self, idx: usize, terminal: bool) -> Option<Value> {
        if let ValueView::Array(arc, _kind) = self.view() {
            if idx >= arc.len() {
                if terminal {
                    // Past the end and nothing to promote: hand back a DEFERRED
                    // vivification token instead of growing, the array twin of
                    // `hash_slot_ref`'s missing-key arm. `my @a = 1, 2;
                    // my $r := @a[5]` must leave `@a` two elements long (raku),
                    // and only the first write through `$r` fills the gap — see
                    // `EntryTerminal`'s array arm, which grows with the same
                    // hole value this used to push eagerly.
                    return Some(Value::from_repr(ValueRepr::HashEntryRef {
                        root: crate::value::EntryRoot::Array(arc.clone()),
                        path: vec![crate::value::EntryStep::Index(idx)],
                        eager: false,
                    }));
                }
                // A non-terminal (intermediate) descent step is the eager
                // analogue of `hash_autovivify_cell`: the level has to exist
                // before the next subscript can descend it.
                self.array_grow_to(idx);
            }
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the items is live across the
            // promotion below.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
            // A typed array's element constraint rides on the promoted cell
            // (ADR-0036 slice 4), so a write through the cell — an lvalue
            // return, a `:=` alias — is checked exactly like `@a[i] = v`.
            let value_type = data.value_type.clone();
            let elem = &mut data[idx];
            if let ValueView::ContainerRef(cell) = elem.view() {
                return Some(Value::ContainerRef(cell.clone()));
            }
            // Only promote a *scalar* leaf to a cell. A container element
            // (Array/Hash) is an intermediate level of a deeper path
            // (`$s[1][1]`, `$s[1]<k>`); return the element value itself —
            // it shares the inner Arc, so the eventual leaf promotion by
            // the next index op lands in the same physical Vec/HashMap the
            // stored element points to (Stage 2: no array element back-reference
            // back-reference needed).
            if !terminal && matches!(elem.view(), ValueView::Array(..) | ValueView::Hash(..)) {
                return Some(elem.clone());
            }
            let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(std::mem::replace(
                elem,
                Value::Nil,
            )));
            // The owner name defaults to the bare sigil, which is what raku
            // prints for an anonymous container; a promotion site that knows
            // the source variable retags it (`retag_element_owner`), so the
            // failure blames `@a` the way a direct store does.
            if let Some(tc) = value_type.as_deref() {
                crate::value::register_element_constraint(&cell, tc, "@");
            }
            *elem = Value::ContainerRef(cell.clone());
            Some(Value::ContainerRef(cell))
        } else {
            None
        }
    }

    /// Encode a Value as a hash key string.
    /// Regex values are encoded with a special prefix to preserve their identity.
    pub fn hash_key_encode(val: &Value) -> String {
        match val.view() {
            ValueView::Regex(pattern) => {
                format!("\0rx:{}", *pattern)
            }
            _ => val.to_string_value(),
        }
    }

    /// Decode a hash key string back to a Value.
    /// Regex-encoded keys (with \0rx: prefix) are restored to Regex values.
    pub fn hash_key_decode(key: &str) -> Value {
        if let Some(pattern) = key.strip_prefix("\0rx:") {
            return Value::regex(pattern.to_string());
        }
        Value::str(key.to_string())
    }

    // --- decont family (see docs/container-identity.md §3) ---
    // mutsu has THREE "decontainerize" operations on THREE different axes.
    // They are intentionally NOT fused into one helper:
    //   - Value::descalarize / into_descalarized — strips `Scalar` ($(...)), RECURSIVE
    //   - Value::with_deref / deref_container / into_deref — reads through `ContainerRef` (:=),
    //     single cell. with_deref is non-cloning; deref_container clones from &self;
    //     into_deref consumes self, cloning only the inner of a ContainerRef (move otherwise).
    //   - ArrayKind::decontainerize               — strips `ItemList`/`ItemArray` flag (list flatten)
    // A "full decont" that strips all three is deferred to Phase 1+; it must NOT be
    // applied at lvalue/container-requiring sites (is-rw writeback, :=, .VAR, =:=,
    // take-rw, autoviv slot-refs), which need the live cell or the container variant.

    /// Unwrap a `Scalar` container, returning a reference to the inner value.
    /// RECURSIVE: nested `$($(...))` are fully stripped. Non-Scalar values are
    /// returned as-is. See the decont family note above; this is the Scalar axis only.
    pub fn descalarize(&self) -> &Value {
        match self.view() {
            ValueView::Scalar(inner) => inner.descalarize(),
            _ => self,
        }
    }

    /// Owned, recursive `Scalar`-strip. Same axis/semantics as [`Value::descalarize`]
    /// but consumes `self` and returns the inner value by value (no extra clone for
    /// callers that already own the value). Canonical replacement for the former
    /// `runtime::methods_mut::strip_scalar`.
    pub fn into_descalarized(self) -> Value {
        match self.into_repr() {
            ValueRepr::Scalar(inner) => inner.into_descalarized(),
            other => Value::from_repr(other),
        }
    }
    pub fn set(s: HashSet<String>) -> Self {
        Value::Set(crate::gc::Gc::new(SetData::new(s)), false)
    }
    pub fn set_hash(s: HashSet<String>) -> Self {
        Value::Set(crate::gc::Gc::new(SetData::new(s)), true)
    }
    /// Create a Set with preserved original key types.
    pub fn set_typed(elements: HashSet<String>, original_keys: HashMap<String, Value>) -> Self {
        Value::Set(
            crate::gc::Gc::new(SetData::with_original_keys(elements, original_keys)),
            false,
        )
    }
    /// Create a SetHash with preserved original key types.
    pub fn set_hash_typed(
        elements: HashSet<String>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        Value::Set(
            crate::gc::Gc::new(SetData::with_original_keys(elements, original_keys)),
            true,
        )
    }
    /// Convert an i64 count map to the arbitrary-precision BigInt map BagData uses.
    fn bag_counts_from_i64(m: HashMap<String, i64>) -> HashMap<String, NumBigInt> {
        m.into_iter()
            .map(|(k, v)| (k, NumBigInt::from(v)))
            .collect()
    }
    pub fn bag(m: HashMap<String, i64>) -> Self {
        Value::Bag(
            crate::gc::Gc::new(BagData::new(Self::bag_counts_from_i64(m))),
            false,
        )
    }
    pub fn bag_hash(m: HashMap<String, i64>) -> Self {
        Value::Bag(
            crate::gc::Gc::new(BagData::new(Self::bag_counts_from_i64(m))),
            true,
        )
    }
    /// Create a Bag from an arbitrary-precision BigInt count map.
    pub fn bag_big(m: HashMap<String, NumBigInt>) -> Self {
        Value::Bag(crate::gc::Gc::new(BagData::new(m)), false)
    }
    /// Create a BagHash from an arbitrary-precision BigInt count map.
    pub fn bag_hash_big(m: HashMap<String, NumBigInt>) -> Self {
        Value::Bag(crate::gc::Gc::new(BagData::new(m)), true)
    }
    /// Create a Bag with preserved original key types.
    pub fn bag_typed(counts: HashMap<String, i64>, original_keys: HashMap<String, Value>) -> Self {
        Value::Bag(
            crate::gc::Gc::new(BagData::with_original_keys(
                Self::bag_counts_from_i64(counts),
                original_keys,
            )),
            false,
        )
    }
    /// Create a Bag with preserved original key types from a BigInt count map.
    pub fn bag_typed_big(
        counts: HashMap<String, NumBigInt>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        Value::Bag(
            crate::gc::Gc::new(BagData::with_original_keys(counts, original_keys)),
            false,
        )
    }
    /// Create a BagHash with preserved original key types.
    pub fn bag_hash_typed(
        counts: HashMap<String, i64>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        Value::Bag(
            crate::gc::Gc::new(BagData::with_original_keys(
                Self::bag_counts_from_i64(counts),
                original_keys,
            )),
            true,
        )
    }
    /// Create a BagHash with preserved original key types from a BigInt count map.
    pub fn bag_hash_typed_big(
        counts: HashMap<String, NumBigInt>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        Value::Bag(
            crate::gc::Gc::new(BagData::with_original_keys(counts, original_keys)),
            true,
        )
    }
    pub fn mix(mut m: HashMap<String, f64>) -> Self {
        m.retain(|_, weight| *weight != 0.0);
        Value::Mix(crate::gc::Gc::new(MixData::new(m)), false)
    }
    pub fn mix_hash(mut m: HashMap<String, f64>) -> Self {
        m.retain(|_, weight| *weight != 0.0);
        Value::Mix(crate::gc::Gc::new(MixData::new(m)), true)
    }
    pub fn mix_with_original_keys(
        mut weights: HashMap<String, f64>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        weights.retain(|_, weight| *weight != 0.0);
        Value::Mix(
            crate::gc::Gc::new(MixData::with_original_keys(weights, original_keys)),
            false,
        )
    }
    pub fn mix_hash_with_original_keys(
        mut weights: HashMap<String, f64>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        weights.retain(|_, weight| *weight != 0.0);
        Value::Mix(
            crate::gc::Gc::new(MixData::with_original_keys(weights, original_keys)),
            true,
        )
    }
    pub fn slip(items: Vec<Value>) -> Self {
        Value::Slip(Arc::new(items))
    }
    pub fn junction(kind: JunctionKind, values: Vec<Value>) -> Self {
        Value::from_repr(ValueRepr::Junction {
            kind,
            values: Arc::new(values),
        })
    }

    /// Fresh code-object data: the declaration-derived fields from the caller,
    /// every closure-creation field (`upvalues`, capture sets, `compiled_code`)
    /// left empty. The `make_sub*` constructors below each vary one or two fields
    /// on top of this; they used to repeat the whole literal.
    fn new_code_object(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: impl Into<std::sync::Arc<Vec<Stmt>>>,
        is_rw: bool,
        env: Env,
    ) -> SubData {
        SubData {
            package,
            name,
            params,
            param_defs,
            body: body.into(),
            is_rw,
            is_raw: false,
            env,
            assumed_positional: Vec::new(),
            assumed_named: HashMap::new(),
            id: next_instance_id(),
            empty_sig: false,
            is_bare_block: false,
            compiled_code: None,
            compiled_fns: None,
            compiled_routine: None,
            is_decl_expr_thunk: false,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            authoritative_captures: Vec::new(),
            upvalues: Vec::new(),
            captured_fatal_mode: false,
        }
    }

    /// Create a new Sub value wrapping the given SubData in an Arc.
    pub(crate) fn make_sub(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: impl Into<std::sync::Arc<Vec<Stmt>>>,
        is_rw: bool,
        env: Env,
    ) -> Self {
        Value::Sub(crate::gc::Gc::new(Self::new_code_object(
            package, name, params, param_defs, body, is_rw, env,
        )))
    }

    /// Create a code object for a *declared routine*: [`Self::make_sub`] plus the
    /// routine's own compiled body, so calling it dispatches as bytecode instead
    /// of re-compiling the AST body every time (see
    /// [`SubData::compiled_routine`], ADR-0019 C6c).
    ///
    /// `compiled_routine` is `None` for a routine the declaration plan could not
    /// attach a compiled function to (a synthesized def, or one installed through
    /// EVAL/the MOP); such a code object keeps the old AST behavior.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_sub_for_routine(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: impl Into<std::sync::Arc<Vec<Stmt>>>,
        is_rw: bool,
        env: Env,
        compiled_routine: Option<Arc<crate::opcode::CompiledFunction>>,
    ) -> Self {
        Self::make_sub_for_routine_owning(
            package,
            name,
            params,
            param_defs,
            body,
            is_rw,
            env,
            compiled_routine,
            Vec::new(),
        )
    }

    /// [`Self::make_sub_for_routine`] plus [`Self::make_sub_owning`]'s vouch:
    /// the named lexicals are installed from the captured env with OVERWRITE at
    /// call time instead of losing to a same-named lexical in the calling
    /// frame. Used when a named `sub` escapes its declaring routine as that
    /// routine's return value — the declaring frame is gone, so its captured
    /// bindings are lexically authoritative and can never go stale.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_sub_for_routine_owning(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: impl Into<std::sync::Arc<Vec<Stmt>>>,
        is_rw: bool,
        env: Env,
        compiled_routine: Option<Arc<crate::opcode::CompiledFunction>>,
        authoritative_captures: Vec<Symbol>,
    ) -> Self {
        let mut data = Self::new_code_object(package, name, params, param_defs, body, is_rw, env);
        data.authoritative_captures = authoritative_captures;
        // Declaration source location for `Code.line`/`Code.file`. Both already
        // ride on the routine's own `CompiledFunction` — the line stamped by
        // `Compiler::compile_sub_body`, the file by
        // `registration_sub::adapt_compiled_to_def` — so every construction site
        // of a declared-routine code object inherits them here rather than
        // needing its own channel. No extra storage: the `String` is cloned only
        // when a code object is materialized, not per routine.
        if let Some(cf) = &compiled_routine {
            data.source_line = cf.code.source_line.map(|l| l as u32);
            data.source_file.clone_from(&cf.source_file);
        }
        data.compiled_routine = compiled_routine;
        Value::Sub(crate::gc::Gc::new(data))
    }

    /// Create a new Sub value with an explicit id.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_sub_with_id(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: impl Into<std::sync::Arc<Vec<Stmt>>>,
        is_rw: bool,
        env: Env,
        id: u64,
    ) -> Self {
        let mut data = Self::new_code_object(package, name, params, param_defs, body, is_rw, env);
        data.id = id;
        Value::Sub(crate::gc::Gc::new(data))
    }

    /// Create a new Sub value that lexically OWNS the given captured names.
    ///
    /// Same as [`Value::make_sub`], but seeds `SubData::authoritative_captures`
    /// so the call-time env merge installs those names from the captured env
    /// with overwrite instead of letting a same-named lexical in whatever frame
    /// happens to invoke the closure win. Used for bodies built from AST at
    /// runtime (a `whenever` body), which have no `CompiledCode` and therefore
    /// no compile-time `authoritative_free_vars` to vouch for them.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_sub_owning(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: impl Into<std::sync::Arc<Vec<Stmt>>>,
        is_rw: bool,
        env: Env,
        authoritative_captures: Vec<Symbol>,
    ) -> Self {
        let mut data = Self::new_code_object(package, name, params, param_defs, body, is_rw, env);
        data.authoritative_captures = authoritative_captures;
        Value::Sub(crate::gc::Gc::new(data))
    }

    /// Build a Sub value from an already-populated `SubData` (e.g. a modified
    /// clone of an existing sub's data).
    pub(crate) fn from_sub_data(data: SubData) -> Value {
        Value::Sub(crate::gc::Gc::new(data))
    }

    /// Access SubData fields if this is a Sub (or upgraded WeakSub).
    ///
    /// Hands out a `&SubData` borrowed from `self`, which a `view()` guard
    /// cannot do (the guard is a by-value temporary scoped to the match);
    /// implemented inside the seam as a payload-pointee deref.
    #[allow(dead_code)]
    pub(crate) fn as_sub(&self) -> Option<&SubData> {
        self.0.as_sub_data()
    }

    /// Upgrade a WeakSub to a Sub, or return Nil if expired.
    #[allow(dead_code)]
    pub(crate) fn upgrade_weak(&self) -> Value {
        match self.view() {
            ValueView::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => Value::Nil,
            },
            _ => self.clone(),
        }
    }

    /// Build a fresh instance. `attributes` is an [`AttrMap`] (the `Symbol`-keyed
    /// storage) or anything convertible into one — notably a
    /// `HashMap<String, Value>`, which the many cold construction sites (typed
    /// exceptions, native-type constructors) still build; those keys are interned
    /// here, once, at construction.
    pub(crate) fn make_instance(class_name: Symbol, attributes: impl Into<AttrMap>) -> Self {
        let id = next_instance_id();
        Self::make_instance_with_id(class_name, attributes, id)
    }

    pub(crate) fn make_instance_without_destroy(
        class_name: Symbol,
        attributes: impl Into<AttrMap>,
    ) -> Self {
        Self::make_instance_with_destroy(class_name, attributes, false)
    }

    /// Build a typed exception instance (`X::Foo`) from `(attr, value)` pairs.
    /// A `message` attr is added if not supplied. Convenience for compile-time
    /// error construction (parser/compiler) where the full sorrow/panic model is
    /// modelled with real exception objects.
    pub(crate) fn make_exception(class_name: &str, attrs: &[(&str, Value)]) -> Self {
        let mut map: HashMap<String, Value> = HashMap::new();
        for (k, v) in attrs {
            map.insert((*k).to_string(), v.clone());
        }
        Self::make_instance(Symbol::intern(class_name), map)
    }

    /// Build an `X::Comp::Group` wrapping a fatal `panic` exception together with
    /// accumulated `sorrows` and `worries` (non-fatal compile-time errors and
    /// warnings). This mirrors rakudo's compile-sorrow accumulator: when a single
    /// construct produces several diagnostics, they are bundled into one group
    /// exception rather than thrown one at a time.
    pub(crate) fn make_comp_group(
        message: String,
        panic: Option<Value>,
        sorrows: Vec<Value>,
        worries: Vec<Value>,
    ) -> Self {
        let mut map: HashMap<String, Value> = HashMap::new();
        map.insert("message".to_string(), Value::str(message));
        map.insert("sorrows".to_string(), Value::array(sorrows));
        map.insert("worries".to_string(), Value::array(worries));
        if let Some(p) = panic {
            map.insert("panic".to_string(), p);
        }
        Self::make_instance(Symbol::intern("X::Comp::Group"), map)
    }

    /// Build an instance with the given id and a fresh attribute cell. Used for
    /// constructing a value with an explicit id (genuinely new instances, or
    /// sentinel ids). Cross-frame sharing of mutations comes from cloning an
    /// existing instance's `crate::gc::Gc<InstanceAttrs>` (see [`Value::instance_sharing_cell`]),
    /// not from this constructor — so this never reuses another holder's cell.
    pub(crate) fn make_instance_with_id(
        class_name: Symbol,
        attributes: impl Into<AttrMap>,
        id: u64,
    ) -> Self {
        Value::from_repr(ValueRepr::Instance {
            class_name,
            attributes: crate::gc::Gc::new(InstanceAttrs::new(
                class_name,
                attributes.into(),
                id,
                true,
            )),
            id,
        })
    }

    /// Phase 3 registry-removal: return a `Value::Instance` that SHARES `attrs`'s
    /// live cell, optionally under a new `class_name` (rebless / role mixin). This
    /// replaces the `make_instance_with_id` rebuild branch, which reused the cell
    /// by looking it up in the global `instance_cells` registry. Sharing the
    /// `crate::gc::Gc<InstanceAttrs>` directly keeps in-place mutations visible to every
    /// existing alias and to the returned value, without the registry.
    pub(crate) fn instance_sharing_cell(
        attrs: &crate::gc::Gc<InstanceAttrs>,
        class_name: Symbol,
        id: u64,
    ) -> Value {
        debug_assert_eq!(attrs.id, id, "instance_sharing_cell id mismatch");
        let attributes = if attrs.class_name() == class_name {
            crate::gc::Gc::clone(attrs)
        } else {
            crate::gc::Gc::new(attrs.with_class(class_name))
        };
        Value::from_repr(ValueRepr::Instance {
            class_name,
            attributes,
            id,
        })
    }

    /// Phase 3 registry-removal: write `map` into `attrs`'s shared cell in place
    /// and return a `Value::Instance` aliasing that same cell. The single helper
    /// for the common writeback-then-rebuild pattern: it replaces the paired
    /// `overwrite_instance_bindings_by_identity(..) + make_instance_with_id(..)`.
    pub(crate) fn write_back_sharing(
        attrs: &crate::gc::Gc<InstanceAttrs>,
        class_name: Symbol,
        map: impl Into<AttrMap>,
        id: u64,
    ) -> Value {
        attrs.commit_attrs(map.into());
        Value::instance_sharing_cell(attrs, class_name, id)
    }

    /// An independent snapshot for `temp`/`let` saves. An instance is deep-copied
    /// into a fresh, *unregistered* cell, so a later in-place mutation through
    /// the live shared cell does not alter the saved value (pre-Stage-1 this
    /// independence came for free from copy-on-write). Arrays/hashes keep their
    /// CoW `Arc` (forked on first mutation), so a plain clone is already
    /// independent for them.
    pub(crate) fn into_temp_snapshot(self) -> Value {
        match self.into_repr() {
            ValueRepr::Instance {
                class_name,
                attributes,
                id,
            } => Value::from_repr(ValueRepr::Instance {
                class_name,
                attributes: crate::gc::Gc::new((*attributes).clone()),
                id,
            }),
            other => Value::from_repr(other),
        }
    }

    fn make_instance_with_destroy(
        class_name: Symbol,
        attributes: impl Into<AttrMap>,
        queue_destroy: bool,
    ) -> Self {
        let id = next_instance_id();
        Value::from_repr(ValueRepr::Instance {
            class_name,
            attributes: crate::gc::Gc::new(InstanceAttrs::new(
                class_name,
                attributes.into(),
                id,
                queue_destroy,
            )),
            id,
        })
    }

    /// Create an Instant value from the current system time.
    pub(crate) fn make_instant_now() -> Self {
        Self::make_instant_from_posix(current_time_secs_f64())
    }

    /// Build an `Instant` from a POSIX timestamp (seconds). Used by IO::Path's
    /// `.modified`/`.accessed`/`.changed`, which return an `Instant` in Raku.
    pub(crate) fn make_instant_from_posix(posix: f64) -> Self {
        let tai = crate::builtins::methods_0arg::temporal::posix_to_instant(posix);
        let mut attrs = HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai));
        Value::make_instance(Symbol::intern("Instant"), attrs)
    }

    /// Create a Match object from TEXT-ONLY positional captures (no spans).
    /// The exploded-builder convenience for carriers that never recorded
    /// offsets (transliteration callbacks, code-block snapshots); each text
    /// renders as an eager leaf Match reporting `0..len`, same as pre-P4.
    /// Span-bearing callers use `make_match_object_full` instead.
    pub(crate) fn make_match_object_with_captures(
        from: i64,
        to: i64,
        positional_texts: &[String],
        named: &HashMap<String, Vec<String>>,
        target: crate::runtime::MatchTarget,
    ) -> Self {
        let m = Self::make_match_object_full(from, to, &[], &HashMap::new(), target.clone());
        if positional_texts.is_empty() && named.is_empty() {
            return m;
        }
        let mut updates: Vec<(&str, Value)> = Vec::new();
        if !positional_texts.is_empty() {
            let list: Vec<Value> = positional_texts
                .iter()
                .map(|s| Value::text_leaf_match(s, &target))
                .collect();
            updates.push(("list", Value::array(list)));
        }
        if !named.is_empty() {
            let named_vals: HashMap<String, Value> = named
                .iter()
                .map(|(k, texts)| {
                    let vals: Vec<Value> = texts
                        .iter()
                        .map(|s| Value::text_leaf_match(s, &target))
                        .collect();
                    let v = if vals.len() == 1 {
                        vals[0].clone()
                    } else {
                        Value::real_array(vals)
                    };
                    (k.clone(), v)
                })
                .collect();
            updates.push(("named", Value::hash(named_vals)));
        }
        m.match_with_attrs(updates).unwrap_or(m)
    }
}
