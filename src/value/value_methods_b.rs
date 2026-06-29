use super::*;

impl Value {
    /// Write a value through a `HashEntryRef`, walk-creating intermediate hashes
    /// and inserting at the terminal key (interior mutation).
    pub fn hash_entry_write(&self, val: Value) {
        if let Some((arc, key)) = self.hash_entry_terminal() {
            // SAFETY: aliased in-place mutation of a shared hash; see
            // `arc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { arc_contents_mut(&arc) };
            Value::hash_insert_through(&mut data.map, key, val);
        }
    }

    /// Push a value to an Array in-place using interior mutation.
    /// This allows shared references (Arc refcount > 1) to see the mutation,
    /// matching Raku's container semantics where all references share state.
    /// Safety: same assumptions as hash_autovivify — callers ensure no
    /// concurrent reads/writes to the same Arc.
    pub fn array_push_in_place(&self, val: Value) -> bool {
        if let Value::Array(arc, _) = self {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `arc_contents_mut`. No borrow into the items is live across the push.
            let data = unsafe { arc_contents_mut(arc) };
            data.items.push(val);
            true
        } else {
            false
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
    pub fn array_slot_ref(&self, idx: usize, terminal: bool) -> Option<Value> {
        if let Value::Array(arc, _kind) = self {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `arc_contents_mut`. No borrow into the items is live across the
            // growth/promotion below.
            let data = unsafe { arc_contents_mut(arc) };
            while data.len() <= idx {
                data.push(Value::Nil);
            }
            let elem = &mut data[idx];
            if let Value::ContainerRef(cell) = elem {
                return Some(Value::ContainerRef(cell.clone()));
            }
            // Only promote a *scalar* leaf to a cell. A container element
            // (Array/Hash) is an intermediate level of a deeper path
            // (`$s[1][1]`, `$s[1]<k>`); return the element value itself —
            // it shares the inner Arc, so the eventual leaf promotion by
            // the next index op lands in the same physical Vec/HashMap the
            // stored element points to (Stage 2: no array element back-reference
            // back-reference needed).
            if !terminal && matches!(elem, Value::Array(..) | Value::Hash(..)) {
                return Some(elem.clone());
            }
            let cell = Arc::new(Mutex::new(std::mem::replace(elem, Value::Nil)));
            *elem = Value::ContainerRef(cell.clone());
            Some(Value::ContainerRef(cell))
        } else {
            None
        }
    }

    /// Encode a Value as a hash key string.
    /// Regex values are encoded with a special prefix to preserve their identity.
    pub fn hash_key_encode(val: &Value) -> String {
        match val {
            Value::Regex(pattern) => {
                format!("\0rx:{}", pattern)
            }
            other => other.to_string_value(),
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
        match self {
            Value::Scalar(inner) => inner.descalarize(),
            other => other,
        }
    }

    /// Owned, recursive `Scalar`-strip. Same axis/semantics as [`Value::descalarize`]
    /// but consumes `self` and returns the inner value by value (no extra clone for
    /// callers that already own the value). Canonical replacement for the former
    /// `runtime::methods_mut::strip_scalar`.
    pub fn into_descalarized(self) -> Value {
        match self {
            Value::Scalar(inner) => inner.into_descalarized(),
            other => other,
        }
    }
    pub fn set(s: HashSet<String>) -> Self {
        Value::Set(Arc::new(SetData::new(s)), false)
    }
    pub fn set_hash(s: HashSet<String>) -> Self {
        Value::Set(Arc::new(SetData::new(s)), true)
    }
    /// Create a Set with preserved original key types.
    pub fn set_typed(elements: HashSet<String>, original_keys: HashMap<String, Value>) -> Self {
        Value::Set(
            Arc::new(SetData::with_original_keys(elements, original_keys)),
            false,
        )
    }
    /// Create a SetHash with preserved original key types.
    pub fn set_hash_typed(
        elements: HashSet<String>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        Value::Set(
            Arc::new(SetData::with_original_keys(elements, original_keys)),
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
        Value::Bag(Arc::new(BagData::new(Self::bag_counts_from_i64(m))), false)
    }
    pub fn bag_hash(m: HashMap<String, i64>) -> Self {
        Value::Bag(Arc::new(BagData::new(Self::bag_counts_from_i64(m))), true)
    }
    /// Create a Bag from an arbitrary-precision BigInt count map.
    pub fn bag_big(m: HashMap<String, NumBigInt>) -> Self {
        Value::Bag(Arc::new(BagData::new(m)), false)
    }
    /// Create a BagHash from an arbitrary-precision BigInt count map.
    pub fn bag_hash_big(m: HashMap<String, NumBigInt>) -> Self {
        Value::Bag(Arc::new(BagData::new(m)), true)
    }
    /// Create a Bag with preserved original key types.
    pub fn bag_typed(counts: HashMap<String, i64>, original_keys: HashMap<String, Value>) -> Self {
        Value::Bag(
            Arc::new(BagData::with_original_keys(
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
            Arc::new(BagData::with_original_keys(counts, original_keys)),
            false,
        )
    }
    /// Create a BagHash with preserved original key types.
    pub fn bag_hash_typed(
        counts: HashMap<String, i64>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        Value::Bag(
            Arc::new(BagData::with_original_keys(
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
            Arc::new(BagData::with_original_keys(counts, original_keys)),
            true,
        )
    }
    pub fn mix(mut m: HashMap<String, f64>) -> Self {
        m.retain(|_, weight| *weight != 0.0);
        Value::Mix(Arc::new(MixData::new(m)), false)
    }
    pub fn mix_hash(mut m: HashMap<String, f64>) -> Self {
        m.retain(|_, weight| *weight != 0.0);
        Value::Mix(Arc::new(MixData::new(m)), true)
    }
    pub fn mix_with_original_keys(
        mut weights: HashMap<String, f64>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        weights.retain(|_, weight| *weight != 0.0);
        Value::Mix(
            Arc::new(MixData::with_original_keys(weights, original_keys)),
            false,
        )
    }
    pub fn mix_hash_with_original_keys(
        mut weights: HashMap<String, f64>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        weights.retain(|_, weight| *weight != 0.0);
        Value::Mix(
            Arc::new(MixData::with_original_keys(weights, original_keys)),
            true,
        )
    }
    pub fn slip(items: Vec<Value>) -> Self {
        Value::Slip(Arc::new(items))
    }
    pub fn junction(kind: JunctionKind, values: Vec<Value>) -> Self {
        Value::Junction {
            kind,
            values: Arc::new(values),
        }
    }

    /// Create a new Sub value wrapping the given SubData in an Arc.
    pub(crate) fn make_sub(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        is_rw: bool,
        env: Env,
    ) -> Self {
        Value::Sub(Arc::new(SubData {
            package,
            name,
            params,
            param_defs,
            body,
            is_rw,
            is_raw: false,
            env,
            assumed_positional: Vec::new(),
            assumed_named: HashMap::new(),
            id: next_instance_id(),
            empty_sig: false,
            is_bare_block: false,
            compiled_code: None,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            upvalues: Vec::new(),
        }))
    }

    /// Create a new Sub value with an explicit id.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_sub_with_id(
        package: Symbol,
        name: Symbol,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        is_rw: bool,
        env: Env,
        id: u64,
    ) -> Self {
        Value::Sub(Arc::new(SubData {
            package,
            name,
            params,
            param_defs,
            body,
            is_rw,
            is_raw: false,
            env,
            assumed_positional: Vec::new(),
            assumed_named: HashMap::new(),
            id,
            empty_sig: false,
            is_bare_block: false,
            compiled_code: None,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            upvalues: Vec::new(),
        }))
    }

    /// Access SubData fields if this is a Sub (or upgraded WeakSub).
    #[allow(dead_code)]
    pub(crate) fn as_sub(&self) -> Option<&SubData> {
        match self {
            Value::Sub(data) => Some(data),
            _ => None,
        }
    }

    /// Upgrade a WeakSub to a Sub, or return Nil if expired.
    #[allow(dead_code)]
    pub(crate) fn upgrade_weak(&self) -> Value {
        match self {
            Value::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => Value::Nil,
            },
            other => other.clone(),
        }
    }

    pub(crate) fn make_instance(class_name: Symbol, attributes: HashMap<String, Value>) -> Self {
        let id = next_instance_id();
        Self::make_instance_with_id(class_name, attributes, id)
    }

    pub(crate) fn make_instance_without_destroy(
        class_name: Symbol,
        attributes: HashMap<String, Value>,
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
    /// existing instance's `Arc<InstanceAttrs>` (see [`Value::instance_sharing_cell`]),
    /// not from this constructor — so this never reuses another holder's cell.
    pub(crate) fn make_instance_with_id(
        class_name: Symbol,
        attributes: HashMap<String, Value>,
        id: u64,
    ) -> Self {
        Value::Instance {
            class_name,
            attributes: Arc::new(InstanceAttrs::new(class_name, attributes, id, true)),
            id,
        }
    }

    /// Phase 3 registry-removal: return a `Value::Instance` that SHARES `attrs`'s
    /// live cell, optionally under a new `class_name` (rebless / role mixin). This
    /// replaces the `make_instance_with_id` rebuild branch, which reused the cell
    /// by looking it up in the global `instance_cells` registry. Sharing the
    /// `Arc<InstanceAttrs>` directly keeps in-place mutations visible to every
    /// existing alias and to the returned value, without the registry.
    pub(crate) fn instance_sharing_cell(
        attrs: &Arc<InstanceAttrs>,
        class_name: Symbol,
        id: u64,
    ) -> Value {
        debug_assert_eq!(attrs.id, id, "instance_sharing_cell id mismatch");
        let attributes = if attrs.class_name == class_name {
            Arc::clone(attrs)
        } else {
            Arc::new(attrs.with_class(class_name))
        };
        Value::Instance {
            class_name,
            attributes,
            id,
        }
    }

    /// Phase 3 registry-removal: write `map` into `attrs`'s shared cell in place
    /// and return a `Value::Instance` aliasing that same cell. The single helper
    /// for the common writeback-then-rebuild pattern: it replaces the paired
    /// `overwrite_instance_bindings_by_identity(..) + make_instance_with_id(..)`.
    pub(crate) fn write_back_sharing(
        attrs: &Arc<InstanceAttrs>,
        class_name: Symbol,
        map: HashMap<String, Value>,
        id: u64,
    ) -> Value {
        attrs.commit_attrs(map);
        Value::instance_sharing_cell(attrs, class_name, id)
    }

    /// An independent snapshot for `temp`/`let` saves. An instance is deep-copied
    /// into a fresh, *unregistered* cell, so a later in-place mutation through
    /// the live shared cell does not alter the saved value (pre-Stage-1 this
    /// independence came for free from copy-on-write). Arrays/hashes keep their
    /// CoW `Arc` (forked on first mutation), so a plain clone is already
    /// independent for them.
    pub(crate) fn into_temp_snapshot(self) -> Value {
        match self {
            Value::Instance {
                class_name,
                attributes,
                id,
            } => Value::Instance {
                class_name,
                attributes: Arc::new((*attributes).clone()),
                id,
            },
            other => other,
        }
    }

    fn make_instance_with_destroy(
        class_name: Symbol,
        attributes: HashMap<String, Value>,
        queue_destroy: bool,
    ) -> Self {
        let id = next_instance_id();
        Value::Instance {
            class_name,
            attributes: Arc::new(InstanceAttrs::new(
                class_name,
                attributes,
                id,
                queue_destroy,
            )),
            id,
        }
    }

    /// Create an Instant value from the current system time.
    pub(crate) fn make_instant_now() -> Self {
        let posix = current_time_secs_f64();
        let tai = crate::builtins::methods_0arg::temporal::posix_to_instant(posix);
        let mut attrs = HashMap::new();
        attrs.insert("value".to_string(), Value::Num(tai));
        Value::make_instance(Symbol::intern("Instant"), attrs)
    }

    /// Create a Match object with positional captures.
    pub(crate) fn make_match_object_with_captures(
        matched: String,
        from: i64,
        to: i64,
        positional: &[String],
        named: &HashMap<String, Vec<String>>,
    ) -> Self {
        Self::make_match_object_full(
            matched,
            from,
            to,
            positional,
            named,
            &HashMap::new(),
            &[],
            &[],
            &[],
            None,
        )
    }
}
