use super::*;

impl Value {
    /// Create a decontainerized Proxy value (result of .VAR on a Proxy).
    /// This Proxy won't be auto-FETCHed by method dispatch.
    pub(crate) fn proxy_var_object(proxy: Value, _target_var: String) -> Self {
        match proxy.into_repr() {
            ValueRepr::Proxy {
                fetcher,
                storer,
                subclass,
                ..
            } => Value::from_repr(ValueRepr::Proxy {
                fetcher,
                storer,
                subclass,
                decontainerized: true,
            }),
            other => Value::from_repr(other),
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
    /// A code-bearing regex literal that closed over `scope` — see
    /// [`crate::value::RegexClosure`]. Views as a plain `Regex`.
    pub(crate) fn regex_closure(
        pattern: Arc<String>,
        scope: Arc<std::collections::HashMap<String, Value>>,
    ) -> Self {
        Value::RegexCaptured(Arc::new(crate::value::RegexClosure { pattern, scope }))
    }
    /// The defining scope this regex closed over, or `None` for a regex that
    /// captured nothing (and for every non-regex value).
    pub(crate) fn regex_closure_scope(
        &self,
    ) -> Option<Arc<std::collections::HashMap<String, Value>>> {
        if let Some(scope) = self.0.regex_closure_scope() {
            return Some(scope.clone());
        }
        match self.view() {
            ValueView::RegexWithAdverbs(a) => a.captured.clone(),
            _ => None,
        }
    }
    pub fn rakuast(node: Box<crate::rakuast::RakuAstNode>) -> Self {
        Value::RakuAst(node)
    }

    pub(crate) fn rakuast_add_statement(
        &self,
        statement: Value,
    ) -> Option<Result<Value, RuntimeError>> {
        self.0.with_rakuast_inplace(|node| {
            if node.class != crate::rakuast::RakuAstClass::StatementList {
                return Err(RuntimeError::new(
                    "add-statement is only available on RakuAST::StatementList",
                ));
            }
            if !matches!(statement.view(), ValueView::RakuAst(_)) {
                return Err(RuntimeError::new(
                    "RakuAST::StatementList.add-statement expects a RakuAST node",
                ));
            }
            node.fields.push(crate::rakuast::RakuAstField {
                name: None,
                value: crate::rakuast::RakuAstFieldValue::Node(statement.clone()),
            });
            Ok(statement)
        })
    }
    pub fn mixin(inner: Value, overrides: HashMap<String, Value>) -> Self {
        Value::Mixin(Arc::new(inner), crate::gc::Gc::new(overrides))
    }
    pub fn generic_range(start: Value, end: Value, excl_start: bool, excl_end: bool) -> Self {
        Value::from_repr(ValueRepr::GenericRange {
            start: Arc::new(start),
            end: Arc::new(end),
            excl_start,
            excl_end,
        })
    }
    pub fn array(items: Vec<Value>) -> Self {
        Value::Array(crate::gc::Gc::new(ArrayData::new(items)), ArrayKind::List)
    }
    /// Create a Capture value. Boxes the positional/named payloads (the variant
    /// stores them behind `Box` to keep `Value` small).
    pub fn capture(positional: Vec<Value>, named: HashMap<String, Value>) -> Self {
        Value::from_repr(ValueRepr::Capture {
            positional: Box::new(positional),
            named: Box::new(named),
        })
    }
    /// A named variable reference (see [`ValueRepr::VarRef`]): the value `value`
    /// tagged with the name of the variable it was read from, so the binder can
    /// alias the caller's container for an `is rw` / `is raw` / `:=` target.
    pub fn varref(name: Symbol, value: Value, index: Option<u32>) -> Self {
        Self::varref_slotted(name, value, index, None)
    }
    /// [`Value::varref`] with the emitting frame's local slot recorded, so
    /// consumers that box the source variable into a shared cell
    /// (`capture_var_cell_inner`) target the binding that was actually in
    /// scope rather than a same-named shadow slot.
    pub fn varref_slotted(
        name: Symbol,
        value: Value,
        index: Option<u32>,
        slot: Option<u32>,
    ) -> Self {
        Value::from_repr(ValueRepr::VarRef {
            name,
            value: Box::new(value),
            index,
            slot,
        })
    }
    /// The `slot` recorded on a [`ValueRepr::VarRef`], or `None`.
    pub fn varref_slot(&self) -> Option<u32> {
        self.0.varref_slot()
    }
    /// The `(name, value, index)` of a [`ValueRepr::VarRef`], or `None`.
    /// Tag-probe gated: runs once per bound parameter, and a `view()` on a
    /// lazy Match would materialize it just to see it is not a VarRef.
    pub fn as_varref(&self) -> Option<(Symbol, &Value, Option<u32>)> {
        if !self.0.is_varref() {
            return None;
        }
        match self.view() {
            ValueView::VarRef { name, value, index } => Some((name, value, index)),
            _ => None,
        }
    }
    /// The value a [`ValueRepr::VarRef`] wraps, or `self` when it is not one.
    /// The binder strips the wrapper here once it has taken the name it needs.
    pub fn unwrap_varref(&self) -> &Value {
        if !self.0.is_varref() {
            return self;
        }
        match self.view() {
            ValueView::VarRef { value, .. } => value,
            _ => self,
        }
    }
    /// Create a big `Rat` value. The numerator/denominator are boxed (the
    /// variant stores them behind `Box` to keep `Value` small).
    pub fn bigrat(num: NumBigInt, den: NumBigInt) -> Self {
        Value::BigRat(Box::new(num), Box::new(den), false)
    }
    /// Create a big `FatRat` value (unlimited precision). Differs from
    /// [`Value::bigrat`] only in the FatRat flag, which drives display,
    /// `.^name`, `.raku`, `.WHICH`, and eqv.
    pub fn bigfatrat(num: NumBigInt, den: NumBigInt) -> Self {
        Value::BigRat(Box::new(num), Box::new(den), true)
    }
    /// True iff this is a big-integer-backed rational carrying the FatRat flag.
    #[inline]
    pub fn is_bigfatrat(&self) -> bool {
        self.0.is_bigfatrat()
    }
    /// The HOW (meta-object) of a `CustomType` or `CustomTypeInstance`, if this
    /// is one. Centralizes access now that both are boxed payloads.
    pub fn custom_how(&self) -> Option<&Value> {
        match self.view() {
            ValueView::CustomType(d) => Some(&d.how),
            ValueView::CustomTypeInstance(d) => Some(&d.how),
            _ => None,
        }
    }
    /// The REPR name of a `CustomType` or `CustomTypeInstance`, if this is one.
    pub fn custom_repr(&self) -> Option<&str> {
        match self.view() {
            ValueView::CustomType(d) => Some(&d.repr),
            ValueView::CustomTypeInstance(d) => Some(&d.repr),
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
    /// Fresh empty Array tagged with the "element" container-descriptor name —
    /// what an unsupplied `@`-param binds (rakudo: `@kh.VAR.name` is
    /// "element" there, and Text::CSV's `method CSV` gates on it).
    pub(crate) fn element_descriptor_array() -> Self {
        let mut data = ArrayData::new(Vec::new());
        data.descriptor_name = Some("element".into());
        Value::Array(crate::gc::Gc::new(data), ArrayKind::Array)
    }
    /// Fresh empty Hash tagged with the "element" container-descriptor name —
    /// the `%`-param twin of [`Value::element_descriptor_array`].
    pub(crate) fn element_descriptor_hash() -> Self {
        let mut data = HashData::new(std::collections::HashMap::new());
        data.descriptor_name = Some("element".into());
        Value::hash(data)
    }
    /// Stamp `name` as this container's descriptor name (rakudo: a `my @x`
    /// declaration names the fresh container "@x", and `.VAR.name` then
    /// reports it through any pass-by-binding chain — a slurpy re-flatten, a
    /// named-arg forward). No-op for non-container values. Overwrites: a
    /// declaration's container is fresh (Raku `=` copy semantics), so any
    /// name it carries is inherited COW state from the assignment source.
    pub(crate) fn stamp_descriptor_name(&mut self, name: &str) {
        if self
            .with_array_mut(|gc, _| {
                let data = crate::value::gc_data_mut(gc);
                if data.descriptor_name.as_deref() != Some(name) {
                    data.descriptor_name = Some(name.into());
                }
            })
            .is_none()
        {
            self.with_hash_mut(|gc| {
                let data = crate::value::gc_data_mut(gc);
                if data.descriptor_name.as_deref() != Some(name) {
                    data.descriptor_name = Some(name.into());
                }
            });
        }
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
    /// Create a true (non-shaped) Array value whose slots are all
    /// *unassigned* -- the multidim-autoviv counterpart of
    /// [`Value::real_array_initialized_at`], used when a nested `@a[i;j] = …`
    /// write autovivifies a fresh row: every gap-marker slot is a hole
    /// (`ArrayData::hole_at`, ADR-0049 §1.6/§4 slice 5) until a later write
    /// marks its index, same convention as [`Value::shaped_array_unassigned`]
    /// but for `ArrayKind::Array` rather than `ArrayKind::Shaped`.
    pub fn real_array_unassigned(items: Vec<Value>) -> Self {
        let mut data = ArrayData::new(items);
        data.initialized = Some(std::collections::HashSet::new());
        Value::Array(crate::gc::Gc::new(data), ArrayKind::Array)
    }
    /// Create a shaped (multidimensional) Array value.
    pub fn shaped_array(items: Vec<Value>) -> Self {
        Value::Array(crate::gc::Gc::new(ArrayData::new(items)), ArrayKind::Shaped)
    }
    /// Create a shaped Array whose slots are all *unassigned*: the declaration
    /// pre-fills them with the element type object, but raku still reports
    /// `my @a[3]; @a[0]:exists` as `False` until something is written there.
    /// An empty `initialized` set says "every gap marker is a hole", where the
    /// `None` of [`Value::shaped_array`] means "bulk-constructed, no gaps".
    pub fn shaped_array_unassigned(items: Vec<Value>) -> Self {
        let mut data = ArrayData::new(items);
        data.initialized = Some(std::collections::HashSet::new());
        Value::Array(crate::gc::Gc::new(data), ArrayKind::Shaped)
    }
    /// Rebuild an array's backing data with new elements, keeping ONLY the
    /// `initialized` set of `like`. Deliberately narrower than
    /// [`Value::array_data_like`]: a typed-element coercion rebuilds every row
    /// of a shaped array, and carrying `shape` down onto the rebuilt rows makes
    /// each row look like a shaped array of its own, which routes every element
    /// write through the multidimensional slow path (measured ~45x on the 100M-cell
    /// `roast/integration/deep-recursion-initing-native-array.t`).
    pub(crate) fn array_data_keeping_initialized(
        like: &ArrayData,
        items: Vec<Value>,
    ) -> crate::gc::Gc<ArrayData> {
        let mut data = ArrayData::new(items);
        data.initialized = like.initialized.clone();
        crate::gc::Gc::new(data)
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
            // `items` is a rebuilt authoritative vector; an old payload may
            // describe the previous vector and must not be carried across.
            native: None,
            value_type: like.value_type.clone(),
            key_type: like.key_type.clone(),
            declared_type: like.declared_type.clone(),
            default: like.default.clone(),
            shape: like.shape.clone(),
            initialized: like.initialized.clone(),
            descriptor_name: like.descriptor_name.clone(),
        })
    }
    /// Construct a `Value::Hash`. Accepts either a bare `HashMap` (fresh hash)
    /// or a `HashData` (a cloned/rebuilt hash whose container metadata is then
    /// preserved) via `Into<HashData>`.
    pub fn hash(map: impl Into<HashData>) -> Self {
        Value::from_repr(ValueRepr::Hash(Gc::new(map.into()), false))
    }

    /// True when this value is a `$`-scalar-itemized hash (`$(%h)` / `.item` /
    /// `my $h = %x`). The per-holder itemization flag lives on the `Value::Hash`
    /// variant, not in the shared `HashData`, so two holders of the same hash
    /// data can differ in itemization (`%x.raku` → `{...}`, `my $h = %x;
    /// $h.raku` → `${...}`).
    /// The per-holder itemization flag is representation state that
    /// `ValueView` hides; this reads the kind tag inside the seam.
    pub fn hash_is_itemized(&self) -> bool {
        self.0.is_hash_itemized()
    }

    /// Whether this is an immutable `Map` — a hash whose `declared_type` says
    /// `Map`, however it was built (`Map.new(...)`, `my %h is Map`, a `Capture`'s
    /// `.hash`). Sees through a `$`-scalar wrapper, because `my $m = Map.new(...)`
    /// itemizes and every mutation route should refuse the same either way.
    ///
    /// raku permits no removal from a Map at all, so this is the one predicate
    /// the delete paths consult; the assign side reaches the same conclusion
    /// from the container's `declared_type` metadata.
    pub fn is_immutable_map(&self) -> bool {
        match self.view() {
            ValueView::Hash(h) => h.declared_type.as_deref() == Some("Map"),
            ValueView::Scalar(inner) => inner.is_immutable_map(),
            _ => false,
        }
    }

    /// Return this value with its hash itemization flag set to `itemized`,
    /// preserving the SAME `HashData` `Gc` (so `=`-shared mutation still tracks).
    /// A non-hash value is returned unchanged.
    pub fn with_hash_itemized(self, itemized: bool) -> Self {
        match self.into_repr() {
            ValueRepr::Hash(gc, _) => Value::Hash(gc, itemized),
            other => Value::from_repr(other),
        }
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
        match self.into_repr() {
            ValueRepr::Array(items, kind) => Value::Array(items, kind.itemize()),
            // Itemization is a Value-level flag now, so `.item` keeps the SAME
            // `HashData` `Gc` (no copy-on-write, so `.WHICH` identity and
            // `=`-shared mutation are preserved).
            ValueRepr::Hash(h, _) => Value::Hash(h, true),
            // Any other aggregate (Range, LazyList, Set/Bag/Mix, ...) is wrapped
            // in a `Scalar` container so it becomes a single non-flattening
            // element in list context (mirrors the `.item` method form in
            // `dispatch_core_math.rs` — `for item(1..3)` must see ONE element).
            other => Value::scalar(Value::from_repr(other)),
        }
    }

    /// ADR-0040: itemize a value about to be stored into a real `Array`/
    /// `Hash` element, for the value kinds whose "one-item-ness" is actually
    /// observable there — `Array`, `List` (both the `ValueView::Array`
    /// variant, discriminated by `ArrayKind`), `Hash`, `Seq`, and every
    /// `Range` shape. Every other kind (`Int`, `Str`, `Pair`, `Set`, `Bag`,
    /// `Mix`, `Nil`, `Bool`, instances, …) is returned unchanged.
    ///
    /// `Value::item()` itself would ALSO wrap those other kinds in a
    /// `Scalar` box — behaviorally a no-op (the renderer/flattening
    /// chokepoints already treat a Scalar-wrapped scalar identically to the
    /// bare value: `docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md`
    /// §1.4/§2), but calling it unconditionally on every stored element
    /// would heap-allocate a `Box` for every plain `Int`/`Str` array/hash
    /// element — a real cost on this hot per-element store path. Gating on
    /// kind here keeps the fix to the value kinds the ADR actually measured.
    pub fn itemize_for_element_store(self) -> Value {
        match self.view() {
            ValueView::Array(..)
            | ValueView::Hash(_)
            | ValueView::Seq(_)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => self.item(),
            _ => self,
        }
    }

    /// ADR-0040 slice 2: would [`Value::itemize_for_element_store`] actually
    /// change this value? The construction-site hooks (list-assign into
    /// `@a`/`%h`, real-container literal construction) scan a whole element
    /// vector with this *before* touching it, so the overwhelmingly common
    /// cases — a flat array of scalars, and `my @a = @b` where `@b`'s
    /// elements a previous store already itemized — keep sharing the source
    /// `Gc` with no rebuild (ADR-0040 §5.2).
    ///
    /// `Shaped`/`Lazy` arrays are excluded because `ArrayKind::itemize()` is
    /// a no-op on them, so `itemize_for_element_store` would return them
    /// unchanged anyway.
    pub fn needs_element_itemization(&self) -> bool {
        match self.view() {
            ValueView::Array(_, kind) => matches!(kind, ArrayKind::List | ArrayKind::Array),
            ValueView::Hash(_) => !self.hash_is_itemized(),
            ValueView::Seq(_)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => true,
            _ => false,
        }
    }

    /// The inverse of [`Value::itemize_for_element_store`], for the few
    /// readers that hand out an element's *value* rather than its container.
    /// Measured on raku: `Array.List` decontainerizes (`@c.List[0].VAR.^name`
    /// is `Array`), while `Array.list` keeps the containers
    /// (`@c.list[0].VAR.^name` is `Scalar`) — see ADR-0040 §8.
    pub fn deitemize_element(self) -> Value {
        match self.view() {
            ValueView::Array(items, kind) if kind.is_itemized() => {
                Value::array_with_kind(items.clone(), kind.decontainerize())
            }
            ValueView::Hash(_) if self.hash_is_itemized() => self.with_hash_itemized(false),
            ValueView::Scalar(inner) => (*inner).clone(),
            _ => self,
        }
    }

    /// ADR-0040 slice 3: the discriminator, stated once. Are this container's
    /// elements `Scalar` containers of their own?
    ///
    /// Raku's model is that a real, mutable `Array`/`Hash` stores each element
    /// in a `Scalar` container, while a `List`/`Seq`/`Range` stores the values
    /// themselves. The *representation* consequence of that (one item in list
    /// context, a `$` in `.raku`) is what slices 1-2 put at the element store;
    /// this is the same fact seen from the *reflection* side, which is the one
    /// place a bare `Int` element still needs the container kind to answer:
    ///
    /// ```text
    /// my @c = 1, (1,2), [3,4];   @c[0..2]>>.VAR>>.^name  is  Scalar Scalar Scalar
    /// my @l := 1, (1,2), [3,4];  @l[0..2]>>.VAR>>.^name  is  Int    List   Array
    /// ```
    ///
    /// `Shaped` and `Lazy` are real `Array` kinds (`my @a[2;2]`, `my @a = ^Inf`)
    /// and so answer `true`, even though `ArrayKind::itemize()` is a no-op on
    /// them. `ItemArray`/`ItemList` are `$[…]`/`$(…)` — the itemization
    /// describes how the aggregate behaves as somebody else's element and says
    /// nothing about its own elements, so they answer the same as the kind they
    /// decontainerize to.
    pub fn elements_are_containers(&self) -> bool {
        match self.view() {
            ValueView::Array(_, kind) => matches!(
                kind,
                ArrayKind::Array | ArrayKind::Shaped | ArrayKind::Lazy | ArrayKind::ItemArray
            ),
            ValueView::Hash(_) => true,
            ValueView::Scalar(inner) => inner.elements_are_containers(),
            _ => false,
        }
    }

    /// Read through a `ContainerRef` or explicit `.VAR` container view and apply
    /// `f` to the inner value WITHOUT cloning it.
    pub fn with_deref<R>(&self, f: impl FnOnce(&Value) -> R) -> R {
        match self.view() {
            ValueView::ContainerRef(arc) | ValueView::ContainerView(arc) => f(&arc.lock().unwrap()),
            _ => f(self),
        }
    }

    /// Read through a `ContainerRef`, returning an owned clone of the inner value.
    /// Non-ContainerRef values are cloned as-is. Use [`Value::with_deref`] instead
    /// when you only need to read the inner value (it avoids the clone).
    pub fn deref_container(&self) -> Value {
        self.with_deref(|inner| {
            if matches!(inner.view(), ValueView::HashEntryRef { .. }) {
                inner.hash_entry_read()
            } else {
                inner.clone()
            }
        })
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
        if let ValueView::ContainerRef(arc) = self.view() {
            return arc.lock().unwrap().clone();
        }
        self
    }

    /// Store `val` through a `ContainerRef` cell. If the cell currently holds a
    /// `HashEntryRef` deferred token (a boxed `\target` bound to a
    /// not-yet-existent hash key — e.g. the escape analysis boxed a captured
    /// sigilless param before its first write), first materialize the binding:
    /// this cell ITSELF is installed at the token's path (walk-creating any
    /// intermediate hashes), so the hash entry and every holder of the cell
    /// alias the same container from then on. A plain `clone_from` would
    /// overwrite the token and silently drop the hash alias.
    pub(crate) fn store_through_cell(
        arc: &crate::gc::Gc<crate::value::ContainerCell>,
        val: &Value,
    ) {
        let mut inner = arc.lock().unwrap();
        if matches!(inner.view(), ValueView::HashEntryRef { .. })
            && let Some(terminal) = inner.hash_entry_terminal()
        {
            terminal.insert(Value::ContainerRef(arc.clone()));
            inner.clone_from(val);
            return;
        }
        // A cell whose CONTENTS is itself a `ContainerRef` is a nested-cell
        // shape: a `:=` bind reused a shared cell but the alias's own
        // pre-bind storage slot (its ADR-0024 mainline/closure capture cell)
        // still merely CONTAINS that shared cell rather than BEING it (see
        // `news/2026-08/bind-alias-reverse-write-through-nested-cell.md`).
        // A later PLAIN VALUE write through the alias must write THROUGH to
        // the nested cell, not overwrite the wrapper -- otherwise the write
        // silently severs the alias link the bind established and the
        // source variable never observes it. Peel through any further
        // nesting recursively. A write of a fresh `ContainerRef` (`val`
        // itself is one) is a REBIND of this slot to a different cell -- that
        // legitimately replaces the wrapper's contents, so it falls through
        // to the plain `clone_from` below unchanged.
        if !matches!(val.view(), ValueView::ContainerRef(_))
            && let ValueView::ContainerRef(nested) = inner.view()
            && !crate::gc::Gc::ptr_eq(&nested, arc)
        {
            let nested = nested.clone();
            drop(inner);
            Self::store_through_cell(&nested, val);
            return;
        }
        inner.clone_from(val);
    }

    /// Assign a value into a `ContainerRef`.
    /// Returns `true` if the value was a ContainerRef and the assignment happened.
    pub fn assign_into_container(&self, new_val: Value) -> bool {
        if let ValueView::ContainerRef(arc) = self.view() {
            let mut inner = arc.lock().unwrap();
            *inner = new_val;
            true
        } else {
            false
        }
    }

    /// Create a new shared container holding this value.
    pub fn into_container_ref(self) -> Value {
        Value::ContainerRef(crate::gc::Gc::new(crate::value::ContainerCell::new(self)))
    }

    /// Assign `val` into an array/hash element `slot`. When the slot already
    /// holds a `ContainerRef` cell (a Phase 2 `:=`-bound element), write
    /// *through* the cell so every alias of that element observes the new
    /// value; otherwise replace the slot in place. This is the single element
    /// write chokepoint that keeps a bound element's alias live across writes.
    pub fn assign_element_slot(slot: &mut Value, val: Value) {
        if let ValueView::ContainerRef(cell) = slot.view() {
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

    /// Tag probe (never decodes `view()`): this runs on every `GetLocal`, and
    /// a `view()` on a lazy Match would materialize it just to see it is not
    /// a `ContainerRef`.
    pub fn is_container_ref(&self) -> bool {
        self.0.is_container_ref()
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
        match self.view() {
            ValueView::Hash(arc) => arc.get(key).cloned(),
            ValueView::Mixin(inner, _) => inner.hash_get_str(key),
            ValueView::Scalar(inner) => inner.hash_get_str(key),
            _ => None,
        }
    }

    /// Returns `None` if `self` is not a `Value::Hash`.
    pub fn hash_autovivify(&self, key: &str) -> Option<Value> {
        if let ValueView::Hash(arc) = self.view() {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
            if !data.map.contains_key(key) {
                let new_hash = Value::hash(HashMap::new());
                data.map.insert(key.to_string(), new_hash);
            }
            // The entry exists (created just above if missing): an EAGER token,
            // whose reads see through to the plain entry value (`is raw`
            // reduce lvalue descent).
            Some(Value::from_repr(ValueRepr::HashEntryRef {
                root: crate::value::EntryRoot::Hash(arc.clone()),
                path: vec![crate::value::EntryStep::Key(key.to_string())],
                eager: true,
            }))
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
        if let ValueView::Hash(arc) = self.view() {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
            // A typed hash's value constraint rides on the promoted cell (see
            // `array_slot_ref`).
            let value_type = data.value_type.clone();
            match data.map.get_mut(key) {
                Some(elem) => {
                    if let ValueView::ContainerRef(cell) = elem.view() {
                        return Some(Value::ContainerRef(cell.clone()));
                    }
                    if matches!(elem.view(), ValueView::Array(..) | ValueView::Hash(..)) {
                        return Some(elem.clone());
                    }
                    let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(
                        std::mem::replace(elem, Value::Nil),
                    ));
                    // See `array_slot_ref` for why the owner name starts as
                    // the bare sigil.
                    if let Some(tc) = value_type.as_deref() {
                        crate::value::register_element_constraint(&cell, tc, "%");
                    }
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
        if let ValueView::Hash(arc) = self.view() {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
            // A typed hash's value constraint rides on the promoted cell (see
            // `array_slot_ref`).
            let value_type = data.value_type.clone();
            match data.map.get_mut(key) {
                Some(elem) => {
                    if let ValueView::ContainerRef(cell) = elem.view() {
                        return Some(Value::ContainerRef(cell.clone()));
                    }
                    if !terminal
                        && matches!(elem.view(), ValueView::Array(..) | ValueView::Hash(..))
                    {
                        // Intermediate container: return the element value
                        // itself — it shares the inner Arc, so the eventual
                        // leaf promotion by the next index op lands in the
                        // physical map the entry points to (Stage 2: no
                        // `HashEntryRef` back-reference needed).
                        return Some(elem.clone());
                    }
                    let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(
                        std::mem::replace(elem, Value::Nil),
                    ));
                    // See `array_slot_ref` for why the owner name starts as
                    // the bare sigil.
                    if let Some(tc) = value_type.as_deref() {
                        crate::value::register_element_constraint(&cell, tc, "%");
                    }
                    *elem = Value::ContainerRef(cell.clone());
                    Some(Value::ContainerRef(cell))
                }
                None => Some(Value::from_repr(ValueRepr::HashEntryRef {
                    root: crate::value::EntryRoot::Hash(arc.clone()),
                    path: vec![crate::value::EntryStep::Key(key.to_string())],
                    eager: false,
                })),
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
        if let ValueView::Hash(arc) = self.view() {
            // SAFETY: aliased in-place mutation of a shared container; see
            // `gc_contents_mut`. No borrow into the map is live across the write.
            let data = unsafe { crate::value::gc_contents_mut(&arc) };
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
        let eager = match self.view() {
            ValueView::HashEntryRef { eager, .. } => eager,
            _ => return self.clone(),
        };
        // A path whose intermediate levels are missing (or are not the
        // container kind their step addresses) has no terminal yet — the
        // deferred bind reads as `Any` without creating anything.
        let Some(terminal) = self.hash_entry_locate() else {
            return Value::Package(crate::symbol::Symbol::intern("Any"));
        };
        // An unconnected slot reads as what an unwritten slot of that container
        // holds: `Any` for a hash entry, the element hole (`Int`, an
        // `is default(...)` value) for an array index past the end.
        let Some(entry) = terminal.peek() else {
            return terminal.unwritten_read();
        };
        match entry.view() {
            ValueView::ContainerRef(cell) => cell.lock().unwrap_or_else(|e| e.into_inner()).clone(),
            _ if eager => entry.clone(),
            _ => terminal.unwritten_read(),
        }
    }
}
