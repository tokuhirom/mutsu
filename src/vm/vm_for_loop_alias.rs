//! ADR-0045: a `for` loop parameter binds the element *container*.
//!
//! Raku binds a `for` parameter to the item the iterator yields, and when the
//! source is a real mutable `Array`/`Hash` that item **is** the element's
//! `Scalar` container. The binding is therefore an alias with the lifetime of
//! the binding, not of the loop body: a closure or `start` block that outlives
//! the iteration still writes through, a read through the alias sees a write
//! made to the element by anyone else, and a direct `@a[i] = v` in the body is
//! not reverted afterwards.
//!
//! mutsu used to bind a plain value clone and copy it back once per iteration
//! (`write_back_for_rw_param` / `write_back_for_topic_item` /
//! `write_back_hash_value_item`), rebuilding the entire backing container to
//! change one element — which is both the cause of the divergence classes in
//! ADR-0045 §1.3 and the reason a mutating loop was O(n²) (§1.5).
//!
//! This module holds the **discriminator**: which loop/source/parameter
//! combinations may have their elements promoted to first-class containers via
//! [`Value::array_slot_ref`] / [`Value::hash_slot_ref`], the primitives
//! ADR-0036 shipped and that `:=`-bound elements already exercise daily. The
//! bind site itself lives in `vm_for_loop_body.rs`.
//!
//! Slices landed here: 1 (direct array source, writable aliasing parameter),
//! 2 (`%h.values`), 3 (the implicit topic; the plain named parameter is a pure
//! deletion with no promotion at all — rows 45/46 pin that `-> $v` binds the
//! *value*), and the `$`-tagged deref'd-container shape of slice 4
//! ([`ForElementAlias::ArrayValue`], row 39).
//!
//! **The other half of slice 4 is deliberately NOT here.** `.kv`, `.reverse`,
//! `.sort` and `.values` alias through their *producers*
//! (`vm_element_producers.rs`, ADR-0036 slice 3's routing): the producer hands
//! out the element container, so the item the loop binds already carries its
//! own identity and this discriminator has nothing left to decide. That is why
//! `kv_mode` / `container_reversed` / `values_mode` still return
//! [`ForElementAlias::None`] below — not "not yet supported", but "already
//! handled one layer up". The bind site recognises such an item with
//! `binding_carries_element_cell` and retires the writeback for it just the
//! same (`vm_for_loop_body.rs`).
//!
//! Slice 6's sweep (2026-09-01) re-measured the whole of ADR-0045 §1.3 against
//! raku: all 45 rows agree.

use super::vm_control_ops::ForLoopSpec;
use super::*;

/// How a `for` loop's binding aliases its source's elements, decided once per
/// loop. Carrying the resolved source name (and, for a hash, the key order the
/// producer yielded) makes the per-iteration promotion a lookup rather than a
/// re-derivation.
pub(super) enum ForElementAlias {
    /// No promotion: the loop keeps the plain value bind and whatever
    /// writeback it had.
    None,
    /// A direct `@`-array source, keyed by iteration index. The source is named
    /// rather than captured so that each iteration re-resolves it: a body that
    /// assigns the array wholesale (`@a = 7, 8`) must have the remaining
    /// iterations alias the container it left behind.
    ArrayIndex(String),
    /// A `$`-tagged deref'd-container source (`for @$s` / `for $s.list`), keyed
    /// by iteration index into the array **resolved once, at loop entry**.
    ///
    /// Re-resolving this shape by name per iteration is wrong, not merely
    /// slower: `for @$s` derefs `$s` once to pick the array it iterates, so a
    /// later write to `$s` cannot redirect the loop. The name is very often
    /// `$_` (`encode($_) for @$_` is the idiomatic recursive structure walk),
    /// and any nested loop in the body rebinds the topic -- so a by-name
    /// re-resolution aliased into whatever container the *inner* loop was
    /// walking. CBOR::Simple's Capture encoding hit exactly that: the second
    /// element of `[$list, $hash]` came back as the inner list's `[1]`.
    ArrayValue(Value),
    /// A plain `$`-scalar source (`for $a -> \x`, `-> $x is rw`, `<->`). A
    /// scalar is not a container OF elements: the loop yields one item, and the
    /// container that item lives in is the variable's own. Carries the
    /// compiler-baked slot so the promotion writes the cell back where the
    /// variable is authoritative.
    ///
    /// This is what retires `store_loop_source_var` for the scalar case: the
    /// parameter binds the variable's container, so a write through it goes
    /// through the ordinary container chokepoint -- and is type-checked there,
    /// which the raw slot write never was (`my SmallInt $a; for $a -> \x
    /// { x = 1000 }` silently stored 1000 where rakudo throws).
    ScalarVar(String, Option<u32>),
    /// A `%h.values` source, keyed by the key order captured before the loop —
    /// the same order the materialized `.values` list was built from, so
    /// position `idx` names the key whose value the loop is binding.
    HashValue(String, Vec<String>),
}

impl ForElementAlias {
    pub(super) fn is_active(&self) -> bool {
        !matches!(self, Self::None)
    }
}

impl Interpreter {
    /// Decide once per loop whether this `for` may bind element containers, and
    /// against which source.
    ///
    /// **Which parameters alias** (ADR-0045 §1.1, measured against raku, not
    /// assumed): `is rw`, `<->`, sigilless `\v` — all of which set
    /// `spec.do_writeback` — and the **implicit topic**. The plain named
    /// parameter `-> $v` is a read-only binding of the *value* and is absent
    /// here on purpose: `for @a -> $v { @a[0] = 9; say $v }` prints `1` in
    /// raku, and the deferred-read form prints `1 2` (rows 45/46).
    ///
    /// **Which sources alias here**: a direct, real, mutable, plain `Array`, the
    /// `$`-tagged deref'd-container shape (`for @$s`), or a `%h.values` over a
    /// real mutable `Hash`.
    ///
    /// The order-derived producers (`.kv`, `.reverse`, `.sort`, `.values`) are
    /// excluded on purpose and permanently: slice 4 routed them at the
    /// *producer*, so the item they yield already IS the element container and
    /// there is no index for this discriminator to reconstruct. `.pairs` /
    /// `.antipairs` yield a `Pair` *wrapping* the element, so the binding is
    /// not the element at all. A multi-parameter loop binds through the
    /// bind-prefix `Stmt::Assign`s rather than this native bind site, so it too
    /// aliases via the producer (row 16).
    #[allow(clippy::too_many_arguments)]
    pub(super) fn plan_for_element_alias(
        &self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        container_binding: Option<&str>,
        container_reversed: bool,
        arity: usize,
        param_name: Option<&str>,
        writes_back_topic: bool,
        topic_readonly: bool,
        hash_keys: Option<&[String]>,
        items: &[Value],
    ) -> ForElementAlias {
        // A `@`/`%`/`&`-sigil parameter binds the element's *container*, not a
        // scalar slot: `for @m -> @row { @row.push(9) }` mutates `@m` through
        // the shared `Gc` with no cell involved (rows 32/33). Promoting such an
        // element would hand the parameter a `ContainerRef` where a
        // Positional/Associative is expected, so element promotion is for
        // scalar bindings only — `$v`, `\v`, and the topic.
        let scalar_binding = !param_name.is_some_and(|n| n.starts_with(['@', '%', '&']));
        let aliasing_param = scalar_binding
            && if spec.do_writeback {
                // `is rw` / `<->` / `\v`, bound natively as a single parameter
                // (or as the topic, for a `<->` block with no signature).
                arity == 1
                    && spec.multi_param_names.is_empty()
                    && (param_name.is_some() || writes_back_topic)
            } else {
                // The implicit topic. An immutable Mix/Set/Bag source, or a
                // source whose items are provably bare values, marks the topic
                // read-only — there is no container behind those items to alias.
                writes_back_topic && !topic_readonly
            };
        // `.pairs`/`.antipairs` yield a `Pair` *wrapping* the element, so the
        // binding is not the element. `.kv` and `.reverse` are derived orders
        // that alias at the producer instead (slice 4) — declining here is what
        // lets the producer's cell reach the bind site unmodified.
        if !aliasing_param || spec.loop_var_wraps_element || spec.kv_mode || container_reversed {
            return ForElementAlias::None;
        }
        let Some(source) = container_binding else {
            return ForElementAlias::None;
        };
        // `@a`, and `@a.list`, which re-tags the array itself: both iterate a
        // real array's elements in order, so iteration position names element
        // position. The `$`-tagged deref'd-container shape (`for @$s` /
        // `for $s.list`, ADR-0045 row 39) iterates the scalar's inner array,
        // which is the same one-for-one relationship -- it differs only in that
        // the array is captured here rather than re-resolved per iteration (see
        // `ForElementAlias::ArrayValue`).
        if source.starts_with('@') || source.starts_with('$') {
            // `@a.values` is an identity-ordered derived producer, and its
            // routing lives with `.reverse`/`.sort` at the producer (slice 4),
            // so it is declined here for the same reason they are.
            if spec.values_mode {
                return ForElementAlias::None;
            }
            let Some(arr) = self.aliasable_source_array(code, source, items) else {
                return ForElementAlias::None;
            };
            let arr = Some(arr);
            return match arr {
                Some(arr) if source.starts_with('$') => ForElementAlias::ArrayValue(arr),
                _ => ForElementAlias::ArrayIndex(source.to_string()),
            };
        }
        // A plain `$`-scalar source. `TagContainerRef` spells such a source with
        // NO sigil (`for $a` tags `"a"`, while the deref'd-container shape
        // `for @$s` tags `"$s"` and a direct array tags `"@a"`), which is
        // exactly the discriminator: a bare name is a scalar variable, and a
        // scalar is not a container OF elements. The loop yields one item and
        // the container that item lives in is the variable's own.
        //
        // `values_mode` is excluded here for the reason spelled out just below:
        // a mutable QuantHash's `for $b.values` also tags a bare scalar name,
        // and its items are WEIGHTS, not the variable. (`kv_mode`,
        // `loop_var_wraps_element` and `container_reversed` were already
        // declined above.)
        if !source.starts_with(['@', '%', '$', '&']) && !spec.values_mode && items.len() == 1 {
            // ... and only when the loop really iterates the VARIABLE, not a
            // derived producer on it. `for $pair.value` tags the same bare name
            // and also yields one item, but that item is the pair's VALUE:
            // aliasing the variable there replaced the whole `Pair` with it
            // (`roast/S04-blocks-and-statements/pointy-rw.t`). The item-is-the-
            // source test is the scalar twin of `items_are_source_elements`.
            let current = spec
                .source_container_local
                .and_then(|slot| self.locals.get(slot as usize))
                .map(|v| v.deref_container())
                .filter(|v| !v.is_nil())
                .or_else(|| self.get_env_with_main_alias(source));
            if current.is_some_and(|v| Self::loop_var_unchanged(&items[0], &v)) {
                return ForElementAlias::ScalarVar(source.to_string(), spec.source_container_local);
            }
            return ForElementAlias::None;
        }
        // A mutable QuantHash (`for $b.values`) binds to a scalar, so it never
        // reaches here: its `container_binding` carries no `%` sigil. That is
        // the ADR-0045 §2.4 carve-out — a BagHash/MixHash *weight* is not a
        // stored element container, and `.value = 0` REMOVES the key, so it is
        // a different operation and keeps `write_back_quanthash_*`.
        if source.starts_with('%') && spec.values_mode {
            let Some(keys) = hash_keys.filter(|k| !k.is_empty()) else {
                return ForElementAlias::None;
            };
            if !self.for_source_is_aliasable_hash(source) {
                return ForElementAlias::None;
            }
            let first = self
                .get_env_with_main_alias(source)
                .map(|v| v.deref_container())
                .and_then(|h| keys.first().and_then(|k| h.hash_get_str(k)));
            if items.len() != keys.len() || !Self::items_are_source_elements(items, first) {
                return ForElementAlias::None;
            }
            return ForElementAlias::HashValue(source.to_string(), keys.to_vec());
        }
        ForElementAlias::None
    }

    /// The `Array` behind a tagged `for` source when this loop may promote its
    /// elements: a real, mutable, plain `Array` (not shaped, lazy, or
    /// native-backed) whose elements the loop iterates **one-for-one**.
    ///
    /// Factored out of [`Self::plan_for_element_alias`] so the multi-parameter
    /// plan below applies exactly the same discriminator; the two differ only in
    /// which positions they promote, never in which sources they accept.
    fn aliasable_source_array(
        &self,
        code: &CompiledCode,
        source: &str,
        items: &[Value],
    ) -> Option<Value> {
        let arr = self.resolve_for_source_array(code, source)?;
        let (len, first) = match arr.view() {
            ValueView::Array(data, kind)
                if !matches!(
                    kind,
                    crate::value::ArrayKind::Shaped | crate::value::ArrayKind::Lazy
                ) && data.shape.is_none()
                    && data.native_storage_node().is_none() =>
            {
                (data.len(), data.items().first().cloned())
            }
            _ => return None,
        };
        if items.len() != len || !Self::items_are_source_elements(items, first) {
            return None;
        }
        Some(arr)
    }

    /// ADR-0045 slice 6: promote a **multi-parameter** rw loop's source elements
    /// before the item vector is chunked, so the chunk the bind-prefix
    /// statements read from carries the SOURCE's element containers.
    ///
    /// A multi-parameter loop does not bind at the native bind site — it binds
    /// through `build_for_bind_stmts`'s `MarkBind` declarations, which read
    /// `_[i]` out of a chunk built by `items.chunks(arity)`. That chunk is a
    /// fresh `Array`, so promoting *it* would alias the temporary, not `@a`.
    /// Promoting the items first makes `array_slot_ref`'s idempotence do the
    /// rest: the chunk holds the source's cells, the bind aliases them, and
    /// `binding_carries_element_cell` retires the writeback for the iteration.
    ///
    /// Before this, the multi-parameter arm reached raku's *write* answer by
    /// accident: the retained writeback stored the chunk's own cell **into** the
    /// source element, so a later write through the parameter did land in `@a` —
    /// but only after the iteration ended. The read direction stayed stale
    /// (`for @a -> $p is rw, $q is rw { @a[1] = 55; say $q }` printed the old
    /// value), a body that rebound the source wholesale was still clobbered by
    /// the snapshot (§1.3 class 3), and every iteration still rebuilt the whole
    /// backing `ArrayData`, so a mutating multi-parameter loop stayed O(n²)
    /// after slice 1 had removed the quadratic from every other shape.
    ///
    /// Only the positions whose parameter actually aliases are promoted.
    /// `rw_param_names` is positionally aligned with the chunk and holds `""`
    /// for a slot that is not genuinely rw, which is the same distinction
    /// `build_for_bind_stmts` makes when it decides between a raw `MarkBind`
    /// declaration and a coercing `Stmt::Assign`. Promoting a non-rw slot would
    /// make a plain `-> $x` read-alias, which rows 45/46 forbid.
    pub(super) fn promote_multi_param_elements(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        container_binding: Option<&str>,
        container_reversed: bool,
        items: &[Value],
    ) -> Option<Vec<Value>> {
        if !spec.chunks_items()
            || !spec.do_writeback
            || spec.kv_mode
            || spec.values_mode
            || spec.loop_var_wraps_element
            || container_reversed
        {
            return None;
        }
        let arity = spec.arity.max(1) as usize;
        // Nothing to align the chunk positions against, so nothing to promote.
        if spec.rw_param_names.len() != arity {
            return None;
        }
        // An `@`/`%`/`&`-sigil parameter binds a Positional/Associative, not a
        // scalar slot — the same carve-out `plan_for_element_alias` makes.
        let promotable: Vec<bool> = spec
            .rw_param_names
            .iter()
            .enumerate()
            .map(|(i, n)| {
                !n.is_empty()
                    && !spec
                        .multi_param_names
                        .get(i)
                        .is_some_and(|m| m.starts_with(['@', '%', '&']))
            })
            .collect();
        if !promotable.iter().any(|p| *p) {
            return None;
        }
        // Only a direct `@`-array source. The `$`-tagged deref'd shape and the
        // hash shapes keep the single-parameter routing; a multi-parameter loop
        // over them is rare and its chunk mapping is not the same one-for-one.
        let source = container_binding.filter(|s| s.starts_with('@'))?;
        let arr = self.aliasable_source_array(code, source, items)?;
        let mut out = items.to_vec();
        let mut promoted_any = false;
        for (i, slot) in out.iter_mut().enumerate() {
            if !promotable[i % arity] {
                continue;
            }
            // A `Proxy` element mediates its own STORE; a cell bound around one
            // would take a plain write instead of calling it (§5 Q6).
            if matches!(slot.view(), ValueView::Proxy { .. }) {
                continue;
            }
            if !Self::array_is_aliasable(&arr, Some(i)) {
                continue;
            }
            if let Some(cell) = arr.array_slot_ref(i, true) {
                *slot = Self::name_element_owner(cell, source);
                promoted_any = true;
            }
        }
        promoted_any.then_some(out)
    }

    /// The element container this iteration's binding should alias, or `None`
    /// when nothing can be promoted — the source stopped being an aliasable
    /// container, the index/key is gone (a body that shrank the source out from
    /// under the loop), or the key was never there. The caller then keeps the
    /// plain value bind *and* the writeback that bind depends on: retiring the
    /// writeback per **loop** rather than per **iteration** silently drops such
    /// an iteration's write.
    ///
    /// The source is resolved fresh on every call, on purpose: a body that
    /// reassigns it wholesale (`@a = 7, 8`) must have the remaining iterations
    /// alias the container it left behind, not the one the loop started with.
    ///
    /// Promotion is idempotent — `array_slot_ref`/`hash_slot_ref` return an
    /// existing cell rather than allocating a second one — so re-looping the
    /// same container costs nothing after the first pass.
    pub(super) fn for_element_alias(
        &mut self,
        code: &CompiledCode,
        plan: &ForElementAlias,
        idx: usize,
    ) -> Option<Value> {
        match plan {
            ForElementAlias::None => None,
            ForElementAlias::ArrayIndex(source) => {
                let arr = self.resolve_for_source_array(code, source)?;
                if !Self::array_is_aliasable(&arr, Some(idx)) {
                    return None;
                }
                Some(Self::name_element_owner(
                    arr.array_slot_ref(idx, true)?,
                    source,
                ))
            }
            ForElementAlias::ArrayValue(arr) => {
                if !Self::array_is_aliasable(arr, Some(idx)) {
                    return None;
                }
                arr.array_slot_ref(idx, true)
            }
            ForElementAlias::ScalarVar(source, slot) => self.scalar_var_container(source, *slot),
            ForElementAlias::HashValue(source, keys) => {
                let key = keys.get(idx)?;
                let hash = self.get_env_with_main_alias(source)?.deref_container();
                if !Self::hash_is_aliasable(&hash) {
                    return None;
                }
                // Only an EXISTING key is an element container. `hash_slot_ref`
                // hands back a lazy `HashEntryRef` token for a missing one,
                // which is a path, not an alias — and this loop's key came from
                // the map, so a miss means the body deleted it.
                hash.hash_get_str(key)?;
                Some(Self::name_element_owner(
                    hash.hash_slot_ref(key, true)?,
                    source,
                ))
            }
        }
    }

    /// The shared `ContainerRef` cell a `$`-scalar loop source binds through,
    /// promoting the variable to one if it is not already.
    ///
    /// The freshly minted cell inherits the variable's declared `of`-type, the
    /// same rule the `:=` bind promotion applies (ADR-0042: the constraint
    /// belongs to the container, not to a name) -- from here on the cell is what
    /// every write to either the loop parameter or the variable reaches, so a
    /// name-keyed check would never run again.
    fn scalar_var_container(&mut self, source: &str, slot: Option<u32>) -> Option<Value> {
        let bare = source.strip_prefix('$').unwrap_or(source);
        let raw = slot
            .and_then(|s| self.locals.get(s as usize).cloned())
            .filter(|v| !v.is_nil())
            .or_else(|| self.get_env_with_main_alias(bare))?;
        if raw.is_container_ref() {
            return Some(raw);
        }
        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(raw));
        if let Some(ty) = self.var_type_constraint(bare) {
            // The tag spells a scalar source without its sigil; the message
            // spells the variable the way the source does.
            let display = format!("${bare}");
            crate::value::register_container_constraint_named(&cell, &ty, &display);
        }
        let container = Value::container_ref(cell);
        // The variable IS the cell now, so a direct write to it and a write
        // through the loop parameter reach the same container.
        if let Some(s) = slot
            && let Some(dst) = self.locals.get_mut(s as usize)
        {
            *dst = container.clone();
        }
        self.set_env_with_main_alias(bare, container.clone());
        Some(container)
    }

    /// Tell a promoted element cell which container it belongs to, so an
    /// element type-check failure blames `@a` rather than the bare `@` the
    /// promotion primitive seeds (ADR-0036 slice 4). A no-op for an untyped
    /// container, whose cell carries no constraint to name.
    fn name_element_owner(item: Value, source: &str) -> Value {
        if let ValueView::ContainerRef(cell) = item.view() {
            crate::value::retag_element_owner(&cell, source);
        }
        item
    }

    /// Loop-entry guard: does this loop iterate the tagged source's elements
    /// **one-for-one**, so that iteration position `idx` names element `idx`?
    ///
    /// The source tag names a container, but it does not by itself prove that.
    /// `for @a,` is the counter-example: a trailing comma builds a **one**-
    /// element list whose single item is the whole `@a`, yet the tag still says
    /// `@a`, so indexing by iteration position would alias `@a[0]` to a binding
    /// that holds all of `@a` (`t/for-modifier-trailing-comma.t`). The old
    /// writeback survived this only because its unchanged-value guard happened
    /// to make it a no-op.
    ///
    /// This is checked **once, before the body has run**, and deliberately not
    /// per iteration: the item vector is a snapshot taken at loop entry, so once
    /// the body has mutated an element the snapshot no longer matches it — and
    /// that is exactly the case an alias must keep serving, not decline (rows
    /// 04/21/38, where the body writes the source directly). A per-iteration
    /// version of this test silently re-broke all three.
    fn items_are_source_elements(items: &[Value], first_element: Option<Value>) -> bool {
        match (items.first(), first_element) {
            // An empty loop never binds anything, so the plan is moot; say yes
            // rather than special-casing the caller.
            (None, _) => true,
            (Some(first_item), Some(elem)) => Self::loop_var_unchanged(first_item, &elem),
            (Some(_), None) => false,
        }
    }

    /// The backing `Array` value behind a `for` loop's tagged source, for both
    /// tag shapes the compiler emits:
    ///
    /// * `@a` — the array variable itself (also what `@a.list` tags).
    /// * `$s` — the SIGILED deref'd-container tag for `for @$s` / `for $s.list`,
    ///   where the loop iterates the *scalar's inner array*. A plain scalar
    ///   local is slot-only (never mirrored to `env`), so the slot is consulted
    ///   first and `env` covers globals and `:=`-bound cells; the scalar may
    ///   hold the array itemized (`Scalar(Array)`) or behind a cell, so both
    ///   wrappers are stripped.
    ///
    /// Returns the `Array` value itself (sharing its `Gc`), so a promotion
    /// through it lands in the container every holder observes.
    fn resolve_for_source_array(&self, code: &CompiledCode, source: &str) -> Option<Value> {
        if let Some(bare) = source.strip_prefix('$') {
            let raw = self
                .find_local_slot(code, bare)
                .and_then(|s| self.locals.get(s))
                .filter(|v| !v.is_nil())
                .cloned()
                .or_else(|| self.get_env_with_main_alias(bare))?;
            return Some(raw.deref_container().into_descalarized());
        }
        Some(self.get_env_with_main_alias(source)?.deref_container())
    }

    /// Whether a `for` loop's tagged `%`-source is a real mutable `Hash`
    /// whose elements may be promoted.
    fn for_source_is_aliasable_hash(&self, source: &str) -> bool {
        self.get_env_with_main_alias(source)
            .is_some_and(|raw| Self::hash_is_aliasable(&raw.deref_container()))
    }

    /// Shared shape test for the array entry points. `idx` additionally
    /// requires that index to exist today.
    ///
    /// The kind test is a **denial list**, not an allow list. Only `Shaped` and
    /// `Lazy` are genuinely unable to hand out an element container; every other
    /// kind is an ordinary `ArrayData` whose elements promote normally. An allow
    /// list of `Array | List` looked equivalent and was not: ADR-0040 stores an
    /// `Array` element *itemized*, so the very common
    /// `for @m -> @row { for @row <-> $x { … } }` binds `@row` to an
    /// `ItemArray`, which an allow list silently dropped back onto the
    /// writeback — and that writeback rebuilds a fresh `ArrayData`, severing
    /// `@row` from the `@m` element it was sharing (row 37).
    fn array_is_aliasable(v: &Value, idx: Option<usize>) -> bool {
        match v.view() {
            ValueView::Array(data, kind) => {
                !matches!(
                    kind,
                    crate::value::ArrayKind::Shaped | crate::value::ArrayKind::Lazy
                ) && data.shape.is_none()
                    && data.native_storage_node().is_none()
                    && idx.is_none_or(|i| i < data.len())
            }
            _ => false,
        }
    }

    /// A real mutable `Hash`. An immutable `Map` is excluded: its elements are
    /// not assignable, so promoting one would offer an alias that must not
    /// exist (slice 5 owns turning that into a bind-time error).
    fn hash_is_aliasable(v: &Value) -> bool {
        match v.view() {
            ValueView::Hash(data) => data.declared_type.as_deref() != Some("Map"),
            _ => false,
        }
    }
}
