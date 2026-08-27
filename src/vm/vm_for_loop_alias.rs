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
//! *value*).

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
    /// A direct `@`-array source, keyed by iteration index.
    ArrayIndex(String),
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
    /// **Which sources alias**: a direct, real, mutable, plain `Array`, or a
    /// `%h.values` over a real mutable `Hash`. Derived producers
    /// (`.kv`/`.pairs`/`.reverse`/`.sort`, and the `$`-tagged `for @$s` shape)
    /// are excluded until slice 4 makes those producers hand out element
    /// containers themselves; a multi-parameter loop binds through the
    /// bind-prefix `Stmt::Assign`s rather than this native bind site, so it is
    /// excluded too.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn plan_for_element_alias(
        &self,
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
        // binding is not the element; `.kv` and `.reverse` are derived orders
        // whose producers do not hand out containers yet (slice 4).
        if !aliasing_param || spec.loop_var_wraps_element || spec.kv_mode || container_reversed {
            return ForElementAlias::None;
        }
        let Some(source) = container_binding else {
            return ForElementAlias::None;
        };
        if source.starts_with('@') {
            // `@a.values` is an identity-ordered derived producer, but it is
            // slice 4's to route; keep it on the writeback so the two array
            // producers convert together.
            if spec.values_mode || !self.for_source_is_aliasable(source) {
                return ForElementAlias::None;
            }
            let arr = self
                .get_env_with_main_alias(source)
                .map(|v| v.deref_container());
            let (len, first) = match arr.as_ref().map(Value::view) {
                Some(ValueView::Array(data, _)) => (data.len(), data.items().first().cloned()),
                _ => return ForElementAlias::None,
            };
            if items.len() != len || !Self::items_are_source_elements(items, first) {
                return ForElementAlias::None;
            }
            return ForElementAlias::ArrayIndex(source.to_string());
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
        plan: &ForElementAlias,
        idx: usize,
    ) -> Option<Value> {
        match plan {
            ForElementAlias::None => None,
            ForElementAlias::ArrayIndex(source) => {
                let arr = self.get_env_with_main_alias(source)?.deref_container();
                if !Self::array_is_aliasable(&arr, Some(idx)) {
                    return None;
                }
                arr.array_slot_ref(idx, true)
            }
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
                hash.hash_slot_ref(key, true)
            }
        }
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

    /// Whether a `for` loop's tagged `@`-source is a real, mutable, plain
    /// `Array` whose elements may be promoted.
    ///
    /// The carve-outs (ADR-0045 §5 Q5) are deliberate and stay until slice 5
    /// decides otherwise:
    ///
    /// * a **shaped** array (`my @a[2;3]`) carries its dimensions in
    ///   `ArrayData::shape` / `ArrayKind::Shaped`, and the writeback path
    ///   deliberately preserves that metadata by cloning the whole `ArrayData`
    ///   (see `vm_loop_writeback.rs`'s "clone the original ArrayData" comment);
    ///   `array_slot_ref` has no such provision.
    /// * a **native-backed** array (`array[int]`, ADR-0015 P3b / ADR-0030)
    ///   keeps its elements in a packed `NativeBacking` payload, which cannot
    ///   hold a `ContainerRef` at all.
    /// * a **lazy** array must not be forced by a promotion.
    ///
    /// `t/cas-shaped-and-for-loop.t` and row 26 of `t/for-loop-element-alias.t`
    /// are the pins for the shaped case.
    pub(super) fn for_source_is_aliasable(&self, source: &str) -> bool {
        self.get_env_with_main_alias(source)
            .is_some_and(|raw| Self::array_is_aliasable(&raw.deref_container(), None))
    }

    /// The hash sibling of [`Self::for_source_is_aliasable`].
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
