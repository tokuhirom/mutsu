//! Container-aware element producers — ADR-0036 slice 3 and ADR-0045 slice 4.
//!
//! An `Array`/`Hash` element is a `Scalar` container, and every construct that
//! hands an element *out* hands out **that container**, not a copy of what it
//! holds. That is why all of these are true in raku:
//!
//! ```text
//! my @a = <A B>; my $p = @a.pairs[0]; @a[0] = "Q"; $p.value      # Q
//! my @a = <A B>; for @a.pairs -> $p { $p.value = "y" }; @a       # [y y]
//! my @a = 10,20; for @a.reverse -> $v is rw { $v = $v + 1 }; @a  # [11 21]
//! ```
//!
//! The pure-value fast path (`builtins/methods_0arg/`) cannot do this: it is
//! value-in/value-out by construction, receiving the *decontainerized* `&[Value]`
//! with neither the invocant's container identity nor the ability to mutate it
//! (ADR-0036 §1.5). Promotion needs both, so the container-aware producers live
//! here, at the VM method-dispatch layer, with the pure-value implementation
//! kept as the fallback for every receiver this declines.
//!
//! **Why this also settles ADR-0045 slice 4's index bookkeeping.** Once a
//! producer hands out element containers, a `for` loop over its output needs no
//! index reconstruction at all — the item it binds *is* the alias, in whatever
//! order the producer chose. That is what makes `container_reversed` /
//! `total_items` deletable rather than fixable: `.reverse` and `.sort` reorder
//! the items, and any scheme that assumes "item *i* came from index *i*" is
//! wrong twice over for them.

use super::*;

/// The methods that hand an element out and therefore hand out its container.
/// `.reverse`/`.sort` are array-only (a Hash has no order to reverse).
///
/// **`.antipairs` is deliberately absent.** It puts the element in the Pair's
/// *key* position, and a Pair's key is never a container in raku — only its
/// value is. Measured: `my @a = <A B>; my $p = @a.antipairs[0]; @a[0] = "Q";
/// $p.key` is `A`, not `Q`, and `$p.key.VAR.^name` is `Str`, not `Scalar`. So
/// `.antipairs` keeps the snapshot producer; routing it here would have made
/// its key track later writes, which is a divergence, not a fix. (ADR-0036 §4
/// lists it with `.pairs`/`.kv`; that grouping is corrected here by
/// measurement.)
///
/// **`.pairs` is here, and what unblocked it was work done elsewhere.**
/// It was implemented and backed out on 2026-08-27: routing it makes every
/// consumer that reads a Pair's value *as data* see a `ContainerRef`, and
/// because `.pairs` promotes the source's elements in place, the exposure is
/// not "consumers of the `.pairs` result" but "consumers of any container a
/// producer has run over". Five leaks were measured then. Re-measured on
/// 2026-09-01 against a full local `make roast` + the whole `t/` suite, **four
/// of the five were already gone** — closed by the intervening pair work (rows
/// 10/11/12, `.WHAT` on a cell, the immutable-value guard) rather than by
/// anything `.pairs` did. The one that remained was not a `.pairs` bug at all:
/// `pair_weight`/`mix_pair_weight` (`builtins/quanthash_coerce.rs`) read a
/// weight without decontainerizing, so a cell fell through every numeric arm to
/// the truthy `_` fallback and became `1`. That was already wrong on `main` for
/// a plain `key => $x` pair (`my $x = 3; my %z is BagHash; %z = ((a => $x),)`
/// gave `BagHash(a)`), so the fix is a general one, pinned in
/// `t/pairs-element-container.t`.
///
/// The rule that came out of it, and the one to apply when routing anything
/// else through here: **a Pair's value is read as DATA everywhere except an
/// lvalue `.value =` and `.VAR`** — so any site that type-tests or numifies one
/// must `deref_container()` first. `.values`/`.reverse`/`.sort` never needed the
/// rule because they hand out a *flat list* of cells and list consumers
/// decontainerize; it is specifically the Pair wrapper that carries a cell into
/// code reading it structurally.
///
/// **`.kv` is here, and it took a change to the CONSUMER to get it here.** A
/// `.kv` loop is a *multi-parameter* loop, and a multi-parameter loop does not
/// bind at the native bind site — it binds through bind-prefix statements
/// (`build_for_bind_stmts`, `compiler/mod.rs`). Those used to be plain
/// `Stmt::Assign`s reading the chunk slot through the ordinary element
/// chokepoint, which **decontainerizes**, so a cell handed out here arrived at
/// `$v` as a plain value and the write was lost — while the writeback that used
/// to carry it had been retired for the iteration precisely because the chunk
/// carried a cell. A *writable* scalar multi-parameter now binds raw
/// (`Stmt::MarkBind` + a declaration, the shape `@`/`%`-sigil multi-params
/// already had), and `array_slot_ref`'s idempotence makes that bind alias the
/// SOURCE element rather than the temporary chunk. ADR-0045 row 16.
///
/// The `.kv` output is a **flat** `key, cell, key, cell, …` list, because the
/// loop chunks it by two. Only the value slot is a container: a key is never one
/// in raku, the same asymmetry that keeps `.antipairs` off this path.
const ELEMENT_PRODUCERS: [&str; 6] = ["Seq", "values", "reverse", "sort", "kv", "pairs"];

impl Interpreter {
    /// Read the current weight behind a carrier Pair. Its stored value remains
    /// suitable for ordinary data consumers, while `.value` must be live for a
    /// following compound assignment.
    pub(super) fn quanthash_weight_pair_value(&self, pair: &Value) -> Option<Value> {
        let source = pair.quanthash_weight_source()?;
        let key = match pair.view() {
            ValueView::Pair(key, _) => Value::str(key.clone()),
            ValueView::ValuePair(key, _) => key.clone(),
            _ => return None,
        };
        let (store_key, _) = crate::runtime::utils::quanthash_elem_entry(&key);
        match self
            .get_env_with_main_alias(&source)
            .as_ref()
            .map(Value::view)
        {
            Some(ValueView::Set(set, true)) => {
                Some(Value::truth(set.elements.contains(&store_key)))
            }
            Some(ValueView::Bag(bag, true)) => Some(
                bag.counts
                    .get(&store_key)
                    .map(|weight| Value::from_bigint(weight.clone()))
                    .unwrap_or_else(|| Value::int(0)),
            ),
            Some(ValueView::Mix(mix, true)) => Some(
                mix.weights
                    .get(&store_key)
                    .map(|weight| crate::value::mix_weight_to_value(*weight))
                    .unwrap_or_else(|| Value::int(0)),
            ),
            _ => None,
        }
    }

    /// Produce `.pairs`/`.values` for a mutable QuantHash. A QuantHash weight is not an
    /// element cell: assigning zero removes the key, so the pair carries the
    /// source binding as dedicated lvalue metadata instead.
    pub(super) fn try_quanthash_weight_pair_producer(
        &mut self,
        target: &Value,
        target_name: &str,
        method: &str,
        args: &[Value],
    ) -> Option<Value> {
        if !matches!(method, "pairs" | "values") || !args.is_empty() || target_name.is_empty() {
            return None;
        }
        let weights: Vec<(Value, Value)> = match target.view() {
            ValueView::Set(set, true) => set
                .iter()
                .map(|key| (set.typed_key(key), Value::TRUE))
                .collect(),
            ValueView::Bag(bag, true) => bag
                .iter()
                .map(|(key, weight)| (bag.typed_key(key), Value::from_bigint(weight.clone())))
                .collect(),
            ValueView::Mix(mix, true) => mix
                .iter()
                .map(|(key, weight)| {
                    (
                        mix.typed_key(key),
                        crate::value::mix_weight_to_value(*weight),
                    )
                })
                .collect(),
            _ => return None,
        };
        Some(Value::seq(
            weights
                .into_iter()
                .map(|(key, weight)| {
                    if method == "pairs" {
                        Value::quanthash_weight_pair(key, weight, target_name.to_string())
                    } else {
                        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(weight));
                        crate::value::register_quanthash_weight_ref(
                            &cell,
                            target_name.to_string(),
                            key,
                        );
                        Value::container_ref(cell)
                    }
                })
                .collect(),
        ))
    }

    /// Produce `method`'s result from `target`'s **element containers** instead
    /// of from clones, or `None` to let the ordinary pure-value producer run.
    ///
    /// Declines — and each decline is the ADR's decision, not an omission:
    ///
    /// * any method outside [`ELEMENT_PRODUCERS`], or any call with arguments
    ///   (`.sort(&by)` runs a user comparator; that is not this routing's job);
    /// * a receiver that is not a **mutable** container. A `List`, `Seq`,
    ///   `Range`, `Capture`, `Match` or immutable `Set`/`Bag`/`Mix` keeps the
    ///   snapshot producer, which is the whole of the immutability story
    ///   (ADR-0036 §2.2): a snapshot pair value is a bare item, so `.value = X`
    ///   on it reaches the existing read-only guard and dies with
    ///   `Cannot modify an immutable <T>` — exactly raku's answer for
    ///   `(1,2).pairs[0].value = 3`, with no new check needed;
    /// * a shaped, native-backed or lazy array, and an immutable `Map` — the
    ///   same carve-outs `vm_for_loop_alias.rs` documents;
    /// * a mutable `QuantHash` (`BagHash`/`MixHash`/`SetHash`), which is routed
    ///   by [`try_quanthash_weight_pair_producer`] because a weight has distinct
    ///   zero-removal semantics (ADR-0036 §5 Q2).
    pub(super) fn try_element_container_producer(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Value> {
        if !args.is_empty() || !ELEMENT_PRODUCERS.contains(&method) {
            return None;
        }
        match target.view() {
            ValueView::Array(..) => self.array_element_producer(target, method),
            ValueView::Hash(..) if !matches!(method, "reverse" | "sort") => {
                self.hash_element_producer(target, method)
            }
            _ => None,
        }
    }

    fn array_element_producer(&mut self, target: &Value, method: &str) -> Option<Value> {
        let len = match target.view() {
            ValueView::Array(data, kind) => {
                // Only a real, mutable, plain array. `List`/`ItemList` are
                // immutable sequences whose items must stay bare items.
                if !matches!(
                    kind,
                    crate::value::ArrayKind::Array
                        | crate::value::ArrayKind::ItemArray
                        | crate::value::ArrayKind::Shaped
                ) || data.native_storage_node().is_some()
                {
                    return None;
                }
                // A MULTI-dimensional shaped array keeps its leaves in nested
                // inner arrays, so they are not this array's own slots and
                // `array_slot_ref` would hand out the rows. `.pairs` over it is
                // keyed by index tuple and takes the recursive path below; the
                // other producers keep whatever they do today. A ONE-dimensional
                // shape stores its leaves flat, so it needs nothing special.
                if data.shape.as_ref().is_some_and(|s| s.len() > 1) {
                    return match method {
                        "pairs" => self.shaped_multidim_pairs(target),
                        _ => None,
                    };
                }
                data.len()
            }
            _ => return None,
        };
        // Promotion is in-place and idempotent (`array_slot_ref` returns an
        // existing cell rather than allocating a second one), so re-running a
        // producer over the same array costs nothing after the first pass.
        let cells: Vec<Value> = (0..len)
            .map(|i| target.array_slot_ref(i, true))
            .collect::<Option<_>>()?;
        Some(match method {
            // `.Seq` has the same element-producing contract as the derived
            // sequence methods below: it preserves an Array element's Scalar
            // container rather than snapshotting its current value. `.List`
            // deliberately remains outside this routing because it
            // decontainerizes Array elements.
            "Seq" => Value::seq_element_containers(cells),
            "values" => Value::seq_element_containers(cells),
            "pairs" => Value::seq_element_containers(
                cells
                    .into_iter()
                    .enumerate()
                    .map(|(i, c)| Value::value_pair(Value::int(i as i64), c))
                    .collect(),
            ),
            // A flat `index, cell, index, cell, ...` list -- the loop chunks it
            // by two, so the value slot of each chunk is the element's own
            // container and `-> $i, $v is rw` aliases it (ADR-0045 row 16).
            "kv" => Value::seq_element_containers(
                cells
                    .into_iter()
                    .enumerate()
                    .flat_map(|(i, c)| [Value::int(i as i64), c])
                    .collect(),
            ),
            "reverse" => {
                let mut cells = cells;
                cells.reverse();
                Value::seq_element_containers(cells)
            }
            "sort" => {
                // Sort the CELLS, ordered by what they hold. Carrying the cell
                // through the sort is what makes `for @a.sort -> $v is rw`
                // alias the right element: reconstructing an index from the
                // sorted *value* afterwards is ambiguous the moment two
                // elements compare equal.
                let mut keyed: Vec<(Value, Value)> = cells
                    .into_iter()
                    .map(|c| {
                        let plain = c.deref_container();
                        (c, plain)
                    })
                    .collect();
                keyed.sort_by(|a, b| crate::runtime::compare_values(&a.1, &b.1).cmp(&0));
                Value::seq_element_containers(keyed.into_iter().map(|(c, _)| c).collect())
            }
            _ => return None,
        })
    }

    /// `.pairs` over a multi-dimensional shaped array. Raku keys each pair by
    /// the full index *tuple* (`(0 1) => …`) and the value is the LEAF
    /// element's container — which lives in an inner array, not in `target`'s
    /// own slot vector, so the promotion has to walk down to it. The nested
    /// arrays share their `Gc` with the outer array's items, so promoting a
    /// leaf there is visible through `@a[0;1]` and through any other alias.
    ///
    /// Key shape and leaf order mirror the pure-value producer in
    /// `builtins/methods_0arg/collection.rs` (`shaped_array_indexed_leaves`),
    /// which is what this replaces for a mutable shaped array.
    fn shaped_multidim_pairs(&mut self, target: &Value) -> Option<Value> {
        let mut pairs = Vec::new();
        let mut indices = Vec::new();
        Self::collect_leaf_cell_pairs(target, &mut indices, &mut pairs)?;
        Some(Value::seq_element_containers(pairs))
    }

    fn collect_leaf_cell_pairs(
        node: &Value,
        indices: &mut Vec<i64>,
        out: &mut Vec<Value>,
    ) -> Option<()> {
        let ValueView::Array(items, _) = node.view() else {
            return None;
        };
        // Clone the children out before promoting: `array_slot_ref` mutates the
        // node's item vector in place, so no borrow into it may be live.
        let children: Vec<Value> = items.iter().cloned().collect();
        if children
            .iter()
            .any(|v| matches!(v.view(), ValueView::Array(..)))
        {
            for (i, child) in children.iter().enumerate() {
                indices.push(i as i64);
                let walked = Self::collect_leaf_cell_pairs(child, indices, out);
                indices.pop();
                walked?;
            }
        } else {
            for i in 0..children.len() {
                let cell = node.array_slot_ref(i, true)?;
                let mut key = indices.clone();
                key.push(i as i64);
                let key = if key.len() == 1 {
                    Value::int(key[0])
                } else {
                    Value::array(key.into_iter().map(Value::int).collect())
                };
                out.push(Value::value_pair(key, cell));
            }
        }
        Some(())
    }

    fn hash_element_producer(&mut self, target: &Value, method: &str) -> Option<Value> {
        // Key order is taken from the same `items.iter()` the pure-value
        // producer uses, so the container-aware path yields the same order.
        let keys: Vec<String> = match target.view() {
            ValueView::Hash(data) => {
                // An immutable `Map`'s elements are not assignable, so promoting
                // one would offer an alias that must not exist.
                if data.declared_type.as_deref() == Some("Map") {
                    return None;
                }
                data.keys().cloned().collect()
            }
            _ => return None,
        };
        // `.values`, `.kv` and `.pairs` reach here; `.reverse`/`.sort` are
        // array-only. `.kv` and `.pairs` share the `hash_typed_key` path below,
        // because both hand the key back to the program; `.values` does not.
        if !matches!(method, "values" | "kv" | "pairs") {
            return None;
        }
        let typed_keys = method != "values" && crate::runtime::utils::hash_uses_typed_keys(target);
        let mut out: Vec<Value> = Vec::with_capacity(keys.len() * 2);
        for k in &keys {
            let cell = target.hash_slot_ref(k, true)?;
            // A missing key hands back a lazy `HashEntryRef` path token rather
            // than an alias; these keys came from the map, so that cannot
            // happen — but decline rather than hand out a path if it ever does.
            if !matches!(cell.view(), ValueView::ContainerRef(_)) {
                return None;
            }
            if method == "pairs" {
                let key = if typed_keys {
                    crate::runtime::utils::hash_typed_key(target, k)
                } else {
                    Value::hash_key_decode(k)
                };
                out.push(Value::value_pair(key, cell));
                continue;
            }
            if method == "kv" {
                // The key is NOT a container -- only the value is (the same
                // asymmetry that keeps `.antipairs` off this path).
                out.push(if typed_keys {
                    crate::runtime::utils::hash_typed_key(target, k)
                } else {
                    Value::hash_key_decode(k)
                });
            }
            out.push(cell);
        }
        Some(Value::seq_element_containers(out))
    }
}
