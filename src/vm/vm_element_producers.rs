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
/// **`.pairs` is absent, and this one was implemented, measured, and backed
/// out.** Routing it makes every consumer that reads a Pair's value *as data*
/// see a `ContainerRef` — and because `.pairs` promotes the source's elements in
/// place, the exposure is not "consumers of the `.pairs` result" but "consumers
/// of any container a producer has run over". Five distinct leaks were measured
/// (`trans` type-testing the value, Hash-from-pairs aliasing two hashes,
/// BagHash-from-pairs collapsing every weight to 1, `.map({.key => .value})`
/// carrying the cell forward, `.antipairs` losing its key de-itemization), and
/// the pattern did not stop: `set_coerce.rs` and `coerce_containers.rs` alone
/// destructure a pair's value structurally in 15 places, with no accessor to
/// route. It needs a read chokepoint for a Pair's value, which conflicts with
/// ADR-0036 row 6 (`(@a[0]:p).value.VAR.^name` must be `Scalar`) and so wants
/// its own decision. Tracked in
/// `todo/deep/pairs-element-containers-leak-through-pair-value-consumers.md`.
/// `.values`/`.reverse`/`.sort` do not have the problem: they hand out a flat
/// list of cells, and list consumers decontainerize.
///
/// **`.kv` is absent for a different reason: the consumer, not the producer.**
/// `.kv` is bound as a *multi-parameter* loop (`-> $i, $v is rw`), and a
/// multi-parameter loop does not bind at the native bind site — it binds through
/// bind-prefix `Stmt::Assign`s that read the chunk element (`build_for_bind_stmts`,
/// `compiler/mod.rs`). That read goes through the ordinary element chokepoint,
/// which **decontainerizes**, so a cell handed out here arrives at `$v` as a
/// plain value and the write is lost — while the writeback that used to carry
/// it has been retired for the iteration precisely because the chunk carried a
/// cell. Routing `.kv` therefore needs a raw (non-decontainerizing) bind for an
/// rw scalar multi-parameter first; `@`/`%`-sigil multi-params already have one
/// (`Stmt::MarkBind`), so that is the shape to extend. Until then `.kv` keeps
/// the snapshot producer and its writeback, which is correct for the direct
/// write (`for @a.kv -> $i, $v is rw { $v += $i }`) and only loses the deferred
/// closure. Tracked in `todo/tickets/for-kv-multi-param-bind-decontainerizes.md`.
const ELEMENT_PRODUCERS: [&str; 4] = ["Seq", "values", "reverse", "sort"];

impl Interpreter {
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
    /// * a mutable `QuantHash` (`BagHash`/`MixHash`/`SetHash`), whose *weights*
    ///   are not stored element containers and whose `.value = 0` **removes**
    ///   the key. That is a genuinely different operation and keeps its
    ///   writeback arm (ADR-0036 §5 Q2, `t/for-pairs-value-quanthash-writeback.t`).
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
                    crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray
                ) || data.shape.is_some()
                    || data.native_storage_node().is_some()
                {
                    return None;
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
            "Seq" => Value::seq(cells),
            "values" => Value::seq(cells),
            "reverse" => {
                let mut cells = cells;
                cells.reverse();
                Value::seq(cells)
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
                Value::seq(keyed.into_iter().map(|(c, _)| c).collect())
            }
            _ => return None,
        })
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
        // Only `.values` reaches here today — `.pairs` is deferred (see the
        // `ELEMENT_PRODUCERS` doc) and `.reverse`/`.sort` are array-only — so
        // the key never has to be rebuilt. When `.pairs` returns, this is where
        // `hash_typed_key` / `hash_uses_typed_keys` come back with it.
        if method != "values" {
            return None;
        }
        let mut out: Vec<Value> = Vec::with_capacity(keys.len());
        for k in &keys {
            let cell = target.hash_slot_ref(k, true)?;
            // A missing key hands back a lazy `HashEntryRef` path token rather
            // than an alias; these keys came from the map, so that cannot
            // happen — but decline rather than hand out a path if it ever does.
            if !matches!(cell.view(), ValueView::ContainerRef(_)) {
                return None;
            }
            out.push(cell);
        }
        Some(Value::seq(out))
    }
}
