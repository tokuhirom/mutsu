//! Coercing a finite value into a real `Array` (the list-assign / `.Array`
//! tail) and the element itemization every real `Array` carries. Pure, so it
//! lives in `value` (#10779); `runtime::utils::coerce_to_array` adds the
//! unbounded-range case (a lazy `@` array) in front of it.

use crate::value::shaped_array::mark_shaped_array;
use crate::value::to_list::{value_to_list, walk_list_candidates};
use crate::value::{ArrayKind, Value, ValueView};

/// Maximum number of elements eagerly pre-populated when a genuinely
/// *infinite* i64 `Range` (`b == i64::MAX`, e.g. `^Inf`, `1..*`) is bound
/// into a `Lazy`-kind Array, a slurpy (`*@`) parameter, or the RHS/index set
/// of a slice assignment — the initial window materialized before further
/// elements are reified on demand. This must NEVER be applied to a *finite*
/// range: a finite range has a real, known bound and always expands to it in
/// full (see `todo/tickets/finite-range-assign-truncates-at-100k.md` — a
/// prior bug applied this cap unconditionally, silently truncating finite
/// assignments above 100k elements).
///
/// This single constant replaces what used to be three independent
/// same-valued literals (`coerce_to_array`'s `MAX_ARRAY_EXPAND`,
/// `assignment_rhs_values`/`slice_indices_from_index`'s
/// `MAX_ASSIGN_SLICE_EXPAND`, and `flatten_into_slurpy`'s
/// `MAX_SLURPY_RANGE_EXPAND`) — all three capped the exact same case
/// (an infinite i64 Range) with the same value, so keeping them as separate
/// numbers was a maintenance hazard, not a deliberate difference.
///
/// Deliberately NOT unified with `value::to_list::MAX_RANGE_EXPAND`: that constant
/// bounds a full, non-lazy, one-shot materialization (`.List`/`.Array`
/// coercion, `map`/`grep` over a range) where a larger allowance is
/// reasonable because the result is not retained as an on-demand `Lazy`
/// array.
pub(crate) const MAX_LAZY_RANGE_PREFIX: i64 = 100_000;

/// ADR-0040 slice 2 (the Array half): itemize every element of a
/// freshly-constructed REAL `Array`. A `List`/`Seq`/`Lazy` result is left
/// alone — a `List` literal's elements are *not* containers (§1.6), which is
/// what keeps `((1,2),(3,4))[0]` a bare `(1, 2)`.
///
/// Scan-then-rebuild-only-if-needed (§5.2): the scan is a cheap discriminant
/// test per element and the `Gc` keeps being shared whenever nothing needs
/// itemizing, so the common `my @a = @b` path costs a refcount bump exactly
/// as before. Once the whole construction surface itemizes, a re-assignment
/// of an already-itemized array is itself a no-op, so the rebuild is paid at
/// most once per aggregate, at the point it first enters a real container.
pub(crate) fn itemize_real_array_elements(mut value: Value) -> Value {
    let needs = match value.view() {
        ValueView::Array(items, ArrayKind::Array | ArrayKind::Shaped | ArrayKind::ItemArray) => {
            items.iter().any(Value::needs_element_itemization)
        }
        _ => false,
    };
    if !needs {
        return value;
    }
    value.with_array_mut(|items, _kind| {
        let data = crate::gc::Gc::make_mut(items);
        for item in data.live_mut() {
            if item.needs_element_itemization() {
                *item = item.clone().itemize_for_element_store();
            }
        }
    });
    value
}

/// The mirror of [`itemize_real_array_elements`], for the one container that
/// must NOT carry the property: the list-destructuring desugar's synthetic
/// staging temp, which models the RHS `List` rather than a user `Array`. A
/// `List`'s elements are values, not containers, so this temp must not carry
/// Array element itemization (see `Interpreter::itemize_elements_for_var_assign`
/// and ADR-0079 §1.1). Same scan-then-rebuild-only-if-needed shape.
pub(crate) fn deitemize_real_array_elements(mut value: Value) -> Value {
    let needs = match value.view() {
        ValueView::Array(items, ArrayKind::Array | ArrayKind::Shaped | ArrayKind::ItemArray) => {
            items.iter().any(|v| {
                matches!(v.view(), ValueView::Array(_, k) if k.is_itemized())
                    || matches!(v.view(), ValueView::Scalar(_))
                    || (matches!(v.view(), ValueView::Hash(_)) && v.hash_is_itemized())
            })
        }
        _ => false,
    };
    if !needs {
        return value;
    }
    value.with_array_mut(|items, _kind| {
        let data = crate::gc::Gc::make_mut(items);
        for item in data.live_mut() {
            *item = item.clone().deitemize_element();
        }
    });
    value
}
/// [`coerce_to_array`](crate::runtime::utils::coerce_to_array) for a value
/// that is not an unbounded range: build the real `Array` and itemize its
/// elements.
pub(crate) fn coerce_finite_to_array(value: Value) -> Value {
    itemize_real_array_elements(coerce_to_array_inner(value))
}

fn coerce_to_array_inner(value: Value) -> Value {
    fn metadata_shape_for_items(
        items: &crate::gc::Gc<crate::value::ArrayData>,
    ) -> Option<Vec<usize>> {
        items.shape.as_deref().map(<[usize]>::to_vec)
    }

    match value.view() {
        ValueView::Array(items, kind) => {
            // Assigning an array to an `@` variable snapshots element VALUES
            // (Raku `=` semantics). A `:=`-bound element is a shared
            // `ContainerRef` cell (Phase 2); decontainerize it on copy so a
            // later write through the bound source does not leak into the copy.
            // Only rebuild when a cell is actually present (common path keeps
            // sharing the Arc, so there is no per-assignment cost). Nil
            // elements are left alone here — this coercion is type-blind, and
            // the assignment sites convert them to the element default (Any,
            // or the typed default via coerce_typed_array_elements).
            let items = if items
                .iter()
                .any(|v| matches!(v.view(), ValueView::ContainerRef(_)))
            {
                crate::gc::Gc::new(items.iter().map(|v| v.deref_container()).collect())
            } else {
                items.clone()
            };
            if kind.is_itemized() {
                // Itemized arrays (from `$` scalar containers) are treated as
                // a single item when assigned to an `@` variable.
                Value::real_array(vec![Value::array_with_kind(items, kind)])
            } else if kind == ArrayKind::Shaped {
                Value::array_with_kind(items, kind)
            } else if let Some(shape) = metadata_shape_for_items(&items) {
                let value = Value::array_with_kind(items, ArrayKind::Shaped);
                mark_shaped_array(&value, Some(&shape));
                value
            } else {
                Value::array_with_kind(items, ArrayKind::Array)
            }
        }
        ValueView::Nil => Value::real_array(vec![Value::package(crate::symbol::wk::any())]),
        ValueView::Range(a, b) => {
            if b == i64::MAX {
                // Infinite range — mark as lazy, capped to
                // MAX_LAZY_RANGE_PREFIX (the array is reified on demand
                // from here; see ArrayKind::Lazy / force_lazy_list_vm_n).
                let end = b.min(a.saturating_add(MAX_LAZY_RANGE_PREFIX));
                Value::array_with_kind(
                    crate::gc::Gc::new((a..=end).map(Value::int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                // Finite range: no cap. The bound is real, so materialize it
                // in full (matches raku, which has no built-in size limit here).
                Value::real_array((a..=b).map(Value::int).collect())
            }
        }
        ValueView::RangeExcl(a, b) => {
            if b == i64::MAX {
                let end = b.min(a.saturating_add(MAX_LAZY_RANGE_PREFIX));
                Value::array_with_kind(
                    crate::gc::Gc::new((a..end).map(Value::int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                Value::real_array((a..b).map(Value::int).collect())
            }
        }
        ValueView::RangeExclStart(a, b) => {
            if b == i64::MAX {
                let end = b.min(a.saturating_add(MAX_LAZY_RANGE_PREFIX));
                Value::array_with_kind(
                    crate::gc::Gc::new((a + 1..=end).map(Value::int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                Value::real_array((a + 1..=b).map(Value::int).collect())
            }
        }
        ValueView::RangeExclBoth(a, b) => {
            if b == i64::MAX {
                let end = b.min(a.saturating_add(MAX_LAZY_RANGE_PREFIX));
                Value::array_with_kind(
                    crate::gc::Gc::new((a + 1..end).map(Value::int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                Value::real_array((a + 1..b).map(Value::int).collect())
            }
        }
        ValueView::GenericRange { start, end, .. }
            if matches!(start.as_ref().view(), ValueView::Str(_))
                && matches!(end.as_ref().view(), ValueView::Str(_)) =>
        {
            Value::real_array(value_to_list(&value))
        }
        ValueView::GenericRange { start, end, .. } => {
            // An infinite numeric range is a lazy list (it cannot be fully
            // materialized): mark the resulting array `Lazy` so native typed
            // arrays reject it (`X::Cannot::Lazy`) and `.elems` stays lazy. This
            // covers a right-infinite end (`0e0..Inf`), a `-Inf`/`NaN` start
            // (`-Inf..0e0`, `NaN..NaN`), and a `Whatever` start (`*..1`). An
            // empty range (start strictly past end, e.g. `Inf..0`) is finite.
            let start_f = match start.as_ref().view() {
                ValueView::Whatever | ValueView::HyperWhatever => f64::NEG_INFINITY,
                _ => start.to_f64(),
            };
            let end_f = match end.as_ref().view() {
                ValueView::Whatever | ValueView::HyperWhatever => f64::INFINITY,
                _ => end.to_f64(),
            };
            // Not infinite when the start strictly exceeds the end (empty
            // range). A NaN endpoint is unordered (`partial_cmp` is `None`), so
            // a NaN range counts as infinite.
            let empty = matches!(
                start_f.partial_cmp(&end_f),
                Some(std::cmp::Ordering::Greater)
            );
            let infinite = (!start_f.is_finite() || !end_f.is_finite()) && !empty;
            if infinite {
                Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(value_to_list(&value))),
                    ArrayKind::Lazy,
                )
            } else {
                Value::real_array(value_to_list(&value))
            }
        }
        ValueView::Slip(_) | ValueView::Seq(_) | ValueView::HyperSeq(_) | ValueView::RaceSeq(_) => {
            let items = value_to_list(&value);
            let items = &items[..];
            // Like the `Array` arm: assigning to an `@` variable snapshots
            // element VALUES, so decontainerize any shared `ContainerRef` cells the
            // Seq carries (e.g. `my @g = @a.grep(...)`, whose Seq references @a's
            // rw slots) so a later write through the copy does not leak into the
            // source. Only rebuild when a cell is actually present. (Nil
            // elements are handled by the assignment sites, not here.)
            let arc = if items
                .iter()
                .any(|v| matches!(v.view(), ValueView::ContainerRef(_)))
            {
                crate::value::Value::array_arc(items.iter().map(|v| v.deref_container()).collect())
            } else {
                crate::value::Value::array_arc(items.to_vec())
            };
            Value::array_with_kind(arc, ArrayKind::Array)
        }
        ValueView::LazyList(_) => value.clone(),
        // A bare Hash assigned to an @-var flattens into its pairs
        // (`my @a = %h`). An *itemized* hash (`my $h = %(...); my @a = $h`)
        // stays a single element — `ItemizeVar` now tags it with the per-holder
        // per-holder Hash itemization flag rather than the old `Scalar(Hash)`
        // wrapper, so it must be handled here too (the `Scalar(inner)` arm below
        // covers the Set/Bag/Mix itemized forms, which still use the wrapper).
        ValueView::Hash(map) if !value.hash_is_itemized() => {
            let pairs: Vec<Value> = map
                .iter()
                .map(|(k, v)| map.typed_pair(k, v.clone()))
                .collect();
            Value::real_array(pairs)
        }
        ValueView::Hash(_) => Value::real_array(vec![value.clone()]),
        // A scalar holding a Hash/Set/Bag/Mix (itemized by `ItemizeVar` as
        // `Scalar(container)`) stays a single element, but unwrap the Scalar so
        // `@a[0]` is the bare container (preserving its `.gist`/`.raku`/type),
        // rather than an opaque `$(...)`-wrapped value.
        ValueView::Scalar(inner)
            if matches!(
                inner.view(),
                ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
            ) =>
        {
            Value::real_array(vec![inner.clone()])
        }
        // Set/Bag/Mix assigned to an @-var flatten into their `key => weight`
        // pairs in list context, exactly like a Hash (Raku: `my @a = set(1,2,3)`
        // yields three `* => True` pairs, so `@a.elems == 3`). This mirrors
        // `value_to_list`. (Note: an array *literal* `[set(...)]` does NOT
        // flatten — that path is handled separately in `exec_make_array_op`.)
        // ADR-0021 I2: data-minted pairs default positional.
        ValueView::Set(items, _) => Value::real_array(
            items
                .iter()
                .map(|s| Value::value_pair(Value::str(s.clone()), Value::TRUE))
                .collect(),
        ),
        ValueView::Bag(items, _) => Value::real_array(
            items
                .iter()
                .map(|(k, v)| {
                    Value::value_pair(Value::str(k.clone()), Value::from_bigint(v.clone()))
                })
                .collect(),
        ),
        ValueView::Mix(items, _) => Value::real_array(
            items
                .iter()
                .map(|(k, v)| {
                    Value::value_pair(Value::str(k.clone()), crate::value::mix_weight_to_value(*v))
                })
                .collect(),
        ),
        // A package Stash is Associative, so assigning it to an array
        // materializes its symbol table as key/value pairs rather than
        // retaining the Stash as one opaque instance.
        ValueView::Instance { class_name, .. }
            if crate::value::types::is_stash_class_name(&class_name.resolve()) =>
        {
            Value::real_array(value_to_list(&value))
        }
        // A WalkList assigned to an `@` variable flattens to its candidate
        // closures, so `my @cands = $x.WALK(...)` yields the per-level candidates.
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name.resolve() == "WalkList" => match walk_list_candidates(&attributes) {
            Some(cands) => Value::real_array(cands),
            None => Value::real_array(vec![value.clone()]),
        },
        _ => Value::real_array(vec![value.clone()]),
    }
}
