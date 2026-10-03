//! `flat`, `.tree` and the infinite-range test: pure list-shape operations, so they
//! live in `value` (#10779); `builtins` re-exports them.

use crate::value::{ArrayKind, Value, ValueView};

pub(crate) fn is_infinite_range(value: &Value) -> bool {
    match value.view() {
        ValueView::Range(_, end)
        | ValueView::RangeExcl(_, end)
        | ValueView::RangeExclStart(_, end)
        | ValueView::RangeExclBoth(_, end) => end == i64::MAX,
        ValueView::GenericRange { end, .. } => match end.as_ref().view() {
            ValueView::HyperWhatever => true,
            ValueView::Num(n) => n.is_infinite() && n.is_sign_positive(),
            ValueView::Rat(n, d) => d == 0 && n > 0,
            ValueView::FatRat(n, d) => d == 0 && n > 0,
            _ => {
                let n = end.as_ref().to_f64();
                n.is_infinite() && n.is_sign_positive()
            }
        },
        _ => false,
    }
}

/// Recursively flatten a value for `flat()`.
///
/// `flatten_arrays`: when true, `ArrayKind::Array` values are flattened
/// (their elements are exposed). Elements coming from a List/Seq context
/// pass `true`; elements coming from inside an Array pass `false`
/// (because `[...]` itemizes its contents in Raku).
/// Flatten `v` into `out`. Single shared `flat` helper for both the pure
/// `native_function("flat", ..)` path and the interpreter's `builtin_flat`
/// (which wraps its args in a `List` and calls this). `flatten_arrays`
/// distinguishes List context (flatten `[...]` children one level) from Array
/// context (preserve nested `[...]`), matching raku — the interpreter's old
/// `flat_into` lacked this and over-flattened nested arrays in `flat(a, b, c)`.
/// Strip ONE level of itemization from a `flat` operand. Raku's `flat`
/// un-itemizes its (top-level) argument — `$(1,2,3).flat` yields `(1,2,3)`,
/// `$[1,2,3].flat` yields `(1,2,3)` — and then flattens. Nested itemized items
/// (e.g. `$[1,2]` *inside* the operand) stay single; that is handled by
/// `flat_val` (which keeps `kind.is_itemized()` arrays opaque). Only the
/// immediate operand is de-itemized here.
pub(crate) fn deitemize_flat_operand(v: &Value) -> Value {
    match v.view() {
        ValueView::Array(items, kind) if kind.is_itemized() => {
            Value::array_with_kind(items.clone(), kind.decontainerize())
        }
        ValueView::Scalar(inner) => (*inner).clone(),
        // A top-level hash operand of `flat` flattens to its pairs
        // (`%h.flat` / `$hash.flat` / `flat(%h)` all yield the pairs). This is
        // done ONLY here, for the top-level operand — a hash that is merely an
        // element of a multi-arg flat list (`flat $hash, <a b>`) stays single
        // (mutsu cannot tell a scalar-held `$hash` from a `%h` at the value
        // level, so flat_val must not flatten hashes in nested positions). The
        // returned `List` lets flat_val descend into the pairs.
        ValueView::Hash(h) => {
            // `hash_with_data` builds a NON-itemized holder, so the pairs spill
            // (a `$`-itemized operand hit the `ValueView::Scalar` arm above, or
            // — for the flag form — flattens here as its top-level operand).
            let pairs = crate::value::to_list::value_to_list(&Value::hash_with_data(h.clone()));
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(pairs)),
                crate::value::ArrayKind::List,
            )
        }
        _ => v.clone(),
    }
}

pub(crate) fn flat_val(v: &Value, out: &mut Vec<Value>, flatten_arrays: bool) {
    match v.view() {
        // Array.Seq may carry an element's Scalar container (ADR-0045). `flat`
        // is a list consumer, so it observes the contained value just as it
        // did before the Seq producer started preserving that container.
        ValueView::ContainerRef(_) => flat_val(&v.deref_container(), out, flatten_arrays),
        // Slips always flatten — slipping is their whole purpose.
        ValueView::Slip(items) => {
            for item in items.iter() {
                flat_val(item, out, true);
            }
        }
        // Lists, Seqs, and hyper sequences flatten in list context; as the element of an
        // itemizing container (a `[...]` Array element lives in a Scalar
        // container) they stay single and itemized: `(1, @a, 5).flat` with
        // `my @a = 2, (3, 4)` yields `(1 2 (3 4) 5)`, and
        // `[(1, 2), $(3, 4)].flat.raku` is `($(1, 2), $(3, 4)).Seq`.
        ValueView::Array(items, ArrayKind::List) => {
            if flatten_arrays {
                for item in items.iter() {
                    flat_val(item, out, true);
                }
            } else {
                out.push(Value::array_with_kind(items.clone(), ArrayKind::ItemList));
            }
        }
        // A `$`-held Seq (`my $t = (1, 2).Seq`) is itemized (its view is
        // `ItemSeq`/`ItemList`) and stays one element, like an itemized List:
        // `join("-", $t)` is `1 2`, `(1, $t).flat.elems` is 2.
        ValueView::Seq(body)
            if matches!(
                body.view(),
                crate::value::SeqView::ItemSeq | crate::value::SeqView::ItemList
            ) =>
        {
            out.push(v.clone())
        }
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            if flatten_arrays {
                for item in items.iter() {
                    flat_val(item, out, true);
                }
            } else {
                out.push(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(items.to_vec())),
                    ArrayKind::ItemList,
                ));
            }
        }
        // Real Arrays ([...]): flatten if flag is set. Children get
        // flatten_arrays=false because [...] itemizes its elements. As an
        // element of an itemizing container the array itself stays single,
        // itemized (`[1, [2, 3]].flat.raku` is `(1, $[2, 3]).Seq`).
        ValueView::Array(items, ArrayKind::Array) => {
            if flatten_arrays {
                for item in items.iter() {
                    flat_val(item, out, false);
                }
            } else {
                out.push(Value::array_with_kind(items.clone(), ArrayKind::ItemArray));
            }
        }
        // A shaped (native) array (`array[int].new(:shape(10), …)`) flattens its
        // elements like a list — `flat 0, @native, 11` splices the native array's
        // values in. Multi-dim shaped arrays descend into their leaves.
        ValueView::Array(items, ArrayKind::Shaped) => {
            for item in items.iter() {
                flat_val(item, out, true);
            }
        }
        // Itemized containers — don't descend
        ValueView::Array(_, kind) if kind.is_itemized() => out.push(v.clone()),
        // A genuinely-lazy `@`-array (`my @a = 1..*`) stays opaque so it renders
        // as `...`/`[...]` (e.g. in `"@a[]"` interpolation) rather than spilling
        // its capped backing prefix. Already-realized lazy lists flatten their
        // cached items; an un-forced lazy list stays opaque (we don't force here).
        ValueView::LazyList(ll) => {
            if ll.in_array_context() && ll.is_genuinely_lazy() {
                out.push(v.clone());
            } else if let Some(cached) = ll.cache.lock().unwrap().clone() {
                for item in &cached {
                    flat_val(item, out, flatten_arrays);
                }
            } else {
                out.push(v.clone());
            }
        }
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => {
            if flatten_arrays {
                out.extend(crate::value::to_list::value_to_list(v));
            } else {
                out.push(v.clone());
            }
        }
        // A bare (non-itemized) Hash in LIST context flattens into its pairs —
        // `flat %new, @new` splices the hash's pairs in (an empty hash
        // contributes nothing). An itemized hash (`$h`), or a hash stored as a
        // real-Array element (`@aoh.flat` — flatten_arrays=false marks the
        // itemizing container), stays a single element.
        ValueView::Hash(map) if flatten_arrays && !v.hash_is_itemized() => {
            for (k, val) in map.iter() {
                out.push(map.typed_pair(k, val.clone()));
            }
        }
        // A role-mixed value flattens according to its INNER value's shape: a
        // container inner (Array/Seq/Slip/Range/Hash) flattens through
        // exactly as the bare container would (`(%h does R).flat` spills
        // `%h`'s pairs — role composition on the container does not carry
        // over to its individual elements) — but a NON-container inner (Int,
        // a type object, Str, ...) is not itself flatten-worthy, so the WHOLE
        // mixin stays the flat element rather than the bare unwrapped inner:
        // raku preserves `(1 but R).flat[0].^name` as `Int+{R}`, and a punned
        // role's type object flattens to itself
        // (`Iterable.^pun.flat[0] === Iterable.^pun`, verified against raku).
        ValueView::Mixin(inner, _) => match inner.as_ref().view() {
            ValueView::Slip(_)
            | ValueView::Array(..)
            | ValueView::Seq(_)
            | ValueView::HyperSeq(_)
            | ValueView::RaceSeq(_)
            | ValueView::LazyList(_)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. }
            | ValueView::Hash(_) => flat_val(inner, out, flatten_arrays),
            _ => out.push(v.clone()),
        },
        _ => out.push(v.clone()),
    }
}

/// Recursively apply `.tree` to nested arrays.
/// `.tree` on an Iterable is `$(self.map(*.tree).Seq)`: every node it descends
/// into becomes an *itemized* Seq, so `(1, (2, 3)).tree.raku` is
/// `$((1, $((2, 3).Seq)).Seq)`. A non-Iterable is its own tree.
///
/// `depth` is how many levels still get treed — `.tree(1)` itemizes only the
/// top node and leaves its children as they are, `.tree(0)` is the identity,
/// and `.tree` / `.tree(*)` are `usize::MAX`. Shared with the `.tree(...)`
/// argument forms in `runtime::…::dispatch_tree`.
/// Cost: O(1) on an Array (each level is a lazy Seq, `ListGen::Tree`, that
/// trees an element as it is pulled); O(e) on a Seq/Slip/Hash/Range, e =
/// elements of that level.
pub(crate) fn tree_to_depth(v: &Value, depth: usize) -> Value {
    if depth == 0 {
        return v.clone();
    }
    let children = match v.view() {
        ValueView::Array(..) => {
            return Value::scalar(Value::seq_list_gen(
                crate::value::ListGen::tree(v.clone(), depth),
                false,
            ));
        }
        ValueView::Seq(inner) => inner.to_vec(),
        ValueView::Slip(inner) => inner.to_vec(),
        ValueView::Hash(_)
        | ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..) => crate::value::to_list::value_to_list(v),
        _ => return v.clone(),
    };
    let treed = children
        .iter()
        .map(|c| tree_to_depth(c, depth - 1))
        .collect();
    Value::scalar(Value::seq(treed))
}
