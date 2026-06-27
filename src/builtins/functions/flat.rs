#![allow(clippy::result_large_err)]
use crate::value::{ArrayKind, Value};

pub(crate) fn is_infinite_range(value: &Value) -> bool {
    match value {
        Value::Range(_, end)
        | Value::RangeExcl(_, end)
        | Value::RangeExclStart(_, end)
        | Value::RangeExclBoth(_, end) => *end == i64::MAX,
        Value::GenericRange { end, .. } => match end.as_ref() {
            Value::HyperWhatever => true,
            Value::Num(n) => n.is_infinite() && n.is_sign_positive(),
            Value::Rat(n, d) => *d == 0 && *n > 0,
            Value::FatRat(n, d) => *d == 0 && *n > 0,
            other => {
                let n = other.to_f64();
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
    match v {
        Value::Array(items, kind) if kind.is_itemized() => {
            Value::Array(items.clone(), kind.decontainerize())
        }
        Value::Scalar(inner) => (**inner).clone(),
        // A top-level hash operand of `flat` flattens to its pairs
        // (`%h.flat` / `$hash.flat` / `flat(%h)` all yield the pairs). This is
        // done ONLY here, for the top-level operand — a hash that is merely an
        // element of a multi-arg flat list (`flat $hash, <a b>`) stays single
        // (mutsu cannot tell a scalar-held `$hash` from a `%h` at the value
        // level, so flat_val must not flatten hashes in nested positions). The
        // returned `List` lets flat_val descend into the pairs.
        Value::Hash(h) => {
            let pairs = crate::runtime::utils::value_to_list(&Value::Hash(
                crate::value::Value::hash_arc_deitemized(h.clone()),
            ));
            Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(pairs)),
                crate::value::ArrayKind::List,
            )
        }
        _ => v.clone(),
    }
}

pub(crate) fn flat_val(v: &Value, out: &mut Vec<Value>, flatten_arrays: bool) {
    match v {
        // Lists, Seqs, and Slips are always flattened; their children
        // inherit flatten_arrays=true since Lists don't itemize.
        Value::Array(items, ArrayKind::List) => {
            for item in items.iter() {
                flat_val(item, out, true);
            }
        }
        Value::Seq(items) | Value::Slip(items) => {
            for item in items.iter() {
                flat_val(item, out, true);
            }
        }
        // Real Arrays ([...]): flatten if flag is set. Children get
        // flatten_arrays=false because [...] itemizes its elements.
        Value::Array(items, ArrayKind::Array) if flatten_arrays => {
            for item in items.iter() {
                flat_val(item, out, false);
            }
        }
        // A shaped (native) array (`array[int].new(:shape(10), …)`) flattens its
        // elements like a list — `flat 0, @native, 11` splices the native array's
        // values in. Multi-dim shaped arrays descend into their leaves.
        Value::Array(items, ArrayKind::Shaped) => {
            for item in items.iter() {
                flat_val(item, out, true);
            }
        }
        // Itemized containers — don't descend
        Value::Array(_, kind) if kind.is_itemized() => out.push(v.clone()),
        // A genuinely-lazy `@`-array (`my @a = 1..*`) stays opaque so it renders
        // as `...`/`[...]` (e.g. in `"@a[]"` interpolation) rather than spilling
        // its capped backing prefix. Already-realized lazy lists flatten their
        // cached items; an un-forced lazy list stays opaque (we don't force here).
        Value::LazyList(ll) => {
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
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => {
            out.extend(crate::runtime::utils::value_to_list(v));
        }
        other => out.push(other.clone()),
    }
}

/// Join `rest` with `sep`, flattening with `flat`/slurpy semantics (the single
/// shared `join` body for both `native_function("join", ..)` and the
/// interpreter's `builtin_join`). Returns `None` when an un-realized lazy list is
/// present, so the interpreter can force it and retry. Top-level shaped arrays
/// join over their leaves.
pub(crate) fn join_flat(sep: &str, rest: &[Value]) -> Option<String> {
    let mut items = Vec::new();
    for v in rest {
        if let Value::LazyList(ll) = v
            && ll.cache.lock().unwrap().is_none()
        {
            return None; // needs interpreter forcing
        }
        if crate::runtime::utils::is_shaped_array(v) {
            items.extend(crate::runtime::utils::shaped_array_leaves(v));
        } else {
            flat_val(v, &mut items, true);
        }
    }
    Some(
        items
            .iter()
            .map(|v| v.to_str_context())
            .collect::<Vec<_>>()
            .join(sep),
    )
}
