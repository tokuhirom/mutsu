use crate::value::RuntimeError;
use crate::value::flat::flat_val;
use crate::value::{Value, ValueView};

/// Whether `join` must render this collection as `...` without pulling it.
/// A cache-backed finite lazy list is joinable after reification unless it was
/// explicitly marked lazy, as reported by `is_genuinely_lazy`.
pub(crate) fn is_join_lazy(value: &Value) -> bool {
    crate::runtime::Interpreter::is_lazy_for_coerce(value)
        || matches!(value.view(), ValueView::Seq(items) if items.is_lazy())
}

/// Does joining `v` need the interpreter, because some element it contributes
/// may carry a *user-defined* `.Str` (a class instance, or a `but`/`does`
/// role-mixed value), or is a `Proxy` whose FETCH has to run? [`join_flat`] can
/// only call `to_str_context`, so those must be routed to
/// `Interpreter::builtin_join`, which dispatches the method first. Without this
/// gate the pure two-argument `join(sep, $obj)` fast path answered before the
/// interpreter ever saw the call, so a role mixin's `Str` was skipped for `join`
/// while `print` on the same value honoured it.
///
/// The `Proxy` arms are ADR-0040 §9.2, and `"@a[]"` interpolation is why they
/// matter beyond a literal `join` call: it compiles to `join(" ", @a)`, so this
/// gate is the only thing standing between an interpolated array and a rendered
/// `Proxy`. An element bound with `@a[0] := $p` holds its Proxy behind the
/// element's own container cell (§9.1), so the scan looks through one.
pub(crate) fn join_needs_interpreter(v: &Value) -> bool {
    match v.view() {
        ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::Proxy { .. } => true,
        // A Junction anywhere in the (recursively flattened) argument list
        // must thread the whole `join` over its eigenstates
        // (`("a"|"b","c","d").join` => `any(acd, bcd)`) — the pure
        // `join_flat`/`flat_val` path can only stringify it in place, so
        // route to `Interpreter::builtin_join`, which does the threading.
        ValueView::Junction { .. } => true,
        // A deferred `.map`/`.grep` Seq (an inner `(3,4).map({...})` returned
        // by an outer map's block) has not run its callback yet; only the
        // interpreter can, and `to_str_context` would render it as "".
        ValueView::LazyList(_) | ValueView::LazyThunk(_) => true,
        ValueView::Seq(items) if items.awaits_vm_reify() => true,
        // Exactly one level, no recursion past the cell: the bind puts the
        // Proxy directly behind it, whereas a cell holding a structure may be a
        // self-reference and following it walks the cycle forever.
        ValueView::ContainerRef(_) | ValueView::ContainerView(_) => {
            v.deref_container().is_proxy_value()
        }
        ValueView::Array(items, kind) if !kind.is_itemized() => {
            items.iter().any(join_needs_interpreter)
        }
        ValueView::Seq(items) => items.iter().any(join_needs_interpreter),
        _ => false,
    }
}

/// Join `rest` with `sep`, flattening with `flat`/slurpy semantics (the single
/// shared `join` body for both `native_function("join", ..)` and the
/// interpreter's `builtin_join`). Returns `None` when an un-realized lazy list is
/// present, so the interpreter can force it and retry. Top-level shaped arrays
/// join over their leaves. An element that is (or holds) a zero-denominator
/// Rational dies like its own `.Str` (GH #9621).
pub(crate) fn join_flat(sep: &str, rest: &[Value]) -> Option<Result<String, RuntimeError>> {
    let mut items = Vec::new();
    for v in rest {
        if crate::builtins::is_join_lazy(v) {
            items.push(Value::str("...".to_string()));
            continue;
        }
        if let ValueView::LazyList(ll) = v.view()
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
    if let Some(err) = items
        .iter()
        .find_map(crate::runtime::utils::zero_denominator_rational_error)
    {
        return Some(Err(err));
    }
    Some(Ok(items
        .iter()
        .map(|v| v.to_str_context())
        .collect::<Vec<_>>()
        .join(sep)))
}

/// If a (already fully flattened) list of items contains a `Junction`
/// anywhere, thread `render` across the cross product of every SAME-kind
/// junction position's eigenstates, returning one flat `Junction` of that
/// kind (`("a"|"b","c"|"d","e").join` => `any(ace, ade, bce, bde)`, not a
/// junction of junctions — mirrors the flattening
/// `eval_concat_with_junctions` applies for two same-kind operands, verified
/// against Rakudo). A junction of a DIFFERENT kind is left for a recursive
/// peel afterward, so mismatched kinds still nest, though this simple
/// leftmost-first peel does not reproduce Rakudo's exact kind-label swap for
/// a mixed-kind, multiple-junction list (verified only for a single
/// junction, or several junctions of one kind, per the acceptance criteria
/// this exists for — a mixed-kind multi-junction list is a deeper case left
/// unfixed, same spirit as the issue's own "record, do not fix blind" note).
/// Returns `None` when no item is a `Junction`, so the caller runs `render`
/// on the unchanged items. `render` is pure (no interpreter access) — every
/// current caller (`join`) only needs `.to_str_context()`.
pub(crate) fn thread_junctions_in_items(
    items: &[Value],
    render: &dyn Fn(&[Value]) -> Value,
) -> Option<Value> {
    let first_idx = items
        .iter()
        .position(|v| matches!(v.view(), ValueView::Junction { .. }))?;
    let ValueView::Junction {
        kind: first_kind, ..
    } = items[first_idx].view()
    else {
        unreachable!("position() just matched a Junction")
    };
    let same_kind_positions: Vec<usize> = items
        .iter()
        .enumerate()
        .filter_map(|(i, v)| match v.view() {
            ValueView::Junction { kind, .. } if kind == first_kind => Some(i),
            _ => None,
        })
        .collect();
    let mut combos: Vec<Vec<Value>> = vec![items.to_vec()];
    for &i in &same_kind_positions {
        let ValueView::Junction { values, .. } = items[i].view() else {
            unreachable!("same_kind_positions only holds Junction indices")
        };
        let mut next = Vec::with_capacity(combos.len() * values.len());
        for combo in &combos {
            for val in values.iter() {
                let mut c = combo.clone();
                c[i] = val.clone();
                next.push(c);
            }
        }
        combos = next;
    }
    let results: Vec<Value> = combos
        .iter()
        .map(|c| thread_junctions_in_items(c, render).unwrap_or_else(|| render(c)))
        .collect();
    Some(Value::junction(first_kind, results))
}
