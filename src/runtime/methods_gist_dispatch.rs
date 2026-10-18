//! `.gist` of a collection that holds a value whose rendering is user code
//! (ADR-11276 §9.188): an instance, a mixin, a type object, a code object. The
//! pure renderer cannot call a user `gist`/`Str`, so the `List`, `Map`, `Seq`
//! and `Pair` rows decline such a collection and these are their interpreter
//! twins (`any_interp_twins`), reached through the late MRO walk.

use super::*;

impl Interpreter {
    /// The `.gist` of `target`, rendered element by element through method
    /// dispatch where an element needs it; `None` when `target` is not a
    /// collection or holds no such element (the pure rows answer it).
    // Cost: O(t), t = rendered size of the first `GIST_ELEM_CAP` elements of
    // each level, plus the elements' own `gist` calls.
    pub(crate) fn collection_gist_with_dispatch(
        &mut self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
            // Cycle-guarded: a circular container (`my @c; @c = 42, @c`) would
            // otherwise recurse until the process aborted on a stack overflow.
            // Only the `Gc`-backed containers can form a cycle, so they are the
            // only ones with an identity to track; `seen` holds every container
            // already walked, not just the ancestors, so a graph with two cyclic
            // edges is not re-walked once per path reaching it.
            fn collection_contains_instance_seen(
                value: &Value,
                seen: &mut std::collections::HashSet<usize>,
                depth: usize,
            ) -> bool {
                const MAX_DEPTH: usize = 256;
                if depth > MAX_DEPTH {
                    return false;
                }
                if matches!(
                    value.view(),
                    // Values that may carry a user-defined `method gist` or
                    // `method Str` must be rendered via method dispatch rather
                    // than the pure `gist_value` fast path.
                    ValueView::Instance { .. }
                        | ValueView::CustomType(_)
                        | ValueView::CustomTypeInstance(_)
                        | ValueView::Mixin(..)
                        | ValueView::Package(..)
                        // A Code object needs dispatch for a different reason:
                        // `gist_value` has no arm for one, so it fell through to
                        // `to_string_value()` — which is Code's *`.Str`* rule,
                        // the bare name. Inside a list that renders an anonymous
                        // block as the EMPTY STRING (`say (&b,)` printed `()`,
                        // so a one-element list read as empty) and drops a named
                        // routine's sigil (`(&f)` printed as `(f)`). The real
                        // `Code.gist` lives in the `Sub` method handler; route
                        // to it rather than growing a second implementation.
                        | ValueView::Sub(_)
                        | ValueView::WeakSub(_)
                        | ValueView::Routine { .. }
                ) {
                    return true;
                }
                let id = match value.view() {
                    ValueView::Array(data, _) => Some(crate::gc::Gc::as_ptr(&data) as usize),
                    ValueView::Hash(data) => Some(crate::gc::Gc::as_ptr(&data) as usize),
                    // A `:=`-bound element holds a cell, and a cycle can close
                    // through one, so cells have an identity of their own.
                    ValueView::ContainerRef(cell) => Some(crate::gc::Gc::as_ptr(&cell) as usize),
                    _ => None,
                };
                if let Some(id) = id
                    && !seen.insert(id)
                {
                    return false;
                }
                if let Some(items) = value.as_list_items() {
                    let items = items.to_vec();
                    return items
                        .iter()
                        .any(|v| collection_contains_instance_seen(v, seen, depth + 1));
                }
                match value.view() {
                    ValueView::Hash(map) => {
                        let values: Vec<Value> = map.values().cloned().collect();
                        values
                            .iter()
                            .any(|v| collection_contains_instance_seen(v, seen, depth + 1))
                    }
                    ValueView::Pair(_, v) => collection_contains_instance_seen(v, seen, depth + 1),
                    ValueView::ValuePair(k, v) => {
                        collection_contains_instance_seen(k, seen, depth + 1)
                            || collection_contains_instance_seen(v, seen, depth + 1)
                    }
                    // An element assigned from a scalar arrives ITEMIZED
                    // (`my @h = $c` compiles an `ItemizeVar`), so the instance
                    // that needs dispatch sits one `Scalar` down. Without this
                    // the probe answered `false` for `my @h = SomeArraySubclass
                    // .new(1,2)` and `@h.gist` fell through to the generic
                    // stringifying fallback, which rendered the element as
                    // `SA()`. The builtins-side twin (`gist_route` in
                    // `dispatch_core_repr`) has always looked through both, which
                    // is why the native probe correctly declined while this one
                    // then declined to take over.
                    ValueView::Scalar(inner) => {
                        collection_contains_instance_seen(inner, seen, depth + 1)
                    }
                    // Clone the contents out and drop the guard before
                    // recursing: `gist_item` holds this very lock across its own
                    // recursion, so a cell reached twice would deadlock.
                    ValueView::ContainerRef(cell) => {
                        let inner = cell.load();
                        collection_contains_instance_seen(&inner, seen, depth + 1)
                    }
                    _ => false,
                }
            }
            fn collection_contains_instance(value: &Value) -> bool {
                collection_contains_instance_seen(value, &mut std::collections::HashSet::new(), 0)
            }
            /// Gist at most `GIST_ELEM_CAP` elements, then ` ...` (Rakudo's
            /// `List.gist` stops the walk there); later elements are never
            /// rendered.
            // Cost: O(t), t = rendered size of the first `GIST_ELEM_CAP` elements.
            fn gist_capped<'a>(
                interp: &mut Interpreter,
                items: impl Iterator<Item = &'a Value>,
                sep: &str,
            ) -> String {
                let mut out = String::new();
                for (i, item) in items.enumerate() {
                    if i > 0 {
                        out.push_str(sep);
                    }
                    if i == crate::runtime::utils::GIST_ELEM_CAP {
                        out.push_str("...");
                        break;
                    }
                    out.push_str(&gist_item(interp, item));
                }
                out
            }
            fn gist_item(interp: &mut Interpreter, value: &Value) -> String {
                use crate::value::ArrayKind;
                if crate::value::render_guard::render_too_deep() {
                    return crate::value::render_guard::TRUNCATED.to_string();
                }
                // Subtrees with no dispatch-needing element render via the pure,
                // bracket-correct fast path.
                if !collection_contains_instance(value) {
                    return crate::runtime::gist_value(value);
                }
                match value.view() {
                    ValueView::Nil => "Nil".to_string(),
                    // `.gist` drops the itemization sigil, so an itemized
                    // element (`$(...)`, and every element `my @h = $x`
                    // produces) gists like its inner value -- the same rule the
                    // pure renderer's `gist_item` twin carries.
                    ValueView::Scalar(inner) => gist_item(interp, inner),
                    // Clone the contents out and drop the guard before
                    // recursing: a cycle can close through a cell
                    // (`my @e; @e.push(@e)`), and holding the lock across the
                    // recursion deadlocks instead of recursing.
                    ValueView::ContainerRef(cell) => {
                        let inner = cell.load();
                        gist_item(interp, &inner)
                    }
                    ValueView::Array(items, kind) => {
                        // Shaped arrays join their rows with a newline, like
                        // the pure fast path (`say my @a[2,2]` is one row per
                        // line even when cells are type objects).
                        let sep = if kind == ArrayKind::Shaped
                            && crate::runtime::utils::shaped_array_has_rows(value)
                        {
                            "\n "
                        } else {
                            " "
                        };
                        // A hole gists as the container's `is default(...)` value,
                        // not the `Package("Any")` marker the slot holds -- the
                        // marker is also what routed this array here in the
                        // first place (`collection_contains_instance`).
                        let inner = gist_capped(interp, items.items_with_default().iter(), sep);
                        match kind {
                            ArrayKind::List | ArrayKind::ItemList => format!("({inner})"),
                            _ => format!("[{inner}]"),
                        }
                    }
                    _ if value.as_list_items().is_some() => {
                        // Seq / Slip / HyperSeq / RaceSeq render parenthesized.
                        let inner = gist_capped(interp, value.as_list_items().unwrap().iter(), " ");
                        format!("({inner})")
                    }
                    ValueView::Hash(map) => {
                        let mut keys: Vec<&String> = map.keys().collect();
                        keys.sort();
                        let parts: Vec<String> = keys
                            .iter()
                            .map(|k| {
                                // An object hash stores `.WHICH` strings as its
                                // keys; the gist must show the original typed
                                // key (`True`, not `Bool|1`). The pure gist path
                                // already does this — this dispatching one, which
                                // takes over as soon as *any* value in the
                                // subtree is an instance with a custom `.gist`,
                                // was rendering the raw stored key instead, so
                                // `%h{True} = Foo.new` gisted as `Bool|1 => ...`
                                // while `%h{True} = 1` gisted correctly.
                                format!(
                                    "{} => {}",
                                    gist_item(interp, &map.typed_key(k)),
                                    gist_item(interp, &map[*k])
                                )
                            })
                            .collect();
                        format!("{{{}}}", parts.join(", "))
                    }
                    ValueView::Pair(k, v) => format!("{} => {}", k, gist_item(interp, v)),
                    ValueView::ValuePair(k, v) => {
                        format!("{} => {}", gist_item(interp, k), gist_item(interp, v))
                    }
                    _ => match interp.render_gist_value(value) {
                        Ok(v) => v,
                        Err(_) => crate::runtime::gist_value(value),
                    },
                }
            }
            // Render a collection that embeds a value with a custom gist via
            // method dispatch, preserving each container's bracket style.
            if matches!(
                target.view(),
                ValueView::Array(..)
                    | ValueView::Seq(..)
                    | ValueView::HyperSeq(..)
                    | ValueView::RaceSeq(..)
                    | ValueView::Slip(..)
                    | ValueView::Hash(..)
                    | ValueView::Pair(..)
                    | ValueView::ValuePair(..)
            ) && collection_contains_instance(target)
            {
                // `gist_item` is a plain recursion: a circular container would
                // run it until the process aborted on a stack overflow. Render
                // such a target with `gist_value`, the one gist renderer that
                // carries the cycle rule (see the twin guard in
                // `builtins::methods_0arg::dispatch_core_repr`).
                if crate::runtime::utils::contains_cycle(target) {
                    return Some(Ok(Value::str(crate::runtime::gist_value(target))));
                }
                return Some(Ok(Value::str(gist_item(self, target))));
            }
        None
    }
}
