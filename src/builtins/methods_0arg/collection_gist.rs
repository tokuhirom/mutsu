//! A collection's `.gist` (`Array`/`List`, `Seq`, `Slip`, `Hash`/`Map`, `Pair`): the one
//! renderer the `gist` rows of `method_table::collections::render` and the
//! `dispatch_core_repr` cascade share (ADR-11276, the rendering names).

use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

use super::raku_repr::promise_raku_repr;
use super::{gist_array_wrap, range_gist_string};

/// Rakudo caps an aggregate's `.gist` at the first 100 elements, then appends
/// ` ...`, so a huge array/list does not flood terminal output.
const GIST_ELEM_CAP: usize = 100;

/// Leaf-value gist for a list/array element: a WhateverCode (`*+1`) gists as
/// `WhateverCode.new`; everything else uses its string value.
fn leaf_gist(v: &Value) -> String {
    if let ValueView::Uni(u) = v.view() {
        // A Uni / normalization form gists as e.g. NFKC:0x<0066 0066>.
        let cps: Vec<String> = u
            .codepoints()
            .into_iter()
            .map(|c| format!("{c:04X}"))
            .collect();
        let form = if u.form.is_empty() {
            "Uni"
        } else {
            u.form.as_str()
        };
        return format!("{}:0x<{}>", form, cps.join(" "));
    }
    if let ValueView::Sub(data) = v.view()
        && matches!(
            data.env.get("__mutsu_callable_type").map(Value::view),
            Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
        )
    {
        return "WhateverCode.new".to_string();
    }
    if let ValueView::Promise(p) = v.view() {
        // Promise has no custom gist; its element gist is the `.raku` form.
        return promise_raku_repr(&p.status());
    }
    if let ValueView::Channel(_) = v.view() {
        return "Channel.new".to_string();
    }
    if let ValueView::Version { .. } = v.view() {
        // A Version element keeps its `v` prefix (`[v1.2.3]`), which its
        // bare string value drops.
        return format!("v{}", v.to_string_value());
    }
    if let ValueView::Capture { positional, named } = v.view() {
        // A Capture element keeps its call shape; its bare string value is
        // the joined `.Str` form.
        return crate::value::capture_text::capture_gist(positional, named);
    }
    v.to_string_value()
}

/// How a collection's `.gist` must be rendered, decided by one walk of it.
enum GistRoute {
    /// The pure per-type renderers below can do it.
    Native,
    /// Some element may carry a user-defined `method gist` (an instance, custom
    /// type, or type object), which the pure path here cannot dispatch — it
    /// would render the default form. Defer to the runtime slow path. (Mixin is
    /// excluded: a Mixin wrapping a List/Array renders via its inner value, so
    /// the pure path is correct and dispatching `.gist` would add a spurious
    /// paren.)
    Dispatch,
    /// The receiver reaches itself. The per-type renderers below are plain
    /// recursions with no cycle handling, so they would run until the process
    /// aborted on a stack overflow; `gist_value` is the one gist renderer that
    /// carries the cycle rule (Rakudo's `(\Array_… = …)` back-reference).
    ///
    /// TODO: an element with a user-defined `method gist` inside a *cyclic*
    /// structure renders with its default gist, because `gist_value` is pure and
    /// cannot dispatch. Fixing that needs the interpreter-side walk to carry the
    /// same visited-set discipline; a crash-free default gist is the better
    /// trade until then.
    Cyclic,
}

/// Decide a collection's gist route in a single walk.
///
/// Both questions are answered together on purpose: `.gist` renders at most
/// `GIST_ELEM_CAP` elements of each list, and the probe stops at the same cap,
/// so it never costs more than the rendering it guards. Adding a
/// second, separate cycle pass would have made every `say @big-array` walk it
/// twice.
///
/// A cycle needs an *ancestor* to repeat, not merely a container to be seen
/// twice — Rakudo renders a shared-but-not-nested container in full at each
/// occurrence — so `active` is the ancestor chain while `done` memoizes subtrees
/// already walked, which keeps a diamond-shaped graph from being re-walked once
/// per path. The depth cap is the backstop for a pathologically deep acyclic
/// structure.
/// Cost: O(t), t = nodes reachable through at most the first `GIST_ELEM_CAP`
/// elements of each list level (the rendered head; a hash level is walked whole).
fn gist_route(v: &Value) -> GistRoute {
    use crate::runtime::utils::GIST_ELEM_CAP;
    /// A `:=`-bound element holds a `ContainerRef` cell, and a cycle can close
    /// through one (`my @e; @e.push(@e)` stores a cell whose contents reach the
    /// array again), so cells are cycle participants with an identity of their
    /// own — not merely something to look through.
    fn container_id(v: &Value) -> Option<usize> {
        match v.view() {
            ValueView::Array(data, _) => Some(crate::gc::Gc::as_ptr(&data) as usize),
            ValueView::Hash(data) => Some(crate::gc::Gc::as_ptr(&data) as usize),
            ValueView::ContainerRef(cell) => Some(crate::gc::Gc::as_ptr(&cell) as usize),
            _ => None,
        }
    }
    /// `None` = nothing found under `v`; `Some(route)` stops the walk. A
    /// `Dispatch` answer may hide a cycle deeper in, but the runtime path it
    /// routes to re-checks for one, so both orders end in the same renderer.
    fn walk(
        v: &Value,
        active: &mut Vec<usize>,
        done: &mut std::collections::HashSet<usize>,
        depth: usize,
    ) -> Option<GistRoute> {
        const MAX_DEPTH: usize = 256;
        if depth > MAX_DEPTH {
            return None;
        }
        if matches!(
            v.view(),
            ValueView::Instance { .. }
                | ValueView::CustomType(..)
                | ValueView::CustomTypeInstance(_)
                | ValueView::Package(..)
                // A `Code` element renders through the Sub method handler
                // (`&name`, `-> $a  #`(Block|N)`), which this pure walk lacks.
                | ValueView::Sub(..)
                | ValueView::WeakSub(..)
                | ValueView::Routine { .. }
        ) {
            return Some(GistRoute::Dispatch);
        }
        let id = container_id(v);
        if let Some(id) = id {
            if active.contains(&id) {
                return Some(GistRoute::Cyclic);
            }
            if done.contains(&id) {
                return None;
            }
            active.push(id);
        }
        let mut found = None;
        {
            let mut visit = |e: &Value| {
                if found.is_none() {
                    found = walk(e, active, done, depth + 1);
                }
            };
            match v.view() {
                // A list renders only its first `GIST_ELEM_CAP` elements, so
                // nothing past them can need dispatch or loop back.
                ValueView::Array(items, _) => items.iter().take(GIST_ELEM_CAP).for_each(&mut visit),
                ValueView::Seq(items) => items.iter().take(GIST_ELEM_CAP).for_each(&mut visit),
                ValueView::Slip(items) => items.iter().take(GIST_ELEM_CAP).for_each(&mut visit),
                ValueView::Hash(map) => map.values().for_each(&mut visit),
                ValueView::Pair(_, val) => visit(val),
                ValueView::ValuePair(k, val) => {
                    visit(k);
                    visit(val);
                }
                // Clone the cell's contents out and drop the guard before
                // descending: the renderers below hold this very lock across
                // their recursion, so a cell reached twice would deadlock rather
                // than recurse (which is exactly the shape this probe exists to
                // divert).
                ValueView::ContainerRef(cell) => {
                    let inner = cell.lock().unwrap().clone();
                    visit(&inner);
                }
                ValueView::Scalar(inner) => visit(inner),
                _ => {}
            }
        }
        if let Some(id) = id {
            active.pop();
            if found.is_none() {
                done.insert(id);
            }
        }
        found
    }
    walk(v, &mut Vec::new(), &mut std::collections::HashSet::new(), 0).unwrap_or(GistRoute::Native)
}

/// One element's gist inside a list, `Seq` or `Slip`.
// Cost: O(d), d = rendered size of the element (nested aggregates render in full).
fn gist_item(v: &Value) -> String {
    match v.view() {
        ValueView::Nil => "Nil".to_string(),
        // Clone the contents out and drop the guard before
        // recursing: a cycle can close through a cell
        // (`my @e; @e.push(@e)`), and holding the lock across
        // the recursion deadlocks instead of recursing.
        ValueView::ContainerRef(cell) => {
            let inner = cell.lock().unwrap().clone();
            gist_item(&inner)
        }
        // `$(...)` itemized element: `.gist` drops the itemization
        // sigil, so it gists like its inner value.
        ValueView::Scalar(inner) => gist_item(inner),
        ValueView::Array(_, crate::value::ArrayKind::Lazy) => "[...]".to_string(),
        ValueView::LazyList(ll) if ll.is_genuinely_lazy() => {
            crate::value::lazy_list_placeholder("gist", ll.in_array_context())
        }
        // A pulled finite lazy list (a `gather` element) is a Seq:
        // the shared gist renders it parenthesised.
        ValueView::LazyList(_) => crate::value::gist::gist_value(v),
        ValueView::Array(inner, kind) => {
            let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
            gist_array_wrap(&elems, kind)
        }
        ValueView::Seq(inner) => {
            let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
            format!("({})", elems)
        }
        ValueView::Slip(inner) => {
            let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
            format!("({})", elems)
        }
        ValueView::Hash(map) => {
            let mut sorted_keys: Vec<&String> = map.keys().collect();
            sorted_keys.sort();
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| {
                    // An object hash stores `.WHICH` string keys;
                    // show the original key (`True`, not `Bool|1`).
                    let key_disp = if map.has_typed_keys() {
                        gist_item(&map.typed_key(k))
                    } else {
                        (*k).clone()
                    };
                    format!("{} => {}", key_disp, gist_item(&map[*k]))
                })
                .collect();
            // A nested immutable Map gists as `Map.new((...))`, not `{...}`.
            if map.declared_type.as_deref() == Some("Map") {
                format!("Map.new(({}))", parts.join(", "))
            } else {
                format!("{{{}}}", parts.join(", "))
            }
        }
        ValueView::Pair(k, v) => format!("{} => {}", k, gist_item(v)),
        ValueView::ValuePair(k, v) => {
            // Parenthesize a Pair-valued key: `(red => 2) => apples`.
            let key = match k.view() {
                ValueView::Pair(..) | ValueView::ValuePair(..) => {
                    format!("({})", gist_item(k))
                }
                _ => gist_item(k),
            };
            format!("{} => {}", key, gist_item(v))
        }
        ValueView::Junction { kind, values } => {
            let kind_str = match kind {
                crate::value::JunctionKind::Any => "any",
                crate::value::JunctionKind::All => "all",
                crate::value::JunctionKind::One => "one",
                crate::value::JunctionKind::None => "none",
            };
            let elems = values.iter().map(gist_item).collect::<Vec<_>>().join(", ");
            format!("{}({})", kind_str, elems)
        }
        // A Match nested in a list/seq gist renders as its full
        // `Match.gist` (corner-quoted text + sub-captures), not the
        // bare matched string.
        ValueView::Instance { attributes, .. } if v.is_match_instance() => {
            crate::runtime::utils::match_gist(&(attributes).as_map(), 0)
        }
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            // A Set/Bag/Mix nested in a list/array gist keeps its
            // type-name wrapper (`Set(a b c)`), like the say/gist
            // fast path, rather than its bare-element `.Str` form.
            crate::runtime::utils::setbagmix_gist(v).unwrap_or_else(|| v.to_string_value())
        }
        _ if v.is_range() => range_gist_string(v),
        _ => leaf_gist(v),
    }
}

// A real array's (`@`-sigiled) elements are Scalar containers, so a
// cell can never hold Nil: assigning Nil reverts it to the element
// type default (`Any` for untyped), and a shaped array fills unused
// cells the same way. Gist such a cell as the type object `(Any)`.
// Lists/Seqs keep a genuine Nil, so this only applies to real arrays.
fn gist_real_array_item(v: &Value) -> String {
    match v.view() {
        ValueView::Nil => "(Any)".to_string(),
        ValueView::Array(inner, kind) if kind.is_real_array() => {
            let elems = inner
                .iter()
                .map(gist_real_array_item)
                .collect::<Vec<_>>()
                .join(" ");
            gist_array_wrap(&elems, kind)
        }
        _ => gist_item(v),
    }
}

/// The gist of a collection, or `None` for a receiver that is not one.
///
/// `Some(None)` sends the call to the interpreter (an element may carry a user
/// `gist`); `Some(Some(r))` is the answer.
// Cost: O(min(e, 100) * d), e = elements, d = rendered size of each shown element
// (nested aggregates render in full); the `gist_route` probe adds O(t) for the
// whole structure, t = nodes it reaches.
pub(crate) fn collection_gist(target: &Value) -> Option<Option<Result<Value, RuntimeError>>> {
    if !matches!(
        target.view(),
        ValueView::Array(..)
            | ValueView::Seq(..)
            | ValueView::Slip(..)
            | ValueView::Hash(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
    ) {
        return None;
    }
    // An element that is a zero-denominator Rational dies when the collection
    // gists it, as `(1/0).gist` does (GH #9608).
    if let Some(err) = runtime::utils::zero_denominator_rational_error(target) {
        return Some(Some(Err(err)));
    }
    // Route the gist: to the runtime slow path when an element may have a custom
    // `method gist` (so per-element gist dispatch is honored), or to `gist_value`
    // when the receiver is circular.
    match gist_route(target) {
        GistRoute::Dispatch => return Some(None),
        GistRoute::Cyclic => {
            return Some(Some(Ok(Value::str(runtime::utils::gist_value(target)))));
        }
        GistRoute::Native => {}
    }
    Some(Some(Ok(match target.view() {
        // A lazy (infinite-backed) array renders a bounded placeholder rather
        // than materializing its (possibly capped 100k) backing, matching
        // Rakudo's `[...]`.
        ValueView::Array(_, crate::value::ArrayKind::Lazy) => Value::str_from("[...]"),
        ValueView::Array(items, kind) => {
            let elem_render: fn(&Value) -> String = if kind.is_real_array() {
                gist_real_array_item
            } else {
                gist_item
            };
            // A hole gists as the container's `is default(...)` value, not the
            // `Any` marker the slot holds (`ArrayData::items_with_default`).
            let items = items.items_with_default();
            // Only an actual multidimensional shape has rows. A 1-D shaped
            // array may hold an Array as one of its leaf values.
            if kind == crate::value::ArrayKind::Shaped
                && runtime::utils::shaped_array_has_rows(target)
            {
                let rows: Vec<String> = items.iter().map(elem_render).collect();
                return Some(Some(Ok(Value::str(format!("[{}]", rows.join("\n "))))));
            }
            Value::str(gist_array_wrap(&capped_join(&items, elem_render), kind))
        }
        // A lazy, not-yet-pulled iterator Seq (`Seq.new($lazy-iterator)`,
        // `Seq.from-loop`) gists as Rakudo's placeholder without being pulled.
        ValueView::Seq(body) if body.gists_as_lazy_placeholder() => {
            Value::str(crate::value::lazy_list_placeholder("gist", false))
        }
        ValueView::Seq(items) => Value::str(format!("({})", capped_join(&items, gist_item))),
        ValueView::Slip(items) => Value::str(format!("({})", capped_join(&items, gist_item))),
        ValueView::Pair(k, v) => Value::str(format!("{} => {}", k, runtime::gist_value(v))),
        // A Pair-valued key is parenthesized so the outer arrow is unambiguous
        // (`(red => 2) => apples`), matching raku's gist and the `gist_value`
        // fast path.
        ValueView::ValuePair(k, v) => {
            let key_gist = match k.view() {
                ValueView::Pair(..) | ValueView::ValuePair(..) => {
                    format!("({})", runtime::gist_value(k))
                }
                _ => runtime::gist_value(k),
            };
            Value::str(format!("{} => {}", key_gist, runtime::gist_value(v)))
        }
        // An immutable `Map` gists as `Map.new((...))`, its first 100 sorted
        // pairs and then `...` (a `Hash` renders in full).
        ValueView::Hash(map) if map.declared_type.as_deref() == Some("Map") => {
            let mut sorted_keys: Vec<&String> = map.keys().collect();
            sorted_keys.sort();
            let mut parts: Vec<String> = sorted_keys
                .iter()
                .take(GIST_ELEM_CAP)
                .map(|k| {
                    let key_disp = runtime::gist_value(&map.typed_key(k));
                    format!("{} => {}", key_disp, runtime::gist_value(&map[*k]))
                })
                .collect();
            if sorted_keys.len() > GIST_ELEM_CAP {
                parts.push("...".to_string());
            }
            Value::str(format!("Map.new(({}))", parts.join(", ")))
        }
        ValueView::Hash(map) => {
            let mut sorted_keys: Vec<&String> = map.keys().collect();
            sorted_keys.sort();
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| {
                    // Object hashes store `.WHICH` string keys; show the original
                    // typed key (e.g. `a`, not `Str|a`).
                    let key_disp = runtime::gist_value(&map.typed_key(k));
                    format!("{} => {}", key_disp, runtime::gist_value(&map[*k]))
                })
                .collect();
            Value::str(format!("{{{}}}", parts.join(", ")))
        }
        _ => unreachable!("checked above"),
    })))
}

/// The first `GIST_ELEM_CAP` elements joined by a space, then ` ...` when there
/// are more (Rakudo caps aggregate gists so a huge array does not flood output).
// Cost: O(min(n, 100) * d), n = elements, d = rendered size of each shown one.
fn capped_join(items: &[Value], render: fn(&Value) -> String) -> String {
    if items.len() > GIST_ELEM_CAP {
        let mut s = items[..GIST_ELEM_CAP]
            .iter()
            .map(render)
            .collect::<Vec<_>>()
            .join(" ");
        s.push_str(" ...");
        s
    } else {
        items.iter().map(render).collect::<Vec<_>>().join(" ")
    }
}
