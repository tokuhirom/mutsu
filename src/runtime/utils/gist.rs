use super::*;
use crate::value::AttrMap;

/// Depth cap for the gist-dispatch probe. Cycles are caught by container
/// identity (below); this only keeps a pathologically deep structure from
/// blowing the stack.
const GIST_PROBE_MAX_DEPTH: usize = 256;

/// Container identity, for the visited set. Only the `Gc`-backed containers can
/// participate in a cycle, so the others have no identity to track. A `:=`-bound
/// element holds a `ContainerRef` cell and a cycle can close through one
/// (`my @e; @e.push(@e)`), so cells count too.
fn container_id(value: &Value) -> Option<usize> {
    match value.view() {
        ValueView::Array(data, _) => Some(crate::gc::Gc::as_ptr(&data) as usize),
        ValueView::Hash(data) => Some(crate::gc::Gc::as_ptr(&data) as usize),
        ValueView::ContainerRef(cell) => Some(crate::gc::Gc::as_ptr(&cell) as usize),
        _ => None,
    }
}

/// `seen` holds every container already walked — not just the ancestors. A
/// circular structure (`my @c; @c = 42, @c`) would otherwise be re-walked once
/// per path reaching it, which is exponential for a graph with two cyclic
/// edges. This mirrors the `.raku` twin, `contains_dispatch_leaf_seen` in
/// `runtime::methods_raku_dispatch`.
fn contains_instance_seen(
    value: &Value,
    seen: &mut std::collections::HashSet<usize>,
    depth: usize,
) -> bool {
    if depth > GIST_PROBE_MAX_DEPTH {
        return false;
    }
    if matches!(value.view(), ValueView::Instance { .. }) {
        return true;
    }
    if let Some(id) = container_id(value)
        && !seen.insert(id)
    {
        return false;
    }
    if let Some(items) = value.as_list_items() {
        return items
            .iter()
            .any(|v| contains_instance_seen(v, seen, depth + 1));
    }
    match value.view() {
        ValueView::Hash(map) => map
            .values()
            .any(|v| contains_instance_seen(v, seen, depth + 1)),
        _ => false,
    }
}

/// Whether `value` reaches itself — some `Gc`-backed container is its own
/// ancestor, as in `my @c; @c = 42, @c`.
///
/// The interpreter-side gist walk (`gist_item` in
/// `runtime::methods_call_dispatch`) is a plain recursion with no cycle handling
/// of its own, so it hands a cyclic receiver to [`gist_value`] — the one gist
/// renderer that carries the cycle rule (Rakudo's `(\Array_… = …)`
/// back-reference). Keeping that rule in one renderer is why this is a handoff
/// rather than another copy of the visited-set walk. (The builtins-side fast
/// path asks the same question, but folds it into the dispatch probe it already
/// runs — `GistRoute` in `builtins::methods_0arg::dispatch_core_repr` — so a
/// large receiver is not walked twice.)
///
/// A plain DAG is *not* a cycle: Rakudo renders a shared-but-not-nested
/// container in full at each occurrence, so the probe tracks the *ancestor*
/// chain. `done` memoizes subtrees already proven acyclic, which keeps a
/// diamond-shaped graph from being re-walked once per path.
pub(crate) fn contains_cycle(value: &Value) -> bool {
    fn walk(
        v: &Value,
        active: &mut Vec<usize>,
        done: &mut std::collections::HashSet<usize>,
        depth: usize,
    ) -> bool {
        if depth > GIST_PROBE_MAX_DEPTH {
            return false;
        }
        let id = container_id(v);
        if let Some(id) = id {
            if active.contains(&id) {
                return true;
            }
            if done.contains(&id) {
                return false;
            }
            active.push(id);
        }
        let children: Vec<Value> = match v.view() {
            ValueView::Hash(map) => map.values().cloned().collect(),
            ValueView::Pair(_, val) => vec![val.clone()],
            ValueView::ValuePair(k, val) => vec![k.clone(), val.clone()],
            ValueView::Scalar(inner) => vec![inner.clone()],
            ValueView::ContainerRef(cell) => vec![cell.lock().unwrap().clone()],
            _ => match v.as_list_items() {
                Some(items) => items.to_vec(),
                None => Vec::new(),
            },
        };
        let found = children.iter().any(|c| walk(c, active, done, depth + 1));
        if let Some(id) = id {
            active.pop();
            if !found {
                done.insert(id);
            }
        }
        found
    }
    walk(
        value,
        &mut Vec::new(),
        &mut std::collections::HashSet::new(),
        0,
    )
}

/// Whether `value` is a *collection* holding (transitively) an `Instance` whose
/// `.gist` may need interpreter dispatch — the guard for the VM's collection
/// gist bypass (`vm::vm_native_dispatch`).
///
/// Only a collection receiver triggers the bypass. A bare instance (e.g. a
/// `Buf`, whose gist `native_method_0arg` renders purely via
/// `dispatch_core_repr`) is dispatched normally — the builtins layer itself
/// defers a collection whose elements may have a user `method gist`, so
/// bypassing a bare instance here only forced a pure native gist (Buf/Blob/Uni)
/// onto the interpreter for nothing.
///
/// The walk is cycle-guarded. Without that guard a circular container aborted
/// the whole process on a stack overflow *here*, in the dispatch probe, before
/// [`gist_value`] — which does detect the cycle — was ever reached.
pub(crate) fn collection_contains_instance(value: &Value) -> bool {
    let mut seen = std::collections::HashSet::new();
    if let Some(id) = container_id(value) {
        seen.insert(id);
    }
    if let Some(items) = value.as_list_items() {
        return items
            .iter()
            .any(|v| contains_instance_seen(v, &mut seen, 1));
    }
    match value.view() {
        ValueView::Hash(map) => map
            .values()
            .any(|v| contains_instance_seen(v, &mut seen, 1)),
        _ => false,
    }
}

/// Render the `.gist` form of a Set/Bag/Mix (and their mutable `*Hash`
/// variants): `Set(a b c)`, `Bag(a b(2))`, `Mix(a(1.5) b)`. Keys are sorted
/// for deterministic output. Returns `None` for any other value. Shared by
/// `gist_value` (the fast say/gist path) and the `.gist` method dispatch so
/// both render identically.
pub(crate) fn setbagmix_gist(value: &Value) -> Option<String> {
    setbagmix_gist_named(value, None)
}

/// [`setbagmix_gist`] with the type-name wrapper supplied by the caller, for a
/// `but`-mixed quanthash: the role belongs in the name (`Set+{R}(a)`), and only
/// the caller holding the `Mixin` value knows it.
pub(crate) fn setbagmix_gist_named(value: &Value, type_override: Option<&str>) -> Option<String> {
    match value.view() {
        ValueView::Set(s, mutable) => {
            let type_name = type_override.unwrap_or(if mutable { "SetHash" } else { "Set" });
            let ptr = crate::gc::Gc::as_ptr(&s) as usize;
            let inner = crate::value::with_quanthash_render_guard(ptr, || {
                let mut keys: Vec<&String> = s.iter().collect();
                keys.sort();
                // Render each element via its original type's gist (so a Pair element
                // is `a => 1`, not the internal `a\t1` string key); a plain Str/Int
                // element gists bare, identical to the raw key.
                keys.iter()
                    .map(|k| gist_value(&s.typed_key(k)))
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .unwrap_or_else(|| "...".to_string());
            Some(format!("{}({})", type_name, inner))
        }
        ValueView::Bag(b, mutable) => {
            let type_name = type_override.unwrap_or(if mutable { "BagHash" } else { "Bag" });
            let ptr = crate::gc::Gc::as_ptr(&b) as usize;
            let inner = crate::value::with_quanthash_render_guard(ptr, || {
                let mut keys: Vec<(&String, &BigInt)> = b.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                keys.iter()
                    .map(|(k, v)| {
                        let key = gist_value(&b.typed_key(k));
                        if **v == BigInt::from(1) {
                            key
                        } else {
                            format!("{}({})", key, v)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .unwrap_or_else(|| "...".to_string());
            Some(format!("{}({})", type_name, inner))
        }
        ValueView::Mix(m, mutable) => {
            let type_name = type_override.unwrap_or(if mutable { "MixHash" } else { "Mix" });
            let ptr = crate::gc::Gc::as_ptr(&m) as usize;
            let inner = crate::value::with_quanthash_render_guard(ptr, || {
                let mut keys: Vec<(&String, &f64)> = m.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                keys.iter()
                    .map(|(k, v)| {
                        let key = gist_value(&m.typed_key(k));
                        if (**v - 1.0).abs() < f64::EPSILON {
                            key
                        } else if v.fract() == 0.0 {
                            format!("{}({})", key, **v as i64)
                        } else {
                            format!("{}({})", key, v)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .unwrap_or_else(|| "...".to_string());
            Some(format!("{}({})", type_name, inner))
        }
        _ => None,
    }
}

pub(crate) fn gist_value(value: &Value) -> String {
    // Cycle detection for recursive data structures (shared hash/array Gcs).
    //
    // Rakudo renders a cycle the way `Mu.gistseen` does: the node the walk
    // loops back to is named after its type and address, and *that* node — not
    // the top-level one — carries a `(\Name = ...)` binding preamble:
    //
    //     my @c; @c = 42, @c;              # (\Array_140… = [42 Array_140…])
    //     my @a; my @b; @b = 1, @b; @a = 0, @b;
    //                                      # [0 (\Array_140… = [1 Array_140…])]
    //
    // The visited set is therefore *ancestor*-scoped (pushed on entry, popped
    // on exit), not walk-global: a plain DAG — the same array reachable by two
    // non-nested paths — is rendered in full both times by Rakudo, with no
    // back-reference. Each entry carries a "was looped back to" flag that the
    // revisit sets and the exit reads.
    thread_local! {
        static SEEN_PTRS: std::cell::RefCell<Vec<(usize, bool)>> = const { std::cell::RefCell::new(Vec::new()) };
    }
    /// Mark `ptr` as being rendered. Returns true when `ptr` is already an
    /// ancestor — a cycle — flagging that ancestor so its exit emits the
    /// binding preamble.
    fn check_and_push(ptrs: &std::cell::RefCell<Vec<(usize, bool)>>, ptr: usize) -> bool {
        let mut s = ptrs.borrow_mut();
        if let Some(entry) = s.iter_mut().rev().find(|(p, _)| *p == ptr) {
            entry.1 = true;
            return true; // cycle detected
        }
        s.push((ptr, false));
        false
    }
    /// Pop `ptr`, reporting whether the walk under it looped back to it.
    fn pop_ptr(ptrs: &std::cell::RefCell<Vec<(usize, bool)>>, ptr: usize) -> bool {
        let mut s = ptrs.borrow_mut();
        match s.iter().rposition(|(p, _)| *p == ptr) {
            Some(pos) => s.remove(pos).1,
            None => false,
        }
    }
    /// `(\Array_140… = [42 Array_140…])` — the binding preamble Rakudo emits on
    /// the node a cycle loops back to.
    fn cycle_binding(name: &str, ptr: usize, rendered: &str) -> String {
        format!("(\\{}_{} = {})", name, ptr, rendered)
    }
    match value.view() {
        // A Uni / normalization form gists as e.g. NFKC:0x<0066 0066>, not as
        // the plain decoded text.
        ValueView::Uni(u) => {
            let cps: Vec<String> = u
                .text
                .chars()
                .map(|c| format!("{:04X}", c as u32))
                .collect();
            let form = if u.form.is_empty() {
                "Uni"
            } else {
                u.form.as_str()
            };
            format!("{}:0x<{}>", form, cps.join(" "))
        }
        // A `:=`-bound element holds a `ContainerRef` cell; render the held
        // value so a bound element gists like a plain one (Phase 5 leak). The
        // contents are cloned out and the guard dropped before recursing: a
        // cycle can close through a cell (`my @e; @e.push(@e)`), and holding the
        // lock across the recursion would deadlock instead of reaching the cycle
        // detection above.
        ValueView::ContainerRef(cell) => {
            let inner = cell.lock().unwrap().clone();
            gist_value(&inner)
        }
        // Promise has no custom gist, so it gists in the default `.raku` form.
        ValueView::Promise(p) => {
            crate::builtins::methods_0arg::raku_repr::promise_raku_repr(&p.status())
        }
        // Channel likewise; its bare string value reads as a type object.
        ValueView::Channel(_) => "Channel.new".to_string(),
        ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _) => {
            // Rat.gist is identical to Rat.Str in Raku
            value.to_string_value()
        }
        ValueView::Array(_, crate::value::ArrayKind::Lazy) => {
            // A lazy (infinite-backed) array renders a bounded placeholder
            // rather than materializing its capped backing (Rakudo: `[...]`).
            "[...]".to_string()
        }
        ValueView::LazyList(ll) if ll.is_genuinely_lazy() => {
            // A genuinely-lazy list renders raku's placeholder without forcing:
            // `[...]` held in `@` array context, `(...)` for a bare Seq.
            crate::value::lazy_list_placeholder("gist", ll.in_array_context())
        }
        ValueView::Array(items, kind) => {
            let ptr = crate::gc::Gc::as_ptr(&items) as usize;
            // The `$id` Rakudo's `gistseen` names the node with is its type.
            let cycle_name = match kind {
                crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList => "List",
                _ => "Array",
            };
            let is_cycle = SEEN_PTRS.with(|seen| check_and_push(seen, ptr));
            if is_cycle {
                return format!("{}_{}", cycle_name, ptr);
            }
            // A real array's (`@`-sigiled) elements are Scalar containers, so a
            // cell can never hold `Nil` -- ADR-0049 decays a stored `Nil` to
            // the container's own default (`Any` for untyped) at the element
            // STORE, everywhere a value is written into a real array. This
            // used to compensate for that here at gist time instead (the
            // render-side half of the "Nil is a hole sentinel" collision the
            // ADR retires); it is unreachable now that the invariant holds at
            // the store, so plain `gist_value` is correct for every element,
            // real-array or not. A List/Seq keeps a genuine Nil (its own arm
            // in `gist_value` handles that directly).
            // Shaped arrays join their rows with a newline (`say my @a[2,2]`
            // prints one row per line), matching the fast-path gist.
            let sep = if kind == crate::value::ArrayKind::Shaped
                && items
                    .iter()
                    .any(|v| matches!(v.view(), ValueView::Array(..)))
            {
                "\n "
            } else {
                " "
            };
            let inner = items.iter().map(gist_value).collect::<Vec<_>>().join(sep);
            let looped = SEEN_PTRS.with(|seen| pop_ptr(seen, ptr));
            let rendered = match kind {
                crate::value::ArrayKind::Array
                | crate::value::ArrayKind::Shaped
                | crate::value::ArrayKind::Lazy
                | crate::value::ArrayKind::ItemArray => {
                    // .gist does NOT show the `$` prefix — only .raku does.
                    format!("[{}]", inner)
                }
                crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList => {
                    format!("({})", inner)
                }
            };
            if looped {
                cycle_binding(cycle_name, ptr, &rendered)
            } else {
                rendered
            }
        }
        ValueView::Hash(items) => {
            let ptr = crate::gc::Gc::as_ptr(&items) as usize;
            let cycle_name = if items.declared_type.as_deref() == Some("Map") {
                "Map"
            } else {
                "Hash"
            };
            let is_cycle = SEEN_PTRS.with(|seen| check_and_push(seen, ptr));
            if is_cycle {
                return format!("{}_{}", cycle_name, ptr);
            }
            let mut sorted_keys: Vec<&String> = items.keys().collect();
            sorted_keys.sort();
            // An object hash stores `.WHICH` keys — gist the original key
            // object (plain hashes gist the string key unchanged).
            let typed = items.has_typed_keys();
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| {
                    let key_gist = if typed {
                        gist_value(&items.typed_key(k))
                    } else {
                        (*k).clone()
                    };
                    format!("{} => {}", key_gist, gist_value(&items[*k]))
                })
                .collect();
            let looped = SEEN_PTRS.with(|seen| pop_ptr(seen, ptr));
            // An immutable Map gists as `Map.new((k => v, ...))`, not `{...}`
            // (matching raku and the `.raku` renderer). `Foo.enums`, `%h.Map`,
            // and `Map.new(...)` all carry the `Map` declared-type tag.
            let rendered = if items.declared_type.as_deref() == Some("Map") {
                format!("Map.new(({}))", parts.join(", "))
            } else {
                format!("{{{}}}", parts.join(", "))
            };
            if looped {
                cycle_binding(cycle_name, ptr, &rendered)
            } else {
                rendered
            }
        }
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            // Set/Bag/Mix gist shows the type-name wrapper, e.g. `Set(a b c)`;
            // their `.Str` (the `_` fall-through) shows only the bare elements.
            setbagmix_gist(value).unwrap_or_else(|| value.to_string_value())
        }
        ValueView::Pair(k, v) => format!("{} => {}", k, gist_value(v)),
        ValueView::ValuePair(k, v) => {
            // A Pair-valued key is parenthesized so the outer arrow is
            // unambiguous: `(red => 2) => apples`, matching raku's gist.
            let key_gist = match k.view() {
                ValueView::Pair(..) | ValueView::ValuePair(..) => {
                    format!("({})", gist_value(k))
                }
                _ => gist_value(k),
            };
            format!("{} => {}", key_gist, gist_value(v))
        }
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            format!(
                "({})",
                items.iter().map(gist_value).collect::<Vec<_>>().join(" ")
            )
        }
        ValueView::Slip(items) => {
            format!(
                "({})",
                items.iter().map(gist_value).collect::<Vec<_>>().join(" ")
            )
        }
        ValueView::Version { .. } => format!("v{}", value.to_string_value()),
        ValueView::Nil => "Nil".to_string(),
        // Range.gist is identical to Range.raku in Rakudo: it shows the range
        // notation (not the expanded elements), numeric endpoints render plainly,
        // string endpoints are quoted (`"a".."c"`), `i64::MAX`/Whatever endpoints
        // render as `Inf`/`-Inf`, and `0..^N` uses the `^N` short form. Delegate
        // to the raku renderer so all of this stays in sync.
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => {
            crate::builtins::methods_0arg::raku_repr::raku_value(value)
        }
        // A Match nested inside a container (e.g. the values of `$/.caps` or a
        // `m:g//` result list) must still gist as `｢matched｣` plus its sub-
        // captures, matching `Match.gist`. The generic Instance fall-through
        // below would otherwise stringify it to the bare matched text.
        ValueView::Instance { attributes, .. } if value.is_match_instance() => {
            match_gist(&(attributes).as_map(), 0)
        }
        // An `is Str` subclass instance gists as its string payload
        // (`Foo.new(:value("hi")).gist` → `hi`), not the generic `Class.new` —
        // `Str.gist` is the string itself.
        ValueView::Instance { attributes, .. } if attributes.contains_key("__mutsu_str_value") => {
            attributes
                .as_map()
                .get("__mutsu_str_value")
                .map(crate::value::Value::to_string_value)
                .unwrap_or_default()
        }
        // An `is Array` subclass instance gists as its backing array elements
        // (`Vector.new(1,2,3).gist` → `[1 2 3]`), not the generic `Class.new`.
        ValueView::Instance { attributes, .. }
            if attributes.contains_key("__mutsu_array_storage") =>
        {
            gist_value(
                &attributes
                    .as_map()
                    .get("__mutsu_array_storage")
                    .cloned()
                    .unwrap_or_else(|| crate::value::Value::real_array(Vec::new())),
            )
        }
        // An `is Hash`/`is Map` subclass instance gists as its backing hash
        // entries (`Bar.new(a=>1).gist` → `{a => 1}`), not the generic
        // `Class.new` — mirrors the `is Array` arm above.
        ValueView::Instance { attributes, .. }
            if attributes.contains_key("__mutsu_hash_storage") =>
        {
            gist_value(
                &attributes
                    .as_map()
                    .get("__mutsu_hash_storage")
                    .cloned()
                    .unwrap_or_else(|| crate::value::Value::hash(std::collections::HashMap::new())),
            )
        }
        // `$(...)` itemized container: `.gist` never shows the itemization sigil,
        // so it gists exactly like its inner value (`${a=>1}.gist` → `{a => 1}`).
        ValueView::Scalar(inner) => gist_value(inner),
        // An allomorph (IntStr/NumStr/…) gists as its preserved source string
        // (`<1e3>.gist` → `1e3`, not the inner Num's `1000`); a general mixin
        // gists via its inner value.
        ValueView::Mixin(inner, mixins) => {
            if crate::value::types::allomorph_type_name(inner, mixins).is_some()
                && let Some(str_val) = mixins.get("Str")
            {
                str_val.to_string_value()
            } else if let Some(rendered) =
                setbagmix_gist_named(inner, Some(&crate::value::types::what_type_name(value)))
            {
                // A quanthash names its type in its gist, and a `but`-mixed one
                // names the role with it: `Set+{R}(a)`. Every other kind gists
                // through its inner value unchanged.
                rendered
            } else {
                gist_value(inner)
            }
        }
        // A WhateverCode (`*+1`, `*.abs`) gists as `WhateverCode.new`, not the
        // empty string its bare closure stringification would yield.
        ValueView::Sub(data)
            if matches!(
                data.env.get("__mutsu_callable_type").map(Value::view),
                Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
            ) =>
        {
            "WhateverCode.new".to_string()
        }
        _ => value.to_string_value(),
    }
}

/// Render a Match's `.gist`: the corner-quoted matched text followed by its
/// positional and named sub-captures, each on its own indented line, ordered by
/// the capture's start position (`from`) and nested recursively. This mirrors
/// Rakudo's `Match.gist`:
///
/// ```text
/// ｢a1b2｣
///  0 => ｢a1｣
///   0 => ｢a｣
///   1 => ｢1｣
///  0 => ｢b2｣
///   0 => ｢b｣
///   1 => ｢2｣
/// ```
///
/// A quantified capture (`(\w)+`) is a list of Match values, each emitted under
/// the same index. `depth` controls indentation (one leading space per level).
pub(crate) fn match_gist(attributes: &AttrMap, depth: usize) -> String {
    // A failed `.subparse` Match renders as `#<failed match>` (Rakudo).
    if attributes
        .get("__failed_match__")
        .is_some_and(|v| v.truthy())
    {
        return "#<failed match>".to_string();
    }
    let matched = attributes
        .get("str")
        .map(|s| s.to_string_value())
        .unwrap_or_default();
    let mut out = format!("\u{FF62}{}\u{FF63}", matched);

    // Flatten captures into (from, label, match-value) entries so a quantified
    // capture contributes one entry per repetition, then order by match start
    // position (named and positional interleave by position).
    let mut entries: Vec<(i64, String, Value)> = Vec::new();
    let push_capture = |label: &str, value: &Value, entries: &mut Vec<(i64, String, Value)>| {
        match value.view() {
            ValueView::Instance { attributes, .. } if value.is_match_instance() => {
                entries.push((
                    match_from(&(attributes).as_map()),
                    label.to_string(),
                    value.clone(),
                ));
            }
            // Quantified capture: a list of Match values under one index.
            ValueView::Array(items, _) => {
                for item in items.iter() {
                    if let ValueView::Instance { attributes, .. } = item.view()
                        && item.is_match_instance()
                    {
                        entries.push((
                            match_from(&(attributes).as_map()),
                            label.to_string(),
                            item.clone(),
                        ));
                    }
                }
            }
            ValueView::Seq(_) | ValueView::Slip(_) => {
                let items = crate::runtime::utils::value_to_list(value);
                for item in items.iter() {
                    if let ValueView::Instance { attributes, .. } = item.view()
                        && item.is_match_instance()
                    {
                        entries.push((
                            match_from(&(attributes).as_map()),
                            label.to_string(),
                            item.clone(),
                        ));
                    }
                }
            }
            _ => {}
        }
    };

    if let Some(ValueView::Array(list, _)) = attributes.get("list").map(Value::view) {
        for (i, cap) in list.iter().enumerate() {
            push_capture(&i.to_string(), cap, &mut entries);
        }
    }
    if let Some(ValueView::Hash(named)) = attributes.get("named").map(Value::view) {
        let mut keys: Vec<&String> = named.keys().collect();
        keys.sort();
        for k in keys {
            if let Some(v) = named.get(k) {
                push_capture(k, v, &mut entries);
            }
        }
    }
    entries.sort_by_key(|(from, _, _)| *from);

    let indent = " ".repeat(depth + 1);
    for (_, label, val) in entries {
        if let ValueView::Instance { attributes, .. } = val.view() {
            out.push_str(&format!(
                "\n{}{} => {}",
                indent,
                label,
                match_gist(&(attributes).as_map(), depth + 1)
            ));
        }
    }
    out
}

/// The `from` (start offset) of a Match's attributes, or 0 when absent.
fn match_from(attributes: &AttrMap) -> i64 {
    match attributes.get("from").map(Value::view) {
        Some(ValueView::Int(n)) => n,
        _ => 0,
    }
}
