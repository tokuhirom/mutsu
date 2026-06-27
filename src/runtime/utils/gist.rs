use super::*;

/// Render the `.gist` form of a Set/Bag/Mix (and their mutable `*Hash`
/// variants): `Set(a b c)`, `Bag(a b(2))`, `Mix(a(1.5) b)`. Keys are sorted
/// for deterministic output. Returns `None` for any other value. Shared by
/// `gist_value` (the fast say/gist path) and the `.gist` method dispatch so
/// both render identically.
pub(crate) fn setbagmix_gist(value: &Value) -> Option<String> {
    match value {
        Value::Set(s, mutable) => {
            let type_name = if *mutable { "SetHash" } else { "Set" };
            let mut keys: Vec<&String> = s.iter().collect();
            keys.sort();
            // Render each element via its original type's gist (so a Pair element
            // is `a => 1`, not the internal `a\t1` string key); a plain Str/Int
            // element gists bare, identical to the raw key.
            let inner = keys
                .iter()
                .map(|k| gist_value(&s.typed_key(k)))
                .collect::<Vec<_>>()
                .join(" ");
            Some(format!("{}({})", type_name, inner))
        }
        Value::Bag(b, mutable) => {
            let type_name = if *mutable { "BagHash" } else { "Bag" };
            let mut keys: Vec<(&String, &BigInt)> = b.iter().collect();
            keys.sort_by_key(|(k, _)| (*k).clone());
            let inner = keys
                .iter()
                .map(|(k, v)| {
                    let key = gist_value(&b.typed_key(k));
                    if **v == BigInt::from(1) {
                        key
                    } else {
                        format!("{}({})", key, v)
                    }
                })
                .collect::<Vec<_>>()
                .join(" ");
            Some(format!("{}({})", type_name, inner))
        }
        Value::Mix(m, mutable) => {
            let type_name = if *mutable { "MixHash" } else { "Mix" };
            let mut keys: Vec<(&String, &f64)> = m.iter().collect();
            keys.sort_by_key(|(k, _)| (*k).clone());
            let inner = keys
                .iter()
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
                .join(" ");
            Some(format!("{}({})", type_name, inner))
        }
        _ => None,
    }
}

pub(crate) fn gist_value(value: &Value) -> String {
    // Cycle detection for recursive data structures (shared hash/array Arcs).
    thread_local! {
        static SEEN_PTRS: std::cell::RefCell<Vec<usize>> = const { std::cell::RefCell::new(Vec::new()) };
    }
    fn check_and_push(ptrs: &std::cell::RefCell<Vec<usize>>, ptr: usize) -> bool {
        let mut s = ptrs.borrow_mut();
        if s.contains(&ptr) {
            return true; // cycle detected
        }
        s.push(ptr);
        false
    }
    fn pop_ptr(ptrs: &std::cell::RefCell<Vec<usize>>, ptr: usize) {
        let mut s = ptrs.borrow_mut();
        if let Some(pos) = s.iter().rposition(|p| *p == ptr) {
            s.remove(pos);
        }
    }
    match value {
        // A `:=`-bound element holds a `ContainerRef` cell; render the held
        // value so a bound element gists like a plain one (Phase 5 leak).
        Value::ContainerRef(cell) => gist_value(&cell.lock().unwrap()),
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => {
            // Rat.gist is identical to Rat.Str in Raku
            value.to_string_value()
        }
        Value::Array(_, crate::value::ArrayKind::Lazy) => {
            // A lazy (infinite-backed) array renders a bounded placeholder
            // rather than materializing its capped backing (Rakudo: `[...]`).
            "[...]".to_string()
        }
        Value::LazyList(ll) if ll.is_genuinely_lazy() => {
            // A genuinely-lazy list renders raku's placeholder without forcing:
            // `[...]` held in `@` array context, `(...)` for a bare Seq.
            crate::value::lazy_list_placeholder("gist", ll.in_array_context())
        }
        Value::Array(items, kind) => {
            let ptr = std::sync::Arc::as_ptr(items) as usize;
            let is_cycle = SEEN_PTRS.with(|seen| check_and_push(seen, ptr));
            if is_cycle {
                return match kind {
                    crate::value::ArrayKind::Array | crate::value::ArrayKind::Shaped => {
                        "[...]".to_string()
                    }
                    _ => "(...)".to_string(),
                };
            }
            let inner = items.iter().map(gist_value).collect::<Vec<_>>().join(" ");
            SEEN_PTRS.with(|seen| pop_ptr(seen, ptr));
            match kind {
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
            }
        }
        Value::Hash(items) => {
            let ptr = std::sync::Arc::as_ptr(items) as usize;
            let is_cycle = SEEN_PTRS.with(|seen| check_and_push(seen, ptr));
            if is_cycle {
                return "{...}".to_string();
            }
            let mut sorted_keys: Vec<&String> = items.keys().collect();
            sorted_keys.sort();
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| format!("{} => {}", k, gist_value(&items[*k])))
                .collect();
            SEEN_PTRS.with(|seen| pop_ptr(seen, ptr));
            format!("{{{}}}", parts.join(", "))
        }
        Value::Set(..) | Value::Bag(..) | Value::Mix(..) => {
            // Set/Bag/Mix gist shows the type-name wrapper, e.g. `Set(a b c)`;
            // their `.Str` (the `_` fall-through) shows only the bare elements.
            setbagmix_gist(value).unwrap_or_else(|| value.to_string_value())
        }
        Value::Pair(k, v) => format!("{} => {}", k, gist_value(v)),
        Value::ValuePair(k, v) => format!("{} => {}", gist_value(k), gist_value(v)),
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) | Value::Slip(items) => {
            format!(
                "({})",
                items.iter().map(gist_value).collect::<Vec<_>>().join(" ")
            )
        }
        Value::Version { .. } => format!("v{}", value.to_string_value()),
        Value::Nil => "Nil".to_string(),
        // Range.gist is identical to Range.raku in Rakudo: it shows the range
        // notation (not the expanded elements), numeric endpoints render plainly,
        // string endpoints are quoted (`"a".."c"`), `i64::MAX`/Whatever endpoints
        // render as `Inf`/`-Inf`, and `0..^N` uses the `^N` short form. Delegate
        // to the raku renderer so all of this stays in sync.
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => crate::builtins::methods_0arg::raku_repr::raku_value(value),
        // A Match nested inside a container (e.g. the values of `$/.caps` or a
        // `m:g//` result list) must still gist as `｢matched｣` plus its sub-
        // captures, matching `Match.gist`. The generic Instance fall-through
        // below would otherwise stringify it to the bare matched text.
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => match_gist(&(attributes).as_map(), 0),
        // `$(...)` itemized container: `.gist` never shows the itemization sigil,
        // so it gists exactly like its inner value (`${a=>1}.gist` → `{a => 1}`).
        Value::Scalar(inner) => gist_value(inner),
        // An allomorph (IntStr/NumStr/…) gists as its preserved source string
        // (`<1e3>.gist` → `1e3`, not the inner Num's `1000`); a general mixin
        // gists via its inner value.
        Value::Mixin(inner, mixins) => {
            if crate::value::types::allomorph_type_name(inner, mixins).is_some()
                && let Some(str_val) = mixins.get("Str")
            {
                str_val.to_string_value()
            } else {
                gist_value(inner)
            }
        }
        // A WhateverCode (`*+1`, `*.abs`) gists as `WhateverCode.new`, not the
        // empty string its bare closure stringification would yield.
        Value::Sub(data)
            if matches!(
                data.env.get("__mutsu_callable_type"),
                Some(Value::Str(kind)) if kind.as_str() == "WhateverCode"
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
pub(crate) fn match_gist(attributes: &HashMap<String, Value>, depth: usize) -> String {
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
        match value {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => {
                entries.push((
                    match_from(&(attributes).as_map()),
                    label.to_string(),
                    value.clone(),
                ));
            }
            // Quantified capture: a list of Match values under one index.
            Value::Array(items, _) => {
                for item in items.iter() {
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = item
                        && class_name == "Match"
                    {
                        entries.push((
                            match_from(&(attributes).as_map()),
                            label.to_string(),
                            item.clone(),
                        ));
                    }
                }
            }
            Value::Seq(items) | Value::Slip(items) => {
                for item in items.iter() {
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = item
                        && class_name == "Match"
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

    if let Some(Value::Array(list, _)) = attributes.get("list") {
        for (i, cap) in list.iter().enumerate() {
            push_capture(&i.to_string(), cap, &mut entries);
        }
    }
    if let Some(Value::Hash(named)) = attributes.get("named") {
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
        if let Value::Instance { attributes, .. } = &val {
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
fn match_from(attributes: &HashMap<String, Value>) -> i64 {
    match attributes.get("from") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    }
}
