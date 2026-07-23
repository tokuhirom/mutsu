/// Representation methods: gist, raku, perl
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

use super::raku_repr::{promise_raku_repr, raku_value};
use super::{format_temporal_num, gist_array_wrap, range_gist_string};

/// Rakudo caps an aggregate's `.gist` at the first 100 elements, then appends
/// ` ...`, so a huge array/list does not flood terminal output.
const GIST_ELEM_CAP: usize = 100;

/// Leaf-value gist for a list/array element: a WhateverCode (`*+1`) gists as
/// `WhateverCode.new`; everything else uses its string value.
fn leaf_gist(v: &Value) -> String {
    if let ValueView::Uni(u) = v.view() {
        // A Uni / normalization form gists as e.g. NFKC:0x<0066 0066>.
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
    v.to_string_value()
}

/// A collection whose gist embeds an element's gist must defer to the runtime
/// slow path when any element may carry a user-defined `method gist` (an
/// instance, custom type, or type object). The pure fast path here cannot
/// dispatch user methods, so it would render the default form. (Mixin is
/// excluded: a Mixin wrapping a List/Array renders via its inner value, so the
/// pure path is correct and dispatching `.gist` would add a spurious paren.)
fn gist_needs_method_dispatch(v: &Value) -> bool {
    match v.view() {
        ValueView::Instance { .. }
        | ValueView::CustomType(..)
        | ValueView::CustomTypeInstance(_)
        | ValueView::Package(..) => true,
        ValueView::Array(items, _) => items.iter().any(gist_needs_method_dispatch),
        ValueView::Seq(items) | ValueView::Slip(items) => {
            items.iter().any(gist_needs_method_dispatch)
        }
        ValueView::Hash(map) => map.values().any(gist_needs_method_dispatch),
        ValueView::Pair(_, val) => gist_needs_method_dispatch(val),
        ValueView::ValuePair(k, val) => {
            gist_needs_method_dispatch(k) || gist_needs_method_dispatch(val)
        }
        _ => false,
    }
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    if !matches!(method, "gist" | "raku" | "perl") {
        return None;
    }
    // Defer collection gist to the runtime slow path when an element may have a
    // custom `method gist`, so per-element gist dispatch is honored. `Match`
    // instances are excluded: their list gist is handled purely below.
    if method == "gist"
        && matches!(
            target.view(),
            ValueView::Array(..)
                | ValueView::Seq(..)
                | ValueView::Slip(..)
                | ValueView::Hash(..)
                | ValueView::Pair(..)
                | ValueView::ValuePair(..)
        )
        && gist_needs_method_dispatch(target)
    {
        return Some(None);
    }
    Some(match target.view() {
        // A parameterized role type object: raku is the bare name
        // (`Cup[EggNog]`), gist wraps it in type-object parens.
        ValueView::ParametricRole { .. } => {
            let name = raku_value(target);
            if method == "gist" {
                Some(Ok(Value::str(format!("({})", name))))
            } else {
                Some(Ok(Value::str(name)))
            }
        }
        ValueView::Bool(b) => {
            if method == "gist" {
                Some(Ok(Value::str(if b { "True" } else { "False" }.to_string())))
            } else {
                Some(Ok(Value::str(
                    if b { "Bool::True" } else { "Bool::False" }.to_string(),
                )))
            }
        }
        // raku/perl and gist of the Nil value are all "Nil". (Uninitialized
        // variables hold the Any type object since PLAN 8.5, so a runtime
        // Nil here is a genuine Nil, not an uninit placeholder.)
        ValueView::Nil => Some(Ok(Value::str_from("Nil"))),
        ValueView::FatRat(n, d) => {
            if d == 0 && (method == "gist" || method == "Str") {
                Some(Err(RuntimeError::numeric_divide_by_zero_with(Some(
                    Value::int(n),
                ))))
            } else if method == "gist" {
                Some(Ok(Value::str(target.to_string_value())))
            } else {
                Some(Ok(Value::str(format!("FatRat.new({}, {})", n, d))))
            }
        }
        ValueView::BigRat(n, d) => {
            use num_traits::Zero;
            if d.is_zero() && (method == "gist" || method == "Str") {
                Some(Err(RuntimeError::numeric_divide_by_zero_with(Some(
                    Value::from_bigint(n.clone()),
                ))))
            } else if method == "gist" {
                Some(Ok(Value::str(target.to_string_value())))
            } else if target.is_bigfatrat() {
                Some(Ok(Value::str(format!("FatRat.new({}, {})", n, d))))
            } else {
                Some(Ok(Value::str(raku_value(target))))
            }
        }
        ValueView::Rat(n, d) => {
            if d == 0 {
                if method == "raku" || method == "perl" {
                    Some(Ok(Value::str(format!("<{}/0>", n))))
                } else {
                    Some(Err(RuntimeError::numeric_divide_by_zero_with(Some(
                        Value::int(n),
                    ))))
                }
            } else if n % d == 0 {
                if method == "raku" || method == "perl" {
                    // .raku on Rat always shows decimal: 27.0, not 27
                    Some(Ok(Value::str(format!("{}.0", n / d))))
                } else {
                    Some(Ok(Value::str(format!("{}", n / d))))
                }
            } else {
                if method == "gist" {
                    return Some(Some(Ok(Value::str(target.to_string_value()))));
                }
                let mut dd = d;
                while dd % 2 == 0 {
                    dd /= 2;
                }
                while dd % 5 == 0 {
                    dd /= 5;
                }
                if dd == 1 {
                    let val = n as f64 / d as f64;
                    Some(Ok(Value::str(format!("{}", val))))
                } else {
                    Some(Ok(Value::str(format!("<{}/{}>", n, d))))
                }
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Signature" => {
            let attr_key = if method == "gist" { "gist" } else { "raku" };
            Some(Ok(attributes
                .as_map()
                .get(attr_key)
                .cloned()
                .unwrap_or_else(|| Value::str(format!("{}()", class_name)))))
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Parameter" && (method == "raku" || method == "perl") => {
            Some(Ok(Value::str(crate::value::signature::parameter_to_raku(
                &(attributes).as_map(),
            ))))
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" => {
            let msg = attributes
                .as_map()
                .get("exception")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Failed".to_string());
            if method == "gist" {
                Some(Ok(Value::str(msg)))
            } else if method == "raku" || method == "perl" {
                let raku_str = if target.is_failure_handled() {
                    // For handled Failures, produce an expression that when
                    // EVALed creates a handled Failure, preserving the flag.
                    format!("do {{ my $f = Failure.new(\"{}\"); $f.Bool; $f }}", msg)
                } else {
                    format!("Failure.new(\"{}\")", msg)
                };
                Some(Ok(Value::str(raku_str)))
            } else {
                // Str, Numeric, Int, etc. -- using a Failure in a value
                // context throws the wrapped exception.
                if let Some(ex) = attributes.as_map().get("exception") {
                    Some(Err(RuntimeError::from_exception_value(ex.clone())))
                } else {
                    Some(Err(RuntimeError::new(&msg)))
                }
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "CallFrame" => {
            if method == "gist" {
                let file = attributes
                    .as_map()
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let line = attributes
                    .as_map()
                    .get("line")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "0".to_string());
                Some(Ok(Value::str(format!("{} at line {}", file, line))))
            } else {
                // .raku: CallFrame.new(...)
                let file = attributes
                    .as_map()
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let line = attributes
                    .as_map()
                    .get("line")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "0".to_string());
                Some(Ok(Value::str(format!(
                    "CallFrame.new(annotations => {{:file(\"{}\"), :line(\"{}\")}}, my => {{}})",
                    file, line
                ))))
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
            if let Some(ValueView::Array(bytes, ..)) =
                attributes.as_map().get("bytes").map(Value::view)
            {
                if method == "raku" || method == "perl" {
                    let elems: Vec<String> = bytes
                        .iter()
                        .map(|b| match b.view() {
                            ValueView::Int(i) => i.to_string(),
                            _ => "0".to_string(),
                        })
                        .collect();
                    // Normalize short names to canonical forms for .raku
                    let cn = class_name.resolve();
                    let canonical = match cn.as_str() {
                        "buf8" => "Buf[uint8]",
                        "buf16" => "Buf[uint16]",
                        "buf32" => "Buf[uint32]",
                        "buf64" => "Buf[uint64]",
                        "blob8" => "Blob[uint8]",
                        "blob16" => "Blob[uint16]",
                        "blob32" => "Blob[uint32]",
                        "blob64" => "Blob[uint64]",
                        other => other,
                    };
                    Some(Ok(Value::str(format!(
                        "{}.new({})",
                        canonical,
                        elems.join(",")
                    ))))
                } else {
                    // gist — show at most 100 elements, append "..." if truncated.
                    // An empty Blob/Buf instance gists as `Blob:0x<>` (empty hex
                    // body), not `Blob()` — the latter is the type-object spelling.
                    if bytes.is_empty() {
                        Some(Ok(Value::str(format!("{}:0x<>", class_name))))
                    } else {
                        let cn = class_name.resolve();
                        let hex_width = if cn.contains("64") {
                            16
                        } else if cn.contains("32") {
                            8
                        } else if cn.contains("16") {
                            4
                        } else {
                            2
                        };
                        let truncated = bytes.len() > 100;
                        let display_bytes = if truncated { &bytes[..100] } else { &bytes[..] };
                        let mut hex: Vec<String> = display_bytes
                            .iter()
                            .map(|b| match b.view() {
                                ValueView::Int(i) => match hex_width {
                                    16 => format!("{:016X}", i as u64),
                                    8 => format!("{:08X}", i as u32),
                                    4 => format!("{:04X}", i as u16),
                                    _ => format!("{:02X}", i as u8),
                                },
                                _ => "0".repeat(hex_width),
                            })
                            .collect();
                        if truncated {
                            hex.push("...".to_string());
                        }
                        Some(Ok(Value::str(format!(
                            "{}:0x<{}>",
                            class_name,
                            hex.join(" ")
                        ))))
                    }
                }
            } else {
                Some(Ok(Value::str(format!("{}()", class_name))))
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => {
            if method == "gist" {
                // `Instant:<tai>` — identical to the Str/gist rendering in
                // `value/display.rs::to_string_value`.
                Some(Ok(Value::str(target.to_string_value())))
            } else {
                let tai = attributes
                    .as_map()
                    .get("value")
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0);
                let posix = crate::builtins::methods_0arg::temporal::instant_to_posix(tai);
                Some(Ok(Value::str(format!(
                    "Instant.from-posix({})",
                    format_temporal_num(posix)
                ))))
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" => {
            if method == "gist" {
                Some(Ok(Value::str(target.to_string_value())))
            } else {
                let val = attributes
                    .as_map()
                    .get("value")
                    .cloned()
                    .unwrap_or(Value::num(0.0));
                Some(Ok(Value::str(format!(
                    "Duration.new({})",
                    format_temporal_num(val.to_f64())
                ))))
            }
        }
        ValueView::Bag(_, _) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(
                    super::raku_repr::setbagmix_raku(target).unwrap(),
                )))
            } else {
                // gist: Bag(key(count) ...) or BagHash(key(count) ...)
                Some(Ok(Value::str(
                    runtime::utils::setbagmix_gist(target).unwrap(),
                )))
            }
        }
        ValueView::Set(_, _) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(
                    super::raku_repr::setbagmix_raku(target).unwrap(),
                )))
            } else {
                // gist: Set(a b c) or SetHash(a b c)
                Some(Ok(Value::str(
                    runtime::utils::setbagmix_gist(target).unwrap(),
                )))
            }
        }
        ValueView::Mix(_, _) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(
                    super::raku_repr::setbagmix_raku(target).unwrap(),
                )))
            } else {
                // gist: Mix(key(weight) ...) or MixHash(key(weight) ...)
                Some(Ok(Value::str(
                    runtime::utils::setbagmix_gist(target).unwrap(),
                )))
            }
        }
        ValueView::Package(name) => {
            let resolved = name.resolve();
            let full = crate::value::user_facing_type_name(&resolved);
            if method == "gist" {
                let short = full.rsplit("::").next().unwrap_or(&full);
                Some(Ok(Value::str(format!("({})", short))))
            } else {
                // .raku returns the full type name
                Some(Ok(Value::str(full.to_string())))
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Stash" && (method == "gist" || method == "Str") => {
            // Stash.gist and Stash.Str return the package name
            if let Some(ValueView::Str(name)) = attributes.as_map().get("name").map(Value::view) {
                Some(Ok(Value::str(name.to_string())))
            } else {
                Some(Ok(Value::str(String::new())))
            }
        }
        ValueView::Instance { .. } | ValueView::Enum { .. } => None,
        ValueView::Version { parts, plus, minus } => {
            let s = Value::version_parts_to_string(parts);
            let suffix = if plus {
                "+"
            } else if minus {
                "-"
            } else {
                ""
            };
            let full = format!("{}{}", s, suffix);
            // .gist always keeps the `v` prefix (`vTrue`); only .raku/.perl
            // switch to the constructor form for a non-literal version.
            if method == "gist" {
                Some(Ok(Value::str(format!("v{}", full))))
            } else {
                Some(Ok(Value::str(super::raku_repr::version_raku_repr(&full))))
            }
        }
        ValueView::Str(s) => {
            if method == "raku" || method == "perl" {
                // .raku wraps strings in quotes and escapes special chars
                Some(Ok(Value::str(super::raku_repr::escape_raku_str(&s))))
            } else {
                Some(Ok(Value::str_arc(s.clone())))
            }
        }
        ValueView::Array(_, kind) if method == "raku" || method == "perl" => {
            if kind == crate::value::ArrayKind::Lazy {
                Some(Ok(Value::str_from("[...]")))
            } else {
                Some(Ok(Value::str(raku_value(target))))
            }
        }
        ValueView::Seq(_) if method == "raku" || method == "perl" => {
            Some(Ok(Value::str(raku_value(target))))
        }
        ValueView::Array(_, kind) if method == "gist" && kind == crate::value::ArrayKind::Lazy => {
            // A lazy (infinite-backed) array renders a bounded placeholder
            // rather than materializing its (possibly capped 100k) backing,
            // matching Rakudo's `[...]`.
            Some(Ok(Value::str_from("[...]")))
        }
        ValueView::Array(items, kind) if method == "gist" => {
            fn gist_item(v: &Value) -> String {
                match v.view() {
                    ValueView::Nil => "Nil".to_string(),
                    ValueView::ContainerRef(cell) => gist_item(&cell.lock().unwrap()),
                    // `$(...)` itemized element: `.gist` drops the itemization
                    // sigil, so it gists like its inner value.
                    ValueView::Scalar(inner) => gist_item(inner),
                    ValueView::Array(_, crate::value::ArrayKind::Lazy) => "[...]".to_string(),
                    ValueView::LazyList(ll) if ll.is_genuinely_lazy() => {
                        crate::value::lazy_list_placeholder("gist", ll.in_array_context())
                    }
                    ValueView::Array(inner, kind) => {
                        let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                        gist_array_wrap(&elems, kind)
                    }
                    ValueView::Seq(inner) | ValueView::Slip(inner) => {
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
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Match" => {
                        crate::runtime::utils::match_gist(&(attributes).as_map(), 0)
                    }
                    ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
                        // A Set/Bag/Mix nested in a list/array gist keeps its
                        // type-name wrapper (`Set(a b c)`), like the say/gist
                        // fast path, rather than its bare-element `.Str` form.
                        crate::runtime::utils::setbagmix_gist(v)
                            .unwrap_or_else(|| v.to_string_value())
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
            let elem_render: fn(&Value) -> String = if kind.is_real_array() {
                gist_real_array_item
            } else {
                gist_item
            };
            // Shaped arrays: format with newlines between rows
            if kind == crate::value::ArrayKind::Shaped
                && items
                    .iter()
                    .any(|v| matches!(v.view(), ValueView::Array(..)))
            {
                let rows: Vec<String> = items.iter().map(elem_render).collect();
                let inner = rows.join("\n ");
                return Some(Some(Ok(Value::str(format!("[{}]", inner)))));
            }
            // `.gist` shows at most the first 100 elements, then ` ...`
            // (Rakudo caps aggregate gists so a huge array does not flood output).
            // Only the first 100 are rendered when truncating.
            let inner = if items.len() > GIST_ELEM_CAP {
                let mut s = items[..GIST_ELEM_CAP]
                    .iter()
                    .map(elem_render)
                    .collect::<Vec<_>>()
                    .join(" ");
                s.push_str(" ...");
                s
            } else {
                items.iter().map(elem_render).collect::<Vec<_>>().join(" ")
            };
            Some(Ok(Value::str(gist_array_wrap(&inner, kind))))
        }
        ValueView::Seq(items) | ValueView::Slip(items) if method == "gist" => {
            fn gist_item(v: &Value) -> String {
                match v.view() {
                    ValueView::Nil => "Nil".to_string(),
                    ValueView::ContainerRef(cell) => gist_item(&cell.lock().unwrap()),
                    // `$(...)` itemized element: `.gist` drops the itemization
                    // sigil, so it gists like its inner value.
                    ValueView::Scalar(inner) => gist_item(inner),
                    ValueView::Array(_, crate::value::ArrayKind::Lazy) => "[...]".to_string(),
                    ValueView::LazyList(ll) if ll.is_genuinely_lazy() => {
                        crate::value::lazy_list_placeholder("gist", ll.in_array_context())
                    }
                    ValueView::Array(inner, kind) => {
                        let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                        gist_array_wrap(&elems, kind)
                    }
                    ValueView::Seq(inner) | ValueView::Slip(inner) => {
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
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Match" => {
                        crate::runtime::utils::match_gist(&(attributes).as_map(), 0)
                    }
                    ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
                        // A Set/Bag/Mix nested in a list/array gist keeps its
                        // type-name wrapper (`Set(a b c)`), like the say/gist
                        // fast path, rather than its bare-element `.Str` form.
                        crate::runtime::utils::setbagmix_gist(v)
                            .unwrap_or_else(|| v.to_string_value())
                    }
                    _ if v.is_range() => range_gist_string(v),
                    _ => leaf_gist(v),
                }
            }
            // Like the Array path: a Seq/List gist shows at most the first 100
            // elements, then ` ...`.
            let inner = if items.len() > GIST_ELEM_CAP {
                let mut s = items[..GIST_ELEM_CAP]
                    .iter()
                    .map(gist_item)
                    .collect::<Vec<_>>()
                    .join(" ");
                s.push_str(" ...");
                s
            } else {
                items.iter().map(gist_item).collect::<Vec<_>>().join(" ")
            };
            Some(Ok(Value::str(format!("({})", inner))))
        }
        ValueView::Slip(items) if method == "raku" || method == "perl" => {
            if items.is_empty() {
                // Empty slip is represented as "Empty" in Raku
                Some(Ok(Value::str_from("Empty")))
            } else {
                let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
                // A one-element slip keeps the list's trailing comma: `slip(3,)`.
                if items.len() == 1 {
                    Some(Ok(Value::str(format!("slip({},)", inner))))
                } else {
                    Some(Ok(Value::str(format!("slip({})", inner))))
                }
            }
        }
        ValueView::Junction { .. } if method == "raku" || method == "perl" => None,
        // A WhateverCode (`*+1`, `*.abs`) renders as `WhateverCode.new` for
        // `.gist`/`.raku`/`.perl`, not the generic closure form.
        ValueView::Sub(data)
            if (method == "raku" || method == "perl" || method == "gist")
                && matches!(
                    data.env.get("__mutsu_callable_type").map(Value::view),
                    Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
                ) =>
        {
            Some(Ok(Value::str_from("WhateverCode.new")))
        }
        // Sub/Routine/WeakSub: delegate to interpreter for proper raku/gist/Str
        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => None,
        ValueView::Pair(k, v) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist: use " => " separator
                Some(Ok(Value::str(format!(
                    "{} => {}",
                    k,
                    runtime::gist_value(v)
                ))))
            }
        }
        ValueView::ValuePair(k, v) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist: use " => " separator. A Pair-valued key is parenthesized
                // so the outer arrow is unambiguous (`(red => 2) => apples`),
                // matching raku's gist and the `gist_value` fast path.
                let key_gist = match k.view() {
                    ValueView::Pair(..) | ValueView::ValuePair(..) => {
                        format!("({})", runtime::gist_value(k))
                    }
                    _ => runtime::gist_value(k),
                };
                Some(Ok(Value::str(format!(
                    "{} => {}",
                    key_gist,
                    runtime::gist_value(v)
                ))))
            }
        }
        ValueView::BigInt(i) => {
            // A BigInt is an integer (its `.^name` is `Int`); `.raku` must render
            // the plain integer, not a float (`100000000000000000000`, not
            // `100000000000000000000.0`), so it round-trips as an Int.
            Some(Ok(Value::str(i.to_string())))
        }
        ValueView::Int(i) => Some(Ok(Value::str(format!("{}", i)))),
        ValueView::Num(_f) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist == Str for a Num: use the canonical Num→Str formatter,
                // which renders Inf/-Inf/NaN with their proper casing and applies
                // scientific notation for very large / small magnitudes. The raw
                // `format!("{}", f)` produced Rust's lowercase `inf`/`-inf` and
                // never switched to scientific (e.g. `1e20.gist` was the full
                // 21-digit integer instead of `1e+20`).
                Some(Ok(Value::str(target.to_string_value())))
            }
        }
        ValueView::Complex(r, i) => {
            if method == "raku" || method == "perl" {
                Some(Ok(Value::str(format!(
                    "<{}>",
                    crate::value::format_complex(r, i)
                ))))
            } else {
                Some(Ok(Value::str(crate::value::format_complex(r, i))))
            }
        }
        ValueView::Hash(map) => {
            if method == "raku" || method == "perl" {
                // Delegate to raku_value which has cycle detection for
                // self-referencing hashes (e.g. %h<b> = %h).
                Some(Ok(Value::str(raku_value(target))))
            } else {
                // gist
                let mut sorted_keys: Vec<&String> = map.keys().collect();
                sorted_keys.sort();
                let parts: Vec<String> = sorted_keys
                    .iter()
                    .map(|k| {
                        // Object hashes store `.WHICH` string keys; show the
                        // original typed key (e.g. `a`, not `Str|a`).
                        let key_disp = runtime::gist_value(&map.typed_key(k));
                        format!("{} => {}", key_disp, runtime::gist_value(&map[*k]))
                    })
                    .collect();
                Some(Ok(Value::str(format!("{{{}}}", parts.join(", ")))))
            }
        }
        _ if target.is_range() && (method == "gist" || method == "raku" || method == "perl") => {
            Some(Ok(Value::str(if method == "gist" {
                range_gist_string(target)
            } else {
                raku_value(target)
            })))
        }
        // A genuinely-lazy (infinite) list renders a `(...)`/`[...]`/`...`
        // placeholder rather than materializing — e.g. `[2,3].roll(*).gist`
        // (raku: `(...)`). Finite/`cat_pull` lazy lists fall through to force.
        ValueView::LazyList(ll)
            if (method == "gist" || method == "raku" || method == "perl")
                && ll.renders_lazy_placeholder() =>
        {
            Some(Ok(Value::str(crate::value::lazy_list_placeholder(
                method,
                ll.in_array_context(),
            ))))
        }
        ValueView::LazyList(_) => None, // fall through to runtime to force
        // Promise has no custom gist, so `.gist` is the default `.raku` form.
        ValueView::Promise(p) => Some(Ok(Value::str(promise_raku_repr(&p.status())))),
        // Channel likewise; its bare string value reads as a type object.
        ValueView::Channel(_) => Some(Ok(Value::str_from("Channel.new"))),
        _ => Some(Ok(Value::str(target.to_string_value()))),
    })
}
