//! `.WHICH` identity keys (object-hash keys, QuantHash element keys). A pure
//! function of the value, so it lives in `value` (#10779); `runtime::utils`
//! re-exports it.

use crate::symbol::Symbol;
use crate::value::type_name::value_type_name;
use crate::value::{Value, ValueView};

/// Compute the `.WHICH` string for a value, used as the internal key
/// in object hashes (`my %h{Any}`).
pub(crate) fn value_which_key(value: &Value) -> String {
    match value.view() {
        ValueView::Int(n) => format!("Int|{}", n),
        ValueView::BigInt(n) => format!("Int|{}", *n),
        ValueView::Num(n) => format!("Num|{}", n),
        ValueView::Str(s) => format!("Str|{}", *s),
        ValueView::Bool(b) => format!("Bool|{}", if b { 1 } else { 0 }),
        ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
        ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
        ValueView::BigRat(n, d) => {
            let flavour = if value.is_bigfatrat() {
                "FatRat"
            } else {
                "Rat"
            };
            format!("{}|{}/{}", flavour, n, d)
        }
        ValueView::Complex(r, i) => format!("Complex|{}|{}", r, i),
        ValueView::Nil => format!("Nil|U{}", Symbol::intern("Nil").id()),
        ValueView::Package(name) => format!("{}|U{}", name.resolve(), name.id()),
        ValueView::CustomType(c) => format!("{}|U{}", c.name.resolve(), c.id),
        // A class may override `WHICH` to give its instances value semantics
        // (`Set(A.new(a=>5)) eqv Set(A.new(a=>5))`). Running that user method
        // needs the interpreter, so it deposits the answer on the instance and
        // we read it here; without an override the identity is the object's own
        // id. See `InstanceAttrs::which_memo`.
        ValueView::Instance {
            class_name,
            attributes,
            id,
        } => match value.user_which_memo() {
            Some(which) => which.to_string(),
            None => match class_name.resolve().as_str() {
                "Date" => {
                    let (year, month, day) =
                        crate::value::temporal_core::date_attrs(&attributes.as_map());
                    format!(
                        "Date|{}",
                        crate::value::temporal_core::daycount(year, month, day)
                    )
                }
                "DateTime" => {
                    let (year, month, day, hour, minute, second, timezone) =
                        crate::value::temporal_core::datetime_attrs(&attributes.as_map());
                    format!(
                        "DateTime|{}",
                        crate::value::temporal_core::format_datetime(
                            year, month, day, hour, minute, second, timezone,
                        )
                    )
                }
                // An ObjAt is keyed by the identity it carries -- the same
                // string its `.WHICH` reports -- so every `$o.WHICH` of one
                // object is one Set/Bag element.
                cn @ ("ObjAt" | "ValueObjAt") => format!(
                    "{}|{}",
                    cn,
                    attributes.as_map().objat_which().unwrap_or_default()
                ),
                // A user subclass of Version keeps its built Version in
                // `__mutsu_version_value`; like Version itself (`Version|1.0`)
                // its identity is the class plus the canonical string.
                _ if attributes.as_map().contains_key("__mutsu_version_value") => format!(
                    "{}|{}",
                    class_name.resolve(),
                    attributes
                        .as_map()
                        .get("__mutsu_version_value")
                        .map(Value::to_string_value)
                        .unwrap_or_default()
                ),
                _ => format!("{}|{}", value_type_name(value), id),
            },
        },
        // Same never-reused id as the `.WHICH` twin in
        // `builtins::methods_0arg::dispatch_core_coerce` -- an address is
        // unique only among LIVE objects, and this string outlives them.
        ValueView::Array(items, ..) => format!("Array|{}", items.which_id.get()),
        ValueView::Hash(map) => format!("Hash|{}", map.which_id.get()),
        // A code object is a reference type: its identity is its id, exactly
        // as its `.WHICH` (`Block|16`) reports. The text fallback below renders
        // every Block alike, so two different closures collided as one
        // object-hash key and as one curried-role argument.
        ValueView::Sub(sub_data) => format!("{}|{}", value_type_name(value), sub_data.id),
        // A regex is a code object too; its payload is shared by every alias
        // and minted afresh by every evaluation of its literal.
        ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => {
            format!("Regex|{}", value.regex_which_id().unwrap_or_default())
        }
        // A Pair with a plain string key and a ValuePair holding a Str key are
        // the same identity (`("x" => 1) === (:x(1))`), so both render the key
        // through its own `.WHICH` (`Pair|Str|x|Int|1`, raku's format).
        ValueView::Pair(k, v) => format!("Pair|Str|{}|{}", k, value_which_key(v)),
        ValueView::ValuePair(k, v) => format!("Pair|{}|{}", value_which_key(k), value_which_key(v)),
        ValueView::Enum { enum_type, key, .. } => {
            format!("{}|{}", enum_type.resolve(), key.resolve())
        }
        // A `but`-mixed value (e.g. `"quux" but $role`) keeps the identity of
        // its base value but is a DISTINCT object per mixed-in role set — raku's
        // `.WHICH` is `Str+{<role>}|quux`. Fold the (sorted) mixin type/role
        // names into the key so two different roles over the same base value are
        // distinct object-hash keys, while the same value+role collides.
        //
        // An allomorph (IntStr/NumStr/RatStr/ComplexStr — a numeric inner with a
        // preserved `Str` part) keys by BOTH halves, matching raku's
        // `IntStr|Int|1|Str|1`: `IntStr.new(1, "one")` and `IntStr.new(1, "1")`
        // are distinct identities, which the role-name fold alone would collapse.
        ValueView::Mixin(inner, mixins) => {
            if let Some(allo_name) = crate::value::types::allomorph_type_name(inner, mixins) {
                let str_part = mixins
                    .get("Str")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                return format!("{}|{}|Str|{}", allo_name, value_which_key(inner), str_part);
            }
            let mut roles: Vec<&str> = mixins.keys().map(|s| s.as_str()).collect();
            roles.sort_unstable();
            format!(
                "{}+{{{}}}|{}",
                value_type_name(inner),
                roles.join(","),
                value_which_key(inner)
            )
        }
        _ => {
            format!("{}|{}", value_type_name(value), value.to_string_value())
        }
    }
}
