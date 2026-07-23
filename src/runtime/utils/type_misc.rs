use super::*;

pub(crate) fn value_type_name(value: &Value) -> &'static str {
    match value.view() {
        // A `VarRef` is a transient binder wrapper, not a type: report the type
        // of the variable's value.
        ValueView::VarRef { value, .. } => value_type_name(value),
        ValueView::RakuAst(node) => node.class.printed_name(),
        ValueView::Int(_) => "Int",
        ValueView::BigInt(_) => "Int",
        ValueView::Num(_) => "Num",
        ValueView::Str(_) => "Str",
        ValueView::Bool(_) => "Bool",
        ValueView::Array(_, kind) if kind.is_real_array() => "Array",
        ValueView::Array(_, _) => "List",
        // A `gather` block evaluates to a `Seq` in Raku; other lazy lists
        // (`lazy for`, arithmetic/closure sequences) present as `Array`/`List`.
        ValueView::LazyList(ll) if ll.is_from_gather() => "Seq",
        ValueView::LazyList(_) => "Array",
        ValueView::Hash(ref h) if h.declared_type.as_deref() == Some("Map") => "Map",
        ValueView::Hash(_) => "Hash",
        ValueView::Range(_, _)
        | ValueView::RangeExcl(_, _)
        | ValueView::RangeExclStart(_, _)
        | ValueView::RangeExclBoth(_, _)
        | ValueView::GenericRange { .. } => "Range",
        ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => "Pair",
        ValueView::Rat(_, _) => "Rat",
        ValueView::FatRat(_, _) => "FatRat",
        ValueView::BigRat(_, _) => {
            if value.is_bigfatrat() {
                "FatRat"
            } else {
                "Rat"
            }
        }
        ValueView::Complex(_, _) => "Complex",
        ValueView::Set(_, is_mutable) => {
            if is_mutable {
                "SetHash"
            } else {
                "Set"
            }
        }
        ValueView::Bag(_, is_mutable) => {
            if is_mutable {
                "BagHash"
            } else {
                "Bag"
            }
        }
        ValueView::Mix(_, is_mutable) => {
            if is_mutable {
                "MixHash"
            } else {
                "Mix"
            }
        }
        ValueView::Nil => "Nil",
        ValueView::Sub(data) => match data.env.get("__mutsu_callable_type").map(Value::view) {
            Some(ValueView::Str(kind)) if kind.as_str() == "Method" => "Method",
            Some(ValueView::Str(kind)) if kind.as_str() == "Submethod" => "Submethod",
            Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
            Some(ValueView::Str(kind)) if kind.as_str() == "Block" => "Block",
            _ => {
                if data.is_bare_block {
                    "Block"
                } else {
                    "Sub"
                }
            }
        },
        ValueView::WeakSub(_) => "Sub",
        ValueView::Routine { is_regex, .. } => {
            if is_regex {
                "Regex"
            } else {
                "Routine"
            }
        }
        ValueView::Package(_) => "Package",
        ValueView::CompUnitDepSpec { .. } => "Any",
        ValueView::Enum { .. } => "Int",
        ValueView::Instance { .. } => "Any",
        ValueView::Junction { .. } => "Junction",
        ValueView::Regex(_) | ValueView::RegexWithAdverbs { .. } => "Regex",
        ValueView::Version { .. } => "Version",
        ValueView::Seq(_) => "Seq",
        ValueView::HyperSeq(_) => "HyperSeq",
        ValueView::RaceSeq(_) => "RaceSeq",
        ValueView::Slip(_) => "Slip",
        ValueView::Promise(_) => "Promise",
        ValueView::Channel(_) => "Channel",
        ValueView::Whatever => "Whatever",
        ValueView::HyperWhatever => "HyperWhatever",
        ValueView::Capture { .. } => "Capture",
        ValueView::Uni(u) => match u.form.as_str() {
            "NFC" => "NFC",
            "NFD" => "NFD",
            "NFKC" => "NFKC",
            "NFKD" => "NFKD",
            _ => "Uni",
        },
        ValueView::Mixin(inner, mixins) => {
            if mixins.contains_key("Str") {
                match inner.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) => "IntStr",
                    ValueView::Num(_) => "NumStr",
                    ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _) => {
                        "RatStr"
                    }
                    ValueView::Complex(_, _) => "ComplexStr",
                    _ => value_type_name(inner),
                }
            } else {
                value_type_name(inner)
            }
        }
        ValueView::Proxy { .. } => "Proxy",
        ValueView::ParametricRole { .. } => "Package",
        ValueView::CustomType { .. } => "CustomType",
        ValueView::CustomTypeInstance(_) => "CustomTypeInstance",
        ValueView::Scalar(inner) => value_type_name(inner),
        ValueView::LazyThunk(thunk_data) => {
            let cache = thunk_data.cache.lock().unwrap();
            if let Some(ref cached) = *cache {
                // Leak the type name since we need a 'static str
                // This is fine because type names are a small finite set
                return value_type_name(cached);
            }
            "Scalar"
        }
        ValueView::LazyIoLines { .. } => "Seq",
        ValueView::HashEntryRef { .. } => value_type_name(&value.hash_entry_read()),
        ValueView::ContainerRef(_) => value.with_deref(value_type_name),
    }
}

pub(crate) fn is_chain_comparison_op(op: &str) -> bool {
    matches!(
        op,
        "==" | "!="
            | "<"
            | ">"
            | "<="
            | ">="
            | "==="
            | "!=="
            | "=:="
            | "eqv"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "before"
            | "after"
            | "~~"
            | "!~~"
            | "cmp"
            | "leg"
            | "<=>"
            | "%%"
            | "!%%"
    ) || matches!(
        op.strip_prefix('!'),
        Some("==")
            | Some("===")
            | Some("=:=")
            | Some("eqv")
            | Some("eq")
            | Some("ne")
            | Some("lt")
            | Some("gt")
            | Some("le")
            | Some("ge")
            | Some("before")
            | Some("after")
            | Some("cmp")
            | Some("leg")
            | Some("<=>")
    )
}

pub(crate) fn reduction_identity(op: &str) -> Value {
    if is_chain_comparison_op(op) {
        return Value::TRUE;
    }
    match op {
        "+" | "-" | "+|" | "+^" => Value::int(0),
        "*" | "**" => Value::int(1),
        "+&" => Value::int(-1), // +^0 (all bits set)
        "~" | "~|" | "~^" => Value::str(String::new()),
        "&&" | "and" | "?&" => Value::TRUE,
        "||" | "or" | "?|" | "^^" => Value::FALSE,
        "?^" => Value::FALSE,
        "//" | "orelse" => Value::package(Symbol::intern("Any")),
        "andthen" | "notandthen" => Value::TRUE,
        "xor" => Value::FALSE,
        "min" => Value::num(f64::INFINITY),
        "max" => Value::num(f64::NEG_INFINITY),
        // Junction operators
        "&" => Value::junction(crate::value::JunctionKind::All, Vec::new()),
        "|" => Value::junction(crate::value::JunctionKind::Any, Vec::new()),
        "^" => Value::junction(crate::value::JunctionKind::One, Vec::new()),
        // Set operators
        "(-)" | "∖" | "(|)" | "∪" | "(&)" | "∩" | "(^)" | "⊖" => Value::set(HashSet::new()),
        "(.)" | "⊍" | "(+)" | "⊎" => Value::bag(HashMap::new()),
        // Comma: empty list
        "," => Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
            ArrayKind::List,
        ),
        // Zip: empty Seq (Raku returns a Seq for arity-0 Z)
        "Z" => Value::seq(Vec::new()),
        _ => {
            // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>
            if let Some(inner) = strip_hyper_delimiters_for_identity(op) {
                return reduction_identity(inner);
            }
            Value::NIL
        }
    }
}

/// Strip hyper operator delimiters to find the inner operator for identity lookup.
fn strip_hyper_delimiters_for_identity(s: &str) -> Option<&str> {
    let after_left = s
        .strip_prefix(">>")
        .or_else(|| s.strip_prefix("<<"))
        .or_else(|| s.strip_prefix('\u{00BB}'))
        .or_else(|| s.strip_prefix('\u{00AB}'))?;
    let inner = after_left
        .strip_suffix(">>")
        .or_else(|| after_left.strip_suffix("<<"))
        .or_else(|| after_left.strip_suffix('\u{00BB}'))
        .or_else(|| after_left.strip_suffix('\u{00AB}'))?;
    if inner.is_empty() {
        return None;
    }
    Some(inner)
}
