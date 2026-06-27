use super::*;

pub(crate) fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Int(_) => "Int",
        Value::BigInt(_) => "Int",
        Value::Num(_) => "Num",
        Value::Str(_) => "Str",
        Value::Bool(_) => "Bool",
        Value::Array(_, kind) if kind.is_real_array() => "Array",
        Value::Array(_, _) => "List",
        Value::LazyList(_) => "Array",
        Value::Hash(_) => "Hash",
        Value::Range(_, _)
        | Value::RangeExcl(_, _)
        | Value::RangeExclStart(_, _)
        | Value::RangeExclBoth(_, _)
        | Value::GenericRange { .. } => "Range",
        Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
        Value::Rat(_, _) => "Rat",
        Value::FatRat(_, _) => "FatRat",
        Value::BigRat(_, _) => "Rat",
        Value::Complex(_, _) => "Complex",
        Value::Set(_, is_mutable) => {
            if *is_mutable {
                "SetHash"
            } else {
                "Set"
            }
        }
        Value::Bag(_, is_mutable) => {
            if *is_mutable {
                "BagHash"
            } else {
                "Bag"
            }
        }
        Value::Mix(_, is_mutable) => {
            if *is_mutable {
                "MixHash"
            } else {
                "Mix"
            }
        }
        Value::Nil => "Any",
        Value::Sub(data) => match data.env.get("__mutsu_callable_type") {
            Some(Value::Str(kind)) if kind.as_str() == "Method" => "Method",
            Some(Value::Str(kind)) if kind.as_str() == "Submethod" => "Submethod",
            Some(Value::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
            Some(Value::Str(kind)) if kind.as_str() == "Block" => "Block",
            _ => {
                if data.is_bare_block {
                    "Block"
                } else {
                    "Sub"
                }
            }
        },
        Value::WeakSub(_) => "Sub",
        Value::Routine { is_regex, .. } => {
            if *is_regex {
                "Regex"
            } else {
                "Routine"
            }
        }
        Value::Package(_) => "Package",
        Value::CompUnitDepSpec { .. } => "Any",
        Value::Enum { .. } => "Int",
        Value::Instance { .. } => "Any",
        Value::Junction { .. } => "Junction",
        Value::Regex(_) | Value::RegexWithAdverbs { .. } => "Regex",
        Value::Version { .. } => "Version",
        Value::Seq(_) => "Seq",
        Value::HyperSeq(_) => "HyperSeq",
        Value::RaceSeq(_) => "RaceSeq",
        Value::Slip(_) => "Slip",
        Value::Promise(_) => "Promise",
        Value::Channel(_) => "Channel",
        Value::Whatever => "Whatever",
        Value::HyperWhatever => "HyperWhatever",
        Value::Capture { positional, named } => {
            // Unwrap internal VarRef captures used by the VM for rw binding
            if positional.is_empty()
                && named
                    .get("__mutsu_varref_name")
                    .is_some_and(|v| matches!(v, Value::Str(_)))
                && let Some(inner) = named.get("__mutsu_varref_value")
            {
                return value_type_name(inner);
            }
            "Capture"
        }
        Value::Uni(u) => match u.form.as_str() {
            "NFC" => "NFC",
            "NFD" => "NFD",
            "NFKC" => "NFKC",
            "NFKD" => "NFKD",
            _ => "Uni",
        },
        Value::Mixin(inner, mixins) => {
            if mixins.contains_key("Str") {
                match inner.as_ref() {
                    Value::Int(_) | Value::BigInt(_) => "IntStr",
                    Value::Num(_) => "NumStr",
                    Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => "RatStr",
                    Value::Complex(_, _) => "ComplexStr",
                    _ => value_type_name(inner),
                }
            } else {
                value_type_name(inner)
            }
        }
        Value::Proxy { .. } => "Proxy",
        Value::ParametricRole { .. } => "Package",
        Value::CustomType { .. } => "CustomType",
        Value::CustomTypeInstance(_) => "CustomTypeInstance",
        Value::Scalar(inner) => value_type_name(inner),
        Value::LazyThunk(thunk_data) => {
            let cache = thunk_data.cache.lock().unwrap();
            if let Some(ref cached) = *cache {
                // Leak the type name since we need a 'static str
                // This is fine because type names are a small finite set
                return value_type_name(cached);
            }
            "Scalar"
        }
        Value::LazyIoLines { .. } => "Seq",
        Value::HashEntryRef { .. } => value_type_name(&value.hash_entry_read()),
        Value::ContainerRef(_) => value.with_deref(value_type_name),
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
        return Value::Bool(true);
    }
    match op {
        "+" | "-" | "+|" | "+^" => Value::Int(0),
        "*" | "**" => Value::Int(1),
        "+&" => Value::Int(-1), // +^0 (all bits set)
        "~" | "~|" | "~^" => Value::str(String::new()),
        "&&" | "and" | "?&" => Value::Bool(true),
        "||" | "or" | "?|" | "^^" => Value::Bool(false),
        "?^" => Value::Bool(false),
        "//" | "orelse" => Value::Package(Symbol::intern("Any")),
        "andthen" => Value::Bool(true),
        "xor" => Value::Bool(false),
        "min" => Value::Num(f64::INFINITY),
        "max" => Value::Num(f64::NEG_INFINITY),
        // Junction operators
        "&" => Value::Junction {
            kind: crate::value::JunctionKind::All,
            values: std::sync::Arc::new(Vec::new()),
        },
        "|" => Value::Junction {
            kind: crate::value::JunctionKind::Any,
            values: std::sync::Arc::new(Vec::new()),
        },
        "^" => Value::Junction {
            kind: crate::value::JunctionKind::One,
            values: std::sync::Arc::new(Vec::new()),
        },
        // Set operators
        "(-)" | "∖" | "(|)" | "∪" | "(&)" | "∩" | "(^)" | "⊖" => Value::set(HashSet::new()),
        "(.)" | "⊍" | "(+)" | "⊎" => Value::bag(HashMap::new()),
        // Comma: empty list
        "," => Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(Vec::new())),
            ArrayKind::List,
        ),
        // Zip: empty Seq (Raku returns a Seq for arity-0 Z)
        "Z" => Value::Seq(std::sync::Arc::new(Vec::new())),
        _ => {
            // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>
            if let Some(inner) = strip_hyper_delimiters_for_identity(op) {
                return reduction_identity(inner);
            }
            Value::Nil
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
