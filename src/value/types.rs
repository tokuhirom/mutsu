use super::*;

/// Returns the Raku type name for a value (used in error messages).
pub(crate) fn what_type_name(val: &Value) -> String {
    match val {
        Value::Int(_) | Value::BigInt(_) => "Int".to_string(),
        Value::Num(_) => "Num".to_string(),
        Value::Str(_) => "Str".to_string(),
        Value::Bool(_) => "Bool".to_string(),
        Value::Rat(_, _) | Value::BigRat(_, _) => "Rat".to_string(),
        Value::FatRat(_, _) => "FatRat".to_string(),
        Value::Complex(_, _) => "Complex".to_string(),
        Value::Array(..) | Value::LazyList(_) => "Array".to_string(),
        Value::Seq(_) => "Seq".to_string(),
        Value::HyperSeq(_) => "HyperSeq".to_string(),
        Value::RaceSeq(_) => "RaceSeq".to_string(),
        Value::Hash(_) => "Hash".to_string(),
        Value::Set(_, is_mutable) => {
            if *is_mutable {
                "SetHash".to_string()
            } else {
                "Set".to_string()
            }
        }
        Value::Bag(_, is_mutable) => {
            if *is_mutable {
                "BagHash".to_string()
            } else {
                "Bag".to_string()
            }
        }
        Value::Mix(_, is_mutable) => {
            if *is_mutable {
                "MixHash".to_string()
            } else {
                "Mix".to_string()
            }
        }
        Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair".to_string(),
        Value::Range(_, _)
        | Value::RangeExcl(_, _)
        | Value::RangeExclStart(_, _)
        | Value::RangeExclBoth(_, _)
        | Value::GenericRange { .. } => "Range".to_string(),
        Value::Nil => "Nil".to_string(),
        Value::Instance { class_name, .. } => class_name.resolve(),
        Value::Package(name) => name.resolve(),
        Value::Enum { enum_type, .. } => enum_type.resolve(),
        Value::Sub(_) | Value::WeakSub(_) => "Sub".to_string(),
        Value::Routine { .. } => "Sub".to_string(),
        Value::Regex(_) => "Regex".to_string(),
        Value::Junction { .. } => "Junction".to_string(),
        Value::Slip(_) => "Slip".to_string(),
        Value::Uni(u) if !u.form.is_empty() => u.form.clone(),
        Value::Uni(_) => "Uni".to_string(),
        Value::Mixin(inner, mixins) => {
            allomorph_type_name(inner, mixins).unwrap_or_else(|| what_type_name(inner))
        }
        Value::ContainerRef(_) => val.with_deref(what_type_name),
        _ => "Any".to_string(),
    }
}

/// Return the allomorphic type name for a Mixin value, if it is allomorphic.
/// An allomorphic Mixin has a "Str" key and a numeric inner value.
pub(crate) fn allomorph_type_name(
    inner: &Value,
    mixins: &std::collections::HashMap<String, Value>,
) -> Option<String> {
    if !mixins.contains_key("Str") {
        return None;
    }
    match inner {
        Value::Int(_) | Value::BigInt(_) => Some("IntStr".to_string()),
        Value::Num(_) => Some("NumStr".to_string()),
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => Some("RatStr".to_string()),
        Value::Complex(_, _) => Some("ComplexStr".to_string()),
        _ => None,
    }
}
