use super::*;

/// Returns the Raku type name for a value (used in error messages).
pub(crate) fn what_type_name(val: &Value) -> String {
    match val {
        Value(ValueRepr::Int(_)) | Value(ValueRepr::BigInt(_)) => "Int".to_string(),
        Value(ValueRepr::Num(_)) => "Num".to_string(),
        Value(ValueRepr::Str(_)) => "Str".to_string(),
        Value(ValueRepr::Bool(_)) => "Bool".to_string(),
        Value(ValueRepr::Rat(_, _)) | Value(ValueRepr::BigRat(_, _)) => "Rat".to_string(),
        Value(ValueRepr::FatRat(_, _)) => "FatRat".to_string(),
        Value(ValueRepr::Complex(_, _)) => "Complex".to_string(),
        Value(ValueRepr::Array(..)) | Value(ValueRepr::LazyList(_)) => "Array".to_string(),
        Value(ValueRepr::Seq(_)) => "Seq".to_string(),
        Value(ValueRepr::HyperSeq(_)) => "HyperSeq".to_string(),
        Value(ValueRepr::RaceSeq(_)) => "RaceSeq".to_string(),
        Value(ValueRepr::Hash(..)) => "Hash".to_string(),
        Value(ValueRepr::Set(_, is_mutable)) => {
            if *is_mutable {
                "SetHash".to_string()
            } else {
                "Set".to_string()
            }
        }
        Value(ValueRepr::Bag(_, is_mutable)) => {
            if *is_mutable {
                "BagHash".to_string()
            } else {
                "Bag".to_string()
            }
        }
        Value(ValueRepr::Mix(_, is_mutable)) => {
            if *is_mutable {
                "MixHash".to_string()
            } else {
                "Mix".to_string()
            }
        }
        Value(ValueRepr::Pair(_, _)) | Value(ValueRepr::ValuePair(_, _)) => "Pair".to_string(),
        Value(ValueRepr::Range(_, _))
        | Value(ValueRepr::RangeExcl(_, _))
        | Value(ValueRepr::RangeExclStart(_, _))
        | Value(ValueRepr::RangeExclBoth(_, _))
        | Value(ValueRepr::GenericRange { .. }) => "Range".to_string(),
        Value(ValueRepr::Nil) => "Nil".to_string(),
        Value(ValueRepr::Instance { class_name, .. }) => class_name.resolve(),
        Value(ValueRepr::Package(name)) => name.resolve(),
        Value(ValueRepr::Enum { enum_type, .. }) => enum_type.resolve(),
        Value(ValueRepr::Sub(_)) | Value(ValueRepr::WeakSub(_)) => "Sub".to_string(),
        Value(ValueRepr::Routine { .. }) => "Sub".to_string(),
        Value(ValueRepr::Regex(_)) => "Regex".to_string(),
        Value(ValueRepr::Junction { .. }) => "Junction".to_string(),
        Value(ValueRepr::Slip(_)) => "Slip".to_string(),
        Value(ValueRepr::Uni(u)) if !u.form.is_empty() => u.form.clone(),
        Value(ValueRepr::Uni(_)) => "Uni".to_string(),
        Value(ValueRepr::Mixin(inner, mixins)) => {
            allomorph_type_name(inner, mixins).unwrap_or_else(|| what_type_name(inner))
        }
        Value(ValueRepr::ContainerRef(_)) => val.with_deref(what_type_name),
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
        Value(ValueRepr::Int(_)) | Value(ValueRepr::BigInt(_)) => Some("IntStr".to_string()),
        Value(ValueRepr::Num(_)) => Some("NumStr".to_string()),
        Value(ValueRepr::Rat(_, _))
        | Value(ValueRepr::FatRat(_, _))
        | Value(ValueRepr::BigRat(_, _)) => Some("RatStr".to_string()),
        Value(ValueRepr::Complex(_, _)) => Some("ComplexStr".to_string()),
        _ => None,
    }
}
