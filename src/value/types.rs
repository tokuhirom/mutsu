use super::*;

/// Returns the Raku type name for a value (used in error messages).
pub(crate) fn what_type_name(val: &Value) -> String {
    match val.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => "Int".to_string(),
        ValueView::Num(_) => "Num".to_string(),
        ValueView::Str(_) => "Str".to_string(),
        ValueView::Bool(_) => "Bool".to_string(),
        ValueView::Rat(_, _) | ValueView::BigRat(_, _) => "Rat".to_string(),
        ValueView::FatRat(_, _) => "FatRat".to_string(),
        ValueView::Complex(_, _) => "Complex".to_string(),
        ValueView::Array(..) | ValueView::LazyList(_) => "Array".to_string(),
        ValueView::Seq(_) => "Seq".to_string(),
        ValueView::HyperSeq(_) => "HyperSeq".to_string(),
        ValueView::RaceSeq(_) => "RaceSeq".to_string(),
        ValueView::Hash(..) => "Hash".to_string(),
        ValueView::Set(_, is_mutable) => {
            if is_mutable {
                "SetHash".to_string()
            } else {
                "Set".to_string()
            }
        }
        ValueView::Bag(_, is_mutable) => {
            if is_mutable {
                "BagHash".to_string()
            } else {
                "Bag".to_string()
            }
        }
        ValueView::Mix(_, is_mutable) => {
            if is_mutable {
                "MixHash".to_string()
            } else {
                "Mix".to_string()
            }
        }
        ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => "Pair".to_string(),
        ValueView::Range(_, _)
        | ValueView::RangeExcl(_, _)
        | ValueView::RangeExclStart(_, _)
        | ValueView::RangeExclBoth(_, _)
        | ValueView::GenericRange { .. } => "Range".to_string(),
        ValueView::Nil => "Nil".to_string(),
        ValueView::Instance { class_name, .. } => class_name.resolve(),
        ValueView::Package(name) => name.resolve(),
        ValueView::Enum { enum_type, .. } => enum_type.resolve(),
        ValueView::Sub(_) | ValueView::WeakSub(_) => "Sub".to_string(),
        ValueView::Routine { .. } => "Sub".to_string(),
        ValueView::Regex(_) => "Regex".to_string(),
        ValueView::Junction { .. } => "Junction".to_string(),
        ValueView::Slip(_) => "Slip".to_string(),
        ValueView::Uni(u) if !u.form.is_empty() => u.form.clone(),
        ValueView::Uni(_) => "Uni".to_string(),
        ValueView::Mixin(inner, mixins) => {
            allomorph_type_name(inner, mixins).unwrap_or_else(|| what_type_name(inner))
        }
        ValueView::ContainerRef(_) => val.with_deref(what_type_name),
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
    match inner.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Some("IntStr".to_string()),
        ValueView::Num(_) => Some("NumStr".to_string()),
        ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _) => {
            Some("RatStr".to_string())
        }
        ValueView::Complex(_, _) => Some("ComplexStr".to_string()),
        _ => None,
    }
}
