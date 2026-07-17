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
            if let Some(name) = allomorph_type_name(inner, mixins) {
                name
            } else {
                let base = what_type_name(inner);
                // A punned role (`R.new`) is `Mixin(Instance{R}, {__mutsu_role__R})`
                // — the role composed onto its OWN same-named (empty) instance, not
                // a mixin onto a different base. Raku names that plain `R`, so drop
                // a suffix entry that merely repeats the base type. A role mixed
                // onto a different base still gets the suffix (`W but R` -> `W+{R}`).
                match role_mixin_suffix_excluding(mixins, &base) {
                    Some(suffix) => format!("{base}+{{{suffix}}}"),
                    None => base,
                }
            }
        }
        ValueView::ContainerRef(_) => val.with_deref(what_type_name),
        _ => "Any".to_string(),
    }
}

/// Build the `+{Role,...}` suffix for a role-mixed value, if any roles were
/// composed in. Role mixins are recorded under `__mutsu_role__{name}` keys (a
/// double underscore distinguishes them from the bookkeeping keys
/// `__mutsu_role_id__` / `__mutsu_role_typeargs__` / `__mutsu_role_param__`).
/// Returns e.g. `Foo::Bar` for `5 but Foo::Bar` so `.^name` reads `Int+{Foo::Bar}`.
pub(crate) fn role_mixin_suffix(
    mixins: &std::collections::HashMap<String, Value>,
) -> Option<String> {
    role_mixin_suffix_excluding(mixins, "")
}

/// [`role_mixin_suffix`], but skipping the role whose name equals `base` — the
/// role-punning case, where `R.new` builds `Mixin(Instance{R}, __mutsu_role__R)`
/// and raku reports plain `R` rather than `R+{R}`. Pass `""` to exclude nothing.
pub(crate) fn role_mixin_suffix_excluding(
    mixins: &std::collections::HashMap<String, Value>,
    base: &str,
) -> Option<String> {
    let mut names: Vec<&str> = mixins
        .keys()
        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
        .filter(|n| *n != base)
        // Anonymous roles (`but role { }`) carry a compiler-internal
        // `__ANON_ROLE_{id}__` name; raku would show `<anon|N>` but mutsu's id
        // does not match, so leave anon mixins un-suffixed (reporting the base
        // type) rather than leaking the internal name.
        .filter(|n| !n.starts_with("__ANON_ROLE_"))
        .collect();
    if names.is_empty() {
        return None;
    }
    // HashMap iteration order is non-deterministic; sort for a stable name.
    names.sort_unstable();
    Some(names.join(","))
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
