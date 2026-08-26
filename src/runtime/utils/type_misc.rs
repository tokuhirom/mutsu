use super::*;

pub(crate) fn value_type_name(value: &Value) -> &'static str {
    match value.view() {
        // A `VarRef` is a transient binder wrapper, not a type: report the type
        // of the variable's value.
        ValueView::VarRef { value, .. } => value_type_name(value),
        // `Buf`/`Blob` element storage never surfaces as a Raku-level value:
        // it lives in the buffer instance's attribute cell and only
        // `value::value_buf` reads it. Answer as the buffer it backs.
        ValueView::BufStorage(_) => "Buf",
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
        // `.List`/`.Array`/`.cache` on a lazy Seq changes the reported type
        // without forcing reification, so honour those context markers first.
        ValueView::LazyList(ll) if ll.in_array_context() => "Array",
        ValueView::LazyList(ll) if ll.in_list_context() => "List",
        // CatHandle iterators pull from the live handle on demand internally,
        // but Rakudo exposes both `.lines` and `.handles` as eager `Seq`s.
        ValueView::LazyList(ll) if ll.is_cat_pull() => "Seq",
        ValueView::LazyList(ll) if ll.is_from_gather() => "Seq",
        // An untagged genuinely-lazy list (an infinite arithmetic/closure
        // sequence, `1…∞`, that was never assigned into an `@` slot or given
        // an explicit `.List`/`.Array` context) is a bare `Seq` in Raku —
        // measured: `(1…∞).^name` is `Seq`. Only a context-tagged or
        // non-lazy `LazyList` defaults to `Array` below.
        ValueView::LazyList(ll) if ll.is_genuinely_lazy() => "Seq",
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
        // A builtin routine handle is a `Sub` (Rakudo: `&say.^name` is "Sub"),
        // except a builtin-method lookup handle (`"abc".^lookup("uc")`), whose
        // package is the owning type, which is a `Method`. "Routine" itself is
        // never a concrete value's type in Rakudo.
        ValueView::Routine {
            is_regex, package, ..
        } => {
            if is_regex {
                "Regex"
            } else if package.with_str(|p| p == "GLOBAL" || p.is_empty()) {
                "Sub"
            } else {
                "Method"
            }
        }
        ValueView::Package(_) => "Package",
        ValueView::CompUnitDepSpec { .. } => "Any",
        ValueView::Enum { .. } => "Int",
        ValueView::Instance { .. } => "Any",
        ValueView::Junction { .. } => "Junction",
        ValueView::Regex(_) | ValueView::RegexWithAdverbs { .. } => "Regex",
        ValueView::Version { .. } => "Version",
        // `.cache`/`.List` on a Seq whose source is not yet reified return a
        // second handle over the same body tagged `SeqView::List` (ADR-0038
        // phase 3) rather than forcing — read that tag here so `.^name`
        // agrees with `type_matches_value` without pulling anything.
        ValueView::Seq(body) => match body.view() {
            crate::value::SeqView::List => "List",
            crate::value::SeqView::Seq => "Seq",
        },
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
        ValueView::HashEntryRef { .. } => value_type_name(&value.hash_entry_read()),
        ValueView::ContainerRef(_) => value.with_deref(value_type_name),
    }
}

/// The type name to *show the user* in a type-check error.
///
/// [`value_type_name`] answers a `&'static str` drawn from the `Value` tag
/// alone, so a native `array[T]` collapses to `"Array"` there. That is fine for
/// the tag-level checks it feeds, but it makes an error read "expected Array,
/// got Array" when an `Array` parameter rejects a native array (they are
/// distinct types — `array`'s MRO is `array, Cool, Any, Mu`). The declared type
/// travels embedded in the array's own backing data, so recovering it here
/// needs no interpreter.
pub(crate) fn value_type_display_name(value: &Value) -> String {
    if let ValueView::Array(items, _) = value.view()
        && let Some(declared) = items.declared_type.as_deref()
        && (declared == "array" || declared.starts_with("array["))
    {
        return declared.to_string();
    }
    value_type_name(value).to_string()
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

/// Env marker identifying the identity-function carrier Sub built by
/// [`identity_callable`]. Resolved by `call_sub_value` the same way a
/// `__mutsu_compose_left`/`right` composition carrier is.
pub(crate) const IDENTITY_CALLABLE_MARKER: &str = "__mutsu_identity_callable";

/// The identity function `-> $x { $x }`, as a `Callable` value.
///
/// This is `infix:<∘>`'s zero-argument value: composing nothing leaves its
/// argument unchanged. It is built as a marker carrier rather than an AST
/// closure so it needs neither a compiler round-trip nor an interpreter
/// handle — `reduction_identity` is a pure function of the operator name.
pub(crate) fn identity_callable() -> Value {
    use std::sync::atomic::{AtomicU64, Ordering};
    static IDENTITY_ID: AtomicU64 = AtomicU64::new(2_000_000);
    let mut env = crate::env::Env::new();
    env.insert(IDENTITY_CALLABLE_MARKER.to_string(), Value::TRUE);
    Value::make_sub_with_id(
        Symbol::intern(""),
        Symbol::intern("<identity>"),
        vec!["arg0".to_string()],
        Vec::new(),
        Vec::new(),
        false,
        env,
        IDENTITY_ID.fetch_add(1, Ordering::Relaxed),
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
        // Function composition: the identity element of `∘` is the identity
        // FUNCTION, so `[∘]` over an empty operand list is a working `Callable`
        // (`my &composed = [∘]; composed("foo")` returns `"foo"`), not a scalar.
        "o" | "\u{2218}" => identity_callable(),
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
