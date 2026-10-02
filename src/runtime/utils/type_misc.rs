use super::*;

/// The typed `X::Multi::NoMatch` a value with no `.Numeric` candidate at all
/// raises when forced into a numeric context: every one of Rakudo's generic
/// numeric infix candidates ends in `.Numeric`, so an operand whose type
/// declares none fails that dispatch rather than numifying to some default.
/// `type_name` is the operand's own Raku type (`Whatever`, `Block`, `Sub`, a
/// user class name, ...).
pub(crate) fn numeric_no_match_error(type_name: &str) -> RuntimeError {
    RuntimeError::typed_msg(
        "X::Multi::NoMatch",
        format!(
            "Cannot resolve caller Numeric({type_name}:D: ); none of these signatures matches:\n    (Mu:U \\v: *%_)"
        ),
    )
}

/// `Err` when `value` has no `.Numeric` candidate at all and would otherwise
/// silently numify to a wrong default: a bare `Whatever`/`HyperWhatever` held
/// in a variable (a curried `WhateverCode` is built at parse time and never
/// reaches here as itself) and a bare `Sub`/`Block`/`Method`/... (#9791). Every
/// other operand — including an `Instance`, whose own `.Numeric`/`.Bridge`
/// bridging lives in `coerce_infix_operand_numeric` — is `Ok(())`.
pub(crate) fn require_numeric_candidate(value: &Value) -> Result<(), RuntimeError> {
    if matches!(
        value.view(),
        ValueView::Whatever
            | ValueView::HyperWhatever
            | ValueView::Sub(_)
            | ValueView::WeakSub(_)
            | ValueView::Routine { .. }
    ) {
        return Err(numeric_no_match_error(value_type_name(value)));
    }
    Ok(())
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

/// Return the zero-argument identity for a reduction operator.
///
/// `None` distinguishes operators with no zero-argument meaning from operators
/// whose identity is itself undefined (notably `orelse`).  Callers which are
/// actually evaluating an empty reduction must turn `None` into the
/// `X::NoZeroArgMeaning` Failure required by Raku.
pub(crate) fn reduction_identity_opt(op: &str) -> Option<Value> {
    // `%%` is chain-associative for non-empty reductions, but unlike the
    // comparison operators it has no zero-argument candidate in Rakudo.
    if op == "%%" {
        return None;
    }
    if is_chain_comparison_op(op) {
        return Some(Value::TRUE);
    }
    Some(match op {
        "+" | "-" | "+|" | "+^" => Value::int(0),
        "*" | "**" => Value::int(1),
        "+&" => Value::int(-1), // +^0 (all bits set)
        "~" | "~|" | "~^" => Value::str(String::new()),
        "&&" | "and" | "?&" => Value::TRUE,
        "||" | "or" | "?|" | "^^" => Value::FALSE,
        "?^" => Value::FALSE,
        "//" => Value::package(crate::symbol::wk::any()),
        "orelse" => Value::NIL,
        "andthen" | "notandthen" => Value::TRUE,
        "xor" => Value::FALSE,
        "min" => Value::num(f64::INFINITY),
        "max" => Value::num(f64::NEG_INFINITY),
        "minmax" => Value::generic_range(
            Value::num(f64::INFINITY),
            Value::num(f64::NEG_INFINITY),
            false,
            false,
        ),
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
        "X" | "Z" => Value::seq(Vec::new()),
        // Function composition: the identity element of `∘` is the identity
        // FUNCTION, so `[∘]` over an empty operand list is a working `Callable`
        // (`my &composed = [∘]; composed("foo")` returns `"foo"`), not a scalar.
        "o" | "\u{2218}" => identity_callable(),
        _ => {
            // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>
            if let Some(inner) = crate::compiled_operator::strip_hyper_delimiters(op) {
                return reduction_identity_opt(inner);
            }
            return None;
        }
    })
}

/// Identity lookup for contexts where an unknown operator historically means
/// `Nil`. Empty reduction evaluation should use [`reduction_identity_opt`].
pub(crate) fn reduction_identity(op: &str) -> Value {
    reduction_identity_opt(op).unwrap_or(Value::NIL)
}
