//! The methods `Allomorph` declares about the allomorph itself: `ACCEPTS`,
//! `WHICH` and `raku` (ADR-11276 slice 5). Neither part of an allomorph can
//! answer them: `ACCEPTS` compares a `Str` argument with the string and a
//! number with the number, `WHICH` names both parts, and `raku` renders the
//! constructor call (`IntStr.new(1, "1")`).
//!
//! The rows are `OWNER_ONLY`: an allomorph has no dispatch shape, and
//! `allomorph::redirect` hands these names to `Allomorph`'s rows with the
//! allomorph itself as the invocant. An object argument to `ACCEPTS`, whose
//! `.Str`/`.Numeric` are user code, is declined here and answered by the late
//! interpreter twin (`allomorph_accepts`), registered after these rows.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Allomorph",
        name: "ACCEPTS",
        arity: 1,
        handler: Handler::Narrow(accepts),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "Allomorph",
        name: "WHICH",
        arity: 0,
        handler: Handler::Narrow(which),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Allomorph",
        name: "raku",
        arity: 0,
        handler: Handler::Narrow(raku),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];

/// `Allomorph.ACCEPTS($x)`; `None` for an object argument.
// Cost: O(1), a numeric or string comparison.
fn accepts(target: &Value, args: &[Value]) -> Answer {
    let [arg] = args else {
        return None;
    };
    crate::builtins::methods_narg::allomorph::allomorph_accepts(target, arg)
        .map(|matched| Ok(Value::truth(matched)))
}

/// `Allomorph.WHICH`: the allomorph's type, its number's `WHICH` and its
/// string (`IntStr|Int|1|Str|1`).
// Cost: O(n), n = chars of the identity.
fn which(target: &Value, _args: &[Value]) -> Answer {
    let ValueView::Mixin(inner, mixins) = target.view() else {
        return None;
    };
    let allo_name = crate::value::types::allomorph_type_name(inner, mixins)?;
    let inner_which = match inner.as_ref().view() {
        ValueView::Int(n) => format!("Int|{}", n),
        ValueView::BigInt(n) => format!("Int|{}", *n),
        ValueView::Num(n) => format!("Num|{}", n),
        ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
        ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
        ValueView::BigRat(n, d) => {
            let flavour = if inner.as_ref().is_bigfatrat() {
                "FatRat"
            } else {
                "Rat"
            };
            format!("{}|{}/{}", flavour, n, d)
        }
        ValueView::Complex(r, i) => format!("Complex|{}+{}i", r, i),
        _ => return None,
    };
    let str_part = mixins
        .get("Str")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "WHICH".to_string(),
        Value::str(format!("{}|{}|Str|{}", allo_name, inner_which, str_part)),
    );
    Some(Ok(Value::make_instance(
        crate::symbol::Symbol::intern("ValueObjAt"),
        attrs,
    )))
}

/// `Allomorph.raku`: `IntStr.new(1, "1")`.
// Cost: O(n), n = chars of the rendering.
fn raku(target: &Value, _args: &[Value]) -> Answer {
    let ValueView::Mixin(inner, mixins) = target.view() else {
        return None;
    };
    crate::value::types::allomorph_type_name(inner, mixins)?;
    Some(Ok(Value::str(crate::value::raku_repr::raku_value(target))))
}
