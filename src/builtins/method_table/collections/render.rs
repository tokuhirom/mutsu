//! `WHICH` on the collections, `gist`/`raku`/`Str` on the quant hashes and
//! `gist`/`raku` on `Range` (ADR-11276 remainder: the rendering and identity
//! names; the quant hashes' `Str` is §9.73).
//!
//! `WHICH` is [`which_of`], the one identity routine every layer shares
//! (`Array`, `Hash`, `Pair`, `Range`, `Set`, `Bag` and `Mix`, the owners Rakudo
//! declares it on; the mutable forms and `List` reach them through the MRO). A quant
//! hash renders through `setbagmix_gist` / `setbagmix_raku`, the renderers a
//! collection's element gist and the cascade also use, and a `Range` through
//! `raku_value` (its `gist` is its `raku` in Rakudo).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::which::which_of;
use crate::value::gist::setbagmix_gist;
use crate::value::raku_repr::{raku_value, setbagmix_raku};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! rows {
    ($name:literal => $handler:ident: $($owner:literal),*) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

pub(super) static WHICH_ROWS: &[MethodRow] = rows!("WHICH" => which:
    "Array", "Hash", "Range", "Set", "Bag", "Mix", "Capture");
/// `Pair.WHICH`. A Pair holding a container or a reference type has no stable
/// identity string (`which_of` mints a fresh id per call), so a second run of
/// the call answers differently: the debug cross-checks must not re-run it.
pub(super) static PAIR_WHICH_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Pair",
    name: "WHICH",
    arity: 0,
    handler: Handler::Narrow(which),
    flags: RowFlags::RANDOM,
    named: &[],
}];
pub(super) static LIST_GIST_ROWS: &[MethodRow] =
    rows!("gist" => collection_gist: "List", "Seq", "Hash", "Pair");
pub(super) static QUANT_GIST_ROWS: &[MethodRow] =
    rows!("gist" => quant_gist: "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static QUANT_RAKU_ROWS: &[MethodRow] =
    rows!("raku" => quant_raku: "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static QUANT_STR_ROWS: &[MethodRow] =
    rows!("Str" => quant_str: "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static RANGE_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Range",
    name: "raku",
    arity: 0,
    handler: Handler::Narrow(range_render),
    flags: RowFlags::NONE,
    named: &[],
}];

/// `.gist` of a list, `Seq`, hash or pair: the renderer the cascade shares. It
/// declines (`None`) a collection whose element may carry a user `gist`, which
/// the interpreter path dispatches.
// Cost: O(min(e, 100) * d + t), e = elements, d = rendered size of each shown
// element, t = nodes `gist_route` walks.
fn collection_gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::methods_0arg::collection_gist::collection_gist(target)?
}

/// `.WHICH` of a collection or a range, and of an instance of a user
/// subclass of one, which reaches this row through `answer_shapeless`'s MRO
/// walk: `which_of`, the one identity routine.
// Cost: O(1) for an `Array`, `Hash` and `Pair` (a per-container id); O(n) for
// a quant hash (n = elements, hashed) and a `Range` (n = chars of the
// rendering).
fn which(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(which_of(target)))
}

/// Whether a quant hash holds an element that may carry a user-defined
/// `gist`/`raku` (an instance, a custom type or a type object). The pure
/// renderers would print its default form, so the row declines and the call
/// takes the interpreter path that dispatches the element's own method.
// Cost: O(n), n = elements.
fn has_object_element(target: &Value) -> bool {
    fn any_object<'a>(
        mut keys: impl Iterator<Item = &'a String>,
        typed: impl Fn(&String) -> Value,
    ) -> bool {
        keys.any(|k| {
            matches!(
                typed(k).view(),
                ValueView::Instance { .. }
                    | ValueView::CustomType(..)
                    | ValueView::CustomTypeInstance(_)
                    | ValueView::Package(..)
            )
        })
    }
    match target.view() {
        ValueView::Set(s, _) => any_object(s.iter(), |k| s.typed_key(k)),
        ValueView::Bag(b, _) => any_object(b.keys(), |k| b.typed_key(k)),
        ValueView::Mix(m, _) => any_object(m.keys(), |k| m.typed_key(k)),
        _ => false,
    }
}

/// A quant hash's `.gist`: `Set(a b)`, `Bag(a(2))`, `Mix(a(0.5))`.
// Cost: O(n log n), n = elements (the keys are sorted).
pub(crate) fn quant_gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if has_object_element(target) {
        return None;
    }
    setbagmix_gist(target).map(|s| Ok(Value::str(s)))
}

/// A quant hash's `.raku`: `Set.new("a","b")`, `("a"=>2).Bag`.
// Cost: O(n log n), n = elements (the keys are sorted).
pub(crate) fn quant_raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if has_object_element(target) {
        return None;
    }
    setbagmix_raku(target).map(|s| Ok(Value::str(s)))
}

/// A quant hash's `.Str`: its elements joined by spaces, a `Set`'s by their
/// `Str`, a `Bag`'s and a `Mix`'s by their `gist` with the weight when it is not
/// 1 (`a(2) b`). `Value::to_string_value` is the one renderer; the cascade's `Str`
/// arm reaches it too.
// Cost: O(n log n), n = elements (the keys are sorted).
pub(crate) fn quant_str(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !matches!(
        target.view(),
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
    ) || has_object_element(target)
    {
        return None;
    }
    Some(Ok(Value::str(target.to_string_value())))
}

/// `Range.raku` (`Range.gist` is the same text, answered by the cascade): one rendering, `1..5`, `^5`, `"a".."c"`.
// Cost: O(n), n = chars of the rendering.
pub(crate) fn range_render(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target
        .is_range()
        .then(|| Ok(Value::str(raku_value(target))))
}
