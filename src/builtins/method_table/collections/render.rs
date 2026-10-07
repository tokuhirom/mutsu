//! `WHICH` on the collections, and `gist`/`raku` on the quant hashes and
//! `Range` (ADR-11276 remainder: the rendering and identity names).
//!
//! `WHICH` is [`which_of`], the one identity routine every layer shares
//! (`List`, `Array`, `Hash`, `Pair`, `Range` and the six quant hashes). A quant
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
    "List", "Array", "Hash", "Pair", "Range", "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static QUANT_GIST_ROWS: &[MethodRow] =
    rows!("gist" => quant_gist: "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static QUANT_RAKU_ROWS: &[MethodRow] =
    rows!("raku" => quant_raku: "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static RANGE_ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Range",
        name: "gist",
        arity: 0,
        handler: Handler::Narrow(range_render),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Range",
        name: "raku",
        arity: 0,
        handler: Handler::Narrow(range_render),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// `.WHICH` of a collection or a range.
// Cost: O(1) for an `Array`, `Hash` and `Pair` (a per-container id); O(n) for
// a quant hash (n = elements, hashed) and a `Range` (n = chars of the
// rendering).
fn which(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Array(..)
        | ValueView::Hash(..)
        | ValueView::Pair(..)
        | ValueView::ValuePair(..)
        | ValueView::Set(..)
        | ValueView::Bag(..)
        | ValueView::Mix(..) => Some(Ok(which_of(target))),
        _ if target.is_range() => Some(Ok(which_of(target))),
        _ => None,
    }
}

/// A quant hash's `.gist`: `Set(a b)`, `Bag(a(2))`, `Mix(a(0.5))`.
// Cost: O(n log n), n = elements (the keys are sorted).
fn quant_gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    setbagmix_gist(target).map(|s| Ok(Value::str(s)))
}

/// A quant hash's `.raku`: `Set.new("a","b")`, `("a"=>2).Bag`.
// Cost: O(n log n), n = elements (the keys are sorted).
fn quant_raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    setbagmix_raku(target).map(|s| Ok(Value::str(s)))
}

/// `Range.gist` and `Range.raku`: one rendering, `1..5`, `^5`, `"a".."c"`.
// Cost: O(n), n = chars of the rendering.
fn range_render(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target
        .is_range()
        .then(|| Ok(Value::str(raku_value(target))))
}
