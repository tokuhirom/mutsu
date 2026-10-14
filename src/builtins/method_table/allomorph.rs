//! An allomorph as a receiver (ADR-11276 §9.101).
//!
//! `<1>` is an `IntStr`, whose MRO is `IntStr Allomorph Str Int Cool Any Mu`
//! (`RatStr`, `NumStr` and `ComplexStr` alike, with their numeric type). mutsu
//! keeps it as its numeric value with the source string mixed in, which has no
//! dispatch shape, so the table answers it through one of its two parts: the
//! row is looked up along that MRO and invoked on the part the method reads.
//!
//! - **`Str`'s own rows** read the string part (`<1e0>.flip` is `"0e1"`).
//!   `Allomorph` overrides the numeric coercions and `succ`/`pred` to read the
//!   number (`ALLOMORPH_NUMERIC`), so those are never `Str`'s.
//! - **The numeric type's own rows** (`Int`, `Real`, `Rational`, `Num`,
//!   `Complex`, `Numeric`) read the number.
//! - **`Cool`'s rows** are `self.Str.METHOD` or `self.Numeric.METHOD` in
//!   Rakudo: the numeric ones ([`crate::value::cool_numeric_method`]) read
//!   the number, the rest the string.
//! - **`Any`'s list methods** ([`crate::value::ANY_LIST_FOR_CLOSED_SHAPES`])
//!   read the allomorph itself, as the one element of its list.
//!
//! `Mu`'s rows and every other name are left to the ordinary path (the
//! rendering and identity names an allomorph answers by its own type). An
//! allomorph with a role mixed in (`<42> but R`) is not plain, and neither is
//! a value mixed with a `Str` (`5 but "x"`): both keep the mixin dispatch.

use super::{Receiver, RowId, resolve, row};
use crate::symbol::Symbol;
use crate::value::{DispatchShape, Value, ValueView};

/// The names `Allomorph` declares to read the numeric part, although `Str`
/// declares them too: the numeric coercions, truth and the increments.
const ALLOMORPH_NUMERIC: [&str; 12] = [
    "Bool", "Numeric", "Real", "Int", "Num", "Rat", "FatRat", "Complex", "Bridge", "succ", "pred",
    "narrow",
];

/// The names an allomorph answers about itself, which neither part can: its
/// rendering and identity (`IntStr.new(1, "1")`, `IntStr|Int|1|Str|1`), a
/// copy, `wordcase`, whose result Rakudo re-allomorphs, and `ACCEPTS`, which
/// compares a `Str` argument with the string and anything else with the number
/// (`methods_narg::allomorph`).
const ALLOMORPH_OWN: [&str; 10] = [
    "raku", "perl", "gist", "Str", "WHICH", "clone", "Capture", "wordcase", "VAR", "ACCEPTS",
];

/// The string and the number of a plain allomorph: an `IntStr`, `RatStr`,
/// `NumStr` or `ComplexStr` carrying no other mixin. `None` for every other
/// receiver.
// Cost: O(1), a tag probe and one map lookup.
fn parts(target: &Value) -> Option<(Value, &Value)> {
    let ValueView::Mixin(inner, mixins) = target.view() else {
        return None;
    };
    if mixins.len() != 1 {
        return None;
    }
    let string = mixins.get("Str")?;
    if !matches!(string.view(), ValueView::Str(_)) {
        return None;
    }
    let number: &Value = inner;
    matches!(
        number.dispatch_shape(),
        Some(
            DispatchShape::Int
                | DispatchShape::Rat
                | DispatchShape::FatRat
                | DispatchShape::Num
                | DispatchShape::Complex
        )
    )
    .then(|| (string.clone(), number))
}

/// The value standing in for the allomorph `target` as the invocant of
/// `method`, and the row it reaches. `None` for a receiver that is not a plain
/// allomorph, and for a name the rows above do not answer for one.
// Cost: O(1): a tag probe, one map lookup and at most two table lookups.
pub(super) fn redirect(target: &Value, method: Symbol, arity: usize) -> Option<(Value, RowId)> {
    let (string, number) = parts(target)?;
    let name = method.as_str();
    if ALLOMORPH_OWN.contains(&name) {
        return None;
    }
    // `Str.Stringy` is `self`, and an allomorph is a `Str`: the allomorph itself.
    if name == "Stringy" {
        let id = resolve(Receiver::instance(DispatchShape::Str), method, arity)?;
        return Some((target.clone(), id));
    }
    let numeric = ALLOMORPH_NUMERIC.contains(&name);
    if !numeric
        && let Some(id) = resolve(Receiver::instance(DispatchShape::Str), method, arity)
        && row(id).owner == "Str"
    {
        return Some((string, id));
    }
    let id = resolve(Receiver::instance(number.dispatch_shape()?), method, arity)?;
    match row(id).owner {
        "Mu" => None,
        "Any" => crate::value::ANY_LIST_FOR_CLOSED_SHAPES
            .contains(&name)
            .then(|| (target.clone(), id)),
        "Cool" if !numeric && !crate::value::cool_numeric_method(name) => Some((string, id)),
        _ => Some((number.clone(), id)),
    }
}

/// Whether the table answers `method` with `args` on the allomorph `target`
/// ([`redirect`]), so a cascade arm must not: the cascades decline such a
/// call, which keeps the debug cross-check of the row honest.
// Cost: as [`redirect`], plus O(a) to admit a arguments.
pub(crate) fn answers(target: &Value, method: Symbol, args: &[Value]) -> bool {
    redirect(target, method, args.len()).is_some_and(|(_, id)| super::admits(id, args))
}

/// The numeric methods that hand their invocant back unchanged
/// (`Int.floor`, `Real.abs` of a non-negative number, `conj`, `Num.Bridge`).
const ALLOMORPH_RETURNS_SELF: [&str; 7] = [
    "floor", "ceiling", "round", "truncate", "abs", "conj", "Bridge",
];

/// An allomorph's answer from a numeric-part row (`part` was the invocant):
/// when the row handed `part` back unchanged, Rakudo's method returned its own
/// invocant, which is the allomorph (`<1>.floor` is the `IntStr`).
// Cost: O(1), a name probe and one equality test of two scalars.
pub(super) fn keep_allomorph<F>(
    allomorph: &Value,
    part: &Value,
    method: Symbol,
    arity: usize,
    answer: Option<(Result<Value, crate::value::RuntimeError>, F)>,
) -> Option<(Result<Value, crate::value::RuntimeError>, F)> {
    match answer {
        Some((Ok(value), flag))
            if !std::ptr::eq(allomorph, part)
                // `round($scale)` and `Int.abs` build a new number even when equal.
                && arity == 0
                && !(method.as_str() == "abs"
                    && matches!(part.dispatch_shape(), Some(DispatchShape::Int)))
                && ALLOMORPH_RETURNS_SELF.contains(&method.as_str())
                && value.dispatch_shape() == part.dispatch_shape()
                && value == *part =>
        {
            Some((Ok(allomorph.clone()), flag))
        }
        other => other,
    }
}
