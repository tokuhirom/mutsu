//! Mix/MixHash weight arithmetic, under Raku's numeric tower.
//!
//! `MixData` stores each weight as a [`MixWeight`] (`Int`, `Rat` or `Num`),
//! the type a Raku `Mix` weight keeps.
//!
//! Combining two weights with raw `f64` arithmetic would lose their type:
//! `<b> (+) (b => 3.14).Mix` must give `4.14` (Rakudo adds `157/50 + 1`
//! exactly), and `1/3 + 1/3` must stay the `Rat` `2/3`. So weights combine
//! *here, and only here*: apply the ordinary numeric tower to the two weights
//! (`Int` + `Rat` stays `Rat`; only a genuine `Num` operand makes the result
//! a `Num`). A non-finite operand has no rational form, so it falls back to
//! the plain `f64` operation, which is what the tower would compute anyway.
//!
//! `Bag`/`BagHash` counts are `BigInt` and never come through this module —
//! their weights are `Int` and must stay `Int`.

use crate::value::{MixWeight, Value};

/// Combine two weights under the numeric tower, keeping the result's type
/// (`Int` + `Rat` is `Rat`; only a `Num` operand makes a `Num`).
fn combine(
    a: MixWeight,
    b: MixWeight,
    tower: impl FnOnce(Value, Value) -> Result<Value, crate::value::RuntimeError>,
    raw: fn(f64, f64) -> f64,
) -> MixWeight {
    let fallback = || MixWeight::Num(raw(a.to_f64(), b.to_f64()));
    if !a.is_finite() || !b.is_finite() {
        return fallback();
    }
    match tower(a.to_value(), b.to_value()) {
        Ok(v) => MixWeight::from_value(&v).unwrap_or_else(|| MixWeight::Num(v.to_f64())),
        Err(_) => fallback(),
    }
}

/// `a + b` -- the weight combination behind `(+)`/`⊎` and duplicate-key
/// accumulation while a Mix is being built.
pub(crate) fn add(a: MixWeight, b: MixWeight) -> MixWeight {
    a.add(b)
}

/// `a - b` -- the weight combination behind `(-)`/`∖` and `(^)`/`⊖`.
pub(crate) fn sub(a: MixWeight, b: MixWeight) -> MixWeight {
    combine(
        a,
        b,
        |x, y| Ok(crate::builtins::arith::arith_sub(x, y)),
        |x, y| x - y,
    )
}

/// `a * b` -- the weight combination behind `(.)`/`⊍`.
pub(crate) fn mul(a: MixWeight, b: MixWeight) -> MixWeight {
    combine(
        a,
        b,
        |x, y| Ok(crate::builtins::arith::arith_mul(x, y)),
        |x, y| x * y,
    )
}

/// Sum a run of weights left to right. Used by `Mix.total`; the caller orders
/// the weights first so a `Num` operand (whose addition is not associative)
/// cannot make the result depend on hash iteration order.
pub(crate) fn sum(weights: impl IntoIterator<Item = MixWeight>) -> MixWeight {
    MixWeight::sum(weights)
}
