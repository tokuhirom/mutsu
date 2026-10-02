//! Mix/MixHash weight arithmetic, under Raku's numeric tower.
//!
//! `MixData` stores each weight as an `f64`, but a Raku `Mix` weight is a
//! `Real`: `(a => 3.14).Mix` holds a `Rat`. The canonical *decoding* of a
//! stored weight is [`crate::value::mix_weight_to_value`] — `Int` for a whole
//! number, `Rat` for one that round-trips through its shortest decimal, `Num`
//! otherwise — and every weight read-out (`$m<a>`, `.pairs`, `.antipairs`)
//! already goes through it.
//!
//! Combining two weights with raw `f64` arithmetic contradicts that decoding.
//! `<b> (+) (b => 3.14).Mix` produced `Mix(b(4.140000000000001))` because
//! `3.14f64 + 1.0` is not the double nearest `4.14`, while Rakudo adds
//! `157/50 + 1` exactly and gets `4.14`. Multiplication (`(.)`) and the
//! `.total` sum had the same defect.
//!
//! So weights combine *here, and only here*: decode both operands, apply the
//! ordinary numeric tower (`Int` + `Rat` stays `Rat`; only a genuine `Num`
//! operand makes the result a `Num`), then re-encode to the stored `f64`. A
//! non-finite operand has no rational decoding, so it falls back to the plain
//! `f64` operation, which is what the tower would compute anyway.
//!
//! `Bag`/`BagHash` counts are `BigInt` and never come through this module —
//! their weights are `Int` and must stay `Int`.

use crate::value::{Value, mix_weight_to_value};

/// Decode both weights, combine them under the numeric tower, re-encode.
fn combine(
    a: f64,
    b: f64,
    tower: impl FnOnce(Value, Value) -> Value,
    raw: fn(f64, f64) -> f64,
) -> f64 {
    if !a.is_finite() || !b.is_finite() {
        return raw(a, b);
    }
    tower(mix_weight_to_value(a), mix_weight_to_value(b)).to_f64()
}

/// `a + b` — the weight combination behind `(+)`/`⊎` and duplicate-key
/// accumulation while a Mix is being built.
pub(crate) fn add(a: f64, b: f64) -> f64 {
    combine(
        a,
        b,
        |x, y| crate::builtins::arith::arith_add(x, y).unwrap_or_else(|_| Value::num(a + b)),
        |x, y| x + y,
    )
}

/// `a - b` — the weight combination behind `(-)`/`∖` and `(^)`/`⊖`.
pub(crate) fn sub(a: f64, b: f64) -> f64 {
    combine(a, b, crate::builtins::arith::arith_sub, |x, y| x - y)
}

/// `a * b` — the weight combination behind `(.)`/`⊍`.
pub(crate) fn mul(a: f64, b: f64) -> f64 {
    combine(a, b, crate::builtins::arith::arith_mul, |x, y| x * y)
}

/// Sum a run of weights left to right. Used by `Mix.total`; the caller orders
/// the weights first so a `Num` operand (whose addition is not associative)
/// cannot make the result depend on hash iteration order.
pub(crate) fn sum(weights: impl IntoIterator<Item = f64>) -> f64 {
    weights.into_iter().fold(0.0, add)
}
