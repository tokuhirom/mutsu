//! A Mix/MixHash weight and the one rule for printing it beside its key. The
//! weight *arithmetic* lives in `builtins::mix_weight`.
//!
//! A Raku `Mix` weight is a `Real` that keeps its own type: `(a => 1/3).Mix<a>`
//! is a `Rat`, `(a => 0.5e0).Mix<a>` a `Num`. [`MixWeight`] stores exactly
//! that, so reading a weight back never guesses its type.

use super::{Value, make_rat};

/// One Mix/MixHash weight: an `Int`, a `Rat` (reduced, `den > 0`) or a `Num`.
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum MixWeight {
    Int(i64),
    Rat(i64, i64),
    Num(f64),
}

impl MixWeight {
    /// The weight as a double (for sorting, sampling and other float uses).
    // Cost: O(1).
    pub fn to_f64(self) -> f64 {
        match self {
            MixWeight::Int(i) => i as f64,
            MixWeight::Rat(n, d) => super::rat_to_f64(n, d),
            MixWeight::Num(f) => f,
        }
    }

    /// The weight as the Raku value it stands for.
    // Cost: O(1).
    pub fn to_value(self) -> Value {
        match self {
            MixWeight::Int(i) => Value::Int(i),
            MixWeight::Rat(n, d) => make_rat(n, d),
            MixWeight::Num(f) => Value::Num(f),
        }
    }

    /// Store a numeric Raku value, keeping its type. `None` for a non-`Real`
    /// (or a big value that has no 64-bit form; those degrade to a `Num`).
    // Cost: O(1) for machine-sized values.
    pub fn from_value(v: &Value) -> Option<MixWeight> {
        match v.view() {
            super::ValueView::Int(i) => Some(MixWeight::Int(i)),
            super::ValueView::Rat(n, d) if d != 0 => Some(MixWeight::Rat(n, d)),
            super::ValueView::Num(f) => Some(MixWeight::Num(f)),
            super::ValueView::Rat(..) => Some(MixWeight::Num(v.to_f64())),
            _ => None,
        }
    }

    /// A `Bag` count as a weight: `Int` when it fits, else the nearest `Num`.
    pub fn from_bigint(n: &num_bigint::BigInt) -> MixWeight {
        use num_traits::ToPrimitive;
        n.to_i64().map_or_else(
            || MixWeight::Num(n.to_f64().unwrap_or(f64::INFINITY)),
            MixWeight::Int,
        )
    }

    /// Whether the weight is exactly 1 (printed as the bare key).
    pub fn is_one(self) -> bool {
        match self {
            MixWeight::Int(i) => i == 1,
            MixWeight::Rat(n, d) => n == d,
            MixWeight::Num(f) => f == 1.0,
        }
    }
}

impl Default for MixWeight {
    fn default() -> Self {
        MixWeight::Int(0)
    }
}

impl MixWeight {
    /// The larger of two weights (`self` on a tie), keeping its own type.
    pub fn max(self, other: MixWeight) -> MixWeight {
        if other > self { other } else { self }
    }

    /// The smaller of two weights (`self` on a tie), keeping its own type.
    pub fn min(self, other: MixWeight) -> MixWeight {
        if other < self { other } else { self }
    }

    /// The absolute value, keeping the type.
    pub fn abs(self) -> MixWeight {
        match self {
            MixWeight::Int(i) => i
                .checked_abs()
                .map_or(MixWeight::Num((i as f64).abs()), MixWeight::Int),
            MixWeight::Rat(n, d) => n
                .checked_abs()
                .map_or(MixWeight::Num(self.to_f64().abs()), |n| {
                    MixWeight::Rat(n, d)
                }),
            MixWeight::Num(f) => MixWeight::Num(f.abs()),
        }
    }

    /// Whether the weight is a finite number (a `Num` may be NaN/Inf).
    pub fn is_finite(self) -> bool {
        self.to_f64().is_finite()
    }
}

impl PartialEq<f64> for MixWeight {
    fn eq(&self, other: &f64) -> bool {
        self.to_f64() == *other
    }
}

impl PartialOrd<f64> for MixWeight {
    fn partial_cmp(&self, other: &f64) -> Option<std::cmp::Ordering> {
        self.to_f64().partial_cmp(other)
    }
}

impl PartialEq for MixWeight {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MixWeight::Int(a), MixWeight::Int(b)) => a == b,
            (MixWeight::Rat(a, b), MixWeight::Rat(c, d)) => a == c && b == d,
            _ => self.to_f64() == other.to_f64(),
        }
    }
}

impl PartialOrd for MixWeight {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.to_f64().partial_cmp(&other.to_f64())
    }
}

/// Transitional constructor for call sites that still compute in `f64`: a
/// whole number becomes `Int`, a decimal that round-trips `Rat`, anything else
/// `Num` (the old decoding, applied once at the store boundary).
impl From<f64> for MixWeight {
    fn from(w: f64) -> Self {
        match decode_f64(w).view() {
            super::ValueView::Int(i) => MixWeight::Int(i),
            super::ValueView::Rat(n, d) => MixWeight::Rat(n, d),
            _ => MixWeight::Num(w),
        }
    }
}

/// Decode a stored weight (or an `f64` from a not-yet-converted caller; see
/// `From<f64> for MixWeight`) to the Raku value it stands for.
pub fn mix_weight_to_value(w: impl Into<MixWeight>) -> Value {
    w.into().to_value()
}

/// Legacy `f64` decoding: `Int` for a whole number, `Rat` for a decimal that
/// round-trips, `Num` otherwise.
fn decode_f64(w: f64) -> Value {
    if w.is_nan() || w.is_infinite() {
        return Value::Num(w);
    }
    // Check for exact integer
    if w == (w as i64 as f64) && w.abs() < i64::MAX as f64 {
        return Value::Int(w as i64);
    }
    // Try to reconstruct as Rat: use the decimal representation to find
    // a rational number. Multiply by powers of 10 to clear the decimal.
    // This works for values like 42.1, 1.5, 3.14 etc.
    let s = format!("{}", w);
    if let Some(dot_pos) = s.find('.') {
        let decimals = s.len() - dot_pos - 1;
        if decimals <= 15 {
            let denom = 10i64.checked_pow(decimals as u32);
            if let Some(d) = denom {
                // Parse the string without the dot as numerator
                let without_dot: String = s.chars().filter(|c| *c != '.').collect();
                if let Ok(n) = without_dot.parse::<i64>() {
                    // Verify round-trip: n/d as f64 == w
                    if (crate::value::rat_to_f64(n, d) - w).abs() < f64::EPSILON * w.abs().max(1.0)
                    {
                        return make_rat(n, d);
                    }
                }
            }
        }
    }
    Value::Num(w)
}

/// How a weight is shown beside its key by `.Str` and `.gist`: `None` when the
/// weight is exactly 1, which Rakudo prints as the bare key (`Mix(a b(2))`).
///
/// Both renderers call this so a weight is *printed* the way it is *read back*.
/// They each used to carry their own copy of the rule, whose `w as i64`
/// shortcut for whole weights saturated: `(a => 2e300).Mix` printed
/// `Mix(a(9223372036854775807))`.
pub(crate) fn mix_weight_render(w: MixWeight) -> Option<String> {
    if w.is_one() {
        None
    } else {
        Some(w.to_value().to_string_value())
    }
}

impl MixWeight {
    /// `self + other` under the numeric tower, keeping the type: `Int` + `Rat`
    /// is an exact `Rat`, and only a `Num` operand (or a total with no 64-bit
    /// form) makes a `Num`. The one weight addition: `builtins::mix_weight`
    /// (`(+)`, duplicate keys, `.total`) and `+$mix` all come through here.
    // Cost: O(1).
    pub fn add(self, other: MixWeight) -> MixWeight {
        fn parts(w: MixWeight) -> Option<(i128, i128)> {
            match w {
                MixWeight::Int(i) => Some((i as i128, 1)),
                MixWeight::Rat(n, d) => Some((n as i128, d as i128)),
                MixWeight::Num(_) => None,
            }
        }
        fn gcd(a: i128, b: i128) -> i128 {
            let (mut a, mut b) = (a.abs(), b.abs());
            while b != 0 {
                (a, b) = (b, a % b);
            }
            a
        }
        let (Some((an, ad)), Some((bn, bd))) = (parts(self), parts(other)) else {
            return MixWeight::Num(self.to_f64() + other.to_f64());
        };
        let (mut n, mut d) = (an * bd + bn * ad, ad * bd);
        let g = gcd(n, d).max(1);
        (n, d) = (n / g, d / g);
        // Either operand a `Rat` keeps the result a `Rat` (`1/3 + 2/3` is `1.0`).
        let rational = matches!(self, MixWeight::Rat(..)) || matches!(other, MixWeight::Rat(..));
        match (i64::try_from(n), i64::try_from(d)) {
            (Ok(n), Ok(1)) if !rational => MixWeight::Int(n),
            (Ok(n), Ok(d)) => MixWeight::Rat(n, d),
            _ => MixWeight::Num(n as f64 / d as f64),
        }
    }

    /// Sum weights left to right with [`MixWeight::add`]; the caller orders
    /// them first so a `Num` operand cannot make the total depend on hash order.
    // Cost: O(n), n = the number of weights.
    pub fn sum(weights: impl IntoIterator<Item = MixWeight>) -> MixWeight {
        weights.into_iter().fold(MixWeight::Int(0), MixWeight::add)
    }
}
