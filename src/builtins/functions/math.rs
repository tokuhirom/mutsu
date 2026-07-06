use crate::value::{Value, ValueView};
use num_bigint::BigInt as NumBigInt;

pub(crate) fn gcd_u64(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Compute n! as a BigInt.
pub(crate) fn factorial_bigint(n: u64) -> std::sync::Arc<NumBigInt> {
    let mut result = NumBigInt::from(1u64);
    for i in 2..=n {
        result *= NumBigInt::from(i);
    }
    std::sync::Arc::new(result)
}

/// If the value represents an integer (even as Num, Rat, Str, or BigInt), return as BigInt.
pub(crate) fn generic_range_as_bigint(v: &Value) -> Option<NumBigInt> {
    match v.view() {
        ValueView::Int(i) => Some(NumBigInt::from(i)),
        ValueView::BigInt(n) => Some((**n).clone()),
        ValueView::Num(f) => {
            if f.is_finite() && f == f.trunc() && f.abs() < i64::MAX as f64 {
                Some(NumBigInt::from(f as i64))
            } else {
                None
            }
        }
        ValueView::Rat(n, d) => {
            if d != 0 && n % d == 0 {
                Some(NumBigInt::from(n / d))
            } else {
                None
            }
        }
        ValueView::Str(s) => s.trim().parse::<i64>().ok().map(NumBigInt::from),
        ValueView::Bool(b) => Some(NumBigInt::from(if b { 1 } else { 0 })),
        _ => None,
    }
}

/// Check if a value is a named pair relevant to min/max/minmax dispatch
/// (i.e. :by, :k, :v, :kv, :p) -- these need interpreter handling.
pub(crate) fn is_extrema_named_pair(v: &Value) -> bool {
    match v.view() {
        ValueView::Pair(name, _) => matches!(name.as_str(), "by" | "k" | "v" | "kv" | "p"),
        ValueView::ValuePair(key, _) => {
            matches!(key.view(), ValueView::Str(name) if matches!(name.as_str(), "by" | "k" | "v" | "kv" | "p"))
        }
        _ => false,
    }
}
