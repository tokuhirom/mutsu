use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::{RuntimeError, Value};

/// Convert an f64 to a Rat by using its decimal string representation.
/// This preserves the visible decimal digits (e.g. 10.987654321 → 10987654321/1000000000).
pub(crate) fn f64_to_decimal_rat(f: f64) -> Value {
    let s = format!("{}", f);
    if let Some(dot_pos) = s.find('.') {
        let decimals = s.len() - dot_pos - 1;
        let mut den = 1i64;
        for _ in 0..decimals {
            den = den.saturating_mul(10);
        }
        // Remove the dot and parse as integer numerator
        let num_str: String = s.chars().filter(|c| *c != '.').collect();
        if let Ok(num) = num_str.parse::<i64>() {
            // Simplify the fraction
            let g = gcd_i64(num.abs(), den);
            return Value::rat_raw(num / g, den / g);
        }
    }
    Value::num(f)
}

pub(crate) fn gcd_i64(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// `succ`/`pred` for `Instant`/`Duration`: both do `Real`, whose `succ`/`pred`
/// step the value by one second and keep the receiver's type (raku:
/// `Instant.from-posix(1).succ` is `Instant:12`, `Duration.new(3).succ` is `4`).
// Cost: O(1).
pub(crate) fn real_role_step(
    class_name: Symbol,
    attributes: AttrMap,
    forward: bool,
) -> Result<Value, RuntimeError> {
    let mut attributes = attributes;
    let value = attributes.get("value").cloned().unwrap_or(Value::int(0));
    let stepped = if forward {
        crate::builtins::arith::arith_add(value, Value::int(1))?
    } else {
        crate::builtins::arith::arith_sub(value, Value::int(1))
    };
    attributes.insert("value", stepped);
    Ok(Value::make_instance(class_name, attributes))
}
