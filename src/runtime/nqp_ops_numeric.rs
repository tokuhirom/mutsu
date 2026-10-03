//! The numeric, transcendental and random-number `nqp::` ops (#11490):
//! `sqrt_n`, `pow_n`, `cos_n`, `gcd_i`, `div_In`, `base_I`, `rand_n`, ...
//!
//! Each body is the one implementation its primitive has: the wrapping
//! native-int ops (`gcd_i`, `lcm_i`, `pow_i`) and `mod_n` live in
//! [`crate::runtime::nqp_native`] beside `div_i`/`mod_i`; the big-integer ones
//! call the shared Int home in `builtins::arith` (ADR-0118) — `base_I` is the
//! routine `Int.base` renders with, `expmod_I` the one behind `expmod`; and
//! `rand_*`/`srand` draw from the same generator Raku's `rand`/`srand` use, so
//! seeding through either spelling seeds both. The `_n` transcendental ops
//! are the platform `f64` functions, which is also all `Num.cos` and friends
//! are.
//!
//! Operands are read the way the neighbouring `_i`/`_n` tables read them: a
//! missing operand is 0, a Str numifies. The trailing type argument of the
//! `_I` ops (`expmod_I($a, $b, $c, Int)`) names the boxing type, which mutsu's
//! single `Int` value does not need.

use crate::builtins;
use crate::builtins::rng::{builtin_rand, builtin_rand_u64, builtin_srand};
use crate::runtime::nqp_native as n;
use crate::value::{RuntimeError, Value};

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn narg(args: &[Value], i: usize) -> f64 {
    args.get(i).map(|v| v.to_f64()).unwrap_or(0.0)
}

/// An `_I` operand at full precision.
fn big(args: &[Value], i: usize) -> num_bigint::BigInt {
    builtins::int_operand(&args.get(i).cloned().unwrap_or_else(|| Value::int(0))).to_bigint()
}

fn num(f: f64) -> Result<Value, RuntimeError> {
    Ok(Value::num(f))
}

/// Dispatch one numeric op, or `None` when `op` is not one of them.
pub(super) fn call_nqp_numeric_op(op: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(match op {
        // -- native int --
        // Cost: O(log min(|a|, |b|)).
        "gcd_i" => Ok(Value::int(n::gcd_i(iarg(args, 0), iarg(args, 1)))),
        // Cost: O(log min(|a|, |b|)).
        "lcm_i" => Ok(Value::int(n::lcm_i(iarg(args, 0), iarg(args, 1)))),
        // Cost: O(log e), e = the exponent.
        "pow_i" => Ok(Value::int(n::pow_i(iarg(args, 0), iarg(args, 1)))),

        // -- native num --
        // Cost: O(1).
        "mod_n" => num(n::mod_n(narg(args, 0), narg(args, 1))),
        // Cost: O(1).
        "pow_n" => num(narg(args, 0).powf(narg(args, 1))),
        // Cost: O(1).
        "ceil_n" => num(narg(args, 0).ceil()),
        // Cost: O(1).
        "floor_n" => num(narg(args, 0).floor()),
        // Cost: O(1).
        "exp_n" => num(narg(args, 0).exp()),
        // Cost: O(1).
        "log_n" => num(narg(args, 0).ln()),
        // Cost: O(1).
        "sqrt_n" => num(narg(args, 0).sqrt()),
        // Cost: O(1).
        "inf" => num(f64::INFINITY),
        // Cost: O(1).
        "neginf" => num(f64::NEG_INFINITY),
        // Cost: O(1).
        "nan" => num(f64::NAN),

        // -- trigonometric (radians) --
        // Cost: O(1).
        "sin_n" => num(narg(args, 0).sin()),
        // Cost: O(1).
        "cos_n" => num(narg(args, 0).cos()),
        // Cost: O(1).
        "tan_n" => num(narg(args, 0).tan()),
        // Cost: O(1).
        "asin_n" => num(narg(args, 0).asin()),
        // Cost: O(1).
        "acos_n" => num(narg(args, 0).acos()),
        // Cost: O(1).
        "atan_n" => num(narg(args, 0).atan()),
        // `atan2_n($y, $x)`.
        // Cost: O(1).
        "atan2_n" => num(narg(args, 0).atan2(narg(args, 1))),
        // Cost: O(1).
        "sinh_n" => num(narg(args, 0).sinh()),
        // Cost: O(1).
        "cosh_n" => num(narg(args, 0).cosh()),
        // Cost: O(1).
        "tanh_n" => num(narg(args, 0).tanh()),

        // -- big integer --
        // The exact quotient of two Ints as a Num: `div_In(7, 2)` is 3.5, a
        // zero divisor gives ±Inf (NaN for 0/0), and a quotient past the Num
        // range is Inf, as in MoarVM.
        // Cost: O(d^2), d = digits of the larger operand.
        "div_In" => num(builtins::arith::bigint_ratio_to_f64(
            &big(args, 0),
            &big(args, 1),
        )),
        // Cost: O(d^2), d = digits of the operand.
        "base_I" => {
            let radix = iarg(args, 1);
            if !(2..=64).contains(&radix) {
                return Some(Err(RuntimeError::new(format!(
                    "nqp::base_I: radix {radix} out of range (2..64)"
                ))));
            }
            let v = args.first().cloned().unwrap_or_else(|| Value::int(0));
            Ok(Value::str(builtins::int_to_base(&v, radix as u32)))
        }
        // Cost: O(d^2 * log e), d = digits of the modulus, e = the exponent.
        "expmod_I" => {
            let arg = |i| args.get(i).cloned().unwrap_or_else(|| Value::int(0));
            builtins::expmod(&arg(0), &arg(1), &arg(2))
        }

        // -- random numbers: the generator Raku's `rand`/`srand` use --
        // A Num in [0, $max).
        // Cost: O(1).
        "rand_n" => num(builtin_rand() * narg(args, 0)),
        // A native int drawn from the full 64-bit range.
        // Cost: O(1).
        "rand_i" => Ok(Value::int(builtin_rand_u64() as i64)),
        // An Int in [0, $max).
        // Cost: O(d), d = digits of $max.
        "rand_I" => {
            let max = big(args, 0);
            if num_traits::Signed::is_negative(&max) || num_traits::Zero::is_zero(&max) {
                return Some(Err(RuntimeError::new(
                    "nqp::rand_I: the upper bound must be positive",
                )));
            }
            Ok(Value::from_bigint(
                builtins::methods_0arg::dispatch_core_range::random_bigint_in_range(&max),
            ))
        }
        // Seeds the generator and answers the seed.
        // Cost: O(1).
        "srand" => {
            let seed = iarg(args, 0);
            builtin_srand(seed as u64);
            Ok(Value::int(seed))
        }
        _ => return None,
    })
}
