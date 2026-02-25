#![allow(clippy::result_large_err)]

mod arith;
mod functions;
mod methods_0arg;
mod methods_narg;
pub(crate) mod rng;
pub(crate) mod unicode;
use crate::value::{RuntimeError, Value};
use num_bigint::BigInt;
use num_traits::{One, Signed, Zero};

fn split_lines_impl(input: &str, chomp: bool) -> Vec<String> {
    let bytes = input.as_bytes();
    let mut lines = Vec::new();
    let mut start = 0usize;
    let mut i = 0usize;

    while i < bytes.len() {
        let sep_len = if bytes[i] == b'\n' {
            1
        } else if bytes[i] == b'\r' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                2
            } else {
                1
            }
        } else {
            i += 1;
            continue;
        };

        let end = if chomp { i } else { i + sep_len };
        lines.push(input[start..end].to_string());
        i += sep_len;
        start = i;
    }

    if start < input.len() {
        lines.push(input[start..].to_string());
    }

    lines
}

pub(crate) fn split_lines_chomped(input: &str) -> Vec<String> {
    split_lines_impl(input, true)
}

pub(crate) fn split_lines_with_chomp(input: &str, chomp: bool) -> Vec<String> {
    split_lines_impl(input, chomp)
}

pub(crate) use arith::{
    arith_add, arith_div, arith_mod, arith_mul, arith_negate, arith_pow, arith_sub,
};
pub(crate) use functions::native_function;
pub(crate) use methods_0arg::native_method_0arg;
pub(crate) use methods_narg::{native_method_1arg, native_method_2arg};
pub(crate) use unicode::{
    samecase_per_word, samecase_string, samemark_string, unicode_titlecase_first,
};

fn normalized_mod(value: BigInt, modulus: &BigInt) -> BigInt {
    let mut r = value % modulus;
    if r.is_negative() {
        r += modulus;
    }
    r
}

fn extended_gcd(a: BigInt, b: BigInt) -> (BigInt, BigInt, BigInt) {
    let mut old_r = a;
    let mut r = b;
    let mut old_s = BigInt::one();
    let mut s = BigInt::zero();
    let mut old_t = BigInt::zero();
    let mut t = BigInt::one();

    while !r.is_zero() {
        let q = &old_r / &r;
        let next_r = old_r - &q * &r;
        old_r = r;
        r = next_r;

        let next_s = old_s - &q * &s;
        old_s = s;
        s = next_s;

        let next_t = old_t - q * &t;
        old_t = t;
        t = next_t;
    }

    if old_r.is_negative() {
        (-old_r, -old_s, -old_t)
    } else {
        (old_r, old_s, old_t)
    }
}

fn modular_inverse(a: &BigInt, modulus: &BigInt) -> Option<BigInt> {
    let a = normalized_mod(a.clone(), modulus);
    let (g, x, _) = extended_gcd(a, modulus.clone());
    if g != BigInt::one() {
        return None;
    }
    Some(normalized_mod(x, modulus))
}

pub(crate) fn expmod(
    base: &Value,
    exponent: &Value,
    modulus: &Value,
) -> Result<Value, RuntimeError> {
    let base = base.to_bigint();
    let exponent = exponent.to_bigint();
    let modulus = modulus.to_bigint();

    if modulus.is_zero() {
        return Err(RuntimeError::new("expmod: modulus must be non-zero"));
    }

    let modulus_abs = modulus.abs();
    if modulus_abs.is_one() {
        return Ok(Value::Int(0));
    }

    let result = if exponent.is_negative() {
        let inv = modular_inverse(&base, &modulus_abs)
            .ok_or_else(|| RuntimeError::new("expmod: no modular inverse exists"))?;
        let pos_exponent = -exponent;
        inv.modpow(&pos_exponent, &modulus_abs)
    } else {
        let base_mod = normalized_mod(base, &modulus_abs);
        base_mod.modpow(&exponent, &modulus_abs)
    };

    Ok(Value::from_bigint(result))
}
