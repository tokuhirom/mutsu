#![allow(clippy::result_large_err)]

pub(crate) mod arith;
pub(crate) mod collation;
mod functions;
pub(crate) mod methods_0arg;
mod methods_narg;
pub(crate) mod parse_base;
pub(crate) mod primality;
pub(crate) mod rng;
pub(crate) mod split;
pub(crate) mod str_increment;
pub(crate) mod transliterate;
pub(crate) mod unicode;
pub(crate) mod unicode_numval_table;
pub(crate) mod uniprop;
mod uniprop_tables;
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

/// Remove exactly one trailing newline sequence (\r\n, \n, or \r).
pub(crate) fn chomp_one(s: &str) -> String {
    if let Some(stripped) = s.strip_suffix("\r\n") {
        stripped.to_string()
    } else if let Some(stripped) = s.strip_suffix('\n') {
        stripped.to_string()
    } else if let Some(stripped) = s.strip_suffix('\r') {
        stripped.to_string()
    } else {
        s.to_string()
    }
}

pub(crate) use arith::{
    arith_add, arith_div, arith_mod, arith_mul, arith_negate, arith_pow, arith_sub,
};
pub(crate) use functions::native_function;
pub(crate) use methods_0arg::native_method_0arg;
pub(crate) use methods_narg::{native_method_1arg, native_method_2arg};
pub(crate) use unicode::{samecase_string, samemark_string, unicode_titlecase_first};

/// Convert a floating-point number to a Rat using a continued fraction algorithm.
/// `epsilon` controls the precision: smaller epsilon means a closer approximation
/// (and typically larger numerator/denominator).
pub(crate) fn num_to_rat_with_epsilon(f: f64, epsilon: f64) -> Value {
    let negative = f < 0.0;
    let f_abs = f.abs();

    // Continued fraction algorithm (Stern-Brocot / mediants)
    let mut p0: i64 = 0;
    let mut q0: i64 = 1;
    let mut p1: i64 = 1;
    let mut q1: i64 = 0;
    let mut x = f_abs;

    for _ in 0..64 {
        let a = x.floor() as i64;
        let p2 = a.saturating_mul(p1).saturating_add(p0);
        let q2 = a.saturating_mul(q1).saturating_add(q0);

        if q2 == 0 {
            break;
        }

        p0 = p1;
        q0 = q1;
        p1 = p2;
        q1 = q2;

        let approx = p1 as f64 / q1 as f64;
        if (approx - f_abs).abs() <= epsilon {
            break;
        }

        let frac = x - a as f64;
        if frac.abs() < 1e-30 {
            break;
        }
        x = 1.0 / frac;
    }

    let numer = if negative { -p1 } else { p1 };
    crate::value::make_rat(numer, q1)
}

fn normalize_builtin_encoding_label(name: &str) -> Option<String> {
    let lowered = name.to_lowercase();
    let normalized = match lowered.as_str() {
        "utf8-c8" => "utf8-c8",
        "utf8" | "utf-8" => "utf-8",
        "utf16" | "utf-16" => "utf-16",
        "utf16le" | "utf-16le" => "utf-16le",
        "utf16be" | "utf-16be" => "utf-16be",
        "ascii" => "ascii",
        "latin-1" | "latin1" | "iso-8859-1" => "iso-8859-1",
        "windows932" | "windows-932" => "windows-932",
        "windows1251" | "windows-1251" => "windows-1251",
        "windows1252" | "windows-1252" => "windows-1252",
        other => {
            if encoding_rs::Encoding::for_label(other.as_bytes()).is_some() {
                other
            } else {
                return None;
            }
        }
    };
    Some(normalized.to_string())
}

fn utf8_line_col(prefix: &[u8]) -> (usize, usize) {
    let s = std::str::from_utf8(prefix).unwrap_or("");
    let (mut line, mut col) = (1usize, 1usize);
    for ch in s.chars() {
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

fn decode_utf16_bytes(bytes: &[u8], big_endian: bool) -> Result<String, RuntimeError> {
    if !bytes.len().is_multiple_of(2) {
        return Err(RuntimeError::new("Invalid utf-16 byte length"));
    }
    let units: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|chunk| {
            if big_endian {
                u16::from_be_bytes([chunk[0], chunk[1]])
            } else {
                u16::from_le_bytes([chunk[0], chunk[1]])
            }
        })
        .collect();
    Ok(String::from_utf16_lossy(&units))
}

fn decode_bytes_with_builtin_encoding(
    bytes: &[u8],
    encoding_name: &str,
) -> Result<String, RuntimeError> {
    match encoding_name {
        "utf8-c8" => Ok(crate::runtime::utf8_c8::decode_utf8_c8(bytes)),
        "ascii" => Ok(bytes
            .iter()
            .map(|b| if *b <= 0x7F { *b as char } else { '\u{FFFD}' })
            .collect()),
        "iso-8859-1" => Ok(bytes.iter().map(|b| *b as char).collect()),
        "utf-16" => {
            if bytes.len() >= 2 && bytes[0] == 0xFE && bytes[1] == 0xFF {
                decode_utf16_bytes(&bytes[2..], true)
            } else if bytes.len() >= 2 && bytes[0] == 0xFF && bytes[1] == 0xFE {
                decode_utf16_bytes(&bytes[2..], false)
            } else {
                decode_utf16_bytes(bytes, false)
            }
        }
        "utf-16le" => decode_utf16_bytes(bytes, false),
        "utf-16be" => decode_utf16_bytes(bytes, true),
        "utf-8" => match std::str::from_utf8(bytes) {
            Ok(s) => Ok(s.strip_prefix('\u{FEFF}').unwrap_or(s).to_string()),
            Err(e) => {
                let vup = e.valid_up_to();
                if e.error_len().is_none() {
                    Err(RuntimeError::new(
                        "Malformed termination of UTF-8 string".to_string(),
                    ))
                } else {
                    let (line, col) = utf8_line_col(&bytes[..vup]);
                    let s = if vup > 0 { vup - 1 } else { 0 };
                    let end = (vup + 2).min(bytes.len());
                    let near: Vec<String> =
                        bytes[s..end].iter().map(|b| format!("{:02x}", b)).collect();
                    Err(RuntimeError::new(format!(
                        "Malformed UTF-8 near bytes {} at line {} col {}",
                        near.join(" "),
                        line,
                        col
                    )))
                }
            }
        },
        _ => {
            let label = if encoding_name == "windows-932" {
                "shift_jis"
            } else {
                encoding_name
            };
            let enc = encoding_rs::Encoding::for_label(label.as_bytes()).ok_or_else(|| {
                RuntimeError::new(format!("Unknown encoding '{}'", encoding_name))
            })?;
            let (decoded, _used_encoding, _had_errors) = enc.decode(bytes);
            Ok(decoded.into_owned())
        }
    }
}

fn decode_buf_target_bytes(target: &Value, encoding_name: &str) -> Option<Vec<u8>> {
    let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
    else {
        return None;
    };

    if !crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) {
        return None;
    }

    let Value::Array(items, ..) = attributes.get("bytes")? else {
        return Some(Vec::new());
    };

    if class_name == "utf16" {
        let use_be = encoding_name == "utf-16be";
        let mut out = Vec::with_capacity(items.len() * 2);
        for item in items.iter() {
            let unit = match item {
                Value::Int(i) => *i as u16,
                _ => 0u16,
            };
            let pair = if use_be {
                unit.to_be_bytes()
            } else {
                unit.to_le_bytes()
            };
            out.extend_from_slice(&pair);
        }
        Some(out)
    } else {
        Some(
            items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect(),
        )
    }
}

pub(crate) fn decode_buf_method(
    target: &Value,
    encoding: Option<&str>,
) -> Option<Result<Value, RuntimeError>> {
    let Value::Instance { class_name, .. } = target else {
        return None;
    };
    if !crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) {
        return None;
    }

    let default_encoding = if class_name == "utf16" {
        "utf-16"
    } else {
        "utf-8"
    };
    let encoding_name = encoding.unwrap_or(default_encoding);
    let normalized = normalize_builtin_encoding_label(encoding_name)?;
    let bytes = decode_buf_target_bytes(target, &normalized)?;
    Some(decode_bytes_with_builtin_encoding(&bytes, &normalized).map(Value::str))
}

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
