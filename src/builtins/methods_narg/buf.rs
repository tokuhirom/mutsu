#![allow(clippy::result_large_err)]

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use num_bigint::BigInt;
use num_traits::ToPrimitive;

pub(crate) fn is_buf_like(val: &Value) -> bool {
    if let Value::Instance { class_name, .. } = val {
        let cn = class_name.resolve();
        cn == "Buf"
            || cn == "Blob"
            || cn == "utf8"
            || cn == "utf16"
            || cn.starts_with("Buf[")
            || cn.starts_with("Blob[")
            || cn.starts_with("buf")
            || cn.starts_with("blob")
    } else {
        false
    }
}

pub(crate) fn buf_class_name(val: &Value) -> String {
    if let Value::Instance { class_name, .. } = val {
        class_name.resolve().to_string()
    } else {
        "Buf".to_string()
    }
}

pub(crate) fn buf_get_int_items(target: &Value) -> Option<Vec<Value>> {
    if let Value::Instance { attributes, .. } = target
        && let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes")
    {
        Some(items.to_vec())
    } else {
        None
    }
}

pub(crate) fn make_buf_from_int_items(class_name: &str, items: &[Value]) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("bytes".to_string(), Value::array(items.to_vec()));
    Value::make_instance(Symbol::intern(class_name), attrs)
}

pub(crate) fn eval_whatever_code(
    sub_data: &std::sync::Arc<crate::value::SubData>,
    arg: i64,
) -> i64 {
    let param = sub_data
        .params
        .first()
        .map(|s: &String| s.as_str())
        .unwrap_or("_");
    let mut sub_env = sub_data.env.clone();
    sub_env.insert(param.to_string(), Value::Int(arg));
    let mut interpreter = crate::runtime::Interpreter::new();
    *interpreter.env_mut() = sub_env;
    if let Ok(result) = interpreter.eval_block_value(&sub_data.body) {
        match result {
            Value::Int(n) => n,
            Value::Num(f) => f as i64,
            _ => 0,
        }
    } else {
        0
    }
}

pub(crate) fn resolve_buf_index(arg: &Value, len: usize) -> i64 {
    match arg {
        Value::Int(n) => *n,
        Value::Num(f) => *f as i64,
        Value::Rat(n, d) => {
            if *d != 0 {
                *n / *d
            } else {
                0
            }
        }
        Value::Sub(data) => eval_whatever_code(data, len as i64),
        Value::Whatever => len as i64,
        _ => 0,
    }
}

pub(crate) fn resolve_buf_len(arg: &Value, total_len: usize, start: usize) -> i64 {
    match arg {
        Value::Int(n) => *n,
        Value::Num(f) => {
            if f.is_infinite() && *f > 0.0 {
                (total_len - start) as i64
            } else {
                *f as i64
            }
        }
        Value::Rat(n, d) => {
            if *d != 0 {
                *n / *d
            } else {
                0
            }
        }
        Value::Whatever => (total_len - start) as i64,
        Value::Sub(data) => {
            // WhateverCode receives total_len and returns an end index (inclusive).
            // Length = max(0, end_index - start + 1)
            let end_idx = eval_whatever_code(data, total_len as i64);
            let len = end_idx - start as i64 + 1;
            if len < 0 { 0 } else { len }
        }
        _ => 0,
    }
}

pub(crate) fn range_bounds(arg: &Value) -> Option<(i64, i64)> {
    match arg {
        Value::Range(start, end) => Some((*start, *end + 1)),
        Value::RangeExcl(start, end) => Some((*start, *end)),
        _ => None,
    }
}

pub(crate) fn out_of_range_error(got: i64, min: i64, max: i64) -> RuntimeError {
    let mut attrs = std::collections::HashMap::new();
    let msg = format!(
        "Index out of range. Is: {}, should be in {}..{}",
        got, min, max
    );
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("got".to_string(), Value::Int(got));
    attrs.insert("range".to_string(), Value::str(format!("{}..{}", min, max)));
    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Extract byte array from a Buf/Blob instance.
pub(crate) fn buf_get_bytes(target: &Value) -> Option<Vec<u8>> {
    if let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
        && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        && let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes")
    {
        return Some(
            items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect(),
        );
    }
    None
}

pub(crate) fn to_int_val(v: &Value) -> i64 {
    match v {
        Value::Int(i) => *i,
        Value::Num(f) => *f as i64,
        _ => 0,
    }
}

pub(crate) fn read_ubits_from_bytes(bytes: &[u8], from: usize, bits: usize) -> BigInt {
    let mut acc = BigInt::ZERO;
    for i in 0..bits {
        let bit_index = from + i;
        let byte = bytes[bit_index / 8];
        let bit = (byte >> (7 - (bit_index % 8))) & 1;
        acc = (acc << 1) + BigInt::from(bit);
    }
    acc
}

pub(crate) fn bigint_to_value(value: BigInt) -> Value {
    if let Some(i) = value.to_i64() {
        Value::Int(i)
    } else {
        Value::bigint(value)
    }
}

pub(crate) fn read_f32_ne(bytes: &[u8]) -> f64 {
    let arr: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
    f32::from_ne_bytes(arr) as f64
}

pub(crate) fn read_f64_ne(bytes: &[u8]) -> f64 {
    let arr: [u8; 8] = [
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    f64::from_ne_bytes(arr)
}

pub(crate) fn read_f32_endian(bytes: &[u8], endian_val: i64) -> f64 {
    let arr: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
    match endian_val {
        1 => f32::from_le_bytes(arr) as f64, // LittleEndian
        2 => f32::from_be_bytes(arr) as f64, // BigEndian
        _ => f32::from_ne_bytes(arr) as f64, // NativeEndian
    }
}

pub(crate) fn read_f64_endian(bytes: &[u8], endian_val: i64) -> f64 {
    let arr: [u8; 8] = [
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    match endian_val {
        1 => f64::from_le_bytes(arr), // LittleEndian
        2 => f64::from_be_bytes(arr), // BigEndian
        _ => f64::from_ne_bytes(arr), // NativeEndian
    }
}

/// Returns (byte_size, is_signed) for a read-int/uint method name.
pub(crate) fn read_int_method_info(method: &str) -> (usize, bool) {
    match method {
        "read-uint8" => (1, false),
        "read-int8" => (1, true),
        "read-uint16" => (2, false),
        "read-int16" => (2, true),
        "read-uint32" => (4, false),
        "read-int32" => (4, true),
        "read-uint64" => (8, false),
        "read-int64" => (8, true),
        "read-uint128" => (16, false),
        "read-int128" => (16, true),
        _ => unreachable!(),
    }
}

/// Read an integer value from a byte slice with the given endianness.
/// endian_val: 0=NativeEndian, 1=LittleEndian, 2=BigEndian
pub(crate) fn read_int_value(bytes: &[u8], size: usize, signed: bool, endian_val: i64) -> Value {
    // Determine effective endianness: NativeEndian resolves to LE or BE
    let is_le = match endian_val {
        1 => true,                           // LittleEndian
        2 => false,                          // BigEndian
        _ => cfg!(target_endian = "little"), // NativeEndian
    };

    // Build unsigned value from bytes
    let unsigned = if is_le {
        // Little-endian: first byte is least significant
        let mut acc = BigInt::ZERO;
        for &b in bytes[..size].iter().rev() {
            acc = (acc << 8) | BigInt::from(b);
        }
        acc
    } else {
        // Big-endian: first byte is most significant
        let mut acc = BigInt::ZERO;
        for &b in &bytes[..size] {
            acc = (acc << 8) | BigInt::from(b);
        }
        acc
    };

    if signed {
        let bits = size * 8;
        let sign_bit = BigInt::from(1u8) << (bits - 1);
        let result = if unsigned >= sign_bit {
            unsigned - (BigInt::from(1u8) << bits)
        } else {
            unsigned
        };
        bigint_to_value(result)
    } else {
        bigint_to_value(unsigned)
    }
}
