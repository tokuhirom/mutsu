// Helpers for Buf/Blob.write-num32 and .write-num64.
//
// Spec: https://docs.raku.org/type/Buf
//   method write-num32(::T:U: $offset, num32 $value, $endian = NativeEndian --> Buf:D)
//   method write-num32(Buf:D: $offset, num32 $value, $endian = NativeEndian --> Buf:D)

use crate::value::{RuntimeError, Value, ValueView};

/// Returns Some(size_in_bytes) if the method is a write-num method.
pub(crate) fn write_num_size(method: &str) -> Option<usize> {
    match method {
        "write-num32" => Some(4),
        "write-num64" => Some(8),
        _ => None,
    }
}

/// Decode an Endian enum / int into the canonical 0=Native / 1=LE / 2=BE.
pub(crate) fn decode_endian(value: &Value) -> i64 {
    match value.view() {
        ValueView::Enum { value, .. } => value.as_i64(),
        ValueView::Int(i) => i,
        _ => 0,
    }
}

/// Convert an arbitrary numeric Value into f64. Used for write-num input.
pub(crate) fn to_f64_value(value: &Value) -> f64 {
    match value.view() {
        ValueView::Num(f) => f,
        ValueView::Int(i) => i as f64,
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
            if d == 0 {
                0.0
            } else {
                n as f64 / d as f64
            }
        }
        ValueView::BigInt(bi) => num_traits::ToPrimitive::to_f64(bi.as_ref()).unwrap_or(0.0),
        ValueView::Bool(b) => i64::from(b) as f64,
        ValueView::Str(s) => s.parse::<f64>().unwrap_or(0.0),
        _ => 0.0,
    }
}

/// Apply a write-num write to a byte slice (resizing if needed).
pub(crate) fn apply_write_num(
    bytes: &mut Vec<u8>,
    method: &str,
    offset: i64,
    value: &Value,
    endian_val: i64,
) -> Result<(), RuntimeError> {
    let size = write_num_size(method).expect("not a write-num method");
    if offset < 0 {
        return Err(RuntimeError::new(format!(
            "Cannot write to a negative offset for {}: {}",
            method, offset
        )));
    }
    let off = offset as usize;
    let needed = off
        .checked_add(size)
        .ok_or_else(|| RuntimeError::new(format!("write-num offset {} too large", offset)))?;
    if bytes.len() < needed {
        bytes.resize(needed, 0u8);
    }
    let v = to_f64_value(value);
    if size == 4 {
        let v32 = v as f32;
        let encoded = match endian_val {
            1 => v32.to_le_bytes(),
            2 => v32.to_be_bytes(),
            _ => v32.to_ne_bytes(),
        };
        bytes[off..off + 4].copy_from_slice(&encoded);
    } else {
        let encoded = match endian_val {
            1 => v.to_le_bytes(),
            2 => v.to_be_bytes(),
            _ => v.to_ne_bytes(),
        };
        bytes[off..off + 8].copy_from_slice(&encoded);
    }
    Ok(())
}

/// Build a fresh Buf instance value from a byte vector.
pub(crate) fn make_buf_value(class_name: &str, bytes: Vec<u8>) -> Value {
    use crate::symbol::Symbol;
    use std::collections::HashMap;
    let items: Vec<Value> = bytes.into_iter().map(|b| Value::int(b as i64)).collect();
    let mut attrs = HashMap::new();
    attrs.insert("bytes".to_string(), Value::array(items));
    Value::make_instance(Symbol::intern(class_name), attrs)
}
