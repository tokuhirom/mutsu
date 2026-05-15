// Helpers for Buf/Blob.write-int{8,16,32,64,128} and .write-uint{8,16,32,64,128}.
//
// Spec: https://docs.raku.org/type/Buf
//   method write-uint8(::T:U: $offset, uint8 $value, $endian = NativeEndian --> Buf:D)
//   method write-uint8(Buf:D: $offset, uint8 $value, $endian = NativeEndian --> Buf:D)
//   (similarly for write-int8, write-uint16, ..., write-int128)

use crate::value::{RuntimeError, Value};

/// Returns Some((byte_size, is_signed)) if the method is a write-int/uint method.
pub(crate) fn write_int_info(method: &str) -> Option<(usize, bool)> {
    match method {
        "write-uint8" => Some((1, false)),
        "write-int8" => Some((1, true)),
        "write-uint16" => Some((2, false)),
        "write-int16" => Some((2, true)),
        "write-uint32" => Some((4, false)),
        "write-int32" => Some((4, true)),
        "write-uint64" => Some((8, false)),
        "write-int64" => Some((8, true)),
        "write-uint128" => Some((16, false)),
        "write-int128" => Some((16, true)),
        _ => None,
    }
}

/// Convert a Value to a u128 for writing.
/// Handles BigInt values that exceed i128 range by extracting low 128 bits.
fn to_u128_value(value: &Value) -> u128 {
    match value {
        Value::Int(i) => *i as u128,
        Value::Num(f) => *f as i128 as u128,
        Value::BigInt(bi) => {
            // Extract the low 128 bits of the BigInt
            use num_bigint::BigInt;
            use num_traits::ToPrimitive;
            let mask = BigInt::from(1u8) << 128;
            let masked: BigInt = ((bi.as_ref() % &mask) + &mask) % &mask;
            masked.to_u128().unwrap_or(0)
        }
        Value::Bool(b) => i64::from(*b) as u128,
        Value::Str(s) => s
            .parse::<u128>()
            .unwrap_or_else(|_| s.parse::<i128>().map(|i| i as u128).unwrap_or(0)),
        Value::Rat(n, d) | Value::FatRat(n, d) => {
            if *d == 0 {
                0
            } else {
                ((*n as i128) / (*d as i128)) as u128
            }
        }
        _ => 0,
    }
}

/// Apply a write-int/uint write to a byte vector (resizing if needed).
pub(crate) fn apply_write_int(
    bytes: &mut Vec<u8>,
    method: &str,
    offset: i64,
    value: &Value,
    endian_val: i64,
) -> Result<(), RuntimeError> {
    let (size, _signed) = write_int_info(method).expect("not a write-int method");
    if offset < 0 {
        return Err(RuntimeError::new(format!(
            "Cannot write to a negative offset for {}: {}",
            method, offset
        )));
    }
    let off = offset as usize;
    let needed = off
        .checked_add(size)
        .ok_or_else(|| RuntimeError::new(format!("{} offset {} too large", method, offset)))?;
    if bytes.len() < needed {
        bytes.resize(needed, 0u8);
    }

    let raw = to_u128_value(value);

    // Write bytes according to endianness
    match size {
        1 => {
            bytes[off] = raw as u8;
        }
        2 => {
            let v = raw as u16;
            let encoded = match endian_val {
                1 => v.to_le_bytes(),
                2 => v.to_be_bytes(),
                _ => v.to_ne_bytes(),
            };
            bytes[off..off + 2].copy_from_slice(&encoded);
        }
        4 => {
            let v = raw as u32;
            let encoded = match endian_val {
                1 => v.to_le_bytes(),
                2 => v.to_be_bytes(),
                _ => v.to_ne_bytes(),
            };
            bytes[off..off + 4].copy_from_slice(&encoded);
        }
        8 => {
            let v = raw as u64;
            let encoded = match endian_val {
                1 => v.to_le_bytes(),
                2 => v.to_be_bytes(),
                _ => v.to_ne_bytes(),
            };
            bytes[off..off + 8].copy_from_slice(&encoded);
        }
        16 => {
            let encoded = match endian_val {
                1 => raw.to_le_bytes(),
                2 => raw.to_be_bytes(),
                _ => raw.to_ne_bytes(),
            };
            bytes[off..off + 16].copy_from_slice(&encoded);
        }
        _ => unreachable!(),
    }

    Ok(())
}
