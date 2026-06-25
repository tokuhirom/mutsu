// Helpers for Buf/Blob.write-int{8,16,32,64,128} and .write-uint{8,16,32,64,128}.
//
// Spec: https://docs.raku.org/type/Buf
//   method write-uint8(::T:U: $offset, uint8 $value, $endian = NativeEndian --> Buf:D)
//   method write-uint8(Buf:D: $offset, uint8 $value, $endian = NativeEndian --> Buf:D)
//   (similarly for write-int8, write-uint16, ..., write-int128)

use crate::symbol::Symbol;
use crate::value::{InstanceAttrs, RuntimeError, Value};
use std::sync::Arc;

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

/// The shared attribute cell of a Buf *instance* receiver, captured so a write can be
/// committed straight back into it (propagating to every binding sharing the same buf).
struct BufWriteCell<'a> {
    attributes: &'a Arc<InstanceAttrs>,
    class_sym: Symbol,
    id: u64,
}

/// Non-mut native dispatch for the Buf write methods (`write-int*` / `write-uint*` /
/// `write-num*`), covering both forms:
/// - **type object** (`buf8.write-int32($off, $val, [$endian])`): builds a *fresh*
///   buf, applies the write, returns it — a pure value transform, no interpreter state.
/// - **instance** (`$b.write-int32(...)` reached without a mut binding, e.g. a
///   `\sigilless`-bound buf or `(buf8.new).write-...`): applies the write to a copy of
///   the current bytes and commits them straight into the receiver's *shared* attribute
///   cell (`write_back_sharing` → `commit_attrs`), so every binding observing the same
///   buf sees the mutation. Returns the updated value (which the method call yields).
///
/// Mirrors the interpreter's non-mut branches in `runtime/methods.rs` exactly: the byte
/// transforms are the shared pure `apply_write_int` / `apply_write_num` impls.
///
/// Returns `None` (fall through to the interpreter) for:
/// - non write-int/uint/num methods,
/// - non Buf/Blob receivers,
/// - `Blob`/`blob8` (type object has no such method in raku; instance is immutable —
///   the interpreter raises "Cannot modify immutable Blob"). Current behavior preserved.
/// - bad arity (not 2 or 3 args) and non-numeric offsets — the interpreter raises those
///   errors / resolves Whatever positions.
pub(crate) fn try_native_buf_write(
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let is_num = crate::builtins::buf_write_num::write_num_size(method).is_some();
    let is_int = write_int_info(method).is_some();
    if !(is_num || is_int) {
        return None;
    }
    // Resolve the receiver into class name, current bytes, and (for instances) the
    // shared attribute cell to commit back into.
    let cn: String;
    let mut bytes: Vec<u8> = Vec::new();
    let inst: Option<BufWriteCell>;
    match target {
        Value::Package(name) => {
            cn = name.resolve();
            if !crate::runtime::utils::is_buf_or_blob_class(&cn) {
                return None;
            }
            inst = None;
        }
        Value::Instance {
            class_name,
            attributes,
            id,
        } => {
            cn = class_name.resolve();
            if !crate::runtime::utils::is_buf_or_blob_class(&cn) {
                return None;
            }
            if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                bytes.reserve(items.len());
                for it in items.iter() {
                    bytes.push(match it {
                        Value::Int(i) => (*i).clamp(0, 255) as u8,
                        Value::Num(f) => (*f as i64).clamp(0, 255) as u8,
                        _ => 0,
                    });
                }
            }
            inst = Some(BufWriteCell {
                attributes,
                class_sym: *class_name,
                id: *id,
            });
        }
        _ => return None,
    }
    // Blob has no write methods (type object) and is immutable (instance) — let the
    // interpreter own those semantics so current behavior is byte-identical.
    if cn == "Blob" || cn.starts_with("Blob[") || cn.starts_with("blob") {
        return None;
    }
    if args.len() < 2 || args.len() > 3 {
        return None;
    }
    let offset_i64 = match &args[0] {
        Value::Int(i) => *i,
        Value::Num(f) => *f as i64,
        _ => return None,
    };
    let endian_val = if args.len() == 3 {
        crate::builtins::buf_write_num::decode_endian(&args[2])
    } else {
        0
    };
    let res = if is_num {
        crate::builtins::buf_write_num::apply_write_num(
            &mut bytes, method, offset_i64, &args[1], endian_val,
        )
    } else {
        apply_write_int(&mut bytes, method, offset_i64, &args[1], endian_val)
    };
    if let Err(e) = res {
        return Some(Err(e));
    }
    match inst {
        Some(cell) => {
            let mut updated_map = cell.attributes.to_map();
            updated_map.insert(
                "bytes".to_string(),
                Value::array(bytes.into_iter().map(|b| Value::Int(b as i64)).collect()),
            );
            Some(Ok(Value::write_back_sharing(
                cell.attributes,
                cell.class_sym,
                updated_map,
                cell.id,
            )))
        }
        None => {
            let normalized = crate::runtime::utils::normalize_buf_type_name(&cn);
            Some(Ok(crate::builtins::buf_write_num::make_buf_value(
                &normalized,
                bytes,
            )))
        }
    }
}
