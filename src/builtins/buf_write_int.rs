// Helpers for Buf/Blob.write-int{8,16,32,64,128} and .write-uint{8,16,32,64,128}.
//
// Spec: https://docs.raku.org/type/Buf
//   method write-uint8(::T:U: $offset, uint8 $value, $endian = NativeEndian --> Buf:D)
//   method write-uint8(Buf:D: $offset, uint8 $value, $endian = NativeEndian --> Buf:D)
//   (similarly for write-int8, write-uint16, ..., write-int128)

use crate::value::value_buf::buf_elem_width;
use crate::value::{RuntimeError, Value, ValueView};

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
    match value.view() {
        ValueView::Int(i) => i as u128,
        ValueView::Num(f) => f as i128 as u128,
        ValueView::BigInt(bi) => {
            // Extract the low 128 bits of the BigInt
            use num_bigint::BigInt;
            use num_traits::ToPrimitive;
            let mask = BigInt::from(1u8) << 128;
            let masked: BigInt = ((bi.as_ref() % &mask) + &mask) % &mask;
            masked.to_u128().unwrap_or(0)
        }
        ValueView::Bool(b) => i64::from(b) as u128,
        ValueView::Str(s) => s
            .parse::<u128>()
            .unwrap_or_else(|_| s.parse::<i128>().map(|i| i as u128).unwrap_or(0)),
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
            if d == 0 {
                0
            } else {
                ((n as i128) / (d as i128)) as u128
            }
        }
        // An enum value (e.g. `CBOR_Tag_Date_Integer` from `enum CBORMinMax (...
        // => 100, ...)`) passed directly as a `write-uint*`/`nqp::writeuint`
        // argument must write its underlying numeric value, not silently write
        // 0 — the pattern `nqp::writeuint($buf, $pos, CBOR_Tag_Date_Integer,
        // $ne8)` (CBOR::Simple's Date tag encoding) hit exactly this gap.
        ValueView::Enum { value, .. } => to_u128_value(&value.to_value()),
        // An instance of a user subclass of `Int` (BSON::Simple's
        // `class Int64 is Int`) writes its integer payload, not 0.
        ValueView::Instance { .. } => crate::builtins::native_base::numeric_subclass_payload(value)
            .map_or(0, |payload| to_u128_value(&payload)),
        _ => 0,
    }
}

/// Where a `write-int*` / `write-num*` of `size` bytes at element `offset`
/// lands in a buffer's raw storage, growing that storage if the write runs past
/// its end.
///
/// The offset counts **elements**, not bytes: Rakudo's `$buf.write-uint32(1, …)`
/// overwrites element 1 of a `buf32`, and spans elements 1 and 2 of a `buf16`.
/// So the byte position is `offset * width`, and every width-1 buffer — `Buf`,
/// `Blob`, `buf8`, the overwhelmingly common case — keeps exactly the plain byte
/// offset it always had.
///
/// The growth rule reproduces MoarVM's, which mixes the units: a write past the
/// end resizes the buffer to `offset + size` *elements*. That reads like a slip
/// in `MVMArray`, but it is observable — `buf32.new(1,2).write-uint32(2, $v)`
/// leaves six elements, not three — so mutsu matches it rather than inventing a
/// tidier rule.
pub(crate) fn write_byte_offset(
    bytes: &mut impl GrowBytes,
    method: &str,
    offset: i64,
    size: usize,
    width: usize,
) -> Result<usize, RuntimeError> {
    if offset < 0 {
        return Err(RuntimeError::new(format!(
            "Cannot write to a negative offset for {}: {}",
            method, offset
        )));
    }
    let width = width.max(1);
    let too_large = || RuntimeError::new(format!("{} offset {} too large", method, offset));
    let off = (offset as usize).checked_mul(width).ok_or_else(too_large)?;
    let end = off.checked_add(size).ok_or_else(too_large)?;
    if bytes.len() < end {
        let elems = (offset as usize).checked_add(size).ok_or_else(too_large)?;
        let grown = elems.checked_mul(width).ok_or_else(too_large)?;
        bytes.grow_to(grown);
    }
    Ok(off)
}

/// Byte storage a write can grow: a plain `Vec<u8>` being assembled, or a
/// live buffer node's [`BufBytes`](crate::value::BufBytes) edited in place.
pub(crate) trait GrowBytes: std::ops::DerefMut<Target = [u8]> {
    /// Resize to `len` bytes, zero-filling any new ones.
    fn grow_to(&mut self, len: usize);
}

impl GrowBytes for Vec<u8> {
    fn grow_to(&mut self, len: usize) {
        self.resize(len, 0);
    }
}

impl GrowBytes for crate::value::BufBytes {
    fn grow_to(&mut self, len: usize) {
        self.resize(len, 0);
    }
}

/// Apply a write-int/uint write to a buffer's raw storage bytes (resizing if
/// needed). `width` is the buffer's element width — see [`write_byte_offset`].
pub(crate) fn apply_write_int(
    bytes: &mut impl GrowBytes,
    method: &str,
    offset: i64,
    value: &Value,
    endian_val: i64,
    width: usize,
) -> Result<(), RuntimeError> {
    let (size, _signed) = write_int_info(method).expect("not a write-int method");
    let off = write_byte_offset(bytes, method, offset, size, width)?;

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

/// Apply the `write-*` call `method(args)` to `bytes`, the elements of a buffer
/// of type `class_name` (empty for a type object, whose call answers a fresh
/// buffer). The one implementation every receiver form shares (ADR-11276
/// §9.170). The arguments are checked against Rakudo's signatures: `write-bits`
/// and `write-ubits` take an offset, a bit count and a value; the others an
/// offset, a value and an optional endianness.
// Cost: O(e + w), e = elements (a write past the end grows the buffer), w =
// bytes written.
pub(crate) fn apply_buf_write(
    class_name: &str,
    bytes: Vec<u8>,
    method: &str,
    args: &[Value],
) -> Result<Vec<u8>, RuntimeError> {
    if matches!(method, "write-bits" | "write-ubits") {
        check_positionals(args.len(), 3, 3)?;
        let Some(from) = crate::runtime::Interpreter::value_to_non_negative_i64(&args[0]) else {
            return Err(RuntimeError::new(
                "write-ubits/write-bits expects Int offset",
            ));
        };
        let Some(bits) = crate::runtime::Interpreter::value_to_non_negative_i64(&args[1]) else {
            return Err(RuntimeError::new(
                "write-ubits/write-bits expects Int bit count",
            ));
        };
        return crate::builtins::buf_bits::write_bits(&bytes, from, bits, &args[2]);
    }
    check_positionals(args.len(), 2, 3)?;
    let offset = match args[0].view() {
        ValueView::Int(i) => i,
        ValueView::Num(f) => f as i64,
        _ => 0,
    };
    let endian = args
        .get(2)
        .map_or(0, crate::builtins::buf_write_num::decode_endian);
    let width = buf_elem_width(class_name);
    let mut bytes = bytes;
    if crate::builtins::buf_write_num::write_num_size(method).is_some() {
        crate::builtins::buf_write_num::apply_write_num(
            &mut bytes, method, offset, &args[1], endian, width,
        )?;
    } else {
        apply_write_int(&mut bytes, method, offset, &args[1], endian, width)?;
    }
    Ok(bytes)
}

/// Rakudo's signature-bind failure for `n` positionals against a method taking
/// `min..=max` besides the invocant, which its count includes.
// Cost: O(1).
fn check_positionals(n: usize, min: usize, max: usize) -> Result<(), RuntimeError> {
    if (min..=max).contains(&n) {
        return Ok(());
    }
    let expected = if min == max {
        format!("{}", min + 1)
    } else {
        format!("{} or {}", min + 1, max + 1)
    };
    let few_or_many = if n < min { "few" } else { "many" };
    Err(RuntimeError::new(format!(
        "Too {few_or_many} positionals passed; expected {expected} arguments but got {}",
        n + 1
    )))
}
