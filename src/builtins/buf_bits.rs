// Pure bit-level transforms for Buf/Blob.read-bits/read-ubits/write-bits/write-ubits.
//
// Spec: https://docs.raku.org/type/Buf
//   method read-ubits(Blob:D: int $pos, uint $bits --> UInt:D)
//   method read-bits(Blob:D: int $pos, uint $bits --> Int:D)
//   method write-ubits(Buf:D: int $pos, uint $bits, UInt:D $value --> Buf:D)
//   method write-bits(Buf:D: int $pos, uint $bits, Int:D $value --> Buf:D)
//
// These operate purely on a byte slice and carry no interpreter state, so they
// live in `builtins/` as the single authoritative implementation shared by both
// the bytecode VM (native dispatch) and the tree-walking interpreter fallback.

use crate::value::{RuntimeError, Value};
use num_bigint::BigInt;
use num_traits::ToPrimitive;

/// Read `bits` bits starting at bit offset `from` from `bytes`, big-endian bit
/// order. When `signed` is true the result is sign-extended (`read-bits`),
/// otherwise it is unsigned (`read-ubits`).
pub(crate) fn read_bits(
    bytes: &[u8],
    from: i64,
    bits: i64,
    signed: bool,
) -> Result<Value, RuntimeError> {
    if from < 0 || bits < 0 {
        return Err(RuntimeError::new(
            "read from out of range. Is: negative offset/length",
        ));
    }
    let from_u = from as usize;
    let bits_u = bits as usize;
    let total_bits = bytes.len().saturating_mul(8);
    if from_u
        .checked_add(bits_u)
        .is_none_or(|end| end > total_bits)
    {
        return Err(RuntimeError::new(format!(
            "read from out of range. Is: {}, should be in 0..{}",
            from, total_bits
        )));
    }
    let mut out = BigInt::from(0u8);
    for i in 0..bits_u {
        let bit_index = from_u + i;
        let byte_index = bit_index / 8;
        let bit_in_byte = 7 - (bit_index % 8);
        let bit = (bytes[byte_index] >> bit_in_byte) & 1;
        out = (out << 1) + BigInt::from(bit);
    }
    if !signed || bits_u == 0 {
        return Ok(Value::from_bigint(out));
    }
    let sign_mask = BigInt::from(1u8) << (bits_u - 1);
    if (&out & &sign_mask) != BigInt::from(0u8) {
        let modulus = BigInt::from(1u8) << bits_u;
        Ok(Value::from_bigint(out - modulus))
    } else {
        Ok(Value::from_bigint(out))
    }
}

/// Write `bits` bits of `value` starting at bit offset `from` into a copy of
/// `bytes`, big-endian bit order, growing the buffer as needed. Returns the new
/// byte vector. `value` is reduced modulo `2**bits` (so both signed and unsigned
/// forms share this path).
pub(crate) fn write_bits(
    bytes: &[u8],
    from: i64,
    bits: i64,
    value: &Value,
) -> Result<Vec<u8>, RuntimeError> {
    if from < 0 || bits < 0 {
        return Err(RuntimeError::new(
            "write out of range. Is: negative offset/length",
        ));
    }
    let from_u = from as usize;
    let bits_u = bits as usize;
    let required_bits = from_u.saturating_add(bits_u);
    let required_bytes = required_bits.div_ceil(8);

    let mut out = bytes.to_vec();
    if out.len() < required_bytes {
        out.resize(required_bytes, 0);
    }

    if bits_u == 0 {
        return Ok(out);
    }

    let modulus = BigInt::from(1u8) << bits_u;
    let mut encoded = value.to_bigint();
    encoded = ((encoded % &modulus) + &modulus) % &modulus;

    let first_bit = from_u & 7;
    let last_bit = required_bits & 7;
    let first_byte = from_u >> 3;
    let last_byte = (required_bits - 1) >> 3;

    if last_bit != 0 {
        encoded <<= 8 - last_bit;
    }

    let left_mask = if first_bit == 0 {
        0
    } else {
        (((1u16 << first_bit) - 1) as u8) << (8 - first_bit)
    };
    // Rakudo retains this single boundary bit rather than every bit to the
    // right of a partial write. In particular, writing four bits at offset
    // zero into 0x05 produces 0x30, not 0x35.
    let right_mask = if last_bit == 0 {
        0
    } else {
        1u8 << (8 - last_bit - 1)
    };
    let low_byte = |value: &BigInt| (value & BigInt::from(0xffu16)).to_u8().unwrap_or(0);

    if first_byte == last_byte {
        out[first_byte] = low_byte(&encoded) | (out[first_byte] & (left_mask | right_mask));
        return Ok(out);
    }

    let mut byte_index = last_byte;
    if last_bit != 0 {
        out[byte_index] = low_byte(&encoded) | (out[byte_index] & right_mask);
        encoded >>= 8;
    } else {
        byte_index += 1;
    }

    let last_full_byte = first_byte + usize::from(first_bit != 0);
    while byte_index > last_full_byte {
        byte_index -= 1;
        out[byte_index] = low_byte(&encoded);
        encoded >>= 8;
    }
    if first_bit != 0 {
        byte_index -= 1;
        out[byte_index] = low_byte(&encoded) | (out[byte_index] & left_mask);
    }
    Ok(out)
}
