/// Native sized integer type support (int8, uint16, int32, etc.)
///
/// Provides type bounds, range checks, wrapping, and coercion for Raku's
/// native integer types.
use num_bigint::BigInt as NumBigInt;

/// All recognized native integer type names.
pub(crate) const NATIVE_INT_TYPES: &[&str] = &[
    "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "byte", "int", "uint",
];

/// Returns true if `name` is a native integer type.
pub(crate) fn is_native_int_type(name: &str) -> bool {
    NATIVE_INT_TYPES.contains(&name)
}

/// Returns (min, max) bounds for a native integer type as BigInt values.
/// `byte` is an alias for `uint8`.
/// `int` is an alias for `int64`, `uint` is an alias for `uint64`.
pub(crate) fn native_int_bounds(type_name: &str) -> Option<(NumBigInt, NumBigInt)> {
    match type_name {
        "int8" => Some((NumBigInt::from(-128i64), NumBigInt::from(127i64))),
        "int16" => Some((NumBigInt::from(-32768i64), NumBigInt::from(32767i64))),
        "int32" => Some((
            NumBigInt::from(-2147483648i64),
            NumBigInt::from(2147483647i64),
        )),
        "int64" | "int" => Some((
            NumBigInt::from(-9223372036854775808i64),
            NumBigInt::from(9223372036854775807i64),
        )),
        "uint8" | "byte" => Some((NumBigInt::from(0u64), NumBigInt::from(255u64))),
        "uint16" => Some((NumBigInt::from(0u64), NumBigInt::from(65535u64))),
        "uint32" => Some((NumBigInt::from(0u64), NumBigInt::from(4294967295u64))),
        "uint64" | "uint" => Some((
            NumBigInt::from(0u64),
            NumBigInt::from(18446744073709551615u128),
        )),
        _ => None,
    }
}

/// Number of bits for each native type.
fn native_type_bits(type_name: &str) -> Option<u32> {
    match type_name {
        "int8" | "uint8" | "byte" => Some(8),
        "int16" | "uint16" => Some(16),
        "int32" | "uint32" => Some(32),
        "int64" | "uint64" | "int" | "uint" => Some(64),
        _ => None,
    }
}

/// Whether the native type is signed.
fn is_signed_native(type_name: &str) -> bool {
    matches!(type_name, "int8" | "int16" | "int32" | "int64" | "int")
}

/// Wrap a BigInt value to fit within the native type's range.
/// This performs modular wrapping (like C integer overflow).
pub(crate) fn wrap_native_int(type_name: &str, value: &NumBigInt) -> NumBigInt {
    let bits = match native_type_bits(type_name) {
        Some(b) => b,
        None => return value.clone(),
    };
    let signed = is_signed_native(type_name);

    // Total range = 2^bits
    let modulus = NumBigInt::from(1u64) << bits;

    if signed {
        // For signed: wrap into [-2^(bits-1), 2^(bits-1) - 1]
        let half = &modulus >> 1; // 2^(bits-1)
        let wrapped = ((value % &modulus) + &modulus) % &modulus;
        if wrapped >= half {
            wrapped - modulus
        } else {
            wrapped
        }
    } else {
        // For unsigned: wrap into [0, 2^bits - 1]
        ((value % &modulus) + &modulus) % &modulus
    }
}

/// Check if a value (as BigInt) is within range for the native type.
pub(crate) fn is_in_native_range(type_name: &str, value: &NumBigInt) -> bool {
    if let Some((min, max)) = native_int_bounds(type_name) {
        value >= &min && value <= &max
    } else {
        true
    }
}

/// Coerce a value to the native type by wrapping.
#[allow(dead_code)]
pub(crate) fn coerce_to_native_int(type_name: &str, value: &NumBigInt) -> NumBigInt {
    wrap_native_int(type_name, value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int8_bounds() {
        let (min, max) = native_int_bounds("int8").unwrap();
        assert_eq!(min, NumBigInt::from(-128));
        assert_eq!(max, NumBigInt::from(127));
    }

    #[test]
    fn test_uint8_bounds() {
        let (min, max) = native_int_bounds("uint8").unwrap();
        assert_eq!(min, NumBigInt::from(0));
        assert_eq!(max, NumBigInt::from(255));
    }

    #[test]
    fn test_byte_is_uint8() {
        assert_eq!(native_int_bounds("byte"), native_int_bounds("uint8"));
    }

    #[test]
    fn test_wrap_int8_overflow() {
        // 128 should wrap to -128
        let val = NumBigInt::from(128);
        assert_eq!(wrap_native_int("int8", &val), NumBigInt::from(-128));
    }

    #[test]
    fn test_wrap_int8_underflow() {
        // -129 should wrap to 127
        let val = NumBigInt::from(-129);
        assert_eq!(wrap_native_int("int8", &val), NumBigInt::from(127));
    }

    #[test]
    fn test_wrap_uint8_overflow() {
        // 256 should wrap to 0
        let val = NumBigInt::from(256);
        assert_eq!(wrap_native_int("uint8", &val), NumBigInt::from(0));
    }

    #[test]
    fn test_wrap_uint8_underflow() {
        // -1 should wrap to 255
        let val = NumBigInt::from(-1);
        assert_eq!(wrap_native_int("uint8", &val), NumBigInt::from(255));
    }

    #[test]
    fn test_in_range() {
        assert!(is_in_native_range("int8", &NumBigInt::from(127)));
        assert!(is_in_native_range("int8", &NumBigInt::from(-128)));
        assert!(!is_in_native_range("int8", &NumBigInt::from(128)));
        assert!(!is_in_native_range("int8", &NumBigInt::from(-129)));
    }

    #[test]
    fn test_coerce_int8() {
        // 255 as int8 should be -1
        assert_eq!(
            coerce_to_native_int("int8", &NumBigInt::from(255)),
            NumBigInt::from(-1)
        );
    }
}
