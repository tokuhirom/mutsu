/// Native sized integer type support (int8, uint16, int32, etc.)
///
/// Provides type bounds, range checks, wrapping, and coercion for Raku's
/// native integer types.
use num_bigint::BigInt as NumBigInt;

/// All recognized native integer type names.
pub(crate) const NATIVE_INT_TYPES: &[&str] = &[
    "int8",
    "int16",
    "int32",
    "int64",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "byte",
    "int",
    "uint",
    "atomicint",
    // The C-width aliases `NativeCall::Types` exports. The marshalling layer
    // (`runtime/nativecall.rs`) already maps every one of these to `CType::I64`
    // / `CType::U64`; declaring them here is what lets a signature or attribute
    // actually *name* one (`has ulong $.length`, `our ulong constant zero = 0`
    // in `DBDish::mysql::Native`). All are 64-bit, matching what MoarVM reports
    // via `nativesizeof` on the platforms mutsu targets (LP64 / LLP64-with-long
    // is not among them).
    "long",
    "ulong",
    "longlong",
    "ulonglong",
    "size_t",
    "ssize_t",
    // `NativeCall::Types::bool` is C's `_Bool`: one byte wide, and *signed* —
    // Rakudo answers -1 for `my bool $x = -1` and 44 for `= 300`, i.e. exactly
    // `int8`. It is an integer type there too (a native `bool` return boxes to
    // `Int`, not to `Bool`).
    "bool",
];

/// Returns true if `name` is a native integer type.
pub(crate) fn is_native_int_type(name: &str) -> bool {
    NATIVE_INT_TYPES.contains(&name)
}

/// The native integer types that are also spelled as a *coercion method* on
/// `Cool` — `42.int8`, `"42".byte`. This is a strict subset of
/// [`NATIVE_INT_TYPES`]: the C-width aliases `NativeCall::Types` exports
/// (`long`, `size_t`, `bool`, ...) and `atomicint` name a type but have no
/// method of that name, so `42.bool` is "No such method" in Rakudo just as
/// `42.Bool` is not spelled `bool`.
///
/// Keeping the two lists apart is not pedantry: a stray coercion method makes
/// `.^can` answer yes for the name on *every* value, and code that probes
/// `$obj.^can($field)` before falling back — the shape
/// `Template::Mustache`'s context lookup has — then calls a method that
/// silently answers 0 instead of moving on to the next candidate.
const NATIVE_INT_COERCE_METHODS: &[&str] = &[
    "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "byte", "int", "uint",
];

/// Whether `name` is spelled as a native-integer coercion method on `Cool`.
pub(crate) fn is_native_int_coerce_method(name: &str) -> bool {
    NATIVE_INT_COERCE_METHODS.contains(&name)
}

/// Returns true if `name` is a native array element type.
pub(crate) fn is_native_array_element_type(name: &str) -> bool {
    is_native_int_type(name) || matches!(name, "num" | "num32" | "num64" | "str")
}

/// Whether `name` spells a *lowercase* native type in term position — the set
/// that Rakudo also lets carry a type smiley (`int:D`, `array:U`, `num64:_`).
///
/// This is the element-type set plus `array` itself. It exists so the parser
/// can accept a smiley after a lowercase native type name; every other
/// lowercase identifier followed by `:D...` stays an ordinary colonpair adverb
/// (`foo:Debug`).
pub(crate) fn is_native_type_name(name: &str) -> bool {
    is_native_array_element_type(name) || name == "array"
}

/// Bool `does Int`, so it unboxes to its integer value (True=1, False=0) when
/// stored in or bound to a native integer slot -- matching raku (`my int $x =
/// 3 >= 2` -> 1, `sub f(int $x); f(True)` -> 1). Any non-Bool value passes
/// through unchanged, so this is a safe prelude for every native-int store/bind
/// path (scalar assign, array element, parameter binding).
pub(crate) fn unbox_bool_to_native_int(val: crate::value::Value) -> crate::value::Value {
    if let crate::value::ValueView::Bool(b) = val.view() {
        crate::value::Value::int(i64::from(b))
    } else {
        val
    }
}

/// Map a native type to the generic family name Rakudo uses in messages such as
/// `Cannot bind to a native <family> array` (e.g. `int8`/`int64` -> `int`,
/// `uint16` -> `uint`, `num32` -> `num`).
pub(crate) fn native_family_name(name: &str) -> &'static str {
    match name {
        "uint" | "uint8" | "uint16" | "uint32" | "uint64" | "byte" | "ulong" | "ulonglong"
        | "size_t" => "uint",
        "num" | "num32" | "num64" => "num",
        "str" => "str",
        _ => "int",
    }
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
        "int64" | "int" | "atomicint" | "long" | "longlong" | "ssize_t" => Some((
            NumBigInt::from(-9223372036854775808i64),
            NumBigInt::from(9223372036854775807i64),
        )),
        "bool" => Some((NumBigInt::from(-128i64), NumBigInt::from(127i64))),
        "uint8" | "byte" => Some((NumBigInt::from(0u64), NumBigInt::from(255u64))),
        "uint16" => Some((NumBigInt::from(0u64), NumBigInt::from(65535u64))),
        "uint32" => Some((NumBigInt::from(0u64), NumBigInt::from(4294967295u64))),
        "uint64" | "uint" | "ulong" | "ulonglong" | "size_t" => Some((
            NumBigInt::from(0u64),
            NumBigInt::from(18446744073709551615u128),
        )),
        _ => None,
    }
}

/// Number of bits for each native type.
pub(crate) fn native_type_bits(type_name: &str) -> Option<u32> {
    match type_name {
        "int8" | "uint8" | "byte" | "bool" => Some(8),
        "int16" | "uint16" => Some(16),
        "int32" | "uint32" => Some(32),
        "int64" | "uint64" | "int" | "uint" | "long" | "ulong" | "longlong" | "ulonglong"
        | "size_t" | "ssize_t" => Some(64),
        _ => None,
    }
}

/// Whether the native type is signed.
pub(crate) fn is_signed_native(type_name: &str) -> bool {
    matches!(
        type_name,
        "int8" | "int16" | "int32" | "int64" | "int" | "long" | "longlong" | "ssize_t" | "bool"
    )
}

/// [`native_int_bounds`] without the two `BigInt` allocations: the bounds of
/// every native integer type fit an `i128`.
pub(crate) fn native_int_bounds_i128(type_name: &str) -> Option<(i128, i128)> {
    let bits = native_type_bits(type_name)?;
    Some(if is_signed_native(type_name) {
        (-(1i128 << (bits - 1)), (1i128 << (bits - 1)) - 1)
    } else {
        (0, (1i128 << bits) - 1)
    })
}

/// [`wrap_native_int`] for a value that fits an `i128` -- which every value a
/// native store or a native-typed parameter ever sees does (`Int` is an
/// `i64`, and a `BigInt` that fits is converted by the caller). Every native
/// type is at most 64 bits wide, so the modulus `2^bits` fits an `i128` too
/// and the wrap is two machine operations. `None` for a name that is not a
/// native integer type.
///
/// The `BigInt` version below computed `((v % m) + m) % m` on heap integers:
/// three divisions and several allocations per store into a `my int` lexical,
/// which the vendored `Test.rakumod` does once per assertion
/// (`$num_of_tests_run = $num_of_tests_run + 1`) and which showed up as ~7k
/// instructions of `num_bigint` division per assertion
/// (`todo/deep/vendor-real-test-module.md`).
pub(crate) fn wrap_native_int_i128(type_name: &str, value: i128) -> Option<i128> {
    let bits = native_type_bits(type_name)?;
    let modulus = 1i128 << bits;
    let wrapped = value.rem_euclid(modulus);
    Some(if is_signed_native(type_name) && wrapped >= modulus >> 1 {
        wrapped - modulus
    } else {
        wrapped
    })
}

/// [`wrap_native_int_i128`] for an `Int` value, producing the wrapped `Value`
/// (an `Int` when the result fits an `i64`, which it always does for a signed
/// type; a `uint64` result above `i64::MAX` boxes). `None` for a name that is
/// not a native integer type.
pub(crate) fn wrap_native_int_value(type_name: &str, value: i64) -> Option<crate::value::Value> {
    let wrapped = wrap_native_int_i128(type_name, value as i128)?;
    Some(match i64::try_from(wrapped) {
        Ok(n) => crate::value::Value::int(n),
        Err(_) => crate::value::Value::bigint(NumBigInt::from(wrapped)),
    })
}

/// Wrap a BigInt value to fit within the native type's range.
/// This performs modular wrapping (like C integer overflow).
pub(crate) fn wrap_native_int(type_name: &str, value: &NumBigInt) -> NumBigInt {
    let bits = match native_type_bits(type_name) {
        Some(b) => b,
        None => return value.clone(),
    };
    // Machine arithmetic whenever the value fits (see `wrap_native_int_i128`).
    if let Some(v) = num_traits::ToPrimitive::to_i128(value)
        && let Some(wrapped) = wrap_native_int_i128(type_name, v)
    {
        return NumBigInt::from(wrapped);
    }
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
    // Machine compare whenever the value fits an `i128` (see
    // `native_int_bounds_i128`); a wider value is out of every native range.
    if let Some((min, max)) = native_int_bounds_i128(type_name) {
        return match num_traits::ToPrimitive::to_i128(value) {
            Some(v) => v >= min && v <= max,
            None => false,
        };
    }
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

    /// The machine-arithmetic fast paths agree with the `BigInt` definitions
    /// they short-circuit, across every width and both signednesses.
    #[test]
    fn i128_fast_paths_agree_with_bigint_paths() {
        use num_traits::ToPrimitive;
        let bigint_wrap = |type_name: &str, value: &NumBigInt| -> NumBigInt {
            let bits = native_type_bits(type_name).unwrap();
            let modulus = NumBigInt::from(1u64) << bits;
            let wrapped = ((value % &modulus) + &modulus) % &modulus;
            if is_signed_native(type_name) && wrapped >= (&modulus >> 1) {
                wrapped - modulus
            } else {
                wrapped
            }
        };
        let samples: [i128; 14] = [
            0,
            1,
            -1,
            127,
            128,
            -128,
            -129,
            255,
            256,
            65_536,
            i64::MAX as i128,
            i64::MIN as i128,
            u64::MAX as i128,
            u64::MAX as i128 + 1,
        ];
        for type_name in [
            "int8", "uint8", "int16", "uint16", "int32", "uint32", "int", "uint",
        ] {
            let (lo, hi) = native_int_bounds(type_name).unwrap();
            assert_eq!(
                native_int_bounds_i128(type_name).unwrap(),
                (lo.to_i128().unwrap(), hi.to_i128().unwrap()),
                "{type_name} bounds"
            );
            for v in samples {
                let big = NumBigInt::from(v);
                assert_eq!(
                    NumBigInt::from(wrap_native_int_i128(type_name, v).unwrap()),
                    bigint_wrap(type_name, &big),
                    "{type_name} wrap {v}"
                );
                assert_eq!(
                    wrap_native_int(type_name, &big),
                    bigint_wrap(type_name, &big),
                    "{type_name} wrap (BigInt entry) {v}"
                );
                assert_eq!(
                    is_in_native_range(type_name, &big),
                    big >= lo && big <= hi,
                    "{type_name} range {v}"
                );
            }
        }
        assert_eq!(wrap_native_int_i128("Str", 5), None);
        assert_eq!(
            wrap_native_int_value("uint8", 300).unwrap(),
            crate::value::Value::int(44)
        );
        assert_eq!(
            wrap_native_int_value("int8", -129).unwrap(),
            crate::value::Value::int(127)
        );
        assert_eq!(
            wrap_native_int_value("uint64", -1).unwrap(),
            crate::value::Value::bigint(NumBigInt::from(u64::MAX))
        );
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
