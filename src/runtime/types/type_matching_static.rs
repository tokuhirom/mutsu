use super::*;

impl Interpreter {
    pub(crate) fn type_matches(constraint: &str, value_type: &str) -> bool {
        if constraint == "Mu" {
            return true;
        }
        if constraint == "Any" {
            // Junction and Mu are direct subtypes of Mu, not Any
            return !matches!(value_type, "Junction" | "Mu");
        }
        if constraint == value_type {
            return true;
        }
        // Qualified name matching: GH2613::R1 should match R1 and vice versa.
        // When one name is qualified (contains ::) and the other is a short name,
        // check if the short name matches the last component of the qualified name.
        if constraint.contains("::")
            && !value_type.contains("::")
            && constraint
                .rsplit("::")
                .next()
                .is_some_and(|short| short == value_type)
        {
            return true;
        }
        if value_type.contains("::")
            && !constraint.contains("::")
            && value_type
                .rsplit("::")
                .next()
                .is_some_and(|short| short == constraint)
        {
            return true;
        }
        // SetHash/BagHash/MixHash are mutable variants sharing the same Value variants
        if constraint == "SetHash" && value_type == "Set" {
            return true;
        }
        if constraint == "BagHash" && value_type == "Bag" {
            return true;
        }
        if constraint == "MixHash" && value_type == "Mix" {
            return true;
        }
        if constraint == "Setty" && matches!(value_type, "Set" | "SetHash") {
            return true;
        }
        if constraint == "Baggy" && matches!(value_type, "Bag" | "BagHash" | "Mix" | "MixHash") {
            return true;
        }
        if constraint == "Mixy" && matches!(value_type, "Mix" | "MixHash") {
            return true;
        }
        // Metamodel:: is an alias for Perl6::Metamodel::
        if constraint.starts_with("Metamodel::") {
            let full = format!("Perl6::{}", constraint);
            if full == value_type {
                return true;
            }
        }
        if value_type.starts_with("Metamodel::") {
            let full = format!("Perl6::{}", value_type);
            if full == constraint {
                return true;
            }
        }
        // Native type aliases: num -> Num, int -> Int, str -> Str
        if constraint == "num" && value_type == "Num" {
            return true;
        }
        if constraint == "int" && value_type == "Int" {
            return true;
        }
        if constraint == "atomicint" && value_type == "Int" {
            return true;
        }
        if constraint == "str"
            && matches!(
                value_type,
                "Str" | "IntStr" | "NumStr" | "RatStr" | "ComplexStr"
            )
        {
            return true;
        }
        // Native integer types match Int values
        if crate::runtime::native_types::is_native_int_type(constraint) && value_type == "Int" {
            return true;
        }
        // Native float types (num32, num64) are subtypes of Num
        if constraint == "Num" && matches!(value_type, "num32" | "num64" | "num") {
            return true;
        }
        if matches!(constraint, "num32" | "num64") && value_type == "Num" {
            return true;
        }
        // Native str type is a subtype of Str
        if constraint == "Str" && value_type == "str" {
            return true;
        }
        // Numeric hierarchy: Int is a Numeric, Num is a Numeric
        if constraint == "Numeric"
            && matches!(
                value_type,
                "Int" | "Num" | "Rat" | "FatRat" | "Complex" | "Bool" | "UInt"
            )
        {
            return true;
        }
        if constraint == "Real"
            && matches!(
                value_type,
                "Int" | "Num" | "Rat" | "FatRat" | "Bool" | "UInt" | "Duration"
            )
        {
            return true;
        }
        // Rational role: Rat and FatRat do Rational[Int,Int]
        if constraint == "Rational" && matches!(value_type, "Rat" | "FatRat") {
            return true;
        }
        // Native integer/num types do Real and Numeric
        if constraint == "Real"
            && (crate::runtime::native_types::is_native_int_type(value_type)
                || matches!(value_type, "num" | "num32" | "num64"))
        {
            return true;
        }
        if constraint == "Numeric"
            && (crate::runtime::native_types::is_native_int_type(value_type)
                || matches!(value_type, "num" | "num32" | "num64"))
        {
            return true;
        }
        // Native str type does Stringy
        if constraint == "Stringy" && value_type == "str" {
            return true;
        }
        if constraint == "Dateish" && matches!(value_type, "Date" | "DateTime") {
            return true;
        }
        // Bool is a subtype of Int in Raku's type hierarchy
        // UInt is a subset of Int
        if constraint == "Int" && matches!(value_type, "Bool" | "UInt") {
            return true;
        }
        if constraint == "Cool"
            && matches!(
                value_type,
                "Int"
                    | "Num"
                    | "Str"
                    | "Bool"
                    | "Rat"
                    | "FatRat"
                    | "Complex"
                    | "Array"
                    | "List"
                    | "Hash"
                    | "Map"
                    | "Range"
                    | "Seq"
            )
        {
            return true;
        }
        if constraint == "Stringy"
            && matches!(
                value_type,
                "Str"
                    | "Buf"
                    | "Blob"
                    | "utf8"
                    | "utf16"
                    | "buf8"
                    | "buf16"
                    | "buf32"
                    | "buf64"
                    | "blob8"
                    | "blob16"
                    | "blob32"
                    | "blob64"
            )
        {
            return true;
        }
        if matches!(constraint, "Callable" | "Code" | "Block")
            && matches!(
                value_type,
                "Sub" | "Routine" | "Method" | "Block" | "WhateverCode" | "Regex"
            )
        {
            return true;
        }
        if constraint == "Routine" && matches!(value_type, "Sub" | "Method" | "Routine") {
            return true;
        }
        if constraint == "Variable" && matches!(value_type, "Scalar" | "Array" | "Hash" | "Sub") {
            return true;
        }
        // ValueObjAt is a subtype of ObjAt
        if constraint == "ObjAt" && value_type == "ValueObjAt" {
            return true;
        }
        // Exception hierarchy: all X::* types are subtypes of Exception
        if constraint == "Exception"
            && (value_type.starts_with("X::")
                || value_type.starts_with("CX::")
                || value_type == "Exception")
        {
            return true;
        }
        // Role-like type relationships
        if constraint == "Positional"
            && matches!(
                value_type,
                "Array"
                    | "List"
                    | "Seq"
                    | "HyperSeq"
                    | "RaceSeq"
                    | "Range"
                    | "Buf"
                    | "Blob"
                    | "Capture"
                    | "array"
            )
        {
            // The bare native `array` type (and its parameterized form
            // `array[int]`, whose base name is `array`) does Positional.
            return true;
        }
        // Array is-a List in Raku type hierarchy
        if constraint == "List"
            && matches!(
                value_type,
                "Array" | "List" | "Seq" | "HyperSeq" | "RaceSeq" | "array"
            )
        {
            return true;
        }
        if constraint == "Associative"
            && matches!(
                value_type,
                "Hash" | "Map" | "Pair" | "Bag" | "Set" | "Mix" | "QuantHash" | "Capture"
            )
        {
            return true;
        }
        // Buf/Blob type hierarchy:
        // Blob is the immutable base; Buf extends Blob (mutable)
        // utf8 is a subtype of Blob
        // buf8/buf16/buf32/buf64 are subtypes of Buf (and transitively Blob)
        // blob8/blob16/blob32/blob64 are subtypes of Blob
        if constraint == "Blob"
            && matches!(
                value_type,
                "Buf"
                    | "utf8"
                    | "utf16"
                    | "buf8"
                    | "buf16"
                    | "buf32"
                    | "buf64"
                    | "blob8"
                    | "blob16"
                    | "blob32"
                    | "blob64"
            )
        {
            return true;
        }
        if constraint == "Buf" && matches!(value_type, "buf8" | "buf16" | "buf32" | "buf64") {
            return true;
        }
        // Sized native buffer constraints. A `blobN` (`Blob[uintN]`) constraint
        // matches any buffer value whose element width is N — including a plain
        // `Buf` (which is `Buf[uint8]`, so `Buf ~~ blob8`) and `utfN` encodings
        // (`utf16 ~~ blob16`). This is what MIME::Base64's binary path needs
        // (`my blob8 $b = pack(...)`, `my blob16 $u = $s.encode('UTF-16')`). A
        // `bufN` constraint is the mutable parameterized type and matches only an
        // explicitly-sized buffer (a plain `Buf` is NOT `buf8`, matching raku).
        if let Some((c_mutable, c_width)) = Self::sized_buf_constraint(constraint) {
            if let Some(v_width) = Self::buf_value_width(value_type, c_mutable) {
                return v_width == c_width;
            }
            return false;
        }
        // IO role: IO::Path and IO::Special do the IO role (IO::Handle does not).
        if constraint == "IO" && matches!(value_type, "IO::Path" | "IO::Special") {
            return true;
        }
        false
    }

    /// Parse a sized native buffer constraint into `(mutable, width_bits)`.
    /// `buf8`/`buf16`/… are mutable (`Buf[uintN]`); `blob8`/… are immutable
    /// (`Blob[uintN]`). Returns `None` for non-sized constraints.
    fn sized_buf_constraint(constraint: &str) -> Option<(bool, u16)> {
        let (mutable, rest) = if let Some(r) = constraint.strip_prefix("buf") {
            (true, r)
        } else if let Some(r) = constraint.strip_prefix("blob") {
            (false, r)
        } else {
            return None;
        };
        match rest {
            "8" => Some((mutable, 8)),
            "16" => Some((mutable, 16)),
            "32" => Some((mutable, 32)),
            "64" => Some((mutable, 64)),
            _ => None,
        }
    }

    /// The element width (in bits) of a buffer-typed value, or `None` if the
    /// value is not a buffer that satisfies a constraint of the given mutability.
    /// A `blobN` constraint (`require_mutable == false`) accepts any buffer of
    /// matching width; a `bufN` constraint (`require_mutable == true`) accepts
    /// only an explicitly-sized mutable buffer (not a plain `Buf`/`utfN`).
    fn buf_value_width(value_type: &str, require_mutable: bool) -> Option<u16> {
        // Explicitly-sized parameterized buffers (`Buf[uint16]`, `Blob[uint8]`).
        if let Some(inner) = value_type
            .strip_prefix("Buf[")
            .or_else(|| value_type.strip_prefix("Blob["))
            .and_then(|s| s.strip_suffix(']'))
        {
            return match inner {
                "uint8" | "int8" => Some(8),
                "uint16" | "int16" => Some(16),
                "uint32" | "int32" => Some(32),
                "uint64" | "int64" => Some(64),
                _ => None,
            };
        }
        if let Some((mutable, width)) = Self::sized_buf_constraint(value_type) {
            if require_mutable && !mutable {
                return None;
            }
            return Some(width);
        }
        if require_mutable {
            // A `bufN` constraint only matches explicitly-sized mutable buffers.
            return None;
        }
        // Role-based widths for a `blobN` constraint: a plain `Buf` is
        // `Buf[uint8]`, and `utfN` encodings are `Blob[uintN]`.
        match value_type {
            "Buf" | "utf8" => Some(8),
            "utf16" => Some(16),
            "utf32" => Some(32),
            _ => None,
        }
    }
}
