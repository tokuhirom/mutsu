use super::*;

impl Interpreter {
    pub(in crate::runtime) fn type_matches(constraint: &str, value_type: &str) -> bool {
        if constraint == "Mu" {
            return true;
        }
        if constraint == "Any" {
            // Junction is a direct subtype of Mu, not Any
            return value_type != "Junction";
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
                "Int" | "Num" | "Rat" | "FatRat" | "Bool" | "UInt"
            )
        {
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
                "Int" | "Num" | "Str" | "Bool" | "Rat" | "FatRat" | "Complex"
            )
        {
            return true;
        }
        if constraint == "Stringy" && matches!(value_type, "Str") {
            return true;
        }
        if matches!(constraint, "Callable" | "Code" | "Block")
            && matches!(
                value_type,
                "Sub" | "Routine" | "Method" | "Block" | "WhateverCode"
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
                "Array" | "List" | "Seq" | "Range" | "Buf" | "Blob" | "Capture"
            )
        {
            return true;
        }
        // Array is-a List in Raku type hierarchy
        if constraint == "List" && matches!(value_type, "Array" | "List" | "Seq") {
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
        false
    }
}
