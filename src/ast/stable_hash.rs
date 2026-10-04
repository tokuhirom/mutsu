//! A structural hash of AST values that is the same in every process.
//!
//! The derived `Hash` of an AST node hashes each `Symbol` by its intern id, and
//! ids depend on the order a process happened to intern names in: a fresh parse
//! and the same AST read back from the precompilation cache intern in different
//! orders. A fingerprint built that way named one routine differently in two
//! processes. The compiled-bytecode cache carries fingerprints from one process
//! into another, where they meet fingerprints computed afresh (ADR-11756 §2.3),
//! so every fingerprint that can end up in compiled code goes through here.
//!
//! The value is walked through its serde `Serialize` impl, where a `Symbol` is
//! its string, and each primitive is fed straight into a `DefaultHasher`
//! (fixed keys, so the result is stable across processes and runs). Nothing is
//! allocated. Container lengths and enum variant indices are hashed too, so
//! adjacent fields cannot run into each other.

use serde::ser::{self, Serialize};
use std::hash::{DefaultHasher, Hash, Hasher};

/// Feed `value` into `hasher` stably (see the module docs). A value that refuses
/// to serialize contributes what was walked before the refusal: still
/// deterministic, just less specific.
// Cost: O(n), n = size of the value.
pub(crate) fn stable_hash_into<T: Serialize + ?Sized>(value: &T, hasher: &mut DefaultHasher) {
    let _ = value.serialize(&mut StableHasher { hasher });
}

/// The stable hash of `value` on its own.
// Cost: O(n), n = size of the value.
pub(crate) fn stable_hash<T: Serialize + ?Sized>(value: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    stable_hash_into(value, &mut hasher);
    hasher.finish()
}

struct StableHasher<'a> {
    hasher: &'a mut DefaultHasher,
}

#[derive(Debug)]
struct Refused;

impl std::fmt::Display for Refused {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("value refused to serialize")
    }
}

impl std::error::Error for Refused {}

impl ser::Error for Refused {
    fn custom<T: std::fmt::Display>(_msg: T) -> Self {
        Refused
    }
}

type Done = Result<(), Refused>;

impl<'a> StableHasher<'a> {
    fn tag(&mut self, tag: u8) {
        tag.hash(self.hasher);
    }
}

impl<'a, 'b> ser::Serializer for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_i8(self, v: i8) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_i16(self, v: i16) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_i32(self, v: i32) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_i64(self, v: i64) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_i128(self, v: i128) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_u8(self, v: u8) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_u16(self, v: u16) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_u32(self, v: u32) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_u64(self, v: u64) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_u128(self, v: u128) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_f32(self, v: f32) -> Done {
        v.to_bits().hash(self.hasher);
        Ok(())
    }
    fn serialize_f64(self, v: f64) -> Done {
        v.to_bits().hash(self.hasher);
        Ok(())
    }
    fn serialize_char(self, v: char) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_str(self, v: &str) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_bytes(self, v: &[u8]) -> Done {
        v.hash(self.hasher);
        Ok(())
    }
    fn serialize_none(self) -> Done {
        self.tag(0);
        Ok(())
    }
    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Done {
        self.tag(1);
        value.serialize(self)
    }
    fn serialize_unit(self) -> Done {
        Ok(())
    }
    fn serialize_unit_struct(self, _name: &'static str) -> Done {
        Ok(())
    }
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
    ) -> Done {
        index.hash(self.hasher);
        Ok(())
    }
    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Done {
        value.serialize(self)
    }
    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Done {
        index.hash(self.hasher);
        value.serialize(self)
    }
    fn serialize_seq(self, len: Option<usize>) -> Result<Self, Refused> {
        len.hash(self.hasher);
        Ok(self)
    }
    fn serialize_tuple(self, len: usize) -> Result<Self, Refused> {
        len.hash(self.hasher);
        Ok(self)
    }
    fn serialize_tuple_struct(self, _name: &'static str, len: usize) -> Result<Self, Refused> {
        len.hash(self.hasher);
        Ok(self)
    }
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<Self, Refused> {
        index.hash(self.hasher);
        len.hash(self.hasher);
        Ok(self)
    }
    fn serialize_map(self, len: Option<usize>) -> Result<Self, Refused> {
        len.hash(self.hasher);
        Ok(self)
    }
    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self, Refused> {
        len.hash(self.hasher);
        Ok(self)
    }
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<Self, Refused> {
        index.hash(self.hasher);
        len.hash(self.hasher);
        Ok(self)
    }
}

impl<'a, 'b> ser::SerializeSeq for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }
    fn end(self) -> Done {
        Ok(())
    }
}

impl<'a, 'b> ser::SerializeTuple for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }
    fn end(self) -> Done {
        Ok(())
    }
}

impl<'a, 'b> ser::SerializeTupleStruct for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }
    fn end(self) -> Done {
        Ok(())
    }
}

impl<'a, 'b> ser::SerializeTupleVariant for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }
    fn end(self) -> Done {
        Ok(())
    }
}

/// Map entries are hashed in the order the map yields them. Every map the AST
/// serializes is ordered or built in source order; a hash-ordered one would make
/// this hash depend on that map's seed.
impl<'a, 'b> ser::SerializeMap for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Done {
        key.serialize(&mut **self)
    }
    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }
    fn end(self) -> Done {
        Ok(())
    }
}

impl<'a, 'b> ser::SerializeStruct for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, _key: &'static str, value: &T) -> Done {
        value.serialize(&mut **self)
    }
    fn end(self) -> Done {
        Ok(())
    }
}

impl<'a, 'b> ser::SerializeStructVariant for &'b mut StableHasher<'a> {
    type Ok = ();
    type Error = Refused;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, _key: &'static str, value: &T) -> Done {
        value.serialize(&mut **self)
    }
    fn end(self) -> Done {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;

    #[test]
    fn symbols_hash_by_name_not_by_id() {
        let a = Symbol::intern("stable-hash-probe-a");
        let b = Symbol::intern("stable-hash-probe-b");
        assert_eq!(stable_hash(&a), stable_hash(&"stable-hash-probe-a"));
        assert_ne!(stable_hash(&a), stable_hash(&b));
    }

    #[test]
    fn adjacent_fields_do_not_run_together() {
        assert_ne!(
            stable_hash(&(vec!["ab"], vec!["c"])),
            stable_hash(&(vec!["a"], vec!["bc"]))
        );
    }
}
