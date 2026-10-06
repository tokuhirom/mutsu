//! The native body of a Raku-allocated `is repr('CStruct')` object
//! (ADR-11209, #11209).
//!
//! A CStruct that C hands back is a handle: an instance whose `address` points
//! at C's memory ([`super::cstruct_layout`] reads and writes its fields there).
//! A struct Raku allocates (`Foo.new`, `Foo.bless`, `nqp::create(Foo)`) used to
//! be an ordinary instance with no C storage, which reached C as NULL.
//!
//! This module gives such an object a **body**: a zeroed, aligned block of
//! `nativesizeof(Foo)` bytes that the instance owns. Its address goes into the
//! same `address` attribute a handle carries, so every path built for handles
//! applies to it unchanged -- field reads, `is rw` accessor writes, `HAS`
//! members, being passed to C, `nativecast`, `.WHERE`, `.REPR`.
//!
//! Three hidden attributes carry the body:
//!
//! - [`BODY_ATTR`]: the byte block that owns the memory, so the body lives
//!   exactly as long as the instance (and its aliases) and is freed with it;
//! - `address`: the aligned address inside that block (the block is
//!   over-allocated rather than trusting the allocator's alignment);
//! - [`LAYOUT_ATTR`]: the field layout the body was allocated with, encoded as
//!   plain values so the write-through hook ([`Interpreter::cstruct_store_through`])
//!   needs no registry access and no `&mut` -- an object keeps the layout it
//!   was built with even if its class is augmented later.
//!
//! The attribute cell stays a cache of the fields (method entry re-reads it
//! from C, see `seed_cstruct_fields_for_method`); `$!x = v` writes the cell and
//! then the body.

use super::cstruct_layout::{FieldLayout, FieldType, write_field};
use super::*;
use crate::symbol::Symbol;
use crate::value::value_buf;
use std::sync::LazyLock;

/// The hidden attribute holding the byte block that owns the body.
pub(crate) const BODY_ATTR: &str = "__mutsu_cstruct_body";

/// The hidden attribute holding the encoded field layout.
pub(crate) const LAYOUT_ATTR: &str = "__mutsu_cstruct_layout";

/// How many values one field takes in the encoded layout:
/// `name, offset, type tag, size`.
const STRIDE: usize = 4;

// Cost: O(1).
fn layout_key() -> Symbol {
    static KEY: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern(LAYOUT_ATTR));
    *KEY
}

// Cost: O(1).
fn address_key() -> Symbol {
    static KEY: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern("address"));
    *KEY
}

/// The type tag a field is encoded under. An [`FieldType::Embedded`] member is
/// tagged `EMBEDDED` with its size in the fourth slot.
// Cost: O(1).
fn type_tag(ty: FieldType) -> (i64, i64) {
    match ty {
        FieldType::I8 => (1, 0),
        FieldType::I16 => (2, 0),
        FieldType::I32 => (3, 0),
        FieldType::I64 => (4, 0),
        FieldType::U8 => (5, 0),
        FieldType::U16 => (6, 0),
        FieldType::U32 => (7, 0),
        FieldType::U64 => (8, 0),
        FieldType::F32 => (9, 0),
        FieldType::F64 => (10, 0),
        FieldType::Str => (11, 0),
        FieldType::Pointer => (12, 0),
        FieldType::Embedded { size, align } => (13 + align as i64, size as i64),
    }
}

/// The inverse of [`type_tag`]; `None` for a tag no layout produces.
// Cost: O(1).
fn type_from_tag(tag: i64, size: i64) -> Option<FieldType> {
    Some(match tag {
        1 => FieldType::I8,
        2 => FieldType::I16,
        3 => FieldType::I32,
        4 => FieldType::I64,
        5 => FieldType::U8,
        6 => FieldType::U16,
        7 => FieldType::U32,
        8 => FieldType::U64,
        9 => FieldType::F32,
        10 => FieldType::F64,
        11 => FieldType::Str,
        12 => FieldType::Pointer,
        t if t > 13 => FieldType::Embedded {
            size: usize::try_from(size).ok()?,
            align: usize::try_from(t - 13).ok()?,
        },
        _ => return None,
    })
}

/// A struct's layout as it is allocated: the fields, the total size and the
/// alignment, plus the encoded form instances carry.
#[derive(Clone)]
pub(crate) struct BodyLayout {
    pub(crate) fields: Vec<FieldLayout>,
    pub(crate) size: usize,
    pub(crate) align: usize,
    encoded: Value,
}

impl BodyLayout {
    /// `None` for a struct with no fields: it has no bytes to own.
    // Cost: O(f), f = fields.
    fn new(fields: Vec<FieldLayout>) -> Option<Self> {
        let last = fields.last()?;
        let end = last.offset + last.ty.size();
        let align = fields.iter().map(|f| f.ty.align()).max().unwrap_or(1);
        let mut flat = Vec::with_capacity(fields.len() * STRIDE);
        for field in &fields {
            let (tag, size) = type_tag(field.ty);
            flat.push(Value::str(field.name.clone()));
            flat.push(Value::int(field.offset as i64));
            flat.push(Value::int(tag));
            flat.push(Value::int(size));
        }
        Some(BodyLayout {
            fields,
            size: end.div_ceil(align) * align,
            align,
            encoded: Value::array(flat),
        })
    }
}

/// Per-class layouts, valid for one registry write generation (the
/// `create_memo` pattern: an answer computed across a registry write is not
/// recorded).
#[derive(Default)]
pub(crate) struct CstructMemo {
    generation: u64,
    layouts: rustc_hash::FxHashMap<Symbol, Option<BodyLayout>>,
}

impl CstructMemo {
    // Cost: O(1) when current; O(e) on a generation change, e = entries dropped.
    fn sync(&mut self, generation: u64) {
        if self.generation != generation {
            self.generation = generation;
            self.layouts.clear();
        }
    }
}

/// The field name a cell key stands for: the part after any owner qualifier
/// (`Owner\0name`), without a sigil or twigil.
// Cost: O(n), n = chars of the key.
fn field_name_of_key(key: &str) -> &str {
    let after_owner = key.rsplit('\0').next().unwrap_or(key);
    after_owner
        .strip_prefix(['$', '@', '%', '&'])
        .unwrap_or(after_owner)
        .trim_start_matches(['!', '.'])
}

/// Find the field called `name` in an encoded layout.
// Cost: O(f), f = fields.
fn lookup_field(encoded: &Value, name: &str) -> Option<FieldLayout> {
    let ValueView::Array(data, _) = encoded.view() else {
        return None;
    };
    let items = data.items();
    items.chunks_exact(STRIDE).find_map(|chunk| {
        if chunk[0].to_string_value() != name {
            return None;
        }
        let offset = usize::try_from(crate::runtime::to_int(&chunk[1])).ok()?;
        let ty = type_from_tag(crate::runtime::to_int(&chunk[2]), crate::runtime::to_int(&chunk[3]))?;
        Some(FieldLayout {
            name: name.to_string(),
            ty,
            offset,
        })
    })
}

impl Interpreter {
    /// The layout a `class` instance is allocated with, or `None` when the class
    /// has no layout NativeCall can compute (or no fields).
    // Cost: O(1) on a memo hit; a miss costs `cstruct_layout`, O(a) in the class's attributes.
    fn cstruct_body_layout(&mut self, class: Symbol) -> Option<BodyLayout> {
        let generation = self.registry_write_generation();
        self.caches.cstruct_memo.sync(generation);
        if let Some(layout) = self.caches.cstruct_memo.layouts.get(&class) {
            return layout.clone();
        }
        let layout = self.cstruct_layout(class.as_str()).and_then(BodyLayout::new);
        // Computing the layout may cache into the registry; an answer computed
        // across such a write is not recorded.
        if layout.is_some() && self.registry_write_generation() == generation {
            self.caches
                .cstruct_memo
                .layouts
                .insert(class, layout.clone());
        }
        layout
    }

    /// The REPR step of `.new` / `bless` / `nqp::create`: when `instance` is an
    /// object of a class declared `is repr('CStruct')` that Raku just built, give
    /// it its native body and migrate what construction left in the cell
    /// (named arguments, defaults, a `BUILD`'s assignments) into it. A no-op for
    /// any other object, for a handle C returned, and for an object that already
    /// has a body (`new` reaching `bless`).
    // Cost: O(1) when no CStruct class exists; O(f + z) otherwise, f = fields,
    // z = bytes of the struct (zeroed).
    pub(crate) fn install_cstruct_storage(&mut self, instance: &Value) {
        if self.registry().cstruct_classes.is_empty() {
            return;
        }
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = instance.view()
        else {
            return;
        };
        if !self.registry().cstruct_classes.contains(class_name.as_str())
            || attributes.contains_key(address_key())
        {
            return;
        }
        let Some(layout) = self.cstruct_body_layout(class_name) else {
            return;
        };
        let (owner, base) = allocate_body(layout.size, layout.align);
        // What construction left in the cell, by field name. A field the
        // constructor never touched keeps the zero the block started with.
        let cell: Vec<(String, Value)> = attributes
            .as_map()
            .iter()
            .map(|(key, value)| (field_name_of_key(key.as_str()).to_string(), value.clone()))
            .collect();
        for field in &layout.fields {
            if let Some((_, value)) = cell.iter().find(|(name, _)| *name == field.name) {
                let value = value.deref_container();
                // SAFETY: `base` is the start of a live, zeroed block of
                // `layout.size` bytes laid out by `layout.fields`, so the field
                // is in bounds.
                unsafe { write_field(base, field, &value) };
            }
        }
        attributes.insert(BODY_ATTR, owner);
        attributes.insert(layout_key(), layout.encoded);
        attributes.insert(address_key(), Value::int(base as i64));
    }

    /// Write-through: the cell attribute `key` of `attributes` was just stored
    /// with `value`; when the object owns a native body, store the field's bytes
    /// there too. A no-op for every ordinary object (one probe of the map).
    // Cost: O(f), f = fields of the struct; O(1) for an object with no body.
    pub(crate) fn cstruct_store_through(
        &self,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        key: Symbol,
        value: &Value,
    ) {
        let (base, field) = {
            let map = attributes.as_map();
            let Some(encoded) = map.get(layout_key()) else {
                return;
            };
            let Some(ValueView::Int(base)) = map.get(address_key()).map(Value::view) else {
                return;
            };
            let Some(field) = lookup_field(encoded, field_name_of_key(key.as_str())) else {
                return;
            };
            (base as usize, field)
        };
        let value = value.deref_container();
        // SAFETY: the body is owned by this object (its `BODY_ATTR` block is
        // alive while `attributes` is) and the layout is the one it was
        // allocated with, so the field is in bounds.
        unsafe { write_field(base, &field, &value) };
    }
}

/// A zeroed block with room for `size` bytes at `align`: the owning byte block
/// and the aligned address inside it.
// Cost: O(size + align) (the block is zeroed).
fn allocate_body(size: usize, align: usize) -> (Value, usize) {
    let block = value_buf::byte_block(vec![0u8; size + align]);
    let start = value_buf::byte_block_address(&block).unwrap_or(0);
    let aligned = start.div_ceil(align) * align;
    (block, aligned)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn field_names_lose_owner_sigil_and_twigil() {
        assert_eq!(field_name_of_key("a"), "a");
        assert_eq!(field_name_of_key("$a"), "a");
        assert_eq!(field_name_of_key("Owner\0a"), "a");
        assert_eq!(field_name_of_key("Owner\0$!a"), "a");
    }

    #[test]
    fn type_tags_round_trip() {
        for ty in [
            FieldType::I8,
            FieldType::U64,
            FieldType::F32,
            FieldType::Str,
            FieldType::Pointer,
            FieldType::Embedded { size: 24, align: 8 },
        ] {
            let (tag, size) = type_tag(ty);
            assert_eq!(type_from_tag(tag, size), Some(ty));
        }
    }

    #[test]
    fn an_encoded_layout_finds_a_field_by_name() {
        let layout = BodyLayout::new(vec![
            FieldLayout {
                name: "a".into(),
                ty: FieldType::I32,
                offset: 0,
            },
            FieldLayout {
                name: "d".into(),
                ty: FieldType::F64,
                offset: 8,
            },
        ])
        .expect("two fields");
        assert_eq!((layout.size, layout.align), (16, 8));
        let d = lookup_field(&layout.encoded, "d").expect("d");
        assert_eq!((d.offset, d.ty), (8, FieldType::F64));
        assert!(lookup_field(&layout.encoded, "missing").is_none());
    }

    #[test]
    fn a_fieldless_struct_has_no_body() {
        assert!(BodyLayout::new(Vec::new()).is_none());
    }

    #[test]
    fn a_body_is_aligned() {
        let (_owner, base) = allocate_body(24, 16);
        assert_eq!(base % 16, 0);
    }
}
