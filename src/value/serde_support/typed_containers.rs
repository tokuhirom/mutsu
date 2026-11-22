//! The container metadata an `Array` or `Hash` carries besides its elements:
//! its declared type (`Map` vs `Hash`, `array[int32]`), element and key type
//! constraints, `is default(...)` value, descriptor name, nqp element kind and
//! the bare-value flags.
//!
//! The plain [`SerValue::Array`]/[`SerValue::Hash`] forms drop all of it, so a
//! `Map` came back from the cache as a `Hash` and a `my int32 @` as an untyped
//! `Array`. A container that carries any of it is written in the typed form
//! instead; one that carries none keeps the plain form (and its encoding).

use super::{SerValue, ser_to_value, value_to_ser};
use crate::value::{ArrayData, ArrayKind, HashData, NqpElemKind, Value};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// The metadata fields shared by `ArrayData` and `HashData`.
#[derive(Serialize, Deserialize)]
pub(super) struct ContainerMeta {
    value_type: Option<String>,
    key_type: Option<String>,
    declared_type: Option<String>,
    default: Option<Box<SerValue>>,
    descriptor_name: Option<String>,
    /// [`NqpElemKind`] as `0` object, `1` int, `2` num, `3` str.
    nqp_elem: u8,
    bare_values: bool,
    storage_cells: bool,
}

impl ContainerMeta {
    // Cost: O(d), d = size of the default value.
    fn new(
        value_type: &Option<String>,
        key_type: &Option<String>,
        declared_type: &Option<String>,
        default: &Option<Box<Value>>,
        descriptor_name: &Option<Box<str>>,
    ) -> Result<Self, String> {
        Ok(ContainerMeta {
            value_type: value_type.clone(),
            key_type: key_type.clone(),
            declared_type: declared_type.clone(),
            default: match default {
                Some(d) => Some(Box::new(value_to_ser(d)?)),
                None => None,
            },
            descriptor_name: descriptor_name.as_deref().map(str::to_string),
            nqp_elem: 0,
            bare_values: false,
            storage_cells: false,
        })
    }

    // Cost: O(1).
    fn is_empty(&self) -> bool {
        self.value_type.is_none()
            && self.key_type.is_none()
            && self.declared_type.is_none()
            && self.default.is_none()
            && self.descriptor_name.is_none()
            && self.nqp_elem == 0
            && !self.bare_values
            && !self.storage_cells
    }

    /// Whether the default value records an object id.
    // Cost: O(d), d = size of the default value.
    pub(super) fn has_identity(&self) -> bool {
        self.default.as_ref().is_some_and(|d| d.has_identity())
    }
}

/// The array `data` of kind `kind` in serialized form: typed when it carries
/// metadata, plain otherwise.
// Cost: O(n), n = size of the array.
pub(super) fn ser_array(data: &ArrayData, kind: ArrayKind) -> Result<SerValue, String> {
    let items: Vec<SerValue> = data.iter().map(value_to_ser).collect::<Result<_, _>>()?;
    let mut meta = ContainerMeta::new(
        &data.value_type,
        &data.key_type,
        &data.declared_type,
        &data.default,
        &data.descriptor_name,
    )?;
    meta.nqp_elem = match data.nqp_elem {
        NqpElemKind::Object => 0,
        NqpElemKind::Int => 1,
        NqpElemKind::Num => 2,
        NqpElemKind::Str => 3,
    };
    if meta.is_empty() {
        return Ok(SerValue::Array(items, kind));
    }
    Ok(SerValue::TypedArray {
        items,
        kind,
        meta: Box::new(meta),
    })
}

/// The hash `data` in serialized form: typed when it carries metadata, plain
/// otherwise.
// Cost: O(n), n = size of the hash.
pub(super) fn ser_hash(data: &HashData) -> Result<SerValue, String> {
    let map: HashMap<String, SerValue> = data
        .map
        .iter()
        .map(|(k, v)| value_to_ser(v).map(|sv| (k.clone(), sv)))
        .collect::<Result<_, _>>()?;
    let mut meta = ContainerMeta::new(
        &data.value_type,
        &data.key_type,
        &data.declared_type,
        &data.default,
        &data.descriptor_name,
    )?;
    meta.bare_values = data.bare_values;
    meta.storage_cells = data.storage_cells;
    if meta.is_empty() {
        return Ok(SerValue::Hash(map));
    }
    Ok(SerValue::TypedHash {
        map,
        meta: Box::new(meta),
    })
}

// Cost: O(d), d = size of the default value.
fn default_value(meta: &mut ContainerMeta) -> Option<Box<Value>> {
    meta.default.take().map(|d| Box::new(ser_to_value(*d)))
}

/// Rebuild a typed array.
// Cost: O(n), n = size of the array.
pub(super) fn de_array(items: Vec<SerValue>, kind: ArrayKind, mut meta: ContainerMeta) -> Value {
    let mut data: ArrayData = items.into_iter().map(ser_to_value).collect();
    data.default = default_value(&mut meta);
    data.value_type = meta.value_type;
    data.key_type = meta.key_type;
    data.declared_type = meta.declared_type;
    data.descriptor_name = meta.descriptor_name.map(String::into_boxed_str);
    data.nqp_elem = match meta.nqp_elem {
        1 => NqpElemKind::Int,
        2 => NqpElemKind::Num,
        3 => NqpElemKind::Str,
        _ => NqpElemKind::Object,
    };
    Value::Array(crate::gc::Gc::new(data), kind)
}

/// Rebuild a typed hash.
// Cost: O(n), n = size of the hash.
pub(super) fn de_hash(map: HashMap<String, SerValue>, mut meta: ContainerMeta) -> Value {
    let mut data = HashData::new(map.into_iter().map(|(k, v)| (k, ser_to_value(v))).collect());
    data.default = default_value(&mut meta);
    data.value_type = meta.value_type;
    data.key_type = meta.key_type;
    data.declared_type = meta.declared_type;
    data.descriptor_name = meta.descriptor_name.map(String::into_boxed_str);
    data.bare_values = meta.bare_values;
    data.storage_cells = meta.storage_cells;
    Value::hash(data)
}
