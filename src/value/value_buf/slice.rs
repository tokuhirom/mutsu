//! A run of a buffer's elements copied into a fresh buffer as storage bytes —
//! the `subbuf` copy, without decoding each element into a `Value` and
//! encoding it back (#18643).

use super::{AttrMap, InstanceAttrs, Symbol, Value, elem_type, elems_key, node_in, storage_value};

/// A fresh `class_name` instance holding elements `from .. from + take` of
/// the buffer `attrs`, copied as the node's storage bytes.
///
/// `None` when the instance carries no element storage, the run is out of
/// range, or the node's element layout is not the one `class_name` spells
/// (a re-tagged node): the caller then takes the per-element path, which
/// re-encodes each element at the class's width.
// Cost: O(t), t = take (one byte-slice copy).
pub(crate) fn buf_slice(
    class_name: Symbol,
    attrs: &InstanceAttrs,
    from: usize,
    take: usize,
) -> Option<Value> {
    let (bytes, width, kind) = {
        let map = attrs.as_map();
        let node = node_in(&map)?;
        if elem_type(&class_name.resolve()) != (node.width, node.kind) {
            return None;
        }
        let w = node.width as usize;
        let start = from.checked_mul(w)?;
        let end = start.checked_add(take.checked_mul(w)?)?;
        (node.bytes.get(start..end)?.to_vec(), node.width, node.kind)
    };
    let mut out = AttrMap::new();
    out.insert(elems_key(), storage_value(bytes, width, kind));
    Some(Value::make_instance(class_name, out))
}
