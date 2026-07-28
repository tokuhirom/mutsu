//! `Buf`/`Blob` element storage — the single accessor layer (ADR-0015 P2 step 1).
//!
//! A `Buf`/`Blob` has no dedicated `Value` variant. It is a plain
//! `Value::Instance` whose one attribute holds a `Value::Array` with **one boxed
//! `Value::Int` per element**. Until now every one of the ~104 places that
//! touched that storage spelled the attribute name itself and open-coded the
//! `ValueView::Array` match, so the representation could not be changed without
//! editing forty files.
//!
//! This module is that chokepoint. The attribute name is private to it, and
//! every read, write, probe and construction goes through one of the functions
//! below. Nothing here changes behaviour — it is a pure refactor whose point is
//! to make [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
//! P2 step 2 (a contiguous native-backed node with an element width, so
//! `Buf.REPR` can honestly answer `VMArray` and NativeCall can hand C a real
//! `MVMArrayB` body) a change to *this file* rather than to its callers.
//!
//! Three levels are offered deliberately:
//!
//! - the **element** functions ([`buf_elems`], [`set_buf_elems`], …) decode to
//!   and from `Vec<Value>`; under P2 they become the encode/decode boundary;
//! - the **byte** functions ([`buf_bytes`], [`with_buf_bytes`], [`buf_len`], …)
//!   speak the representation P2 will actually store, so a caller that only
//!   wants bytes or a count never round-trips through boxed `Value`s;
//! - the **storage** functions ([`buf_storage`], [`set_buf_storage`]) move the
//!   container across without decoding it, for the coercions that only re-tag a
//!   buffer (`.Buf`, `.Blob`); under P2 they become a node share.
//!
//! [`buf_elem_width`] is the fourth thing P2 needs and the one that is *not*
//! stored in the data at all today: the element width lives only in the class
//! name (`Buf[uint16]`), and four separate places used to re-derive it with
//! their own `cn.contains("16")` ladder. It is derived here now, so P2 has one
//! place to move it from the name into the node.
//!
//! [`is_buf_or_blob_class`](crate::runtime::utils::is_buf_or_blob_class) stays
//! the companion class-name filter: this module answers "what is in there", not
//! "is this a Buf".

use super::{AttrMap, InstanceAttrs, Value, ValueView};
use crate::symbol::Symbol;

/// The attribute a `Buf`/`Blob`-shaped instance keeps its elements under.
///
/// Private on purpose: it is the thing P2 deletes. If you find yourself wanting
/// it outside this module, the accessor you need is missing — add it here.
const ELEMS_ATTR: &str = "bytes";

/// The elements of a `Buf`/`Blob`-shaped instance, as owned `Value`s.
///
/// `None` when the instance carries no element storage — a type object
/// (`Blob` itself), or an instance that merely happens to be passed in here.
/// Most callers want [`buf_elems_or_empty`]; use this one when "absent" and
/// "empty" must be told apart.
pub(crate) fn buf_elems(attrs: &InstanceAttrs) -> Option<Vec<Value>> {
    buf_elems_in(&attrs.as_map())
}

/// [`buf_elems`] with an absent buffer read as empty — the shape of the many
/// call sites that opened with `if let Some(..) = .. else { Vec::new() }`.
pub(crate) fn buf_elems_or_empty(attrs: &InstanceAttrs) -> Vec<Value> {
    buf_elems(attrs).unwrap_or_default()
}

/// [`buf_elems`] against an attribute map already in hand (a `to_map()` snapshot
/// or a live read guard), so a caller holding one does not take a second lock.
pub(crate) fn buf_elems_in(map: &AttrMap) -> Option<Vec<Value>> {
    match map.get(ELEMS_ATTR)?.view() {
        ValueView::Array(items, ..) => Some(items.to_vec()),
        _ => None,
    }
}

/// Run `f` over the elements without copying them.
///
/// The borrow is held by an attribute read guard for the duration of the call,
/// so `f` must not re-enter the same instance's attribute cell for writing.
/// Returns `None` (without calling `f`) when there is no element storage.
pub(crate) fn with_buf_elems<R>(attrs: &InstanceAttrs, f: impl FnOnce(&[Value]) -> R) -> Option<R> {
    let map = attrs.as_map();
    match map.get(ELEMS_ATTR)?.view() {
        ValueView::Array(items, ..) => Some(f(items.as_slice())),
        _ => None,
    }
}

/// Whether this instance carries element storage at all. Distinguishes a real
/// (possibly empty) buffer from a `Blob`/`Buf` **type object**, which has none.
pub(crate) fn has_buf_elems(attrs: &InstanceAttrs) -> bool {
    attrs.contains_key(ELEMS_ATTR)
}

/// [`has_buf_elems`] against an attribute map already in hand.
pub(crate) fn has_buf_elems_in(map: &AttrMap) -> bool {
    map.contains_key(ELEMS_ATTR)
}

/// A fresh attribute map holding `elems` — the map to hand
/// `Value::make_instance` / `Value::write_back_sharing`.
pub(crate) fn buf_attrs(elems: Vec<Value>) -> AttrMap {
    let mut map = AttrMap::new();
    set_buf_elems(&mut map, elems);
    map
}

/// A fresh `Buf`/`Blob`-shaped instance of `class_name` holding `elems`.
pub(crate) fn make_buf(class_name: Symbol, elems: Vec<Value>) -> Value {
    Value::make_instance(class_name, buf_attrs(elems))
}

/// A fresh plain `Buf` over raw bytes — the commonest construction of all (I/O
/// reads, socket receives, `Proc::Async` chunks). Under P2 this stops boxing.
pub(crate) fn make_buf_from_u8(bytes: &[u8]) -> Value {
    make_buf_from_bytes(Symbol::intern("Buf"), bytes)
}

/// Raw bytes as the boxed element list this representation stores.
pub(crate) fn bytes_to_elems(bytes: &[u8]) -> Vec<Value> {
    bytes.iter().map(|b| Value::int(*b as i64)).collect()
}

/// Store `elems` into a map being built or updated, then handed to
/// `make_instance` / `commit_attrs`.
pub(crate) fn set_buf_elems(map: &mut AttrMap, elems: Vec<Value>) {
    map.insert(ELEMS_ATTR, Value::array(elems));
}

/// Store `elems` into a **live** instance, through its shared attribute cell —
/// visible to every alias, no rebind of the holding variable needed.
pub(crate) fn store_buf_elems(attrs: &InstanceAttrs, elems: Vec<Value>) {
    attrs.insert(ELEMS_ATTR, Value::array(elems));
}

/// Mutate the elements in place through the shared cell, without decoding the
/// whole buffer first (`$b[i] = v`). `None` when there is no element storage.
pub(crate) fn with_buf_elems_mut<R>(
    attrs: &InstanceAttrs,
    f: impl FnOnce(&mut Vec<Value>) -> R,
) -> Option<R> {
    attrs
        .with_attr_mut(ELEMS_ATTR, |slot| {
            slot.with_array_mut(|items, _| f(crate::gc::Gc::make_mut(items)))
        })
        .flatten()
}

/// The element container itself, cloned, for the coercions that re-tag a buffer
/// without looking inside it (`.Buf`, `.Blob`). Pair with [`set_buf_storage`].
pub(crate) fn buf_storage(map: &AttrMap) -> Option<Value> {
    let stored = map.get(ELEMS_ATTR)?;
    matches!(stored.view(), ValueView::Array(..)).then(|| stored.clone())
}

/// Store a container obtained from [`buf_storage`] into a map being built.
pub(crate) fn set_buf_storage(map: &mut AttrMap, storage: Value) {
    map.insert(ELEMS_ATTR, storage);
}

/// The elements as an array `Value` of `kind`, **sharing** the backing node
/// rather than copying it — what the `.List`/`.list`/`.Array` coercions do
/// today. The share is invisible: element writes go through
/// [`with_buf_elems_mut`], whose `Gc::make_mut` forks a shared node.
pub(crate) fn buf_elems_as_array(map: &AttrMap, kind: super::ArrayKind) -> Option<Value> {
    match map.get(ELEMS_ATTR)?.view() {
        ValueView::Array(items, ..) => Some(Value::array_with_kind(items.clone(), kind)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Byte-level access — what P2 will actually store.
// ---------------------------------------------------------------------------

/// One element, as the byte it occupies in a width-1 buffer.
///
/// **Truncating**, which is the convention Raku itself stores by: `Buf.new(300)`
/// holds `0x2C` and `Buf.new(-1)` holds `0xFF` in both mutsu and Rakudo, and
/// every mutation path (`.new`, `[i] =`, `push`, `append`, `unshift`, `splice`)
/// already masks on the way in. Three different conventions used to be spelled
/// out at the call sites — this one, a `.clamp(0, 255)` one, and one going via
/// `to_int` — and they could only disagree about a buffer whose elements exceed
/// its width, which the masking makes unreachable. For a wider buffer this is
/// the low byte of the element, matching what the truncating sites did.
pub(crate) fn elem_to_u8(v: &Value) -> u8 {
    match v.view() {
        ValueView::Int(i) => i as u8,
        ValueView::Num(f) => f as i64 as u8,
        ValueView::BigInt(n) => {
            use num_traits::ToPrimitive;
            // `to_u64` is `None` for a negative or oversized value; fall back to
            // the low 64 bits so this stays a truncation rather than a zero.
            n.as_ref()
                .to_u64()
                .unwrap_or_else(|| n.as_ref().to_i64().unwrap_or(0) as u64) as u8
        }
        _ => 0,
    }
}

/// The number of **elements**, without decoding any of them. Not the number of
/// bytes — see [`buf_elem_width`], and `.bytes` is `elems * width`.
pub(crate) fn buf_len(attrs: &InstanceAttrs) -> Option<usize> {
    with_buf_elems(attrs, <[Value]>::len)
}

/// [`buf_len`] with an absent buffer read as empty.
pub(crate) fn buf_len_or_zero(attrs: &InstanceAttrs) -> usize {
    buf_len(attrs).unwrap_or(0)
}

/// The elements as one truncated byte each ([`elem_to_u8`]).
///
/// `None` when the instance carries no element storage, exactly as
/// [`buf_elems`]. For a width-1 buffer — every `Buf`/`Blob`/`utf8` — these are
/// the buffer's real bytes, and P2 hands them over without decoding anything.
pub(crate) fn buf_bytes(attrs: &InstanceAttrs) -> Option<Vec<u8>> {
    with_buf_bytes(attrs, <[u8]>::to_vec)
}

/// [`buf_bytes`] with an absent buffer read as empty.
pub(crate) fn buf_bytes_or_empty(attrs: &InstanceAttrs) -> Vec<u8> {
    buf_bytes(attrs).unwrap_or_default()
}

/// [`buf_bytes`] against an attribute map already in hand.
pub(crate) fn buf_bytes_in(map: &AttrMap) -> Option<Vec<u8>> {
    Some(buf_elems_in(map)?.iter().map(elem_to_u8).collect())
}

/// Run `f` over the bytes without handing out an owned `Vec`.
///
/// Today the slice is a temporary this function builds; under P2 a width-1
/// buffer passes its storage straight through. Callers that go on to mutate the
/// bytes want [`buf_bytes`] instead.
pub(crate) fn with_buf_bytes<R>(attrs: &InstanceAttrs, f: impl FnOnce(&[u8]) -> R) -> Option<R> {
    let bytes = with_buf_elems(attrs, |items| {
        items.iter().map(elem_to_u8).collect::<Vec<u8>>()
    })?;
    Some(f(&bytes))
}

/// Store raw bytes into a map being built or updated.
pub(crate) fn set_buf_bytes(map: &mut AttrMap, bytes: &[u8]) {
    set_buf_elems(map, bytes_to_elems(bytes));
}

/// Store raw bytes into a **live** instance, through its shared attribute cell.
pub(crate) fn store_buf_bytes(attrs: &InstanceAttrs, bytes: &[u8]) {
    store_buf_elems(attrs, bytes_to_elems(bytes));
}

/// A fresh `Buf`/`Blob`-shaped instance of `class_name` over raw bytes.
pub(crate) fn make_buf_from_bytes(class_name: Symbol, bytes: &[u8]) -> Value {
    make_buf(class_name, bytes_to_elems(bytes))
}

/// How many bytes one element of a `Buf`/`Blob`-shaped class occupies.
///
/// The width is **not in the data**: it lives only in the class name, which is
/// why this is a string probe rather than a field read. `Buf`, `Blob`, `utf8`
/// and the `uint8`/`int8` parameterisations are 1; `utf16` and anything naming
/// 16/32/64 widen accordingly. Moving this into the node is P2's job — and the
/// reason it is a single function now is so that P2 changes it here rather than
/// in the four places that each had their own `contains` ladder.
pub(crate) fn buf_elem_width(class_name: &str) -> usize {
    if class_name.contains("64") {
        8
    } else if class_name.contains("32") {
        4
    } else if class_name.contains("16") {
        2
    } else {
        1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn buf_of(elems: Vec<Value>) -> Value {
        make_buf(Symbol::intern("Buf"), elems)
    }

    /// The instance's shared attribute cell — a `Gc` clone, so two calls on the
    /// same `Value` alias one cell (which is what the mutation test relies on).
    fn attrs_of(v: &Value) -> crate::gc::Gc<InstanceAttrs> {
        match v.view() {
            ValueView::Instance { attributes, .. } => (*attributes).clone(),
            _ => panic!("not an instance"),
        }
    }

    #[test]
    fn round_trips_elements() {
        let b = buf_of(vec![Value::int(1), Value::int(2)]);
        assert_eq!(
            buf_elems(&attrs_of(&b)),
            Some(vec![Value::int(1), Value::int(2)])
        );
        assert_eq!(with_buf_elems(&attrs_of(&b), <[Value]>::len), Some(2));
    }

    #[test]
    fn absent_storage_is_distinguishable_from_empty() {
        let empty = buf_of(Vec::new());
        assert_eq!(buf_elems(&attrs_of(&empty)), Some(Vec::new()));
        assert!(has_buf_elems(&attrs_of(&empty)));

        let bare = Value::make_instance(Symbol::intern("Buf"), AttrMap::new());
        assert_eq!(buf_elems(&attrs_of(&bare)), None);
        assert_eq!(buf_elems_or_empty(&attrs_of(&bare)), Vec::new());
        assert!(!has_buf_elems(&attrs_of(&bare)));
    }

    #[test]
    fn in_place_mutation_is_visible_through_an_alias() {
        let b = buf_of(vec![Value::int(1)]);
        let alias = attrs_of(&b);
        with_buf_elems_mut(&attrs_of(&b), |items| items.push(Value::int(9)));
        assert_eq!(buf_elems(&alias), Some(vec![Value::int(1), Value::int(9)]));
    }

    #[test]
    fn byte_view_matches_the_element_view() {
        let b = buf_of(bytes_to_elems(&[0, 1, 254, 255]));
        assert_eq!(buf_bytes(&attrs_of(&b)), Some(vec![0, 1, 254, 255]));
        assert_eq!(buf_len(&attrs_of(&b)), Some(4));
        assert_eq!(with_buf_bytes(&attrs_of(&b), <[u8]>::len), Some(4));

        let bare = Value::make_instance(Symbol::intern("Buf"), AttrMap::new());
        assert_eq!(buf_bytes(&attrs_of(&bare)), None);
        assert_eq!(buf_bytes_or_empty(&attrs_of(&bare)), Vec::<u8>::new());
        assert_eq!(buf_len_or_zero(&attrs_of(&bare)), 0);
    }

    /// The unified convention truncates rather than clamping — the same thing
    /// Raku does on the way in, so `Buf.new(300)` is `0x2C` and not `0xFF`. It
    /// only becomes observable for a wider buffer, whose elements legitimately
    /// exceed a byte; there it is the element's low byte.
    #[test]
    fn bytes_truncate_rather_than_clamp() {
        assert_eq!(elem_to_u8(&Value::int(300)), 0x2C);
        assert_eq!(elem_to_u8(&Value::int(-1)), 0xFF);
        assert_eq!(elem_to_u8(&Value::int(0x1170)), 0x70);
        assert_eq!(elem_to_u8(&Value::num(300.9)), 0x2C);
        assert_eq!(elem_to_u8(&Value::str("nope".to_string())), 0);

        let wide = make_buf(Symbol::intern("Buf[uint16]"), vec![Value::int(0x1170)]);
        assert_eq!(buf_bytes(&attrs_of(&wide)), Some(vec![0x70]));
    }

    #[test]
    fn element_width_comes_from_the_class_name() {
        for name in ["Buf", "Blob", "utf8", "Buf[uint8]", "Blob[int8]"] {
            assert_eq!(buf_elem_width(name), 1, "{name}");
        }
        assert_eq!(buf_elem_width("utf16"), 2);
        assert_eq!(buf_elem_width("Buf[uint16]"), 2);
        assert_eq!(buf_elem_width("blob32"), 4);
        assert_eq!(buf_elem_width("Buf[int64]"), 8);
    }

    #[test]
    fn bytes_round_trip_through_the_write_side() {
        let mut map = AttrMap::new();
        set_buf_bytes(&mut map, &[1, 2, 3]);
        let b = Value::make_instance(Symbol::intern("Buf"), map);
        assert_eq!(buf_bytes(&attrs_of(&b)), Some(vec![1, 2, 3]));

        store_buf_bytes(&attrs_of(&b), &[9]);
        assert_eq!(buf_bytes(&attrs_of(&b)), Some(vec![9]));

        let fresh = make_buf_from_bytes(Symbol::intern("Blob"), &[4, 5]);
        assert_eq!(buf_bytes(&attrs_of(&fresh)), Some(vec![4, 5]));
    }

    #[test]
    fn storage_moves_across_without_decoding() {
        let b = buf_of(bytes_to_elems(&[7, 8]));
        let storage = buf_storage(&attrs_of(&b).as_map()).expect("storage");
        let mut map = AttrMap::new();
        set_buf_storage(&mut map, storage);
        let blob = Value::make_instance(Symbol::intern("Blob"), map);
        assert_eq!(buf_elems(&attrs_of(&blob)), Some(bytes_to_elems(&[7, 8])));
    }
}
