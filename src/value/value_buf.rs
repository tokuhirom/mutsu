//! `Buf`/`Blob` element storage — the single accessor layer (ADR-0015 P2).
//!
//! A `Buf`/`Blob` is a plain `Value::Instance` whose one attribute holds its
//! elements. That storage used to be a `Value::Array` with **one boxed
//! `Value::Int` per element**: a megabyte buffer cost a million boxed `Value`s
//! and a million GC edges, the element width was recoverable only by matching on
//! the class-name string, and there was no contiguous memory to hand a C
//! function a pointer into.
//!
//! It is now a [`BufData`] node — contiguous little-endian bytes plus the
//! element type. That node is payload-only (it holds no `Value`s), so it can
//! never form a cycle and ADR-0001's container type filter pays nothing for it.
//!
//! This module is the chokepoint that made the swap possible and is what keeps
//! it contained: the attribute name is private to it, and every read, write,
//! probe and construction goes through one of the functions below, so the ~170
//! call sites across forty files did not have to know the representation
//! changed. Those contiguous bytes are what
//! [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
//! P2 hands C directly: [`buf_storage_node`] is the pointer a native call
//! marshals, and [`buf_repr_body_address`] is the `MVMArrayB` body behind an
//! honest `Buf.REPR` of `VMArray` (see [`super::value_buf_repr`]).
//!
//! Four levels are offered deliberately:
//!
//! - the **element** functions ([`buf_elems`], [`set_buf_elems`], …) are the
//!   encode/decode boundary: they hand out and take back `Vec<Value>`;
//! - the **byte** functions ([`buf_bytes`], [`with_buf_bytes`], [`buf_len`], …)
//!   speak what the node stores, so a caller that only wants bytes or a count
//!   never round-trips through boxed `Value`s;
//! - the **storage** functions ([`buf_storage`], [`set_buf_storage`]) move the
//!   node across without decoding it, for the coercions that only re-tag a
//!   buffer (`.Buf`, `.Blob`);
//! - the **native** functions ([`buf_storage_node`], [`buf_repr_body_address`])
//!   hand out the node itself and the address of its REPR body, which is what a
//!   `void*` argument and `.WHERE` are.
//!
//! **Reads need no class name; construction does.** The node carries the
//! element type, so everything that reads a buffer — including
//! [`with_buf_elems_mut`], which re-encodes at the width the buffer already
//! has — works from the node alone. Only the functions that *create* storage
//! take a `class_name`, because the name is where Raku keeps the element type
//! (`Blob[int8]`) and there is nowhere else to read it from.
//!
//! [`is_buf_or_blob_class`](crate::runtime::utils::is_buf_or_blob_class) stays
//! the companion class-name filter: this module answers "what is in there", not
//! "is this a Buf".

use super::{AttrMap, BufData, InstanceAttrs, Value, ValueRepr, ValueView};
use crate::gc::Gc;
use crate::symbol::Symbol;

/// The attribute a `Buf`/`Blob`-shaped instance keeps its storage under.
///
/// Private on purpose. If you find yourself wanting it outside this module, the
/// accessor you need is missing — add it here.
const ELEMS_ATTR: &str = "bytes";

// ---------------------------------------------------------------------------
// The node, and the encode/decode across it.
// ---------------------------------------------------------------------------

/// The element type of a `Buf`/`Blob`-shaped class: bytes per element, and
/// whether an element reads back signed.
///
/// Recovered from the class name, which is where Raku puts it (`Blob[int8]`).
/// This is the *only* place that reading stops — from here on the type travels
/// in the node, which is what lets `Blob[int8].new(-1)[0]` answer `-1`.
fn elem_type(class_name: &str) -> (u8, bool) {
    // `uint` contains `int`, so the unsigned test has to come first.
    let signed = !class_name.contains("uint") && class_name.contains("int");
    (buf_elem_width(class_name) as u8, signed)
}

/// One element as the unsigned integer its bytes spell.
///
/// **Truncating**, which is the convention Raku itself stores by: `Buf.new(300)`
/// holds `0x2C` and `Buf.new(-1)` holds `0xFF` in both mutsu and Rakudo, and
/// every mutation path (`.new`, `[i] =`, `push`, `append`, `unshift`, `splice`)
/// masks on the way in. Three different conventions used to be spelled out at
/// the call sites — this one, a `.clamp(0, 255)` one, and one going via
/// `to_int` — and they could only disagree about a buffer whose elements exceed
/// its width, which the masking makes unreachable.
fn elem_to_u64(v: &Value) -> u64 {
    match v.view() {
        ValueView::Int(i) => i as u64,
        // `to_int` saturates a `BigInt` at `i64::MAX`, which would turn a
        // legitimate `uint64` element into `0x7FFF_FFFF_FFFF_FFFF`; take the
        // full-range conversion here and let it fall through only for a value
        // that does not fit either way.
        ValueView::BigInt(n) => {
            use num_traits::ToPrimitive;
            n.as_ref()
                .to_u64()
                .unwrap_or_else(|| n.as_ref().to_i64().unwrap_or(0) as u64)
        }
        // Everything else goes through the general numeric coercion: elements
        // do not always arrive as bare `Int`s. `Blob.allocate(10, <1 2 3>)` and
        // `$buf.append(array[int].new: <7 1 3>)` hand over `IntStr` allomorphs,
        // and a `:=`-bound element arrives as a `ContainerRef`. The boxed
        // representation stored those as-is and converted lazily on read, so
        // encoding at write time has to do the same conversion or they silently
        // become zeros (`roast/S32-container/buf.t` 3/14/16).
        _ => crate::runtime::to_int(v) as u64,
    }
}

/// Elements to the contiguous little-endian bytes the node stores.
fn encode_elems(elems: &[Value], width: u8) -> Vec<u8> {
    let w = width as usize;
    let mut bytes = Vec::with_capacity(elems.len() * w);
    for v in elems {
        bytes.extend_from_slice(&elem_to_u64(v).to_le_bytes()[..w]);
    }
    bytes
}

/// The node's bytes back to elements.
///
/// A `uint64` element above `i64::MAX` becomes a `BigInt` rather than wrapping
/// negative — `buf64.new(0xFFFF_FFFF_FFFF_FFFF)[0]` is `18446744073709551615`,
/// as in Rakudo. A signed element is sign-extended from its width.
fn decode_elems(data: &BufData) -> Vec<Value> {
    let w = data.width as usize;
    data.bytes
        .chunks_exact(w)
        .map(|chunk| {
            let mut raw = [0u8; 8];
            raw[..w].copy_from_slice(chunk);
            let u = u64::from_le_bytes(raw);
            if data.signed {
                // Sign-extend from the element's own width.
                let shift = 64 - w * 8;
                Value::int(((u << shift) as i64) >> shift)
            } else if w == 8 && u > i64::MAX as u64 {
                Value::bigint(num_bigint::BigInt::from(u))
            } else {
                Value::int(u as i64)
            }
        })
        .collect()
}

/// The storage `Value` a buffer instance keeps under [`ELEMS_ATTR`].
fn storage_value(bytes: Vec<u8>, width: u8, signed: bool) -> Value {
    Value::from_repr(ValueRepr::BufStorage(Gc::new(BufData::new(
        bytes, width, signed,
    ))))
}

/// The node behind a buffer instance's storage attribute, if it has one.
fn node_in(map: &AttrMap) -> Option<super::GcRef<'_, BufData>> {
    match map.get(ELEMS_ATTR)?.view() {
        ValueView::BufStorage(data) => Some(data),
        _ => None,
    }
}

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
    let node = node_in(map)?;
    Some(decode_elems(&node))
}

/// Run `f` over the elements without copying them.
///
/// The borrow is held by an attribute read guard for the duration of the call,
/// so `f` must not re-enter the same instance's attribute cell for writing.
/// Returns `None` (without calling `f`) when there is no element storage.
pub(crate) fn with_buf_elems<R>(attrs: &InstanceAttrs, f: impl FnOnce(&[Value]) -> R) -> Option<R> {
    let elems = buf_elems(attrs)?;
    Some(f(&elems))
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
pub(crate) fn buf_attrs(class_name: Symbol, elems: Vec<Value>) -> AttrMap {
    let mut map = AttrMap::new();
    set_buf_elems(&mut map, class_name, elems);
    map
}

/// A fresh `Buf`/`Blob`-shaped instance of `class_name` holding `elems`.
pub(crate) fn make_buf(class_name: Symbol, elems: Vec<Value>) -> Value {
    Value::make_instance(class_name, buf_attrs(class_name, elems))
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
pub(crate) fn set_buf_elems(map: &mut AttrMap, class_name: Symbol, elems: Vec<Value>) {
    let (width, signed) = elem_type(&class_name.resolve());
    map.insert(
        ELEMS_ATTR,
        storage_value(encode_elems(&elems, width), width, signed),
    );
}

/// Put `bytes` into a live instance's storage, through its shared attribute
/// cell — visible to every alias, no rebind of the holding variable needed.
///
/// Writes **through** the existing node whenever this instance is its only
/// holder, rather than swapping a fresh node in. That is what makes ADR-0015 §2
/// contract 3 hold: a C structure handed `pointer-to($buf)` keeps a valid
/// pointer across an ordinary Raku-side write, and the node's REPR body block
/// stays put. (Growing past the allocation still reallocates and invalidates
/// the pointer — the same contract Rakudo's `VMArray` offers, no more.)
///
/// A **shared** node is replaced instead: `.Buf`/`.Blob` re-tag one buffer's
/// storage under another name without copying it, and Raku's copy semantics
/// mean a write to one must not be seen by the other.
fn put_bytes(attrs: &InstanceAttrs, bytes: Vec<u8>, width: u8, signed: bool) {
    {
        let map = attrs.as_map();
        if let Some(node) = node_in(&map)
            && node.strong_count() == 1
        {
            // SAFETY: audited aliased in-place container write (see
            // `value::aliased_mut`). The node is unshared, no borrow into it is
            // live across the write, and the read guard above covers only the
            // attribute map — which is not what is being mutated.
            let data = unsafe { crate::value::gc_contents_mut(&node) };
            // `clear` + `extend` rather than an assignment, so the allocation —
            // and with it the address C may be holding — survives a same-size
            // or shrinking write.
            data.bytes.clear();
            data.bytes.extend_from_slice(&bytes);
            data.width = width;
            data.signed = signed;
            return;
        }
    }
    attrs.insert(ELEMS_ATTR, storage_value(bytes, width, signed));
}

/// Mutate the elements in place through the shared cell, without decoding the
/// whole buffer first (`$b[i] = v`). `None` when there is no element storage.
pub(crate) fn with_buf_elems_mut<R>(
    attrs: &InstanceAttrs,
    f: impl FnOnce(&mut Vec<Value>) -> R,
) -> Option<R> {
    // The node knows its own element type, so unlike the construction helpers
    // this one needs no class name: decode, hand `f` the elements, re-encode at
    // the width the buffer already has.
    let map = attrs.as_map();
    let (mut elems, width, signed) = {
        let node = node_in(&map)?;
        (decode_elems(&node), node.width, node.signed)
    };
    drop(map);
    let out = f(&mut elems);
    put_bytes(attrs, encode_elems(&elems, width), width, signed);
    Some(out)
}

/// The element container itself, cloned, for the coercions that re-tag a buffer
/// without looking inside it (`.Buf`, `.Blob`). Pair with [`set_buf_storage`].
pub(crate) fn buf_storage(map: &AttrMap) -> Option<Value> {
    let stored = map.get(ELEMS_ATTR)?;
    matches!(stored.view(), ValueView::BufStorage(..)).then(|| stored.clone())
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
    Some(Value::array_with_kind(
        crate::gc::Gc::new(super::ArrayData::new(buf_elems_in(map)?)),
        kind,
    ))
}

// ---------------------------------------------------------------------------
// Native access — the buffer as C sees it.
// ---------------------------------------------------------------------------

/// The address of this buffer's synthesised `VMArray` REPR body, which is what
/// its `.WHERE` answers (ADR-0015 §2; see
/// [`ReprBody`](super::value_buf_repr::ReprBody)).
///
/// `None` for an instance with no element storage, whose `.REPR` therefore
/// stays `P6opaque` — under-reporting is safe, claiming `VMArray` without a body
/// behind it is not.
pub(crate) fn buf_repr_body_address(attrs: &InstanceAttrs) -> Option<usize> {
    let map = attrs.as_map();
    let node = node_in(&map)?;
    Some(node.body.address(&node))
}

/// This buffer's storage node, with a reference of its own.
///
/// What a native call marshals a `Blob`/`Buf` argument through: C is handed
/// `bytes.as_mut_ptr()` of the returned node — the object's *actual* storage,
/// not a copy — so a callee that writes into the buffer is writing into the
/// Raku object, and one that retains the pointer keeps seeing live memory.
/// Holding the returned `Gc` is what guarantees the latter for the duration of
/// the call.
pub(crate) fn buf_storage_node(attrs: &InstanceAttrs) -> Option<Gc<BufData>> {
    let map = attrs.as_map();
    let node = node_in(&map)?;
    Some((*node).clone())
}

// ---------------------------------------------------------------------------
// Byte-level access.
// ---------------------------------------------------------------------------

/// One element as the fixed-width hex digits a `Buf` gists with
/// (`Buf[uint16]:0x<1170>`), big-endian within the element as Rakudo prints it.
///
/// Formatting from the element's *unsigned* bit pattern is what makes a signed
/// or oversized element print the bytes it actually occupies: `Blob[int8]`
/// holding `-1` is `FF`, and a `uint64` element above `i64::MAX` — a `BigInt`
/// once decoded — is its own sixteen digits rather than zeros.
pub(crate) fn elem_hex(v: &Value, width: usize) -> String {
    let u = elem_to_u64(v);
    match width {
        8 => format!("{u:016X}"),
        4 => format!("{:08X}", u as u32),
        2 => format!("{:04X}", u as u16),
        _ => format!("{:02X}", u as u8),
    }
}

/// The number of **elements**, without decoding any of them. Not the number of
/// bytes — see [`buf_elem_width`]; `.bytes` is `elems * width`.
pub(crate) fn buf_len(attrs: &InstanceAttrs) -> Option<usize> {
    buf_len_in(&attrs.as_map())
}

/// [`buf_len`] against an attribute map already in hand. A division, not a
/// decode: the node's byte count divided by its element width.
pub(crate) fn buf_len_in(map: &AttrMap) -> Option<usize> {
    let node = node_in(map)?;
    Some(node.bytes.len() / node.width as usize)
}

/// [`buf_len`] with an absent buffer read as empty.
pub(crate) fn buf_len_or_zero(attrs: &InstanceAttrs) -> usize {
    buf_len(attrs).unwrap_or(0)
}

/// The elements as one byte each — the buffer's real bytes for a width-1
/// buffer (every `Buf`/`Blob`/`utf8`), read straight off the node.
///
/// `None` when the instance carries no element storage, exactly as
/// [`buf_elems`]. For a wider buffer this is the low byte of each element,
/// which is what the byte-shaped callers have always taken.
pub(crate) fn buf_bytes(attrs: &InstanceAttrs) -> Option<Vec<u8>> {
    buf_bytes_in(&attrs.as_map())
}

/// [`buf_bytes`] with an absent buffer read as empty.
pub(crate) fn buf_bytes_or_empty(attrs: &InstanceAttrs) -> Vec<u8> {
    buf_bytes(attrs).unwrap_or_default()
}

/// [`buf_bytes`] against an attribute map already in hand.
pub(crate) fn buf_bytes_in(map: &AttrMap) -> Option<Vec<u8>> {
    let node = node_in(map)?;
    Some(node_bytes(&node).into_owned())
}

/// The byte view of a node: its storage as-is for a width-1 buffer (every
/// `Buf`/`Blob`/`utf8`, so the common case borrows and copies nothing extra),
/// and the low byte of each element for a wider one.
fn node_bytes<'a>(node: &'a BufData) -> std::borrow::Cow<'a, [u8]> {
    if node.width == 1 {
        std::borrow::Cow::Borrowed(&node.bytes)
    } else {
        std::borrow::Cow::Owned(
            node.bytes
                .chunks_exact(node.width as usize)
                .map(|c| c[0])
                .collect(),
        )
    }
}

/// Run `f` over the bytes without handing out an owned `Vec`.
///
/// For a width-1 buffer this is the node's storage, borrowed — no decode and no
/// allocation. Callers that go on to mutate the bytes want [`buf_bytes`].
pub(crate) fn with_buf_bytes<R>(attrs: &InstanceAttrs, f: impl FnOnce(&[u8]) -> R) -> Option<R> {
    let map = attrs.as_map();
    let node = node_in(&map)?;
    Some(f(&node_bytes(&node)))
}

/// Store raw bytes into a map being built or updated, one byte per element.
pub(crate) fn set_buf_bytes(map: &mut AttrMap, class_name: Symbol, bytes: &[u8]) {
    set_buf_elems(map, class_name, bytes_to_elems(bytes));
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

/// The element type name a `Buf`/`Blob`-shaped class answers from `.of`:
/// the bracket parameter when spelled (`Blob[int8]` → `int8`), else the
/// width/signedness the short name encodes (`buf16` → `uint16`), defaulting
/// to `uint8` — matching Rakudo (`Buf.of` is `(uint8)`).
pub(crate) fn buf_elem_type_name(class_name: &str) -> String {
    if let Some(inner) = class_name
        .split_once('[')
        .and_then(|(_, rest)| rest.strip_suffix(']'))
    {
        return inner.to_string();
    }
    let (width, signed) = elem_type(class_name);
    let bits = width as usize * 8;
    if signed {
        format!("int{bits}")
    } else {
        format!("uint{bits}")
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
        assert_eq!(elem_to_u64(&Value::int(300)) as u8, 0x2C);
        assert_eq!(elem_to_u64(&Value::int(-1)) as u8, 0xFF);
        assert_eq!(elem_to_u64(&Value::int(0x1170)) as u8, 0x70);
        assert_eq!(elem_to_u64(&Value::num(300.9)) as u8, 0x2C);
        assert_eq!(elem_to_u64(&Value::str("nope".to_string())), 0);

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
        let buf = Symbol::intern("Buf");
        let mut map = AttrMap::new();
        set_buf_bytes(&mut map, buf, &[1, 2, 3]);
        let b = Value::make_instance(buf, map);
        assert_eq!(buf_bytes(&attrs_of(&b)), Some(vec![1, 2, 3]));

        with_buf_elems_mut(&attrs_of(&b), |items| *items = bytes_to_elems(&[9]));
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

    /// The node stores contiguous bytes at the class's element width, so a
    /// wide buffer's storage is `elems * width` bytes — not one byte per
    /// element, which is all the old boxed representation could express.
    #[test]
    fn the_node_stores_contiguous_bytes_at_the_element_width() {
        let wide = make_buf(Symbol::intern("Buf[uint16]"), vec![Value::int(0x1170)]);
        let attrs = attrs_of(&wide);
        let map = attrs.as_map();
        let node = node_in(&map).expect("node");
        assert_eq!(node.bytes, vec![0x70, 0x11]); // little-endian
        assert_eq!(node.width, 2);
        assert!(!node.signed);
    }

    /// The element type used to live only in the class-name string, so every
    /// element read back unsigned. It is data now.
    #[test]
    fn signed_elements_read_back_signed() {
        let signed = make_buf(Symbol::intern("Blob[int8]"), vec![Value::int(-1)]);
        assert_eq!(buf_elems(&attrs_of(&signed)), Some(vec![Value::int(-1)]));
        // Same bytes, unsigned type: a different value.
        let unsigned = make_buf(Symbol::intern("Blob[uint8]"), vec![Value::int(-1)]);
        assert_eq!(buf_elems(&attrs_of(&unsigned)), Some(vec![Value::int(255)]));

        let wide = make_buf(Symbol::intern("Buf[int16]"), vec![Value::int(-2)]);
        assert_eq!(buf_elems(&attrs_of(&wide)), Some(vec![Value::int(-2)]));
    }

    /// `uint64` is the one width whose range does not fit `i64`; the old
    /// representation wrapped it negative on the way out.
    #[test]
    fn oversized_unsigned_elements_decode_to_bigint() {
        let b = make_buf(Symbol::intern("Buf[uint64]"), vec![Value::int(-1)]);
        let elems = buf_elems(&attrs_of(&b)).expect("elems");
        assert_eq!(elems[0].to_string_value(), "18446744073709551615");
        assert_eq!(elem_hex(&elems[0], 8), "FFFFFFFFFFFFFFFF");
    }

    /// Elements do not always arrive as bare `Int`s — `Blob.allocate(10, <1 2 3>)`
    /// hands over `IntStr` allomorphs. The boxed representation stored them
    /// as-is and converted on read; encoding at write time has to convert too.
    #[test]
    fn non_int_elements_are_coerced_not_zeroed() {
        let b = buf_of(vec![Value::str("7".to_string()), Value::num(13.9)]);
        assert_eq!(buf_bytes(&attrs_of(&b)), Some(vec![7, 13]));
    }

    #[test]
    fn element_type_reads_signedness_off_the_name() {
        for name in ["Buf", "Blob", "utf8", "utf16", "Buf[uint8]", "Blob[uint64]"] {
            assert!(!elem_type(name).1, "{name} should be unsigned");
        }
        for name in ["Blob[int8]", "Buf[int16]", "Buf[int64]"] {
            assert!(elem_type(name).1, "{name} should be signed");
        }
    }

    /// An in-place element mutation re-encodes at the buffer's own width — it
    /// takes no class name, because the node already knows.
    #[test]
    fn in_place_mutation_keeps_the_element_width() {
        let wide = make_buf(Symbol::intern("Buf[uint16]"), vec![Value::int(1)]);
        let attrs = attrs_of(&wide);
        with_buf_elems_mut(&attrs, |items| items.push(Value::int(0x1234)));
        assert_eq!(
            buf_elems(&attrs),
            Some(vec![Value::int(1), Value::int(0x1234)])
        );
        let map = attrs.as_map();
        assert_eq!(node_in(&map).expect("node").bytes, vec![1, 0, 0x34, 0x12]);
    }
}
