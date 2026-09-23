//! In-place edits of a buffer's storage node — the O(k) mutation layer.
//!
//! [`with_buf_elems_mut`](super::with_buf_elems_mut) is the general-purpose
//! element editor, but it decodes the *whole* buffer into boxed `Value`s and
//! re-encodes all of it afterwards, so every call is O(e) in the buffer's
//! length. Binary-protocol code (`CBOR::Simple`, written almost entirely in
//! `nqp::writeuint` / `nqp::bindpos_i` / `nqp::splice`) fills a buffer one
//! small write at a time, which made that fill O(n²) (#9132).
//!
//! The functions here edit the node's bytes directly: they touch only the bytes
//! a write covers (plus an amortized grow), exactly like MoarVM's `VMArray`.
//! Like [`put_bytes`] they write **through** an unshared node
//! (keeping any address C holds valid) and fork a shared one (`.Buf`/`.Blob`
//! re-tag one node under two names, and Raku's copy semantics mean a write to
//! one must not show through the other).

use super::{BufData, ELEMS_ATTR, InstanceAttrs, Value, decode_elem_bits, elem_bits, node_in};
use super::{bytes_to_elems, decode_elems, encode_elems, put_bytes, storage_value};

/// Run `f` over the node's raw storage (`elems * width` little-endian bytes)
/// and its element width, in place. `None` (without calling `f`) when the
/// instance carries no element storage.
///
/// `f` must leave the storage a whole number of elements long, and must not
/// re-enter the interpreter: the attribute read guard is held across it.
pub(crate) fn with_buf_storage_mut<R>(
    attrs: &InstanceAttrs,
    f: impl FnOnce(&mut Vec<u8>, usize) -> R,
) -> Option<R> {
    let (mut bytes, w, kind) = {
        let map = attrs.as_map();
        let node = node_in(&map)?;
        if node.strong_count() == 1 {
            // SAFETY: audited aliased in-place container write (see
            // `value::aliased_mut`), the one `put_bytes` performs. The node is
            // unshared, `f` is a pure byte edit that never re-enters the
            // interpreter, and the read guard covers only the attribute map —
            // which is not what is being mutated.
            let data: &mut BufData = unsafe { crate::value::gc_contents_mut(&node) };
            let width = data.width as usize;
            return Some(f(&mut data.bytes, width));
        }
        // A shared node is forked: copy, edit the copy, install it.
        (node.bytes.clone(), node.width, node.kind)
    };
    let out = f(&mut bytes, w as usize);
    attrs.insert(ELEMS_ATTR, storage_value(bytes, w, kind));
    Some(out)
}

/// Run `f` over the buffer's **byte view** — one byte per element, the shape
/// the `nqp::` byte ops (`writeuint`, `splice`, `readfh`, ...) address — and
/// store what it leaves behind as the new elements.
///
/// For a width-1 buffer (every `Buf`/`Blob`/`utf8`/`buf8`) that view *is* the
/// storage, so the edit happens in place and costs only what `f` touches. A
/// wider buffer's view is the low byte of each element, which has no storage of
/// its own: that case projects, edits and re-encodes the whole buffer, O(e).
// TODO: a wide buffer's byte ops should address its raw storage, as MoarVM's
// do (`nqp::writeuint` on a `buf32` writes bytes, not low-byte elements);
// until then this keeps the long-standing low-byte projection for them (#9191).
pub(crate) fn with_buf_bytes_mut<R>(
    attrs: &InstanceAttrs,
    f: impl FnOnce(&mut Vec<u8>) -> R,
) -> Option<R> {
    let wide = {
        let map = attrs.as_map();
        let node = node_in(&map)?;
        (node.width != 1).then(|| (decode_elems(&node), node.width, node.kind))
    };
    let Some((elems, width, kind)) = wide else {
        return with_buf_storage_mut(attrs, |bytes, _| f(bytes));
    };
    let mut bytes: Vec<u8> = elems
        .iter()
        .map(|e| crate::runtime::to_int(e) as u8)
        .collect();
    let out = f(&mut bytes);
    put_bytes(
        attrs,
        encode_elems(&bytes_to_elems(&bytes), width, kind),
        width,
        kind,
    );
    Some(out)
}

/// Store `val` as element `idx`, growing the buffer with zero elements when
/// `idx` is past the end (`nqp::bindpos_i`). Encodes one element at the
/// buffer's own width. `None` when there is no element storage.
pub(crate) fn set_buf_elem(attrs: &InstanceAttrs, idx: usize, val: &Value) -> Option<()> {
    // Encode outside the guard: the numeric coercion of an allomorph or a
    // container element may call back into the interpreter.
    let (width, kind) = {
        let map = attrs.as_map();
        let node = node_in(&map)?;
        (node.width, node.kind)
    };
    let bits = elem_bits(val, width, kind).to_le_bytes();
    with_buf_storage_mut(attrs, |bytes, w| {
        let start = idx * w;
        if bytes.len() < start + w {
            bytes.resize(start + w, 0);
        }
        bytes[start..start + w].copy_from_slice(&bits[..w]);
    })
}

/// Remove and return the last element (`nqp::pop` on a buffer). The outer
/// `None` is "no element storage", the inner one "empty buffer".
pub(crate) fn pop_buf_elem(attrs: &InstanceAttrs) -> Option<Option<Value>> {
    let kind = {
        let map = attrs.as_map();
        node_in(&map)?.kind
    };
    with_buf_storage_mut(attrs, |bytes, w| {
        let start = bytes.len().checked_sub(w)?;
        let v = elem_from(&bytes[start..], w, kind);
        bytes.truncate(start);
        Some(v)
    })
}

/// Remove and return the first element (`nqp::shift` on a buffer). The outer
/// `None` is "no element storage", the inner one "empty buffer".
// TODO: this shifts the remaining bytes down, O(e) per call; MoarVM keeps a
// start offset in the VMArray body and is O(1) (#9191).
pub(crate) fn shift_buf_elem(attrs: &InstanceAttrs) -> Option<Option<Value>> {
    let kind = {
        let map = attrs.as_map();
        node_in(&map)?.kind
    };
    with_buf_storage_mut(attrs, |bytes, w| {
        if bytes.len() < w {
            return None;
        }
        let v = elem_from(&bytes[..w], w, kind);
        bytes.drain(..w);
        Some(v)
    })
}

/// The element whose bytes begin `chunk`.
fn elem_from(chunk: &[u8], w: usize, kind: crate::value::ElemKind) -> Value {
    let mut raw = [0u8; 8];
    raw[..w].copy_from_slice(&chunk[..w]);
    decode_elem_bits(u64::from_le_bytes(raw), w as u8, kind)
}

/// The class name and attribute cell of a `Buf`/`Blob`-shaped value with
/// element storage — the targets the in-place editors above apply to. `None`
/// for anything else (a list, an `IterationBuffer`, a type object, or a user
/// class that merely has an attribute of the same name).
pub(crate) fn buf_target(
    v: &Value,
) -> Option<(crate::symbol::Symbol, crate::gc::Gc<InstanceAttrs>)> {
    match v.view() {
        crate::value::ValueView::Instance {
            class_name,
            attributes,
            ..
        } if node_in(&attributes.as_map()).is_some() => Some((class_name, (*attributes).clone())),
        _ => None,
    }
}
