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
//! Like [`put_bytes`](super::put_bytes) they write **through** an unshared node
//! (keeping any address C holds valid) and fork a shared one (`.Buf`/`.Blob`
//! re-tag one node under two names, and Raku's copy semantics mean a write to
//! one must not show through the other).

use super::storage_value;
use super::{
    BufBytes, BufData, ELEMS_ATTR, ElemKind, InstanceAttrs, Value, decode_elem_bits, elem_bits,
    node_in,
};

/// Run `f` over the node's raw storage (`elems * width` little-endian bytes)
/// and its element width, in place. `None` (without calling `f`) when the
/// instance carries no element storage.
///
/// `f` must leave the storage a whole number of elements long, and must not
/// re-enter the interpreter: the attribute read guard is held across it.
pub(crate) fn with_buf_storage_mut<R>(
    attrs: &InstanceAttrs,
    f: impl FnOnce(&mut BufBytes, usize) -> R,
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
    attrs.insert(ELEMS_ATTR, storage_value(bytes.into_vec(), w, kind));
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
// Cost: O(1) amortized — `BufBytes` advances its head offset rather than
// moving the remaining bytes, as MoarVM's `VMArray` advances `start`.
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
        bytes.drop_front(w);
        Some(v)
    })
}

/// The buffer's elements as storage bytes of element type `width`/`kind`: its
/// raw storage when it already has that type, otherwise each element
/// re-encoded at `width` (truncated, as MoarVM's `splice` truncates a `buf16`
/// element spliced into a `buf8`). `None` when there is no element storage.
// Cost: O(e), e = elements (one copy).
pub(crate) fn buf_storage_as(attrs: &InstanceAttrs, width: u8, kind: ElemKind) -> Option<Vec<u8>> {
    let map = attrs.as_map();
    let node = node_in(&map)?;
    if node.width == width && node.kind == kind {
        return Some(node.bytes.to_vec());
    }
    let w = width as usize;
    let mut out = Vec::with_capacity(node.elems() * w);
    for chunk in node.bytes.chunks_exact(node.width as usize) {
        let v = elem_from(chunk, node.width as usize, node.kind);
        out.extend_from_slice(&elem_bits(&v, width, kind).to_le_bytes()[..w]);
    }
    Some(out)
}

/// The element whose bytes begin `chunk`.
fn elem_from(chunk: &[u8], w: usize, kind: ElemKind) -> Value {
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
