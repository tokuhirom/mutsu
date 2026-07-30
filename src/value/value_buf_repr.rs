//! The synthesised REPR bodies of the storage-backed containers: `VMArray` for
//! a `Buf`/`Blob` (ADR-0015 P2) and `CArray` for a native-backed `CArray[T]`
//! (P3).
//!
//! `NativeHelpers::Blob`'s `pointer-to` — the thing `DBDish::mysql` needs to
//! hand a C library an out-buffer — does not ask mutsu for an address directly.
//! It asks MoarVM's guts:
//!
//! ```raku
//! sub BODY_OF(Mu \any) {
//!     my \type = %known-bodies{any.REPR};      # VMArray / CArray / CStruct
//!     nativecast(Pointer[type], Pointer.new(any.WHERE + Offset)).deref;
//! }
//! ```
//!
//! So a `Buf` whose `.REPR` answers `VMArray` is promising that a MoarVM
//! `MVMArrayB` — `{u64 elems; u64 start; u64 ssize; void* any}` — is readable at
//! its `.WHERE`. mutsu has no object header, so `Offset` probes as 0 and the
//! body sits at `.WHERE` itself; `.realstart` then reads `any` (with `start`
//! always 0) and that is the pointer C is given.
//!
//! mutsu does not *store* a buffer this way — its storage is a
//! [`BufData`](super::BufData) node — so the block is **synthesised**: a
//! documented compatibility surface, described in
//! [docs/nativecall-repr-bodies.md](../../docs/nativecall-repr-bodies.md), not
//! an internal detail anyone else reads.
//!
//! Three properties matter, and they are why this cannot reuse
//! `nativecall::native_object_where` (which memoises one immutable, permanently
//! leaked zero block per payload address — enough for the CStruct and CArray
//! bodies, whose words past the first are all zero, and not enough here):
//!
//! - **per node**, because three of the four words are live and differ per
//!   buffer;
//! - **mutable**, because ADR-0015 §2 contract 3 promises the block stays put
//!   while its data pointer is rewritten on a reallocation;
//! - **owned**, because it must die with the buffer rather than leak.

use std::sync::Mutex;

use super::BufData;

/// MoarVM's `VMArray` REPR body, laid out exactly as `MoarVM::Guts::REPRs`
/// declares it.
///
/// `any` is a `void*` held as a `usize` so the block stays `Send`/`Sync`; the
/// two have identical size and alignment on every target mutsu builds for, and
/// C never sees the Rust type — only the bytes.
#[repr(C)]
#[derive(Default, Debug)]
struct MVMArrayB {
    elems: u64,
    start: u64,
    ssize: u64,
    any: usize,
}

/// MoarVM's `CArray` REPR body, laid out exactly as `MoarVM::Guts::REPRs`
/// declares it.
///
/// `managed` is what tells `NativeHelpers::Blob` whether the array owns its
/// memory and therefore knows its own length: it is 1 for a `CArray[T]` built
/// in Raku — which is what having a node at all means — and 0 for a
/// `nativecast`ed handle, whose body comes from the zero block
/// `nativecall::native_object_where` hands out instead.
///
/// `child` stays NULL: it is MoarVM's table of the *object* elements of a
/// `CArray[Str]`/`CArray[CStruct]`, and those element types keep the boxed
/// representation (see `value_buf::native_elem_type`), so a node-backed array
/// never has any.
#[repr(C)]
#[derive(Default, Debug)]
struct CArrayB {
    storage: usize,
    child: usize,
    managed: i32,
    allocated: i32,
    elems: i32,
}

/// The block a node hands out, in the layout its *container* asks for.
///
/// A node belongs to exactly one container, so which layout it is asked for
/// never changes over its lifetime; the enum exists because `Buf` and `CArray`
/// share the node and MoarVM gives them different bodies.
#[derive(Debug)]
enum Block {
    VMArray(Box<MVMArrayB>),
    CArray(Box<CArrayB>),
}

/// A buffer's REPR body block: absent until something asks for the buffer's
/// `.WHERE`, then allocated once and refreshed in place.
///
/// Not part of a buffer's *value*, which is why the three trait impls below are
/// hand-written rather than derived: a cloned node has its own byte allocation
/// and so must start with no block of its own, and two buffers holding the same
/// bytes are equal whether or not either has ever been handed to C.
#[derive(Default)]
pub(crate) struct ReprBody(Mutex<Option<Block>>);

impl ReprBody {
    /// The address of `node`'s `VMArray` body, refreshed from the node's current
    /// storage — a `Buf`/`Blob`'s `.WHERE`.
    ///
    /// Stable for the node's lifetime: the block is boxed once and only its
    /// contents change afterwards, so a C structure that captured the address
    /// keeps reading the live element pointer even after the buffer has been
    /// reallocated underneath it.
    pub(crate) fn address(&self, node: &BufData) -> usize {
        let width = node.width.max(1) as usize;
        let mut slot = self.0.lock().unwrap_or_else(|e| e.into_inner());
        let block = match slot.get_or_insert_with(|| Block::VMArray(Box::default())) {
            Block::VMArray(b) => b,
            // Cannot happen: the container decides the layout and never changes
            // its mind. Re-tagging rather than panicking keeps a future caller
            // that gets it wrong reading a well-formed block of the layout it
            // asked for.
            other => {
                *other = Block::VMArray(Box::default());
                match other {
                    Block::VMArray(b) => b,
                    _ => unreachable!(),
                }
            }
        };
        block.elems = (node.bytes.len() / width) as u64;
        // Always 0: mutsu's storage never has an unused prefix, so
        // `realstart == any` and the module's `+$!start` branch is dead.
        block.start = 0;
        block.ssize = (node.bytes.capacity() / width) as u64;
        block.any = node.bytes.as_ptr() as usize;
        (&raw const **block) as usize
    }

    /// The address of `node`'s `CArray` body — a native-backed `CArray[T]`'s
    /// `.WHERE`. Same allocate-once-refresh-in-place contract as
    /// [`Self::address`].
    pub(crate) fn carray_address(&self, node: &BufData) -> usize {
        let width = node.width.max(1) as usize;
        let mut slot = self.0.lock().unwrap_or_else(|e| e.into_inner());
        let block = match slot.get_or_insert_with(|| Block::CArray(Box::default())) {
            Block::CArray(b) => b,
            other => {
                *other = Block::CArray(Box::default());
                match other {
                    Block::CArray(b) => b,
                    _ => unreachable!(),
                }
            }
        };
        block.storage = node.bytes.as_ptr() as usize;
        block.child = 0;
        // A node exists only for an array mutsu allocated, which is exactly
        // what `managed` means.
        block.managed = 1;
        block.allocated = (node.bytes.capacity() / width) as i32;
        block.elems = (node.bytes.len() / width) as i32;
        (&raw const **block) as usize
    }
}

impl Clone for ReprBody {
    /// A cloned node owns a fresh byte allocation, so it must not inherit a
    /// block describing the original's.
    fn clone(&self) -> ReprBody {
        ReprBody::default()
    }
}

impl PartialEq for ReprBody {
    /// Buffers compare by their bytes; whether either has ever been handed to C
    /// is not part of the comparison.
    fn eq(&self, _other: &ReprBody) -> bool {
        true
    }
}

impl std::fmt::Debug for ReprBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let present = self.0.lock().map(|s| s.is_some()).unwrap_or(false);
        f.debug_tuple("ReprBody").field(&present).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The layout mutsu commits to. If this ever fails, every module reading a
    /// body through `MoarVM::Guts::REPRs` reads garbage.
    #[test]
    fn the_body_has_moarvms_layout() {
        assert_eq!(std::mem::size_of::<MVMArrayB>(), 32);
        assert_eq!(std::mem::align_of::<MVMArrayB>(), 8);
        // `{void*; void**; i32; i32; i32}` — two pointers then three `int32`s,
        // the last of which is tail-padded to the pointer alignment.
        assert_eq!(std::mem::size_of::<CArrayB>(), 32);
        assert_eq!(std::mem::align_of::<CArrayB>(), 8);
    }

    /// A native-backed `CArray[T]` is `managed`, knows its own length, and
    /// points at its own storage — the three words `NativeHelpers::Blob` reads.
    #[test]
    fn the_carray_block_describes_the_nodes_storage() {
        let node = BufData::new(vec![1, 2, 3, 4], 2, crate::value::ElemKind::Uint);
        let addr = node.body.carray_address(&node);
        // SAFETY: the address is of a live `Box` owned by `node`.
        let block = unsafe { &*(addr as *const CArrayB) };
        assert_eq!(block.storage, node.bytes.as_ptr() as usize);
        assert_eq!(block.managed, 1);
        assert_eq!(block.elems, 2, "four bytes at width two");
        assert_eq!(block.child, 0);
        assert_eq!(node.body.carray_address(&node), addr, "the block stays put");
    }

    #[test]
    fn the_block_describes_the_nodes_storage() {
        let node = BufData::new(vec![1, 2, 3, 4], 2, crate::value::ElemKind::Uint);
        let addr = node.body.address(&node);
        // SAFETY: the address is of a live `Box` owned by `node`.
        let block = unsafe { &*(addr as *const MVMArrayB) };
        assert_eq!(block.elems, 2, "four bytes at width two");
        assert_eq!(block.start, 0);
        assert_eq!(block.any, node.bytes.as_ptr() as usize);
    }

    /// The address is what a C structure captures, so it must not move when the
    /// buffer is asked again — or, later, when its storage is reallocated.
    #[test]
    fn the_block_address_is_stable_and_refreshed() {
        let mut node = BufData::new(vec![7], 1, crate::value::ElemKind::Uint);
        let first = node.body.address(&node);
        assert_eq!(node.body.address(&node), first);

        node.bytes = vec![7, 8, 9];
        let again = node.body.address(&node);
        assert_eq!(again, first, "the block itself must stay put");
        // SAFETY: as above.
        let block = unsafe { &*(again as *const MVMArrayB) };
        assert_eq!(block.elems, 3);
        assert_eq!(block.any, node.bytes.as_ptr() as usize);
    }

    /// A clone is a distinct buffer with its own bytes, so it starts with no
    /// block — and still compares equal.
    #[test]
    fn a_clone_starts_without_a_block_and_still_compares_equal() {
        let node = BufData::new(vec![1], 1, crate::value::ElemKind::Uint);
        node.body.address(&node);
        let copy = node.clone();
        assert!(copy.body.0.lock().unwrap().is_none());
        assert_eq!(node, copy);
        assert_ne!(node.body.address(&node), copy.body.address(&copy));
    }
}
