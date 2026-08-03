//! Miri-checked probes for the handle operations call sites perform **while a
//! `gc_contents_mut` `&mut` is live**.
//!
//! # Why this module exists
//!
//! [`crate::gc::gc_contents_mut`] takes `&Gc<T>` and returns `&mut T`, so the
//! borrow checker offers **no** protection at its ~62 call sites: keeping a
//! Deref'd `&T` alive across the write compiles fine and is the one failure mode
//! Miri actually catches (ADR-0013 §8).
//!
//! Auditing those sites by hand turns almost entirely on one question: which
//! operations touch the *payload* and which touch only the `GcBox` header? Real
//! call sites do all of the following with the `&mut` still live — e.g.
//! `fixup_circular_array_refs` holds `data: &mut ArrayData` across
//! `result_arc.clone()` and across passing `&result_arc` to a recursive helper:
//!
//! - `Gc::clone` (build a self-reference into the node being written)
//! - `Gc::as_ptr` (identity comparison against another node)
//! - `Gc::strong_count` / `Gc::ptr_eq` (the aliased-vs-unique routing decision)
//!
//! Reasoning says none of them dereference the payload — `clone` bumps
//! `header.strong`, `as_ptr` projects through the `UnsafeCell` with
//! `raw_get`, and the counts live in the header. But "reasoning says" is exactly
//! what the ADR-0013 over-promise was, so pin it instead: these probes fail
//! under Miri the moment one of those operations starts going through `Deref`.
//! That is what lets the audit clear a whole family of call sites mechanically
//! rather than one careful read at a time.
//!
//! A `Deref` **is** a payload access, and holding one across the write is UB —
//! that shape is deliberately absent here, because a test that triggers UB fails
//! the gate rather than documenting it. It is recorded in ADR-0013 §8's
//! measurement table instead.

#[cfg(test)]
mod tests {
    use crate::gc::{ErasedGc, Gc, Trace, gc_contents_mut};

    /// Payload with an edge, so a probe can store a handle to the very node it
    /// is writing (the self-reference shape `fixup_circular_array_refs` builds).
    #[derive(Clone)]
    struct Node {
        value: i64,
        edge: Option<Gc<Node>>,
    }

    impl Trace for Node {
        fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
            if let Some(e) = &self.edge {
                visit(&e.erased());
            }
        }
    }

    fn node(value: i64) -> Gc<Node> {
        Gc::new(Node { value, edge: None })
    }

    /// Drop a probe's self-edge before the handle goes out of scope. A
    /// self-referential node is exactly what the cycle collector exists to
    /// reclaim, so leaving one alive would make these tests leak — and this
    /// module runs in the Miri step that keeps the leak check ON (the
    /// interpreter-level step is the one that must disable it). Severing the
    /// edge here keeps that check meaningful for the primitive.
    fn break_cycle(gc: &Gc<Node>) {
        // SAFETY: no borrow into the payload is live across this write.
        unsafe { gc_contents_mut(gc) }.edge = None;
    }

    /// `Gc::clone` while the aliased `&mut` is live. This is the self-reference
    /// build: the handle produced by the clone is stored *into* the node the
    /// `&mut` points at.
    #[test]
    fn cloning_the_handle_while_the_aliased_mut_is_live() {
        let gc = node(1);
        // SAFETY: the probe's whole point is that nothing below dereferences the
        // payload through another borrow while this `&mut` is live.
        let data = unsafe { gc_contents_mut(&gc) };
        data.edge = Some(gc.clone());
        data.value = 7;
        assert_eq!(gc.value, 7);
        assert_eq!(
            gc.edge.as_ref().unwrap().value,
            7,
            "the edge is the node itself"
        );
        break_cycle(&gc);
    }

    /// `Gc::as_ptr` while the aliased `&mut` is live — the identity comparison
    /// the circular-reference fixups run against every element.
    #[test]
    fn taking_as_ptr_while_the_aliased_mut_is_live() {
        let gc = node(1);
        let other = node(2);
        // SAFETY: as above.
        let data = unsafe { gc_contents_mut(&gc) };
        let same = Gc::as_ptr(&gc) as usize;
        let different = Gc::as_ptr(&other) as usize;
        assert_ne!(same, different);
        data.value = 7;
        assert_eq!(gc.value, 7);
    }

    /// `strong_count` / `ptr_eq` while the aliased `&mut` is live — the routing
    /// decision `gc_data_mut` and its inlined copies make.
    #[test]
    fn reading_the_counts_while_the_aliased_mut_is_live() {
        let gc = node(1);
        let alias = gc.clone();
        // SAFETY: as above.
        let data = unsafe { gc_contents_mut(&gc) };
        assert_eq!(Gc::strong_count(&gc), 2);
        assert!(Gc::ptr_eq(&gc, &alias));
        data.value = 7;
        assert_eq!(
            alias.value, 7,
            "the shared write is visible through the alias"
        );
    }

    /// Payload with a byte buffer, for the FFI-marshalling shape below.
    #[derive(Clone)]
    struct BufNode {
        value: i64,
        bytes: Vec<u8>,
    }
    impl Trace for BufNode {
        fn trace(&self, _visit: &mut dyn FnMut(&ErasedGc)) {}
    }

    /// The one structurally different call-site family: `nativecall`'s
    /// `marshal_arg` (`CType::Buf`) derives a raw pointer *from* the `&mut`,
    /// hands it to C, and retains the node next to it — so the derived tag has
    /// to survive an arbitrary window in which the Raku object may be read.
    ///
    /// This is the shape that makes the derivation ORDER load-bearing. Measured
    /// on the pinned toolchain: taking a `&T` **before** the write and using it
    /// **after** is UB under both Stacked and Tree Borrows, but deriving a raw
    /// pointer first and reading through `Deref` afterwards is fine — the read
    /// pushes above the pointer's tag rather than popping it. Reverse the two
    /// and this becomes the UB case.
    ///
    /// The Miri job runs `--no-default-features --features native` to drop FFI,
    /// so the real `nativecall` path can never be checked directly. This probe
    /// is the stand-in for it; do not delete it as "not a real call site".
    #[test]
    fn nativecall_shape_raw_pointer_survives_a_later_deref() {
        let gc = Gc::new(BufNode {
            value: 1,
            bytes: vec![0u8; 4],
        });
        // `marshal_arg`'s CType::Buf arm: derive a raw pointer FROM the `&mut`
        // and hand it to C, retaining the node next to it.
        let ptr = unsafe { gc_contents_mut(&gc) }.bytes.as_mut_ptr();
        // Anything that reads the Raku object during the FFI call Derefs the
        // node -- a `&T` over the payload.
        let observed = gc.value;
        assert_eq!(observed, 1);
        // C writes through the retained pointer.
        unsafe { *ptr = 42 };
        assert_eq!(gc.bytes[0], 42);
    }

    /// Whole-struct overwrite (`*gc_contents_mut(&x) = new`) where the
    /// replacement carries a handle to the very node being overwritten — the
    /// `quanthash_store_preserving_identity` / `array_inplace_reassign` shape.
    /// The old contents are dropped while a live handle to the node exists, so
    /// this also checks that dropping the outgoing edge does not free the node
    /// out from under the write.
    #[test]
    fn overwriting_the_whole_payload_with_a_self_referential_value() {
        let gc = node(1);
        // Seed an outgoing edge so the overwrite has something to drop.
        unsafe { gc_contents_mut(&gc) }.edge = Some(node(2));
        let replacement = Node {
            value: 7,
            edge: Some(gc.clone()),
        };
        // SAFETY: no borrow into the payload is live across the write.
        unsafe { *gc_contents_mut(&gc) = replacement };
        assert_eq!(gc.value, 7);
        assert_eq!(gc.edge.as_ref().unwrap().value, 7);
        break_cycle(&gc);
    }
}
