//! Miri-checked probes for `ArrayData`'s native `array[T]` decode cache
//! (docs/adr/0030), modeled directly on [`crate::gc::borrow_shapes`], which
//! exists for the same reason: the borrow checker offers no protection at a
//! `SyncUnsafeCell` write site, so pin the shapes instead of reasoning about
//! them.
//!
//! # Why a `Gc<BufData>` write stands in for a real native write
//!
//! The Miri job runs `--no-default-features --features native`, which drops
//! FFI — so the real `nativecall`/C-side write path can never be checked
//! directly. These probes simulate it with a direct write to the node's
//! `bytes` through [`crate::gc::gc_contents_mut`], exactly as
//! `borrow_shapes::nativecall_shape_raw_pointer_survives_a_later_deref`
//! stands in for the real `nativecall` path.

#[cfg(test)]
mod tests {
    use crate::value::{ArrayData, Value};

    /// A fresh `int64`-backed native array over `[10, 20, 30]`, promoted so
    /// `data.native` is `Some`.
    fn native_array() -> ArrayData {
        let mut data = ArrayData::new(vec![Value::int(10), Value::int(20), Value::int(30)]);
        data.promote_native_storage("int64");
        assert!(
            data.native_storage_node().is_some(),
            "int64 is a supported native element type"
        );
        data
    }

    /// Overwrite element `i` of `data`'s native node directly, simulating an
    /// external (C-side) write mutsu never observes through `items_mut`.
    fn simulate_native_write(data: &ArrayData, i: usize, value: i64) {
        let node = data.native_storage_node().unwrap();
        // SAFETY: single-threaded probe; no other borrow into the node's
        // payload is live across this write (mirrors the real FFI write this
        // stands in for — see the module docs).
        unsafe {
            let buf = crate::gc::gc_contents_mut(&node);
            let width = buf.width as usize;
            let start = i * width;
            buf.bytes[start..start + width].copy_from_slice(&value.to_le_bytes()[..width]);
        }
    }

    /// The core shape: a `&Vec<Value>` obtained from `items()`, then a
    /// native-side byte change, then a second `items()` that re-syncs, then
    /// **using the first reference**. This is exactly the aliasing shape
    /// that was UB before docs/adr/0030 (a write through a pointer derived
    /// from the very `&self` the first reference still borrows from) — it
    /// must be clean now, and is the test that would have caught the bug.
    #[test]
    fn first_reference_survives_a_later_resync() {
        let data = native_array();
        let first = data.items();
        assert_eq!(first[2], Value::int(30));
        simulate_native_write(&data, 2, 99);
        let second = data.items();
        assert_eq!(
            second[2],
            Value::int(99),
            "second reference sees the resync"
        );
        // The line the old `*const Self as *mut Self` write made UB: reading
        // `first` again, after the resync `second` triggered.
        assert_eq!(first[2], Value::int(30), "first reference is untouched");
    }

    /// Generation stability (docs/adr/0030 §2.2): the first reference keeps
    /// reporting the *old* decode after a resync (it points at a retired
    /// generation), while a freshly-taken one reports the new — pinning the
    /// graveyard's semantics so a later "optimization" that overwrote the
    /// slot in place would fail here.
    #[test]
    fn retired_generations_are_never_overwritten_in_place() {
        let data = native_array();
        let gen0 = data.items() as *const Vec<Value>;
        simulate_native_write(&data, 0, 111);
        let gen1 = data.items();
        assert_eq!(gen1[0], Value::int(111));
        // SAFETY: `gen0` was derived from `&data` and `data` is still alive
        // and not otherwise borrowed here; dereferencing it is exactly the
        // "read the old generation" this test pins.
        let gen0_contents = unsafe { &*gen0 };
        assert_eq!(
            gen0_contents[0],
            Value::int(10),
            "the retired generation still reads its own decode, not gen1's"
        );
    }

    /// Pruning soundness: take a reference (forcing a resync so a generation
    /// exists), drop it, call an `&mut self` method, and assert the
    /// graveyard collapsed to a single generation. `&mut self` proves no
    /// shared borrow into any generation is live, so this is the point every
    /// exclusive-access entry point prunes at.
    #[test]
    fn an_exclusive_access_prunes_the_graveyard() {
        let mut data = native_array();
        simulate_native_write(&data, 1, 42);
        let _ = data.items(); // forces a resync: one generation now exists
        assert_eq!(data.native_generation_count(), Some(1));
        simulate_native_write(&data, 1, 43);
        let _ = data.items(); // a second resync: two generations exist
        assert_eq!(data.native_generation_count(), Some(2));
        data.items_mut()[0] = Value::int(7); // `&mut self` — prunes
        assert_eq!(
            data.native_generation_count(),
            Some(0),
            "items_mut collapses the graveyard back into the seed"
        );
        assert_eq!(data.items()[0], Value::int(7));
        assert_eq!(
            data.items()[1],
            Value::int(43),
            "the last resync's value survived the collapse"
        );
    }

    /// `Sync` posture: `ArrayData` (and therefore `Gc<ArrayData>`) must stay
    /// `Send + Sync` — the whole reason `SyncUnsafeCell` exists instead of a
    /// plain `UnsafeCell` field (docs/adr/0030 §2, `SyncUnsafeCell`'s own
    /// docs). A compile failure here means the cell's bound regressed.
    #[test]
    fn array_data_and_its_gc_handle_stay_send_and_sync() {
        fn assert_send<T: Send>() {}
        fn assert_sync<T: Sync>() {}
        assert_send::<ArrayData>();
        assert_sync::<ArrayData>();
        assert_send::<crate::gc::Gc<ArrayData>>();
        assert_sync::<crate::gc::Gc<ArrayData>>();
    }

    /// `Clone` independence (docs/adr/0030 §2.3): the node is shared (same
    /// `Gc` handle, matching pre-ADR-0030 semantics) but the decode
    /// *cache* — the generation graveyard, `dirty`, `snapshot` — is not: a
    /// `&mut`-side mutation on the clone that has not yet flushed to the
    /// shared node (i.e. `items_mut` without a following `items()` on the
    /// same handle) must not perturb the original's own cached decode. This
    /// is the exact scenario ADR-0030 §2.3 probed as benign
    /// (`my int @d = @c; @d[0] = 555` leaves `@c[0]` unchanged); it does NOT
    /// claim isolation once the clone's dirty write is actually flushed to
    /// the shared node by reading the clone itself — that residual sharing
    /// is open question #2 in the ADR, deliberately preserved rather than
    /// fixed here.
    #[test]
    fn clone_shares_the_node_but_not_the_cache() {
        let data = native_array();
        simulate_native_write(&data, 2, 99);
        let _ = data.items(); // establish a generation to clone from
        let mut cloned = data.clone();
        assert_eq!(
            data.native_storage_node()
                .map(|n| crate::gc::Gc::as_ptr(&n) as usize),
            cloned
                .native_storage_node()
                .map(|n| crate::gc::Gc::as_ptr(&n) as usize),
            "the native node is shared, matching pre-ADR-0030 Clone semantics"
        );
        cloned.items_mut()[0] = Value::int(555);
        assert_eq!(
            data.items()[0],
            Value::int(10),
            "the original's decode cache is untouched by an unflushed clone mutation"
        );
    }
}
