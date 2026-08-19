//! [`SyncUnsafeCell`] — the interior-mutability primitive for a **read-path
//! cache fill performed under a shared borrow the caller keeps using**.
//!
//! mutsu has two interior-mutability primitives, on purpose (docs/adr/0030):
//!
//! - [`crate::gc::gc_contents_mut`] is for a caller that **holds the `Gc`
//!   handle** and performs a structural write, then drops the `&mut` before
//!   any other borrow of the payload is taken. ADR-0013 §8 measured under
//!   Miri that carrying a `Deref`'d `&T` *across* that write is UB — that
//!   shape is exactly what this primitive is for instead.
//! - `SyncUnsafeCell<T>` is for a **field** inside a payload that is
//!   otherwise read through an ordinary `&self`/`Deref`, where one specific
//!   field needs to be filled in lazily (a decode cache) without upgrading
//!   every read site to `&mut`. Only the cell's contents are ever mutated
//!   through a shared reference; the container that embeds the cell keeps
//!   its normal `&self`/`&mut self` API.
//!
//! Whichever primitive a call site reaches for, it must say why the *other*
//! one does not apply — see docs/adr/0030 §1.4/§2 for the worked example
//! (`ArrayData`'s native-array decode cache).

use std::cell::UnsafeCell;

/// `UnsafeCell<T>` plus the `Sync` impl `UnsafeCell` itself deliberately
/// withholds. Adding a plain `UnsafeCell` field to a payload type makes it
/// `!Sync`, which would strip `Gc<T>`'s `unsafe impl<T: ?Sized + Sync> Sync
/// for GcBox<T>` (`src/gc/gc_ptr.rs`) of the `Sync` it depends on — and
/// mutsu's containers cross `start`/`Promise`/`hyper`/`race` thread
/// boundaries, so that `Sync` is load-bearing, not incidental.
///
/// # Safety contract for implementors
///
/// `T: Send` is enough to make `&SyncUnsafeCell<T>` safely shareable across
/// threads in the same sense a `Mutex<T>` would be, **except this type
/// performs none of a `Mutex`'s synchronization** — `get()` hands out a raw
/// pointer with no locking, no atomics, and no ordering guarantee. Callers
/// that write through it while another thread might read or write the same
/// cell concurrently need their own synchronization; mutsu's contract for
/// this is the residual cross-thread race deferred to ADR-0001 layer 3c (the
/// `__mutsu_atomic_arr::` / `shared_vars` lanes), unchanged by this
/// primitive. `SyncUnsafeCell` only legalizes the **single-threaded**
/// aliasing shape — a write under a live shared borrow of the *enclosing*
/// struct — that a plain `UnsafeCell` already permits; it does not add any
/// cross-thread guarantee `UnsafeCell` lacks.
pub(crate) struct SyncUnsafeCell<T>(UnsafeCell<T>);

// SAFETY: see the contract above. `T: Send` (not `Sync`) is the correct bound
// because this cell only ever grants one live access path (through the
// owning struct's borrow), never a genuinely-shared read across threads —
// the same reasoning `std`'s own unstable `SyncUnsafeCell` uses.
unsafe impl<T: Send> Sync for SyncUnsafeCell<T> {}

impl<T> SyncUnsafeCell<T> {
    pub(crate) fn new(value: T) -> Self {
        SyncUnsafeCell(UnsafeCell::new(value))
    }

    /// Raw pointer to the contents. Dereferencing it is unsafe: the caller
    /// must ensure no other live reference (shared or exclusive) into `T`
    /// exists for the duration of the access this pointer is used for.
    pub(crate) fn get(&self) -> *mut T {
        self.0.get()
    }

    /// Exclusive access via the enclosing `&mut SyncUnsafeCell<T>` — safe,
    /// since the borrow checker already proves no other reference exists.
    pub(crate) fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_and_get_mut_round_trip() {
        let mut cell = SyncUnsafeCell::new(1i64);
        // SAFETY: no other reference into the cell is live.
        unsafe { *cell.get() = 2 };
        assert_eq!(*cell.get_mut(), 2);
        *cell.get_mut() = 3;
        // SAFETY: as above.
        assert_eq!(unsafe { *cell.get() }, 3);
    }

    #[test]
    fn is_sync_for_a_send_payload() {
        fn assert_sync<T: Sync>() {}
        assert_sync::<SyncUnsafeCell<i64>>();
    }

    #[test]
    fn write_through_a_shared_reference_is_observable() {
        // The shape this primitive exists for: a write performed while only
        // a shared `&SyncUnsafeCell<T>` is held (no `&mut` in scope at all).
        let cell = SyncUnsafeCell::new(vec![1, 2, 3]);
        let shared: &SyncUnsafeCell<Vec<i32>> = &cell;
        // SAFETY: single-threaded, no other reference into the cell is live.
        unsafe { (*shared.get()).push(4) };
        // SAFETY: as above.
        assert_eq!(unsafe { &*shared.get() }, &[1, 2, 3, 4]);
    }
}
