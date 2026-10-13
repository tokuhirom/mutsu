//! Mutual exclusion for the *structural* mutation of an aliased container
//! (ADR-0068).
//!
//! `crate::gc::gc_contents_mut` is the codebase's single aliased-container-write
//! primitive: given a `Gc<T>` whose strong count is greater than one it hands
//! out a `&mut T`, so a write through one alias is visible through every holder
//! of the node. That is how mutsu implements Raku container identity, and it is
//! correct on one thread. Across threads it is a data race on a `Vec<Value>` /
//! `HashMap`: two `Vec::resize` calls on the same allocation reallocate under
//! each other, which shows up as `free(): double free detected in tcache 2`,
//! `double free or corruption (out)`, or a silently lost update.
//!
//! The name-keyed cross-thread lanes in `runtime/runtime_shared_vars.rs`
//! (`__mutsu_atomic_arr::`, `shared_array_elem_set`) cover the common case, but
//! they *decline* for a container that has been boxed into a shared
//! `ContainerRef` cell, on the recorded premise that such a container "is
//! already shared through the Mutex". It is not: `descend_container_ref` takes
//! `ContainerCell`'s `Mutex<Value>` only long enough to derive a raw pointer
//! into the cell's slot, then releases it and lets the rest of the statement
//! mutate through `gc_contents_mut` with nothing held. The store therefore
//! excludes neither another writer nor a *reader* — and the reader is the
//! dominant hazard, because `Value::with_deref` / `into_deref` clone the inner
//! `Value` out under that same (already released) lock and can take a refcount
//! on a node the writer has just dropped.
//!
//! This module supplies the missing edge, as a small table of striped mutexes
//! that both sides take. The key is the address of the shared
//! **`ContainerCell`**, not of the container node: measured, twenty threads
//! writing one celled array reach *thirteen* distinct `Gc<ArrayData>` addresses
//! (`Gc::make_mut` copies an aliased node), so a node-keyed lock excludes
//! nothing, while the cell is the one thing every alias genuinely shares. The
//! node address is the fallback for an uncelled root. It is deliberately not
//! `ContainerCell`'s own `Mutex`: `descend_container_ref` must take that to
//! produce the pointer it returns, so holding it across the mutation would
//! self-deadlock on the next read of the same cell. See ADR-0068 §7.
//!
//! Three properties keep it affordable and deadlock-free:
//!
//! - **Single-threaded programs pay one relaxed atomic load.** Nothing is
//!   locked until a thread that can run user VM code has actually been spawned
//!   ([`note_mutator_thread_spawned`], ADR-0068 §4 step 1).
//! - **A thread holds at most one of these locks at a time.** A nested
//!   acquisition is a no-op, so no lock-ordering cycle can exist between two
//!   threads and a re-entrant store (`@a[0] = ...` reaching another container
//!   store while the first is in flight) cannot self-deadlock.
//! - **The guarded region is a leaf.** Callers take it around the structural
//!   mutation itself, not around a region that can call back into user Raku
//!   code, so a holder never blocks on another thread while holding it.
//!
//! Measured (2026-09-09, ADR-0068 §14, [#7613]): "affordable" holds in both
//! directions. A program that never spawns a mutator thread is within noise
//! over 6.4M reads that would each have locked; a program that spawns one and
//! then reads a bound container single-threaded pays +5-7% on a loop that does
//! nothing else; and a genuinely concurrent one runs 12-13% FASTER with these
//! locks than without, because serializing readers on a stripe is cheaper than
//! letting several cores contend on the cell's own `Mutex<Value>` and on the
//! inner node's refcount atomics. `STRIPES` is not a lever either: four
//! independent cells measure -0.6% / +3.2%, and where the cost concentrates
//! (one hot cell) more stripes cannot help, since excluding on that cell is
//! what correctness requires.
//!
//! [#7613]: https://github.com/tokuhirom/mutsu/issues/7613
//!
//! The at-most-one rule leaves a known hole — a read of a *different* cell,
//! reached from inside a guarded store, is unprotected. ADR-0068 §6 question 1
//! anticipated exactly this and set the stress harness rather than the argument
//! as the acceptance gate; it is at 0/240 where the unfixed build was 93/96.

use std::cell::Cell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, MutexGuard};

/// Set once a thread that can run user VM code has been spawned; never
/// cleared. A program that never spawns one skips every lock below.
static MULTI_MUTATOR_THREADS: AtomicBool = AtomicBool::new(false);

/// Record that a second VM mutator thread now exists. Called from the
/// registered-spawn helpers in `runtime::builtins_system`.
pub(crate) fn note_mutator_thread_spawned() {
    MULTI_MUTATOR_THREADS.store(true, Ordering::Relaxed);
}

/// Whether more than one VM mutator thread has ever been live. This is the
/// ADR-0068 §4 step 1 gate: it is deliberately sticky (never cleared when a
/// thread finishes), because a container written by a thread that has since
/// exited can still be aliased by one that has not.
#[inline]
pub(crate) fn multi_mutator_threads_live() -> bool {
    MULTI_MUTATOR_THREADS.load(Ordering::Relaxed)
}

/// Number of striped mutexes. A power of two so the index is a mask.
const STRIPES: usize = 64;

static STRIPE_LOCKS: [Mutex<()>; STRIPES] = [const { Mutex::new(()) }; STRIPES];

fn stripe_for(addr: usize) -> &'static Mutex<()> {
    &STRIPE_LOCKS[stripe_index(addr)]
}

fn stripe_index(addr: usize) -> usize {
    // Container nodes are heap allocations, so the low bits are alignment
    // padding; shift them out before masking or every node lands in a handful
    // of stripes.
    (addr >> 4) & (STRIPES - 1)
}

thread_local! {
    /// Whether this thread already holds a container-structure lock. See the
    /// module docs: at most one per thread is what makes the scheme
    /// deadlock-free without a re-entrant mutex.
    static HOLDS_STRUCT_LOCK: Cell<bool> = const { Cell::new(false) };
}

/// Held for the duration of an aliased container's structural mutation.
///
/// Obtain it with [`ContainerStructGuard::acquire_for`] and keep it in a local
/// so it is released when the mutation region ends.
pub(crate) struct ContainerStructGuard {
    _guard: MutexGuard<'static, ()>,
    /// The node's stripe, for a celled container whose node hashes to a
    /// different stripe than its cell. See [`Self::acquire_for`].
    _node_guard: Option<MutexGuard<'static, ()>>,
}

impl ContainerStructGuard {
    /// Lock the stripe covering a container the caller is about to mutate
    /// structurally, if a lock is needed at all.
    ///
    /// `cell_addr` is the address of the shared `ContainerRef` cell the
    /// container was reached through, when there was one; it takes precedence
    /// over the container node, because two threads writing one celled
    /// container share the cell but NOT the node — measured, they reach 13
    /// distinct node addresses, since `Gc::make_mut` copies an aliased node.
    /// The node address is the fallback for an uncelled root.
    ///
    /// Returns `None` — and locks nothing — when the process has never spawned
    /// a second VM mutator thread, when this thread already holds one of these
    /// locks, or when there is nothing shared to key on.
    pub(crate) fn acquire_for(
        cell_addr: Option<usize>,
        container: &crate::value::Value,
    ) -> Option<Self> {
        if !multi_mutator_threads_live() {
            return None;
        }
        let node = container_node_addr(container);
        match (cell_addr, node) {
            // A container reached through a cell is keyed on BOTH: the cell is
            // what two threads writing through it share (they can hold
            // different nodes once `Gc::make_mut` copies an aliased one), and
            // the node is what a reader that was handed the container itself
            // (an argument, a `.map` source) keys on, since it never saw the
            // cell. A writer that mutates the node in place
            // (`gc_contents_mut`) must exclude both (#13016).
            (Some(cell), Some(node)) => Self::acquire_pair(cell, node),
            (Some(addr), None) | (None, Some(addr)) => Self::acquire(addr),
            (None, None) => None,
        }
    }

    /// [`Self::acquire_for`] for a writer that holds the cell itself: keyed on
    /// the cell and on the node the cell holds while the stripes are held. The
    /// node is read before the stripes are taken, so a writer that re-seats
    /// the cell in between would leave the wrong node stripe held; the read is
    /// repeated under the stripes and the pair re-taken until they agree.
    // Cost: O(1) expected; one retry per concurrent re-seat of the cell.
    pub(crate) fn acquire_for_celled(cell: &crate::value::ContainerCell) -> Option<Self> {
        Self::acquire_for_cell_holding(std::ptr::from_ref(cell) as usize, || cell.load())
    }

    /// [`Self::acquire_for_celled`] for a caller that reaches the cell's value
    /// through its own path (`load`, run before and under the stripes, returns
    /// what the cell at `cell_addr` holds now; under the stripes it must not
    /// block on one).
    // Cost: O(1) expected, plus two calls of `load`; one retry per concurrent
    // re-seat of the cell.
    pub(crate) fn acquire_for_cell_holding(
        cell_addr: usize,
        mut load: impl FnMut() -> crate::value::Value,
    ) -> Option<Self> {
        if !multi_mutator_threads_live() {
            return None;
        }
        loop {
            let node = container_node_addr(&load());
            let guard = match node {
                Some(node) => Self::acquire_pair(cell_addr, node),
                None => Self::acquire(cell_addr),
            }?;
            if container_node_addr(&load()) == node {
                return Some(guard);
            }
        }
    }

    /// [`Self::acquire_for`] for a pure read that walks two containers
    /// together (`eqv`): keyed on both nodes, taken in stripe order like
    /// every pair (#13079).
    // Cost: O(1).
    pub(crate) fn acquire_for_both(
        a: &crate::value::Value,
        b: &crate::value::Value,
    ) -> Option<Self> {
        if !multi_mutator_threads_live() {
            return None;
        }
        match (container_node_addr(a), container_node_addr(b)) {
            (Some(x), Some(y)) => Self::acquire_pair(x, y),
            (Some(x), None) | (None, Some(x)) => Self::acquire(x),
            (None, None) => None,
        }
    }

    /// Lock the stripes of two keys. They are taken in stripe order, which is
    /// the one order every pair acquisition uses, so two threads cannot wait
    /// on each other; a single-key acquisition holds one stripe and waits for
    /// none, which keeps the scheme deadlock-free.
    fn acquire_pair(a: usize, b: usize) -> Option<Self> {
        let (ia, ib) = (stripe_index(a), stripe_index(b));
        if ia == ib {
            return Self::acquire(a);
        }
        if HOLDS_STRUCT_LOCK.with(Cell::get) {
            return None;
        }
        HOLDS_STRUCT_LOCK.with(|held| held.set(true));
        let lock = |i: usize| {
            STRIPE_LOCKS[i]
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner())
        };
        let first = lock(ia.min(ib));
        let second = lock(ia.max(ib));
        Some(Self {
            _guard: first,
            _node_guard: Some(second),
        })
    }

    /// Exclude concurrent access to what a shared `ContainerRef`/`ContainerView`
    /// cell points at.
    ///
    /// The cell's own `Mutex<Value>` is NOT enough on its own: a reader takes it
    /// to clone the inner `Value` out, while the element store derives a raw
    /// pointer into the same slot, releases the lock, and mutates for the rest
    /// of the statement (see `Interpreter::descend_container_ref`). The two
    /// therefore never exclude each other, and a reader can clone a `Value`
    /// mid-overwrite — which increments the refcount of a node the writer has
    /// already dropped. Both sides take this instead.
    pub(crate) fn acquire_for_cell(
        cell: &crate::gc::Gc<crate::value::ContainerCell>,
    ) -> Option<Self> {
        if !multi_mutator_threads_live() {
            return None;
        }
        Self::acquire(crate::gc::Gc::as_ptr(cell) as usize)
    }

    /// The address-keyed half of [`Self::acquire_for`], for a caller that
    /// already knows the node address.
    pub(crate) fn acquire(addr: usize) -> Option<Self> {
        if !multi_mutator_threads_live() || HOLDS_STRUCT_LOCK.with(Cell::get) {
            return None;
        }
        HOLDS_STRUCT_LOCK.with(|held| held.set(true));
        // A panic inside a guarded region poisons the stripe, and every
        // unrelated container that hashes to it would then panic too. The
        // invariant this lock protects is "one writer at a time", which a
        // previous panic does not invalidate, so recover rather than propagate.
        let guard = stripe_for(addr)
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        Some(Self {
            _guard: guard,
            _node_guard: None,
        })
    }
}

impl Drop for ContainerStructGuard {
    fn drop(&mut self) {
        HOLDS_STRUCT_LOCK.with(|held| held.set(false));
    }
}

impl crate::value::ContainerCell {
    /// Replace the value this cell holds, under the cell's stripe as well as
    /// its own mutex, and return the value it held before.
    ///
    /// The mutex alone excludes only the other holders of the mutex. A writer
    /// that mutates through the pointer `descend_container_ref` derives (a
    /// `Mut` method row, `push_to_shared_var`, an element store) holds the
    /// cell's stripe instead and has released the mutex, so a bare
    /// `*cell.lock() = v` replaced the slot under it: the two dropped the old
    /// node twice, and the main thread later died on `Gc::drop strong-count
    /// underflow` (#13016, a callee's `@x` writeback racing a worker's
    /// `@x.splice`). Every store that replaces a cell's whole value goes
    /// through here. A no-op beyond the mutex until a second VM mutator thread
    /// exists, and when this thread already holds a stripe.
    ///
    /// The old value is returned rather than dropped inside the locks, so a
    /// caller that does not want it drops it with nothing held.
    // Cost: O(1).
    pub(crate) fn store(&self, value: crate::value::Value) -> crate::value::Value {
        let _structure = ContainerStructGuard::acquire(std::ptr::from_ref(self) as usize);
        let mut slot = self
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        std::mem::replace(&mut *slot, value)
    }

    /// A clone of the value this cell holds, taken under the cell's stripe as
    /// well as its mutex: the read half of [`Self::store`]. Cloning under the
    /// mutex alone can take a refcount on a node that a writer holding the
    /// stripe (and not the mutex) is dropping at that moment (#13016). A
    /// reader that only inspects the value in place uses `Value::with_deref`,
    /// which takes the same pair.
    // Cost: O(1).
    pub(crate) fn load(&self) -> crate::value::Value {
        let _structure = ContainerStructGuard::acquire(std::ptr::from_ref(self) as usize);
        self.lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
    }
}

/// The address of `container`'s backing GC node. This is the FALLBACK key, used
/// only when the container was not reached through a cell -- two threads that
/// share a cell do not reliably share a node (see `acquire_for`). `None` for
/// anything without shared structure.
pub(crate) fn container_node_addr(container: &crate::value::Value) -> Option<usize> {
    match container.view() {
        crate::value::ValueView::Array(items, ..) => Some(crate::gc::Gc::as_ptr(&items) as usize),
        crate::value::ValueView::Hash(map) => Some(crate::gc::Gc::as_ptr(&map) as usize),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_thread_takes_at_most_one_structure_lock() {
        note_mutator_thread_spawned();
        let outer = ContainerStructGuard::acquire(0x1000);
        assert!(outer.is_some(), "first acquisition locks");
        let inner = ContainerStructGuard::acquire(0x2000);
        assert!(
            inner.is_none(),
            "a nested acquisition must be a no-op, or two threads could take \
             two stripes in opposite orders and deadlock"
        );
        drop(inner);
        drop(outer);
        assert!(
            ContainerStructGuard::acquire(0x2000).is_some(),
            "the thread-local flag must be cleared on drop"
        );
    }

    #[test]
    fn a_celled_container_locks_both_its_cell_and_its_node() {
        // A writer through a `:=` cell and a reader handed the container
        // itself must exclude each other (#13016): the writer holds the cell's
        // stripe AND the node's, the reader only knows the node.
        note_mutator_thread_spawned();
        let (cell, node) = (0x7020, 0x7030);
        assert_ne!(stripe_index(cell), stripe_index(node));
        let writer = ContainerStructGuard::acquire_pair(cell, node);
        assert!(writer.is_some());
        let (cell_free, node_free) = std::thread::spawn(move || {
            (
                stripe_for(cell).try_lock().is_ok(),
                stripe_for(node).try_lock().is_ok(),
            )
        })
        .join()
        .unwrap();
        assert!(!cell_free, "the cell's stripe is held");
        assert!(!node_free, "the node's stripe is held");
        drop(writer);
        assert!(stripe_for(node).try_lock().is_ok());
        // One key, or two keys on one stripe, takes a single lock.
        assert!(ContainerStructGuard::acquire_pair(0x8040, 0x8040 + (STRIPES << 4)).is_some());
    }

    #[test]
    fn stripes_are_stable_and_spread() {
        assert!(std::ptr::eq(stripe_for(0x5550), stripe_for(0x5550)));
        let distinct = (0..STRIPES)
            .map(|i| stripe_for(0x1000 + (i << 4)) as *const _)
            .collect::<std::collections::HashSet<_>>();
        assert_eq!(
            distinct.len(),
            STRIPES,
            "consecutive nodes must not collide"
        );
    }
}
