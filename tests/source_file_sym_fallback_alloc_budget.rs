//! #9684: `unit_of_source_sym` allocates a fresh `String` per call once
//! `program_path` is set (as `src/main.rs` always sets it before `run`).
//!
//! `Symbol::resolve()` is documented as "prefer `as_str()` on hot paths -- this
//! allocates a fresh `String`", but `unit_of_source_sym`
//! (`src/runtime/compunit_scope.rs`) used it to compare a routine's declaring
//! file against `$*PROGRAM` on every call: `(Some(f), Some(prog)) if
//! f.resolve() == prog`. Before PR #9627 that branch was essentially
//! unreachable on a plain script's hot path (`CompiledFunction::source_file_sym`
//! returned `None` for an ordinary top-level `sub`); #9627 widened
//! `source_file_sym` to fall back to `self.code.source_file`, so the branch --
//! and its allocation -- now fires on every call of every routine. `as_str()`
//! performs the identical string comparison with no allocation.
//!
//! The pin is deterministic rather than timed: a thread-local counting global
//! allocator records heap allocations, and the two-length-slope method isolates
//! one call's own cost from the loop's fixed and per-iteration overhead --
//! same method as `tests/regex_match_intern_budget.rs`, but for allocations
//! instead of interns (`symbol::intern_calls()` does not move here: the
//! allocation this bug adds is a `String`, not a re-intern).
//!
//! `set_program_path` is required to reproduce the bug: with no program path
//! (as plain `Interpreter::new().run(..)` leaves it, the shape the other
//! budget tests in this directory use), `unit_of_source_sym`'s `Some(prog)`
//! guard never matches and the allocating branch is never reached at all.
//!
//! Compiled out under the `alloc-stats` feature: that build installs its own
//! counting `#[global_allocator]` in the library, and a crate may link only one.

#![cfg(not(feature = "alloc-stats"))]

use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::Cell;

struct CountingAllocator;

thread_local! {
    static ALLOCS: Cell<u64> = const { Cell::new(0) };
}

unsafe impl GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let _ = ALLOCS.try_with(|c| c.set(c.get().wrapping_add(1)));
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let _ = ALLOCS.try_with(|c| c.set(c.get().wrapping_add(1)));
        unsafe { System.alloc_zeroed(layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let _ = ALLOCS.try_with(|c| c.set(c.get().wrapping_add(1)));
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static ALLOC: CountingAllocator = CountingAllocator;

fn allocs_now() -> u64 {
    ALLOCS.with(Cell::get)
}

/// Allocations made running `src` as a top-level program, with `program_path`
/// set the way the CLI always sets it, measured after a warm-up run so
/// one-time parsing/compilation cost is not counted.
///
/// Runs on its own thread with a generous native stack: deep recursion (the
/// call shape this test needs to reach the bug) is far past libtest's 2 MiB
/// default. `ALLOCS` is thread-local, so spawning a fresh thread per call
/// also starts the count at zero with no explicit reset.
fn allocs_for(src: &str) -> u64 {
    const STACK_SIZE: usize = 512 * 1024 * 1024;
    let src = src.to_string();
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || {
            let mut warm = mutsu::Interpreter::new();
            warm.set_program_path("script.raku");
            warm.run(&src).expect("program runs");
            drop(warm);

            let before = allocs_now();
            let mut interp = mutsu::Interpreter::new();
            interp.set_program_path("script.raku");
            interp.run(&src).expect("program runs");
            allocs_now() - before
        })
        .expect("spawn measurement thread")
        .join()
        .expect("measurement thread")
}

/// Allocations attributable to one extra recursion level, isolated from the
/// program's fixed startup cost by running `down($n)` -- a linear, single
/// recursive call per level, exactly `benchmarks/fib.raku`'s own call shape --
/// at two depths. `down` calls itself, matching the recursive-call path the
/// bug is reported against: a mainline-to-sub call did not reproduce it (a
/// plain call from the mainline into a top-level sub takes a different,
/// already-allocation-free entry than one routine frame calling another).
fn allocs_per_recursive_call() -> f64 {
    const LOW: usize = 100;
    const HIGH: usize = 600;
    let program = |n: usize| {
        format!("sub down(Int $n) {{ $n <= 0 ?? 0 !! 1 + down($n - 1) }}\nsay down({n});")
    };
    let lo = allocs_for(&program(LOW));
    let hi = allocs_for(&program(HIGH));
    // A whole run's count moves by a few allocations from process to process
    // (hash tables seeded per process grow at different points), and with a
    // call path that allocates nothing that noise is all the two runs differ
    // by: 30 runs of this test gave 1-6 allocations between them, and one run
    // under a loaded machine gave -1. So the sanity check allows that much;
    // anything below it means the measurement itself is broken.
    const NOISE: u64 = 16;
    assert!(
        hi + NOISE >= lo,
        "allocations cannot shrink with more recursion: {lo} -> {hi}"
    );
    hi.saturating_sub(lo) as f64 / (HIGH - LOW) as f64
}

#[test]
fn recursive_call_does_not_allocate_a_string_to_find_its_unit() {
    let per_call = allocs_per_recursive_call();
    eprintln!("recursive call: {per_call:.3} allocations per call");
    // Before the fix, `unit_of_source_sym` allocated one `String` per call via
    // `Symbol::resolve()` on this exact path (a top-level `sub`'s declaring
    // file always equals `$*PROGRAM`, so one routine frame calling another
    // always took the allocating branch): measured 1.002 allocations/call on
    // this test's own program, matching the issue's measured +1,172%
    // allocation jump on `benchmarks/fib.raku` (whose calls otherwise
    // allocate close to nothing). 0.0 after the fix.
    let limit = 0.5;
    assert!(
        per_call <= limit,
        "a recursive call allocates a String to compare its declaring file \
         against $*PROGRAM ({per_call:.3} allocations/call, budget {limit}); \
         see #9684"
    );
}
