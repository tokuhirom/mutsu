//! The integration-test binary for the allocation-budget tests: each counts
//! the heap allocations a measured program makes through the thread-local
//! counting `#[global_allocator]` below, and a binary may link only one global
//! allocator, so they share this root instead of `tests/integration.rs`.
//!
//! Compiled out under the `alloc-stats` feature: that build installs its own
//! counting `#[global_allocator]` in the library.

#![cfg(not(feature = "alloc-stats"))]

use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::Cell;

mod attribute_source_file_alloc_budget;
mod grammar_parse_alloc_budget;
mod source_file_sym_fallback_alloc_budget;

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

/// Heap allocations made on the current thread so far.
fn allocs_now() -> u64 {
    ALLOCS.with(Cell::get)
}
