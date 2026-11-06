//! A per-thread observer told when the thread first reaches a blocking point.
//!
//! `Supply.throttle` runs its callbacks on several workers so that callbacks
//! that block (`await $proc`, `sleep`) overlap, but a callback that never
//! blocks must still run in input order, as it does on rakudo where the next
//! job starts only after a thread picks it up. The throttle therefore starts
//! the next job when the running one has finished *or has parked*, and this is
//! how it learns about the park: every blocking point passes through
//! [`super::enter_blocking`], which calls [`notify`].

use std::cell::RefCell;

thread_local! {
    static OBSERVER: RefCell<Option<Box<dyn FnOnce()>>> = const { RefCell::new(None) };
}

/// Run `f` with `observer` installed on this thread; the observer fires at
/// most once, the first time `f` (or anything it calls) reaches a blocking
/// point. The observer is removed again when `f` returns.
// Cost: O(1) plus `f`.
pub(crate) fn run_with_park_observer<R>(observer: impl FnOnce() + 'static, f: impl FnOnce() -> R) -> R {
    OBSERVER.with(|o| *o.borrow_mut() = Some(Box::new(observer)));
    let result = f();
    OBSERVER.with(|o| o.borrow_mut().take());
    result
}

/// The calling thread reached a blocking point: fire the observer, if any.
// Cost: O(1) plus the observer.
pub(super) fn notify() {
    // Taken out before it runs, so an observer that itself blocks cannot fire
    // twice.
    if let Some(observer) = OBSERVER.with(|o| o.borrow_mut().take()) {
        observer();
    }
}
