//! How the VM knows whether a `next`/`last`/`redo` has a loop to act on.
//!
//! In rakudo a loop-control statement that finds no construct to act on is an
//! ordinary, catchable exception at the point it is raised:
//!
//! ```raku
//! try { my $i; { $i++; next; $i--; } };
//! say $!.^name;      # X::ControlFlow
//! say $!.illegal;    # next
//! say $!.enclosing;  # loop construct
//! ```
//!
//! mutsu raises a *control signal*, and `try`/`CATCH` deliberately pass control
//! signals through, so the signal escaped every handler and surfaced only at the
//! top of the program — uncatchable.
//!
//! The discriminator cannot be static. A control signal legitimately crosses
//! routine and `EVAL` boundaries (`sub f { next }; for 1..3 { f() }` iterates
//! three times, in rakudo and in mutsu), so neither "there is no lexically
//! enclosing loop" nor "we crossed a call frame" is the question. The question
//! is dynamic: **is there a construct on the dynamic chain right now that would
//! handle the signal?**
//!
//! So every construct that handles one raises this depth for the dynamic extent
//! in which it would catch, and the raise site consults it. A thread-local with
//! an RAII guard, rather than a field on `Interpreter`, for two reasons: the
//! guard needs no borrow of `self` (the handler sites are deep inside loops that
//! already hold `&mut self`), and `Drop` makes it correct on every early return
//! and `?` — a hand-written decrement would have to be repeated at each of the
//! dozens of exits and a single missed one turns a working `next` into a thrown
//! exception. A control signal never crosses a thread, so per-thread state is
//! the right scope.
//!
//! **The sweep has to stay complete.** A construct that catches
//! `is_next()`/`is_last()`/`is_redo()` without holding a guard will still catch
//! the signal *if one is raised* — but the raise site will have converted it to
//! an exception first, silently breaking that loop. `git grep -l
//! 'is_next()\|is_last()\|is_redo()'` is the checklist.

use std::cell::Cell;

thread_local! {
    static LOOP_HANDLER_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// True while some construct on the dynamic chain would handle a loop-control
/// signal. Consulted by the `Last`/`Next`/`Redo` opcodes.
pub(crate) fn loop_handler_in_scope() -> bool {
    LOOP_HANDLER_DEPTH.with(|d| d.get() > 0)
}

/// Raises the loop-control handler depth until dropped.
pub(crate) struct LoopHandlerGuard;

impl LoopHandlerGuard {
    pub(crate) fn new() -> Self {
        LOOP_HANDLER_DEPTH.with(|d| d.set(d.get() + 1));
        Self
    }
}

impl Drop for LoopHandlerGuard {
    fn drop(&mut self) {
        LOOP_HANDLER_DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}
