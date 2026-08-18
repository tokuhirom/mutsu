//! How the VM knows whether a bare `done` has a react/supply drive loop to
//! terminate.
//!
//! In rakudo, `done` outside any react/supply is an ordinary, catchable
//! exception at the point it is raised:
//!
//! ```raku
//! try { done };
//! say $!.^name;      # X::ControlFlow
//! say $!.illegal;    # done
//! say $!.enclosing;  # supply or react
//! ```
//!
//! mutsu raised a *control signal* unconditionally (`RuntimeError::react_done_signal()`
//! at the `ReactDone` opcode), and `try` deliberately lets a `done` signal pass
//! through so it can reach its react/supply drive loop even across nested
//! `try`s — but that also meant a *bare* `done`, with nothing to catch it,
//! escaped every handler and surfaced only as an uncaught Rust-level error at
//! the top of the program.
//!
//! The discriminator has to be dynamic and per-thread, for the same two
//! reasons documented in `loop_handler_depth`: a `done` legitimately crosses
//! routine/`EVAL` boundaries the moment a `whenever` body calls a nested sub
//! (`t/supply-nested-sub-emit-routes-to-own-supply.t`), and a `whenever`
//! callback frequently runs on a scheduler *worker thread* distinct from the
//! thread that started the `react`/drove the on-demand `supply` — so an
//! `Interpreter` field like `react_active`/`supply_emit_buffer` (both reset
//! to empty on a freshly spawned worker thread's `Interpreter`, see
//! `runtime_thread.rs`) is invisible from the callback's own thread. A
//! thread-local depth, raised for the extent of
//! [`Interpreter::call_react_callback`] — the single dispatch point every
//! `whenever`/`LAST`/`QUIT`/`CLOSE` callback body goes through, on whichever
//! thread actually runs it — is what correctly answers "is a react/supply
//! consumer dynamically active on *this* thread right now?".
//!
//! **The sweep has to stay complete** the same way `loop_handler_depth`'s
//! does: any other entry point that would itself synchronously consume a
//! `done` signal (`is_react_done()`) without holding this guard will still
//! catch the signal if one is raised, but the `ReactDone` opcode will have
//! already converted it into the illegal/uncatchable-by-that-consumer form
//! first.

use std::cell::Cell;

thread_local! {
    static REACT_DONE_HANDLER_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// True while a react/supply drive loop is dynamically active on this
/// thread and would consume a `done` signal. Consulted by the `ReactDone`
/// opcode.
pub(crate) fn react_done_handler_in_scope() -> bool {
    REACT_DONE_HANDLER_DEPTH.with(|d| d.get() > 0)
}

/// Raises the react/supply `done`-handler depth until dropped.
pub(crate) struct ReactDoneHandlerGuard;

impl ReactDoneHandlerGuard {
    pub(crate) fn new() -> Self {
        REACT_DONE_HANDLER_DEPTH.with(|d| d.set(d.get() + 1));
        Self
    }
}

impl Drop for ReactDoneHandlerGuard {
    fn drop(&mut self) {
        REACT_DONE_HANDLER_DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}
