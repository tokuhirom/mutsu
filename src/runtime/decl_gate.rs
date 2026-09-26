//! Hold a background consumer's start until the declaration its callback
//! captures has been stored (#9590).
//!
//! In `my $tap = Supply.interval(0.05).tap({ ...; $tap.close })` the callback
//! is created inside `$tap`'s own initializer and reads `$tap` through the
//! shared cell a self-captured declaration gets
//! (`CompiledCode::self_capture_decl_locals`). A channel-backed `.tap` runs
//! that callback on a pool worker, so the first event can reach it before
//! `.tap` has returned and the declaration has stored the Tap — the callback
//! then sees `Any`. How often it loses is a matter of scheduling, so the
//! ordering is made structural instead: the tap does not submit its consumer
//! while the declaration is in flight, and the declaration's store submits it.
//!
//! A held task is also released at the declaring thread's next blocking point
//! (`worker_pool::enter_blocking`, which `gc::block_quiescent` and
//! `gc::wait_until` both go through). An initializer that waits for its own
//! callback (`my $t = await-then-return(...tap({ $p.keep; $t }))`) would
//! otherwise deadlock; blocking there gives up the ordering, which is exactly
//! the behavior Rakudo has.
//!
//! The pending list is per thread: the tap and the declaration's store run on
//! the same thread, in the same interpreter.
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};
use std::cell::RefCell;

type Task = Box<dyn FnOnce() + Send + 'static>;

thread_local! {
    /// Consumers held until every named declaration is stored, oldest first.
    static PENDING: RefCell<Vec<(Vec<Symbol>, Task)>> = const { RefCell::new(Vec::new()) };
}

/// The self-captured declarations whose initializer created `callback`, if it
/// is such a closure (see `CompiledCode::captures_own_declaration`).
fn own_declarations(callback: &Value) -> Option<Vec<Symbol>> {
    match callback.view() {
        ValueView::Sub(data) => data
            .compiled_code
            .as_ref()
            .filter(|cc| !cc.captures_own_declaration.is_empty())
            .map(|cc| cc.captures_own_declaration.clone()),
        _ => None,
    }
}

/// Submit `task` — a background consumer that will call `callback` — to the
/// worker pool, or hold it until the declaration `callback` captures from
/// inside its own initializer is stored.
// Cost: O(1) amortized; O(k) to copy the k declarations it waits for (almost always 1).
pub(crate) fn submit_after_declaration(callback: &Value, task: impl FnOnce() + Send + 'static) {
    let Some(decls) = own_declarations(callback) else {
        crate::runtime::worker_pool::submit(task);
        return;
    };
    // A closure nested in several such initializers waits for all of them.
    PENDING.with(|p| p.borrow_mut().push((decls, Box::new(task))));
}

/// The declaration `sym` has stored its value: submit every consumer held for it.
// Cost: O(p * k), p = consumers held on this thread (0 outside an in-flight self-captured declaration), k = declarations each waits for.
pub(crate) fn declared(sym: Symbol) {
    let ready: Vec<Task> = PENDING.with(|p| {
        let mut p = p.borrow_mut();
        if p.is_empty() {
            return Vec::new();
        }
        let mut ready = Vec::new();
        let mut held = Vec::with_capacity(p.len());
        for (mut decls, task) in p.drain(..) {
            decls.retain(|s| *s != sym);
            if decls.is_empty() {
                ready.push(task);
            } else {
                held.push((decls, task));
            }
        }
        *p = held;
        ready
    });
    for task in ready {
        crate::runtime::worker_pool::submit(task);
    }
}

/// The thread is about to block: submit every held consumer, since the
/// declaration they wait for may be the thing this thread is waiting on.
// Cost: O(p), p = consumers held on this thread (almost always 0).
pub(crate) fn release_all() {
    // `try_with`: blocking points also run during thread teardown.
    let ready: Vec<Task> = PENDING
        .try_with(|p| {
            let mut p = p.borrow_mut();
            p.drain(..).map(|(_, t)| t).collect()
        })
        .unwrap_or_default();
    for task in ready {
        crate::runtime::worker_pool::submit(task);
    }
}
