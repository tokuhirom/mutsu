//! The rows of `Lock`, `Lock::Async`, `Lock::ConditionVariable` and
//! `Cancellation` (ADR-11276 §9.157): receivers that stand for live OS state
//! by an id into the runtime's tables (`lock-id`, `cond-id`,
//! `cancellation-id`).
//!
//! None of them has a shape, so every row is reached through its owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`). A handler reads the ids off a
//! snapshot of the attributes, so a method that blocks (`lock`, `wait`) or runs
//! user code (`protect`) holds no lock on them.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $handler:expr) => {
        row!($owner, $name, $handler, RowFlags::OWNER_ONLY)
    };
    ($owner:literal, $name:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: $handler,
            flags: $flags,
            named: &[],
        }
    };
}

/// A row whose arguments its handler checks itself: `protect`'s one block
/// (anything else is `X::Multi::NoMatch`) and `wait`'s optional test.
const BLOCK_ARGS: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS);

pub(super) static ROWS: &[MethodRow] = &[
    row!("Lock", "protect", Handler::Interp(protect), BLOCK_ARGS),
    row!("Lock", "lock", Handler::Interp(lock)),
    row!("Lock", "unlock", Handler::Interp(unlock)),
    row!("Lock", "condition", Handler::Narrow(condition)),
    row!(
        "Lock::Async",
        "protect",
        Handler::Interp(protect),
        BLOCK_ARGS
    ),
    row!("Lock::Async", "lock", Handler::Interp(lock)),
    row!("Lock::Async", "unlock", Handler::Interp(unlock)),
    row!(
        "Lock::ConditionVariable",
        "wait",
        Handler::Interp(wait),
        BLOCK_ARGS
    ),
    row!("Lock::ConditionVariable", "signal", Handler::Narrow(signal)),
    row!(
        "Lock::ConditionVariable",
        "signal_all",
        Handler::Narrow(signal_all)
    ),
    row!("Cancellation", "cancel", Handler::Narrow(cancel)),
    row!("Cancellation", "cancelled", Handler::Narrow(cancelled)),
];

/// The owners an instance of `class` is answered from. `Lock::Async` is its own
/// class in Rakudo, with no `condition`; `Lock::Soft` is a `Lock`.
// Cost: O(1).
pub(super) fn owners_of(class: &str) -> Option<&'static [&'static str]> {
    Some(match class {
        "Lock" | "Lock::Soft" => &["Lock"],
        "Lock::Async" => &["Lock::Async"],
        "Lock::ConditionVariable" => &["Lock::ConditionVariable"],
        "Cancellation" => &["Cancellation"],
        _ => return None,
    })
}

/// A snapshot of the receiver's attributes.
// Cost: O(a), a = attributes.
fn attrs(target: &Value) -> Option<AttrMap> {
    match target.view() {
        ValueView::Instance { attributes, .. } => Some(attributes.to_map()),
        _ => None,
    }
}

// Cost: O(1) plus the wait for the lock and the block's own cost.
fn protect(interp: &mut Interpreter, target: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(interp.lock_protect(&attrs(target)?, args))
}

// Cost: O(1) plus the wait for the lock.
fn lock(interp: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(interp.lock_lock(&attrs(target)?))
}

// Cost: O(1).
fn unlock(interp: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(interp.lock_unlock(&attrs(target)?))
}

// Cost: O(1).
fn condition(target: &Value, _: &[Value]) -> Answer {
    Some(Interpreter::lock_condition(&attrs(target)?))
}

// Cost: O(1) per wake plus the test's cost.
fn wait(interp: &mut Interpreter, target: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if args.len() > 1 {
        return Some(Err(
            crate::runtime::methods_signature_errors::make_multi_no_match_error("wait"),
        ));
    }
    Some(interp.condition_wait(&attrs(target)?, args.first().cloned()))
}

// Cost: O(1).
fn signal(target: &Value, _: &[Value]) -> Answer {
    Some(Interpreter::condition_signal(&attrs(target)?, false))
}

// Cost: O(1).
fn signal_all(target: &Value, _: &[Value]) -> Answer {
    Some(Interpreter::condition_signal(&attrs(target)?, true))
}

// Cost: O(1) plus a bounded wait of at most 100 ms.
fn cancel(target: &Value, _: &[Value]) -> Answer {
    Some(Ok(Interpreter::cancellation_cancel(&attrs(target)?)))
}

// Cost: O(1).
fn cancelled(target: &Value, _: &[Value]) -> Answer {
    Some(Ok(Interpreter::cancellation_cancelled(&attrs(target)?)))
}
