//! A `whenever <Promise>` source as a one-shot supply event stream.

use super::{SupplyReceiver, supply_event_channel};
use crate::runtime::native_methods::SupplyEvent;
use crate::value::SharedPromise;

/// The event stream of a `whenever $promise` source: a Kept promise emits its
/// result and is done, a Broken one quits with its cause — exactly what
/// Rakudo's `Promise.Supply` does, so a broken promise reaches the `whenever`'s
/// `QUIT` phaser, or quits the enclosing `react`/`supply` block.
///
/// Delivery rides the promise's own waiter list rather than an OS thread
/// parked in `wait()`: a `whenever <Promise>` costs no thread, and a block
/// that ends before its promise settles leaves nothing behind (issue #7609 —
/// `roast/S17-procasync/stress.t`'s 1200 `whenever Promise.in(5)` blocks
/// peaked at 420 concurrent `promise-wait` threads). `mark_observed` is what
/// `wait()` used to do: a Broken promise consumed by a `whenever` must not
/// also trip the destruction-time "unhandled" diagnostic. The waiter runs
/// either inline (an already-resolved promise, on this registered thread) or
/// on a pooled worker from `dispatch_waiters`; both are registered GC
/// mutators, which the `Value` clone in `send` requires.
// Cost: O(1) to register; the waiter sends one or two events.
pub(crate) fn promise_source_events(shared: &SharedPromise) -> SupplyReceiver {
    let (tx, rx) = supply_event_channel();
    shared.mark_observed();
    shared.on_resolve(Box::new(move |status, result, _output, _stderr| {
        if status == "Broken" {
            let _ = tx.send(SupplyEvent::Quit(
                crate::runtime::Interpreter::promise_cause_value(result),
            ));
        } else {
            let _ = tx.send(SupplyEvent::Emit(result));
            let _ = tx.send(SupplyEvent::Done);
        }
    }));
    rx
}
