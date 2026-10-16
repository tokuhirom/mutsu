//! The rows of `Supplier` (and `Supplier::Preserving`, which inherits them),
//! `ThreadPoolScheduler` and `CurrentThreadScheduler` (ADR-11276 §9.133).
//!
//! Their implementations are the native dispatchers' (`native_supplier_mut`,
//! `native_supplier`, `native_scheduler`, `native_scheduler_mut`). A row says
//! which one it runs: a mutating row works on a copy of the instance's
//! attributes committed through its shared cell, a reading row reads them.
//! Before, the receiver picked: a variable took the mutating path first and a
//! temporary or an attribute (`$!supplier.emit`) the reading one, so the two
//! copies of `emit`, `done` and `quit` answered different receivers (the
//! reading copy without the serialize guard of `Supply.serialize`). The rows are
//! `OWNER_ONLY`, found by owner from the instance dispatch and the mutating
//! method call.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $mutating:literal) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp({
                fn handler(
                    i: &mut Interpreter,
                    t: &Value,
                    args: &[Value],
                    named: Named<'_>,
                ) -> Answer {
                    let mut all = args.to_vec();
                    all.extend(named.pairs().iter().cloned());
                    i.native_owner_call(t, $name, all, $mutating)
                }
                handler
            }),
            flags: $flags,
            named: &[],
        }
    };
}

const SLURPY: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::SLURPY);
const ONE: RowFlags = RowFlags::OWNER_ONLY;

pub(super) static ROWS: &[MethodRow] = &[
    row!("Supplier", "emit", 0, SLURPY, true),
    row!("Supplier", "done", 0, SLURPY, true),
    row!("Supplier", "quit", 0, SLURPY, true),
    row!("Supplier", "Supply", 0, ONE, false),
    row!("ThreadPoolScheduler", "cue", 0, SLURPY, false),
    row!("ThreadPoolScheduler", "loads", 0, ONE, false),
    row!("ThreadPoolScheduler", "max_threads", 0, ONE, false),
    row!("ThreadPoolScheduler", "usage", 0, ONE, false),
    // The getter reads the handler; the setter (one argument) installs it.
    row!("ThreadPoolScheduler", "uncaught_handler", 0, ONE, false),
    row!("ThreadPoolScheduler", "uncaught_handler", 1, ONE, true),
    row!("CurrentThreadScheduler", "cue", 0, SLURPY, false),
    row!("CurrentThreadScheduler", "loads", 0, ONE, false),
    row!("CurrentThreadScheduler", "uncaught_handler", 0, ONE, false),
    row!("CurrentThreadScheduler", "uncaught_handler", 1, ONE, true),
];

/// The owner whose rows answer `target`: `Supplier` for a supplier (a
/// `Supplier::Preserving` included), the scheduler's own class for a scheduler;
/// `None` for anything else.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    Some(match class_name.as_str() {
        "Supplier" | "Supplier::Preserving" => &["Supplier"],
        "ThreadPoolScheduler" => &["ThreadPoolScheduler"],
        "CurrentThreadScheduler" => &["CurrentThreadScheduler"],
        _ => return None,
    })
}
