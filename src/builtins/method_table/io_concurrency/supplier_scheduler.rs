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
//! reading copy without the serialize guard of `Supply.serialize`).
//! `Supplier`'s rows are `OWNER_ONLY`, found by owner from the instance
//! dispatch and the mutating method call; a scheduler has an opaque dispatch
//! shape (`DispatchShape::is_opaque`), so every call entry reaches its rows
//! through the table.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $mutating:literal) => {
        row!($owner, $name, $arity, $flags, $mutating, named: &[])
    };
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $mutating:literal, named: $named:expr) => {
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
            named: $named,
        }
    };
}

/// The named arguments of a scheduler's `cue`.
const CUE_NAMED: &[&str] = &["at", "catch", "every", "in", "stop", "times"];

const SLURPY: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::SLURPY);
const ONE: RowFlags = RowFlags::OWNER_ONLY;
/// A scheduler's rows: its class has an opaque shape, so the table answers
/// them from every entry; the handler reads its arguments itself.
const SCHED_SLURPY: RowFlags = RowFlags::SLURPY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS);
const SCHED_ONE: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS);

pub(super) static ROWS: &[MethodRow] = &[
    row!("Supplier", "emit", 0, SLURPY, true),
    row!("Supplier", "done", 0, SLURPY, true),
    row!("Supplier", "quit", 0, SLURPY, true),
    row!("Supplier", "Supply", 0, ONE, false),
    row!("ThreadPoolScheduler", "cue", 0, SCHED_SLURPY, false, named: CUE_NAMED),
    row!("ThreadPoolScheduler", "loads", 0, SCHED_ONE, false),
    row!("ThreadPoolScheduler", "max_threads", 0, SCHED_ONE, false),
    row!("ThreadPoolScheduler", "usage", 0, SCHED_ONE, false),
    // The getter reads the handler; the setter (one argument) installs it.
    row!(
        "ThreadPoolScheduler",
        "uncaught_handler",
        0,
        SCHED_ONE,
        false
    ),
    row!(
        "ThreadPoolScheduler",
        "uncaught_handler",
        1,
        SCHED_ONE,
        true
    ),
    row!("CurrentThreadScheduler", "cue", 0, SCHED_SLURPY, false, named: CUE_NAMED),
    row!("CurrentThreadScheduler", "loads", 0, SCHED_ONE, false),
    row!(
        "CurrentThreadScheduler",
        "uncaught_handler",
        0,
        SCHED_ONE,
        false
    ),
    row!(
        "CurrentThreadScheduler",
        "uncaught_handler",
        1,
        SCHED_ONE,
        true
    ),
];

/// The attribute marking an instance of a user subclass of `Supplier`: it
/// carries a supplier's state (seeded at construction, as `Promise`'s and
/// `Channel`'s subclasses carry their payloads), so `Supplier`'s rows answer
/// it like any supplier.
pub(crate) const SUPPLIER_SUBCLASS: &str = "__mutsu_supplier_subclass";
