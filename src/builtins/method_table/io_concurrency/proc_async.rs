//! The rows of `Proc::Async` (ADR-11276 §9.128): `start`, `kill`, `write`,
//! `print`, `put`, `say`, `close-stdin`, `bind-stdin`, `bind-stdout`,
//! `bind-stderr`, `ready`, `stdout`, `stderr`, `Supply`, `command`, `started`,
//! `w` and `pid`.
//!
//! A `Proc::Async` is an instance whose state is its shared attribute cell, so
//! a method that changes it (`start`, the binds, selecting an output Supply)
//! commits through that cell and no row needs a receiver place: they are all
//! pure-entry `OWNER_ONLY` rows, found by owner from the instance dispatch.
//! Their one implementation is `Interpreter::proc_async_call`
//! (`runtime/native_proc_async.rs`), which takes the method as a
//! [`ProcAsyncOp`]. The methods read their positional and named arguments as
//! one list (`start(:ENV, :cwd)`, `stdout(:bin)`), so the rows are slurpy and
//! the handler hands both back.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::native_proc_async::ProcAsyncOp;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $op:ident) => {
        MethodRow {
            owner: "Proc::Async",
            name: $name,
            arity: 0,
            handler: Handler::Interp({
                fn handler(
                    i: &mut Interpreter,
                    t: &Value,
                    args: &[Value],
                    named: Named<'_>,
                ) -> Answer {
                    call(i, t, ProcAsyncOp::$op, args, named)
                }
                handler
            }),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::SLURPY),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("start", Start),
    row!("kill", Kill),
    row!("write", Write),
    row!("print", Print),
    row!("put", Put),
    row!("say", Say),
    row!("close-stdin", CloseStdin),
    row!("bind-stdin", BindStdin),
    row!("bind-stdout", BindStdout),
    row!("bind-stderr", BindStderr),
    row!("ready", Ready),
    row!("stdout", Stdout),
    row!("stderr", Stderr),
    row!("Supply", Supply),
    row!("command", Command),
    row!("started", Started),
    row!("w", W),
    row!("pid", Pid),
];

/// `Proc::Async`'s rows for a `Proc::Async` instance; `None` for anything else.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "Proc::Async" => {
            Some(&["Proc::Async"])
        }
        _ => None,
    }
}

// Cost: the method's own.
fn call(
    interp: &mut Interpreter,
    target: &Value,
    op: ProcAsyncOp,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    Some(interp.proc_async_call(&attributes, op, all))
}
