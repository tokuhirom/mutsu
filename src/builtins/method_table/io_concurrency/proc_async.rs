//! The rows of `Proc::Async` (ADR-11276 §9.128): `start`, `kill`, `write`,
//! `print`, `put`, `say`, `close-stdin`, `bind-stdin`, `bind-stdout`,
//! `bind-stderr`, `ready`, `stdout`, `stderr`, `Supply`, `command`, `started`,
//! `w` and `pid`.
//!
//! A `Proc::Async` is an instance whose state is its shared attribute cell, so
//! a method that changes it (`start`, the binds, selecting an output Supply)
//! commits through that cell and no row needs a receiver place. The class has
//! an opaque dispatch shape (`DispatchShape::is_opaque`), so every call entry
//! reaches the rows through the table; a user subclass's instance, which
//! carries the state too, reaches them through its owner (`owners_of`).
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
            flags: RowFlags::SLURPY
                .or(RowFlags::ANY_ARGS)
                .or(RowFlags::OBJECT_ARGS),
            named: named_of($name),
        }
    };
}

/// The named arguments Rakudo's `Proc::Async` method `name` declares.
// Cost: O(1).
const fn named_of(name: &str) -> &'static [&'static str] {
    use crate::value::const_str_eq;
    if const_str_eq(name, "start") {
        &["ENV", "cwd", "scheduler"]
    } else if const_str_eq(name, "print") || const_str_eq(name, "write") {
        &["scheduler"]
    } else if const_str_eq(name, "stdout")
        || const_str_eq(name, "stderr")
        || const_str_eq(name, "Supply")
    {
        &["bin", "enc", "translate-nl"]
    } else {
        &[]
    }
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
