//! The rows of `Tap`, `IO::Socket::Async::ListenSocket` (the `Tap` a
//! listening socket's supply answers) and `Proc` (ADR-11276 §9.157).
//!
//! None of them has a shape, so every row is reached through its owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`). `Proc.spawn` and `Proc.shell`
//! re-run a command and commit what it changed through the instance's shared
//! attribute cell.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::native_methods::ProcStream;
use crate::value::{ArrayKind, AttrMap, RuntimeError, Value, ValueView};

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

/// `spawn` and `shell` take the command's words and its adverbs.
const COMMAND_ARGS: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS)
    .or(RowFlags::ANY_NAMED);

pub(super) static ROWS: &[MethodRow] = &[
    row!("Tap", "close", Handler::Interp(close)),
    row!(
        "IO::Socket::Async::ListenSocket",
        "socket-port",
        Handler::Narrow(|t, _| promised(t, "socket-port"))
    ),
    row!(
        "IO::Socket::Async::ListenSocket",
        "socket-host",
        Handler::Narrow(|t, _| promised(t, "socket-host"))
    ),
    row!(
        "Proc",
        "exitcode",
        Handler::Narrow(|t, _| stream(t, ProcStream::Exitcode, "exitcode"))
    ),
    row!("Proc", "signal", Handler::Narrow(signal)),
    row!(
        "Proc",
        "out",
        Handler::Narrow(|t, _| stream(t, ProcStream::Out, "out"))
    ),
    row!(
        "Proc",
        "err",
        Handler::Narrow(|t, _| stream(t, ProcStream::Err, "err"))
    ),
    row!(
        "Proc",
        "in",
        Handler::Narrow(|t, _| stream(t, ProcStream::In, "in"))
    ),
    row!("Proc", "pid", Handler::Narrow(|t, _| field(t, "pid"))),
    row!("Proc", "command", Handler::Narrow(command)),
    row!("Proc", "Numeric", Handler::Narrow(numeric)),
    row!("Proc", "Bool", Handler::Narrow(truth)),
    row!(
        "Proc",
        "spawn",
        Handler::Interp(|i, t, a, n| respawn(i, t, a, n, false)),
        COMMAND_ARGS
    ),
    row!(
        "Proc",
        "shell",
        Handler::Interp(|i, t, a, n| respawn(i, t, a, n, true)),
        COMMAND_ARGS
    ),
];

/// The owners an instance of `class` is answered from.
// Cost: O(1).
pub(super) fn owners_of(class: &str) -> Option<&'static [&'static str]> {
    Some(match class {
        "Tap" => &["Tap"],
        "IO::Socket::Async::ListenSocket" => &["IO::Socket::Async::ListenSocket", "Tap"],
        "Proc" => &["Proc"],
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

/// The receiver's attribute `name`, `Nil` when unset.
// Cost: O(a), a = attributes.
fn field(target: &Value, name: &str) -> Answer {
    Some(Ok(attrs(target)?.get(name).cloned().unwrap_or(Value::NIL)))
}

// Cost: O(u + c), u = upstream taps, c = CLOSE phasers.
fn close(interp: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(interp.tap_close(&attrs(target)?))
}

/// `socket-port` / `socket-host`: a `Promise` kept with the bound address.
// Cost: O(a), a = attributes.
fn promised(target: &Value, name: &str) -> Answer {
    Some(Ok(attrs(target)?.get(name).cloned().unwrap_or_else(|| {
        Value::promise(crate::value::SharedPromise::new())
    })))
}

/// A stream or status a live proc's reaping decides, else the attribute.
// Cost: O(o), o = output collected when a live child is reaped.
fn stream(target: &Value, which: ProcStream, name: &str) -> Answer {
    let attrs = attrs(target)?;
    Some(Ok(Interpreter::proc_live_stream(&attrs, which)
        .unwrap_or_else(|| {
            attrs.get(name).cloned().unwrap_or(Value::NIL)
        })))
}

/// `Proc.signal`: the signal that ended the child, 0 when none did. An un-run
/// `Proc.new` seeds the attribute with its `Any` default (so `.raku` renders
/// `signal => Any`); the getter reports 0 there.
// Cost: O(o), o = output collected when a live child is reaped.
fn signal(target: &Value, _: &[Value]) -> Answer {
    let attrs = attrs(target)?;
    if let Some(live) = Interpreter::proc_live_stream(&attrs, ProcStream::Signal) {
        return Some(Ok(live));
    }
    Some(Ok(match attrs.get("signal").map(Value::view) {
        Some(ValueView::Int(n)) => Value::int(n),
        _ => Value::int(0),
    }))
}

/// `Proc.command`, a `List`.
// Cost: O(w), w = words of the command.
fn command(target: &Value, _: &[Value]) -> Answer {
    let cmd = attrs(target)?
        .get("command")
        .cloned()
        .unwrap_or_else(|| Value::array(Vec::new()));
    Some(Ok(match cmd.view() {
        ValueView::Array(items, _) => Value::array_with_kind(items.clone(), ArrayKind::List),
        _ => cmd,
    }))
}

/// The exit code, -1 when the child has not run.
// Cost: O(a), a = attributes.
fn exit_code(attrs: &AttrMap) -> i64 {
    match attrs.get("exitcode").map(Value::view) {
        Some(ValueView::Int(c)) => c,
        _ => -1,
    }
}

// Cost: O(a), a = attributes.
fn numeric(target: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::int(exit_code(&attrs(target)?))))
}

/// `Proc.Bool`: whether the child succeeded. A signal-killed child reports
/// `exitcode = 0`, so the signal has to be consulted too (rakudo:
/// `$!exitcode == 0 && $!signal == 0`).
// Cost: O(a), a = attributes.
fn truth(target: &Value, _: &[Value]) -> Answer {
    let attrs = attrs(target)?;
    let signal = match attrs.get("signal").map(Value::view) {
        Some(ValueView::Int(s)) => s,
        _ => 0,
    };
    Some(Ok(Value::truth(exit_code(&attrs) == 0 && signal == 0)))
}

// Cost: the child's run, plus O(a), a = attributes updated.
fn respawn(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
    shell: bool,
) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    Some(interp.proc_respawn(&all, shell).map(|(result, updates)| {
        attributes.write_keys(updates);
        result
    }))
}
