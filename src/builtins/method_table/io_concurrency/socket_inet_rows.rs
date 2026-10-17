//! The rows of `IO::Socket::INET` (ADR-11276 §9.158): a blocking TCP socket,
//! which stands for an entry of the interpreter's I/O handle table by its
//! `handle` id.
//!
//! It has no shape, so every row is reached through its owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`). Each handler is one `Interpreter`
//! method (`runtime/native_methods/socket_inet.rs`).

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:expr) => {
        row!($name, $handler, RowFlags::OWNER_ONLY)
    };
    ($name:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: "IO::Socket::INET",
            name: $name,
            arity: 0,
            handler: $handler,
            flags: $flags,
            named: &[],
        }
    };
}

/// A row taking any positional arguments, which its handler reads itself.
const ARGS: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS);

pub(super) static ROWS: &[MethodRow] = &[
    // Rakudo has no `getpeername`, but roast pins it
    // (`S16-unfiled/getpeername.t`), so it stays a row.
    row!(
        "getpeername",
        Handler::Interp(|i, t, _, _| Some(i.inet_getpeername(&attrs(t)?)))
    ),
    row!(
        "close",
        Handler::Interp(|i, t, _, _| Some(i.inet_close(&attrs(t)?)))
    ),
    row!("localport", Handler::Narrow(localport)),
    row!(
        "accept",
        Handler::Interp(|i, t, _, _| Some(i.inet_accept(&attrs(t)?)))
    ),
    row!(
        "print",
        Handler::Interp(|i, t, a, _| Some(i.inet_print(&attrs(t)?, a, false))),
        ARGS
    ),
    row!(
        "put",
        Handler::Interp(|i, t, a, _| Some(i.inet_print(&attrs(t)?, a, true))),
        ARGS
    ),
    row!(
        "write",
        Handler::Interp(|i, t, a, _| Some(i.inet_write(&attrs(t)?, a))),
        ARGS
    ),
    MethodRow {
        owner: "IO::Socket::INET",
        name: "recv",
        arity: 0,
        handler: Handler::Interp(recv),
        flags: ARGS,
        named: &["bin"],
    },
    row!(
        "read",
        Handler::Interp(|i, t, a, _| Some(i.inet_read(&attrs(t)?, a))),
        ARGS
    ),
    row!(
        "get",
        Handler::Interp(|i, t, _, _| Some(i.inet_get(&attrs(t)?)))
    ),
    row!(
        "lines",
        Handler::Interp(|i, t, _, _| Some(i.inet_lines(&attrs(t)?)))
    ),
    row!(
        "nl-in",
        Handler::Interp(|i, t, _, _| Some(i.inet_nl_in(&attrs(t)?)))
    ),
];

/// The owners an instance of `class` is answered from.
// Cost: O(1).
pub(super) fn owners_of(class: &str) -> Option<&'static [&'static str]> {
    match class {
        "IO::Socket::INET" => Some(&["IO::Socket::INET"]),
        _ => None,
    }
}

/// A snapshot of the receiver's attributes.
// Cost: O(a), a = attributes.
fn attrs(target: &Value) -> Option<AttrMap> {
    match target.view() {
        ValueView::Instance { attributes, .. } => Some(attributes.to_map()),
        _ => None,
    }
}

/// `localport`: the port the socket is bound to.
// Cost: O(a), a = attributes.
fn localport(target: &Value, _: &[Value]) -> Answer {
    Some(Ok(attrs(target)?
        .get("localport")
        .cloned()
        .unwrap_or(Value::int(0))))
}

/// `recv($limit?, :bin)`.
// Cost: O(n), n = bytes received.
fn recv(interp: &mut Interpreter, target: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    Some(interp.inet_recv(&attrs(target)?, &all))
}
