//! The instance rows of the socket classes (ADR-11276 §9.158):
//! `IO::Socket::INET`, a blocking TCP socket standing for an entry of the
//! interpreter's I/O handle table by its `handle` id, and `IO::Socket::Async`,
//! a TCP connection (`conn-id`) or a bound UDP socket (`udp-socket-id`).
//!
//! Each has an opaque dispatch shape (`DispatchShape::is_opaque`), so every
//! call entry reaches the rows through the table. Each handler is one `Interpreter`
//! method (`runtime/native_methods/socket_inet.rs`, `socket_async*.rs`).

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:expr) => {
        row!($name, $handler, RowFlags::NONE)
    };
    ($name:literal, $handler:expr, $flags:expr) => {
        row!("IO::Socket::INET", $name, $handler, $flags, &[])
    };
    ($owner:literal, $name:literal, $handler:expr, $flags:expr, $named:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: $handler,
            flags: $flags,
            named: $named,
        }
    };
}

/// An `IO::Socket::Async` row.
macro_rules! async_row {
    ($name:literal, $handler:expr) => {
        row!("IO::Socket::Async", $name, $handler, RowFlags::NONE, &[])
    };
    ($name:literal, $handler:expr, $flags:expr, $named:expr) => {
        row!("IO::Socket::Async", $name, $handler, $flags, $named)
    };
}

/// A row taking any positional arguments, which its handler reads itself.
const ARGS: RowFlags = RowFlags::SLURPY
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
    row!(
        "IO::Socket::INET",
        "recv",
        Handler::Interp(recv),
        ARGS,
        &["bin"]
    ),
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
    async_row!(
        "socket-port",
        Handler::Narrow(|t, _| field_or(t, "socket-port", || Value::int(0)))
    ),
    async_row!(
        "socket-host",
        Handler::Narrow(|t, _| field_or(t, "socket-host", || Value::str_from("0.0.0.0")))
    ),
    async_row!(
        "peer-port",
        Handler::Narrow(|t, _| field_or(t, "peer-port", || Value::int(0)))
    ),
    async_row!(
        "peer-host",
        Handler::Narrow(|t, _| field_or(t, "peer-host", || Value::str_from("0.0.0.0")))
    ),
    async_row!(
        "native-descriptor",
        Handler::Narrow(|t, _| Some(Ok(Interpreter::async_socket_native_descriptor(&attrs(t)?))))
    ),
    async_row!(
        "close",
        Handler::Interp(|i, t, _, _| Some(Ok(i.async_socket_close(&attrs(t)?))))
    ),
    async_row!(
        "Supply",
        Handler::Interp(|i, t, a, n| Some(i.async_socket_supply(&attrs(t)?, &with_named(a, n)))),
        ARGS,
        &["bin", "enc"]
    ),
    async_row!(
        "print",
        Handler::Interp(|i, t, a, n| Some(i.async_socket_send(
            &attrs(t)?,
            &with_named(a, n),
            true
        ))),
        ARGS,
        &["enc"]
    ),
    async_row!(
        "write",
        Handler::Interp(|i, t, a, n| Some(i.async_socket_send(
            &attrs(t)?,
            &with_named(a, n),
            false
        ))),
        ARGS,
        &[]
    ),
    async_row!(
        "print-to",
        Handler::Interp(|i, t, a, _| Some(i.async_socket_send_to(&attrs(t)?, a, true))),
        ARGS,
        &[]
    ),
    async_row!(
        "write-to",
        Handler::Interp(|i, t, a, _| Some(i.async_socket_send_to(&attrs(t)?, a, false))),
        ARGS,
        &[]
    ),
];

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

/// The receiver's attribute `name`, or `default` when it has none.
// Cost: O(a), a = attributes.
fn field_or(target: &Value, name: &str, default: fn() -> Value) -> Answer {
    Some(Ok(attrs(target)?
        .get(name)
        .cloned()
        .unwrap_or_else(default)))
}

/// The call's positional arguments followed by its named ones, the one list
/// the implementations read.
// Cost: O(a), a = arguments.
fn with_named(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    all
}

/// `recv($limit?, :bin)`.
// Cost: O(n), n = bytes received.
fn recv(interp: &mut Interpreter, target: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(interp.inet_recv(&attrs(target)?, &with_named(args, named)))
}
