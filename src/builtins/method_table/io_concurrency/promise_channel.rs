//! The rows of `Promise`, `Promise::Vow` and `Channel` (ADR-11276 §9.118).
//!
//! A promise and a channel are native value kinds (`ValueView::Promise`,
//! `ValueView::Channel`) shared between threads; a vow is a `Promise::Vow`
//! instance holding its promise. None has a dispatch shape: the rows are reached
//! through their owner (`RowFlags::OWNER_ONLY`, `invoke_owner`) from the place in
//! `call_method_with_values` that answered them by name before
//! ([`owners_of`]), and a vow's from its native methods (`native_promise_vow`).
//! The implementations are `Interpreter` methods in `runtime/methods_promise.rs`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, SharedChannel, SharedPromise, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A row of `$owner` reached through its owner, needing the interpreter.
macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Promise", "get-await-handle", 0, promise_await_handle),
    row!("Promise", "result", 0, promise_result),
    row!("Promise", "status", 0, promise_status),
    row!("Promise", "scheduler", 0, promise_scheduler),
    row!("Promise", "then", 1, promise_then),
    row!("Promise", "andthen", 1, promise_andthen),
    row!("Promise", "orelse", 1, promise_orelse),
    row!("Promise", "keep", 0, promise_keep),
    row!("Promise", "keep", 1, promise_keep),
    row!("Promise", "break", 0, promise_break),
    row!("Promise", "break", 1, promise_break),
    row!("Promise", "cause", 0, promise_cause),
    row!("Promise", "Bool", 0, promise_bool),
    row!("Promise", "vow", 0, promise_vow),
    row!("Promise::Vow", "keep", 0, vow_keep),
    row!("Promise::Vow", "keep", 1, vow_keep),
    row!("Promise::Vow", "break", 0, vow_break),
    row!("Promise::Vow", "break", 1, vow_break),
    row!("Channel", "get-await-handle", 0, channel_await_handle),
    row!("Channel", "send", 1, channel_send),
    row!("Channel", "receive", 0, channel_receive),
    row!("Channel", "poll", 0, channel_poll),
    row!("Channel", "close", 0, channel_close),
    row!("Channel", "fail", 0, channel_fail),
    row!("Channel", "fail", 1, channel_fail),
    row!("Channel", "list", 0, channel_list),
    row!("Channel", "closed", 0, channel_closed),
    MethodRow {
        owner: "Channel",
        name: "elems",
        arity: 0,
        handler: Handler::Narrow(channel_elems),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    row!("Channel", "Supply", 0, channel_supply),
];

/// `Any`'s list coercions (`self.list.METHOD`) a `Channel` inherits: the
/// interpreter answers them as `Interpreter::channel_list_coercion`.
pub(crate) const CHANNEL_LIST_COERCIONS: [&str; 3] = ["Seq", "List", "Array"];

/// The attribute an instance of a user subclass of `Promise` keeps its native
/// promise in (#13634), as `is Pair` subclasses keep their `Pair`
/// (`builtins::native_base`).
pub(crate) const PROMISE_PAYLOAD: &str = "__mutsu_promise_value";

/// The attribute an instance of a user subclass of `Channel` keeps its native
/// channel in (#13634).
pub(crate) const CHANNEL_PAYLOAD: &str = "__mutsu_channel_value";

/// The owners a promise's or a channel's rows are found under.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Promise(_) => Some(&["Promise"]),
        ValueView::Channel(_) => Some(&["Channel"]),
        _ => None,
    }
}

/// The value the rows answer `target` on: a promise or a channel itself, or
/// the native promise or channel an instance of a user subclass carries
/// ([`PROMISE_PAYLOAD`], [`CHANNEL_PAYLOAD`]): Rakudo's subclass inherits
/// every method of its base. `None` for any other value.
// Cost: O(1), plus one attribute read for an instance.
pub(crate) fn receiver_of(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Promise(_) | ValueView::Channel(_) => Some(target.clone()),
        ValueView::Instance { attributes, .. } => {
            let attributes = attributes.as_map();
            attributes
                .get(PROMISE_PAYLOAD)
                .or_else(|| attributes.get(CHANNEL_PAYLOAD))
                .cloned()
        }
        _ => None,
    }
}

/// The receiver's promise, marked observed (an unobserved broken promise
/// warns when it is collected).
// Cost: O(1).
fn promise(target: &Value) -> Option<SharedPromise> {
    let ValueView::Promise(shared) = target.view() else {
        return None;
    };
    shared.mark_observed();
    Some(shared.clone())
}

/// The promise a vow holds.
// Cost: O(1).
fn vowed(target: &Value) -> Option<SharedPromise> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let promise = attributes.as_map().get("promise")?.clone();
    let ValueView::Promise(shared) = promise.view() else {
        return None;
    };
    Some(shared.clone())
}

/// The receiver's channel.
// Cost: O(1).
fn channel(target: &Value) -> Option<SharedChannel> {
    let ValueView::Channel(ch) = target.view() else {
        return None;
    };
    Some(ch.clone())
}

/// `Promise.get-await-handle`: a promise is its own native await handle, so a
/// user `Awaitable` class can delegate to one as it does on Rakudo.
// Cost: O(1).
fn promise_await_handle(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::promise(promise(t)?)))
}

/// `Promise.result`.
// Cost: O(1) after the wait.
fn promise_result(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let shared = promise(t)?;
    Some(i.promise_result(&shared))
}

/// `Promise.status`: a `PromiseStatus` value.
// Cost: O(1).
fn promise_status(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let shared = promise(t)?;
    Some(Ok(Interpreter::promise_status_value(&shared.status())))
}

/// `Promise.scheduler`.
// Cost: O(1).
fn promise_scheduler(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let shared = promise(t)?;
    Some(Ok(i.promise_scheduler_value(&shared)))
}

/// `Promise.then` / `.andthen` / `.orelse` (`Interpreter::promise_chain`).
// Cost: O(1) excluding the callback, which runs user code.
fn chain(i: &mut Interpreter, t: &Value, method: &str, args: &[Value]) -> Answer {
    let shared = promise(t)?;
    let block = args.first().cloned().unwrap_or(Value::NIL);
    Some(i.promise_chain(&shared, method, block))
}

/// `Promise.then(&code)`: runs the block whatever the status.
// Cost: O(1) excluding the callback, which runs user code.
fn promise_then(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    chain(i, t, "then", args)
}

/// `Promise.andthen(&code)`: runs the block on a kept promise.
// Cost: O(1) excluding the callback, which runs user code.
fn promise_andthen(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    chain(i, t, "andthen", args)
}

/// `Promise.orelse(&code)`: runs the block on a broken promise.
// Cost: O(1) excluding the callback, which runs user code.
fn promise_orelse(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    chain(i, t, "orelse", args)
}

/// `Promise.keep($value?)`.
// Cost: O(s), s = the resolution's subscribers.
fn promise_keep(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let shared = promise(t)?;
    Some(i.promise_keep_or_break(&shared, true, args.first().cloned()))
}

/// `Promise.break($reason?)`.
// Cost: O(s), s = the resolution's subscribers.
fn promise_break(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let shared = promise(t)?;
    Some(i.promise_keep_or_break(&shared, false, args.first().cloned()))
}

/// `Promise.cause`.
// Cost: O(1).
fn promise_cause(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let shared = promise(t)?;
    Some(Interpreter::promise_cause(&shared))
}

/// `Promise.Bool`: whether the promise is resolved.
// Cost: O(1).
fn promise_bool(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::truth(promise(t)?.is_resolved())))
}

/// `Promise.vow`.
// Cost: O(1).
fn promise_vow(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let shared = promise(t)?;
    Some(Interpreter::promise_vow(&shared, t))
}

/// `Promise::Vow.keep($value?)`: holding the vow is the permission, so only an
/// already-settled promise is an error.
// Cost: O(s), s = the resolution's subscribers.
fn vow_keep(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let shared = vowed(t)?;
    Some(i.promise_resolve_through_vow(&shared, true, args.first().cloned()))
}

/// `Promise::Vow.break($reason?)`.
// Cost: O(s), s = the resolution's subscribers.
fn vow_break(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let shared = vowed(t)?;
    Some(i.promise_resolve_through_vow(&shared, false, args.first().cloned()))
}

/// `Channel.get-await-handle`.
// Cost: O(1).
fn channel_await_handle(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::channel(channel(t)?)))
}

/// `Channel.send($value)`.
// Cost: O(v * t), v = values the pump moves, t = the channel's taps.
fn channel_send(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let ch = channel(t)?;
    Some(i.channel_send(&ch, args.first().cloned().unwrap_or(Value::NIL)))
}

/// `Channel.receive`.
// Cost: O(1) after the wait.
fn channel_receive(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Interpreter::channel_receive(&channel(t)?))
}

/// `Channel.poll`: the next value if one is queued, else `Nil`.
// Cost: O(1).
fn channel_poll(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(channel(t)?
        .poll_result()
        .ok()
        .flatten()
        .unwrap_or(Value::NIL)))
}

/// `Channel.close`.
// Cost: O(v * t), v = values still queued, t = the channel's taps.
fn channel_close(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let ch = channel(t)?;
    Some(i.channel_close(&ch, None))
}

/// `Channel.fail($reason?)`.
// Cost: O(v * t), v = values still queued, t = the channel's taps.
fn channel_fail(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let ch = channel(t)?;
    let reason = args
        .first()
        .cloned()
        .unwrap_or_else(|| Value::str_from("Died"));
    Some(i.channel_close(&ch, Some(reason)))
}

/// `Channel.list`.
// Cost: O(1).
fn channel_list(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Interpreter::channel_list(&channel(t)?)))
}

/// `Channel.closed`: a promise kept once the channel is closed.
// Cost: O(1).
fn channel_closed(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::promise(channel(t)?.closed_promise())))
}

/// `Channel.elems`: a channel has no element count (Rakudo's message).
// Cost: O(1).
pub(crate) fn channel_elems(t: &Value, _: &[Value]) -> Answer {
    channel(t)?;
    Some(Err(RuntimeError::new(
        "Cannot determine number of elements on a Channel",
    )))
}

/// `Channel.Supply`: an on-demand supply whose taps compete for the queue.
// Cost: O(1).
fn channel_supply(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Interpreter::make_channel_supply(&channel(t)?)))
}
