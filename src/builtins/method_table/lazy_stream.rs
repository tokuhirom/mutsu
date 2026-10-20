//! What a method does to an unbounded receiver it can stream: its *stream*
//! (ADR-13891 §4).
//!
//! A pull (`lazy_pull`) answers a `LazyList` from what it pulls, or refuses
//! it. A stream is the other half of the lazy step: it answers a receiver
//! that may not end with a lazy stage over it, so nothing is pulled until the
//! stage is (`(1..*).skip(2)`, `gather { ... }.batch(3)`, `(^Inf).pairs`).
//! Streams serve two receivers:
//!
//! - a genuinely lazy `LazyList` (a lazy pipe, a gather, an infinite
//!   sequence), looked up on the MRO of the type it reports, as its pulls
//!   are;
//! - an unbounded `Range` (`1..*`, `^Inf`, `"a"..*`), looked up on `Range`'s
//!   MRO.
//!
//! They are a table of their own, not more pulls, because a `Range` answers
//! the counting and sampling methods itself (`(1..*).sum` is `Inf`,
//! `(1..*).elems` throws only for a `LazyList`): the pulls' lazy refusals,
//! registered on `Any`, must not reach it. A stream is registered on the
//! owner Rakudo declares the method on (`streams_are_declared_by_rakudo`), so
//! `Range.flat`, which Rakudo declares on `Range` itself, finds no stream.

use crate::runtime::Interpreter;
use crate::value::{IndexTransform, LazyList, Value, ValueView};

/// A stream: the interpreter, the receiver, the call's arguments. `None`
/// declines (an argument form only the eager implementation understands).
pub(crate) type StreamFn = fn(&mut Interpreter, &Value, &[Value]) -> Option<Value>;

/// One method's stream on one owner.
pub(crate) struct LazyStream {
    /// The type Rakudo declares the method on.
    pub(crate) owner: &'static str,
    pub(crate) name: &'static str,
    pub(crate) stream: StreamFn,
}

macro_rules! streams {
    ($($owner:literal . $name:literal => $stream:path),* $(,)?) => {
        &[$(LazyStream { owner: $owner, name: $name, stream: $stream }),*]
    };
}

/// Every stream, in no particular order: the walk asks each owner in turn.
pub(crate) static STREAMS: &[LazyStream] = streams![
    "Any"."skip" => Interpreter::stream_skip,
    "Any"."rotor" => Interpreter::stream_rotor,
    "Any"."batch" => Interpreter::stream_batch,
    "Any"."unique" => Interpreter::stream_unique,
    "Any"."repeated" => Interpreter::stream_repeated,
    "Any"."squish" => Interpreter::stream_squish,
    "Any"."produce" => Interpreter::stream_produce,
    // Rakudo declares `flat` on `Iterable`, which mutsu's catalogue MROs do
    // not list; `Seq` and `List` (`Array` inherits it) are where its rows
    // are.
    "Seq"."flat" => Interpreter::stream_flat,
    "List"."flat" => Interpreter::stream_flat,
    "Any"."pairs" => pairs,
    "Any"."antipairs" => antipairs,
    "Any"."kv" => kv,
    "Any"."keys" => keys,
];

/// The owners `target`'s streams are looked up on, if it is a receiver a
/// stream serves: a genuinely lazy `LazyList` (the MRO of the type it
/// reports) or an unbounded `Range`.
// Cost: O(1).
fn streamed_owners(target: &Value) -> Option<&'static [&'static str]> {
    const RANGE: &[&str] = match crate::builtin_types::catalog::builtin_type_mro_strs("Range") {
        Some(mro) => mro,
        None => panic!("Range is catalogued"),
    };
    match target.view() {
        ValueView::LazyList(ll) => (ll.is_genuinely_lazy()
            && (ll.lazy_pipe.is_some() || ll.needs_vm_lazy_dispatch()))
        .then(|| super::lazy_pull::owners_of(target)),
        _ if Interpreter::is_lazy_pipe_source(target) => Some(RANGE),
        _ => None,
    }
}

/// The stream `method` has on `target`, if `target` is a receiver streams
/// serve and the first owner of its MRO that has one for `method` does.
// Cost: O(o * s), o = owners in the MRO (at most 5), s = registered streams.
pub(crate) fn stream_of(target: &Value, method: &str) -> Option<StreamFn> {
    streamed_owners(target)?.iter().find_map(|&owner| {
        STREAMS
            .iter()
            .find(|entry| entry.owner == owner && entry.name == method)
            .map(|entry| entry.stream)
    })
}

/// A zero-argument index view (`pairs`, `antipairs`, `kv`, `keys`) as a lazy
/// index-pipe stage over the receiver.
// Cost: O(1).
fn index_pipe(target: &Value, args: &[Value], transform: IndexTransform) -> Option<Value> {
    if !args.is_empty() {
        return None;
    }
    Some(Value::lazy_list(crate::gc::Gc::new(
        LazyList::new_index_pipe(target.clone(), transform),
    )))
}

// Cost: O(1).
fn pairs(_: &mut Interpreter, target: &Value, args: &[Value]) -> Option<Value> {
    index_pipe(target, args, IndexTransform::Pairs)
}

// Cost: O(1).
fn antipairs(_: &mut Interpreter, target: &Value, args: &[Value]) -> Option<Value> {
    index_pipe(target, args, IndexTransform::AntiPairs)
}

// Cost: O(1).
fn kv(_: &mut Interpreter, target: &Value, args: &[Value]) -> Option<Value> {
    index_pipe(target, args, IndexTransform::Kv)
}

// Cost: O(1).
fn keys(_: &mut Interpreter, target: &Value, args: &[Value]) -> Option<Value> {
    index_pipe(target, args, IndexTransform::Keys)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every stream is registered on an owner Rakudo declares the method on.
    #[test]
    fn streams_are_declared_by_rakudo() {
        let undeclared: Vec<String> = STREAMS
            .iter()
            .filter(|s| {
                !crate::builtins::method_introspection::native_method_declared(s.owner, s.name)
            })
            .map(|s| format!("{}.{}", s.owner, s.name))
            .collect();
        assert!(
            undeclared.is_empty(),
            "streams Rakudo does not declare: {undeclared:?}"
        );
    }

    /// One stream per `(owner, name)`: the walk takes the first.
    #[test]
    fn streams_are_unique() {
        for (i, a) in STREAMS.iter().enumerate() {
            assert!(
                !STREAMS[i + 1..]
                    .iter()
                    .any(|b| a.owner == b.owner && a.name == b.name),
                "duplicate stream {}.{}",
                a.owner,
                a.name
            );
        }
    }
}
