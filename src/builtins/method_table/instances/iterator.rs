//! The rows of the `Iterator` and `PredictiveIterator` roles (ADR-11276
//! §9.126): `pull-one`, `push-exactly`, `push-at-least`, `push-all`,
//! `push-until-lazy`, `sink-all`, `skip-one`, `skip-at-least` and
//! `skip-at-least-pull-one` of `Iterator`, `count-only` and `bool-only` of
//! `PredictiveIterator`.
//!
//! mutsu's built-in iterators are instances of one class, `Iterator`, whose
//! state lives in the instance's shared attribute cell: an array-backed cursor
//! (`items`/`index`, topped up from a `lazy_source`), a deferred `.map`/`.grep`
//! stream, or a `squish` scan. Rakudo's are one class per kind, each doing
//! `Iterator` and, when it knows its count, `PredictiveIterator`; the opaque
//! shapes `DispatchShape::Iterator` and `PredictiveIterator` decode the kind
//! from the instance's attributes (`value/dispatch_shape/iterator_kind.rs`).
//! The rows are reached through those shapes' owners (`RowFlags::OWNER_ONLY`)
//! wherever a call on a built-in iterator lands. A protocol method writes the advanced cursor
//! back through the shared cell, so the receiver's place is never needed and no
//! row is a `Handler::Mut`.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::runtime::iterator_protocol::IterOp;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $op:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp({
                fn handler(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
                    i.builtin_iterator_call(t, IterOp::$op, args)
                }
                handler
            }),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Iterator", "pull-one", 0, PullOne),
    row!("Iterator", "push-exactly", 2, PushExactly),
    row!("Iterator", "push-at-least", 2, PushAtLeast),
    row!("Iterator", "push-all", 1, PushAll),
    row!("Iterator", "push-until-lazy", 1, PushUntilLazy),
    row!("Iterator", "sink-all", 0, SinkAll),
    row!("Iterator", "skip-one", 0, SkipOne),
    row!("Iterator", "skip-at-least", 1, SkipAtLeast),
    row!("Iterator", "skip-at-least-pull-one", 1, SkipAtLeastPullOne),
    row!("PredictiveIterator", "count-only", 0, CountOnly),
    row!("PredictiveIterator", "bool-only", 0, BoolOnly),
];
