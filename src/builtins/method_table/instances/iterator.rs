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
//! `Iterator` and, when it knows its count, `PredictiveIterator`; [`owners_of`]
//! names those roles for the instance's kind. The rows are reached through
//! their owner (`RowFlags::OWNER_ONLY`, `invoke_owner`) wherever a call on a
//! built-in iterator lands: the method-call fallback, the mutating method call
//! and the VM's `CallMethodMut`. A protocol method writes the advanced cursor
//! back through the shared cell, so the receiver's place is never needed and no
//! row is a `Handler::Mut`.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::runtime::iterator_map_grep_stream::MAP_GREP_STREAM_ATTR;
use crate::runtime::iterator_protocol::IterOp;
use crate::value::{RuntimeError, Value, ValueView};

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

/// The roles a built-in iterator does: `PredictiveIterator` and `Iterator` for
/// an array-backed one that knows how many elements it has left, `Iterator`
/// alone for a stream (Rakudo's map iterator), a `squish` scan
/// (`Any::Squish-With`) and an iterator over a lazy source with no known count
/// (a `gather`, an unending range). `None` for anything else.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    if class_name != "Iterator" {
        return None;
    }
    let map = attributes.as_map();
    let predictive = matches!(
        map.get("items").map(Value::view),
        Some(ValueView::Array(..))
    ) && !map.contains_key(MAP_GREP_STREAM_ATTR)
        && !map.contains_key("squish_source")
        && (map.contains_key("known_count") || !map.contains_key("lazy_source"));
    Some(if predictive {
        &["PredictiveIterator", "Iterator"]
    } else {
        &["Iterator"]
    })
}
