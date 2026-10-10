//! `Any` rows whose handlers need the interpreter (`Handler::Interp`).
//!
//! `collate` reads the dynamic `$*COLLATION`; the iteration methods (`map`,
//! `grep`, `first`, `reduce`, `produce`, `rotor`, `skip`, `squish`, `eager`,
//! `iterator`, `match`, `classify`, `categorize`) run user code through the
//! interpreter. Each handler calls the one `Interpreter::dispatch_*_method` the
//! cascade's arm calls too (ADR-11276 slice 3C remainder), after declining the
//! receivers `try_native_method_raw` deferred to the interpreter by name.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

const ARGS: RowFlags = RowFlags::ANY_ARGS;
/// `match` binds the regex adverbs (`:g`, `:ov`, `:x`, ...), which are open-ended.
const ARGS_NAMED: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::ANY_NAMED);

macro_rules! any_row {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident) => {
        any_row!($owner, $name, $arity, $flags, $handler, &[])
    };
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident, $named:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: $named,
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    any_row!("Any", "collate", 0, RowFlags::NONE, collate),
    any_row!("Any", "map", 1, ARGS, map),
    any_row!("Any", "grep", 1, ARGS, grep, &["k", "v", "kv", "p"]),
    any_row!("Any", "first", 0, ARGS, first, &["k", "v", "kv", "p", "end"]),
    any_row!("Any", "first", 1, ARGS, first, &["k", "v", "kv", "p", "end"]),
    any_row!("Any", "reduce", 1, ARGS, reduce),
    any_row!("Any", "produce", 1, ARGS, produce),
    any_row!("Any", "iterator", 0, RowFlags::NONE, iterator),
    any_row!("Any", "eager", 0, RowFlags::NONE, eager),
    any_row!("Any", "squish", 0, ARGS, squish, &["as", "with"]),
    any_row!("Any", "rotor", 0, ARGS.or(RowFlags::SLURPY), rotor, &["partial"]),
    any_row!("Any", "skip", 0, ARGS.or(RowFlags::SLURPY), skip),
    any_row!("Any", "match", 1, ARGS_NAMED, match_),
    any_row!("Any", "classify", 1, ARGS, classify, &["as", "into"]),
    any_row!("Any", "categorize", 1, ARGS, categorize, &["as", "into"]),
    any_row!("Hash", "classify-list", 2, ARGS, classify_list, &["as"]),
    any_row!("Hash", "categorize-list", 2, ARGS, categorize_list, &["as"]),
];

/// The call's arguments as the cascades take them: the positional ones, then
/// the named ones as string-keyed pairs.
// Cost: O(a), a = arguments of the call.
fn joined(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

/// Whether `try_native_method_raw` deferred this call to the interpreter
/// before it reached the pure cascades: a receiver whose elements live behind
/// a user `iterator`, an `IterationBuffer`, or a lazy source the interpreter
/// turns into a pipeline stage. A row runs in front of those probes, so it
/// declines the same calls.
// Cost: O(1) tag probes; O(m) method-table lookups for an instance, m = MRO length.
fn deferred_to_interpreter(interp: &mut Interpreter, target: &Value, name: &str) -> bool {
    // An associative or quantity receiver reads as its pairs, which the pure
    // cascades build for `squish`/`eager`/`produce` and the interpreter's own
    // routines do not.
    if !matches!(name, "map" | "grep" | "first") {
        return matches!(name, "squish" | "eager" | "produce")
            && matches!(
                target.view(),
                ValueView::Hash(..) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
            );
    }
    if crate::runtime::nqp_ops_list::is_iteration_buffer(target) {
        return true;
    }
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if !attributes.contains_key("__mutsu_array_storage")
            && !attributes.contains_key("__mutsu_hash_storage") =>
        {
            let class_name = class_name.as_str().to_string();
            if interp.has_user_method(&class_name, "iterator")
                && !interp.has_user_method(&class_name, name)
            {
                return true;
            }
        }
        ValueView::Mixin(..)
            if interp.mixin_composes_method(target, "iterator")
                && !interp.mixin_composes_method(target, name) =>
        {
            return true;
        }
        _ => {}
    }
    matches!(name, "map" | "grep")
        && target.is_lazy_list_value()
        && Interpreter::is_lazy_pipe_source(target)
}

macro_rules! handler {
    ($fn_name:ident, $name:literal, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
        fn $fn_name(
            interp: &mut Interpreter,
            target: &Value,
            args: &[Value],
            named: Named<'_>,
        ) -> Option<Result<Value, RuntimeError>> {
            if deferred_to_interpreter(interp, target, $name) {
                return None;
            }
            let $args = joined(args, named);
            let $target = target.clone();
            let $interp = interp;
            $body
        }
    };
}

/// `Any.collate`: sort by Unicode collation order under the dynamic
/// `$*COLLATION`, which only the interpreter can read. The native cascade's
/// `collate` arm calls the same `dispatch_collate` for the receivers the
/// table does not cover (a `Supply`, a `Seq`, a `Range`).
// Cost: O(e log e) comparisons, e = elements of the invocant, plus one
// dynamic-variable lookup.
fn collate(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    args.is_empty()
        .then(|| interp.dispatch_collate(target.clone()))
}

// Cost: O(1) at the call on a live or lazy source; O(e) otherwise, e = elements
// (one mapper call each at consumption).
handler!(map, "map", |interp, target, args| Some(
    interp.dispatch_map_method(target, args)
));
// Cost: O(1) at the call on a live or lazy source; O(e) otherwise, e = elements
// (one matcher call each).
handler!(grep, "grep", |interp, target, args| interp
    .dispatch_grep_method(target, args));
// Cost: O(e) matcher calls, e = elements scanned until the first hit.
handler!(first, "first", |interp, target, args| interp
    .dispatch_first_method(target, args));
// Cost: O(e) reducer calls, e = elements of the invocant.
handler!(reduce, "reduce", |interp, target, args| Some(
    interp.dispatch_reduce_method(target, args)
));
// Cost: O(e) callable calls, e = elements of the invocant.
handler!(produce, "produce", |interp, target, args| interp
    .dispatch_produce_method(target, args));
// Cost: O(e) to build the iterator over a copy of the elements, e = elements.
handler!(iterator, "iterator", |interp, target, args| args
    .is_empty()
    .then(|| interp.dispatch_iterator_method(target)));
// Cost: O(e), e = elements of the invocant.
handler!(eager, "eager", |interp, target, args| args
    .is_empty()
    .then(|| interp.dispatch_eager_row(target))
    .flatten());
// Cost: O(e) comparisons, e = elements of the invocant.
handler!(squish, "squish", |interp, target, args| Some(
    interp.dispatch_squish_method(target, args)
));
// Cost: O(e), e = elements of the invocant.
handler!(rotor, "rotor", |interp, target, args| Some(
    interp.dispatch_rotor_method(target, args)
));
// Cost: O(1) at the call on a lazy source; O(e) otherwise, e = elements.
handler!(skip, "skip", |interp, target, args| Some(
    interp.dispatch_skip_method(target, args)
));
// Cost: O(n) in the haystack for a plain pattern.
handler!(match_, "match", |interp, target, args| Some(
    interp.dispatch_match_row(target, args)
));
// Cost: O(e) test calls, e = elements of the invocant.
handler!(classify, "classify", |interp, target, args| interp
    .dispatch_classify_method(target, "classify", args));
// Cost: O(e) test calls, e = elements of the invocant.
handler!(categorize, "categorize", |interp, target, args| interp
    .dispatch_classify_method(target, "categorize", args));
// Cost: O(e) test calls, e = elements of the list.
handler!(classify_list, "classify-list", |interp, target, args| Some(
    interp.dispatch_classify_list_method(target, "classify-list", args)
));
// Cost: O(e) test calls, e = elements of the list.
handler!(categorize_list, "categorize-list", |interp, target, args| Some(
    interp.dispatch_classify_list_method(target, "categorize-list", args)
));
