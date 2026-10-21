//! `Any` rows whose handlers need the interpreter (`Handler::Interp`).
//!
//! `collate` reads the dynamic `$*COLLATION`; the iteration methods (`map`,
//! `flatmap`, `deepmap`, `duckmap`, `nodemap`, `grep`, `first`, `reduce`, `produce`, `rotor`, `skip`, `squish`, `eager`,
//! `iterator`, `match`, `classify`, `categorize`) run user code through the
//! interpreter. Each handler calls the one `Interpreter::dispatch_*_method` the
//! cascade's arm calls too (ADR-11276 slice 3C remainder). The receivers the
//! VM's native entry leaves to the interpreter are the receiver step's
//! (`receiver_step::general_entry_owns`), not the rows' (§9.134).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

const ARGS: RowFlags = RowFlags::ANY_ARGS;
/// `match` binds the regex adverbs (`:g`, `:ov`, `:x`, ...), which are open-ended.
/// The named arguments `map` binds: `Any.map`'s six and the `:batch` and
/// `:degree` a `HyperSeq`/`RaceSeq` reads from its `*%options`.
pub(crate) const MAP_NAMED: &[&str] = &[
    "batch", "deep", "degree", "duck", "flat", "item", "label", "node",
];
const ARGS_NAMED: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::ANY_NAMED);
/// `Any`'s iteration methods take a type object as a one-item list
/// (`Int.map(...)`, `Int.first`); `rotor` does not (Rakudo's candidates are
/// `Any:D:`).
const TYPE_OK: RowFlags = RowFlags::TYPE_OBJECT_OK;
const ARGS_TYPE: RowFlags = ARGS.or(RowFlags::TYPE_OBJECT_OK);
/// `grep`'s and `first`'s matcher is `Mu`: a `Junction` matches as itself.
/// They validate their adverbs and arity themselves (`X::Adverb`, the
/// `grep(List:D: )` no-match), so their rows take every call.
const MU_ARGS: RowFlags = ARGS_TYPE
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_NAMED)
    .or(RowFlags::JUNCTION_ARGS);

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
    any_row!("Any", "collate", 0, TYPE_OK, collate),
    // `map` reports a non-callable or a list of mappers itself
    // (`X::Cannot::Map`), so its row takes every positional after the first.
    any_row!(
        "Any",
        "map",
        0,
        ARGS_TYPE.or(RowFlags::SLURPY),
        map,
        MAP_NAMED
    ),
    any_row!("Any", "grep", 0, MU_ARGS, grep),
    any_row!("Any", "first", 0, MU_ARGS, first),
    any_row!("Any", "flatmap", 1, ARGS_TYPE, flatmap, &["label"]),
    any_row!("Any", "deepmap", 1, ARGS_TYPE, deepmap),
    any_row!("Any", "duckmap", 1, ARGS_TYPE, duckmap),
    any_row!("Any", "nodemap", 1, ARGS_TYPE, nodemap),
    any_row!("Any", "reduce", 1, ARGS_TYPE, reduce),
    any_row!("Any", "produce", 1, ARGS_TYPE, produce),
    any_row!("Any", "iterator", 0, TYPE_OK, iterator),
    // `Mu` declares `iterator` too: `Mu.iterator` and `Mu.new.iterator`
    // iterate the one value. No shape stops at `Mu` before `Any`, so only the
    // MRO walk of a receiver that is no `Any` reaches it.
    any_row!(
        "Mu",
        "iterator",
        0,
        TYPE_OK.or(RowFlags::OWNER_ONLY),
        iterator
    ),
    any_row!("Any", "eager", 0, TYPE_OK, eager),
    any_row!("Any", "squish", 0, ARGS_TYPE, squish, &["as", "with"]),
    any_row!(
        "Any",
        "rotor",
        0,
        ARGS.or(RowFlags::SLURPY),
        rotor,
        &["partial"]
    ),
    any_row!("Any", "skip", 0, ARGS_TYPE.or(RowFlags::SLURPY), skip),
    any_row!(
        "Any",
        "match",
        1,
        ARGS_NAMED.or(RowFlags::TYPE_OBJECT_OK),
        match_
    ),
    any_row!("Any", "classify", 1, ARGS_TYPE, classify, &["as", "into"]),
    any_row!(
        "Any",
        "categorize",
        1,
        ARGS_TYPE,
        categorize,
        &["as", "into"]
    ),
    any_row!(
        "Hash",
        "classify-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        classify_list,
        &["as"]
    ),
    any_row!(
        "Hash",
        "categorize-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        categorize_list,
        &["as"]
    ),
    any_row!(
        "Bag",
        "classify-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        classify_list,
        &["as"]
    ),
    any_row!(
        "Bag",
        "categorize-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        categorize_list,
        &["as"]
    ),
    any_row!(
        "BagHash",
        "classify-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        classify_list,
        &["as"]
    ),
    any_row!(
        "BagHash",
        "categorize-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        categorize_list,
        &["as"]
    ),
    any_row!(
        "Mix",
        "classify-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        classify_list,
        &["as"]
    ),
    any_row!(
        "Mix",
        "categorize-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        categorize_list,
        &["as"]
    ),
    any_row!(
        "MixHash",
        "classify-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        classify_list,
        &["as"]
    ),
    any_row!(
        "MixHash",
        "categorize-list",
        1,
        ARGS.or(RowFlags::SLURPY),
        categorize_list,
        &["as"]
    ),
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

/// `Any`'s methods that read `self.iterator` (measured against Rakudo 2026.09
/// on a plain class declaring only `iterator`): a receiver that brings its own
/// `iterator` answers them from its elements (ADR-11276 §9.148).
pub(crate) const ANY_VIA_ITERATOR: [&str; 21] = [
    "head", "tail", "first", "skip", "grep", "map", "sort", "min", "max", "minmax", "sum",
    "unique", "squish", "rotor", "batch", "Seq", "repeated", "toggle", "flatmap", "deepmap",
    "duckmap",
];

/// Whether `Any`'s row cannot serve this receiver: an associative or quantity
/// receiver reads as its pairs, which the pure cascades build for
/// `squish`/`eager`/`produce` and the interpreter's own routines do not.
// Cost: O(1).
fn reads_as_pairs(target: &Value, name: &str) -> bool {
    matches!(name, "squish" | "eager" | "produce")
        && matches!(
            target.view(),
            ValueView::Hash(..) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
        )
}

macro_rules! handler {
    ($fn_name:ident, $name:literal, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
        fn $fn_name(
            interp: &mut Interpreter,
            target: &Value,
            args: &[Value],
            named: Named<'_>,
        ) -> Option<Result<Value, RuntimeError>> {
            if reads_as_pairs(target, $name) {
                return None;
            }
            let $args = joined(args, named);
            // An invocant held in a scalar (`my $l = <a b>; $l.skip`) or itemized
            // by a hyper call (`@a».reduce(&[+])`) is the list, not one item of
            // it: these methods read its elements.
            let $target = target.clone().deitemize_for_sigil_bind();
            let $interp = interp;
            $body
        }
    };
}

/// The owners Rakudo declares `iterator` on besides `Any`: their own method
/// tables carry the name, but the answer is `Any`'s (`dispatch_iterator_method`
/// builds the one `Iterator` instance over a copy of the elements). Found by
/// owner only, so the shape lookup still reaches `Any`'s row first.
macro_rules! iterator_rows {
    ($($owner:literal),*) => {
        &[$(any_row!($owner, "iterator", 0, TYPE_OK.or(RowFlags::OWNER_ONLY), iterator)),*]
    };
}

pub(super) static ITERATOR_ROWS: &[MethodRow] = iterator_rows!(
    "Array", "Bag", "BagHash", "List", "Map", "Mix", "MixHash", "Pair", "Range", "Seq", "Set",
    "SetHash"
);

/// `deepmap`, `eager`, `first` and `map` where Rakudo declares them on a
/// class other than `Any` (the quant hashes, `Seq`, `Range`, `Backtrace`): the
/// owner's row runs `Any`'s handler.
pub(super) static DECLARED_ELSEWHERE_ROWS: &[MethodRow] = &[
    any_row!(
        "Bag",
        "deepmap",
        1,
        ARGS_TYPE.or(RowFlags::OWNER_ONLY),
        deepmap
    ),
    any_row!(
        "BagHash",
        "deepmap",
        1,
        ARGS_TYPE.or(RowFlags::OWNER_ONLY),
        deepmap
    ),
    any_row!(
        "Mix",
        "deepmap",
        1,
        ARGS_TYPE.or(RowFlags::OWNER_ONLY),
        deepmap
    ),
    any_row!(
        "MixHash",
        "deepmap",
        1,
        ARGS_TYPE.or(RowFlags::OWNER_ONLY),
        deepmap
    ),
    any_row!(
        "Set",
        "deepmap",
        1,
        ARGS_TYPE.or(RowFlags::OWNER_ONLY),
        deepmap
    ),
    any_row!(
        "SetHash",
        "deepmap",
        1,
        ARGS_TYPE.or(RowFlags::OWNER_ONLY),
        deepmap
    ),
    any_row!("Seq", "eager", 0, TYPE_OK.or(RowFlags::OWNER_ONLY), eager),
    any_row!("Range", "first", 0, MU_ARGS.or(RowFlags::OWNER_ONLY), first),
    any_row!(
        "Backtrace",
        "first",
        0,
        MU_ARGS.or(RowFlags::OWNER_ONLY),
        first
    ),
    any_row!(
        "Backtrace",
        "map",
        0,
        ARGS_TYPE.or(RowFlags::SLURPY).or(RowFlags::OWNER_ONLY),
        map,
        MAP_NAMED
    ),
];

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
handler!(flatmap, "flatmap", |interp, target, args| Some(
    interp.dispatch_flatmap_method(target, args)
));
handler!(deepmap, "deepmap", |interp, target, args| Some(
    interp
        .deepmap_iterate(&args[0], &target)
        .map(|mapped| interp.single_item_result(&target, mapped))
));
handler!(duckmap, "duckmap", |interp, target, args| Some(
    interp
        .duckmap_iterate(&args[0], &target)
        .map(|mapped| interp.single_item_result(&target, mapped))
));
handler!(nodemap, "nodemap", |interp, target, args| Some(
    interp.nodemap_iterate(&args[0], &target)
));
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
handler!(
    categorize_list,
    "categorize-list",
    |interp, target, args| Some(interp.dispatch_classify_list_method(
        target,
        "categorize-list",
        args
    ))
);
