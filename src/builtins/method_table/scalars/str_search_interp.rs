//! `Str`'s and `Cool`'s search and slice rows for every argument form
//! (ADR-11276 §9.129): `contains`, `index` and `rindex` with a needle and an
//! optional position, `starts-with`, `ends-with`, `substr`, `substr-eq`,
//! `comb` with a matcher and an optional limit, and `wordcase` with
//! `:filter`/`:where`.
//!
//! A plain needle and no adverb is answered by the pure candidate
//! (`str_search.rs`, `str_args.rs`, `str_iter.rs`, `str.rs`), which borrows the
//! invocant; anything else — a `Regex` needle, `:i`/`:m`, a `Range` or a
//! `WhateverCode` position, a callable `:filter` — by the interpreter routine
//! the by-name arms of `dispatch_method_by_name_1` called before these rows
//! took every form. So each row is a `Handler::Interp` row with the pure
//! answer as its fast path, and a receiver the shape lookup does not take
//! (an `IO::Path`, a `Seq`, an enum, a `Cool` instance) reaches the same row
//! through its MRO (`method_table::invoke_mro`).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

/// Any positional argument, and every named one (the adverbs `:i`, `:m`,
/// `:match`, `:filter`, `:where`).
const ARGS_NAMED: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::ANY_NAMED);

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $pure:ident, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::InterpPureFirst($pure, $handler),
            flags: ARGS_NAMED,
            named: &[],
        }
    };
}

/// The rows `Str` and `Cool` both declare.
macro_rules! search_rows {
    ($owner:literal) => {
        &[
            row!($owner, "contains", 1, contains_pure, contains),
            row!($owner, "contains", 2, contains_pure, contains),
            row!($owner, "starts-with", 1, starts_with_pure, starts_with),
            row!($owner, "ends-with", 1, ends_with_pure, ends_with),
            row!($owner, "index", 1, index_pure, index),
            row!($owner, "index", 2, index_pure, index),
            row!($owner, "rindex", 1, rindex_pure, rindex),
            row!($owner, "rindex", 2, rindex_pure, rindex),
            row!($owner, "substr", 1, substr_pure, substr),
            row!($owner, "substr", 2, substr_pure, substr),
            row!($owner, "substr-eq", 1, substr_eq_pure, substr_eq),
            row!($owner, "substr-eq", 2, substr_eq_pure, substr_eq),
            row!($owner, "comb", 1, comb_pure, comb),
            row!($owner, "comb", 2, comb_pure, comb),
            row!($owner, "wordcase", 0, wordcase_pure, wordcase),
        ]
    };
}

pub(super) static STR_ROWS: &[MethodRow] = search_rows!("Str");
pub(super) static COOL_ROWS: &[MethodRow] = search_rows!("Cool");

/// The call's arguments as the interpreter routines take them: the positional
/// ones, then the named ones as pairs.
// Cost: O(a), a = arguments of the call.
fn joined(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

/// Whether the pure candidate binds the call: one plain-scalar needle. The
/// pure half runs only for a call with no adverb (`dispatch::call`).
// Cost: O(1).
fn one_plain_needle(args: &[Value]) -> bool {
    args.len() == 1 && plain_needle(&args[0])
}

// The pure halves of the rows (`Handler::InterpPureFirst`): each answers the
// argument forms its routine's pure candidate binds, and declines the rest to
// the interpreter half below.

// Cost: as `str_search::contains`.
fn contains_pure(t: &Value, args: &[Value]) -> Answer {
    one_plain_needle(args).then(|| super::str_search::contains(t, args))
}

// Cost: O(m), m = chars of the needle.
fn starts_with_pure(t: &Value, args: &[Value]) -> Answer {
    one_plain_needle(args).then(|| super::str_search::starts_with(t, args))
}

// Cost: O(m), m = chars of the needle.
fn ends_with_pure(t: &Value, args: &[Value]) -> Answer {
    one_plain_needle(args).then(|| super::str_search::ends_with(t, args))
}

// Cost: as `str_search::index`.
fn index_pure(t: &Value, args: &[Value]) -> Answer {
    one_plain_needle(args).then(|| super::str_search::index(t, args))
}

// Cost: as `str_search::rindex`.
fn rindex_pure(t: &Value, args: &[Value]) -> Answer {
    one_plain_needle(args).then(|| super::str_search::rindex(t, args))
}

/// The pure slice for in-range `Int` positions; a `Range`, a `WhateverCode`,
/// a negative or out-of-range position is the interpreter's.
// Cost: O(k) amortized, k = chars returned.
fn substr_pure(t: &Value, args: &[Value]) -> Answer {
    if !args.iter().all(plain_needle) {
        return None;
    }
    super::str_search::substr(t, args)
}

// Cost: O(m), m = chars of the needle.
fn substr_eq_pure(t: &Value, args: &[Value]) -> Answer {
    if !args.iter().all(plain_needle) {
        return None;
    }
    super::str_args::substr_eq(t, args)
}

// Cost: O(n), n = chars of the invocant.
fn comb_pure(t: &Value, args: &[Value]) -> Answer {
    if !one_plain_needle(args) {
        return None;
    }
    crate::builtins::comb::native_comb_method(t, args)
}

// Cost: O(n), n = chars of the invocant.
fn wordcase_pure(t: &Value, args: &[Value]) -> Answer {
    Some(super::str::wordcase(t, args))
}

/// Whether a needle is one the pure candidates read through its string form:
/// not a pattern, a type object (which binds no candidate) or a list.
// Cost: O(1).
fn plain_needle(arg: &Value) -> bool {
    !matches!(
        arg.view(),
        crate::value::ValueView::Regex(..)
            | crate::value::ValueView::RegexWithAdverbs(..)
            | crate::value::ValueView::Package(_)
            | crate::value::ValueView::Array(..)
    )
}

/// `contains($needle, $pos?, :i, :m)`.
// Cost: as `str_search::contains` for a plain needle; see `dispatch_contains`.
fn contains(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    // A regex needle built in a sub from its lexicals reads them through the
    // scope it closed over, as `.match` does.
    Some(i.with_regex_closure_scope(args.first().cloned(), |me| {
        me.dispatch_contains(t.clone(), &joined(args, named))
    }))
}

/// `starts-with($needle, :i, :m)`.
// Cost: O(m), m = chars of the needle.
fn starts_with(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_starts_with(t.clone(), &joined(args, named)))
}

/// `ends-with($needle, :i, :m)`.
// Cost: O(m), m = chars of the needle.
fn ends_with(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_ends_with(t.clone(), &joined(args, named)))
}

/// `index($needle, $pos?, :i, :m)`.
// Cost: as `str_search::index` for a plain needle; see `dispatch_index`.
fn index(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_index(t.clone(), &joined(args, named)))
}

/// `rindex($needle, $pos?)`.
// Cost: as `str_search::rindex` for a plain needle; see `dispatch_rindex`.
fn rindex(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_rindex(t.clone(), &joined(args, named)))
}

/// `substr($from, $chars?)`: the pure slice for in-range `Int` positions, the
/// interpreter for a `Range`, a `WhateverCode`, a negative or out-of-range
/// position.
// Cost: O(k) amortized, k = chars returned.
fn substr(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_substr(t.clone(), &joined(args, named)))
}

/// `substr-eq($needle, $pos?, :i, :m)`.
// Cost: O(m), m = chars of the needle.
fn substr_eq(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_substr_eq(t.clone(), &joined(args, named)))
}

/// `comb($matcher, $limit?, :match)`: the pure candidate for an `Int` chunk
/// size or a `Str` needle, the interpreter for a pattern (installed around the
/// call, as `~~` does, since a regex may close over its defining scope).
// Cost: O(n), n = chars of the invocant, plus the pattern's matching cost.
fn comb(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let args = joined(args, named);
    let matcher = args.first().cloned();
    i.with_regex_closure_scope(matcher, |me| me.dispatch_comb_with_args(t.clone(), &args))
}

/// `wordcase(:&filter, :$where)`.
// Cost: O(n) plus one `:where` smartmatch and one `:filter` call per word.
fn wordcase(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_wordcase(t.clone(), &joined(args, named)))
}
