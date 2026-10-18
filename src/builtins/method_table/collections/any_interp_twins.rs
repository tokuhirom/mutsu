//! The interpreter twins of `Any`'s pure aggregate and transform rows
//! (ADR-11276 §9.135).
//!
//! `Any.min`, `max`, `minmax`, `minpairs`, `maxpairs`, `sort`, `unique` and
//! `repeated` have a `Handler::Narrow` row (`list_aggregate`,
//! `list_transform`) that answers a plain list with no callback. A call with a
//! callback or an adverb (`.max(&by)`, `.sort(&cmp)`, `.unique(:as(...))`), or
//! a receiver the pure row declines (an instance, a type object), is the
//! interpreter's: these rows, registered after the pure ones so the pure row
//! stays the one the guard step and `owner_row` find, take it through the same
//! routine the by-name cascade's arm called. The MRO walk (`invoke_mro`) tries
//! an owner's rows at the call's arity in turn, so it reaches the twin when
//! the pure row declines.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// Found by owner only, every argument and adverb handed to the routine,
/// which validates them; a type object reads as a one-item list.
const TWIN: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_NAMED)
    .or(RowFlags::TYPE_OBJECT_OK);
/// `minpairs`/`maxpairs` take no argument.
const TWIN_NULLARY: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK);

macro_rules! twin {
    ($name:literal, $flags:expr, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
        twin!("Any", $name, $flags, |$interp, $target, $args| $body)
    };
    ($owner:literal, $name:literal, $flags:expr, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp({
                fn handler(
                    interp: &mut Interpreter,
                    target: &Value,
                    args: &[Value],
                    named: Named<'_>,
                ) -> Option<Result<Value, RuntimeError>> {
                    let mut $args = Vec::with_capacity(args.len() + named.pairs().len());
                    $args.extend(args.iter().cloned());
                    $args.extend(named.pairs().iter().cloned());
                    // A scalar-held list is the list (`my $l = (3, 1); $l.sort`).
                    let $target = target.clone().deitemize_for_sigil_bind();
                    let $interp = interp;
                    $body
                }
                handler
            }),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    twin!("min", TWIN, |interp, target, args| Some(
        interp.dispatch_min_max_method(target, "min", args)
    )),
    twin!("max", TWIN, |interp, target, args| Some(
        interp.dispatch_min_max_method(target, "max", args)
    )),
    twin!("minmax", TWIN, |interp, target, args| interp
        .dispatch_minmax_method(target, "minmax", args)),
    twin!("minpairs", TWIN_NULLARY, |interp, target, _args| Some(
        interp.dispatch_minmaxpairs(target, "minpairs")
    )),
    twin!("maxpairs", TWIN_NULLARY, |interp, target, _args| Some(
        interp.dispatch_minmaxpairs(target, "maxpairs")
    )),
    twin!("sort", TWIN, |interp, target, args| Some(
        interp.dispatch_sort_method(target, args)
    )),
    twin!("unique", TWIN, |interp, target, args| Some(
        interp.dispatch_unique(target, &args)
    )),
    twin!("repeated", TWIN, |interp, target, args| Some(
        interp.dispatch_repeated(target, &args)
    )),
    // `tail` of a receiver the pure row declines: a lazy list (which refuses
    // with `X::Cannot::Lazy`), a `Seq.new($iterator)` body, an instance, a
    // count that is a `Callable` or a `Whatever` (§9.186).
    // `tail` declares no adverb, so a named argument is ignored
    // (`(1, 2, 3).tail(:zzz)` is 3).
    twin!("tail", TWIN, |interp, target, args| {
        let counts: Vec<Value> = args
            .into_iter()
            .filter(|arg| !arg.is_string_pair_value())
            .collect();
        Some(interp.dispatch_tail(target, &counts))
    }),
    // `head(&callable)` / `head(*)` of a finite list (§9.186).
    twin!("head", TWIN, |interp, target, args| {
        match args.as_slice() {
            [count]
                if matches!(
                    count.view(),
                    ValueView::Whatever
                        | ValueView::Sub(_)
                        | ValueView::WeakSub(_)
                        | ValueView::Routine { .. }
                ) && !matches!(target.view(), ValueView::Instance { .. })
                    && !super::terminals::is_lazy_tail_target(&target) =>
            {
                Some(interp.dispatch_head(target, count))
            }
            _ => None,
        }
    }),
    // `fmt` with a `Format` object, or over elements whose `.Str`/`.Numeric`
    // is user code, which the pure rows decline (§9.187).
    twin!("fmt", TWIN, |interp, target, args| interp
        .dispatch_fmt_with_format(&target, &args)
        .or_else(
            || interp.dispatch_fmt_with_user_coercion(&target, &args)
        )),
    // `Mu.Capture` of a receiver whose pure answer is wrong: a user object (its
    // public attributes read through their accessor methods), a `Channel` or
    // `Supply` (drained), a list with a non-`Str` `Pair` key (stringified
    // through its `.Str`), a cat handle's lines (§9.187).
    twin!("Mu", "Capture", TWIN_NULLARY, |interp, target, _args| {
        interp.try_interpreter_capture(&target)
    }),
    // `gist` of a collection holding a value whose rendering is user code,
    // which the pure rows decline (§9.188).
    twin!("List", "gist", TWIN_NULLARY, |interp, target, _args| interp
        .collection_gist_with_dispatch(&target)),
    twin!("Map", "gist", TWIN_NULLARY, |interp, target, _args| interp
        .collection_gist_with_dispatch(&target)),
    twin!("Seq", "gist", TWIN_NULLARY, |interp, target, _args| interp
        .collection_gist_with_dispatch(&target)),
    twin!("Pair", "gist", TWIN_NULLARY, |interp, target, _args| interp
        .collection_gist_with_dispatch(&target)),
];
