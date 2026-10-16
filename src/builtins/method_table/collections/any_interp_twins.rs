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
use crate::value::{RuntimeError, Value};

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
        MethodRow {
            owner: "Any",
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
];
