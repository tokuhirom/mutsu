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
/// [`TWIN`] for a method whose named arguments are the ones its row lists
/// (Rakudo's signature); any other is swallowed by the implicit `*%_`.
const TWIN_DECLARED: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY)
    .or(RowFlags::TYPE_OBJECT_OK);
/// `minpairs`/`maxpairs` take no argument.
const TWIN_NULLARY: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK);

/// The `raku` twin of a collection owner: the receiver as it is (its
/// itemization is part of the text), rendered with each leaf's own `raku`.
macro_rules! raku_twin {
    ($owner:literal) => {
        MethodRow {
            owner: $owner,
            name: "raku",
            arity: 0,
            handler: Handler::Interp({
                fn handler(
                    interp: &mut Interpreter,
                    target: &Value,
                    _args: &[Value],
                    _named: Named<'_>,
                ) -> Option<Result<Value, RuntimeError>> {
                    interp
                        .raku_repr_with_dispatch(target)
                        .map(|text| Ok(Value::str(text)))
                }
                handler
            }),
            flags: TWIN_NULLARY,
            named: &[],
        }
    };
}

macro_rules! twin {
    ($name:literal, $flags:expr, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
        twin!("Any", $name, $flags, named: &[], |$interp, $target, $args| $body)
    };
    ($name:literal, $flags:expr, named: $named:expr, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
        twin!("Any", $name, $flags, named: $named, |$interp, $target, $args| $body)
    };
    ($owner:literal, $name:literal, $flags:expr, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
        twin!($owner, $name, $flags, named: &[], |$interp, $target, $args| $body)
    };
    ($owner:literal, $name:literal, $flags:expr, named: $named:expr, |$interp:ident, $target:ident, $args:ident| $body:expr) => {
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
            named: $named,
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
    twin!("minmax", TWIN_DECLARED, named: &["by"], |interp, target, args| interp
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
    // Rakudo declares `sort` on `Map` and `Seq` besides `List`, and the
    // `minpairs`/`maxpairs` of a `Setty`, with the same answer.
    twin!("Map", "sort", TWIN, |interp, target, args| Some(
        interp.dispatch_sort_method(target, args)
    )),
    twin!("Seq", "sort", TWIN, |interp, target, args| Some(
        interp.dispatch_sort_method(target, args)
    )),
    twin!("Set", "minpairs", TWIN_NULLARY, |interp, target, _args| {
        Some(interp.dispatch_minmaxpairs(target, "minpairs"))
    }),
    twin!(
        "SetHash",
        "minpairs",
        TWIN_NULLARY,
        |interp, target, _args| Some(interp.dispatch_minmaxpairs(target, "minpairs"))
    ),
    twin!("Set", "maxpairs", TWIN_NULLARY, |interp, target, _args| {
        Some(interp.dispatch_minmaxpairs(target, "maxpairs"))
    }),
    twin!(
        "SetHash",
        "maxpairs",
        TWIN_NULLARY,
        |interp, target, _args| Some(interp.dispatch_minmaxpairs(target, "maxpairs"))
    ),
    twin!("unique", TWIN_DECLARED, named: &["as", "expires", "with"], |interp, target, args| Some(
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
    twin!("tail", TWIN_DECLARED, |interp, target, args| {
        let counts: Vec<Value> = args
            .into_iter()
            .filter(|arg| !arg.is_string_pair_value())
            .collect();
        // `Any.tail(n)` is `self.list.tail(n)`: an instance (a `CArray`, say)
        // counts from the end of its own list, not of itself.
        if matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..)
        ) && let Some(answer) = via_self_list_args(interp, &target, "tail", counts.clone())
        {
            return Some(answer);
        }
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
            // `Any.head` is `self.list.head`: an instance (its `list` may be
            // its own) reads its list's first element (#13819).
            [] if matches!(target.view(), ValueView::Instance { .. }) => {
                via_self_list(interp, &target, "head")
            }
            _ => None,
        }
    }),
    // `Any.reverse` is `self.list.reverse`, for an instance the pure row
    // declines (#13819).
    twin!("reverse", TWIN_NULLARY, |interp, target, _args| {
        // A type object is left to the rows that answer or refuse it
        // (`Supply.reverse` must die).
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..)
        ) {
            return None;
        }
        via_self_list(interp, &target, "reverse")
    }),
    // `fmt` with a `Format` object, or over elements whose `.Str`/`.Numeric`
    // is user code, which the pure rows decline (§9.187).
    twin!("fmt", TWIN_DECLARED, |interp, target, args| interp
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
    // `raku` of a collection holding a value whose rendering is user code,
    // which the pure `raku` rows decline (§9.194). The receiver keeps its
    // itemization: `$(...)` is part of the text.
    raku_twin!("List"),
    raku_twin!("Map"),
    raku_twin!("Seq"),
    raku_twin!("Pair"),
    raku_twin!("Capture"),
    raku_twin!("Set"),
    raku_twin!("SetHash"),
    raku_twin!("Bag"),
    raku_twin!("BagHash"),
    raku_twin!("Mix"),
    raku_twin!("MixHash"),
];

/// `self.list.METHOD` for an instance: its `list` (a user one, or `Any`'s
/// one-element list of itself), then `method` on that. `None` when the
/// instance's `list` answers an instance, which would recurse.
// Cost: O(1) plus the two calls'.
fn via_self_list(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    via_self_list_args(interp, target, method, Vec::new())
}

/// [`via_self_list`] for a method that takes arguments (`.tail(2)`).
// Cost: O(1) plus the two calls'.
fn via_self_list_args(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: Vec<Value>,
) -> Option<Result<Value, RuntimeError>> {
    let list = match interp.call_method_with_values(target.clone(), "list", Vec::new()) {
        Ok(list) => list,
        Err(e) => return Some(Err(e)),
    };
    if matches!(list.view(), ValueView::Instance { .. }) {
        return None;
    }
    Some(interp.call_method_with_values(list, method, args))
}
