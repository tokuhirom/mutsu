//! `pick`, `roll` and `pickpairs` (ADR-11276 remainder: the sampling methods).
//!
//! Rakudo declares `pick` and `roll` on `List`, `Range`, `Map`, `Bool` and `Any`, and on
//! the six quant hashes, which also declare `pickpairs`. Every row calls the one
//! implementation in [`crate::builtins::sampling`]; a receiver with no dispatch
//! shape (a `Seq`, a lazy list, a shaped array) reaches `Any`'s rows through
//! `method_table::answer_shapeless`. The answer is random, so every row is flagged
//! `RANDOM`: the debug cross-check does not re-run it. The count-taking rows are
//! interpreter rows, which call a `Callable` count with the receiver's size
//! ([`callable_count`], ADR-11276 §9.165).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::builtins::sampling;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! rows {
    ($name:literal => $handler:path, $counted:path: $($owner:literal),*) => {
        rows!($name => $handler, $counted, RowFlags::NONE; $($owner),*)
    };
    ($name:literal => $handler:path, $counted:path, $extra:expr; $($owner:literal),*) => {
        &[$(
            MethodRow {
                owner: $owner,
                name: $name,
                arity: 0,
                handler: Handler::Narrow($handler),
                flags: RowFlags::RANDOM.or($extra),
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: $name,
                arity: 1,
                handler: Handler::Interp($counted),
                flags: RowFlags::RANDOM.or(RowFlags::ANY_ARGS).or($extra),
                named: &[],
            },
        )*]
    };
}

/// The count-taking form of a sampling method: a `Callable` count is called
/// with the receiver's size (`total` for a quant hash, which is its weight sum,
/// `elems` otherwise; `elems` for the `*pairs` methods) and its answer, as an
/// `Int`, is the count (`$bag.pick(* / 2)`, Rakudo's
/// `self.pick($calculate(self.total))`).
macro_rules! counted {
    ($fn:ident, $handler:path, $pairs:literal) => {
        counted!($fn, $handler, $pairs, |target: &Value| target.clone());
    };
    ($fn:ident, $handler:path, $pairs:literal, $sampled:expr) => {
        // Cost: the sampling's own, plus the callable's call and one `total`
        // or `elems` call for a `Callable` count.
        fn $fn(
            interp: &mut Interpreter,
            target: &Value,
            args: &[Value],
            _named: Named<'_>,
        ) -> Option<Result<Value, RuntimeError>> {
            match callable_count(interp, &$sampled(target), &args[0], $pairs)? {
                Ok(count) => $handler(target, std::slice::from_ref(&count)),
                Err(err) => Some(Err(err)),
            }
        }
    };
}

counted!(pick_counted, sampling::pick, false);
counted!(roll_counted, sampling::roll, false);
counted!(pickpairs_counted, sampling::pickpairs, true);
// `Bool`'s count is over the two values it samples.
counted!(bool_pick_counted, bool_pick, false, bool_values);
counted!(bool_roll_counted, bool_roll, false, bool_values);

/// The count a sampling method takes: `arg` itself, or a `Callable`'s answer
/// for the receiver's size, `total` for a quant hash's `pick`/`roll`/`grab`
/// (its weight sum) and `elems` otherwise (see [`counted!`]).
// Cost: O(1) for a plain count; the callable's call plus one size call
// otherwise.
pub(crate) fn callable_count(
    interp: &mut Interpreter,
    target: &Value,
    arg: &Value,
    pairs: bool,
) -> Option<Result<Value, RuntimeError>> {
    if arg.as_sub().is_none() {
        return Some(Ok(arg.clone()));
    }
    let quant = matches!(
        target.descalarize().view(),
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
    );
    let size_method = if quant && !pairs { "total" } else { "elems" };
    let size = match interp.call_method_with_values(target.clone(), size_method, Vec::new()) {
        Ok(size) => size,
        Err(err) => return Some(Err(err)),
    };
    Some(
        interp
            .call_sub_value(arg.clone(), vec![size])
            .map(crate::builtins::method_table::mutating::quanthash::callable_count_to_int),
    )
}

pub(super) static PICK_ROWS: &[MethodRow] = rows!("pick" => sampling::pick, pick_counted:
    "List", "Range", "Map", "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
pub(super) static ROLL_ROWS: &[MethodRow] = rows!("roll" => sampling::roll, roll_counted:
    "List", "Range", "Map", "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");
/// `Any.pick` and `Any.roll` are `self.list.pick` / `self.list.roll`, which a
/// type object answers too (`Int.pick` is `Int`). Every closed shape reaches
/// them (`ANY_LIST_FOR_CLOSED_SHAPES`), and every receiver with no shape
/// through `method_table::answer_shapeless`.
pub(super) static ANY_PICK_ROWS: &[MethodRow] =
    rows!("pick" => sampling::pick, pick_counted, RowFlags::TYPE_OBJECT_OK; "Any");
pub(super) static ANY_ROLL_ROWS: &[MethodRow] =
    rows!("roll" => sampling::roll, roll_counted, RowFlags::TYPE_OBJECT_OK; "Any");
pub(super) static PICKPAIRS_ROWS: &[MethodRow] = rows!("pickpairs" => sampling::pickpairs, pickpairs_counted:
    "Set", "SetHash", "Bag", "BagHash", "Mix", "MixHash");

/// `Bool.pick` and `Bool.roll` are declared for the type object only (`Bool:U`):
/// `Bool.pick` is `True` or `False`, and a defined `True.pick` has no candidate.
pub(super) static BOOL_ROWS: &[MethodRow] = &[
    bool_row("pick", 0, Handler::Narrow(bool_pick)),
    bool_row("pick", 1, Handler::Interp(bool_pick_counted)),
    bool_row("roll", 0, Handler::Narrow(bool_roll)),
    bool_row("roll", 1, Handler::Interp(bool_roll_counted)),
];

const fn bool_row(name: &'static str, arity: u8, handler: Handler) -> MethodRow {
    MethodRow {
        owner: "Bool",
        name,
        arity,
        handler,
        flags: if arity == 0 {
            RowFlags::RANDOM.or(RowFlags::TYPE_OBJECT_OK)
        } else {
            RowFlags::RANDOM
                .or(RowFlags::TYPE_OBJECT_OK)
                .or(RowFlags::ANY_ARGS)
        },
        named: &[],
    }
}

/// The error a defined `Bool` gets: only `Bool:U` candidates exist.
fn bool_no_candidate(method: &str) -> RuntimeError {
    crate::runtime::methods_signature_errors::make_multi_no_match_error(method)
}

fn bool_pick(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Some(Err(bool_no_candidate("pick")));
    }
    sampling::pick(&bool_values(target), args)
}

fn bool_roll(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if matches!(target.view(), ValueView::Bool(_)) {
        return Some(Err(bool_no_candidate("roll")));
    }
    sampling::roll(&bool_values(target), args)
}

/// The values `Bool.pick`/`Bool.roll` sample: `False` and `True`, in enum order.
fn bool_values(_type_object: &Value) -> Value {
    Value::array(vec![Value::FALSE, Value::TRUE])
}
