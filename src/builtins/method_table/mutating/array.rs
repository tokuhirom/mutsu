//! The `Array` mutators (ADR-11276 §9.23): `push`, `append`, `unshift`,
//! `prepend`, `pop`, `shift` and `splice`, and the `List` rows that refuse them.
//!
//! One row answers every receiver the copies it replaces served: an `@` variable,
//! a scalar that holds an array (`my $r := @a`, `my $n = [1, 2]`), an array held
//! by value (`f().pop`, `[1, 2].shift`, an element) and the backing storage of an
//! `is Array` instance. Container identity (ADR-0013 §3) means every write goes
//! **in place** through the array's shared `Gc` node, so every holder sees it;
//! what the place adds is the name, which keys the declared element type, the
//! `is default` value, the thread-shared store and a compunit's unit-lexical or
//! an `our` package array, and which is also where a name that no longer
//! resolves to an array is rebuilt from the receiver.
//!
//! `List` declares the six non-splice mutators too and refuses them with
//! `X::Immutable`; it has no `splice`, so a `List` receiver reaches `Array`'s
//! row and gets Rakudo's "no candidates". A `List` and an `Array` of the
//! same value are the same shape to the table, told apart by their
//! `ArrayKind`, so both owners' rows are one handler.
//!
//! `Array.grab` picks random elements like `pick` and removes them, one `splice`
//! each, so it inherits every rule the other mutators obey.
//!
//! Not here: the VM's thread-shared lanes in front of the dispatch
//! (`exec_call_method_mut_op_impl`), which route a plain `@name` through the
//! atomic store once a thread exists; and the `ArrayPush` opcode.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::methods_signature_errors::{make_no_candidates_error, make_x_immutable_error};
use crate::runtime::utils::{is_shaped_array, make_empty_array_failure_what};
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};

mod operations;
use operations::{grow, shrink, splice};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        row!($owner, $name, $handler, RowFlags::NONE)
    };
    ($owner:literal, $name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Mut($handler),
            flags: RowFlags::SLURPY.or($flags),
            named: &[],
        }
    };
}

/// Every row is slurpy from zero arguments: `push` takes any number, and `pop`
/// and `shift` take none but must answer a call with some with Rakudo's arity
/// error rather than "No such method". A type-object receiver (#13881) is
/// refused by the methods Rakudo declares `Array:D` only: `pop` and `shift`
/// (`only`, an invalid concreteness), `splice` and `grab` (multis, no
/// candidate). The growing methods keep vivifying a type object a binding holds.
pub(super) static ROWS: &[MethodRow] = &[
    row!("Array", "push", push_row),
    row!("Array", "append", append_row),
    row!("Array", "unshift", unshift_row),
    row!("Array", "prepend", prepend_row),
    row!("Array", "pop", pop_row, RowFlags::DEFINITE),
    row!("Array", "shift", shift_row, RowFlags::DEFINITE),
    row!("Array", "splice", splice_row, RowFlags::DEFINITE_MULTI),
    row!("Array", "grab", grab_row, RowFlags::DEFINITE_MULTI),
    row!("List", "push", push_row, RowFlags::IMMUTABLE),
    row!("List", "append", append_row, RowFlags::IMMUTABLE),
    row!("List", "unshift", unshift_row, RowFlags::IMMUTABLE),
    row!("List", "prepend", prepend_row, RowFlags::IMMUTABLE),
    row!("List", "pop", pop_row, RowFlags::IMMUTABLE),
    row!("List", "shift", shift_row, RowFlags::IMMUTABLE),
];

/// The seven mutators.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Op {
    Push,
    Append,
    Unshift,
    Prepend,
    Pop,
    Shift,
    Splice,
}

impl Op {
    // Cost: O(1).
    fn name(self) -> &'static str {
        match self {
            Op::Push => "push",
            Op::Append => "append",
            Op::Unshift => "unshift",
            Op::Prepend => "prepend",
            Op::Pop => "pop",
            Op::Shift => "shift",
            Op::Splice => "splice",
        }
    }

    /// Whether the elements go in at the front.
    // Cost: O(1).
    fn is_front(self) -> bool {
        matches!(self, Op::Unshift | Op::Prepend)
    }
}

macro_rules! handler {
    ($vis:vis $fn:ident, $op:expr) => {
        // Cost: see `run`.
        $vis fn $fn(
            interp: &mut Interpreter,
            place: &mut ReceiverPlace<'_>,
            args: &[Value],
            _named: Named<'_>,
        ) -> Option<Result<Value, RuntimeError>> {
            run(interp, place, $op, args)
        }
    };
}

handler!(pub(super) push_row, Op::Push);
handler!(pub(super) append_row, Op::Append);
handler!(pub(super) unshift_row, Op::Unshift);
handler!(pub(super) prepend_row, Op::Prepend);
handler!(pop_row, Op::Pop);
handler!(shift_row, Op::Shift);
handler!(splice_row, Op::Splice);

/// Apply `op` to the array the place holds. `None` for a receiver that is not
/// an array.
// Cost: O(k) amortized for a push, append, unshift or prepend of k elements
// (`ArrayData::extend` / `prepend_values`), O(1) amortized for a pop or shift,
// O(n + r + (e - s - n)) for a splice (see `splice`); every typed or object
// element adds one type check.
fn run(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    op: Op,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    // A binding more than one thread can reach writes to its one shared home
    // (`Interpreter::shared_array_lane`, ADR-11276 §9.169).
    if let Some(name) = place.name()
        && let Some(answer) = interp.shared_array_lane(name, place.value(), op.name(), args)
    {
        return Some(answer);
    }
    let ValueView::Array(_, kind) = place.value().descalarize().view() else {
        return None;
    };
    Some(apply(interp, place, op, kind, args))
}

/// [`run`] on a receiver known to be an array of `kind`.
// Cost: see `run`.
fn apply(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    op: Op,
    kind: ArrayKind,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let method = op.name();
    // An immutable `List`: the six mutators Rakudo defines on it throw
    // `X::Immutable`. `splice` is not among them (Rakudo declares it on `Array`
    // only), so a `List` receiver resolves no candidate at all rather than
    // hitting an immutability check.
    if !kind.is_real_array() {
        return Err(if op == Op::Splice {
            make_no_candidates_error(method, place.value(), args)
        } else {
            make_x_immutable_error(method, "List")
        });
    }
    // A shaped (multidimensional) array has fixed dimensions.
    if is_shaped_array(place.value().descalarize()) {
        return Err(RuntimeError::illegal_on_fixed_dimension_array(method));
    }
    match op {
        Op::Push | Op::Append | Op::Unshift | Op::Prepend => grow(interp, place, op, args),
        Op::Pop | Op::Shift => shrink(interp, place, op, args),
        Op::Splice => splice(interp, place, args),
    }
}

/// `@a.grab` / `@a.grab($n)` / `@a.grab(*)`: remove random elements and return
/// them (a bare element for no argument, a `Seq` otherwise). Only a plain `@`
/// array grabs here, and `None` declines an argument shape the generic path
/// should report (a Callable count).
// Cost: O(n * k), n = elements, k = grabbed (one in-place `splice` each).
fn grab_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    place.name().filter(|name| name.starts_with('@'))?;
    let ValueView::Array(items, ArrayKind::Array) = place.value().view() else {
        return None;
    };
    // A `Callable` count is called with the element count (§9.165).
    let counted = match args.first() {
        Some(arg) if arg.as_sub().is_some() => {
            let target = place.value().clone();
            match crate::builtins::method_table::sampling_callable_count(
                interp, &target, arg, false,
            )? {
                Ok(count) => Some(count),
                Err(err) => return Some(Err(err)),
            }
        }
        _ => None,
    };
    let args = counted.as_ref().map(std::slice::from_ref).unwrap_or(args);
    let mut pool: Vec<usize> = (0..items.len()).collect();
    let want = match args.first().map(Value::view) {
        None => None,
        Some(ValueView::Whatever) => Some(pool.len()),
        Some(ValueView::Int(i)) => Some(i.max(0) as usize),
        Some(ValueView::Num(f)) if f.is_infinite() && f > 0.0 => Some(pool.len()),
        _ => return None,
    };
    if args.len() > 1 {
        return None;
    }
    let take = want.unwrap_or(1).min(pool.len());
    let mut picked = Vec::with_capacity(take);
    for _ in 0..take {
        let j = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize % pool.len();
        picked.push(pool.swap_remove(j));
    }
    let mut out = Vec::with_capacity(take);
    for &i in &picked {
        out.push(items.get(i).cloned().unwrap_or(Value::NIL));
    }
    let mut removal = picked;
    removal.sort_unstable_by(|a, b| b.cmp(a));
    for i in removal {
        if let Err(e) = splice(interp, place, &[Value::int(i as i64), Value::int(1)]) {
            return Some(Err(e));
        }
    }
    Some(Ok(match want {
        None => out.into_iter().next().unwrap_or(Value::NIL),
        Some(_) => Value::seq(out),
    }))
}

/// What a failure on an empty array names: `array[num]` for a native typed
/// array, `Array[Int]` for an array with an element type, `Array` otherwise.
// Cost: O(1).
fn empty_what(interp: &Interpreter, name: Option<&str>) -> String {
    match name.and_then(|n| interp.var_type_constraint(n)) {
        Some(c)
            if crate::runtime::native_types::is_native_array_element_type(&c)
                || matches!(c.as_str(), "num" | "num32" | "num64" | "str") =>
        {
            format!("array[{c}]")
        }
        Some(c) if !matches!(c.as_str(), "Any" | "Mu" | "") => format!("Array[{c}]"),
        _ => "Array".to_string(),
    }
}

/// Check `values` against the array's element type: the container's own
/// metadata first, then (for an `@` variable only, since a scalar's constraint
/// is the variable's, not the elements') the variable's declaration.
// Cost: O(v) type checks, v = values.
pub(super) fn check_element_types(
    interp: &mut Interpreter,
    name: Option<&str>,
    target: &Value,
    values: &[Value],
) -> Result<(), RuntimeError> {
    match name {
        Some(name) if name.starts_with('@') => {
            interp.check_container_element_types(name, target, values)
        }
        _ => interp.check_array_value_element_types(target, values),
    }
}
