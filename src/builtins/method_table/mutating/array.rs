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

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Mut($handler),
            flags: RowFlags::SLURPY,
            named: &[],
        }
    };
}

/// Every row is slurpy from zero arguments: `push` takes any number, and `pop`
/// and `shift` take none but must answer a call with some with Rakudo's arity
/// error rather than "No such method".
pub(super) static ROWS: &[MethodRow] = &[
    row!("Array", "push", push_row),
    row!("Array", "append", append_row),
    row!("Array", "unshift", unshift_row),
    row!("Array", "prepend", prepend_row),
    row!("Array", "pop", pop_row),
    row!("Array", "shift", shift_row),
    row!("Array", "splice", splice_row),
    row!("Array", "grab", grab_row),
    row!("List", "push", push_row),
    row!("List", "append", append_row),
    row!("List", "unshift", unshift_row),
    row!("List", "prepend", prepend_row),
    row!("List", "pop", pop_row),
    row!("List", "shift", shift_row),
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
    ($fn:ident, $op:expr) => {
        // Cost: see `run`.
        fn $fn(
            interp: &mut Interpreter,
            place: &mut ReceiverPlace<'_>,
            args: &[Value],
            _named: Named<'_>,
        ) -> Option<Result<Value, RuntimeError>> {
            run(interp, place, $op, args)
        }
    };
}

handler!(push_row, Op::Push);
handler!(append_row, Op::Append);
handler!(unshift_row, Op::Unshift);
handler!(prepend_row, Op::Prepend);
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
fn check_element_types(
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

/// Run `f` on the data of the array the place resolves to, through its shared
/// node (container identity, §3), and answer what it answers; `None` when the
/// name resolves to no array and `target` is not one a holder shares.
// Cost: O(1) plus `f`.
fn with_array_data<R>(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    target: &Value,
    f: impl FnOnce(&mut crate::value::ArrayData, ArrayKind) -> R,
) -> Option<R> {
    let mut f = Some(f);
    let resolved = place.slot(interp).and_then(|slot| {
        slot.with_array_mut(|arc_items, kind| {
            let f = f.take().expect("the closure runs once");
            f(crate::value::gc_data_mut(arc_items), *kind)
        })
    });
    if resolved.is_some() {
        return resolved;
    }
    // A shared array the name does not resolve to (a scalar holding an element's
    // array, `my $row = @grid[0]`): write it in place, as the by-name path does,
    // not into a detached copy.
    if matches!(target.view(), ValueView::Array(arc_items, _) if crate::gc::Gc::strong_count(&arc_items) > 1)
    {
        let f = f.take()?;
        return target.with_array_inplace(f);
    }
    None
}

/// `push`, `append`, `unshift`, `prepend`: add the arguments at an end.
// Cost: O(k) amortized, k = elements added; the rebuild of a name that no longer
// resolves to an array copies all e elements.
fn grow(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    op: Op,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let name = place.name().map(str::to_string);
    let key = name.as_deref().unwrap_or("");
    let target = place.value().descalarize().clone();
    // The mutators below reallocate the backing buffer when the node is shared,
    // which orphans the pointer-keyed type metadata; re-attach it afterwards.
    let saved_meta = interp.container_type_metadata(&target);
    // `push` and `unshift` take each argument as one element (a lone `Slip` is
    // spread); `append` and `prepend` flatten a single iterable argument (the
    // one-argument rule), and several arguments are appended as they are.
    let values = match op {
        Op::Push | Op::Unshift => Interpreter::normalize_push_unshift_args(args.to_vec()),
        _ => crate::runtime::flatten_append_args(args.to_vec()),
    };
    check_element_types(interp, name.as_deref(), &target, &values)?;
    // Storing `Nil` into a fresh element resets it to the element default:
    // `is default(...)` first, then the native element zero, then the declared
    // element type object, else `Any` (ADR-0049).
    let values = if values.iter().any(Value::is_nil) {
        let default = interp.assign_store_nil_default(key, &target);
        values
            .into_iter()
            .map(|v| if v.is_nil() { default.clone() } else { v })
            .collect()
    } else {
        values
    };
    // ADR-0040: itemize per element, after the one-argument rule's decision, so
    // a single pushed aggregate becomes one itemized element.
    let values: Vec<Value> = values.into_iter().map(Interpreter::itemize_value).collect();

    let result = if op == Op::Push && name.as_deref().is_some_and(|n| n.starts_with('@')) {
        // Thread-aware, name-keyed: a compunit's unit-lexical cell, an `our`
        // package array and the shared store are all found by the name.
        interp.push_to_shared_var(key, values, &target)
    } else {
        grow_in_place(interp, place, op, &target, values)
    };
    if name.is_some() {
        interp.reattach_array_type_metadata(key, &saved_meta);
    }
    Ok(result)
}

/// Add `values` at an end of the array the place resolves to, through its shared
/// node, or rebuild the array under the name when it resolves to none.
// Cost: O(k) amortized, k = elements added; the rebuild copies all e elements.
fn grow_in_place(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    op: Op,
    target: &Value,
    values: Vec<Value>,
) -> Value {
    let front = op.is_front();
    let mut pending = Some(values);
    let written = with_array_data(interp, place, target, |items, _| {
        let values = pending.take().expect("the closure runs once");
        if front {
            items.prepend_values(values);
        } else {
            items.extend(values);
        }
    });
    if written.is_some() {
        // The same node every holder shares.
        return match place.slot(interp).map(|slot| slot.view()) {
            Some(ValueView::Array(arc_items, kind)) => Value::array_with_kind(arc_items, kind),
            _ => target.clone(),
        };
    }
    // The name does not resolve to an array: build one from the receiver as the
    // call read it, and bind it under the name.
    let values = pending.take().expect("the closure did not run");
    let (mut items, kind) = match target.view() {
        ValueView::Array(v, kind) => (v.to_vec(), kind),
        _ => (Vec::new(), ArrayKind::Array),
    };
    if front {
        items.splice(0..0, values);
    } else {
        items.extend(values);
    }
    let result = Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(items)),
        kind,
    );
    place.assign(interp, result.clone());
    result
}

/// `pop` and `shift`: remove an element from an end and answer it, or answer a
/// `Failure` on an empty array.
// Cost: O(1) amortized (`ArrayData::pop` / `remove(0)` advances the front head
// offset, #9121); the rebuild of a name that no longer resolves to an array
// copies all e elements.
fn shrink(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    op: Op,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let method = op.name();
    if !args.is_empty() {
        return Err(RuntimeError::new(format!(
            "Too many positionals passed; expected 1 argument but got {}",
            args.len() + 1
        )));
    }
    let name = place.name().map(str::to_string);
    let key = name.as_deref().unwrap_or("");
    let target = place.value().descalarize().clone();
    let saved_meta = interp.container_type_metadata(&target);
    let empty_what = empty_what(interp, name.as_deref());
    // `pop` has no end to reach on a lazy array.
    if op == Op::Pop
        && let Some(slot) = place.slot(interp)
        && let ValueView::Array(_, kind) = slot.view()
        && kind.is_lazy()
    {
        return Err(RuntimeError::cannot_lazy("pop"));
    }
    // Avoid `gc_data_mut` on an empty array: it would clone a shared node and
    // drop the native type metadata keyed by the old pointer, demoting
    // `array[num]` to a plain `Array`. The live array decides when the name
    // resolves to one, the receiver as the call read it otherwise.
    let live_empty = place.slot(interp).and_then(|slot| match slot.view() {
        ValueView::Array(a, _) => Some(a.is_empty()),
        _ => None,
    });
    let empty = live_empty
        .unwrap_or_else(|| matches!(target.view(), ValueView::Array(a, _) if a.is_empty()));
    if empty {
        return Ok(make_empty_array_failure_what(method, &empty_what));
    }
    let removed = with_array_data(interp, place, &target, |items, _| {
        if op == Op::Pop {
            items.pop()
        } else if items.is_empty() {
            None
        } else {
            Some(items.remove(0))
        }
    });
    let out = match removed {
        Some(Some(out)) => out,
        Some(None) => return Ok(make_empty_array_failure_what(method, &empty_what)),
        None => {
            // The name resolves to no array: remove from a copy of the receiver as
            // the call read it, and bind the rest under the name.
            let (mut items, kind) = match target.view() {
                ValueView::Array(v, kind) => (v.to_vec(), kind),
                _ => (Vec::new(), ArrayKind::Array),
            };
            if items.is_empty() {
                return Ok(make_empty_array_failure_what(method, &empty_what));
            }
            let out = if op == Op::Pop {
                items.pop().unwrap_or(Value::NIL)
            } else {
                items.remove(0)
            };
            let rebuilt = Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                kind,
            );
            place.assign(interp, rebuilt);
            out
        }
    };
    if name.is_some() {
        interp.reattach_array_type_metadata(key, &saved_meta);
    }
    Ok(out)
}

/// Resolve a splice position argument to a signed integer for validation.
// Cost: O(1).
fn resolve_splice_raw(v: &Value, len: usize) -> Option<i64> {
    match v.view() {
        ValueView::Int(i) => Some(i),
        _ if crate::runtime::utils::is_integer_value(v) => Some(crate::runtime::to_int(v)),
        ValueView::Whatever => Some(len as i64),
        ValueView::Str(s) => s.parse::<i64>().ok(),
        ValueView::Num(n) => Some(n as i64),
        // Allomorphic types (`IntStr`).
        ValueView::Mixin(inner, _) => resolve_splice_raw(inner, len),
        _ => None,
    }
}

/// Resolve the splice range `start..end` against `len`, and flatten the
/// replacement.
// Cost: O(r), r = replacement elements.
fn splice_plan(len: usize, args: &[Value]) -> (usize, usize, Vec<Value>) {
    let start = args
        .first()
        .and_then(|v| resolve_splice_raw(v, len))
        .unwrap_or(0)
        .max(0) as usize;
    let start = start.min(len);
    let count = args
        .get(1)
        .and_then(|v| resolve_splice_raw(v, len))
        .unwrap_or(len.saturating_sub(start) as i64)
        .max(0) as usize;
    let end = (start + count).min(len);
    // Collect the replacement BEFORE draining: the array is mutated in place
    // through the shared node (container identity §3), so a self-splice
    // replacement (`splice(@a, .., @a)`) aliases it and must be snapshotted
    // first. splice's own one-argument rule (a lone `Positional` flattens, several
    // arguments never do), ADR-0040 itemization and the ADR-0049 Nil-to-Any
    // decay all live in the one shared helper.
    let new_items = crate::runtime::flatten_splice_replacement_args(args.get(2..).unwrap_or(&[]));
    (start, end, new_items)
}

/// Whether `v` is a valid splice offset or size: an `Int`, `Whatever` or a
/// callable (`*-1`).
// Cost: O(1).
fn is_valid_splice_index(v: &Value) -> bool {
    match v.view() {
        ValueView::Whatever | ValueView::Sub(..) | ValueView::WeakSub(..) => true,
        ValueView::Mixin(inner, _) => is_valid_splice_index(inner),
        _ => crate::runtime::utils::is_integer_value(v),
    }
}

/// The `X::TypeCheck::Splice` of inserting `v` into an array of element type
/// `constraint`.
// Cost: O(1).
fn splice_type_error(constraint: &str, v: &Value, expected: Value) -> RuntimeError {
    RuntimeError::typed(
        "X::TypeCheck::Splice",
        [
            (
                "message".to_string(),
                // Rakudo's own wording names the operation and repeats the
                // type object's `.raku` in parentheses; the generic
                // element-store message ("for an element of @a") is a different
                // exception's text.
                Value::str(format!(
                    "Type check failed in splice; expected {} but got {} ({})",
                    constraint,
                    crate::runtime::utils::got_type_name(v),
                    crate::runtime::utils::got_type_name(v)
                )),
            ),
            ("action".to_string(), Value::str_from("splice")),
            (
                "got".to_string(),
                Value::package(crate::symbol::Symbol::intern(
                    &crate::runtime::utils::got_type_name(v),
                )),
            ),
            ("expected".to_string(), expected),
        ]
        .into_iter()
        .collect(),
    )
}

/// `splice(start?, count?, *@replacement)`: remove `count` elements at `start`,
/// insert the replacement there, and answer the removed elements as the same
/// typed container as the receiver.
// Cost: O(n + r + (e - s - n)), e = elements of the array, s = offset, n =
// removed, r = replacement elements; O(n + r) amortized at the front
// (`ArrayData::splice_live`).
fn splice(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let name = place.name().map(str::to_string);
    let key = name.as_deref().unwrap_or("");
    let target = place.value().descalarize().clone();
    let saved_meta = interp.container_type_metadata(&target);
    // Pre-resolve callable arguments (`*-3`) before borrowing the array
    // mutably: the length is the live binding's.
    let arr_len = match place.slot(interp).map(|v| v.view()) {
        Some(ValueView::Array(v, ..)) => v.len(),
        _ => match target.view() {
            ValueView::Array(v, ..) => v.len(),
            _ => 0,
        },
    };
    // A lazy replacement cannot be spliced in.
    {
        let type_name = match name.as_deref().and_then(|n| interp.var_type_constraint(n)) {
            Some(constraint)
                if crate::runtime::native_types::is_native_array_element_type(&constraint) =>
            {
                format!("array[{constraint}]")
            }
            _ => "Array".to_string(),
        };
        for arg in args.iter().skip(2) {
            let has_lazy = match arg.view() {
                ValueView::Array(items, _) => items
                    .iter()
                    .any(crate::builtins::methods_0arg::is_value_lazy),
                _ => crate::builtins::methods_0arg::is_value_lazy(arg),
            };
            if has_lazy {
                return Err(RuntimeError::typed(
                    "X::Cannot::Lazy",
                    [
                        (
                            "message".to_string(),
                            Value::str(format!("Cannot splice a lazy list into a {type_name}")),
                        ),
                        ("action".to_string(), Value::str_from("splice in")),
                    ]
                    .into_iter()
                    .collect(),
                ));
            }
        }
    }
    // Type-check the replacement against the array's declared element type,
    // after splice's one-argument rule has flattened it: the check runs on the
    // values `splice_plan` resolves and `splice_live` will actually store, so a
    // `Nil` replacement (decayed to `Any`) is checked as that and a lone
    // `Positional` contributes its elements while several contribute themselves.
    let replacement = crate::runtime::flatten_splice_replacement_args(args.get(2..).unwrap_or(&[]));
    let constraint = match name.as_deref() {
        Some(n) if n.starts_with('@') => interp.element_constraint_for(n, &target),
        _ => interp
            .container_type_metadata(&target)
            .map(|info| info.value_type),
    };
    if let Some(constraint) = constraint
        && !matches!(constraint.as_str(), "" | "Any" | "Mu")
    {
        for v in &replacement {
            if !interp.type_matches_value(&constraint, v) {
                let declared = interp
                    .container_type_metadata(&target)
                    .and_then(|info| info.declared_type);
                let on_variable = name
                    .as_deref()
                    .is_some_and(|n| interp.var_type_constraint(n).is_some());
                let expected_name = declared
                    .or_else(|| (!on_variable).then(|| "Array".to_string()))
                    .unwrap_or_else(|| {
                        if crate::runtime::native_types::is_native_array_element_type(&constraint) {
                            format!("array[{constraint}]")
                        } else {
                            format!("Array[{constraint}]")
                        }
                    });
                let expected = if on_variable {
                    Value::package(crate::symbol::Symbol::intern(&expected_name))
                } else {
                    Value::str(expected_name)
                };
                return Err(splice_type_error(&constraint, v, expected));
            }
        }
    }
    // Resolve a callable offset/count (`*-1`) against the array's length.
    let resolved_args = interp.resolve_splice_callable_args(arr_len, args);
    // The offset and size take `Int` (plus `Whatever`/`Callable`, resolved
    // above): a `Num`, `Str` or `Array` there matches no candidate and must throw
    // `X::Multi::NoMatch`, not coerce.
    for v in resolved_args.iter().take(2) {
        if !is_valid_splice_index(v) {
            return Err(
                crate::runtime::methods_signature_errors::make_multi_no_match_error("splice"),
            );
        }
    }
    Interpreter::validate_splice_range(arr_len, &resolved_args)?;
    let removed = splice_in_place(interp, place, &target, &resolved_args);
    if name.is_some() {
        interp.reattach_array_type_metadata(key, &saved_meta);
    }
    // Rakudo's `splice` answers the removed elements as the same typed
    // container as the receiver (`array[int]` splices to `array[int]`).
    let mut removed = Value::real_array(removed);
    if let Some(info) = &saved_meta {
        removed = interp.tag_container_metadata(removed, info.clone());
    }
    Ok(removed)
}

/// Splice the array the place resolves to, through its shared node, or rebuild
/// it under the name when it resolves to none.
// Cost: O(n + r + (e - s - n)), see `splice`.
fn splice_in_place(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    target: &Value,
    args: &[Value],
) -> Vec<Value> {
    if let Some(removed) = with_array_data(interp, place, target, |items, _| {
        let (start, end, new_items) = splice_plan(items.items().len(), args);
        items.splice_live(start, end, new_items)
    }) {
        return removed;
    }
    let mut items = match target.view() {
        ValueView::Array(v, ..) => v.to_vec(),
        _ => Vec::new(),
    };
    let (start, end, new_items) = splice_plan(items.len(), args);
    let removed: Vec<Value> = items.splice(start..end, new_items).collect();
    place.assign(interp, Value::real_array(items));
    removed
}
