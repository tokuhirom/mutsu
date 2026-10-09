//! Calling a row: the guard step and the entries (ADR-11276 §2.4).
//!
//! A call reaches a row only through [`dispatch`], which decodes the receiver,
//! finds the row, admits the arguments and splits the named ones from the
//! positional ones, once, whichever entry the call came through. The checks a
//! row's handler used to repeat (or silently skip) live here.

use super::{Handler, MethodRow, Named, Receiver, ReceiverPlace, RowFlags, RowId, mutating, row};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{DispatchShape, RuntimeError, Value};

/// What a row answered: its result, and whether the debug cross-check may
/// re-run the call (the handler is pure and its answer is not random).
type Answer = (Result<Value, RuntimeError>, bool);

/// Run row `id`'s handler on already-admitted arguments, or `None` when a
/// narrowing row does not bind them. `target` must have the shape the row
/// was resolved for. An [`Handler::Interp`] row needs `interp`; without one
/// it declines.
// Cost: O(1) plus the handler's own cost.
#[inline]
fn call(
    row: &MethodRow,
    interp: Option<&mut Interpreter>,
    target: &Value,
    positional: &[Value],
    named: Named<'_>,
) -> Option<Answer> {
    // A random answer cannot be checked by running the call a second time.
    let rerunnable = !row.flags.contains(RowFlags::RANDOM);
    // ADR-0068: a leaf READ of a container (`%!h.keys`) walks the backing map,
    // which a concurrent structural write on another thread reallocates (#11701).
    // A no-op (one relaxed load) until a second VM mutator thread exists, and
    // for a receiver that is no `Hash`/`Array`.
    let _read_exclusion = if crate::value::container_lock::multi_mutator_threads_live()
        && crate::value::container_lock::is_leaf_structure_read(row.name)
    {
        crate::value::container_lock::ContainerStructGuard::acquire_for(None, target)
    } else {
        None
    };
    match row.handler {
        Handler::Pure(f) => Some((f(target, positional), rerunnable)),
        Handler::Narrow(f) => f(target, positional).map(|r| (r, rerunnable)),
        Handler::Named(f) => f(target, positional, named).map(|r| (r, rerunnable)),
        Handler::Interp(f) => f(interp?, target, positional, named).map(|r| (r, false)),
        // A row that writes through its receiver needs a place, which only
        // `invoke_mut` has: no shape lookup, lane or pure entry runs it.
        Handler::Mut(_) => None,
    }
}

/// Run row `id`'s handler, or `None` when a narrowing row does not bind
/// these arguments. `args` must have the row's arity, no named argument, and
/// be admitted by [`admits`]; `target` must have the shape the row was
/// resolved for. A row that needs the interpreter declines.
// Cost: O(1) plus the handler's own cost.
#[inline]
pub(crate) fn invoke(
    id: RowId,
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    call(row(id), None, target, args, Named::NONE).map(|(result, _)| result)
}

/// [`invoke`] for a row that may need the interpreter.
// Cost: O(1) plus the handler's own cost.
#[inline]
pub(crate) fn invoke_in(
    id: RowId,
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    call(row(id), Some(interp), target, args, Named::NONE).map(|(result, _)| result)
}

/// Answer `method` from the first row any of `owners` declares for it, for a
/// receiver the table has no shape for (an instance of a user subclass of a
/// built-in class, which the instance dispatch reaches by walking its MRO to
/// the owner) or a call the guard step declined (an argument or a named
/// argument it does not admit). `target` is built only once a row exists, so a
/// method without one pays a bit test.
///
/// The arguments are not admitted: this is the slow path, which accepted any
/// argument before the row existed, and a named argument the row does not
/// declare was stripped by the caller (ADR-0070). `None` when no owner has a
/// row for the call, or a narrowing row does not bind the arguments.
// Cost: O(o) to find the row, o = owners, plus O(a) to split a arguments, plus
// the handler's own cost.
pub(crate) fn invoke_owner(
    interp: &mut Interpreter,
    owners: &[&str],
    method: &str,
    args: &[Value],
    target: impl FnOnce() -> Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = Symbol::intern(method);
    let named_count = args.iter().filter(|arg| arg.is_string_pair_value()).count();
    let arity = args.len() - named_count;
    let id = owners
        .iter()
        .find_map(|owner| super::owner_row(Symbol::intern(owner), method, arity))?;
    let row = row(id);
    let target = target();
    if named_count == 0 {
        return call(row, Some(interp), &target, args, Named::NONE).map(|(result, _)| result);
    }
    let (named, positional): (Vec<Value>, Vec<Value>) = args
        .iter()
        .cloned()
        .partition(|arg| arg.is_string_pair_value());
    call(row, Some(interp), &target, &positional, Named::new(&named)).map(|(result, _)| result)
}

/// [`invoke_owner`] for a metaobject call (ADR-11276 slice 3G): `args` is the
/// type object followed by the call's own arguments, every one positional. A
/// metamethod reads its flags itself (`.^methods(:all)`), so no argument is
/// split off as named and the row is found by the full count. The target is
/// `args[0]`.
// Cost: O(o) to find the row, o = owners, plus the handler's own cost.
pub(crate) fn invoke_owner_raw(
    interp: &mut Interpreter,
    owners: &[&str],
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let method = Symbol::intern(method);
    let id = owners
        .iter()
        .find_map(|owner| super::owner_row(Symbol::intern(owner), method, args.len()))?;
    call(row(id), Some(interp), args.first()?, args, Named::NONE).map(|(result, _)| result)
}

/// Answer `method` from the [`RowFlags::DEFERRAL_BASE`] row `owner` declares:
/// the last candidate of a user override's `callsame`/`nextsame` chain.
/// `args` is the receiver followed by the override's own arguments, named ones
/// included as pairs. `None` when `owner` has no such row, or the handler
/// declines the receiver.
// Cost: O(k) to find the row, k = rows named `method`, plus the handler's own
// cost.
pub(crate) fn invoke_base(
    interp: &mut Interpreter,
    owner: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let id = super::base_row(Symbol::intern(owner), Symbol::intern(method))?;
    call(row(id), Some(interp), args.first()?, args, Named::NONE).map(|(result, _)| result)
}

/// Answer a receiver-mutating method from its row, or `None` to take the
/// cascades. The one entry a [`Handler::Mut`] row has: the caller names the
/// receiver's [`ReceiverPlace`], and the row is found by the owner chain of
/// the receiver's value kind (`Array` then `List`, `BagHash`, ...), not by a
/// shape, because a mutator is not answered by a pure entry, a call-site lane
/// or the debug cross-check (a second run would apply the mutation twice).
///
/// The guard step of this entry: a named argument the row does not declare is
/// dropped (a method has the implicit `*%_`, ADR-0070), the row is found by
/// the positional arity, and no argument is admitted or refused, because a
/// mutator reads its arguments raw (`@a.push(@b)`, `$bag.add(<a b>)`). A
/// handler declines with `None` for a call outside its signature.
// Cost: O(1) when no mutating row has the name (a bit test); otherwise O(a) to
// split a arguments, O(o) owner lookups, plus the handler's own cost.
#[inline]
pub(crate) fn invoke_mut(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let named_count = args.iter().filter(|arg| arg.is_string_pair_value()).count();
    let arity = args.len() - named_count;
    if !super::table::names_a_mut_row(method, arity) {
        return None;
    }
    let owners = mutating::owners_of(place.value(), place.name().is_some())?;
    let id = owners
        .iter()
        .find_map(|owner| super::owner_row(Symbol::intern(owner), method, arity))?;
    let row = row(id);
    let Handler::Mut(handler) = row.handler else {
        return None;
    };
    // An `augment` or `.wrap` of the receiver's type that defines the method
    // takes the call from the row, which has effects and so must not run first.
    if interp.native_lever_a_user_override_sym(place.value().descalarize(), method) {
        return None;
    }
    // ADR-0068 §4 step 3: a mutating method on a container two threads can
    // reach (`$obj.attr.push($v)`) restructures the backing node, so the row
    // runs under the container-structure lock keyed on that node. Taken here,
    // at the one entry a `Mut` row has, so every caller (the VM's opcodes, the
    // by-name and by-value entries) is excluded alike. A no-op until a second
    // VM mutator thread is spawned, and nested acquisition is a no-op too.
    let _exclusion =
        crate::value::container_lock::ContainerStructGuard::acquire_for(None, place.value());
    if named_count == 0 {
        return handler(interp, place, args, Named::NONE);
    }
    let (named, positional): (Vec<Value>, Vec<Value>) = args
        .iter()
        .cloned()
        .partition(|arg| arg.is_string_pair_value());
    let named: Vec<Value> = named
        .into_iter()
        .filter(|pair| names_declared(row, pair))
        .collect();
    handler(interp, place, &positional, Named::new(&named))
}

/// Whether `arg` is a plain scalar a row may be handed by default: a `Str`
/// or a number.
// Cost: O(1), a tag probe.
#[inline]
fn is_plain_scalar(arg: &Value) -> bool {
    matches!(
        arg.dispatch_shape(),
        Some(
            DispatchShape::Str
                | DispatchShape::Int
                | DispatchShape::Num
                | DispatchShape::Rat
                | DispatchShape::FatRat
        )
    )
}

/// Whether every positional argument is one row `id` may be handed: a plain
/// scalar by default, or any plain argument for a row flagged
/// [`RowFlags::ANY_ARGS`]. A `Junction` (which must autothread), a `Failure`
/// (which may explode under `use fatal`), a lazy `Seq` (which must be
/// reified), a container and a proxy each need a probe the table skips, so a
/// call carrying any of them takes the cascades.
// Cost: O(a), a = arguments (one tag probe each).
#[inline]
pub(crate) fn admits(id: RowId, args: &[Value]) -> bool {
    admits_row(row(id), args)
}

#[inline]
fn admits_row(row: &MethodRow, args: &[Value]) -> bool {
    if row.flags.contains(RowFlags::ANY_ARGS) {
        args.iter().all(Value::is_plain_argument)
    } else {
        args.iter().all(is_plain_scalar)
    }
}

/// The one guard step: find the row a call dispatches to, admit its
/// arguments, split the named ones, and run the handler. `None` takes the
/// cascades, always safe: the call then walks them exactly as it did before
/// the table existed.
// Cost: O(1) to find the row (a bit test, a tag probe and one hash lookup),
// plus O(a) to admit and split a arguments, plus the handler's own cost.
#[inline]
fn dispatch(
    mut interp: Option<&mut Interpreter>,
    allow: impl FnOnce(&mut Interpreter) -> bool,
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Answer> {
    // Named-ness is a call-site property carried by the value's flavour
    // (ADR-0021): a string-keyed `Pair` is a named argument, and a row is
    // looked up by its positional arity.
    let named_count = args.iter().filter(|arg| arg.is_string_pair_value()).count();
    let arity = args.len() - named_count;
    if !super::names_a_row(method, arity) {
        return None;
    }
    let receiver = Receiver::of_settled(target)?;
    let id = super::resolve(receiver, method, arity)?;
    let row = row(id);
    // The caller's veto, asked once the row is known and before the handler
    // runs: an interpreter row has effects, so it must not run and then be
    // discarded.
    if let Some(interp) = interp.as_deref_mut()
        && !allow(interp)
    {
        return None;
    }
    if named_count == 0 {
        // After the lookup: a call that misses never pays for the argument scan.
        if !admits_row(row, args) {
            return None;
        }
        return call(row, interp, target, args, Named::NONE);
    }
    let (named, positional): (Vec<Value>, Vec<Value>) = args
        .iter()
        .cloned()
        .partition(|arg| arg.is_string_pair_value());
    if !named.iter().all(|pair| names_declared(row, pair)) || !admits_row(row, &positional) {
        return None;
    }
    call(row, interp, target, &positional, Named::new(&named))
}

/// Whether the named argument `pair` is one `row` binds.
// Cost: O(n), n = names the row declares.
fn names_declared(row: &MethodRow, pair: &Value) -> bool {
    if row.flags.contains(RowFlags::ANY_NAMED) {
        return true;
    }
    match pair.view() {
        crate::value::ValueView::Pair(key, _) => row.named.contains(&key.as_str()),
        _ => false,
    }
}

/// Answer a built-in method call from its row, or `None` to take the
/// cascades. Pure: a row that needs the interpreter declines.
///
/// `None` is always safe: the call then walks the cascades exactly as it did
/// before the table existed.
// Cost: O(1) to find the row (a bit test, a tag probe and one hash lookup),
// plus the handler's own cost.
#[cfg(test)]
pub(crate) fn try_dispatch(
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let (result, rerunnable) = dispatch(None, |_| true, target, method, args)?;
    if rerunnable {
        debug_assert_matches_full_path(target, method, args, &result);
    }
    Some(result)
}

/// `try_dispatch` for a caller that has the interpreter, so a row that
/// needs it answers too. `allow` is the caller's veto (an `augment` of the
/// receiver's type that defines the method, for one), asked once the row is
/// found and before its handler runs. Only a pure row is cross-checked in debug builds: an
/// interpreter row has effects (closure calls, dynamic-variable reads, I/O),
/// which running it a second time would repeat.
// Cost: O(1) to find the row (a bit test, a tag probe and one hash lookup),
// plus the handler's own cost.
#[inline]
pub(crate) fn try_dispatch_in(
    interp: &mut Interpreter,
    allow: impl FnOnce(&mut Interpreter) -> bool,
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let (result, pure) = dispatch(Some(interp), allow, target, method, args)?;
    if pure {
        debug_assert_matches_full_path(target, method, args, &result);
    }
    Some(result)
}

/// `try_dispatch` without the debug cross-check: what the cascades' own
/// entry (`native_method_0arg`) asks first.
// Cost: O(1) to find the row (a bit test, a tag probe and one hash lookup),
// plus the handler's own cost.
#[inline]
pub(crate) fn answer(
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    dispatch(None, |_| true, target, method, args).map(|(result, _)| result)
}

/// In debug builds, re-answer a table hit through the cascades and assert
/// the two agree.
///
/// This is the maintenance net for the table. A row is admitted by an argument
/// about which skipped probes could claim the call, and a later commit adding
/// a probe has no way of knowing it invalidated one; running both paths over
/// the whole TAP suite turns that silent divergence into a failing assertion.
/// A cascade that declines agrees: a migrated method has no arm left there.
/// Sound to run twice only because every pure row is side-effect free.
fn debug_assert_matches_full_path(
    target: &Value,
    method: Symbol,
    args: &[Value],
    fast: &Result<Value, RuntimeError>,
) {
    #[cfg(debug_assertions)]
    {
        let slow = match args {
            [] => super::super::methods_0arg::native_method_0arg_cascade(target, method),
            [a] => super::super::native_method_1arg(target, method, a),
            [a, b] => super::super::native_method_2arg(target, method, a, b),
            _ => return,
        };
        let render = |r: Option<&Result<Value, RuntimeError>>| match r {
            None => "<declined>".to_string(),
            Some(Ok(v)) => format!("ok:{}", crate::runtime::gist_value(v)),
            Some(Err(e)) => format!("err:{}", e.message),
        };
        let Some(slow) = slow else {
            return;
        };
        debug_assert_eq!(
            render(Some(fast)),
            render(Some(&slow)),
            "method_table row disagrees with the full path for .{} on a {:?} \
             receiver -- a probe the table skips now claims this call",
            method.as_str(),
            target.dispatch_shape(),
        );
    }
    #[cfg(not(debug_assertions))]
    {
        let _ = (target, method, args, fast);
    }
}
