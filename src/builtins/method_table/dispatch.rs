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
    let rerunnable = !row.flags.skips_cross_check();
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
    // #12456: user code runs over the elements, so run on a snapshot instead.
    let snapshot = if crate::value::container_lock::multi_mutator_threads_live()
        && crate::value::container_lock::is_snapshot_read(row.name)
    {
        crate::value::container_lock::shared_snapshot(target)
    } else {
        None
    };
    let target = snapshot.as_ref().unwrap_or(target);
    let answer = match row.handler {
        Handler::Pure(f) => Some((f(target, positional), rerunnable)),
        Handler::Narrow(f) => f(target, positional).map(|r| (r, rerunnable)),
        Handler::Named(f) => f(target, positional, named).map(|r| (r, rerunnable)),
        Handler::Interp(f) => f(interp?, target, positional, named).map(|r| (r, false)),
        // A row that writes through its receiver needs a place, which only
        // `invoke_mut` has: no shape lookup, lane or pure entry runs it.
        Handler::Mut(_) => None,
    };
    // #11217: a renderer (`.Str` / `.gist` / `.raku`) that ran out of native
    // stack truncated its text; raise instead of answering with it.
    answer
        .map(|(result, rerunnable)| (crate::value::render_guard::check_render(result), rerunnable))
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

/// Answer a call on a value the table has no shape for whose structure is
/// `shape`'s (a grammar cursor or an instance of a `Match` subclass reads as a
/// plain `Match`): the row a receiver of `shape` reaches, if it admits `args`.
/// A row that needs the interpreter declines.
// Cost: O(1) to find the row (one hash lookup), plus O(a) to admit a
// arguments, plus the handler's own cost.
pub(crate) fn answer_as(
    shape: DispatchShape,
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let id = super::resolve(Receiver::instance(shape), method, args.len())?;
    if !admits(id, args) {
        return None;
    }
    invoke(id, target, args)
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
    // `Any` ends every owner chain: the proto a kind with no mutator of its
    // own falls back to (`Any.splice`).
    let owners = mutating::owners_of(place.value(), place.name().is_some()).unwrap_or(&[]);
    let id = owners
        .iter()
        .chain(&["Any"])
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
    let payload;
    let (target, id) = match Receiver::of_settled(target) {
        Some(receiver) => (target, super::resolve(receiver, method, arity)?),
        // The native-base resolution (§9.79): an instance of a user class
        // deriving from a built-in scalar type has no shape of its own, and is
        // answered by the rows of its payload's shape, the payload as invocant.
        None => {
            payload = crate::builtins::native_base::redirect(target, method.as_str())?;
            let receiver = Receiver::of_settled(&payload)?;
            (&payload, super::resolve(receiver, method, arity)?)
        }
    };
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

/// `Iterable`'s methods a receiver with no shape reaches through the row of
/// the built-in class it is a form of ([`answer_shapeless`]): Rakudo declares
/// them on `List`, `Seq` and `Map`, not on `Any` (`5.lazy` is
/// `X::Method::NotFound`).
const ITERABLE_FOR_SHAPELESS: [&str; 3] = ["lazy", "race", "hyper"];

/// Answer a call the cascades declined from the row `Mu`, `Cool`, `Any`,
/// `List` or `Seq` declares, for a receiver with no shape (ADR-11276 §9.79,
/// §9.80, §9.81).
///
/// - `Cool`'s audited text methods ([`crate::value::COOL_TEXT_FOR_CLOSED_SHAPES`])
///   answer a shapeless `Cool`: an enum value, a `Seq` or `Slip` the table has
///   no settled shape for, or an instance whose MRO reaches `Cool`.
/// - `Any`'s list methods ([`crate::value::ANY_LIST_FOR_CLOSED_SHAPES`]) answer
///   those and every other receiver the cascades declined: a code object, a
///   regex, a promise, a RakuAST node, a type object with no shape, a shaped
///   or lazy array, an instance. `join` takes its separator.
/// - `Mu`'s answers ([`crate::value::MU_FOR_CLOSED_SHAPES`]: `so`, `not`,
///   `is-lazy`) answer every receiver, a `Failure` included (asking its truth
///   handles it); an instance answers from the first class in its MRO that
///   declares the method.
/// - `Iterable`'s `lazy` ([`ITERABLE_FOR_SHAPELESS`]) answers a `LazyList`
///   or unsettled `Seq` from `Seq`'s row and a lazy or shaped array or a
///   `Slip` from `List`'s.
///
/// An instance walks its class's MRO and declines when a built-in class ahead
/// of `Cool`/`Any` declares the method (`class MyPath is IO::Path` reads its
/// file with `IO::Path.lines`). The handlers read the receiver's string form
/// or its list, which is all the name-matching arms they replace did. It runs
/// after the cascades, so a receiver-specific answer there (an enum's `sign`,
/// a `Cool` subclass's own `Numeric`, a mixin's `flat`) comes first. An
/// instance is asked of the interpreter's MRO, so without one it declines; a
/// `Failure` must explode and is never answered.
// Cost: O(m) to test the name, m = audited names, plus O(k) to find the row,
// k = rows named `method`; for an instance one memoized MRO read and O(d * k)
// to walk it, d = MRO depth; plus the handler's own cost.
pub(crate) fn answer_shapeless(
    interp: Option<&mut Interpreter>,
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if args.is_empty()
        && let Some(id) = super::collections::type_params::type_object_row(target, method)
    {
        return call(row(id), interp, target, args, Named::NONE).map(|(result, _)| result);
    }
    let id = shapeless_row(interp, target, method, args.len())?;
    call(row(id), None, target, args, Named::NONE).map(|(result, _)| result)
}

/// The row [`answer_shapeless`] answers a call with `arity` positional
/// arguments from, without running it.
// Cost: as [`answer_shapeless`], without the handler.
pub(crate) fn shapeless_row(
    interp: Option<&mut Interpreter>,
    target: &Value,
    method: Symbol,
    arity: usize,
) -> Option<RowId> {
    use crate::value::ValueView;
    // `join` (its separator), `pick`, `roll`, `head` and `tail` (their count)
    // alone take an argument.
    if arity > 0
        && (arity > 1 || !matches!(method.as_str(), "join" | "pick" | "roll" | "head" | "tail"))
    {
        return None;
    }
    let text = crate::value::COOL_TEXT_FOR_CLOSED_SHAPES.contains(&method.as_str());
    let list = crate::value::ANY_LIST_FOR_CLOSED_SHAPES.contains(&method.as_str());
    let mu = crate::value::MU_FOR_CLOSED_SHAPES.contains(&method.as_str());
    let iterable = ITERABLE_FOR_SHAPELESS.contains(&method.as_str());
    if !(text || list || mu || iterable) || Receiver::of_settled(target).is_some() {
        return None;
    }
    let (cool, any) = (Symbol::intern("Cool"), Symbol::intern("Any"));
    let mu_owner = Symbol::intern("Mu");
    let owner = match target.view() {
        // `Iterable`'s methods: a list still being produced is a `Seq`, a
        // lazy or shaped array a `List`; nothing else declares them.
        ValueView::LazyList(_)
        | ValueView::Seq(_)
        | ValueView::HyperSeq(_)
        | ValueView::RaceSeq(_)
            if iterable =>
        {
            Symbol::intern("Seq")
        }
        ValueView::Array(..) | ValueView::Slip(_) if iterable => Symbol::intern("List"),
        // The `Iterable` role's type object lists as itself, as Rakudo's
        // `Iterable.hyper` does (the `hyperize` dist reads
        // `Iterable.hyper.configuration`).
        ValueView::Package(name) if iterable && name == "Iterable" => Symbol::intern("List"),
        _ if iterable => return None,
        // `Mu`'s answers reach a `Failure` too: asking its truth handles it.
        ValueView::Instance { class_name, .. } if mu && class_name == "Failure" => mu_owner,
        ValueView::Instance { class_name, .. } if mu => {
            // The first class in the MRO that declares the method answers it
            // (`class M is Match` keeps `Match.not`); `Mu` ends every MRO.
            interp?
                .class_mro(class_name.as_str())
                .iter()
                .copied()
                .find(|&class| super::owner_row(class, method, 0).is_some())
                .unwrap_or(mu_owner)
        }
        _ if mu => mu_owner,
        ValueView::Enum { .. }
        | ValueView::LazyList(_)
        | ValueView::Seq(_)
        | ValueView::HyperSeq(_)
        | ValueView::RaceSeq(_)
        | ValueView::Slip(_) => {
            if text {
                cool
            } else {
                any
            }
        }
        ValueView::Instance { class_name, .. } if class_name != "Failure" => {
            let mut owner = None;
            for &class in interp?.class_mro(class_name.as_str()).iter() {
                if (class == cool && text) || (class == any && list) {
                    owner = Some(class);
                    break;
                }
                if super::owner_row(class, method, 0).is_some() {
                    break;
                }
            }
            owner?
        }
        ValueView::Instance { .. } => return None,
        // `Mu` is no `Any`: `Mu.flat` and `Mu.pick` have no candidate.
        ValueView::Package(name) if name == "Mu" => return None,
        // Every other receiver the cascades declined (a code object, a regex,
        // a promise, a RakuAST node, a type object with no shape, a shaped or
        // lazy array) lists as itself or its elements.
        _ if list => any,
        _ => return None,
    };
    super::owner_row(owner, method, arity)
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
        // The two answers are computed one after the other, so another thread
        // writing the receiver in between makes them differ legitimately. The
        // net is for the single-threaded suite; once a second VM mutator thread
        // exists the comparison proves nothing (#11701).
        if crate::value::container_lock::multi_mutator_threads_live() {
            return;
        }
        let slow = match args {
            [] => super::super::methods_0arg::native_method_0arg_cascade(target, method),
            [a] => super::super::native_method_1arg_without_table(target, method, a),
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
