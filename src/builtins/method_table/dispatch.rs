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
        && crate::value::container_read::is_leaf_structure_read(row.name)
    {
        crate::value::container_lock::ContainerStructGuard::acquire_for(None, target)
    } else {
        None
    };
    // `Any`'s methods written on `self.iterator` read the elements of a
    // receiver whose class declares its own `iterator` (`IO::Path::Parts`'s
    // pairs), not the receiver as one item.
    let iterated = iterated_receiver(row, target);
    let target = iterated.as_ref().unwrap_or(target);
    // #12456: user code runs over the elements, so run on a snapshot instead.
    let snapshot = crate::value::container_read::snapshot_read_receiver(row.name, target);
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
    answer.map(|(result, rerunnable)| {
        let result =
            result.map(|value| super::native_array_result::retype(target, row.name, value));
        (crate::value::render_guard::check_render(result), rerunnable)
    })
}

/// The List of elements `Any`'s row reads from a built-in receiver with an
/// `iterator` of its own (an `IO::Path::Parts` iterates its three pairs), when
/// `row` is one of `Any`'s methods written on `self.iterator`; `None` for every
/// other row or receiver.
// Cost: O(1) for any row but `Any`'s; O(m) name scan otherwise, m = 18.
fn iterated_receiver(row: &MethodRow, target: &Value) -> Option<Value> {
    if row.owner != "Any" {
        return None;
    }
    let pairs = super::io_path_parts_pairs(target)?;
    super::ANY_VIA_ITERATOR
        .contains(&row.name)
        .then(|| Value::array(pairs))
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
    let Some(id) = owners
        .iter()
        .find_map(|owner| super::owner_row(Symbol::intern(owner), method, arity))
    else {
        // The invocant binds before the arguments do: a `:D` row of another
        // arity still refuses a type object (`Supply.start`).
        let target = target();
        let crate::value::ValueView::Package(name) = target.view() else {
            return None;
        };
        let refusal = owners.iter().find_map(|owner| {
            (0..=7).find_map(|arity| {
                let row = row(super::owner_row(Symbol::intern(owner), method, arity)?);
                (!row.flags.contains(RowFlags::TYPE_OBJECT_OK))
                    .then(|| row.type_object_refusal(name.as_str()))
                    .flatten()
            })
        })?;
        return Some(Err(refusal));
    };
    let row = row(id);
    let target = target();
    // A type object reaches only a row that admits one, as through the guard
    // step: an instance row's handler never sees one (ADR-11276 §9.123).
    if !row.flags.contains(RowFlags::TYPE_OBJECT_OK)
        && let crate::value::ValueView::Package(name) = target.view()
    {
        // A `:D` invocant refuses it (`X::NYI.throw`).
        return row.type_object_refusal(name.as_str()).map(Err);
    }
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
    // A container type object (`Array.pop`, `SetHash.set`) is refused by the
    // rows Rakudo declares for a defined invocant only (#13881); the others
    // (`Array.push`, which vivifies a type object a binding holds) go on.
    if let Some(type_owners) = mutating::type_object_owners(place.value())
        && let Some(id) = type_owners
            .iter()
            .find_map(|owner| super::owner_row(Symbol::intern(owner), method, arity))
        && let crate::value::ValueView::Package(type_name) = place.value().descalarize().view()
        && let Some(refusal) = row(id).type_object_refusal(&type_name.resolve())
    {
        // A multi's no-match error names the call's argument types, as
        // Rakudo's does (`STORE(Array:U: List:D)`).
        if row(id).flags.contains(RowFlags::DEFINITE_MULTI) {
            let profile = interp.format_call_arg_profile(args);
            return Some(Err(
                crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
                    row(id).name,
                    &type_name.resolve(),
                    false,
                    &profile,
                    &[],
                ),
            ));
        }
        return Some(Err(refusal));
    }
    // `Any` ends every owner chain: the proto a kind with no mutator of its
    // own falls back to (`Any.splice`).
    let owners = mutating::owners_of(place.value()).unwrap_or(&[]);
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
    //
    // A receiver reached through a `:=`-bound cell (`my $c := @a; $c.push`)
    // is keyed on that cell, as the element store through it is
    // (`env_root_descended_mut_tracked`): threads sharing one cell hold
    // different nodes once `Gc::make_mut` copies an aliased one, so a
    // node-keyed lock here excluded nothing against `$c[$i] = v` (#13016).
    let _exclusion = if crate::value::container_lock::multi_mutator_threads_live() {
        // The node is re-read under the stripes: another thread's row can
        // re-seat the cell between the first read and the acquisition, and a
        // stripe held for the old node excludes no reader of the new one.
        match place.binding_cell(interp) {
            Some((cell, held)) => {
                let mut first = Some(held);
                crate::value::container_lock::ContainerStructGuard::acquire_for_cell_holding(
                    cell,
                    || match first.take() {
                        Some(held) => held,
                        None => place
                            .binding_cell(interp)
                            .map_or(Value::NIL, |(_, held)| held),
                    },
                )
            }
            None => {
                crate::value::container_lock::ContainerStructGuard::acquire_for(None, place.value())
            }
        }
    } else {
        None
    };
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
    if row.flags.contains(RowFlags::INDEX_ARG) {
        // Any other index is bound through its `.Int` (`index_arg`).
        args.iter()
            .all(|arg| arg.dispatch_shape() == Some(DispatchShape::Int))
    } else if row.flags.contains(RowFlags::VM_ARRAY_ARGS) {
        args.iter()
            .all(|arg| arg.is_vm_array() || arg.is_plain_argument())
    } else if row.flags.contains(RowFlags::ANY_ARGS) {
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
    let allomorph = target;
    let (target, id) = match Receiver::of_settled(target) {
        Some(receiver) => match super::resolve(receiver, method, arity) {
            Some(id) => (target, id),
            None => {
                let refusal = type_object_refusal(receiver, target, method, arity)?;
                if let Some(interp) = interp.as_deref_mut()
                    && !allow(interp)
                {
                    return None;
                }
                return Some((Err(refusal), false));
            }
        },
        // A type object with no shape of its own whose instances have one
        // (`UInt`, the one built-in subset, is an `Int`; `IO::Path::Unix` is an
        // `IO::Path`) is refused by the rows its instances reach.
        None if let Some(shape) = instance_shape_of_type_object(target) => {
            let refusal = type_object_refusal(Receiver::type_object(shape), target, method, arity)?;
            if let Some(interp) = interp.as_deref_mut()
                && !allow(interp)
            {
                return None;
            }
            return Some((Err(refusal), false));
        }
        // The native-base resolution (§9.79): an instance of a user class
        // deriving from a built-in scalar type has no shape of its own, and is
        // answered by the rows of its payload's shape, the payload as invocant.
        None => match crate::builtins::native_base::redirect(target, method.as_str()) {
            // A `Seq` payload is consumed through the interpreter (its
            // iterator is user code): the instance dispatch redirects it.
            Some(base) if base.is_seq_value() => return None,
            // An `is Range`/`is Seq` subclass with its own `iterator` lists
            // through it, which the interpreter does (#15361).
            Some(_)
                if interp.as_deref_mut().is_some_and(|i| {
                    i.payload_instance_lists_own_iterator(target, method.as_str())
                }) =>
            {
                return None;
            }
            Some(base) => {
                payload = base;
                let receiver = Receiver::of_settled(&payload)?;
                (&payload, super::resolve(receiver, method, arity)?)
            }
            // A grammar cursor or an instance of a `Match` subclass has no
            // shape, but its structure is a `Match`'s: it reads as one.
            None if target.is_match_instance() => (
                target,
                super::resolve(Receiver::instance(DispatchShape::Match), method, arity)?,
            ),
            // An allomorph is answered on the part the row reads (§9.101).
            None => {
                let (part, id) = super::allomorph::redirect(target, method, arity)?;
                payload = part;
                (&payload, id)
            }
        },
    };
    // A receiver state the receiver step decides (a container, a hash-like
    // receiver's or a Match's `self.list` methods, ...) is the entry's to
    // normalize first (`receiver_step`); the row would read it as one item.
    if super::receiver_step::claims(allomorph, method.as_str()) {
        return None;
    }
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
        let answer = call(row, interp, target, args, Named::NONE);
        return super::allomorph::keep_allomorph(allomorph, target, method, args.len(), answer);
    }
    let (named, positional): (Vec<Value>, Vec<Value>) = args
        .iter()
        .cloned()
        .partition(|arg| arg.is_string_pair_value());
    if !admits_row(row, &positional) {
        return None;
    }
    let named = bind_named(row, named)?;
    call(row, interp, target, &positional, Named::new(&named))
}

/// The refusal a type object `receiver` gets for a call no row admitting a
/// type object answers: the one of the row an instance of its shape reaches,
/// when that row's invocant is `:D` (`RowFlags::DEFINITE`,
/// `RowFlags::DEFINITE_MULTI`). `None` for an instance receiver or a row that
/// says nothing about a type object.
// Cost: O(1) to find the row (one hash lookup), plus the message's.
fn type_object_refusal(
    receiver: Receiver,
    target: &Value,
    method: Symbol,
    arity: usize,
) -> Option<RuntimeError> {
    if !receiver.type_object {
        return None;
    }
    let crate::value::ValueView::Package(name) = target.view() else {
        return None;
    };
    // The invocant binds before the arguments do, so a row of any arity
    // refuses the type object (`IO::Path.split` with no argument).
    let instance = Receiver::instance(receiver.shape);
    let id = std::iter::once(arity)
        .chain(0..=7)
        .find_map(|arity| super::resolve(instance, method, arity))?;
    row(id).type_object_refusal(name.as_str())
}

/// The shape of the instances of the type object `target` when it has none
/// of its own: `UInt` (a subset of `Int`) and the classes whose instances
/// share a shape (`IO::Path::Unix`).
// Cost: O(s), s = classes with an instance shape.
fn instance_shape_of_type_object(target: &Value) -> Option<DispatchShape> {
    let crate::value::ValueView::Package(name) = target.view() else {
        return None;
    };
    if name == "UInt" {
        return Some(DispatchShape::Int);
    }
    DispatchShape::from_instance_class(name)
}

/// `Iterable`'s methods a receiver with no shape reaches through the row of
/// the built-in class it is a form of ([`answer_shapeless`]): Rakudo declares
/// them on `List`, `Seq` and `Map`, not on `Any` (`5.lazy` is
/// `X::Method::NotFound`).
const ITERABLE_FOR_SHAPELESS: [&str; 4] = ["lazy", "race", "hyper", "lazy-if"];

/// The container methods `Array` and `Hash` declare, which an array or hash
/// with no settled shape (a typed, shaped or lazy one) answers from its own
/// type's row ([`answer_shapeless`]; ADR-11276 §9.163).
const CONTAINER_FOR_SHAPELESS: [&str; 1] = ["default"];

/// `Cool`'s coercions a receiver with no shape reaches ([`answer_shapeless`];
/// ADR-11276 §9.92): an enum, a lazy or shaped array, an unsettled `Seq`, an
/// instance whose MRO reaches `Cool` with no built-in class declaring the name
/// first, and a `Cool` type object with no shape.
const COOL_COERCE_FOR_SHAPELESS: [&str; 7] =
    ["Real", "Num", "UInt", "Int", "Numeric", "Rat", "FatRat"];

/// Answer a call by the first row of `owners` that binds it (ADR-11276
/// §9.129; an owner's rows at the call's arity in turn, §9.135): the
/// receiver's MRO as `.^can` walks it, with the interpreter,
/// after every receiver-specific answer declined. A row binds when it takes
/// the call's arity, admits its arguments and named ones as the guard step
/// would, admits a type object only with `TYPE_OBJECT_OK`, and its handler
/// does not decline; otherwise the walk goes on to the next owner, as a
/// multi candidate of a parent class is still a candidate in Rakudo
/// (`Range.contains(/re/)` is `Cool`'s).
// Cost: O(o * k) to find the rows, o = owners, k = rows named `method`, plus
// O(a) per row to admit a arguments, plus the handlers' own cost.
pub(crate) fn invoke_mro(
    interp: &mut Interpreter,
    owners: &[&str],
    method: &str,
    args: &[Value],
    target: &Value,
) -> Option<Result<Value, RuntimeError>> {
    walk_mro(interp, owners, method, args, target, false)
}

/// [`invoke_mro`] over the late rows only (ADR-11276 §9.138): the
/// `OWNER_ONLY` interpreter rows a cascade's last resort reaches, after every
/// other answer, including the owners' own pure rows, declined. A call no late
/// row binds is `None`, never `X::Multi::NoMatch`: the cascade goes on.
// Cost: as [`invoke_mro`].
pub(crate) fn invoke_mro_late(
    interp: &mut Interpreter,
    owners: &[&str],
    method: &str,
    args: &[Value],
    target: &Value,
) -> Option<Result<Value, RuntimeError>> {
    walk_mro(interp, owners, method, args, target, true)
}

// Cost: as [`invoke_mro`].
fn walk_mro(
    interp: &mut Interpreter,
    owners: &[&str],
    method: &str,
    args: &[Value],
    target: &Value,
    late_only: bool,
) -> Option<Result<Value, RuntimeError>> {
    let method = Symbol::intern(method);
    let named_count = args.iter().filter(|arg| arg.is_string_pair_value()).count();
    let arity = args.len() - named_count;
    let (named, positional): (Vec<Value>, Vec<Value>) = args
        .iter()
        .cloned()
        .partition(|arg| arg.is_string_pair_value());
    let type_object = matches!(target.view(), crate::value::ValueView::Package(_));
    let mut declared = false;
    // Whether a row took the call's arity, and whether every such row turned
    // the call down only for its type-object receiver (`Int.slice(0, 2)`).
    let mut arity_rows = false;
    let mut only_type_refused = true;
    let answer = owners.iter().find_map(|owner| {
        declared |= super::owner_declares(owner, method);
        super::owner_rows_at(Symbol::intern(owner), method, arity).find_map(|id| {
            let row = row(id);
            if late_only
                && !(matches!(row.handler, Handler::Interp(_))
                    && row.flags.contains(RowFlags::OWNER_ONLY))
            {
                return None;
            }
            // An interpreter row reads any argument the way its routine does (a
            // `Seq` of pairs for `trans`, a list of needles for `index`); only a
            // `Junction`, which must autothread, is refused, unless the row's
            // parameters are `Mu` (`JUNCTION_ARGS`).
            let admitted = if matches!(row.handler, Handler::Interp(_)) {
                row.flags.contains(RowFlags::JUNCTION_ARGS)
                    || !positional
                        .iter()
                        .any(|arg| matches!(arg.view(), crate::value::ValueView::Junction { .. }))
            } else {
                admits_row(row, &positional)
            };
            arity_rows = true;
            let type_refused = type_object && !row.flags.contains(RowFlags::TYPE_OBJECT_OK);
            // The nearest candidate with a `:D` invocant refuses the type
            // object outright (`IO::Path.e`).
            if type_refused
                && admitted
                && let crate::value::ValueView::Package(name) = target.view()
                && let Some(refusal) = row.type_object_refusal(name.as_str())
            {
                return Some(Err(refusal));
            }
            let bound = bind_named(row, named.clone());
            if type_refused || bound.is_none() || !admitted {
                only_type_refused &= type_refused && bound.is_some() && admitted;
                return None;
            }
            only_type_refused = false;
            let bound = bound.unwrap_or_default();
            call(
                row,
                Some(&mut *interp),
                target,
                &positional,
                Named::new(&bound),
            )
            .map(|(result, _)| result)
        })
    });
    // An owner declares the method, but no candidate of any owner takes this
    // many arguments: Rakudo's multi dispatch finds none (`"".subst`).
    // Or every candidate that takes the arity is for a defined invocant only:
    // Rakudo's `Any:D` candidates do not bind a type object either.
    if answer.is_none()
        && !late_only
        && declared
        && (!owners
            .iter()
            .any(|owner| super::owner_row(Symbol::intern(owner), method, arity).is_some())
            || (arity_rows && only_type_refused))
    {
        return Some(Err(
            crate::runtime::methods_signature_errors::make_multi_no_match_error(method.as_str()),
        ));
    }
    answer
}

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
/// - `Iterable`'s `lazy` ([`ITERABLE_FOR_SHAPELESS`]) and `flat($levels)`
///   answer a `LazyList` or unsettled `Seq` from `Seq`'s row and a lazy or
///   shaped array or a `Slip` from `List`'s.
/// - `Array`'s and `Hash`'s `default` ([`CONTAINER_FOR_SHAPELESS`]) answer a
///   typed, shaped or lazy array or hash from its own type's row.
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
    let mut interp = interp;
    // A row that declares named arguments (`flat($levels, :hammer)`) is
    // handed them split from the positionals, as the guard step does; the
    // other rows keep reading every argument as a positional.
    let named_count = args.iter().filter(|arg| arg.is_string_pair_value()).count();
    if named_count > 0
        && let Some(id) = shapeless_row(
            interp.as_deref_mut(),
            target,
            method,
            args.len() - named_count,
        )
        && !row(id).named.is_empty()
    {
        let (named, positional): (Vec<Value>, Vec<Value>) = args
            .iter()
            .cloned()
            .partition(|arg| arg.is_string_pair_value());
        if let Some(named) = bind_named(row(id), named) {
            return call(row(id), None, target, &positional, Named::new(&named))
                .map(|(result, _)| result);
        }
    }
    let id = shapeless_row(interp.as_deref_mut(), target, method, args.len())?;
    // `Cool`'s text methods are `self.Str.METHOD`: an instance with a `Str`
    // method of its own is read through it, not through its built-in string
    // form (`class C is Cool { method Str { "cool " } }; C.new.uc` is `COOL `).
    if row(id).owner == "Cool"
        && cool_text_for_shapeless(method.as_str())
        && let crate::value::ValueView::Instance { class_name, .. } = target.view()
        && let Some(interp) = interp.as_deref_mut()
        && interp.has_user_method(class_name.as_str(), "Str")
    {
        let text = match interp.call_method_with_values(target.clone(), "Str", Vec::new()) {
            Ok(text) => text,
            Err(err) => return Some(Err(err)),
        };
        // A pattern or a limit (`comb(/\w/)`) is the interpreter's.
        if method == "comb" && !args.is_empty() {
            return Some(interp.call_method_with_values(text, "comb", args.to_vec()));
        }
        return call(row(id), None, &text, args, Named::NONE).map(|(result, _)| result);
    }
    // `comb($pattern, $limit)` has no row: it is the user `Str` case alone.
    if method == "comb" && args.len() == 2 {
        return None;
    }
    // A user subset's type object coerces as the type it refines
    // (`subset S of Int; S.Int` is `Int.Int` of the invocant `S`), so a
    // coercion that answers its invocant answers the subset's type object
    // (#14045). `UInt` and the subsets' bases are resolved the same way.
    if let crate::value::ValueView::Package(name) = target.view()
        && COOL_COERCE_FOR_SHAPELESS.contains(&method.as_str())
        && let Some(interp) = interp.as_deref()
    {
        let base = interp.resolve_subset_base_type(name.as_str());
        if base != name.as_str() {
            let base = Value::package(Symbol::intern(&base));
            if let Some((Ok(answer), _)) = call(row(id), None, &base, args, Named::NONE)
                && answer == base
            {
                return Some(Ok(target.clone()));
            }
        }
    }
    // `Mu.WHERE` reads the class registry for a NativeCall handle (§9.91) and
    // `Cool.UInt` reads `$*TOLERANCE` for a `Complex` (§9.92); the other rows
    // found here are pure.
    let interp = interp.filter(|_| {
        row(id).owner == "Mu"
            || method == "ACCEPTS"
            || COOL_COERCE_FOR_SHAPELESS.contains(&method.as_str())
            // An interpreter row has no answer without one (a count-taking
            // `pick`/`roll`, §9.165).
            || matches!(row(id).handler, Handler::Interp(_))
    });
    call(row(id), interp, target, args, Named::NONE).map(|(result, _)| result)
}

/// `Cool`'s text methods a receiver with no shape reaches: the audited list
/// plus `chars`, which the closed shapes keep answering from their own rows.
// Cost: O(m), m = audited names.
pub(crate) fn cool_text_for_shapeless(method: &str) -> bool {
    method == "chars"
        || crate::value::COOL_TEXT_FOR_CLOSED_SHAPES.contains(&method)
        || COOL_SEARCH_FOR_SHAPELESS.contains(&method)
        || method == "split"
}

/// The argument-taking `Cool` search methods, read through a `Cool` instance's
/// own `Str` like the text methods above (#13035).
const COOL_SEARCH_FOR_SHAPELESS: [&str; 9] = [
    "contains",
    "starts-with",
    "ends-with",
    "index",
    "rindex",
    "substr",
    "subst",
    "trans",
    "indices",
];

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
    // and the `Cool` text methods that read a property, a pattern, an
    // amount (§9.90) or a count (`words`, `lines`, `chop`) alone take an
    // argument.
    if arity > 0
        && (arity > 1
            || !matches!(
                method.as_str(),
                "join"
                    | "pick"
                    | "roll"
                    | "head"
                    | "tail"
                    | "combinations"
                    | "batch"
                    | "uniprop"
                    | "uniprops"
                    | "unimatch"
                    | "samecase"
                    | "samemark"
                    | "indent"
                    | "words"
                    | "lines"
                    | "chop"
                    | "comb"
                    | "flat"
                    | "isa"
                    | "lazy-if"
                    | "AT-POS"
                    | "EXISTS-POS"
                    | "AT-KEY"
                    | "EXISTS-KEY"
                    | "ACCEPTS"
                    | "WALK"
            ))
        // `comb($pattern, $limit)` has no row of its own: only an instance
        // with a user `Str` takes it (`answer_shapeless`).
        && !(arity == 2 && method == "comb")
        // `Mu.rakuseen($id, &code)` takes two, `Mu.WALK` any number.
        && !(arity == 2 && method == "rakuseen")
        && method != "WALK"
    {
        return None;
    }
    // A type object with no closer candidate answers `Mu.ACCEPTS`'s type
    // check (§9.164).
    if method == "ACCEPTS" && matches!(target.view(), ValueView::Package(_)) {
        return super::owner_row(Symbol::intern("Mu"), method, arity);
    }
    // `Any.ACCEPTS` answers an instance whose MRO declares nothing closer
    // (§9.108). A code object, a regex, an enum or a mixin has its own
    // `ACCEPTS`, which the interpreter answers.
    if method == "ACCEPTS" && !matches!(target.view(), ValueView::Instance { .. }) {
        return None;
    }
    // A class composing `Real` has `Numeric.ACCEPTS` (`a == self`) ahead of
    // `Any`'s identity (#13894).
    let mut interp = interp;
    if method == "ACCEPTS"
        && arity == 1
        && let Some(interp) = interp.as_deref_mut()
        && super::is_user_real_instance(interp, target)
    {
        return super::owner_row(Symbol::intern("Numeric"), method, arity);
    }
    // `Any`'s subscript protocol answers a value that is not `Positional`; a
    // list with no settled shape (lazy, shaped, a `Seq`, a `Slip`, a buffer)
    // indexes its elements through the interpreter, though it has no keys
    // either (§9.106).
    let positional = matches!(method.as_str(), "AT-POS" | "EXISTS-POS");
    if (positional || matches!(method.as_str(), "AT-KEY" | "EXISTS-KEY"))
        && match target.descalarize().view() {
            ValueView::Array(..)
            | ValueView::BufStorage(..)
            | ValueView::Seq(..)
            | ValueView::HyperSeq(..)
            | ValueView::RaceSeq(..)
            | ValueView::Slip(..)
            | ValueView::LazyList(..) => positional,
            // A mixin delegates to the value it wraps (`%h but R`).
            ValueView::Junction { .. } | ValueView::Proxy { .. } | ValueView::Mixin(..) => true,
            // A built-in type object has no keys (`Int.AT-KEY` autovivifies to
            // `Any`, §9.154); another package's stash is `Associative`
            // (`CORE::<&say>`), and an instance's `{ }` subscript may be a user
            // `postcircumfix:<{ }>` or a tied store, which the interpreter
            // resolves first.
            ValueView::Package(name) => {
                !positional
                    && name != "Any"
                    && crate::builtin_types::catalog::builtin_type_mro_strs(name.as_str()).is_none()
            }
            ValueView::Instance { .. } => !positional,
            _ => false,
        }
    {
        return None;
    }
    // An `is Pair` or quant-hash subclass answers its base's methods on the
    // value it carries (`native_base::redirect`), not `Any`'s on itself.
    if crate::builtins::native_base::has_collection_payload(target)
        && crate::builtins::native_base::redirect(target, method.as_str()).is_some()
    {
        return None;
    }
    let text = cool_text_for_shapeless(method.as_str());
    let list = crate::value::ANY_LIST_FOR_CLOSED_SHAPES.contains(&method.as_str());
    let mu = crate::value::MU_FOR_CLOSED_SHAPES.contains(&method.as_str());
    // `flat($levels)` is `Iterable`'s alone: `Any` declares only `flat()`.
    let iterable =
        ITERABLE_FOR_SHAPELESS.contains(&method.as_str()) || (arity == 1 && method == "flat");
    // `Cool.Complex` answers a lazy list's `X::Cannot::Lazy` and an unsettled
    // `Seq`'s element count (§9.110); an instance or a type object with no
    // shape is the interpreter's.
    let lazy_complex = method == "Complex"
        && matches!(
            target.view(),
            ValueView::LazyList(_)
                | ValueView::Seq(_)
                | ValueView::HyperSeq(_)
                | ValueView::RaceSeq(_)
                | ValueView::Slip(_)
        );
    // `Cool`'s numeric methods (`self.Numeric.METHOD`) answer a `Seq` or `Slip`
    // with no settled shape on its element count, as they do a settled one
    // (§9.112).
    let list_numeric = crate::value::cool_numeric_method(method.as_str())
        && matches!(
            target.view(),
            ValueView::Seq(_) | ValueView::HyperSeq(_) | ValueView::RaceSeq(_) | ValueView::Slip(_)
        );
    let coerce =
        COOL_COERCE_FOR_SHAPELESS.contains(&method.as_str()) || lazy_complex || list_numeric;
    let text = text || coerce;
    // A shaped array's element type lives in the interpreter's registry, so
    // a pure entry leaves its `raku` to the interpreter's row below.
    if method == "raku"
        && interp.is_none()
        && matches!(
            target.view(),
            ValueView::Array(_, crate::value::ArrayKind::Shaped)
        )
    {
        return None;
    }
    // A shaped or itemized array or hash renders as its own type's `raku`
    // (a typed one as `Array[Int].new(...)`, §9.185), an interpreter row; a
    // pure entry keeps `Mu`'s rendering.
    let container = arity == 0
        && (CONTAINER_FOR_SHAPELESS.contains(&method.as_str())
            || (method == "raku"
                && interp.is_some()
                && matches!(target.view(), ValueView::Array(..) | ValueView::Hash(_))));
    // An opaque instance (`DispatchShape::is_opaque`) is shapeless here: only its
    // own class's rows reach it through its shape.
    if !(text || list || mu || iterable || container)
        || Receiver::of_settled(target).is_some_and(|receiver| !receiver.shape.is_opaque())
    {
        return None;
    }
    // A `.VAR` descriptor asks the value its container holds (ADR-0064), which
    // only `Interpreter::try_var_meta_delegate` can read: `$u.VAR.Bool` is the
    // truth of the undefined value, not of the descriptor.
    if crate::runtime::runtime_var_meta::var_meta_descriptor_defers(target, method.as_str()) {
        return None;
    }
    let (cool, any) = (Symbol::intern("Cool"), Symbol::intern("Any"));
    let mu_owner = Symbol::intern("Mu");
    // A role mixed into a class with its own `iterator` or `list` keeps the
    // class listing itself (`own_iteration_view` answers it).
    if (list || iterable)
        && let ValueView::Mixin(inner, _) = target.view()
        && let ValueView::Instance { class_name, .. } = inner.view()
        && let Some(interp) = interp.as_deref_mut()
        && !interp.mixin_composes_method(target, method.as_str())
        && interp.own_iteration_reads(class_name.as_str(), method.as_str())
    {
        return None;
    }
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
        // `lazy-if` is `Any`'s too: a receiver `Iterable` does not reach answers
        // from `Any`'s row, which is the receiver itself.
        _ if iterable && !list => return None,
        ValueView::Array(..) if container => Symbol::intern("Array"),
        ValueView::Hash(_) if container => Symbol::intern("Hash"),
        _ if container => return None,
        // `Mu`'s answers reach a `Failure` too: asking its truth handles it.
        ValueView::Instance { class_name, .. } if mu && class_name == "Failure" => mu_owner,
        // Without an interpreter no MRO can be read: `Mu`'s renderings answer
        // the built-in classes' instances they know (a `Signature`, a `Stash`)
        // and decline every other instance, whose class may declare its own.
        ValueView::Instance { .. }
            if mu && interp.is_none() && matches!(method.as_str(), "gist" | "raku") =>
        {
            mu_owner
        }
        ValueView::Instance { class_name, .. } if mu => {
            // The first class in the MRO that declares the method answers it
            // (`class M is Match` keeps `Match.not`); `Mu` ends every MRO.
            // A role the class composes comes before its parents: its row
            // (`Numeric.Bool` of a `does Real` class, §9.179) answers through
            // `composed_roles::answer`, so `Mu`'s declines.
            let interp = interp?;
            if arity == 0
                && super::composed_roles::role_declares(interp, class_name.as_str(), method)
            {
                return None;
            }
            let owner = interp
                .class_mro(class_name.as_str())
                .iter()
                .copied()
                .find(|&class| super::owner_row(class, method, 0).is_some())
                .unwrap_or(mu_owner);
            // A built-in class's rendering reads its own state, not a
            // subclass's: an `Exception` subclass renders its own `message`,
            // a `Str` subclass keeps its type. The interpreter answers those
            // (§9.93).
            if matches!(method.as_str(), "Str" | "Stringy" | "gist" | "raku") && owner != mu_owner {
                return None;
            }
            owner
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
        // A package stash is a `Hash` of its symbols that `Any`'s `kv`,
        // `pairs`, `hash` and `elems` read as such (`any_views`, §9.109,
        // §9.163); its MRO has no row that does.
        ValueView::Instance { class_name, .. }
            if matches!(method.as_str(), "kv" | "pairs" | "hash" | "list" | "elems")
                && crate::value::types::is_stash_class_name(&class_name.resolve()) =>
        {
            any
        }
        // An instance of a built-in class with no shape (`Supply`) answers from
        // its own class's row, which was written for it (§9.110); no MRO is
        // needed to find it. Only a built-in class owns rows, so a user class
        // never matches here.
        ValueView::Instance { class_name, .. }
            if class_name != "Failure" && super::owner_row(class_name, method, arity).is_some() =>
        {
            class_name
        }
        ValueView::Instance { class_name, .. } if class_name != "Failure" => {
            let interp = interp?;
            // A class with its own `iterator` or `list` is listed through it,
            // which the interpreter does (§9.148).
            if list && interp.own_iteration_reads(class_name.as_str(), method.as_str()) {
                return None;
            }
            let mut owner = None;
            let subscript = matches!(
                method.as_str(),
                "AT-POS" | "EXISTS-POS" | "AT-KEY" | "EXISTS-KEY"
            );
            let mro = interp.class_mro(class_name.as_str());
            let mut exhausted = true;
            for &class in mro.iter() {
                if (class == cool && text) || (class == any && list) {
                    owner = Some(class);
                    break;
                }
                // A subclass of a built-in class (`is Map`, `is Array`) keeps
                // its storage's subscript protocol, which the interpreter
                // answers (§9.106).
                if subscript
                    && class != cool
                    && class != any
                    && crate::builtin_types::catalog::builtin_type_mro_strs(class.as_str())
                        .is_some()
                {
                    exhausted = false;
                    break;
                }
                if super::owner_row(class, method, arity).is_some() {
                    exhausted = false;
                    // A subclass of a built-in class that declares its own
                    // `ACCEPTS` (`class DD is Date`) answers from that row,
                    // and so does an instance of the built-in class itself
                    // that has no shape (`Supply.list`, §9.110).
                    // An instance with no subscript protocol of its own is
                    // the one-element list holding itself (`Any`'s, §9.154).
                    if method == "ACCEPTS" || class == class_name || (subscript && class == any) {
                        owner = Some(class);
                    }
                    break;
                }
            }
            // An `Exception` subclass's MRO ends at `Exception`, whose
            // catalog chain (`Any`, `Mu`) is not spelled out in it: `Any`
            // answers what no closer class declared.
            // An instance with no subscript protocol in its MRO is the
            // one-element list holding itself (`Any`'s, §9.154), unless its
            // class is declared `is Mu`.
            if owner.is_none()
                && exhausted
                && subscript
                && !interp
                    .registry()
                    .classes
                    .get(class_name.as_str())
                    .is_some_and(|def| def.parents.iter().any(|parent| parent == "Mu"))
            {
                owner = Some(any);
            }
            if owner.is_none() && exhausted && list && !coerce {
                let is_any = |class: &Symbol| {
                    crate::builtin_types::catalog::builtin_type_mro_strs(class.as_str())
                        .is_some_and(|chain| chain.contains(&"Any"))
                };
                if mro.iter().any(is_any) {
                    owner = Some(any);
                }
            }
            // A coercion falls back to the `Cool`/`Mu` rows, which answer the
            // instances they know (a `StrDistance`, a `Stash`) and decline the
            // rest to the interpreter.
            if coerce {
                owner.unwrap_or(cool)
            } else {
                owner?
            }
        }
        ValueView::Instance { .. } => return None,
        // A lazy or shaped array coerces as the `Array` it is; a type object
        // with no shape (`RatStr`, `IntStr`) as the `Cool` its catalog MRO
        // reaches.
        ValueView::Array(..) if coerce => cool,
        ValueView::Package(name) if coerce => {
            // `UInt`, the one built-in subset, is not in the catalog: it is
            // an `Int`; a user subset is the type it refines (`subset S of
            // Int` coerces as `Int`, #14045).
            let name = name.resolve();
            let refined;
            let class = if name == "UInt" {
                "Int"
            } else if let Some(interp) = interp.as_deref() {
                refined = interp.resolve_subset_base_type(&name);
                refined.as_str()
            } else {
                name.as_ref()
            };
            crate::builtin_types::catalog::builtin_type_mro_strs(class)?
                .contains(&"Cool")
                .then_some(cool)?
        }
        // `Mu` is no `Any`: `Mu.flat` and `Mu.pick` have no candidate.
        ValueView::Package(name) if name == "Mu" => return None,
        // `Nil` declares its own `AT-POS` and `AT-KEY` (§9.154); its other
        // subscript methods are `Any`'s.
        ValueView::Nil if super::owner_row(Symbol::intern("Nil"), method, arity).is_some() => {
            Symbol::intern("Nil")
        }
        // A hash or a quant hash with no settled shape (a `Map` subclass's raw
        // storage) answers its type's associative rows (§9.154).
        ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
            if matches!(method.as_str(), "AT-KEY" | "EXISTS-KEY") =>
        {
            match target.view() {
                ValueView::Hash(_) => Symbol::intern("Hash"),
                _ => Symbol::intern(crate::runtime::utils::value_type_name(target)),
            }
        }
        // Every other receiver the cascades declined (a code object, a regex,
        // a promise, a RakuAST node, a type object with no shape, a shaped or
        // lazy array) lists as itself or its elements.
        _ if list => any,
        _ => return None,
    };
    // `Int` and `Numeric` are `Mu`'s, not `Cool`'s (§9.92).
    let arity = if method == "comb" {
        arity.min(1)
    } else {
        arity
    };
    // `Cool`'s owner-only rows (`Cool.Int`, which `.^can` lists) are never
    // the coercion fallback's answer.
    super::owner_row(owner, method, arity)
        .filter(|&id| owner != cool || !super::row(id).flags.contains(RowFlags::OWNER_ONLY))
        .or_else(|| {
            coerce
                .then(|| super::owner_row(mu_owner, method, arity))
                .flatten()
        })
}

/// The named arguments `row` is handed for a call passing `named`: the ones it
/// binds, after dropping every name no row of the method binds (the implicit
/// `*%_` of a Raku method swallows it: `"abc".chop(:zzz)` is `"ab"`). `None`
/// when another row of the method binds a name this one does not: that row
/// (an interpreter twin reading `unique(:as)`) answers the call, so this one
/// declines.
// Cost: O(a) for a names the row binds; otherwise O(a * k * n), k = rows named
// the method, n = names each declares.
fn bind_named(row: &MethodRow, named: Vec<Value>) -> Option<Vec<Value>> {
    if named.iter().all(|pair| names_declared(row, pair)) {
        return Some(named);
    }
    let method = Symbol::intern(row.name);
    let mut bound = Vec::with_capacity(named.len());
    for pair in named {
        if names_declared(row, &pair) {
            bound.push(pair);
        } else if match pair.view() {
            crate::value::ValueView::Pair(key, _) => {
                super::some_row_binds_named(method, key.as_str())
            }
            _ => true,
        } {
            return None;
        }
    }
    Some(bound)
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
