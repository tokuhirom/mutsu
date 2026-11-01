//! `Mu.self`, `Mu.defined`, `Mu.DEFINITE`, `Mu.Bool`, `Mu.WHICH`, `Mu.WHERE`
//! and `Mu.clone` (ADR-11276 slice 5, §9.91): the identity and definedness
//! answers every receiver reaches by name.
//!
//! Rakudo declares them on `Mu`, so a type object, a `Junction`, a code object
//! and a closed shape answer them as well as a value: `Int.defined` is
//! `False`, `Int.WHICH` is the type's identity. A shape whose own type
//! declares one (`Capture.Bool`, `Match.WHICH`, `Date.WHICH`) answers its own
//! row first. A receiver with no shape reaches them through
//! [`crate::builtins::method_table::answer_shapeless`]; an instance there walks
//! its MRO, so a built-in class ahead of `Mu` that declares the method keeps
//! its own answer.
//!
//! These are the arity-0 rows a call dispatches to; the arity-1 rows of
//! `mu_base` with the same names are the last candidate of a user override's
//! `callsame` chain and are reached only by their owner.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{JunctionKind, RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:expr) => {
        MethodRow {
            owner: "Mu",
            name: $name,
            arity: 0,
            handler: $handler,
            flags: RowFlags::TYPE_OBJECT_OK,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("self", Handler::Pure(self_)),
    row!("defined", Handler::Pure(defined)),
    row!("DEFINITE", Handler::Pure(definite)),
    row!("Bool", Handler::Narrow(bool_)),
    row!("WHICH", Handler::Pure(which)),
    row!("WHERE", Handler::Interp(where_in)),
    row!("clone", Handler::Narrow(clone)),
];

/// `Map` declares `clone` itself: `Mu.clone`'s answer for a map value. Found by
/// owner only; a `Hash` receiver reaches `Mu`'s row.
pub(super) static MAP_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Map",
    name: "clone",
    arity: 0,
    handler: Handler::Narrow(clone),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];

/// The exception an unhandled `Failure` carries, which `.self` and `.Bool`
/// of the `Failure` must not hide.
// Cost: O(1).
fn unhandled_failure_exception(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" && !target.is_failure_handled() => {
            attributes.as_map().get("exception").cloned()
        }
        _ => None,
    }
}

/// Mark a `Failure` handled: asking its definedness or truth handles it.
// Cost: O(1).
fn handle_failure(target: &Value) {
    if let ValueView::Instance { class_name, .. } = target.view()
        && class_name == "Failure"
    {
        target.mark_failure_handled();
    }
}

/// `Mu.self`: the receiver's value. An unhandled `Failure` throws its
/// exception; an itemized aggregate loses its `$` marker (`{a=>1}.self.raku`
/// is `{:a(1)}`, #8490), as `deitemize_element` does for `$x<>`.
// Cost: O(1).
fn self_(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if let Some(ex) = unhandled_failure_exception(target) {
        let mut err = RuntimeError::new(ex.to_string_value());
        err.exception = Some(Box::new(ex));
        return Err(err);
    }
    Ok(target.clone().deitemize_element())
}

/// Whether `v` is defined, a `Junction` collapsing its eigenstates' answers.
// Cost: O(e), e = eigenstates (nested ones included).
fn value_defined(v: &Value) -> bool {
    match v.view() {
        ValueView::Nil | ValueView::Package(_) | ValueView::ParametricRole { .. } => false,
        ValueView::Slip(items) if items.is_empty() => false,
        ValueView::Instance { class_name, .. } if class_name == "Failure" => false,
        ValueView::VarRef { value, .. } => crate::runtime::types::value_is_defined(value),
        ValueView::Junction { kind, values } => {
            let mut answers = values.iter().map(value_defined);
            match kind {
                JunctionKind::Any => answers.any(|b| b),
                JunctionKind::All => answers.all(|b| b),
                JunctionKind::One => answers.filter(|&b| b).count() == 1,
                JunctionKind::None => answers.all(|b| !b),
            }
        }
        _ => true,
    }
}

/// `Mu.defined`: `False` for a type object, `Nil`, an empty `Slip` and a
/// `Failure` (which it handles); a `Junction` autothreads and collapses.
// Cost: O(1); O(e) for a `Junction`, e = eigenstates.
fn defined(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    handle_failure(target);
    Ok(Value::truth(value_defined(target)))
}

/// `Mu.DEFINITE`: whether the receiver is a concrete instance rather than a
/// type object. Not `.defined`: a `Failure` instance is still definite.
// Cost: O(1).
fn definite(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(crate::runtime::value_is_definite(target)))
}

/// `Mu.Bool`: the receiver's truth, by the one truthiness rule
/// (`Value::truthy`); a `Failure` is handled by it. A `Real` or `Numeric`
/// instance (a user number class) and a regex, which smartmatches against `$_`,
/// are the interpreter's.
// Cost: O(1) for a scalar or an aggregate (`truthy` tests emptiness only).
fn bool_(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance { .. } if target.does_check("Real") || target.does_check("Numeric") => {
            None
        }
        ValueView::Regex(_)
        | ValueView::RegexWithAdverbs(..)
        | ValueView::Routine { is_regex: true, .. } => None,
        _ => {
            handle_failure(target);
            Some(Ok(Value::truth(target.truthy())))
        }
    }
}

/// `Mu.WHICH`: the receiver's identity (`methods_0arg::which`).
// Cost: O(1) for a reference type; O(n) for a value type, n = its rendering.
pub(super) fn which(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::builtins::methods_0arg::which::which_of(target))
}

/// `Mu.WHERE` with the interpreter: a NativeCall handle (a `nativecast`ed
/// `CStruct`/`CUnion`/`CArray`, a native array) reports the address of its
/// storage, which only the class registry can tell
/// (`Interpreter::try_native_handle_repr_where`): a module dereferences it
/// (`MoarVM::Guts::REPRs`' `OBJECT_BODY`), so the identity hash of [`where_`]
/// would make it read wild memory. A pure entry has no interpreter and declines.
// Cost: O(1) plus the class-registry probe; as [`where_`] otherwise.
pub(super) fn where_in(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if let Some(address) = interp.try_native_handle_repr_where(target, "WHERE") {
        return Some(Ok(address));
    }
    where_(target, &[])
}

/// `Mu.WHERE`: Rakudo answers the object's memory address. mutsu's scalars
/// are unboxed, so this is a per-identity-stable `Int` hashed from the
/// `WHICH` identity (same object, same `WHERE`; distinct objects, distinct).
/// A NativeCall `Pointer` reports its real, readable address instead:
/// bindings walk the object's memory through it (`NativeHelpers`'
/// `MoarVM::Guts::REPRs` scans `.WHERE` for the pointer it stored), so it must
/// not fall through to the identity hash. The class is matched on its last
/// `::` component, because the prelude's `Pointer` picks up the enclosing
/// package when prepended inside a module (`Probe::Pointer`).
// Cost: O(1) for a reference type; O(n) for a value type, n = its rendering.
fn where_(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && crate::qualified::last_segment(class_name).as_str() == "Pointer"
    {
        let addr = attributes
            .as_map()
            .get("address")
            .map(|v| crate::runtime::to_int(v) as usize)
            .unwrap_or(0);
        return Some(Ok(Value::int(
            crate::runtime::nativecall::native_object_where(addr) as i64,
        )));
    }
    let objat = crate::builtins::methods_0arg::which::which_of(target);
    let which_str = match objat.view() {
        ValueView::Instance { attributes, .. } => attributes.as_map().objat_which(),
        _ => None,
    }?;
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    which_str.hash(&mut hasher);
    Some(Ok(Value::int((hasher.finish() >> 1) as i64)))
}

/// `Mu.clone` of a value: a type object and `Nil` are themselves, an
/// immutable value (a scalar, a `Range`, a `Version`, a `Capture`, a `Uni`) is
/// itself, an aggregate or a `Pair` is a shallow copy
/// (`collection_clone::container_clone`), a lazy array an independent lazy
/// list over the same generator, a `Seq` a new eager `Seq`, a `Slip` a copy
/// that keeps its itemization, an instance a new instance of its class over a
/// copy of its attributes (`InstanceAttrs::to_clone_map`, what the
/// interpreter's clone with `:attr(v)` twiddles starts from; a call with
/// twiddles declines to it).
// Cost: O(1) for a scalar; O(n) for an aggregate, n = elements; O(a) for an
// instance, a = attributes.
fn clone(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Package(_) | ValueView::Nil => Some(Ok(target.clone())),
        ValueView::LazyList(ll) => {
            let mut copy = (**ll).clone();
            // A generator-driven array shares its reifier with the clone.
            if ll.closure_seq.is_some() {
                copy.generator_origin = Some(ll.clone());
            }
            Some(Ok(Value::lazy_list(crate::gc::Gc::new(copy))))
        }
        ValueView::Array(..)
        | ValueView::Hash(..)
        | ValueView::Set(..)
        | ValueView::Bag(..)
        | ValueView::Mix(..)
        | ValueView::Pair(..)
        | ValueView::ValuePair(..) => {
            crate::builtins::method_table::collection_clone::container_clone(target).map(Ok)
        }
        ValueView::Seq(items) => Some(Ok(Value::seq(items.to_vec()))),
        ValueView::Slip(items) => Some(Ok(
            Value::slip(items.to_vec()).with_slip_itemized(target.slip_is_itemized())
        )),
        ValueView::Sub(_) => {
            crate::builtins::method_table::code::pure_answer(target, Symbol::intern("clone"))
        }
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Str(_)
        | ValueView::Bool(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..)
        | ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. }
        | ValueView::Version { .. }
        | ValueView::Capture { .. }
        | ValueView::Uni(_)
        | ValueView::Enum { .. } => Some(Ok(target.clone())),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => Some(Ok(Value::make_instance(
            class_name,
            attributes.to_clone_map(),
        ))),
        _ => None,
    }
}
