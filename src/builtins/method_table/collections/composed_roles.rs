//! The methods a user class gets from composing `Positional`, `Associative` or
//! `Iterable` (ADR-11276 §9.165): `Positional.of`, `Associative.of` and
//! `.keyof`, and `Iterable`'s `hyper`, `race` and `lazy`; and the entry the
//! `Real`/`Numeric` (`composed_real`, §9.179) and `Dateish`
//! (`composed_dateish`, §9.181) rows are reached through.
//!
//! A class that composes `Positional[V]` or `Associative[V, K]` keeps the
//! role's type arguments in its class metadata, not in a native payload, so
//! `.of`/`.keyof` read the composition
//! (`Interpreter::composed_container_role_value_type`), on an instance and on
//! the type object alike. A user `Iterable` with its own `iterator` is listed
//! through it before it is hyperized or made lazy (#12929).
//!
//! The roles have no shape: an instance or a type object of a user class
//! reaches the rows through [`answer`], and a handler declines a class whose
//! composition does not answer the call. `Positional.of` and
//! `Associative.of`/`keyof` also have a pure row reading a parameterized
//! type's name (`type_params`), which every other entry finds; these are its
//! late twins (§9.136), registered after it and reached only by [`answer`].

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const ROLE: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK);

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: ROLE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Positional", "of", value_type),
    row!("Associative", "of", value_type),
    row!("Associative", "keyof", key_type),
    row!("Iterable", "hyper", hyper),
    row!("Iterable", "race", race),
    row!("Iterable", "lazy", lazy),
];

/// The roles whose rows [`answer`] tries, in order.
const OWNERS: [&str; 6] = [
    "Positional",
    "Associative",
    "Iterable",
    "Real",
    "Numeric",
    "Dateish",
];

/// The answer of a composed role's row for a call on an instance or a type
/// object of a class, or `None` when no such row answers it.
// Cost: O(o) to find the rows, o = 6 owners, plus the handler's own cost.
pub(crate) fn answer(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: &[Value],
) -> Answer {
    match target.view() {
        ValueView::Instance { .. } => {}
        // A parameterized type object (`Array[Int]`) answers from its own
        // parameter, not a composition.
        ValueView::Package(name) if !name.as_str().contains('[') => {}
        _ => return None,
    }
    let method_sym = Symbol::intern(method);
    let owners: Vec<&str> = OWNERS
        .into_iter()
        .zip(owner_syms())
        .filter(|&(_, &owner)| super::super::owner_row(owner, method_sym, args.len()).is_some())
        .map(|(owner, _)| owner)
        .collect();
    if owners.is_empty() {
        return None;
    }
    // A composed role's method stands behind the class's own: the entry runs
    // before the class's methods are looked up, so a class (or a parent, or a
    // role it composes) that declares the method answers it (`method Bridge`
    // of a `does Real` class is the one `Real.sqrt` itself calls).
    if let Some(class) = class_of(target)
        && interp.has_user_method_sym(class.as_str(), method_sym)
    {
        return None;
    }
    crate::builtins::method_table::invoke_mro_late(interp, &owners, method, args, target)
}

/// [`OWNERS`] interned once per process: the checks below run on every truth
/// test and string coercion of an instance.
// Cost: O(1) after the first call.
fn owner_syms() -> &'static [Symbol; 6] {
    static CELL: std::sync::OnceLock<[Symbol; 6]> = std::sync::OnceLock::new();
    CELL.get_or_init(|| OWNERS.map(Symbol::intern))
}

/// Whether a role `class_name` composes declares a zero-argument `method` row
/// here: the question a context that would otherwise call only a user method
/// asks (infix `~` stringifies a `does Real` instance through `Real.Str`).
// Cost: O(o) row probes, o = 6 owners, and a memoized role lookup for each
// owner that has the row.
pub(crate) fn role_declares(interp: &mut Interpreter, class_name: &str, method: Symbol) -> bool {
    OWNERS
        .into_iter()
        .zip(owner_syms())
        .any(|(owner, &owner_sym)| {
            super::super::owner_row(owner_sym, method, 0).is_some()
                && interp.class_does_role(class_name, owner)
        })
}

/// The class `target` is an instance or the type object of.
// Cost: O(1).
fn class_of(target: &Value) -> Option<Symbol> {
    match target.view() {
        ValueView::Instance { class_name, .. } => Some(class_name),
        ValueView::Package(name) => Some(name),
        _ => None,
    }
}

/// `Positional.of` / `Associative.of`: the value type the composition names.
// Cost: as `Interpreter::composed_container_role_value_type`.
fn value_type(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    let class = class_of(target)?;
    let value_type = interp.composed_container_role_value_type(class.as_str())?;
    Some(Ok(Value::package(Symbol::intern(&value_type))))
}

/// `Associative.keyof`: the key type the composition names (#12989).
// Cost: as `Interpreter::composed_container_role_key_type`.
fn key_type(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    let class = class_of(target)?;
    let key_type = interp.composed_container_role_key_type(class.as_str())?;
    Some(Ok(Value::package(Symbol::intern(&key_type))))
}

macro_rules! iterable_row {
    ($fn:ident, $name:literal) => {
        // Cost: see `iterate`.
        fn $fn(
            interp: &mut Interpreter,
            target: &Value,
            _args: &[Value],
            _named: Named<'_>,
        ) -> Answer {
            iterate(interp, target, $name)
        }
    };
}

iterable_row!(hyper, "hyper");
iterable_row!(race, "race");
iterable_row!(lazy, "lazy");

/// `Iterable.hyper`/`race`/`lazy` of an `Iterable` instance with its own
/// `iterator` (a user class's, or a built-in's like `IO::Path::Parts`): the elements it yields, as a `List`, hyperized or made lazy.
// Cost: O(n), n = elements the iterator yields (one `pull-one` call each),
// plus the list's own `method`.
fn iterate(interp: &mut Interpreter, target: &Value, method: &str) -> Answer {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    // A built-in `Iterable` with an `iterator` of its own (`IO::Path::Parts`
    // iterates its three pairs) yields those elements.
    if let Some(pairs) = crate::builtins::method_table::io_path_parts_pairs(target) {
        return Some(interp.call_method_with_values(Value::array(pairs), method, Vec::new()));
    }
    let class = class_name.resolve();
    if !interp.class_does_role(&class, "Iterable") || !interp.has_user_method(&class, "iterator") {
        return None;
    }
    Some(
        interp
            .list_via_user_iterator(target)
            .and_then(|list| interp.call_method_with_values(list, method, Vec::new())),
    )
}
