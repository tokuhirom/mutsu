//! The element and key type methods `of` and `keyof` (ADR-11276 §9.86).
//!
//! Rakudo declares them on `Hash` and `Array`, on the roles `Positional`,
//! `Associative` and `Callable`, on the quant-hash roles `Baggy`, `Setty` and
//! `Mixy` (`of`) and on `QuantHash` (`keyof`), and on `Blob`
//! ([`crate::builtins::method_table::scalars::blob`]). A role's rows reach the
//! shapes whose type composes it ([`crate::value::DispatchShape::reaches_role`]);
//! a type object with no shape (`Positional`, `Sub`, `Bag[Int]`,
//! `Hash[Int,Str]`) reaches them through
//! [`crate::builtins::method_table::answer_shapeless`]. A type object's
//! parameters are read from its name, which is where a parameterized type
//! keeps them; an instance's from the container descriptor it carries.

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::{ContainerTypeInfo, embedded_container_type_info};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        row!($owner, $name, $handler, RowFlags::TYPE_OBJECT_OK)
    };
    ($owner:literal, $name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Hash", "of", hash_of),
    row!("Hash", "keyof", hash_keyof),
    row!("Array", "of", array_of),
    row!("Positional", "of", value_param_or_mu),
    row!("Associative", "of", value_param_or_mu),
    row!("Associative", "keyof", associative_keyof),
    // No shape is a code object: the code type objects reach it through
    // `answer_shapeless`, a code object through `Code.of`.
    row!(
        "Callable",
        "of",
        value_param_or_mu,
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::OWNER_ONLY)
    ),
    row!("Baggy", "of", baggy_of),
    row!("Setty", "of", setty_of),
    row!("Mixy", "of", mixy_of),
    // Every quant hash composes `Baggy`, `Setty` or `Mixy` first, so only the
    // `QuantHash` type object reaches it.
    row!(
        "QuantHash",
        "of",
        value_param_or_mu,
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::OWNER_ONLY)
    ),
    row!("QuantHash", "keyof", quanthash_keyof),
];

/// The names whose rows answer a type object with no shape, for the
/// type-object branch of `answer_shapeless`: this module's, and the numeric
/// types' `Range` (`scalars::numeric_range`, ADR-11276 §9.97), which a native
/// integer type object (`int8`) and the `Rational` role reach.
const NAMES: [&str; 3] = ["of", "keyof", "Range"];

/// The roles a role with rows does itself, for a role type object (`Baggy.keyof`
/// is `QuantHash`'s). The catalog lists classes only.
const ROLE_PARENTS: [(&str, &[&str]); 4] = [
    ("Baggy", &["QuantHash", "Associative"]),
    ("Setty", &["QuantHash", "Associative"]),
    ("Mixy", &["Baggy", "QuantHash", "Associative"]),
    ("QuantHash", &["Associative"]),
];

/// The row a type object with no shape answers `of`/`keyof` from (ADR-11276
/// §9.86), or `Array`'s for a shaped or lazy array: the first row along its base type's MRO, each class followed by
/// the roles it composes, or a role's own rows and those of the roles it does.
/// A parameterized type object (`Bag[Int]`, `Hash[Int,Str]`, `Positional[Int]`,
/// `buf16`) is found by its base name; its handler reads the parameters from
/// the name. A user type is not in the catalog and declines.
// Cost: O(d * (r + k)), d = MRO depth, r = roles per class, k = rows named
// `method`; a catalog scan for the base name.
pub(crate) fn type_object_row(
    target: &Value,
    method: Symbol,
) -> Option<crate::builtins::method_table::RowId> {
    use crate::builtin_types::catalog::{
        builtin_type_mro_strs, builtin_type_roles_strs, composes_role_at,
    };
    use crate::value::ValueView;
    if !NAMES.contains(&method.as_str()) {
        return None;
    }
    if crate::builtins::method_table::Receiver::of_settled(target).is_some() {
        return None;
    }
    let name = match target.view() {
        ValueView::Package(name) => name,
        ValueView::CustomType(data) => data.name,
        // A shaped or lazy array has no shape and is still an `Array`.
        ValueView::Array(..) => {
            return crate::builtins::method_table::owner_row(Symbol::intern("Array"), method, 0);
        }
        _ => return None,
    };
    let name = name.resolve();
    let row_of =
        |owner: &str| crate::builtins::method_table::owner_row(Symbol::intern(owner), method, 0);
    if crate::runtime::utils::is_native_elems_class(&name) {
        return row_of("Blob");
    }
    // A native integer type the catalog does not list (NativeCall's `long`,
    // `size_t`) is an `Int`.
    if crate::runtime::native_types::is_native_int_type(&name)
        && builtin_type_mro_strs(&name).is_none()
    {
        return row_of("Int");
    }
    let base = name.split_once('[').map_or(name.as_ref(), |(base, _)| base);
    if crate::value::ROLES_WITH_ROWS.contains(&base) {
        let parents = ROLE_PARENTS
            .iter()
            .find(|&&(role, _)| role == base)
            .map_or(&[][..], |&(_, parents)| parents);
        return std::iter::once(base)
            .chain(parents.iter().copied())
            .find_map(row_of);
    }
    // A role the catalog lists no MRO for (`Rational`) answers its own rows.
    let Some(mro) = builtin_type_mro_strs(base) else {
        return row_of(base);
    };
    mro.iter().enumerate().find_map(|(m, class)| {
        row_of(class).or_else(|| {
            builtin_type_roles_strs(class)
                .iter()
                .filter(|role| {
                    crate::value::ROLES_WITH_ROWS.contains(role) && composes_role_at(mro, m, role)
                })
                .find_map(|role| row_of(role))
        })
    })
}

fn package(name: &str) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::package(Symbol::intern(name))))
}

/// The name of the type object `target`, or `None` for an instance.
// Cost: O(1).
fn type_object_name(target: &Value) -> Option<Symbol> {
    match target.view() {
        ValueView::Package(name) => Some(name),
        ValueView::CustomType(data) => Some(data.name),
        _ => None,
    }
}

/// The parameters of a parameterized type object's name (`Hash[Int,Str]` is
/// `[Int, Str]`, `Hash[Int{Str}]` the object-hash spelling of the same), or
/// `None` for an unparameterized one.
// Cost: O(n), n = the name's length.
fn parameters(name: &str) -> Option<Vec<String>> {
    let open = name.find('[')?;
    let inner = name[open + 1..].strip_suffix(']')?;
    let (value, key) = crate::runtime::types::split_object_hash_constraint(inner);
    if let Some(key) = key {
        return Some(vec![value.to_string(), key.to_string()]);
    }
    Some(crate::runtime::split_balanced_comma_list(inner))
}

/// The `n`th parameter of the type object `target`'s name.
// Cost: O(n), n = the name's length.
fn parameter(target: &Value, n: usize) -> Option<String> {
    parameters(&type_object_name(target)?.resolve())?
        .into_iter()
        .nth(n)
}

/// The element type an instance's descriptor names, if it names one.
// Cost: O(1).
fn descriptor_value_type(info: &ContainerTypeInfo) -> Option<&str> {
    Some(info.value_type.as_str()).filter(|t| !t.is_empty())
}

/// `Positional.of`, `Associative.of`, `Callable.of`: the first parameter of a
/// parameterized type object (`Positional[Int].of` is `Int`), `Mu` otherwise.
// Cost: O(n), n = the type object's name length.
fn value_param_or_mu(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    package(parameter(target, 0).as_deref().unwrap_or("Mu"))
}

/// `Array.of`: the element type the array was declared with (`my Int @a`,
/// `Array[Int]`), or `Mu`.
// Cost: O(1) for an instance; O(n) for a type object, n = its name length.
fn array_of(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if type_object_name(target).is_some() {
        return value_param_or_mu(target, &[]);
    }
    let info = embedded_container_type_info(target);
    package(
        info.as_ref()
            .and_then(descriptor_value_type)
            .unwrap_or("Mu"),
    )
}

/// `Hash.of`: the value type the hash was declared with (`my Int %h`,
/// `Hash[Int,Str]`), or `Mu`.
// Cost: as [`array_of`].
fn hash_of(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    array_of(target, args)
}

/// `Hash.keyof`: the key type of an object hash (`my %h{Int}`,
/// `Hash[Int,Str]`, a `Hash[V,K]` descriptor), or `Str(Any)`.
// Cost: as [`array_of`].
fn hash_keyof(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if type_object_name(target).is_some() {
        return associative_keyof(target, &[]);
    }
    let info = embedded_container_type_info(target);
    let declared_key = || {
        let declared = info.as_ref()?.declared_type.as_deref()?;
        parameters(declared)?.into_iter().nth(1)
    };
    match info.as_ref().and_then(|info| info.key_type.clone()) {
        Some(key) => package(&key),
        None => package(declared_key().as_deref().unwrap_or("Str(Any)")),
    }
}

/// `Associative.keyof`: the second parameter of a parameterized type object
/// (`Associative[Int,Str].keyof` is `Str`), `Str(Any)` otherwise.
// Cost: O(n), n = the type object's name length.
fn associative_keyof(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    package(parameter(target, 1).as_deref().unwrap_or("Str(Any)"))
}

/// `Baggy.of`: a bag's values are weights.
// Cost: O(1).
fn baggy_of(_target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    package("UInt")
}

/// `Setty.of`: a set's values are membership flags.
// Cost: O(1).
fn setty_of(_target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    package("Bool")
}

/// `Mixy.of`: a mix's values are real weights.
// Cost: O(1).
fn mixy_of(_target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    package("Real")
}

/// `QuantHash.keyof`: the element type of a parameterized quant hash
/// (`Bag[Int]`, an instance made by `Bag[Int].new`), or `Mu`.
// Cost: O(n), n = the type's name length.
fn quanthash_keyof(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if type_object_name(target).is_some() {
        return package(parameter(target, 0).as_deref().unwrap_or("Mu"));
    }
    let declared = embedded_container_type_info(target).and_then(|info| info.declared_type);
    let key = declared
        .as_deref()
        .and_then(parameters)
        .and_then(|params| params.into_iter().next());
    package(key.as_deref().unwrap_or("Mu"))
}
