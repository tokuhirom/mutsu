//! `CompUnit`'s and `CompUnit::Handle`'s rows (ADR-11276 §9.117, #11002).
//!
//! Each class has an opaque dispatch shape (`DispatchShape::is_opaque`), so
//! every call entry reaches the rows through the table. A `CompUnit` (from a repository's `need`,
//! `load` or `resolve`) carries its accessors as attributes; a handle of a
//! loaded unit carries the unit's name (`unit-name`) and, for an
//! `Installation` repository, the symbols the unit loaded
//! (`globalish-symbols`), kept hidden until `merge-symbols`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A row of `$owner` reached through its owner.
macro_rules! row {
    ($owner:literal, $name:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: $handler,
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!(
        "CompUnit::Handle",
        "export-package",
        Handler::Interp(export_package)
    ),
    row!(
        "CompUnit::Handle",
        "export-how-package",
        Handler::Interp(export_how_package)
    ),
    row!(
        "CompUnit::Handle",
        "globalish-package",
        Handler::Interp(globalish_package)
    ),
    row!("CompUnit", "short-name", Handler::Narrow(short_name)),
    row!("CompUnit", "Str", Handler::Narrow(short_name)),
    row!("CompUnit", "gist", Handler::Narrow(short_name)),
    row!("CompUnit", "version", Handler::Narrow(version)),
    row!("CompUnit", "api", Handler::Narrow(api)),
    row!("CompUnit", "auth", Handler::Narrow(auth)),
    row!("CompUnit", "from", Handler::Narrow(from)),
    row!("CompUnit", "repo", Handler::Narrow(repo)),
    row!("CompUnit", "repo-id", Handler::Narrow(repo_id)),
    row!("CompUnit", "distribution", Handler::Narrow(distribution)),
    row!("CompUnit", "precompiled", Handler::Narrow(precompiled)),
    // A new `CompUnit::Handle` each call: an identity of its own.
    MethodRow {
        owner: "CompUnit",
        name: "handle",
        arity: 0,
        handler: Handler::Narrow(handle),
        flags: RowFlags::RANDOM,
        named: &[],
    },
];

/// The attribute `key` of the instance, `None` inside when it was not set.
// Cost: O(1).
fn attr(target: &Value, key: &str) -> Option<Option<Value>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(attributes.as_map().get(key).cloned())
}

/// `.short-name`, and `.Str`/`.gist`, which render it.
// Cost: O(1).
fn short_name(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "short-name")?.unwrap_or(Value::NIL)))
}

/// `.version`: the `Version` type object when the unit has none.
// Cost: O(1).
fn version(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "version")?.unwrap_or_else(|| {
        Value::package(Symbol::intern("Version"))
    })))
}

/// `.api`: the `Version` type object when the unit has none.
// Cost: O(1).
fn api(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "api")?.unwrap_or_else(|| {
        Value::package(Symbol::intern("Version"))
    })))
}

/// `.auth`: the `Str` type object when the unit has none.
// Cost: O(1).
fn auth(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(
        attr(target, "auth")?.unwrap_or_else(|| Value::package(Symbol::intern("Str")))
    ))
}

/// `.from`: the language the unit is written in, `Perl6` by default.
// Cost: O(1).
fn from(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(
        attr(target, "from")?.unwrap_or_else(|| Value::str_from("Perl6"))
    ))
}

/// `.repo`.
// Cost: O(1).
fn repo(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "repo")?.unwrap_or(Value::NIL)))
}

/// `.repo-id`.
// Cost: O(1).
fn repo_id(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "repo-id")?.unwrap_or(Value::NIL)))
}

/// `.distribution`.
// Cost: O(1).
fn distribution(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "distribution")?.unwrap_or(Value::NIL)))
}

/// `.precompiled`.
// Cost: O(1).
fn precompiled(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "precompiled")?.unwrap_or(Value::FALSE)))
}

/// `.handle`: the handle a loaded unit carries; otherwise one naming the unit
/// (for `export-package`) and carrying the symbols it loaded, so they can later
/// be merged into `GLOBAL` through `merge-symbols`.
// Cost: O(1).
fn handle(target: &Value, _args: &[Value]) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let attributes = attributes.as_map();
    if let Some(handle) = attributes.get("handle") {
        return Some(Ok(handle.clone()));
    }
    let mut handle_attrs = std::collections::HashMap::new();
    if let Some(syms) = attributes.get("globalish-symbols") {
        handle_attrs.insert("globalish-symbols".to_string(), syms.clone());
    }
    if let Some(name) = attributes.get("short-name") {
        handle_attrs.insert("unit-name".to_string(), name.clone());
    }
    Some(Ok(Value::make_instance(
        Symbol::intern("CompUnit::Handle"),
        handle_attrs,
    )))
}

/// `CompUnit::Handle.export-package`: the unit's `EXPORT` stash, a `Stash` of
/// the `EXPORT::<tag>` packages.
// Cost: O(s), s = symbols in the unit's `EXPORT` package.
fn export_package(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    let unit = attr(target, "unit-name")?.map(|v| v.to_string_value())?;
    Some(Ok(interp.package_stash_value(&format!("{unit}::EXPORT"))))
}

/// `CompUnit::Handle.export-how-package`: the unit's `EXPORTHOW` stash (its
/// `DECLARE` / `SUPERSEDE` packages), or `Nil` when the unit declares none.
// Cost: O(s), s = symbols in the unit's `EXPORTHOW` package.
fn export_how_package(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    let unit = attr(target, "unit-name")?.map(|v| v.to_string_value())?;
    Some(Ok(interp
        .exporthow_stash_of_unit(&unit)
        .unwrap_or(Value::NIL)))
}

/// `CompUnit::Handle.globalish-package`: the package the unit's symbols were
/// loaded into (`Interpreter::make_globalish_package`).
// Cost: O(1).
fn globalish_package(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    let symbols = attr(target, "globalish-symbols")?;
    Some(Ok(interp.make_globalish_package(symbols.as_ref())))
}
