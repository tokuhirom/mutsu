//! The distributions' rows (ADR-11276 §9.117): `Distribution::Hash`,
//! `Distribution::Path`, a module's `$?DISTRIBUTION`
//! (`CompUnit::Repository::Distribution`) and an installed distribution
//! (`CompUnit::Repository::Installation::LazyDistribution`).
//!
//! Each distribution class has an opaque dispatch shape
//! (`DispatchShape::is_opaque`), as the repositories do, so every call entry
//! reaches the rows through the table. Every distribution carries its `meta` hash; the
//! two `Distribution::*` classes and an installed distribution carry their
//! `prefix` and a `files` map too, while a `$?DISTRIBUTION` keeps its prefix in
//! `meta<prefix>`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const HASH: &str = "Distribution::Hash";
const PATH: &str = "Distribution::Path";
const REPO_DIST: &str = "CompUnit::Repository::Distribution";
const LAZY: &str = "CompUnit::Repository::Installation::LazyDistribution";

/// A row of `$owner` reached through its owner.
macro_rules! row {
    ($owner:expr, $name:literal, $arity:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!(HASH, "meta", 0, Handler::Narrow(meta)),
    row!(HASH, "prefix", 0, Handler::Interp(prefix)),
    row!(HASH, "content", 1, Handler::Interp(content)),
    row!(PATH, "meta", 0, Handler::Narrow(meta)),
    row!(PATH, "prefix", 0, Handler::Interp(prefix)),
    row!(PATH, "content", 1, Handler::Interp(content)),
    row!(REPO_DIST, "meta", 0, Handler::Narrow(meta)),
    row!(REPO_DIST, "prefix", 0, Handler::Interp(prefix)),
    row!(REPO_DIST, "content", 1, Handler::Interp(content)),
    row!(REPO_DIST, "Str", 0, Handler::Narrow(identity)),
    row!(REPO_DIST, "dist-id", 0, Handler::Narrow(identity)),
    row!(LAZY, "meta", 0, Handler::Narrow(meta)),
    row!(LAZY, "prefix", 0, Handler::Interp(prefix)),
    row!(LAZY, "content", 1, Handler::Interp(content)),
    row!(LAZY, "Str", 0, Handler::Narrow(identity)),
    row!(LAZY, "dist-id", 0, Handler::Narrow(dist_id)),
];

/// The attribute `key` of the instance, `None` inside when it is not set.
// Cost: O(1).
fn attr(target: &Value, key: &str) -> Option<Option<Value>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(attributes.as_map().get(key).cloned())
}

/// The distribution's prefix as text: its `prefix` attribute, or the
/// `meta<prefix>` a `$?DISTRIBUTION` keeps it in.
// Cost: O(p), p = chars of the prefix.
fn prefix_text(target: &Value) -> Option<String> {
    let prefix = match attr(target, "prefix")? {
        Some(prefix) => Some(prefix),
        None => attr(target, "meta")?.and_then(|meta| meta.hash_get_str("prefix")),
    };
    Some(prefix.map(|v| v.to_string_value()).unwrap_or_default())
}

/// `.meta`.
// Cost: O(1).
fn meta(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "meta")?.unwrap_or(Value::NIL)))
}

/// `.prefix`: the distribution's directory as an `IO::Path`.
// Cost: O(p), p = chars of the prefix.
fn prefix(interp: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_text(target)?;
    Some(Ok(interp.make_io_path_instance(&prefix)))
}

/// `.content($address)`: an unopened `IO::Handle` on the file a
/// distribution-relative address names. The
/// `files` map comes first (it resolves the platform-specific library names:
/// `resources/libraries/foo` -> `$prefix/resources/libraries/libfoo.so`, and an
/// installed distribution's hashed file names); any other address is a path
/// under the prefix.
// Cost: O(p + m), p = chars of the path, m = provided modules.
fn content(interp: &mut Interpreter, target: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let address = args.first()?.to_string_value();
    let prefix = prefix_text(target)?;
    let files = match attr(target, "files")? {
        Some(files) => Some(files),
        None => attr(target, "meta")?.and_then(|meta| meta.hash_get_str("files")),
    };
    let mapped = files
        .and_then(|files| files.hash_get_str(&address))
        .map(|v| v.to_string_value());
    let provided_source = attr(target, "meta")?
        .and_then(|meta| meta.hash_get_str("provides"))
        .and_then(|provides| match provides.view() {
            ValueView::Hash(modules) => modules.values().find_map(|module| {
                let ValueView::Hash(paths) = module.view() else {
                    return None;
                };
                let entry = paths.get(&address)?;
                // Current installs store { path => { file => id } }. Older
                // metadata omitted this path and falls through to the legacy
                // prefix/address lookup below.
                entry.hash_get_str("file").map(|id| id.to_string_value())
            }),
            _ => None,
        });
    let full_path = match mapped.or_else(|| provided_source.map(|id| format!("sources/{id}"))) {
        Some(abs) if std::path::Path::new(&abs).is_absolute() => abs,
        // A `Distribution::Path` over a relative prefix already maps its files
        // to `prefix/...`: joining the prefix again would double it.
        Some(relative)
            if !prefix.is_empty()
                && std::path::Path::new(&relative).starts_with(&prefix) =>
        {
            relative
        }
        Some(relative) => std::path::Path::new(&prefix)
            .join(relative)
            .to_string_lossy()
            .to_string(),
        None => std::path::Path::new(&prefix)
            .join(&address)
            .to_string_lossy()
            .to_string(),
    };
    // Rakudo's `Distribution::Path.content` answers an unopened `IO::Handle`
    // on the file (`$dist.content($name).open.slurp`, zef's
    // `$handle.close; $handle.path`).
    let path = interp.make_io_path_instance(&full_path);
    Some(interp.dispatch_new(
        Value::package(crate::symbol::Symbol::intern("IO::Handle")),
        vec![Value::pair("path".to_string(), path)],
    ))
}

/// `.Str` (and a `$?DISTRIBUTION`'s `.dist-id`): the identity
/// `name:ver<..>:auth<..>:api<..>`, an absent key rendering empty
/// (`ver` falls back to `version`, `auth` to `authority`).
// Cost: O(n), n = chars of the identity.
fn identity(target: &Value, _args: &[Value]) -> Answer {
    let meta = attr(target, "meta")?.unwrap_or(Value::NIL);
    let field = |keys: &[&str]| {
        keys.iter()
            .find_map(|key| meta.hash_get_str(key).filter(|v| !v.is_nil()))
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    };
    Some(Ok(Value::str(format!(
        "{}:ver<{}>:auth<{}>:api<{}>",
        field(&["name"]),
        field(&["ver", "version"]),
        field(&["auth", "authority"]),
        field(&["api"]),
    ))))
}

/// An installed distribution's `.dist-id`: the id it is stored under.
// Cost: O(1).
fn dist_id(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attr(target, "dist-id")?.unwrap_or(Value::NIL)))
}
