//! `IO::Path`'s lexical rows (ADR-11276 §9.19): the methods that derive a new
//! path (or a string or a bool) from the receiver's `path` and `SPEC`
//! attributes and the arguments alone, with no filesystem access, no cwd and no
//! environment.
//!
//! Rakudo declares each on `IO::Path`; the `IO::Path::Unix`, `Win32`, `Cygwin`
//! and `QNX` classes share these rows (one shape, `DispatchShape::IoPath`), and
//! a handler round-trips the receiver's concrete class, so
//! `IO::Path::Win32.new("x").parent` stays an `IO::Path::Win32`. A user
//! subclass of `IO::Path` has no shape and reaches the same handlers through
//! the owner lookup of the slow path (`method_table::invoke_owner`).

use super::io_path_ctx::PathCtx;
use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};
use std::collections::HashMap;
use std::path::Path;

/// A zero-argument row.
macro_rules! row0 {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "IO::Path",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

/// A row with positional arguments (any plain scalar) and no named one.
macro_rules! rown {
    ($name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: "IO::Path",
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: RowFlags::ANY_ARGS,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row0!("Str", str_row),
    row0!("gist", gist_row),
    row0!("IO", io_row),
    row0!("SPEC", spec_row),
    row0!("basename", basename_row),
    row0!("dirname", dirname_row),
    row0!("volume", volume_row),
    row0!("cleanup", cleanup_row),
    row0!("parts", parts_row),
    row0!("parent", parent_row),
    rown!("parent", 1, parent_row),
    rown!("sibling", 1, sibling_row),
    MethodRow {
        owner: "IO::Path",
        name: "add",
        arity: 0,
        handler: Handler::Narrow(add_row),
        flags: RowFlags::ANY_ARGS.or(RowFlags::SLURPY),
        named: &[],
    },
    row0!("is-absolute", is_absolute_row),
    row0!("is-relative", is_relative_row),
    row0!("succ", succ_row),
    row0!("pred", pred_row),
    MethodRow {
        owner: "IO::Path",
        name: "extension",
        arity: 0,
        handler: Handler::Named(extension_row),
        flags: RowFlags::NONE,
        named: &["parts"],
    },
    MethodRow {
        owner: "IO::Path",
        name: "extension",
        arity: 1,
        handler: Handler::Named(extension_row),
        flags: RowFlags::ANY_ARGS,
        named: &["parts", "joiner"],
    },
];

/// `IO::Path.Str`: the path as given.
// Cost: O(p), p = chars of the path.
fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(Value::str(ctx.path)))
}

/// `IO::Path.gist`: the path as the `.IO` call that makes it.
// Cost: O(p), p = chars of the path.
fn gist_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let p = &ctx.path;
    let attributes = &ctx.attributes;
    let original = Path::new(p);
    let shown = if Interpreter::io_path_is_absolute_spec(attributes, p, original) {
        if Interpreter::is_win32_spec(attributes) && (p.ends_with('/') || p.ends_with('\\')) {
            p.clone()
        } else if Interpreter::is_win32_spec(attributes) {
            Interpreter::canonpath_win32(p, false)
        } else if Interpreter::is_cygwin_spec(attributes) {
            Interpreter::canonpath_cygwin(&p.replace('\\', "/"), false)
        } else {
            p.clone()
        }
    } else {
        p.clone()
    };
    Some(Ok(Value::str(format!(
        "\"{}\".IO",
        shown.replace('"', "\\\"")
    ))))
}

/// `IO::Path.IO`: the path itself.
// Cost: O(a), a = attributes of the instance.
fn io_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(Value::make_instance(ctx.class, ctx.attributes)))
}

/// `IO::Path.SPEC`: the `IO::Spec` type object the path follows.
// Cost: O(a), a = attributes of the instance.
fn spec_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let spec_name = ctx
        .attributes
        .get("SPEC")
        .and_then(|s| match s.view() {
            ValueView::Package(n) => Some(n.resolve().to_string()),
            ValueView::Instance { class_name, .. } => Some(class_name.resolve().to_string()),
            _ => None,
        })
        .unwrap_or_else(|| "IO::Spec::Unix".to_string());
    Some(Ok(Value::package(Symbol::intern(&spec_name))))
}

/// `IO::Path.basename`.
// Cost: O(p), p = chars of the path.
fn basename_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let (_, _, basename) = Interpreter::io_path_parts_spec(&ctx.path, &ctx.attributes);
    Some(Ok(Value::str(basename)))
}

/// `IO::Path.dirname`.
// Cost: O(p), p = chars of the path.
fn dirname_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let (_, dirname, _) = Interpreter::io_path_parts_spec(&ctx.path, &ctx.attributes);
    Some(Ok(Value::str(dirname)))
}

/// `IO::Path.volume`.
// Cost: O(p), p = chars of the path.
fn volume_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let (volume, _, _) = Interpreter::io_path_parts_spec(&ctx.path, &ctx.attributes);
    Some(Ok(Value::str(volume)))
}

/// `IO::Path.cleanup`: the path with its `.` and `..` segments and repeated
/// separators folded, purely lexically.
// Cost: O(p), p = chars of the path.
fn cleanup_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let normalized = if Interpreter::is_win32_spec(&ctx.attributes) {
        Interpreter::cleanup_io_path_lexical_win32(&ctx.path)
    } else {
        Interpreter::cleanup_io_path_lexical(&ctx.path)
    };
    Some(Ok(ctx.with_path(normalized)))
}

/// `IO::Path.parts`: the `volume`, `dirname` and `basename` as an
/// `IO::Path::Parts`.
// Cost: O(p), p = chars of the path.
fn parts_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let (volume, dirname, basename) = Interpreter::io_path_parts_spec(&ctx.path, &ctx.attributes);
    let mut parts = HashMap::new();
    parts.insert("volume".to_string(), Value::str(volume));
    parts.insert("dirname".to_string(), Value::str(dirname));
    parts.insert("basename".to_string(), Value::str(basename));
    Some(Ok(Value::make_instance(
        Symbol::intern("IO::Path::Parts"),
        parts,
    )))
}

/// `IO::Path.parent` and `parent($levels)`: the path `$levels` directories up.
// Cost: O(l * p), l = levels, p = chars of the path.
fn parent_row(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let p = &ctx.path;
    let mut levels = 1i64;
    if let Some(ValueView::Int(i)) = args.first().map(Value::view) {
        if i < 0 {
            return Some(Err(RuntimeError::new(
                "X::IO::ParentOutOfRange: Cannot go to a negative parent",
            )));
        }
        levels = i;
    }
    if levels == 0 {
        return Some(Ok(Value::make_instance(ctx.class, ctx.attributes)));
    }
    let sep = Interpreter::io_path_sep(&ctx.attributes);
    let mut path = if Interpreter::is_cygwin_spec(&ctx.attributes) {
        p.replace('\\', "/")
    } else {
        p.clone()
    };
    for _ in 0..levels {
        // A bare current-directory path — with or without a trailing
        // separator (`.`, `./`, `.\`) — has `..` as its parent, like
        // raku (`'./'.IO.parent` is `".."`).
        if path == "." || path == "./" || path == ".\\" {
            path = "..".to_string();
            continue;
        }
        // A path that is NOTHING BUT a chain of `..` segments
        // (`..`, `../..`, `../../..`, ...) has no real directory
        // component left to strip: raku's `.parent` stacks one
        // more `..` instead of collapsing back to the previous
        // level (`'..'.IO.parent` is `"../.."`, not `.`).
        // Critically, this does NOT apply once a real name
        // appears anywhere before the trailing `..` — `foo/..`,
        // `/foo/..` and `../a/..` all still just strip their
        // ordinary dirname (`foo`, `/foo`, `../a`), matching
        // raku's `roast/S32-io/io-path-unix.t` exactly; only the
        // *entirely*-dotdot dirname (`.`, `..`, `../..`, ...)
        // marks a path with nothing further to remove.
        let (volume, dirname, basename) = Interpreter::io_path_parts(&path);
        let dirname_is_pure_dotdot = dirname == "."
            || (volume.is_empty()
                && !dirname.starts_with(['/', '\\'])
                && !dirname.is_empty()
                && dirname.split(['/', '\\']).all(|seg| seg == ".."));
        if basename == ".." && dirname_is_pure_dotdot {
            path = format!("{}{}{}", path, sep, "..");
            continue;
        }
        let full_vol_dir = format!("{}{}", volume, dirname);
        if dirname == "/" || dirname == "\\" {
            let new_path = format!("{}{}", volume, dirname);
            if new_path == path {
                break;
            }
            path = new_path;
        } else if dirname == "." && volume.is_empty() {
            path = ".".to_string();
        } else {
            path = full_vol_dir;
        }
    }
    Some(Ok(ctx.with_path(path)))
}

/// `IO::Path.sibling($name)`: the path of `$name` in the same directory.
// Cost: O(p + n), p = chars of the path, n = chars of the name.
fn sibling_row(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let sibling_name = args.first().map(Value::to_string_value).unwrap_or_default();
    let (volume, dirname, _) = Interpreter::io_path_parts(&ctx.path);
    let dir_with_volume = format!("{}{}", volume, dirname);
    let sibling_path = if dir_with_volume == "/" || dir_with_volume == "\\" {
        format!("{}{}", dir_with_volume, sibling_name)
    } else if dir_with_volume == "." {
        sibling_name
    } else if dir_with_volume.ends_with('/') || dir_with_volume.ends_with('\\') {
        format!("{}{}", dir_with_volume, sibling_name)
    } else {
        format!("{}/{}", dir_with_volume, sibling_name)
    };
    Some(Ok(ctx.with_path(sibling_path)))
}

/// `IO::Path.add(*@children)`: a lexical join of each child, flattened, onto
/// the path. `.add(<bar baz>)` is `foo/bar/baz`, not `foo/bar baz`.
// Cost: O(c * p), c = children after flattening, p = chars of the joined path.
fn add_row(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let mut children = Vec::new();
    for value in args {
        crate::builtins::flat_val(value, &mut children, true);
    }
    Some(join_children(
        &ctx,
        children.iter().map(Value::to_string_value),
    ))
}

fn join_children(
    ctx: &PathCtx,
    segments: impl Iterator<Item = String>,
) -> Result<Value, RuntimeError> {
    let mut current = ctx.path.clone();
    for segment in segments {
        current = Interpreter::io_path_join_child(&ctx.attributes, &current, &segment)?;
    }
    Ok(ctx.with_path(current))
}

/// `IO::Path.extension`, `extension($replacement)`: the extension, or the path
/// with it replaced. `:parts` selects how many dot-separated parts count and
/// `:joiner` is what goes before the replacement.
// Cost: O(p), p = chars of the path.
fn extension_row(
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    let p = &ctx.path;
    let subst = args.first().map(Value::to_string_value);
    let parts = named.get("parts").cloned();
    let joiner = named.get("joiner").cloned();
    Some((|| -> Result<Value, RuntimeError> {
        let parts_spec = Interpreter::io_path_extension_parts_spec_of(parts.as_ref())?;
        let selected_parts = parts_spec.select(Interpreter::io_path_extension_part_count(p));
        let Some(subst) = subst else {
            let Some(parts) = selected_parts else {
                return Ok(Value::str(String::new()));
            };
            return Ok(Value::str(
                Interpreter::io_path_extension_with_n_parts(p, parts).unwrap_or_default(),
            ));
        };
        let Some(parts_to_replace) = selected_parts else {
            return Ok(Value::make_instance(ctx.class, ctx.attributes.clone()));
        };
        let joiner = joiner.map(|v| v.to_string_value()).unwrap_or_else(|| {
            if subst.is_empty() {
                "".to_string()
            } else {
                ".".to_string()
            }
        });
        let (dir_prefix, basename) = Interpreter::split_path_for_extension(p);
        let Some(base_without_ext) =
            Interpreter::io_path_extension_strip_n_parts(basename, parts_to_replace)
        else {
            return Ok(Value::make_instance(ctx.class, ctx.attributes.clone()));
        };
        let mut new_basename = format!("{base_without_ext}{joiner}{subst}");
        if new_basename.is_empty() {
            new_basename = ".".to_string();
        }
        Ok(ctx.with_path(format!("{dir_prefix}{new_basename}")))
    })())
}

/// `IO::Path.is-absolute`.
// Cost: O(p), p = chars of the path.
fn is_absolute_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(Value::truth(Interpreter::io_path_is_absolute_spec(
        &ctx.attributes,
        &ctx.path,
        Path::new(&ctx.path),
    ))))
}

/// `IO::Path.is-relative`.
// Cost: O(p), p = chars of the path.
fn is_relative_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(Value::truth(!Interpreter::io_path_is_absolute_spec(
        &ctx.attributes,
        &ctx.path,
        Path::new(&ctx.path),
    ))))
}

/// `IO::Path.succ`: the path with its basename incremented as a string.
// Cost: O(p), p = chars of the path.
fn succ_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(step_basename(
        &ctx,
        crate::value::str_increment::string_succ,
    )))
}

/// `IO::Path.pred`: the path with its basename decremented as a string.
// Cost: O(p), p = chars of the path.
fn pred_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(step_basename(
        &ctx,
        crate::value::str_increment::string_pred,
    )))
}

fn step_basename(ctx: &PathCtx, step: fn(&str) -> String) -> Value {
    let (volume, dirname, basename) = Interpreter::io_path_parts(&ctx.path);
    let sep = Interpreter::io_path_sep(&ctx.attributes);
    ctx.with_path(Interpreter::join_io_path_parts(
        &volume,
        &dirname,
        &step(&basename),
        sep,
    ))
}
