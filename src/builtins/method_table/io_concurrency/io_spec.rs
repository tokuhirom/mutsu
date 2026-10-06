//! The rows of `IO::Spec::Unix`, `IO::Spec::Win32`, `IO::Spec::Cygwin` and
//! `IO::Spec::QNX` (ADR-11276 §9.19): the path-syntax methods `$*SPEC` offers.
//!
//! Rakudo declares each method on the classes that override it and the rest
//! come from `IO::Spec::Unix`, which the others are. One handler serves every
//! class that declares its method, and reads which class the receiver is
//! ([`SpecKind`]); a shape of the `IO::Spec` family reaches its own rows and
//! `IO::Spec::Unix`'s (`DispatchShape::reaches`). The receiver is nearly always
//! the type object (`$*SPEC`), so every row answers one.
//!
//! A row takes the positional arguments Rakudo's signature requires and any
//! number more (the methods are lenient about extras, as they were before the
//! table); the named ones are `:parent` of `canonpath` and `:nofile` of
//! `splitpath`. A call with fewer positionals than the signature requires is
//! an error in Rakudo, and no row answers it.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::{Interpreter, SpecKind};
use crate::value::{RuntimeError, Value, ValueView};

/// The flags of every row: a type object answers, and an argument is any plain
/// one.
const TYPE_OBJECT: RowFlags = RowFlags::TYPE_OBJECT_OK;
const LENIENT: RowFlags = RowFlags::TYPE_OBJECT_OK
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY);

/// One row per owner that declares the method, all with the same handler.
macro_rules! spec_rows {
    ($(($name:literal, $arity:literal, $handler:expr, $flags:expr, $named:expr, [$($owner:literal),+]),)*) => {
        pub(super) static ROWS: &[MethodRow] = &[
            $($(MethodRow {
                owner: $owner,
                name: $name,
                arity: $arity,
                handler: $handler,
                flags: $flags,
                named: $named,
            },)+)*
        ];
    };
}

spec_rows! {
    ("abs2rel", 1, Handler::Narrow(abs2rel_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Cygwin"]),
    ("basename", 1, Handler::Narrow(basename_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32"]),
    ("canonpath", 1, Handler::Named(canonpath_row), LENIENT, &["parent"],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin", "IO::Spec::QNX"]),
    ("catdir", 0, Handler::Narrow(catdir_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("catfile", 0, Handler::Narrow(catfile_row), LENIENT, &[], ["IO::Spec::Unix"]),
    ("catpath", 3, Handler::Narrow(catpath_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("curdir", 0, Handler::Narrow(curdir_row), TYPE_OBJECT, &[], ["IO::Spec::Unix"]),
    ("curupdir", 0, Handler::Interp(curupdir_row), TYPE_OBJECT, &[], ["IO::Spec::Unix"]),
    ("devnull", 0, Handler::Narrow(devnull_row), TYPE_OBJECT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32"]),
    ("dir-sep", 0, Handler::Narrow(dir_sep_row), TYPE_OBJECT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32"]),
    ("extension", 1, Handler::Narrow(extension_row), LENIENT, &[], ["IO::Spec::Unix"]),
    ("is-absolute", 1, Handler::Narrow(is_absolute_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("join", 3, Handler::Narrow(join_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("path", 0, Handler::Interp(path_row), TYPE_OBJECT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32"]),
    ("rel2abs", 1, Handler::Narrow(rel2abs_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("rootdir", 0, Handler::Narrow(rootdir_row), TYPE_OBJECT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32"]),
    ("split", 1, Handler::Narrow(split_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("splitdir", 1, Handler::Narrow(splitdir_row), LENIENT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32"]),
    ("splitpath", 1, Handler::Named(splitpath_row), LENIENT, &["nofile"],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("tmpdir", 0, Handler::Interp(tmpdir_row), TYPE_OBJECT, &[],
        ["IO::Spec::Unix", "IO::Spec::Win32", "IO::Spec::Cygwin"]),
    ("updir", 0, Handler::Narrow(updir_row), TYPE_OBJECT, &[], ["IO::Spec::Unix"]),
}

/// A pure row: the receiver's class and the arguments.
macro_rules! narrow {
    ($(#[$doc:meta])* $handler:ident, $op:ident) => {
        $(#[$doc])*
        fn $handler(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
            Some(Interpreter::$op(SpecKind::of(target)?, args))
        }
    };
}

/// A pure row of a method that takes no argument.
macro_rules! constant {
    ($(#[$doc:meta])* $handler:ident, $op:ident) => {
        $(#[$doc])*
        fn $handler(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
            Some(Interpreter::$op(SpecKind::of(target)?))
        }
    };
}

narrow!(
    /// `abs2rel($path, $base)`.
    // Cost: O(n), n = path and base length.
    abs2rel_row, io_spec_abs2rel
);
narrow!(
    /// `basename($path)`.
    // Cost: O(n), n = path length.
    basename_row, io_spec_basename
);
narrow!(
    /// `catdir(*@parts)`.
    // Cost: O(n), n = total chars of the parts.
    catdir_row, io_spec_catdir
);
narrow!(
    /// `catfile(*@parts)`.
    // Cost: O(n), n = total chars of the parts.
    catfile_row, io_spec_catfile
);
narrow!(
    /// `catpath($volume, $dir, $file)`.
    // Cost: O(n), n = total input length.
    catpath_row, io_spec_catpath
);
narrow!(
    /// `extension($path)`.
    // Cost: O(n), n = path length.
    extension_row, io_spec_extension
);
narrow!(
    /// `is-absolute($path)`.
    // Cost: O(p), p = chars of the path.
    is_absolute_row, io_spec_is_absolute
);
narrow!(
    /// `join($volume, $dirname, $basename)`.
    // Cost: O(n), n = total input length.
    join_row, io_spec_join
);
narrow!(
    /// `rel2abs($path, $base)`.
    // Cost: O(n), n = path, base and cwd length.
    rel2abs_row, io_spec_rel2abs
);
narrow!(
    /// `split($path)`: an `IO::Path::Parts`.
    // Cost: O(n), n = path length.
    split_row, io_spec_split
);
narrow!(
    /// `splitdir($path)`.
    // Cost: O(n), n = path length.
    splitdir_row, io_spec_splitdir
);
constant!(
    /// `devnull`.
    // Cost: O(1).
    devnull_row, io_spec_devnull
);
constant!(
    /// `dir-sep`.
    // Cost: O(1).
    dir_sep_row, io_spec_dir_sep
);
constant!(
    /// `rootdir`.
    // Cost: O(1).
    rootdir_row, io_spec_rootdir
);

/// `curdir`: `.`.
// Cost: O(1).
fn curdir_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    SpecKind::of(target)?;
    Some(Ok(Value::str_from(".")))
}

/// `updir`: `..`.
// Cost: O(1).
fn updir_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    SpecKind::of(target)?;
    Some(Ok(Value::str_from("..")))
}

/// `curupdir`: the test object that matches `.` and `..`. Every call makes a
/// new object, an effect the debug cross-check cannot re-run and compare, so
/// the row is an interpreter row though it reads nothing of it.
// Cost: O(1).
fn curupdir_row(
    _interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    SpecKind::of(target)?;
    Some(Interpreter::io_spec_curupdir())
}

/// `canonpath($path, :parent)`: the path with its redundant separators and `.`
/// segments folded (and `..` with `:parent`). An undefined path is the empty
/// string.
// Cost: O(n), n = path length.
fn canonpath_row(
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let kind = SpecKind::of(target)?;
    let parent = named.get("parent").is_some_and(Value::truthy);
    let first = args.first();
    if matches!(
        first.map(Value::view),
        None | Some(ValueView::Nil) | Some(ValueView::Package(_))
    ) {
        return Some(Ok(Value::str_from("")));
    }
    let path = first.map(Value::to_string_value).unwrap_or_default();
    Some(Ok(Value::str(match kind {
        SpecKind::Win32 => Interpreter::canonpath_win32(&path, parent),
        SpecKind::Cygwin => Interpreter::canonpath_cygwin(&path, parent),
        SpecKind::Qnx => Interpreter::canonpath_qnx(&path, parent),
        SpecKind::Unix => Interpreter::canonpath_unix(&path, parent),
    })))
}

/// `splitpath($path, :nofile)`.
// Cost: O(n), n = path length.
fn splitpath_row(
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let kind = SpecKind::of(target)?;
    let joined: Vec<Value> = args.iter().chain(named.pairs()).cloned().collect();
    Some(Interpreter::io_spec_splitpath(kind, &joined))
}

/// `path`: the directories of `%*ENV<PATH>`.
// Cost: O(n), n = chars of the variable.
fn path_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let kind = SpecKind::of(target)?;
    Some(Ok(interp.io_spec_path(kind)))
}

/// `tmpdir`: the temporary directory, as an `IO::Path`.
// Cost: O(1).
fn tmpdir_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    SpecKind::of(target)?;
    Some(Ok(interp.io_spec_tmpdir()))
}
