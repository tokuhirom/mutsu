//! The rows of `Rakudo::Internals` and `Rakudo::Sorting`, the core Rakudo
//! classes ecosystem code calls on their type objects with no `use`
//! (`raku -e 'say Rakudo::Internals.IS-WIN'`).
//!
//! Both are type-object-only shapes: nothing instantiates them. Only the
//! handful of methods real distributions call have rows; any other name reaches
//! ordinary method resolution and its `X::Method::NotFound`.
//!
//! - `IS-WIN`: a platform predicate, from the host target (NativeLibs picks the
//!   library name by `Rakudo::Internals.IS-WIN()`). `IS-MACOS` is mutsu's own
//!   companion of it; Rakudo declares no such method.
//! - `INITTIME`: when the process started, as a `Num` of seconds since the epoch
//!   (Telemetry measures its wallclock column from it).
//! - `INCLUDE`: the `-I` paths the running process was started with, read from
//!   `%*COMPILING<%?OPTIONS><I>` exactly as Rakudo does. Test suites use it to
//!   re-exec `$*EXECUTABLE` with the same include path.
//! - `DIR-RECURSE`: every file under a directory, recursively (the helper
//!   `CompUnit::Repository::Installation` and Pakku walk `bin/` and `resources/`
//!   with). `:dir` / `:file` are matchers for the basename of each
//!   subdirectory entered / each file listed.
//! - `Rakudo::Sorting.MERGESORT-str`: sorts a native `str` array (an
//!   `nqp::list_s`) by codepoint and returns the sorted native array
//!   (ValueMap's `WHICH` sorts its keys with it).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!(
        "Rakudo::Internals",
        "IS-WIN",
        0,
        Handler::Narrow(is_win),
        RowFlags::TYPE_OBJECT_OK
    ),
    row!(
        "Rakudo::Internals",
        "IS-MACOS",
        0,
        Handler::Narrow(is_macos),
        RowFlags::TYPE_OBJECT_OK
    ),
    row!(
        "Rakudo::Internals",
        "INITTIME",
        0,
        Handler::Narrow(inittime),
        RowFlags::TYPE_OBJECT_OK
    ),
    row!(
        "Rakudo::Internals",
        "INCLUDE",
        0,
        Handler::Interp(include),
        RowFlags::TYPE_OBJECT_OK
    ),
    MethodRow {
        owner: "Rakudo::Internals",
        name: "DIR-RECURSE",
        arity: 1,
        handler: Handler::Interp(dir_recurse),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &["dir", "file"],
    },
    row!(
        "Rakudo::Sorting",
        "MERGESORT-str",
        1,
        Handler::Narrow(mergesort_str),
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::VM_ARRAY_ARGS)
    ),
];

// Cost: O(1).
fn is_win(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::truth(cfg!(target_os = "windows"))))
}

// Cost: O(1).
fn is_macos(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::truth(cfg!(target_os = "macos"))))
}

// Cost: O(1).
fn inittime(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::num(
        crate::vm::process_start_epoch_nanos() as f64 / 1e9,
    )))
}

// Cost: O(n), n = number of `-I` paths.
fn include(interp: &mut Interpreter, _: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(interp.rakudo_internals_include()))
}

/// `Rakudo::Sorting.MERGESORT-str(@str)`: the keys sorted by codepoint, as a
/// native `str` list.
// Cost: O(n log n) comparisons, each O(p), n = elements, p = common prefix.
// The sort is stable, matching the merge sort it stands in for.
fn mergesort_str(_: &Value, args: &[Value]) -> Answer {
    let [list] = args else {
        return None;
    };
    let ValueView::Array(items, ..) = list.view() else {
        return Some(Err(RuntimeError::new(
            "Rakudo::Sorting.MERGESORT-str expects a native str array".to_string(),
        )));
    };
    let mut keys: Vec<String> = items.iter().map(|v| v.to_string_value()).collect();
    keys.sort_by(|a, b| crate::builtins::str_prim::str_order(a, b));
    let sorted = keys.into_iter().map(Value::str).collect();
    Some(Ok(Value::nqp_typed_list(
        sorted,
        crate::value::NqpElemKind::Str,
    )))
}

/// `Rakudo::Internals.DIR-RECURSE($path, :$dir = True, :$file = True)`: the
/// paths (`$path` joined with the relative name) of every file below `$path`,
/// hidden ones included. `:dir` filters the subdirectories entered and `:file`
/// the files listed, each by smartmatching the entry's basename. A symlink to a
/// directory is not entered (a cycle would never end). Entries come in name
/// order, so the listing is deterministic.
// Cost: O(e log e + m), e = directory entries visited, m = matcher calls (one
// per entry).
fn dir_recurse(interp: &mut Interpreter, _: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let Some(root) = args.first() else {
        return None;
    };
    let root = root.to_string_value();
    let dir_matcher = named.get("dir").cloned();
    let file_matcher = named.get("file").cloned();
    let mut accepts = |matcher: &Option<Value>, name: &str| -> bool {
        match matcher {
            None => true,
            Some(m) if matches!(m.view(), ValueView::Bool(_)) => m.truthy(),
            Some(m) => interp.smart_match_values(&Value::str(name.to_string()), m),
        }
    };
    let mut out = Vec::new();
    let mut pending = vec![root];
    while let Some(dir) = pending.pop() {
        let Ok(read) = std::fs::read_dir(&dir) else {
            continue;
        };
        let mut entries: Vec<_> = read.flatten().collect();
        entries.sort_by_key(|e| e.file_name());
        let mut subdirs = Vec::new();
        for entry in entries {
            let name = entry.file_name().to_string_lossy().to_string();
            let path = format!("{dir}/{name}");
            let Ok(kind) = entry.file_type() else {
                continue;
            };
            if kind.is_dir() {
                if accepts(&dir_matcher, &name) {
                    subdirs.push(path);
                }
            } else if accepts(&file_matcher, &name) {
                out.push(Value::str(path));
            }
        }
        // Depth-first in name order: the next directory popped is the first.
        pending.extend(subdirs.into_iter().rev());
    }
    Some(Ok(Value::seq(out)))
}
