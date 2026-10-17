//! The rows of the `Systemic` classes (ADR-11276 §9.155): `Distro`, `Kernel`,
//! `VM`, `Raku` (and its old name `Perl`) and `Compiler`, the objects behind
//! `$*DISTRO`, `$*KERNEL`, `$*VM`, `$*RAKU` and `$*RAKU.compiler`.
//!
//! None of them has a shape, so every row is reached through its owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`). Most methods read the attribute of
//! their name, which the runtime filled from the running process when it built
//! the object; the others compute their answer through one `Interpreter`
//! method each. A method Rakudo does not declare on the class (`Distro.raku`)
//! is `Mu`'s.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A row that answers the receiver's attribute named `$name`.
macro_rules! attr {
    ($owner:literal, $name:literal) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow(|target, _| attribute(target, $name)),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK),
            named: &[],
        }
    };
}

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
    ($owner:literal, $name:literal, $arity:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: RowFlags::OWNER_ONLY.or($flags),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    attr!("Distro", "name"),
    attr!("Distro", "auth"),
    attr!("Distro", "desc"),
    attr!("Distro", "release"),
    attr!("Distro", "path-sep"),
    attr!("Distro", "is-win"),
    attr!("Distro", "version"),
    attr!("Distro", "signature"),
    row!("Distro", "gist", 0, Handler::Narrow(name_and_version)),
    row!("Distro", "Str", 0, Handler::Narrow(name)),
    attr!("Kernel", "name"),
    attr!("Kernel", "auth"),
    attr!("Kernel", "desc"),
    attr!("Kernel", "release"),
    attr!("Kernel", "hardware"),
    attr!("Kernel", "arch"),
    attr!("Kernel", "bits"),
    attr!("Kernel", "hostname"),
    attr!("Kernel", "version"),
    attr!("Kernel", "signature"),
    attr!("Kernel", "signals"),
    attr!("Kernel", "endian"),
    row!(
        "Kernel",
        "cpu-cores",
        0,
        Handler::Narrow(cpu_cores),
        RowFlags::TYPE_OBJECT_OK
    ),
    row!(
        "Kernel",
        "free-memory",
        0,
        Handler::Narrow(free_memory),
        RowFlags::TYPE_OBJECT_OK
    ),
    row!(
        "Kernel",
        "total-memory",
        0,
        Handler::Narrow(total_memory),
        RowFlags::TYPE_OBJECT_OK
    ),
    row!(
        "Kernel",
        "signal",
        1,
        Handler::Narrow(signal),
        RowFlags::ANY_ARGS
    ),
    row!("Kernel", "gist", 0, Handler::Narrow(name)),
    row!("Kernel", "Str", 0, Handler::Narrow(name)),
    row!("Kernel", "raku", 0, Handler::Narrow(kernel_raku)),
    attr!("VM", "name"),
    attr!("VM", "auth"),
    attr!("VM", "version"),
    attr!("VM", "precomp-ext"),
    attr!("VM", "precomp-target"),
    attr!("VM", "prefix"),
    attr!("VM", "desc"),
    attr!("VM", "signature"),
    attr!("VM", "config"),
    attr!("VM", "osname"),
    row!(
        "VM",
        "request-garbage-collection",
        0,
        Handler::Interp(request_garbage_collection)
    ),
    row!(
        "VM",
        "platform-library-name",
        1,
        Handler::Interp(platform_library_name),
        RowFlags::ANY_ARGS.or(RowFlags::ANY_NAMED)
    ),
    row!("VM", "gist", 0, Handler::Narrow(name)),
    row!("VM", "Str", 0, Handler::Narrow(name)),
    attr!("Raku", "name"),
    attr!("Raku", "auth"),
    attr!("Raku", "desc"),
    attr!("Raku", "version"),
    attr!("Raku", "signature"),
    row!("Raku", "compiler", 0, Handler::Narrow(compiler)),
    row!("Raku", "gist", 0, Handler::Narrow(name_and_version)),
    row!("Raku", "Str", 0, Handler::Narrow(name)),
    attr!("Compiler", "name"),
    attr!("Compiler", "auth"),
    attr!("Compiler", "desc"),
    attr!("Compiler", "version"),
    attr!("Compiler", "signature"),
    attr!("Compiler", "release"),
    attr!("Compiler", "codename"),
    row!(
        "Compiler",
        "id",
        0,
        Handler::Narrow(compiler_id),
        RowFlags::TYPE_OBJECT_OK
    ),
    row!("Compiler", "backend", 0, Handler::Narrow(backend)),
    row!(
        "Compiler",
        "verbose-config",
        0,
        Handler::Narrow(verbose_config)
    ),
    row!("Compiler", "gist", 0, Handler::Narrow(name_and_version)),
    row!("Compiler", "Str", 0, Handler::Narrow(name)),
];

/// The owners a `Systemic` receiver's methods are looked up in, for an
/// instance or type object of `class` (`Perl` is the old name of `Raku`).
// Cost: O(1).
pub(crate) fn owners_of(class: &str) -> Option<&'static [&'static str]> {
    Some(match class {
        "Distro" => &["Distro"],
        "Kernel" => &["Kernel"],
        "VM" => &["VM"],
        "Raku" | "Perl" => &["Raku"],
        "Compiler" => &["Compiler"],
        _ => return None,
    })
}

/// The methods `owner` declares here: the native methods of the class
/// `runtime_init` registers (`Perl` is `Raku`), so that list cannot drift from
/// the rows.
// Cost: O(r), r = rows of the family.
pub(crate) fn names(owner: &str) -> impl Iterator<Item = &'static str> + '_ {
    let owner = if owner == "Perl" { "Raku" } else { owner };
    ROWS.iter()
        .filter(move |row| row.owner == owner)
        .map(|row| row.name)
}

/// The attribute `name` of the receiver; `Nil` when it has none (a type object).
// Cost: O(1).
fn attribute(target: &Value, name: &str) -> Answer {
    Some(Ok(match target.view() {
        ValueView::Instance { attributes, .. } => {
            attributes.as_map().get(name).cloned().unwrap_or(Value::NIL)
        }
        _ => Value::NIL,
    }))
}

/// The attribute `name` of the receiver as a string, empty when it has none.
// Cost: O(n), n = chars of the attribute.
fn text(target: &Value, name: &str) -> String {
    match target.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get(name)
            .map(Value::to_string_value)
            .unwrap_or_default(),
        _ => String::new(),
    }
}

/// `Str` (and `gist` of `Kernel` and `VM`): the receiver's `name`.
// Cost: O(n), n = chars of the name.
fn name(target: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::str(text(target, "name"))))
}

/// `gist` of `Distro`, `Raku` and `Compiler`: `name (version)`.
// Cost: O(n), n = chars of the rendering.
fn name_and_version(target: &Value, _: &[Value]) -> Answer {
    let version = match target.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get("version")
            .map(|v| match v.view() {
                ValueView::Version { parts, .. } => Value::version_parts_to_string(parts),
                _ => v.to_string_value(),
            })
            .unwrap_or_default(),
        _ => String::new(),
    };
    Some(Ok(Value::str(format!(
        "{} ({version})",
        text(target, "name")
    ))))
}

/// `Kernel.raku`.
// Cost: O(n), n = chars of the rendering.
fn kernel_raku(target: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::str(format!(
        "Kernel.new(release => Str, hardware => Str, arch => Str, bits => Int, name => \"{}\", auth => \"{}\", version => Version, signature => Blob, desc => Str)",
        text(target, "name"),
        text(target, "auth")
    ))))
}

// Cost: O(1) plus one syscall.
fn cpu_cores(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::int(
        crate::runtime::sys_resources::cpu_core_count(),
    )))
}

// Cost: O(f), f = size of /proc/meminfo.
fn free_memory(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::int(crate::runtime::sys_resources::free_memory())))
}

// Cost: O(f), f = size of /proc/meminfo.
fn total_memory(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(
        Value::int(crate::runtime::sys_resources::total_memory()),
    ))
}

/// `Kernel.signal`: the number of a signal named by an `Int`, a name with or
/// without its `SIG` prefix, or a `Signal` enum value.
// Cost: O(s), s = signals in the table.
fn signal(_: &Value, args: &[Value]) -> Answer {
    Some(Ok(Value::int(match args[0].view() {
        ValueView::Int(n) => n,
        ValueView::Str(s) => crate::runtime::signal_table::signal_number(&s),
        ValueView::Enum { key, .. } => crate::runtime::signal_table::signal_number(&key.resolve()),
        _ => 0,
    })))
}

// Cost: one cycle collect plus the pending DESTROYs.
fn request_garbage_collection(i: &mut Interpreter, _: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.request_garbage_collection().map(|_| Value::NIL))
}

// Cost: O(n), n = chars of the name.
fn platform_library_name(
    i: &mut Interpreter,
    _: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    Some(Ok(i.platform_library_name(&args[0], named.pairs())))
}

// Cost: O(1).
fn compiler(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Interpreter::compiler_instance()))
}

// Cost: O(1).
fn compiler_id(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::str(Interpreter::compiler_id())))
}

// Cost: O(1).
fn backend(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::str_from("mutsu")))
}

// Cost: O(k), k = configuration keys.
fn verbose_config(_: &Value, _: &[Value]) -> Answer {
    Some(Ok(Interpreter::compiler_verbose_config()))
}
