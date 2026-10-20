//! The rows of the `Systemic` classes (ADR-11276 §9.155): `Distro`, `Kernel`,
//! `VM`, `Raku` (and its old name `Perl`) and `Compiler`, the objects behind
//! `$*DISTRO`, `$*KERNEL`, `$*VM`, `$*RAKU` and `$*RAKU.compiler`.
//!
//! Each has an opaque dispatch shape (`DispatchShape::is_opaque`), so every
//! call entry reaches the rows through the table. Most methods read the attribute of
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
            // An attribute needs an instance: the type object's call raises
            // Rakudo's "Cannot look up attributes in a ... type object".
            flags: RowFlags::NONE,
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
            flags: RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS),
            named: &[],
        }
    };
    ($owner:literal, $name:literal, $arity:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS).or($flags),
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
    row!(
        "Kernel",
        "hostname",
        0,
        Handler::Narrow(kernel_hostname),
        RowFlags::TYPE_OBJECT_OK
    ),
    attr!("Kernel", "version"),
    attr!("Kernel", "signature"),
    // A method, not an attribute: its type object is refused for its `:D`
    // invocant (#13952).
    row!(
        "Kernel",
        "signals",
        0,
        Handler::Narrow(|target, _| attribute(target, "signals")),
        RowFlags::DEFINITE
    ),
    row!(
        "Kernel",
        "endian",
        0,
        Handler::Narrow(kernel_endian),
        RowFlags::TYPE_OBJECT_OK
    ),
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
        // It changes between two reads.
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::RANDOM)
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
    // Rakudo has no `VM.properties` (RT #124624), but roast pins it
    // (`S02-magicals/VM.t`), so it stays a row.
    attr!("VM", "properties"),
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
    // `Raku.version` is the language revision, so the type object answers it
    // too (#13952).
    row!(
        "Raku",
        "version",
        0,
        Handler::Narrow(raku_version),
        RowFlags::TYPE_OBJECT_OK
    ),
    attr!("Raku", "signature"),
    // A new `Compiler` object each call: an identity of its own.
    row!(
        "Raku",
        "compiler",
        0,
        Handler::Narrow(compiler),
        RowFlags::RANDOM
    ),
    row!(
        "Raku",
        "legacy",
        0,
        Handler::Narrow(raku_legacy),
        RowFlags::TYPE_OBJECT_OK
    ),
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

/// `Raku.version`: the instance's `version`, or for the type object the
/// language revision in force.
// Cost: O(1).
fn raku_version(target: &Value, _: &[Value]) -> Answer {
    match target.view() {
        ValueView::Instance { .. } => attribute(target, "version"),
        _ => Some(Ok(Interpreter::language_version_value())),
    }
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

/// `Kernel.hostname`: the instance's attribute, or for the type object the
/// running host's name (`Kernel.hostname` is what `Sys::Hostname` calls).
// Cost: O(1), reads the process-cached uname(2) result.
fn kernel_hostname(target: &Value, _: &[Value]) -> Answer {
    if let ValueView::Instance { .. } = target.view() {
        return attribute(target, "hostname");
    }
    Some(Ok(Value::str(
        crate::runtime::io_sysinfo_host::host_info()
            .hostname
            .clone(),
    )))
}

/// `Kernel.endian`: the instance's attribute, or for the type object the
/// byte order mutsu was built for.
// Cost: O(1).
fn kernel_endian(target: &Value, _: &[Value]) -> Answer {
    if let ValueView::Instance { .. } = target.view() {
        return attribute(target, "endian");
    }
    let little = cfg!(target_endian = "little");
    let index = if little { 1 } else { 2 };
    Some(Ok(Value::enum_parts(
        crate::symbol::Symbol::intern("Endian"),
        crate::symbol::Symbol::intern(if little { "LittleEndian" } else { "BigEndian" }),
        crate::value::EnumValue::Int(index),
        index as usize,
    )))
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

/// `Raku.legacy` (`method legacy(Raku:U:)`, ADR-0098): which compiler frontend
/// compiles a module -- `True` under the NQP/`Perl6::World` one, `False` under
/// RakuAST. mutsu is neither, but the compile-time surface it offers is the
/// RakuAST-shaped one (a `$*LANG` handle with `define_slang`/`slang_grammar`/
/// `slang_actions`, ADR-0026, ADR-0091; there is no `$*W` World to mix a role
/// into), so `False` names the branch a module can honour. The row answers the
/// type object (§9.196's hook) and refuses an instance, as Rakudo does.
// Cost: O(1).
fn raku_legacy(target: &Value, _: &[Value]) -> Answer {
    if let Some(e) = super::class_methods::type_object_only(target, "legacy") {
        return Some(Err(e));
    }
    Some(Ok(Value::FALSE))
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
