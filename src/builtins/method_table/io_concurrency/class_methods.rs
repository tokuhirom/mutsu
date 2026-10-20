//! Class methods of built-in classes, called on the type object (ADR-11276
//! §9.122): `Supply`'s `from-list`, `on-demand` and `interval` (§9.196), `CompUnit::RepositoryRegistry`'s
//! `repository-for-name`, `repository-for-spec` and `run-script`,
//! `Encoding::Registry`'s `find` and `register`, `IO::Socket::INET`'s `connect`
//! and `listen`, and `IO::Socket::Async`'s `connect`, `listen`, `bind-udp` and
//! `udp`; `Rakudo::Internals.REGISTER-DYNAMIC`, `Deprecation.report` and
//! `CompUnit::Loader`'s `load-source` and `load-source-file` (§9.159);
//! `Thread.yield`.
//!
//! The rows are reached through their owner (`RowFlags::OWNER_ONLY`,
//! `RowFlags::CLASS_METHOD`) from the by-name dispatch's one class-method
//! lookup (`Interpreter::type_object_class_method`), for the type object of
//! the owner or of a user subclass of it. The
//! implementations read the call's positional and named arguments as one list
//! (`:$scheduler`, `:$enc`, ...), so a handler hands them back that way; the
//! socket and registry rows take any number of arguments (`SLURPY`), as the
//! arms did.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A class-method row of `$owner` taking `$arity` positional arguments or
/// more.
macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        row!($owner, $name, $arity, $handler, named: &[])
    };
    ($owner:literal, $name:literal, $arity:literal, $handler:ident, named: $named:expr) => {
        row!($owner, $name, $arity, $handler, RowFlags::NONE, $named)
    };
    ($owner:literal, $name:literal, $arity:literal, $handler:ident, $extra:expr, $named:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY
                .or(RowFlags::TYPE_OBJECT_OK)
                .or(RowFlags::CLASS_METHOD)
                .or(RowFlags::SLURPY)
                .or($extra),
            named: $named,
        }
    };
}

/// The named arguments of `IO::Socket::Async`'s connecting class methods.
const ASYNC_SOCKET_NAMED: &[&str] = &["enc", "scheduler"];
/// ... and of its UDP ones.
const ASYNC_UDP_NAMED: &[&str] = &["broadcast", "enc", "scheduler"];

pub(super) static ROWS: &[MethodRow] = &[
    row!("Supply", "from-list", 0, supply_from_list, named: &["scheduler"]),
    row!("Supply", "on-demand", 1, supply_on_demand, named: &["closing", "scheduler"]),
    row!("Supply", "interval", 0, supply_interval, named: &["scheduler"]),
    row!(
        "CompUnit::RepositoryRegistry",
        "repository-for-name",
        1,
        repository_for_name
    ),
    row!(
        "CompUnit::RepositoryRegistry",
        "repository-for-spec",
        1,
        repository_for_spec,
        named: &["next-repo"]
    ),
    row!("CompUnit::RepositoryRegistry", "run-script", 1, run_script),
    row!("Encoding::Registry", "find", 1, encoding_find),
    row!("Encoding::Registry", "register", 1, encoding_register),
    row!("IO::Socket::INET", "connect", 0, inet_connect, named: &["family"]),
    row!("IO::Socket::INET", "listen", 0, inet_listen, named: &["family"]),
    row!("IO::Socket::Async", "connect", 0, async_connect, named: ASYNC_SOCKET_NAMED),
    row!("IO::Socket::Async", "listen", 0, async_listen, named: ASYNC_SOCKET_NAMED),
    row!("IO::Socket::Async", "bind-udp", 0, async_bind_udp, named: ASYNC_UDP_NAMED),
    row!("IO::Socket::Async", "udp", 0, async_udp, named: ASYNC_UDP_NAMED),
    row!("Seq", "from-loop", 0, seq_from_loop, named: &["fire-last", "label", "repeat"]),
    row!("X::AdHoc", "from-slurpy", 0, adhoc_from_slurpy),
    // `Thread.start(&code, *%adverbs)`: `:name`, `:app_lifetime`, and the
    // rest passed on to `Thread.new`.
    row!("Thread", "start", 1, thread_start, RowFlags::ANY_NAMED, &[]),
    row!("Thread", "yield", 0, thread_yield),
    row!("Rakudo::Internals", "REGISTER-DYNAMIC", 0, register_dynamic, named: &["override"]),
    row!("Deprecation", "report", 0, deprecation_report),
    row!("CompUnit::Loader", "load-source", 0, load_source),
    row!("CompUnit::Loader", "load-source-file", 0, load_source_file),
    // `IO::Special.new($what)`: the stand-in for a standard stream (§9.197).
    MethodRow {
        owner: "IO::Special",
        name: "new",
        arity: 1,
        handler: Handler::Narrow(io_special_new),
        flags: RowFlags::OWNER_ONLY
            .or(RowFlags::TYPE_OBJECT_OK)
            .or(RowFlags::CLASS_METHOD),
        named: &[],
    },
    // `Supply.start` is an instance method: a supply of the block's results.
    MethodRow {
        owner: "Supply",
        name: "start",
        arity: 1,
        handler: Handler::Interp(supply_start),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::DEFINITE),
        named: &[],
    },
];

/// The call's positional arguments followed by its named ones, the one list the
/// implementations read.
// Cost: O(a), a = arguments.
fn all_args(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    all
}

/// Rakudo's refusal of a class method (`method m(Supply:U: ...)`) called on
/// an instance; `None` for a type object.
// Cost: O(1).
pub(super) fn type_object_only(target: &Value, method: &str) -> Option<RuntimeError> {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    let class = class_name.resolve();
    Some(RuntimeError::parameter_invalid_concreteness(
        &class, &class, method, "self", false, true,
    ))
}

/// `IO::Special.new($what)`: an `IO::Special` standing for the stream `$what`
/// names (`<STDOUT>`).
// Cost: O(w), w = chars of the name.
fn io_special_new(_target: &Value, args: &[Value]) -> Answer {
    let [what] = args else {
        return None;
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("what".to_string(), Value::str(what.to_string_value()));
    Some(Ok(Value::make_instance(
        crate::symbol::Symbol::intern("IO::Special"),
        attrs,
    )))
}

/// `Supply.from-list(*@values)`.
// Cost: O(n), n = values (a lazy source is pulled).
fn supply_from_list(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    if let Some(e) = type_object_only(t, "from-list") {
        return Some(Err(e));
    }
    Some(i.dispatch_supply_from_list(&all_args(args, named)))
}

/// `Supply.interval($seconds, $delay?, :$scheduler)` (§9.196).
// Cost: see `Interpreter::dispatch_supply_interval`.
fn supply_interval(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    if let Some(e) = type_object_only(t, "interval") {
        return Some(Err(e));
    }
    Some(i.dispatch_supply_interval(&all_args(args, named)))
}

/// `Supply.on-demand(&producer, ...)`: the producer must be callable.
// Cost: O(1).
fn supply_on_demand(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    if let Some(e) = type_object_only(t, "on-demand") {
        return Some(Err(e));
    }
    if let Err(e) = i.bind_callable_param("&producer", &args[0]) {
        return Some(Err(e));
    }
    Some(i.dispatch_supply_on_demand(&all_args(args, named)))
}

/// `CompUnit::RepositoryRegistry.repository-for-name($name)`.
// Cost: as the repository's constructor.
fn repository_for_name(i: &mut Interpreter, _t: &Value, args: &[Value], _n: Named<'_>) -> Answer {
    Some(i.repo_registry_for_name(&args[0].to_string_value()))
}

/// `CompUnit::RepositoryRegistry.repository-for-spec($spec)`.
// Cost: as the repository's constructor.
fn repository_for_spec(i: &mut Interpreter, _t: &Value, args: &[Value], _n: Named<'_>) -> Answer {
    Some(i.repo_registry_for_spec(&args[0].to_string_value()))
}

/// `CompUnit::RepositoryRegistry.run-script($name)`: runs the named script of
/// an installed distribution.
// Cost: O(r), r = repositories searched, plus the script's run.
fn run_script(i: &mut Interpreter, _t: &Value, args: &[Value], _n: Named<'_>) -> Answer {
    Some(i.run_script_from_repos(&args[0].to_string_value()))
}

/// `Encoding::Registry.find($name)`.
// Cost: O(e), e = registered encodings.
fn encoding_find(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_encoding_registry_find(&all_args(args, named)))
}

/// `Encoding::Registry.register($encoding)`.
// Cost: O(n), n = the encoding's names.
fn encoding_register(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_encoding_registry_register(&all_args(args, named)))
}

/// `IO::Socket::INET.connect($host, $port, ...)`.
// Cost: one connect.
fn inet_connect(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_socket_inet_connect(&all_args(args, named)))
}

/// `IO::Socket::INET.listen($host, $port, ...)`.
// Cost: one bind.
fn inet_listen(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_socket_inet_listen(&all_args(args, named)))
}

/// `IO::Socket::Async.connect($host, $port, ...)`: a promise of the socket.
// Cost: O(1) to start the connect.
fn async_connect(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_socket_async_connect(&all_args(args, named)))
}

/// `IO::Socket::Async.listen($host, $port, ...)`: a supply of connections.
// Cost: O(1).
fn async_listen(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_socket_async_listen(&all_args(args, named)))
}

/// `IO::Socket::Async.bind-udp($host, $port, ...)`.
// Cost: one bind.
fn async_bind_udp(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_socket_async_bind_udp(&all_args(args, named)))
}

/// `IO::Socket::Async.udp(...)`: an unbound UDP socket.
// Cost: one socket.
fn async_udp(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_socket_async_udp(&all_args(args, named)))
}

/// `Seq.from-loop(&body, &cond?, :$repeat, :$label)`.
// Cost: O(1) to build the lazy sequence.
fn seq_from_loop(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_seq_from_loop(all_args(args, named)))
}

/// `X::AdHoc.from-slurpy(*@parts)`: an `X::AdHoc` whose payload is the parts.
// Cost: O(n), n = parts.
fn adhoc_from_slurpy(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_adhoc_from_slurpy(&all_args(args, named)))
}

/// `Thread.start(&code, :$name, :$app_lifetime)`: a started thread.
// Cost: one OS thread spawn.
fn thread_start(i: &mut Interpreter, _t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_thread_start(&all_args(args, named)))
}

/// `Thread.yield`: lets the scheduler run another OS thread.
// Cost: O(1), one scheduler yield for the calling OS thread.
fn thread_yield(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    // `method yield(Thread:U:)`.
    if let Some(e) = type_object_only(t, "yield") {
        return Some(Err(e));
    }
    crate::runtime::thread_usage::note_thread_yield();
    std::thread::yield_now();
    Some(Ok(Value::NIL))
}

/// `Supply.start(&code)`: a supply of supplies, each running the block on one
/// value.
// Cost: O(1) to build the derived supply.
fn supply_start(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_supply_transform(t.clone(), "start", &all_args(args, named)))
}

/// `Rakudo::Internals.REGISTER-DYNAMIC` (ADR-11276 §9.159).
// Cost: the block's own.
fn register_dynamic(
    interp: &mut Interpreter,
    _: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    Some(interp.register_dynamic(&all_args(args, named)))
}

/// `Deprecation.report`.
// Cost: O(r), r = chars of the report.
fn deprecation_report(_: &mut Interpreter, _: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Interpreter::deprecation_report()))
}

/// `CompUnit::Loader.load-source($buf)`.
// Cost: the source's compile and run.
fn load_source(interp: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(interp.compunit_load_source(args))
}

/// `CompUnit::Loader.load-source-file($path)`.
// Cost: the file's read, compile and run.
fn load_source_file(interp: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(interp.compunit_load_source_file(args))
}
