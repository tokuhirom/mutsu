//! Class methods of built-in classes, called on the type object (ADR-11276
//! §9.122): `Supply`'s `from-list` and `on-demand`, `CompUnit::RepositoryRegistry`'s
//! `repository-for-name`, `repository-for-spec` and `run-script`,
//! `Encoding::Registry`'s `find` and `register`, `IO::Socket::INET`'s `connect`
//! and `listen`, and `IO::Socket::Async`'s `connect`, `listen`, `bind-udp` and
//! `udp`.
//!
//! The rows are reached through their owner (`RowFlags::OWNER_ONLY`,
//! `invoke_owner`) from the by-name dispatch that answered them before
//! (`dispatch_method_by_name_3`), for the type object of the owner itself. The
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
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY
                .or(RowFlags::TYPE_OBJECT_OK)
                .or(RowFlags::SLURPY),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Supply", "from-list", 0, supply_from_list),
    row!("Supply", "on-demand", 1, supply_on_demand),
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
        repository_for_spec
    ),
    row!("CompUnit::RepositoryRegistry", "run-script", 1, run_script),
    row!("Encoding::Registry", "find", 1, encoding_find),
    row!("Encoding::Registry", "register", 1, encoding_register),
    row!("IO::Socket::INET", "connect", 0, inet_connect),
    row!("IO::Socket::INET", "listen", 0, inet_listen),
    row!("IO::Socket::Async", "connect", 0, async_connect),
    row!("IO::Socket::Async", "listen", 0, async_listen),
    row!("IO::Socket::Async", "bind-udp", 0, async_bind_udp),
    row!("IO::Socket::Async", "udp", 0, async_udp),
];

/// The owner whose class methods `target` answers: the type object of one of
/// the classes above.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    let name = match target.view() {
        ValueView::Package(name) => name,
        // `Supply`'s class methods refuse an instance with Rakudo's message.
        ValueView::Instance { class_name, .. } if class_name == "Supply" => {
            return Some(&["Supply"]);
        }
        _ => return None,
    };
    Some(match name.as_str() {
        "Supply" => &["Supply"],
        "CompUnit::RepositoryRegistry" => &["CompUnit::RepositoryRegistry"],
        "Encoding::Registry" => &["Encoding::Registry"],
        "IO::Socket::INET" => &["IO::Socket::INET"],
        "IO::Socket::Async" => &["IO::Socket::Async"],
        _ => return None,
    })
}

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
fn type_object_only(target: &Value, method: &str) -> Option<RuntimeError> {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    let class = class_name.resolve();
    Some(RuntimeError::new(format!(
        "Invocant of method '{method}' must be a type object of type '{class}', not an object \
         instance of type '{class}'. Did you forget a 'multi'?"
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
