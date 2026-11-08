//! The core `Awaitable::Handle` role, as real Raku source (ADR-15519).
//!
//! Rakudo's `await` protocol: an `Awaitable` hands out a handle from
//! `get-await-handle`, and a class composing the role carries the settled
//! state (`already`, `success`, `result`, `cause`), builds an already-settled
//! handle through two factories and must implement `subscribe-awaiter`. A
//! composing class that omits `subscribe-awaiter` fails to compile, as in
//! rakudo. This slice registers the role itself; making `await` consume it is
//! the ADR's second unit.

use super::source_code_text::CodeText;
use super::*;

/// Measured on rakudo 2026.09: `Awaitable::Handle.^attributes` is
/// `$!already $!success $!result $!cause`, `^methods(:local)` is
/// `already-success already-failure subscribe-awaiter`, and the factories
/// build an instance of the invocant's class (`already-failure` leaves the
/// result an untouched `Mu`).
const AWAITABLE_HANDLE_ROLE_PRELUDE: &str = r#"
role GLOBAL::Awaitable::Handle {
    has Bool $.already;
    has Bool $.success;
    has Mu $.result;
    has Exception $.cause;
    method already-success(::?CLASS:U: Mu \result) {
        self.new(:already, :success, :result(result))
    }
    method already-failure(::?CLASS:U: Mu \cause) {
        self.new(:already, :!success, :cause(cause))
    }
    method subscribe-awaiter(&subscriber) {...}
}
# `await` turns a handle into a Promise and waits on that, so a settled
# handle, a pending one and a failed one all go through the one Promise
# settlement and `X::Await::Died` path.
module MUTSU_AWAIT {
    our sub handle-promise(Mu \handle) {
        my $promise = Promise.new;
        my $vow = $promise.vow;
        if handle.already {
            handle.success ?? $vow.keep(handle.result) !! $vow.break(handle.cause);
        }
        else {
            handle.subscribe-awaiter(-> $success, $result {
                $success ?? $vow.keep($result) !! $vow.break($result);
            });
        }
        $promise
    }
}
"#;

impl Interpreter {
    /// Prepend [`AWAITABLE_HANDLE_ROLE_PRELUDE`] to a unit that mentions
    /// `Awaitable::Handle`, unless it declares its own. Injected for modules
    /// too: the unit that composes the role is usually a module the program
    /// only loads.
    // Cost: O(n), n = source bytes (one substring search when absent).
    pub(super) fn inject_awaitable_handle_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("Awaitable::Handle") || source.contains("role Awaitable::Handle") {
            return;
        }
        use std::sync::OnceLock;
        static HANDLE_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = HANDLE_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(AWAITABLE_HANDLE_ROLE_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }
}
