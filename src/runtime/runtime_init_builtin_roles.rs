//! The built-in roles of the class registry, built once per process by
//! `build_builtin_registry` (PR-A slice 4: roles live in the shared
//! `Registry`, not an `Interpreter` field).
//!
//! Every one is empty-bodied except two interface roles whose required methods
//! are stubs (`CompUnit::Repository`, `Distribution`). They are inserted
//! straight into the registry's sharded table: there is no staging map to
//! convert.

use std::collections::HashMap;

use super::registry::RoleTable;
use super::registry_sharded_map::ShardedMap;
use super::{MethodDef, RoleDef};
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;

/// The empty-bodied built-in roles.
const EMPTY_ROLES: &[&str] = &[
    "Encoding",
    // Rakudo exposes the streaming encoding interfaces as roles. The native
    // dispatch table is still registered as a class-shaped entry, but user
    // implementations must be able to compose these role names (for example
    // Encoding::Emacs::Registry's SingleByteDecoder/SingleByteEncoder).
    "Encoding::Decoder",
    "Encoding::Encoder",
    "Iterator",
    "PredictiveIterator",
    "Iterable",
    "X::Control",
    // ADR-0029: role-shaped `X::` exception "namespaces". Measured against
    // real rakudo (2026-08-17), 59% of the `X::` classes mutsu raises or tests
    // against compose one or more of these marker roles rather than
    // inheriting from a same-named class -- `X::Comp`, `X::Syntax`, `X::IO`,
    // and `X::OS` are the heavily-used ones (135/69/22/22 classes
    // respectively), the rest are 1-7 each. All are empty-bodied here, as they
    // are in rakudo too: mutsu's exception machinery supplies the behaviour,
    // these exist purely so `.^roles`, `.^does`, and `~~` agree with rakudo
    // about which classes compose them. Registering them as roles (not
    // classes) also satisfies `type_matching.rs`'s `resolve_role_key` gate for
    // type-object `~~` (e.g. `X::Comp::FailGoal ~~ X::Comp`).
    // `X::Nominalizable` / `X::Role::Attribute` (Slice 3) were not in the
    // ADR's original 14 -- its corpus was a 303-name roast/t sample; Slice 3's
    // broader capture (roast/t plus every `X::...` string literal in mutsu's
    // own source) surfaced these two additional roles the same way.
    // `X::Wrapper` used to be listed here too, but it is NOT actually empty in
    // rakudo (unlike the rest of this list) -- it is a real, stateful role
    // (`has $!exception`, `!wrappee-message`, ...) that wraps a lower-level
    // exception, and a composing class calling its private methods died with
    // `X::Method::NotFound` against the empty stand-in (#8573). It now gets
    // the full-behaviour prelude treatment instead, the same way
    // `Enumeration`/`Rational`/`IO::Socket` do -- see `X_WRAPPER_ROLE_PRELUDE`
    // in `run.rs`.
    "X::Comp",
    "X::Syntax",
    "X::IO",
    "X::OS",
    "X::Trait",
    "X::Proc::Async",
    "X::BadType",
    "X::Temporal",
    "X::MOP",
    "X::Encoding",
    "X::Pod",
    "X::RoleApplier",
    "X::RoleApplier::Method",
    "X::Nominalizable",
    "X::Role::Attribute",
    // Not a marker role like the rest: rakudo mixes `X::Promise::Broken` into
    // the *cause* exception that `Promise.result` rethrows, producing the
    // anonymous mixin type `X::AdHoc+{X::Promise::Broken}`. It is registered
    // here so `eval_does_values` can compose it and `~~ X::Promise::Broken`
    // answers correctly.
    "X::Promise::Broken",
    // The same shape for a `react` block that dies: rakudo rethrows the
    // exception `but X::React::Died` (its `gist` lives in
    // `wrapper_role_gist`).
    "X::React::Died",
    // And for `await` of a broken promise: rakudo rethrows the cause
    // `but X::Await::Died` (gist in `wrapper_role_gist`).
    "X::Await::Died",
    // Also not a marker role: rakudo's `X::AdHoc.from-slurpy(...)` mixes this
    // into the `Capture` it stores as `.payload`, so the payload's `.^name` is
    // `Capture+{X::AdHoc::SlurpySentry}` and `.Str` knows to concatenate
    // rather than render the capture. Registered here so the composition can
    // happen at all.
    "X::AdHoc::SlurpySentry",
];

/// The interface roles with required methods, each stubbed to die when a
/// composing class leaves it unimplemented.
const STUBBED_ROLES: &[(&str, &[&str])] = &[
    // Rakudo's CompUnit::Repository role requires exactly `id`, `need`, and
    // `loaded` (a class doing the role must implement those three). `load` is
    // NOT a required method.
    ("CompUnit::Repository", &["id", "need", "loaded"]),
    // Real Rakudo defines `Distribution` with required stub methods `meta`
    // and `content`; user distribution classes (e.g. `Zef::Distribution does
    // Distribution`) supply the implementations. Registering the role lets
    // such classes compose and lets `~~ Distribution` recognize them.
    ("Distribution", &["meta", "content"]),
];

/// An empty-bodied role with `methods`.
// Cost: O(1), plus the move of `methods`.
fn role(methods: HashMap<String, Vec<MethodDef>>) -> RoleDef {
    RoleDef {
        attributes: Vec::new(),
        attribute_built: HashMap::new(),
        methods,
        is_stub_role: false,
        is_hidden: false,
        captured_env: None,
        wildcard_handles: Vec::new(),
        role_id: 0,
        attribute_conflicts: Vec::new(),
        own_attribute_names: std::collections::HashSet::new(),
        deferred_body: Vec::new(),
        body_bind_cells: Vec::new(),
        decl_file: None,
        deferred_custom_traits: Vec::new(),
        pending_param_type_checks: Vec::new(),
    }
}

/// A required-method stub: its body dies through `__mutsu_stub_die`.
// Cost: O(1).
fn stub_method() -> MethodDef {
    let body = vec![Stmt::Expr(Expr::Call {
        name: Symbol::intern("__mutsu_stub_die"),
        args: vec![],
        listop: false,
    })];
    MethodDef {
        value_prototype: None,
        syms: Default::default(),
        lexical_package: crate::symbol::wk::global_package(),
        params: (Vec::new()).into(),
        param_defs: (Vec::new()).into(),
        body: std::sync::Arc::new(body),
        is_rw: false,
        is_raw: false,
        is_private: false,
        is_multi: false,
        is_my: false,
        role_origin: None,
        original_role: None,
        return_type: None,
        compiled_code: None,
        compiled_fns: None,
        delegation: None,
        is_default: false,
        deprecated_message: None,
        is_submethod: false,
        is_hidden_from_backtrace: false,
        captured_env: None,
        source_file: None,
        role_param_bindings: None,
        nested_capture_index: None,
        captured_readonly: None,
        routine_cell: Default::default(),
        augment_epoch: 0,
    }
}

/// The built-in roles, as the registry's role table.
// Cost: O(R + M), R = built-in roles, M = their stub methods.
pub(super) fn builtin_roles() -> RoleTable {
    let mut roles = ShardedMap::default();
    roles.reserve(EMPTY_ROLES.len() + STUBBED_ROLES.len());
    for name in EMPTY_ROLES {
        roles.insert((*name).to_string(), role(HashMap::new()).into());
    }
    for (name, required) in STUBBED_ROLES {
        let methods = required
            .iter()
            .map(|method| ((*method).to_string(), vec![stub_method()]))
            .collect();
        roles.insert((*name).to_string(), role(methods).into());
    }
    roles.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_builtin_role_is_registered_once() {
        let roles = builtin_roles();
        assert_eq!(roles.len(), EMPTY_ROLES.len() + STUBBED_ROLES.len());
        for name in EMPTY_ROLES {
            assert!(roles.get(*name).is_some_and(|r| r.methods.is_empty()), "{name}");
        }
        let repo = roles.get("CompUnit::Repository").unwrap();
        let mut required: Vec<&str> = repo.methods.keys().map(String::as_str).collect();
        required.sort_unstable();
        assert_eq!(required, ["id", "loaded", "need"]);
    }
}
