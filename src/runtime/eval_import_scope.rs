//! The import scope an `EVAL` opens around its compunit (#11069), and the
//! `env` half of every import scope's rollback.
//!
//! What a `use` imports is lexical to the unit that holds it. A `use`-holding
//! block rolls back the routine/class registries and the `env` aliases with
//! `push_import_scope`/`pop_import_scope`. An `EVAL` already rolls back its
//! registries itself (`eval_eval_string_unit`'s snapshots, which know to keep
//! an EVAL's own `our sub` and the classes it declares), so it needs only the
//! `env` half: the aliases `import_module` wrote (a `sub EXPORT` hook's
//! terms, an exported class name) must not outlive the EVAL.

use super::*;

impl Interpreter {
    /// Run `body` (an `EVAL`'s compunit) with its imported `env` aliases
    /// dropped again afterwards, on every exit path, along with the alias
    /// bookkeeping (`imported_env_aliases`) they were recorded in.
    // Cost: O(F + C + k) plus the body, F/C = registered routines/classes (the
    // import-scope snapshot), k = aliases the body imported.
    pub(crate) fn with_eval_import_scope<T>(
        &mut self,
        body: impl FnOnce(&mut Self) -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        self.push_import_scope();
        let result = body(self);
        if let Some(snapshot) = self.import_scope_stack.pop() {
            let ImportScopeSnapshot {
                imported_env_keys,
                mut shadowed_env_values,
                imported_env_aliases,
                ..
            } = snapshot;
            self.restore_import_env_keys(imported_env_keys, &mut shadowed_env_values);
            self.imported_env_aliases = imported_env_aliases;
        }
        result
    }

    /// Drop the `env` aliases an import scope recorded. `import_module` writes
    /// an imported symbol's aliased name straight into `env` (a
    /// bare `&ok`/`$CONST`, or the `GLOBAL::name`-qualified form under
    /// the importing package), alongside the registry entry
    /// `pop_import_scope` rolls back. Remove exactly the keys `record_import_env_key` recorded
    /// for THIS scope — never a before/after diff of the whole `env`.
    /// `env` also carries ordinary statement-level state with nothing
    /// to do with imports (`$!`, `$_`, a plain `my` local, a `package`
    /// type object, ...), and a block is not required to run through
    /// the general `BlockScope` restore that scopes those (a
    /// `use`-containing block takes this lighter path instead, purely
    /// so the registries above can be scoped) — so diffing dropped
    /// any of them that happened to be written for the first time
    /// inside a `use`-containing block. That silently erased `$!`
    /// itself the first time any block anywhere in the process wrote
    /// it while a `use` was in scope, breaking `$!.backtrace`'s
    /// identity across a later, unrelated block
    /// (`roast/integration/error-reporting.t` "Backtrace does not
    /// change on additional .backtrace").
    ///
    /// Same keep-rule as the registry for the keys we DO track: a
    /// module's own package-qualified entry (`Foo::name`, not
    /// `GLOBAL::name`) persists, because a sibling block's later
    /// `use` re-imports by reading that qualified env value (see the
    /// `vars` loop in `import_module`) — though in practice
    /// `import_module` never records one of those (it writes the
    /// module's own qualified form separately, at module-load time,
    /// never through `record_import_env_key`); the check is kept for
    /// symmetry with `pop_import_scope`'s registry-side rule and to cover the
    /// trait-value path's `&{importing_pkg}::{name}` write. A sigil
    /// (`$@%&`) may prefix the qualifier, so strip it before checking.
    ///
    /// A PRELOAD scope (`scope_classes == false`, see
    /// `push_preload_scope`) never removes these bare aliases at all,
    /// for the same reason it keeps the classes/qualified defs a
    /// preload registers (see that function's doc comment): a custom
    /// `sub EXPORT`'s installed symbol (e.g. JSON::Fast's `&to-json`)
    /// lives ONLY in `env` -- unlike a tag-based `is export` routine,
    /// it has no registry entry to fall back on -- and a `sub`
    /// hoisted to the head of the SAME package block (`RegisterDecl`,
    /// emitted before the block's own in-position `use` runs) needs it
    /// visible right away. Removing it here and relying on the
    /// in-position `use`'s later re-install left that hoisted sub
    /// permanently unable to resolve the symbol (#8564): the preload
    /// and the hoisted registration both run before the in-position
    /// `use`, so the bare alias must already be live by then. A real
    /// scope-exit removal still happens for a genuine user block
    /// (`{ use JSON::Fast; ... }` brackets the in-position `use`
    /// with its own ordinary `ImportScope` region, which is not a
    /// preload scope).
    // Cost: O(k), k = keys the scope imported.
    pub(crate) fn restore_import_env_keys(
        &mut self,
        imported_env_keys: HashSet<Symbol>,
        shadowed_env_values: &mut HashMap<Symbol, Value>,
    ) {
        for key in imported_env_keys {
            let ks = key.resolve();
            let unqualified = ks.strip_prefix(['$', '@', '%', '&']).unwrap_or(ks.as_str());
            let is_module_owned_qualified =
                unqualified.contains("::") && !unqualified.starts_with("GLOBAL::");
            if is_module_owned_qualified {
                continue;
            }
            match shadowed_env_values.remove(&key) {
                Some(previous) => {
                    self.env.insert_sym(key, previous);
                }
                None => {
                    self.env.remove_sym(key);
                }
            }
        }
    }
}
