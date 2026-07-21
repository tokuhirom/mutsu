//! Custom module export via `sub EXPORT`.
//!
//! A module may define `sub EXPORT(...)` which Raku calls with the `use`
//! arguments and whose return `Map` (or list of `Map`s) names the symbols to
//! install into the importing scope. mutsu calls it at module-load time (see
//! `load_module`), after the module body has run and its subs are registered.
use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// If the just-loaded module defined `sub EXPORT`, call it with the `use`
    /// arguments and install the symbols from its returned `Map`(s) into the
    /// caller's scope. `EXPORT` itself is special (never an export), so it is
    /// removed from the registry afterwards to avoid leaking as a callable.
    pub(super) fn apply_module_export(
        &mut self,
        export_args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        // The module body runs under GLOBAL, so `sub EXPORT` registers as
        // `GLOBAL::EXPORT`. Only participate when it is actually present.
        let Some(def) = self.resolve_function("EXPORT") else {
            return Ok(());
        };
        // Run EXPORT through the compiled call path (not the tree-walk
        // `call_function` slow path): its params become real local slots, so a
        // sub the EXPORT returns can capture a use-argument (`sub EXPORT($x)
        // { Map.new: '&f' => sub { ...$x... } }`).
        //
        // Snapshot env across the call and restore it afterwards: the call's
        // scalar return-merge writes EXPORT's own params/locals (`$x`, a `my $y`)
        // back into this (the caller's) env as their post-return values. A sub
        // EXPORT returns that closes over such a lexical carries the correct
        // captured value, but a later bareword call of it merges the caller env
        // with `merge_all` (keep-existing) semantics — so the leaked stale entry
        // would shadow the capture. Dropping EXPORT's env writes keeps the
        // caller env clean; EXPORT's real effects are its return value and
        // control flow (die/note/exit), not caller-env mutation.
        let empty_fns = crate::opcode::CompiledFns::default();
        let saved_env = self.env.clone();
        let result = self.compile_and_call_function_def(&def, export_args, &empty_fns)?;
        self.env = saved_env;
        // `EXPORT` must not itself become a callable in (or leak from) the
        // module; drop every registered `EXPORT` routine now that it has run.
        self.remove_export_routine();
        self.install_export_map(&result);
        Ok(())
    }

    /// Remove any `EXPORT` routine registered by the module body (it runs under
    /// GLOBAL, so the key is `GLOBAL::EXPORT`; be liberal in case a package
    /// prefix was used) so it does not leak into the caller as `EXPORT()`.
    fn remove_export_routine(&mut self) {
        self.registry_mut().functions.retain(|key, _| {
            let ks = key.resolve();
            ks != "EXPORT" && !ks.ends_with("::EXPORT")
        });
    }

    /// Install the symbols named by an `EXPORT` return value. Accepts a single
    /// `Map`/`Hash` (`'&name' => sub {...}`, `'$name' => value`) or a list of
    /// them (recursing), matching `sub EXPORT { Map.new: ... }` and the
    /// multi-tag `%(...)` form.
    fn install_export_map(&mut self, val: &Value) {
        match val.view() {
            ValueView::Hash(gc) => {
                let pairs: Vec<(String, Value)> =
                    gc.map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                for (key, value) in pairs {
                    self.install_export_symbol(key, value);
                }
            }
            ValueView::Array(items, ..) => {
                let items: Vec<Value> = items.iter().cloned().collect();
                for item in items {
                    self.install_export_map(&item);
                }
            }
            _ => {}
        }
    }

    /// Install one exported symbol under its sigilled name (`&greet`, `$foo`,
    /// `@bar`, `%baz`) into the current (caller's) scope. An exported operator
    /// sub is also registered with the parser so runtime-parsed code (EVAL)
    /// recognizes the new operator symbol.
    fn install_export_symbol(&mut self, key: String, value: Value) {
        let sigil = key.chars().next();
        if let Some('&') = sigil {
            let op = &key[1..];
            if matches!(
                op.split_once(":<").map(|(c, _)| c),
                Some("prefix" | "postfix" | "infix" | "circumfix" | "postcircumfix")
            ) {
                self.imported_operator_names.insert(op.to_string());
            }
            if op.starts_with("infix:<") {
                self.user_declared_infix_ops.insert(op.to_string());
                crate::vm::vm_jit::note_user_infix_decl();
            }
        }
        // Install into env under the key the reader looks up. A `$scalar` read
        // compiles to a bare (sigil-stripped) `GetGlobal` — the same key an
        // `our $x` module global lands under — so the sigil must be dropped for
        // scalars. Arrays/hashes/subs are read under their sigilled name.
        self.unsuppress_name(&key);
        let env_key = match sigil {
            Some('$') => key[1..].to_string(),
            _ => key,
        };
        self.env.insert(env_key, value);
        self.fn_resolve_gen += 1;
    }
}
