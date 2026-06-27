use super::*;

impl Interpreter {
    /// Save current function/class keys for lexical import scoping.
    pub(crate) fn push_import_scope(&mut self) {
        let func_keys: HashSet<Symbol> = self.registry().functions.keys().copied().collect();
        let class_keys: HashSet<String> = self.registry().classes.keys().cloned().collect();
        self.import_scope_stack.push((
            func_keys,
            class_keys,
            self.newline_mode,
            self.strict_mode,
            self.fatal_mode,
            self.monkey_typing,
        ));
    }

    /// Restore function/class registries to the last saved snapshot,
    /// removing any entries added since the push.
    pub(crate) fn pop_import_scope(&mut self) {
        if let Some((
            func_snapshot,
            class_snapshot,
            newline_mode,
            strict_mode,
            fatal_mode,
            monkey_typing,
        )) = self.import_scope_stack.pop()
        {
            self.registry_mut()
                .functions
                .retain(|key, _| func_snapshot.contains(key));
            self.registry_mut()
                .classes
                .retain(|key, _| class_snapshot.contains(key));
            self.newline_mode = newline_mode;
            self.strict_mode = strict_mode;
            self.fatal_mode = fatal_mode;
            self.monkey_typing = monkey_typing;
            // Removing imported functions when a lexical import scope pops must
            // invalidate the name-keyed function-resolution caches: a sub that
            // was OTF-compiled and cached under its bare name while in scope
            // (otf_call_cache) would otherwise still be reachable after the
            // scope exits — e.g. `{ use Foo } EVAL('foo()')` must die, not hit
            // the stale cache (roast/S11-modules/lexical.t). Registration bumps
            // fn_resolve_gen; the matching un-registration here must too.
            self.fn_resolve_gen += 1;
        }
    }

    pub fn use_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        self.use_module_with_tags(module, &[])
    }

    pub fn use_module_with_tags(
        &mut self,
        module: &str,
        tags: &[String],
    ) -> Result<(), RuntimeError> {
        if self.loaded_modules.contains(module) {
            if module == "strict" {
                self.strict_mode = true;
            } else if module == "fatal" {
                self.fatal_mode = true;
            }
            // Propagate package declarations from the already-loaded module
            // into the current chain so that chain_has_package_decl checks
            // correctly detect namespace contributions from transitive deps.
            if let Some(pkgs) = self.module_packages.get(module).cloned() {
                self.chain_declared_packages.extend(pkgs);
            }
            // When a module that declares a class/role matching its own name
            // is directly `use`d at the top level, un-hide it and its related
            // classes from the package stash. This handles the case where the
            // module was first loaded transitively by a non-contributing module.
            if self.module_load_stack.is_empty() && module.contains("::") {
                let is_contributor = {
                    let registry = self.registry();
                    registry.classes.contains_key(module) || registry.roles.contains_key(module)
                };
                if is_contributor {
                    self.package_stash_hidden.remove(module);
                }
            }
            return match self.import_module(module, tags) {
                Ok(()) => Ok(()),
                Err(err) if err.message.starts_with("No exports found for module:") => Ok(()),
                Err(err) => Err(err),
            };
        }
        if self.module_load_stack.iter().any(|m| m == module) {
            let mut chain = self.module_load_stack.clone();
            chain.push(module.to_string());
            return Err(RuntimeError::new(format!(
                "circular module dependency detected: {}",
                chain.join(" -> ")
            )));
        }
        // At the top level (no modules currently loading), save and clear
        // the chain-scoped package declarations so each top-level `use` gets
        // a fresh chain. Nested uses inherit the parent's chain.
        let is_top_level_use = self.module_load_stack.is_empty();
        let saved_chain_pkgs = if is_top_level_use {
            std::mem::take(&mut self.chain_declared_packages)
        } else {
            HashSet::new()
        };
        self.module_load_stack.push(module.to_string());
        let class_snapshot: HashSet<String> = self.registry().classes.keys().cloned().collect();
        let role_snapshot: HashSet<String> = self.registry().roles.keys().cloned().collect();
        let env_snapshot: HashSet<Symbol> = self.env.keys().copied().collect();
        let func_keys_before: HashSet<Symbol> = self.registry().functions.keys().copied().collect();

        let result = if module == "Test"
            || matches!(
                module,
                "strict"
                    | "warnings"
                    | "MONKEY-SEE-NO-EVAL"
                    | "MONKEY-TYPING"
                    | "nqp"
                    | "MONKEY"
                    | "newline"
                    | "soft"
                    | "fatal"
                    | "oo"
                    | "class"
                    // NativeCall: the `is native(...)` trait machinery is built
                    // into the VM (see runtime/nativecall.rs); `use NativeCall`
                    // only needs to be a recognized no-op.
                    | "NativeCall"
                    // JSON::Fast / JSON::Tiny: the real distributions depend on
                    // ~50 nqp ops mutsu does not implement. Recognize them as
                    // built-in modules and provide native `to-json`/`from-json`
                    // (see runtime/json.rs, dispatched in vm_native_json.rs).
                    | "JSON::Fast"
                    | "JSON::Tiny"
            ) {
            // Track MONKEY-TYPING pragma
            if module == "MONKEY-TYPING" || module == "MONKEY" {
                self.monkey_typing = true;
            }
            Ok(())
        } else if module == "Test::Tap" {
            // Handle Test::Tap as built-in
            Ok(())
        } else if module.starts_with("Test::") {
            // Load Test:: submodules from source as regular modules.
            // Parse errors should propagate like other `use` failures.
            // Missing helper modules remain non-fatal for compatibility.
            match self.load_module(module) {
                Ok(()) => Ok(()),
                Err(err) if err.is_unsatisfied_dependency() => Ok(()),
                Err(err) => Err(err),
            }
        } else {
            self.load_module(module)
        };

        self.module_load_stack.pop();
        if result.is_ok() {
            let module_short = if let Some((_, short)) = module.rsplit_once("::") {
                short
            } else {
                module
            };
            let class_names: Vec<String> = self.registry().classes.keys().cloned().collect();
            for class_name in &class_names {
                if class_snapshot.contains(class_name) {
                    continue;
                }
                let class_short = class_name
                    .rsplit_once("::")
                    .map(|(_, short)| short)
                    .unwrap_or(class_name.as_str());
                if class_short != module_short {
                    self.need_hidden_classes.insert(class_name.clone());
                    self.need_hidden_classes.insert(class_short.to_string());
                }
            }
            for key in self.env.keys() {
                if env_snapshot.contains(key) {
                    continue;
                }
                if key.starts_with("$")
                    || key.starts_with("@")
                    || key.starts_with("%")
                    || key.starts_with("&")
                {
                    continue;
                }
                let key_s = key.resolve();
                let key_short = key_s
                    .rsplit_once("::")
                    .map(|(_, short)| short)
                    .unwrap_or(key_s.as_str());
                if !key_short
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
                {
                    continue;
                }
                if key_short != module_short {
                    self.need_hidden_classes.insert(key_s.clone());
                    self.need_hidden_classes.insert(key_short.to_string());
                }
            }
            // Determine if new classes/roles from this module should be
            // hidden from the parent namespace's package stash. This prevents
            // transitive dependencies from leaking into namespace stash lookups
            // (e.g. `Example2::.keys` should not show classes loaded only as
            // transitive deps of a module that doesn't contribute to the namespace).
            //
            // A module "contributes" to namespace X if either:
            //   (a) it registered a class/role matching its own FQ name (e.g.
            //       `use Example2::F` and `class Example2::F` was registered), or
            //   (b) its dependency chain includes a `package X {}` declaration
            //       (tracked in `chain_declared_packages` which is scoped to this load).
            // If neither condition holds, new classes/roles are hidden from X's stash.
            if let Some((namespace, _)) = module.rsplit_once("::") {
                let module_declares_own_class = {
                    let registry = self.registry();
                    registry.classes.contains_key(module) || registry.roles.contains_key(module)
                };
                let chain_has_package_decl = self.chain_declared_packages.contains(namespace);
                if !module_declares_own_class && !chain_has_package_decl {
                    // Hide newly registered classes/roles from the namespace stash
                    let class_names: Vec<String> =
                        self.registry().classes.keys().cloned().collect();
                    for class_name in &class_names {
                        if !class_snapshot.contains(class_name)
                            && class_name.starts_with(namespace)
                            && class_name.get(namespace.len()..namespace.len() + 2) == Some("::")
                        {
                            self.package_stash_hidden.insert(class_name.clone());
                        }
                    }
                    let role_names: Vec<String> = self.registry().roles.keys().cloned().collect();
                    for role_name in &role_names {
                        if !role_snapshot.contains(role_name)
                            && role_name.starts_with(namespace)
                            && role_name.get(namespace.len()..namespace.len() + 2) == Some("::")
                        {
                            self.package_stash_hidden.insert(role_name.clone());
                        }
                    }
                }
            }
            // Record which packages were declared during this module's chain
            // so they can be propagated when the module is re-used.
            if !self.chain_declared_packages.is_empty() {
                self.module_packages
                    .insert(module.to_string(), self.chain_declared_packages.clone());
            }
            // Restore the chain-scoped package declarations (top-level only)
            if is_top_level_use {
                self.chain_declared_packages = saved_chain_pkgs;
            }

            if module == "strict" {
                self.strict_mode = true;
            } else if module == "fatal" {
                self.fatal_mode = true;
            }
            // Remove GLOBAL:: function aliases for non-DEFAULT/non-MANDATORY
            // exports that were created by sub hoisting during module loading.
            // The hoisting registers ALL exported subs under GLOBAL:: before
            // export tag filtering can occur. We use the exported_subs table
            // to identify which functions should NOT be globally accessible.
            let requested_tags: HashSet<String> = if tags.is_empty() {
                ["DEFAULT".to_string()].into_iter().collect()
            } else {
                tags.iter().cloned().collect()
            };
            let want_all = requested_tags.contains("ALL");
            // Check exports registered under GLOBAL (for unit modules) and under module name
            let export_sources = ["GLOBAL", module];
            for source in &export_sources {
                if let Some(subs) = self.exported_subs.get(*source) {
                    for (name, symbol_tags) in subs {
                        let is_mandatory = symbol_tags.contains("MANDATORY");
                        if !want_all && !is_mandatory && symbol_tags.is_disjoint(&requested_tags) {
                            // This export should NOT be imported — remove from GLOBAL
                            let global_key = Symbol::intern(&format!("GLOBAL::{}", name));
                            if !func_keys_before.contains(&global_key) {
                                self.registry_mut().functions.remove(&global_key);
                            }
                            // Also remove multi-dispatch variants
                            let prefix = format!("GLOBAL::{}/", name);
                            let multi_keys: Vec<Symbol> = self
                                .registry()
                                .functions
                                .keys()
                                .filter(|k| {
                                    let ks = k.resolve();
                                    ks.starts_with(&prefix) && !func_keys_before.contains(k)
                                })
                                .copied()
                                .collect();
                            for mk in multi_keys {
                                self.registry_mut().functions.remove(&mk);
                            }
                        }
                    }
                }
            }

            // Remove GLOBAL:: operator sub entries (infix/prefix/postfix/circumfix)
            // that were added by sub hoisting during module loading but are NOT
            // exported. This prevents non-exported operators from leaking into
            // the caller's namespace while preserving regular function hoisting.
            let mut exported_op_names: HashSet<String> = HashSet::new();
            for source in ["GLOBAL", module] {
                if let Some(subs) = self.exported_subs.get(source) {
                    for name in subs.keys() {
                        if name.contains(":<") {
                            exported_op_names.insert(name.clone());
                        }
                    }
                }
            }
            if let Some(subs) = self.unit_module_exported_subs.get(module) {
                for name in subs.keys() {
                    if name.contains(":<") {
                        exported_op_names.insert(name.clone());
                    }
                }
            }
            let non_exported_op_globals: Vec<Symbol> = self
                .registry()
                .functions
                .keys()
                .filter(|k| {
                    if func_keys_before.contains(k) {
                        return false;
                    }
                    let ks = k.resolve();
                    if let Some(name) = ks.strip_prefix("GLOBAL::") {
                        let base = name.split('/').next().unwrap_or(name);
                        // Only remove operator subs (infix:<...>, prefix:<...>, etc.)
                        base.contains(":<") && !exported_op_names.contains(base)
                    } else {
                        false
                    }
                })
                .copied()
                .collect();
            for k in non_exported_op_globals {
                self.registry_mut().functions.remove(&k);
            }

            // Remove GLOBAL:: sub aliases that were leaked by sub hoisting during
            // this module load, are NOT exported, and shadow a core builtin.
            // A `unit module X` declares `our sub foo` under `X::foo`, but mutsu's
            // hoist pre-pass registers the body under `GLOBAL::foo` (the runtime
            // package is not switched by the compile-time `unit` declaration). For
            // a non-exported sub whose name matches a core builtin, that GLOBAL
            // alias wrongly masks the builtin in the caller's scope (e.g.
            // Test::Util's non-exported `our sub run` was hiding the core `run`
            // Proc spawner). Such a sub is reachable from the caller only as a bare
            // name — which Raku does not allow for a non-exported `our sub` — so we
            // drop the leaked alias. Gating on builtin collision keeps the change
            // safe: non-colliding helpers stay in GLOBAL so the module's own
            // (GLOBAL-package) bodies can still call them.
            {
                let mut exported_names: HashSet<String> = HashSet::new();
                for source in ["GLOBAL", module] {
                    if let Some(subs) = self.exported_subs.get(source) {
                        for name in subs.keys() {
                            exported_names.insert(name.clone());
                        }
                    }
                }
                if let Some(subs) = self.unit_module_exported_subs.get(module) {
                    for name in subs.keys() {
                        exported_names.insert(name.clone());
                    }
                }
                let leaked_globals: Vec<Symbol> = self
                    .registry()
                    .functions
                    .keys()
                    .filter(|k| {
                        if func_keys_before.contains(k) {
                            return false;
                        }
                        let ks = k.resolve();
                        let Some(name) = ks.strip_prefix("GLOBAL::") else {
                            return false;
                        };
                        let base = name.split('/').next().unwrap_or(name);
                        !exported_names.contains(base) && Self::is_builtin_function(base)
                    })
                    .copied()
                    .collect();
                for k in leaked_globals {
                    self.registry_mut().functions.remove(&k);
                }
            }

            self.loaded_modules.insert(module.to_string());
            if let Err(err) = self.import_module(module, tags)
                && !err.message.starts_with("No exports found for module:")
            {
                return Err(err);
            }
        }
        result
    }
}
