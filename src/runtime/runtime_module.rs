use super::*;

#[derive(Clone, Default)]
pub(crate) struct ModuleOwnedTypes {
    pub(crate) declared: HashSet<String>,
    pub(crate) exported: HashSet<String>,
}

impl Interpreter {
    /// Attribute a type declaration to the module whose body is currently
    /// running. Nested module loads push their own name, so the outer module
    /// does not accidentally claim a dependency's declarations.
    pub(crate) fn record_module_owned_type(&mut self, name: &str) {
        self.release_foreign_provenance(name);
        let Some(module) = self.module.module_load_stack.last().cloned() else {
            return;
        };
        crate::runtime::cow_table_mut(&mut self.module.module_visibility.module_declared_types)
            .insert(name.to_string());
        crate::runtime::cow_table_mut(&mut self.module.module_owned_types)
            .entry(module)
            .or_default()
            .declared
            .insert(name.to_string());
    }

    /// Remember which of a module's own type aliases may cross an import.
    pub(crate) fn record_module_exported_type_alias(&mut self, module: &str, name: &str) {
        let owned = crate::runtime::cow_table_mut(&mut self.module.module_owned_types)
            .entry(module.to_string())
            .or_default();
        if owned.declared.contains(name) {
            owned.exported.insert(name.to_string());
        }
    }

    pub(crate) fn module_load_in_progress(&self) -> bool {
        !self.module.module_load_stack.is_empty()
    }

    /// Whether `use <module>` has already been executed in this process.
    ///
    /// Read by declaration-time pre-passes that validate names a module is
    /// expected to supply: a module still unloaded cannot be asked what it
    /// exports, so the pre-pass must defer rather than reject.
    pub(crate) fn is_module_loaded(&self, module: &str) -> bool {
        self.module.loaded_modules.contains(module)
    }

    /// True while some module compunit's mainline is currently running
    /// (`load_module`'s `run_block` is on the Rust call stack, tracked by
    /// `module_load_stack` for the whole nested chain, not just the outermost
    /// `use`). A `use`d module's own top-level named subs register under
    /// `GLOBAL` at `block_scope_depth() == 0` too, exactly like the host
    /// program's mainline — this distinguishes the two so ADR-0024's mainline
    /// lexical capture (`exec_register_sub_op`) does not treat a module's subs
    /// as the host program's mainline subs.
    pub(crate) fn module_load_active(&self) -> bool {
        !self.module.module_load_stack.is_empty()
    }

    /// The open import scopes, outermost first: one per running block that
    /// holds a `use`/`import`/`no` of its own.
    pub(crate) fn import_scopes(&self) -> &[crate::runtime::ImportScopeSnapshot] {
        &self.module.import_scope_stack
    }

    /// Save current function/class/proto keys for lexical import scoping.
    pub(crate) fn push_import_scope(&mut self) {
        self.push_import_scope_scoping_classes(true);
    }

    /// The scope the BEGIN-time module preload runs its load inside.
    ///
    /// Same rollback as an import scope for the routine and proto registries —
    /// mutsu's sub hoisting installs every exported routine under `GLOBAL::`
    /// while a module body loads, and those aliases belong to whoever wrote the
    /// `use`, not to the whole file — but the class registry is left alone: a
    /// package the module declares is exactly what the preload hoists the load
    /// in order to publish, and raku installs it into GLOBAL at load time too.
    pub(crate) fn push_preload_scope(&mut self) {
        self.push_import_scope_scoping_classes(false);
    }

    // Cost: O(1): each registry table is copy-on-write, so the snapshot holds
    // the table itself (#9170). The table's first write in the scope then
    // copies it, once.
    fn push_import_scope_scoping_classes(&mut self, scope_classes: bool) {
        let snapshot = {
            let reg = self.registry();
            crate::runtime::ImportScopeSnapshot {
                functions: std::sync::Arc::clone(&reg.functions),
                proto_subs: reg.proto_subs_snapshot(),
                proto_functions: std::sync::Arc::clone(&reg.proto_functions),
                shadowed_functions: HashMap::new(),
                function_writes_at_open: reg.function_writes,
                logged_function_writes: 0,
                installed_function_keys: Vec::new(),
                shadowed_proto_functions: HashMap::new(),
                shadowed_proto_names: HashSet::new(),
                imported_env_keys: Default::default(),
                block_bound_keys: Default::default(),
                shadowed_env_values: HashMap::new(),
                imported_routine_aliases: self.module.imported_routine_aliases.clone(),
                own_routine_imports: Default::default(),
                imported_exported_proto_tags: self.module.imported_exported_proto_tags.clone(),
                newline_mode: self.io.newline_mode,
                strict_mode: self.module.strict_mode,
                fatal_mode: self.module.fatal_mode,
                lexical_fatal_mode: self.module.lexical_fatal_mode,
                monkey_typing: self.module.monkey_typing,
                scope_classes,
                unit_env_imports: self.module.unit_env_imports.clone(),
                leave_phasers: Vec::new(),
                lexical_sets: Vec::new(),
                routine_depth: self.routine_stack().len(),
                unit: self.executing_unit_sym_for_module_load(),
                importer_type_aliases: (
                    self.import_target_package_for_load(),
                    std::sync::Arc::clone(&self.types.package_type_aliases),
                ),
            }
        };
        self.module.import_scope_stack.push(snapshot);
    }

    /// The package a module load or import running now imports into: the
    /// explicit import target, else the module whose body is loading, else
    /// the current package. Its `package_type_aliases` entry is where an
    /// import records the exported types' short names.
    // Cost: O(|package|), one clone.
    pub(crate) fn import_target_package_for_load(&self) -> String {
        self.module
            .import_target_package
            .clone()
            .or_else(|| self.module.unit_module_loading_stack.last().cloned())
            .unwrap_or_else(|| self.current_package())
    }

    /// Put back the short type-name aliases an import scope's importer package
    /// held when the scope opened ([`ImportScopeSnapshot::importer_type_aliases`]).
    // Cost: O(1) when the scope wrote no alias; otherwise one table write (a
    // copy-on-write clone of the table while the snapshot or a thread shares
    // it).
    pub(crate) fn restore_importer_type_aliases(
        &mut self,
        (importer, before): (String, std::sync::Arc<crate::runtime::package_keyed::PackageKeyed<String>>),
    ) {
        if std::sync::Arc::ptr_eq(&before, &self.types.package_type_aliases) {
            return;
        }
        let aliases = before.get(&importer);
        if self.types.package_type_aliases.get(&importer) == aliases {
            return;
        }
        // When the importer's entry is all that changed (the module itself
        // was loaded before the scope opened), put the very `before` table
        // back: a replayed block `use` finds its tables by identity
        // (`lexical_import_replay`).
        let current = &self.types.package_type_aliases;
        let others = |table: &crate::runtime::package_keyed::PackageKeyed<String>| {
            table.len() - usize::from(table.contains_key(&importer))
        };
        if others(current) == others(&before)
            && current
                .iter()
                .filter(|(pkg, _)| **pkg != importer)
                .all(|(pkg, entries)| before.get(pkg) == Some(entries))
        {
            self.types.package_type_aliases = before;
            return;
        }
        let aliases = aliases.cloned();
        let table = crate::runtime::cow_table_mut(&mut self.types.package_type_aliases);
        match aliases {
            Some(aliases) => {
                table.insert(importer, aliases);
            }
            None => {
                table.remove(&importer);
            }
        }
    }

    /// Record that `import_module` just wrote `key` into `env` as an
    /// imported alias, so the innermost open import scope (if any) knows to
    /// remove it again on pop. No-op outside any `use`-containing block —
    /// a top-level `use` (or one inside a block with no `use` of its own)
    /// has nothing on `import_scope_stack` to record against, and its
    /// imports are meant to persist anyway.
    pub(crate) fn record_import_env_key(&mut self, key: &str) {
        let key_sym = Symbol::intern(key);
        self.note_lexical_import_env_key(key_sym);
        let unit = self.executing_unit_sym_for_module_load();
        self.note_unit_env_import(key_sym);
        // A block's import over a name an enclosing import of the same unit
        // already bound shadows that binding, and its scope exit puts it back
        // (`use M :t; { use M } t`). Only an import visible when the scope
        // opened counts: an alias a BEGIN-time preload left in `env` is not
        // one.
        if let Some(top) = self.module.import_scope_stack.last_mut()
            && top.imported_env_keys.insert(key_sym)
            && top
                .unit_env_imports
                .get(&key_sym)
                .is_some_and(|entry| entry.units.contains(&unit))
            && let Some(previous) = self.env.get_sym(key_sym)
        {
            top.shadowed_env_values.insert(key_sym, previous.clone());
        }
    }

    /// Record a routine name imported into `package`. A later local
    /// declaration may replace this alias, while another declaration after
    /// that replacement remains a genuine redeclaration.
    pub(crate) fn record_imported_routine_alias(&mut self, package: &str, name: &str) {
        self.note_lexical_import_routine(package, name);
        let alias = Symbol::intern(crate::qualified::qualified_text(package, name).as_str());
        // Only the importing compunit's own scope owns the import: a module
        // loaded while the importer's block is open records its own `use`s on
        // that block's scope otherwise, and the block then shadows the module's
        // private copy of the routine (`Cro::Iri`'s `decode-percents`).
        let importer = self.executing_unit_sym_for_module_load();
        if let Some(top) = self.module.import_scope_stack.last_mut()
            && top.unit == importer
        {
            top.own_routine_imports.insert(alias);
        }
        std::sync::Arc::make_mut(&mut self.module.imported_routine_aliases).insert(alias);
    }

    pub(crate) fn imported_routine_alias(&self, package: &str, name: &str) -> bool {
        self.module
            .imported_routine_aliases
            .contains(&Symbol::intern(
                crate::qualified::qualified_text(package, name).as_str(),
            ))
    }

    /// Whether `name` is an imported routine alias of any package a bare name
    /// resolves through here (`bare_name_packages_syms`).
    // Cost: O(p), p = enclosing packages; O(1) when nothing was imported.
    pub(crate) fn imported_routine_alias_in_scope(&self, name: &str) -> bool {
        if self.module.imported_routine_aliases.is_empty() {
            return false;
        }
        let name_sym = Symbol::intern(name);
        self.bare_name_packages_syms().iter().any(|&package| {
            self.module
                .imported_routine_aliases
                .contains(&crate::qualified::qualified(package, name_sym))
        })
    }

    pub(crate) fn remove_imported_routine_alias(&mut self, package: &str, name: &str) {
        std::sync::Arc::make_mut(&mut self.module.imported_routine_aliases).remove(
            &Symbol::intern(crate::qualified::qualified_text(package, name).as_str()),
        );
    }

    pub(crate) fn record_imported_exported_proto(
        &mut self,
        package: &str,
        name: &str,
        tags: impl IntoIterator<Item = String>,
    ) {
        let key = crate::qualified::qualified(Symbol::intern(package), Symbol::intern(name));
        std::sync::Arc::make_mut(&mut self.module.imported_exported_proto_tags)
            .entry(key)
            .or_default()
            .extend(tags);
    }

    pub(crate) fn imported_exported_proto_tags(
        &self,
        package: &str,
        name: &str,
    ) -> Option<HashSet<String>> {
        let key = crate::qualified::qualified(Symbol::intern(package), Symbol::intern(name));
        self.module.imported_exported_proto_tags.get(&key).cloned()
    }

    /// Run `body` (a role's deferred-body `use`/`need` statement) and persist
    /// any package-qualified function keys it installed into
    /// `module_registered_functions`, exactly as a genuine module's own
    /// top-level declarations are tracked.
    ///
    /// A role's own `use` statement runs exactly once, memoized by role
    /// composition (`Registry::composed_role_bodies`) — semantically it is
    /// the role's compunit doing its own one-time import, indistinguishable
    /// from an ordinary module body's `use`. But nothing calls it from a
    /// module-load context: it runs from `run_role_body_for_composition` /
    /// `run_composed_role_deferred_body`, invoked lazily wherever the role
    /// happens to be composed for the first time — often deep inside an
    /// ordinary bare `{ ... }` block several calls down the stack. That block
    /// compiles to `OpCode::BlockScope`, which unconditionally snapshots and
    /// restores the routine registry around its body
    /// (`Interpreter::restore_routine_registry`) — a mechanism completely
    /// separate from the `use`-triggered `ImportScope`
    /// bracket, and blind to the fact that the import it is rolling back was
    /// never lexically scoped to begin with. Without this, the imported
    /// operator/routine was reachable only for the remainder of whichever
    /// block first triggered composition and silently vanished (`Unknown
    /// function`/`Two terms in a row`) the next time the role's own methods
    /// tried to call it from a different call stack (#8646).
    pub(crate) fn run_role_deferred_use_stmt(
        &mut self,
        body: impl FnOnce(&mut Self) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        // The caller has already set `current_package` to the role/class this
        // `use`/`need` statement belongs to (`type_owner`/`base_role_name`).
        let owner_sym = self.current_package_sym();
        let before: HashSet<Symbol> = self.registry().functions.keys().copied().collect();
        let result = body(self);
        let new_keys: Vec<Symbol> = self
            .registry()
            .functions
            .keys()
            .filter(|k| !before.contains(*k))
            .filter(|k| {
                let s = k.resolve();
                crate::qualified::is_qualified_str(&s) && !s.starts_with("GLOBAL::")
            })
            .copied()
            .collect();
        if !new_keys.is_empty() {
            let table = crate::runtime::cow_table_mut(&mut self.module.module_registered_functions);
            for key in new_keys {
                table.insert(key);
            }
            crate::runtime::cow_table_mut(&mut self.module.packages_with_deferred_use_imports)
                .insert(owner_sym);
        }
        result
    }

    /// Whether `class_name`'s own package received a routine import from a
    /// role's deferred `use`/`need` body statement — see
    /// `run_role_deferred_use_stmt`. Consulted by method dispatch as an extra
    /// reason to anchor `current_package` to a FLAT (non-namespaced)
    /// receiver class, which owns no `::` for the existing checks to key off
    /// (#8646 shape 1).
    ///
    /// `class_name` here is the *receiver's* class, which for a parameterised
    /// role-pun carries its type argument in brackets (`FlatHolder[FlatComparable]`)
    /// — the deferred body ran, and was recorded, against the role's own bare
    /// name (`FlatHolder`; see `run_role_body_for_composition`'s `type_owner`).
    /// Strip the bracket suffix before looking up, exactly like
    /// `compute_bare_name_packages` does for the same reason.
    // Cost: O(1) when no package received a deferred import (the common case).
    pub(crate) fn package_has_deferred_use_imports(&self, class_name: &str) -> bool {
        if self.module.packages_with_deferred_use_imports.is_empty() {
            return false;
        }
        let base = class_name.split('[').next().unwrap_or(class_name);
        Symbol::lookup(base).is_some_and(|sym| {
            self.module
                .packages_with_deferred_use_imports
                .contains(&sym)
        })
    }

    /// Whether `key` is a `GLOBAL::MAIN` candidate key (`GLOBAL::MAIN` or
    /// `GLOBAL::MAIN/<signature>`), the program's own MAIN slot.
    // Cost: O(k), k = key length.
    fn is_global_main_key(key: &str) -> bool {
        key.strip_prefix("GLOBAL::MAIN")
            .is_some_and(|rest| rest.is_empty() || rest.starts_with('/'))
    }

    /// Remove the `GLOBAL::MAIN` candidates a module (not the program)
    /// declared: lexical to the import scope that loaded the module, even when
    /// they were registered before it opened (see `pop_import_scope`).
    // Cost: O(k + m), k = interned `GLOBAL::MAIN/…` names, m = symbols interned
    // since the family index last caught up.
    fn drop_foreign_global_main_candidates(&mut self, program_path: Option<Symbol>) -> Vec<Symbol> {
        // The routine table this check last found clean is still the one
        // installed: versions are never reused, so nothing was added since.
        let version = self.registry().functions.version();
        if self.module.foreign_main_clean_version == Some(version) {
            return Vec::new();
        }
        let main = crate::symbol::wk::global_main();
        let mut keys = crate::qualified_tail_index::names_in_family("GLOBAL::MAIN");
        keys.push(main);
        let foreign: Vec<Symbol> = {
            let reg = self.registry();
            keys.into_iter()
                .filter(|key| {
                    reg.functions.get(key).is_some_and(|def| {
                        def.source_file
                            .as_deref()
                            .map(Symbol::intern)
                            .is_some_and(|f| Some(f) != program_path)
                    })
                })
                .collect()
        };
        if !foreign.is_empty() {
            let mut reg = self.registry_mut();
            let functions = reg.functions_mut();
            for key in &foreign {
                functions.remove(key);
            }
        }
        let version = self.registry().functions.version();
        self.module.foreign_main_clean_version = Some(version);
        foreign
    }

    /// Log one functions-map write an import made while `import_scope_stack`'s
    /// innermost scope is open, with the key it installed (`None` for a
    /// removal, which that scope's `shadowed_functions` restores). See
    /// `ImportScopeSnapshot::logged_function_writes`.
    // Cost: O(1) amortized.
    pub(crate) fn log_import_scope_function_write(&mut self, installed: Option<Symbol>) {
        if let Some(scope) = self.module.import_scope_stack.last_mut() {
            scope.logged_function_writes += 1;
            if let Some(key) = installed {
                scope.installed_function_keys.push(key);
            }
        }
    }

    /// Whether an import scope's rollback keeps the registry function `key`
    /// (see `pop_import_scope`).
    // Cost: O(|key|).
    fn import_scope_keeps_function(
        key: &Symbol,
        def: &Arc<FunctionDef>,
        func_snapshot: &crate::runtime::function_table::FunctionTable,
        program_path: Option<Symbol>,
        module_keys: &HashSet<Symbol>,
        scope_aliases: &HashSet<Symbol>,
    ) -> bool {
        // A `GLOBAL::MAIN` candidate a module declared is lexical to the
        // block that imported it even when a parse-time `use` already
        // registered it before the block opened (and so into
        // `func_snapshot`): only the program's own `MAIN` stays.
        // The key test first: it is a byte comparison, the source file test an
        // intern, and this runs for every registered routine.
        let is_main = Self::is_global_main_key(key.as_str());
        if is_main
            && let Some(file) = def.source_file.as_deref().map(Symbol::intern)
            && Some(file) != program_path
        {
            return false;
        }
        if func_snapshot.contains_key(key) {
            return true;
        }
        let ks = key.as_str();
        // A `MAIN` the block imported (`{ use CLI::Ecosystem; }` -- the
        // idiom for loading a MAIN-exporting module without running
        // it) is lexical to the block, so the program's own MAIN
        // dispatch must not find it once the block is gone. The
        // module's load keeps it in `module_keys`, which protects
        // other routines but is not an owner of the importing scope's
        // MAIN.
        if is_main {
            return false;
        }
        if module_keys.contains(key) {
            return true;
        }
        if !scope_aliases.is_empty() {
            let base = ks.split_once('/').map_or(ks, |(base, _)| base);
            if Symbol::lookup(base).is_some_and(|sym| scope_aliases.contains(&sym)) {
                return false;
            }
        }
        crate::qualified::is_qualified_str(ks) && !ks.starts_with("GLOBAL::")
    }

    // Cost: O(1) per registry table the scope did not write (the snapshot is the
    // table itself). A written routine table is O(x + k), x = keys the scope's
    // imports installed, k = interned `GLOBAL::MAIN/…` names, when the scope's
    // logged imports were its only writes, else O(F), F = registered routines
    // (the rollback filter). The class table is never rolled back: a class
    // an import registered is the module's own, and a name it exports binds
    // lexically (ADR-8751). Rakudo: O(1) -- see #9170.
    pub(crate) fn pop_import_scope(&mut self) {
        if let Some(snapshot) = self.module.import_scope_stack.pop() {
            let crate::runtime::ImportScopeSnapshot {
                functions: func_snapshot,
                proto_subs: proto_sub_snapshot,
                proto_functions: proto_fn_snapshot,
                shadowed_functions,
                function_writes_at_open,
                logged_function_writes,
                installed_function_keys,
                shadowed_proto_functions,
                shadowed_proto_names: _,
                imported_env_keys,
                block_bound_keys: _,
                mut shadowed_env_values,
                unit_env_imports,
                imported_routine_aliases,
                own_routine_imports: _,
                imported_exported_proto_tags,
                newline_mode,
                strict_mode,
                fatal_mode,
                lexical_fatal_mode,
                monkey_typing,
                scope_classes,
                leave_phasers: _,
                lexical_sets: _,
                routine_depth: _,
                unit: _,
                importer_type_aliases,
            } = snapshot;
            self.restore_importer_type_aliases(importer_type_aliases);
            // Whether the scope left every routine table as it found it (a
            // `use` that bound only lexical routines, or a replayed one,
            // #13906): then no name-keyed resolution cache can hold a routine
            // this pop removes, and none needs invalidating.
            let tables_untouched =
                std::sync::Arc::ptr_eq(&func_snapshot, &self.registry().functions)
                    && std::sync::Arc::ptr_eq(
                        &proto_sub_snapshot,
                        &self.registry().proto_subs_snapshot(),
                    )
                    && std::sync::Arc::ptr_eq(&proto_fn_snapshot, &self.registry().proto_functions)
                    && shadowed_functions.is_empty()
                    && shadowed_proto_functions.is_empty();
            // Remove functions added since the push, EXCEPT a module's own
            // fully-qualified source definitions (`Fancy::Utilities::lolgreet`,
            // `Fancy::Utilities::EXPORT::ALL::lolgreet`). Those persist as long
            // as the module is loaded — a later block-scoped `use Fancy...` in a
            // sibling block re-imports from them (`import_module`), and dropping
            // them left the re-import with nothing to alias ("Unknown function").
            // The IMPORTED aliases (bare names and `GLOBAL::name`) are still
            // removed, so a bare call after the block exits still dies (roast
            // S11-modules/lexical.t: `{ use Foo } EVAL('foo()')`).
            // `GLOBAL::`-prefixed entries the block itself imported still go,
            // but one a LOADED MODULE's own body installed stays: a `unit
            // module`'s body runs at `current_package() == GLOBAL`, so its own
            // `use NativeCall` registers `GLOBAL::nativecast`, and dropping that
            // when an enclosing block's import scope popped left the module
            // half-loaded -- `loaded_modules` still claimed it was loaded, so
            // the later top-level `use` short-circuited and could not put it
            // back. `module_registered_functions` is exactly that set: its delta
            // is taken BEFORE `import_module`, so an alias installed for the
            // IMPORTING scope is never in it and `{ use Foo } foo()` still dies
            // (`roast/S11-modules/lexical.t`). This is the block twin of the
            // carve-out `reinstate_module_functions` gives the EVAL rollback.
            //
            // A package-qualified key is not always the module's own
            // definition, though: an operator `use`d inside a routine body is
            // aliased under the routine's unit package (`ModT::infix:<**>`, see
            // `import_module`'s `target_pkg`), which has exactly the shape of a
            // definition. Such an alias was recorded as an imported routine
            // alias during this scope (it is absent from the snapshot's alias
            // set), so it goes with the scope too -- otherwise
            // `sub f { use FiniteField; ... }` inside a module leaked its
            // `infix:<**>` into every later call in that package.
            let program_path = self.io.program_path_sym;
            // A table the scope never wrote added nothing to roll back; only a
            // module-declared `GLOBAL::MAIN` candidate registered before the
            // scope opened still goes (the first rule below).
            let functions_unchanged =
                std::sync::Arc::ptr_eq(&func_snapshot, &self.registry().functions);
            // Every key whose presence the rollback changes, so the
            // invalidation below can evict just those base names from the
            // key index instead of dropping all of it (#9170).
            let mut changed_keys: Vec<Symbol> = Vec::new();
            if functions_unchanged {
                changed_keys = self.drop_foreign_global_main_candidates(program_path);
            } else {
                let scope_aliases: HashSet<Symbol> = self
                    .module
                    .imported_routine_aliases
                    .difference(&imported_routine_aliases)
                    .copied()
                    .collect();
                let module_keys = std::sync::Arc::clone(&self.module.module_registered_functions);
                let all_writes_logged = self.registry().function_writes
                    == function_writes_at_open + logged_function_writes;
                if all_writes_logged {
                    // Every write since the push was one of the logged
                    // installs: a key not in the snapshot is one of
                    // `installed_function_keys`, so only those are judged,
                    // and the snapshot's own keys answer to the MAIN rule
                    // alone (#9170).
                    let mut seen = rustc_hash::FxHashSet::default();
                    let dropped: Vec<Symbol> = {
                        let registry = self.registry();
                        installed_function_keys
                            .iter()
                            .filter(|key| seen.insert(**key))
                            .filter(|key| {
                                registry.functions.get(*key).is_some_and(|def| {
                                    !Self::import_scope_keeps_function(
                                        key,
                                        def,
                                        &func_snapshot,
                                        program_path,
                                        &module_keys,
                                        &scope_aliases,
                                    )
                                })
                            })
                            .copied()
                            .collect()
                    };
                    if !dropped.is_empty() {
                        let mut registry = self.registry_mut();
                        let functions = registry.functions_mut();
                        for key in &dropped {
                            functions.remove(key);
                        }
                    }
                    changed_keys = dropped;
                    changed_keys.extend(self.drop_foreign_global_main_candidates(program_path));
                } else {
                let removed = &mut changed_keys;
                self.registry_mut().functions_mut().retain(|key, def| {
                    let keep = Self::import_scope_keeps_function(
                        key,
                        def,
                        &func_snapshot,
                        program_path,
                        &module_keys,
                        &scope_aliases,
                    );
                    if !keep {
                        removed.push(*key);
                    }
                    keep
                });
                }
            }
            // An imported proto/multi family has lexical shadowing semantics,
            // but its candidates share the importing package's flat registry
            // keys. Restore any enclosing definitions that the first import in
            // this scope hid before leaving the scope.
            // Only when the import hid something: `functions_mut` copies the
            // table while the snapshot still shares it.
            if !shadowed_functions.is_empty() {
                changed_keys.extend(shadowed_functions.keys().copied());
                self.registry_mut()
                    .functions_mut()
                    .extend(shadowed_functions.into_iter().filter(|(key, def)| {
                        // ...and what the import hid is not the program's `MAIN`
                        // either when a module declared it.
                        !(Self::is_global_main_key(key.as_str())
                            && def
                                .source_file
                                .as_deref()
                                .map(Symbol::intern)
                                .is_some_and(|file| Some(file) != program_path))
                    }));
            }
            // `proto sub name(|) is export` imports under the importing package
            // (`GLOBAL::skip`) into BOTH proto tables, and `has_proto` reads the
            // name set. Left behind, an imported proto kept a bare call on the
            // user-routine dispatch path after the block exited — which is what
            // made `roast/S32-list/skip.t`'s selective `do { use Test; ... }`
            // import still hand `skip(5, @a)` a VarRef-wrapped array instead of
            // the flattened list the core routine expects. Same keep-rule as
            // functions: the module's own `Test::skip` stays for a later
            // re-import, only the `GLOBAL::` alias goes.
            if !std::sync::Arc::ptr_eq(&proto_sub_snapshot, &self.registry().proto_subs_snapshot())
            {
                self.registry_mut().proto_subs_retain(|key| {
                    proto_sub_snapshot.contains(key)
                        || (crate::qualified::is_qualified_str(key) && !key.starts_with("GLOBAL::"))
                });
            }
            if !std::sync::Arc::ptr_eq(&proto_fn_snapshot, &self.registry().proto_functions) {
                self.registry_mut().proto_functions_mut().retain(|key, _| {
                    if proto_fn_snapshot.contains_key(key) {
                        return true;
                    }
                    let ks = key.resolve();
                    crate::qualified::is_qualified_str(&ks) && !ks.starts_with("GLOBAL::")
                });
            }
            if !shadowed_proto_functions.is_empty() {
                self.registry_mut()
                    .proto_functions_mut()
                    .extend(shadowed_proto_functions);
            }
            // The `env` half: drop the aliases this scope imported (see
            // `restore_import_env_keys`). A PRELOAD scope (`scope_classes ==
            // false`, see `push_preload_scope`) keeps them.
            if scope_classes {
                self.restore_import_env_keys(imported_env_keys, &mut shadowed_env_values);
            }
            self.io.newline_mode = newline_mode;
            self.module.strict_mode = strict_mode;
            self.module.fatal_mode = fatal_mode;
            self.module.lexical_fatal_mode = lexical_fatal_mode;
            self.module.monkey_typing = monkey_typing;
            self.module.imported_routine_aliases = imported_routine_aliases;
            self.module.imported_exported_proto_tags = imported_exported_proto_tags;
            self.module.unit_env_imports = unit_env_imports;
            // Removing imported functions when a lexical import scope pops must
            // invalidate the name-keyed function-resolution caches: a sub that
            // was OTF-compiled and cached under its bare name while in scope
            // (otf_call_cache) would otherwise still be reachable after the
            // scope exits — e.g. `{ use Foo } EVAL('foo()')` must die, not hit
            // the stale cache (roast/S11-modules/lexical.t). Registration bumps
            // fn_resolve_gen; the matching un-registration here must too.
            // Only the base names of the keys the rollback removed or put back
            // leave the key index; the rest of it still describes the map.
            if !tables_untouched {
                self.invalidate_fn_resolution_changing_keys(changed_keys);
            }
        }
    }

    /// The ordinary export tags a `use` of a module with a `sub EXPORT` hook
    /// still imports. The hook receives the positional `use` arguments, but a
    /// colonpair tag (`use M :u`) keeps selecting the module's `is export(:u)`
    /// symbols -- and an undeclared one is still `X::Import::NoSuchTag` -- as
    /// in rakudo (#9389). `:ALL` requests the whole surface.
    // Cost: O(t), t = tags.
    fn export_hook_ordinary_tags(tags: &[String]) -> Vec<String> {
        if tags.iter().any(|tag| tag.eq_ignore_ascii_case("all")) {
            return vec!["ALL".to_string()];
        }
        tags.to_vec()
    }

    pub fn use_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        self.use_module_with_tags(module, &[])
    }

    /// Split `Name:auth<zef:foo>:ver<0.0.20+>` into the bare module name and
    /// its distribution selectors. The parser only appends the dist adverbs
    /// (`ver`/`auth`/`api`) in this literal angle form, so the split is
    /// unambiguous: a `::`-qualified name never contains a lone `:`.
    ///
    /// `:v<…>` is Raku's short spelling of `:ver<…>` and normalizes to it. The
    /// two never collide: the trailing `<` makes `:v<` and `:ver<` distinct
    /// patterns.
    pub(crate) fn split_dist_selectors(module: &str) -> (&str, Vec<(String, String)>) {
        let mut selectors = Vec::new();
        let mut bare_end = module.len();
        for (key, canonical) in [
            ("ver", "ver"),
            ("v", "ver"),
            ("auth", "auth"),
            ("api", "api"),
        ] {
            let pat = format!(":{}<", key);
            if let Some(pos) = module.find(&pat) {
                bare_end = bare_end.min(pos);
                let after = &module[pos + pat.len()..];
                if let Some(end) = after.find('>') {
                    selectors.push((canonical.to_string(), after[..end].to_string()));
                }
            }
        }
        (&module[..bare_end], selectors)
    }

    /// Load a module the way `use` does — registering its exports, package
    /// globals and types — but *without* importing anything into the current
    /// lexical scope.
    ///
    /// This is the BEGIN-time half of `use` (see
    /// [`crate::opcode::OpCode::PreloadModule`]): Raku loads every `use`d
    /// compunit before the importing unit's mainline runs, so its packages are
    /// visible everywhere, while the import itself stays lexical to the scope
    /// holding the `use`. `need_module` is not a substitute — it skips the
    /// `use`-only work (export-hook re-runs, the import itself).
    pub(crate) fn preload_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        self.use_module_with_tags_scoped(module, &[], false)
    }

    pub fn use_module_with_tags(
        &mut self,
        module: &str,
        tags: &[String],
    ) -> Result<(), RuntimeError> {
        self.use_module_with_tags_scoped(module, tags, true)
    }

    /// `import`: whether the module's exports are installed into the current
    /// lexical scope. False only for the BEGIN-time preload above.
    fn use_module_with_tags_scoped(
        &mut self,
        module: &str,
        tags: &[String],
        import: bool,
    ) -> Result<(), RuntimeError> {
        // The parser rides dist selectors on the module name
        // (`JSON::Class:auth<zef:jonathanstowe>:api<1.0>`). Split them off here
        // so every registry/loaded_modules key below uses the bare name, and
        // only distribution resolution sees the constraints. Save/restore
        // around the load: a transitive `use` inside the module body must
        // resolve with ITS OWN (usually absent) selectors, not the outer ones.
        let (module, dist_selectors) = Self::split_dist_selectors(module);
        let saved = std::mem::replace(&mut self.module.pending_dist_selectors, dist_selectors);
        // `suppress_exports` is set for the whole duration of an enclosing
        // `CompUnit::Repository.need` load (see `load_module_from_path`), so
        // its own compunit's `is export` subs never get registered as
        // importable. But an explicit `use` nested inside that compunit's
        // body (e.g. a needed `CT.rakumod` that itself says `use Test;`) must
        // still register and import Test's exports normally -- otherwise CT's own methods can
        // never resolve `diag` via `module_imported_lexical_names`, even
        // though CT's own mainline genuinely imported it (#7805). `use`
        // always wants ordinary export semantics regardless of an ambient
        // `need`, so suspend the flag for exactly this nested load.
        let saved_suppress_exports = std::mem::replace(&mut self.module.suppress_exports, false);
        let saved_no_import = std::mem::replace(&mut self.module.loading_without_import, false);
        let result = self.use_module_with_tags_inner(module, tags, import);
        self.module.loading_without_import = saved_no_import;
        self.module.suppress_exports = saved_suppress_exports;
        self.module.pending_dist_selectors = saved;
        // `load_module` consumes `pending_use_export_args`; clear any residue
        // here so a native/pragma/already-loaded path (which never reaches
        // `load_module`) cannot leak this `use`'s args into a later one.
        self.module.pending_use_export_args = None;
        result
    }

    fn use_module_with_tags_inner(
        &mut self,
        module: &str,
        tags: &[String],
        import: bool,
    ) -> Result<(), RuntimeError> {
        if self.module.loaded_modules.contains(module) {
            if module == "strict" {
                self.module.strict_mode = true;
                self.mark_strict_pragma(true);
            } else if module == "fatal" {
                self.module.fatal_mode = true;
                self.module.lexical_fatal_mode = true;
            } else if module == "MONKEY-TYPING" || module == "MONKEY" {
                self.module.monkey_typing = true;
            }
            // The module stays loaded, so this `use` is a no-op — but a scope
            // that restored `env` wholesale since the load (a sub call around a
            // `require`, a block, an `EVAL`) may have taken its `our` globals
            // with it. Put them back, or this no-op leaves the caller unable to
            // reach a symbol the module really does define.
            self.reinstate_module_package_globals(module);
            // Propagate package declarations from the already-loaded module
            // into the current chain so that chain_has_package_decl checks
            // correctly detect namespace contributions from transitive deps.
            if let Some(pkgs) = self.module.module_packages.get(module).cloned() {
                crate::runtime::cow_table_mut(&mut self.module.chain_declared_packages)
                    .extend(pkgs);
            }
            // When a module that declares a class/role matching its own name
            // is directly `use`d at the top level, un-hide it and its related
            // classes from the package stash. This handles the case where the
            // module was first loaded transitively by a non-contributing module.
            if self.module.module_load_stack.is_empty()
                && crate::qualified::is_qualified_str(module)
            {
                let is_contributor = {
                    let registry = self.registry();
                    registry.classes.contains_key(module) || registry.roles.contains_key(module)
                };
                if is_contributor {
                    crate::runtime::cow_table_mut(&mut self.types.package_stash_hidden)
                        .remove(module);
                }
            }
            // A re-`use` of an already-loaded module skips `load_module_inner`
            // entirely (nothing new to register), so the importer-scoped
            // class/role short-name aliasing that runs there on first load
            // (see `run_modules.rs`, `new_types`) never fires for a second
            // importer. Do the same copy here instead, from the aliases
            // already recorded against the module's own name (populated by
            // both that first-load pass and by `exec_register_class_op`'s own
            // declaration-time write) into this importer's own entry — e.g.
            // `DBDish::Pg::ErrorHandling`'s `use DBDish::Pg::Native;` loads
            // it first (importer "GLOBAL"), and `DBDish::Pg`'s own `use
            // DBDish::Pg::Native;` later must independently see `PGconn`
            // bare too, even though the module itself is already loaded.
            if let Some(module_aliases) = self.types.package_type_aliases.get(module).cloned() {
                let importer_package = self.import_target_package_for_load();
                let entry = crate::runtime::cow_table_mut(&mut self.types.package_type_aliases)
                    .entry(importer_package)
                    .or_default();
                let exported = self.module.module_owned_types.get(module);
                for (short, qualified) in module_aliases {
                    // The module's own alias table also holds private types
                    // and aliases it imported from dependencies. Copy only a
                    // type this module declared and exported.
                    if exported.is_some_and(|types| types.exported.contains(&qualified)) {
                        entry.entry(short).or_insert(qualified);
                    }
                }
            }
            let importer_package = self.import_target_package_for_load();
            self.copy_published_outer_packages(module, &importer_package);
            // #7797: same gap as the aliasing copy just above, for package-
            // qualified-name visibility instead of bare short-name aliasing.
            self.replay_module_visibility_grant(module);
            // A module with a `sub EXPORT` runs it on every import — its map
            // may depend on the `use` arguments (the Slangify pattern) — even
            // though the module body itself is not re-run.
            if !import {
                return Ok(());
            }
            self.rerun_module_export(module)?;
            // A module-defined EXPORT receives the positional use arguments
            // itself (`use JSON::Fast <immutable !pretty>`); its returned map
            // is combined with the ordinary `is export` declarations the
            // colonpair tags select (see `export_hook_ordinary_tags`).
            if self.module.module_export_defs.contains_key(module) {
                let ordinary_tags = Self::export_hook_ordinary_tags(tags);
                return match self.import_module(module, &ordinary_tags) {
                    Ok(()) => Ok(()),
                    Err(err) if err.message.starts_with("No exports found for module:") => Ok(()),
                    Err(err) => Err(err),
                };
            }
            return match self.import_module(module, tags) {
                Ok(()) => Ok(()),
                Err(err) if err.message.starts_with("No exports found for module:") => Ok(()),
                Err(err) => Err(err),
            };
        }
        if self.module.module_load_stack.iter().any(|m| m == module) {
            let mut chain = self.module.module_load_stack.clone();
            chain.push(module.to_string());
            return Err(RuntimeError::new(format!(
                "circular module dependency detected: {}",
                chain.join(" -> ")
            )));
        }
        // At the top level (no modules currently loading), save and clear
        // the chain-scoped package declarations so each top-level `use` gets
        // a fresh chain. Nested uses inherit the parent's chain.
        let is_top_level_use = self.module.module_load_stack.is_empty();
        let saved_chain_pkgs = if is_top_level_use {
            std::mem::take(&mut self.module.chain_declared_packages)
        } else {
            Default::default()
        };
        self.module.module_load_stack.push(module.to_string());
        let class_snapshot: HashSet<String> = self.registry().classes.keys().cloned().collect();
        let role_snapshot: HashSet<String> = self.registry().roles.keys().cloned().collect();
        let env_snapshot: HashSet<Symbol> = self.env.keys().copied().collect();
        let package_symbols_before = self.module.module_toplevel.package_symbols.clone();
        let func_keys_before: HashSet<Symbol> = self.registry().functions.keys().copied().collect();
        let proto_keys_before = std::sync::Arc::clone(&self.registry().proto_functions);

        // `Test` and `NativeCall` load rakudo's own `.rakumod` (#7566, #11203),
        // which run their own `is export` declarations.
        let result = if matches!(
            module,
            "strict"
                    | "warnings"
                    | "MONKEY-SEE-NO-EVAL"
                    | "MONKEY-TYPING"
                    // `use MONKEY-GUTS` only lifts the ban on `nqp::` ops in
                    // user code, which mutsu does not enforce; recognizing it is
                    // enough. It is a core pragma (roast's Test::Util uses it),
                    // and failing the `use` aborted the rest of that module's
                    // mainline — every declaration after it silently vanished.
                    | "MONKEY-GUTS"
                    | "nqp"
                    | "MONKEY"
                    | "newline"
                    | "soft"
                    // `use worries`: a parse-time warning toggle (see the parser).
                    | "worries"
                    // `use trace`: the parser emits `Stmt::Trace` hooks; nothing
                    // is left to do when the `use` itself runs.
                    | "trace"
                    | "fatal"
                    | "oo"
                    | "class"
        ) {
            // Track MONKEY-TYPING pragma
            if module == "MONKEY-TYPING" || module == "MONKEY" {
                self.module.monkey_typing = true;
            }
            if module == "MONKEY-SEE-NO-EVAL" || module == "MONKEY" {
                self.set_monkey_see_no_eval(true);
            }
            Ok(())
        } else if module.starts_with("Test::") && !self.module.require_propagates_missing_module {
            // Load Test:: submodules from source as regular modules.
            // Parse errors should propagate like other `use` failures.
            // Missing helper modules remain non-fatal for compatibility —
            // except under `require`, whose whole contract is that a missing
            // module is a catchable X::CompUnit::UnsatisfiedDependency
            // (HTTP::UserAgent's `t/001-meta` skips itself that way).
            match self.load_module(module) {
                Ok(()) => Ok(()),
                Err(err) if err.is_unsatisfied_dependency() => {
                    // Still non-fatal (real raku hard-errors here, but mutsu
                    // deliberately tolerates a missing Test::* helper for
                    // compatibility — see the comment above), but a fully
                    // silent no-op can mask a genuinely missing dependency
                    // (a typo, not a deliberately-unvendored helper) behind
                    // a test file that quietly runs zero assertions. A
                    // stderr note at least leaves a trace in a CI log.
                    self.write_warn_to_stderr(&format!(
                        "WARNING: could not find module {module} to use, ignoring"
                    ));
                    Ok(())
                }
                Err(err) => Err(err),
            }
        } else {
            self.load_module(module)
        };

        self.module.module_load_stack.pop();
        if result.is_ok() {
            let module_short = if let Some((_, short)) =
                crate::qualified::split_qualified(crate::qualified::known_symbol(module))
                    .map(|(head, tail)| (head.as_str(), tail.as_str()))
            {
                short
            } else {
                module
            };
            let class_names: Vec<String> = self.registry().classes.keys().cloned().collect();
            for class_name in &class_names {
                if class_snapshot.contains(class_name) {
                    continue;
                }
                let class_short =
                    crate::qualified::last_segment(crate::qualified::known_symbol(class_name))
                        .as_str();
                if class_short != module_short {
                    crate::runtime::cow_table_mut(&mut self.module.need_hidden_classes)
                        .insert(class_name.clone());
                    crate::runtime::cow_table_mut(&mut self.module.need_hidden_classes)
                        .insert(class_short.to_string());
                }
            }
            // A module's top-level package-qualified symbols live off the env
            // (ADR-0084 §2 group 2), so the new ones are scanned alongside.
            let new_package_symbols = self
                .module
                .module_toplevel
                .package_symbols
                .keys()
                .filter(|k| !package_symbols_before.contains_key(*k));
            for key in self.env.keys().chain(new_package_symbols) {
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
                let key_short = crate::qualified::last_segment(*key).as_str();
                if !key_short
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
                {
                    continue;
                }
                if key_short != module_short {
                    crate::runtime::cow_table_mut(&mut self.module.need_hidden_classes)
                        .insert(key_s.clone());
                    crate::runtime::cow_table_mut(&mut self.module.need_hidden_classes)
                        .insert(key_short.to_string());
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
            if let Some((namespace, _)) =
                crate::qualified::split_qualified(crate::qualified::known_symbol(module))
                    .map(|(head, tail)| (head.as_str(), tail.as_str()))
            {
                let module_declares_own_class = {
                    let registry = self.registry();
                    registry.classes.contains_key(module) || registry.roles.contains_key(module)
                };
                let chain_has_package_decl =
                    self.module.chain_declared_packages.contains(namespace);
                if !module_declares_own_class && !chain_has_package_decl {
                    // Hide newly registered classes/roles from the namespace
                    // stash -- except the ones this module's own file declares
                    // (`class Foo::p {}` inside `Foo::Tags`): those are its
                    // direct contribution to the namespace, only a dependency's
                    // are transitive.
                    let transitive = |name: &String| {
                        self.module
                            .module_registered_types
                            .iter()
                            .any(|(m, types)| m != module && types.contains(name))
                    };
                    let class_names: Vec<String> =
                        self.registry().classes.keys().cloned().collect();
                    let role_names: Vec<String> = self.registry().roles.keys().cloned().collect();
                    let hide: Vec<String> = class_names
                        .iter()
                        .filter(|n| !class_snapshot.contains(*n))
                        .chain(role_names.iter().filter(|n| !role_snapshot.contains(*n)))
                        .filter(|n| {
                            n.starts_with(namespace)
                                && n.get(namespace.len()..namespace.len() + 2) == Some("::")
                                && transitive(n)
                        })
                        .cloned()
                        .collect();
                    for name in hide {
                        crate::runtime::cow_table_mut(&mut self.types.package_stash_hidden)
                            .insert(name);
                    }
                }
            }
            let registered: HashSet<String> = self
                .registry()
                .classes
                .keys()
                .filter(|n| !class_snapshot.contains(*n))
                .chain(
                    self.registry()
                        .roles
                        .keys()
                        .filter(|n| !role_snapshot.contains(*n)),
                )
                .cloned()
                .collect();
            crate::runtime::cow_table_mut(&mut self.module.module_registered_types)
                .insert(module.to_string(), registered);
            // Record which packages were declared during this module's chain
            // so they can be propagated when the module is re-used.
            if !self.module.chain_declared_packages.is_empty() {
                let chain = (*self.module.chain_declared_packages).clone();
                crate::runtime::cow_table_mut(&mut self.module.module_packages)
                    .insert(module.to_string(), chain);
            }
            // Restore the chain-scoped package declarations (top-level only)
            if is_top_level_use {
                self.module.chain_declared_packages = saved_chain_pkgs;
            }

            if module == "strict" {
                self.module.strict_mode = true;
                self.mark_strict_pragma(true);
            } else if module == "fatal" {
                self.module.fatal_mode = true;
                self.module.lexical_fatal_mode = true;
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
            // Collect the exports THIS module owns: everything it registered
            // while loading (attributed via `module_owned_exports`) plus any
            // exports registered directly under its package name. Deliberately
            // does NOT consult the blanket `exported_subs["GLOBAL"]`, which also
            // pools exports pulled in by transitive `use`s inside this module
            // (e.g. a nested `use Other :tag`). Those are legitimately imported
            // into the inner module and its methods must still resolve them, so
            // hiding them here would break the inner module.
            let mut owned_exports: rustc_hash::FxHashMap<String, rustc_hash::FxHashSet<String>> =
                self.module
                    .module_owned_exports
                    .get(module)
                    .cloned()
                    .unwrap_or_default();
            if let Some(pkg_subs) = self.module.exported_subs.get(module) {
                for (name, symbol_tags) in pkg_subs {
                    owned_exports
                        .entry(name.clone())
                        .or_default()
                        .extend(symbol_tags.iter().cloned());
                }
            }
            for (name, symbol_tags) in &owned_exports {
                let is_mandatory = symbol_tags.contains("MANDATORY");
                if !want_all
                    && !is_mandatory
                    && !symbol_tags.iter().any(|tag| requested_tags.contains(tag))
                {
                    // This export should NOT be imported under the current tags —
                    // hide it from GLOBAL. Rather than DELETE it (which would lose
                    // the definition and make a later `use MOD :tag` unable to
                    // restore it — a bare-file module registers exports only under
                    // GLOBAL::), RENAME it to a module-qualified `MOD::name` key.
                    // That keeps it out of the unqualified namespace while leaving
                    // `import_module` able to re-alias it on a subsequent tagged
                    // `use` (its source lookup is `MOD::name`). See T-042
                    // (Math::Arrow `use ... :constants` after a plain `use`).
                    let global_key = Symbol::intern(&format!("GLOBAL::{}", name));
                    if !func_keys_before.contains(&global_key) {
                        let removed = self.registry_mut().functions_mut().remove(&global_key);
                        if let Some(def) = removed {
                            let qualified = Symbol::intern(
                                crate::qualified::qualified_text(module, name).as_str(),
                            );
                            self.registry_mut()
                                .functions_mut()
                                .entry(qualified)
                                .or_insert(def);
                        }
                    }
                    // Also rename multi-dispatch variants (`GLOBAL::name/sig`).
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
                    // Invalidate name-keyed resolution caches (keys renamed).
                    self.invalidate_fn_resolution();
                    for mk in multi_keys {
                        let removed = self.registry_mut().functions_mut().remove(&mk);
                        if let Some(def) = removed {
                            let suffix = mk.as_str().strip_prefix("GLOBAL::").unwrap().to_string();
                            let qualified = Symbol::intern(
                                crate::qualified::qualified_text(module, &suffix).as_str(),
                            );
                            self.registry_mut()
                                .functions_mut()
                                .entry(qualified)
                                .or_insert(def);
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
                if let Some(subs) = self.module.exported_subs.get(source) {
                    for name in subs.keys() {
                        if name.contains(":<") {
                            exported_op_names.insert(name.clone());
                        }
                    }
                }
            }
            if let Some(subs) = self.module.unit_module_exported_subs.get(module) {
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
                        // The arity suffix, NOT the first `/`: `infix:</>` carries
                        // a `/` of its own, and splitting at it spelled the name
                        // `infix:<`, which no `exported_op_names` entry can match —
                        // so an exported `multi infix:</>` was reaped here right
                        // after declaring it (Math::Vector's `$vector / $scalar`).
                        let base =
                            crate::runtime::dispatch_resolve::function_key_strip_arity_suffix(name);
                        // Only remove operator subs (infix:<...>, prefix:<...>, etc.)
                        // An OUR-bound code alias is package-owned too, even when
                        // the source routine was not marked `is export` here.
                        base.contains(":<")
                            && !exported_op_names.contains(base)
                            && !self.registry().our_scoped_functions.contains_key(k)
                    } else {
                        false
                    }
                })
                .copied()
                .collect();
            for k in non_exported_op_globals {
                self.registry_mut().functions_mut().remove(&k);
            }
            // Invalidate name-keyed resolution caches.
            self.invalidate_fn_resolution();

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
                    if let Some(subs) = self.module.exported_subs.get(source) {
                        for name in subs.keys() {
                            exported_names.insert(name.clone());
                        }
                    }
                }
                if let Some(subs) = self.module.unit_module_exported_subs.get(module) {
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
                        let base =
                            crate::runtime::dispatch_resolve::function_key_strip_arity_suffix(name);
                        !exported_names.contains(base) && Self::is_builtin_function(base)
                    })
                    .copied()
                    .collect();
                for k in leaked_globals {
                    self.registry_mut().functions_mut().remove(&k);
                }
                // Invalidate name-keyed resolution caches.
                self.invalidate_fn_resolution();
            }

            crate::runtime::cow_table_mut(&mut self.module.loaded_modules)
                .insert(module.to_string());
            // Record the routines this module load registered, so a later
            // registry restore cannot drop them while `loaded_modules` still
            // claims the module is loaded.
            //
            // The delta is taken BEFORE `import_module`, and that timing is what
            // separates the two kinds of `GLOBAL::` alias:
            //
            //  - one installed while the module's OWN body ran (its `use
            //    NativeCall`, its `use Inner`) is in the delta. It is lexical to
            //    *this module*, which stays loaded, so it has to survive any
            //    scope the load happened to sit inside. Dropping it left a module
            //    first loaded inside an `EVAL` -- `Test`'s `use-ok` is
            //    `EVAL ( "use $code" )` -- unable to resolve its own imports ever
            //    after, because `loaded_modules` still claimed it was loaded and
            //    the later real `use` short-circuited. Measured against rakudo:
            //    after `EVAL 'use Outer; 1'`, an outer `use Outer; outer-probe()`
            //    works, so the module and its imports persist.
            //  - one `import_module` installs for the IMPORTING scope is added
            //    after this delta and so is excluded, keeping it lexical to that
            //    scope: `{ use Foo } EVAL('foo()')` still dies
            //    (roast S11-modules/lexical.t).
            let module_funcs: Vec<Symbol> = self
                .registry()
                .functions
                .keys()
                .filter(|k| !func_keys_before.contains(k))
                .filter(|k| crate::qualified::is_qualified_str(k.as_str()))
                .copied()
                .collect();
            crate::runtime::cow_table_mut(&mut self.module.module_registered_functions)
                .extend(module_funcs);
            self.record_module_registered_protos(&proto_keys_before);
            // Same for the module's `our` package variables, which live in `env`
            // rather than the routine registry. Collected BEFORE `import_module`
            // so the bare aliases it installs — lexical to the importing scope —
            // are excluded, exactly as for the routines above.
            //
            // Also keep the module's OWN bare package-name binding (e.g. a
            // `unit module Foo;` binds bare "Foo" in env): a `::`-only filter
            // dropped it, so a module first loaded from inside a nested scope
            // that discards its env overlay on return (a sub call wrapping an
            // `EVAL`, e.g. `Test`'s `use-ok`) came back with `loaded_modules`
            // still claiming it loaded but its own name unresolvable ever
            // after, since a later re-`use` is a no-op that only reinstates
            // what THIS set records (#7806).
            let package_globals: Vec<(Symbol, Value)> = self
                .env
                .keys()
                .filter(|k| {
                    !env_snapshot.contains(k)
                        && (crate::qualified::is_qualified_str(k.as_str()) || k.resolve() == module)
                })
                .filter_map(|k| self.env.get_sym(*k).map(|v| (*k, v.clone())))
                .collect();
            if !package_globals.is_empty() {
                crate::runtime::cow_table_mut(&mut self.module.module_package_globals)
                    .entry(module.to_string())
                    .or_default()
                    .extend(package_globals);
            }
            let import_tags = if self.module.module_export_defs.contains_key(module) {
                Self::export_hook_ordinary_tags(tags)
            } else {
                tags.to_vec()
            };
            // The module body ran in the loading scope's env, and its `is
            // export` sigil-less constants are left there for `import_module`
            // (`collect_unit_package_scope_names` skips them). A BEGIN-time
            // preload imports nothing, so those term bindings would otherwise
            // be visible to the whole mainline ahead of the nested `use` that
            // asked for them — and, as a live term binding, shadow a
            // same-named type there (#9963). The in-position `use` installs
            // them from the export tables.
            if !import {
                let leaked_terms: Vec<Symbol> = self
                    .env
                    .keys()
                    .filter(|k| {
                        !env_snapshot.contains(k)
                            && crate::runtime::term_names::term_spelling(&k.resolve()).is_some()
                    })
                    .copied()
                    .collect();
                for key in leaked_terms {
                    self.env.remove_sym(key);
                }
            }
            if import
                && let Err(err) = self.import_module(module, &import_tags)
                && !err.message.starts_with("No exports found for module:")
            {
                return Err(err);
            }
        }
        result
    }
}
