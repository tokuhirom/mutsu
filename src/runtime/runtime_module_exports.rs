use super::*;

impl Interpreter {
    /// Record a trait-modified routine value for an exported sub, so that
    /// `import_module` can restore the `&name` env binding with the role mixed in.
    /// Install an imported multi candidate under `target_key`, merging with
    /// candidates other modules already installed there: strip any `__mN`
    /// suffix to the base key, then walk base, `__m1`, `__m2`, ... and insert
    /// at the first vacant slot. A slot already holding this exact `Arc`
    /// (same-module re-import) makes the call a no-op.
    fn import_multi_candidate_merged(&mut self, target_key: &str, def: Arc<FunctionDef>) {
        let base = match target_key.rfind("__m") {
            Some(pos)
                if pos + 3 < target_key.len()
                    && target_key[pos + 3..].chars().all(|c| c.is_ascii_digit()) =>
            {
                &target_key[..pos]
            }
            _ => target_key,
        };
        let mut registry = self.registry_mut();
        let funcs = registry.functions_mut();
        let mut idx = 0usize;
        loop {
            let key = if idx == 0 {
                base.to_string()
            } else {
                format!("{}__m{}", base, idx)
            };
            match funcs.entry(Symbol::intern(&key)) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(def);
                    return;
                }
                std::collections::hash_map::Entry::Occupied(entry) => {
                    if Arc::ptr_eq(entry.get(), &def) {
                        return;
                    }
                }
            }
            idx += 1;
        }
    }

    /// Hide the candidate family already visible at target_single before an
    /// imported proto is installed. Raku treats the imported proto as a new
    /// lexical family, while the flat registry otherwise merges its candidates
    /// with an enclosing my sub and lets the older declaration win. Keep the
    /// hidden definitions on the import-scope snapshot so the enclosing family
    /// is restored when the block exits. Further imports in the same scope are
    /// allowed to merge with the first imported family.
    fn shadow_imported_proto_family(&mut self, target_single: &str) {
        let should_shadow = match self.import_scope_stack.last_mut() {
            Some(snapshot) => snapshot
                .shadowed_proto_names
                .insert(target_single.to_string()),
            None => false,
        };
        if !should_shadow {
            return;
        }
        let prefix = format!("{target_single}/");
        let visible_keys: HashSet<Symbol> = self
            .import_scope_stack
            .last()
            .map(|snapshot| {
                snapshot
                    .functions
                    .iter()
                    .filter(|key| **key == *target_single || key.resolve().starts_with(&prefix))
                    .copied()
                    .collect()
            })
            .unwrap_or_default();
        let keys: Vec<Symbol> = self
            .registry()
            .functions
            .keys()
            .filter(|key| visible_keys.contains(key))
            .copied()
            .collect();
        let mut shadowed_functions = HashMap::new();
        for key in keys {
            if let Some(def) = self.registry_mut().functions_mut().remove(&key) {
                shadowed_functions.insert(key, def);
            }
        }
        let proto_key = Symbol::intern(target_single);
        let proto_was_visible = self
            .import_scope_stack
            .last()
            .is_some_and(|snapshot| snapshot.proto_functions.contains(&proto_key));
        let shadowed_proto = self
            .registry_mut()
            .proto_functions_mut()
            .remove(&proto_key)
            .filter(|_| proto_was_visible);
        if let Some(snapshot) = self.import_scope_stack.last_mut() {
            snapshot.shadowed_functions.extend(shadowed_functions);
            if let Some(def) = shadowed_proto {
                snapshot.shadowed_proto_functions.insert(proto_key, def);
            }
        }
        self.fn_resolve_gen += 1;
    }

    pub(crate) fn record_exported_sub_value(&mut self, package: String, name: String, val: Value) {
        crate::runtime::cow_table_mut(&mut self.exported_sub_values)
            .entry(package)
            .or_default()
            .insert(name, val);
    }

    /// Capture an `is export` regex declarator's bodies so `import_module` can
    /// re-install them under the importing package. Keyed by the module being
    /// loaded (any kind: unit, package-block or bare file), matching how
    /// `register_exported_sub` attributes an export to its owner. Outside a
    /// module load there is nothing to import into, so nothing is recorded.
    pub(crate) fn record_exported_token_defs(
        &mut self,
        name: &str,
        defs: Vec<std::sync::Arc<FunctionDef>>,
    ) {
        let Some(owner) = self.module_load_stack.last().cloned() else {
            return;
        };
        crate::runtime::cow_table_mut(&mut self.exported_token_defs)
            .entry(owner)
            .or_default()
            .insert(name.to_string(), defs);
    }

    pub(crate) fn register_exported_sub(
        &mut self,
        package: String,
        name: String,
        mut tags: Vec<String>,
    ) {
        if tags.is_empty() {
            tags.push("DEFAULT".to_string());
        }
        // Register EXPORT namespace aliases so that EXPORT::TAG::name and
        // Package::EXPORT::TAG::name resolve via normal function lookup.
        let fq_key = format!("{}::{}", package, name);
        let fq_sym = crate::symbol::Symbol::intern(&fq_key);
        // Hoist the clone to a `let` so the read guard drops before the
        // registry_mut writes below (read->write on the same lock deadlocks).
        let def = self.registry().functions.get(&fq_sym).cloned();
        if let Some(def) = def {
            for tag in &tags {
                // Bare EXPORT::TAG::name (accessible from the same package)
                let bare_export = format!("EXPORT::{}::{}", tag, name);
                self.registry_mut()
                    .functions_mut()
                    .entry(crate::symbol::Symbol::intern(&bare_export))
                    .or_insert_with(|| def.clone());
                // Fully-qualified Package::EXPORT::TAG::name
                let pkg_export = format!("{}::EXPORT::{}::{}", package, tag, name);
                self.registry_mut()
                    .functions_mut()
                    .entry(crate::symbol::Symbol::intern(&pkg_export))
                    .or_insert_with(|| def.clone());
            }
            // Always register under EXPORT::ALL::name
            if !tags.contains(&"ALL".to_string()) {
                let bare_all = format!("EXPORT::ALL::{}", name);
                self.registry_mut()
                    .functions_mut()
                    .entry(crate::symbol::Symbol::intern(&bare_all))
                    .or_insert_with(|| def.clone());
                let pkg_all = format!("{}::EXPORT::ALL::{}", package, name);
                self.registry_mut()
                    .functions_mut()
                    .entry(crate::symbol::Symbol::intern(&pkg_all))
                    .or_insert_with(|| def);
            }
        }
        // Mirror this export into the unit-module export table so that
        // `import_module` can validate tags for `unit module X` files whose
        // runtime package registration used "GLOBAL".
        if let Some(unit_mod) = self.unit_module_loading_stack.last().cloned() {
            let mirror = crate::runtime::cow_table_mut(&mut self.unit_module_exported_subs)
                .entry(unit_mod)
                .or_default()
                .entry(name.clone())
                .or_default();
            for tag in &tags {
                mirror.insert(tag.clone());
            }
        }
        // Attribute this export to the module currently being loaded (any kind:
        // unit, package-block, or bare-file). The `use MOD` tag-filter uses this
        // to hide only MOD's own exports, never a symbol MOD imported from a
        // transitively-`use`d module.
        if let Some(owner) = self.module_load_stack.last().cloned() {
            let owned = crate::runtime::cow_table_mut(&mut self.module_owned_exports)
                .entry(owner)
                .or_default()
                .entry(name.clone())
                .or_default();
            for tag in &tags {
                owned.insert(tag.clone());
            }
        }
        let entry = crate::runtime::cow_table_mut(&mut self.exported_subs)
            .entry(package)
            .or_default()
            .entry(name)
            .or_default();
        for tag in tags {
            entry.insert(tag);
        }
    }

    pub(crate) fn register_exported_var(
        &mut self,
        package: String,
        name: String,
        mut tags: Vec<String>,
    ) {
        if tags.is_empty() {
            tags.push("DEFAULT".to_string());
        }
        let entry = crate::runtime::cow_table_mut(&mut self.exported_vars)
            .entry(package)
            .or_default()
            .entry(name)
            .or_default();
        for tag in tags {
            entry.insert(tag);
        }
    }

    /// Publish `NativeCall`'s export list.
    ///
    /// mutsu implements NativeCall inside the VM, so `use NativeCall` loads no
    /// Raku module — but the module's *export surface* is introspectable in
    /// Rakudo (`NativeCall::EXPORT::ALL`) and real bindings read it: `NativeLibs`
    /// re-exports the whole stash into its own `UNIT::EXPORT` so that its users
    /// get NativeCall transitively. With no entries, that stash was empty and
    /// the re-export silently did nothing.
    ///
    /// The list is Rakudo's `NativeCall.rakumod` / `NativeCall::Types` export
    /// set: the trait that makes a sub native, the five DEFAULT helper
    /// routines, the TEST-only library-name helper, and the C type objects.
    ///
    /// A module loaded from source populates `exported_subs` as its `is export`
    /// declarations are registered, and `package_stash_value` builds
    /// `Mod::EXPORT`/`Mod::EXPORT::<tag>` stashes from that table. NativeCall
    /// runs no such declarations, so without this its table stayed empty and
    /// `::("NativeCall::EXPORT::DEFAULT::&nativecast")` answered "No such
    /// symbol" where raku resolves it.
    pub(crate) fn register_nativecall_exports(&mut self) {
        const SUBS: [&str; 6] = [
            "trait_mod:<is>",
            "nativecast",
            "nativesizeof",
            "cglobal",
            "explicitly-manage",
            "refresh",
        ];
        const TYPES: [&str; 11] = [
            "Pointer",
            "OpaquePointer",
            "CArray",
            "void",
            "bool",
            "long",
            "longlong",
            "ulong",
            "ulonglong",
            "size_t",
            "ssize_t",
        ];
        if self.exported_subs.contains_key("NativeCall") {
            return;
        }
        for name in SUBS {
            self.register_exported_sub("NativeCall".to_string(), name.to_string(), Vec::new());
        }
        // guess_library_name is a TEST-only routine in Rakudo. It is kept out
        // of DEFAULT so use NativeCall :TEST is the import that exposes it.
        self.register_exported_sub(
            "NativeCall".to_string(),
            "guess_library_name".to_string(),
            vec!["TEST".to_string()],
        );
        for name in TYPES {
            self.register_exported_var("NativeCall".to_string(), name.to_string(), Vec::new());
        }
        // `::('NativeCall')` must resolve to the package rather than failing:
        // `NativeLibs`' own `EXPORT` sub passes `NativeCall` through as a value.
        self.env.insert(
            "NativeCall".to_string(),
            Value::package(Symbol::intern("NativeCall")),
        );
    }

    pub(crate) fn import_module(
        &mut self,
        module: &str,
        tags: &[String],
    ) -> Result<(), RuntimeError> {
        let requested: HashSet<String> = if tags.is_empty() {
            ["DEFAULT".to_string()].into_iter().collect()
        } else {
            tags.iter().cloned().collect()
        };
        let import_all = requested.contains("ALL");

        let subs = self.exported_subs.get(module).cloned().unwrap_or_default();
        let vars = self.exported_vars.get(module).cloned().unwrap_or_default();
        // For `unit module Foo`, sub registration at runtime may have used
        // the default "GLOBAL" package (because the interpreter's runtime
        // `current_package` is not switched by the compile-time unit
        // declaration). When a module declared the `unit_module` marker,
        // its exports are tracked separately so we can still validate tags
        // and report X::Import::NoSuchTag correctly.
        let unit_global_subs: HashMap<String, HashSet<String>> = self
            .unit_module_exported_subs
            .get(module)
            .cloned()
            .unwrap_or_default();
        // A bare-file module (no `unit module`/package block) registers its
        // exports only under GLOBAL, so `exported_subs[module]` is empty; its
        // owned exports (name -> tags) live in `module_owned_exports`. Consult
        // that too, so a later `use MOD :tag` (after a plain `use MOD` hid the
        // tagged exports by renaming them to `MOD::name`) can re-import them.
        let owned_subs: HashMap<String, HashSet<String>> = self
            .module_owned_exports
            .get(module)
            .cloned()
            .unwrap_or_default();
        if subs.is_empty()
            && vars.is_empty()
            && unit_global_subs.is_empty()
            && owned_subs.is_empty()
        {
            return Err(RuntimeError::new(format!(
                "No exports found for module: {}",
                module
            )));
        }

        // Validate that all requested tags actually exist in the module's exports.
        if !tags.is_empty() && !import_all {
            // Collect all known tags from the module
            let mut known_tags: HashSet<String> = HashSet::new();
            known_tags.insert("DEFAULT".to_string());
            known_tags.insert("ALL".to_string());
            known_tags.insert("MANDATORY".to_string());
            for symbol_tags in subs.values() {
                for tag in symbol_tags {
                    known_tags.insert(tag.clone());
                }
            }
            for symbol_tags in vars.values() {
                for tag in symbol_tags {
                    known_tags.insert(tag.clone());
                }
            }
            for symbol_tags in unit_global_subs.values() {
                for tag in symbol_tags {
                    known_tags.insert(tag.clone());
                }
            }
            for symbol_tags in owned_subs.values() {
                for tag in symbol_tags {
                    known_tags.insert(tag.clone());
                }
            }
            for tag in &requested {
                if !known_tags.contains(tag) {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("source-package".to_string(), Value::str(module.to_string()));
                    attrs.insert("tag".to_string(), Value::str(tag.clone()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Error while importing from '{}': no such tag '{}' declared",
                            module, tag
                        )),
                    );
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Import::NoSuchTag"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(format!(
                        "Error while importing from '{}': no such tag '{}' declared",
                        module, tag
                    ));
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }

        // Import into the current package scope so that `use Foo` inside
        // `module Bar { }` makes Foo's exports available as `Bar::name`
        // rather than polluting the GLOBAL namespace.
        let target_pkg = self.current_package();

        // For a `unit module Foo`, the actual exports live in
        // `unit_module_exported_subs[Foo]` (runtime registration used the
        // GLOBAL package), while `exported_subs[Foo]` is empty. Merge both so a
        // re-import (`use Foo; use Foo :tag`) of a unit-module export is
        // actually re-installed, not just tag-validated.
        let mut merged_subs = subs;
        for (name, tags) in unit_global_subs.iter() {
            merged_subs
                .entry(name.clone())
                .or_default()
                .extend(tags.iter().cloned());
        }
        // Bare-file module exports (see `owned_subs` above). Their source
        // functions live under `MOD::name` (a plain `use` renamed the hidden
        // tagged ones there; DEFAULT ones stay `GLOBAL::name` and are already
        // imported, so the re-alias below is a harmless no-op for them).
        for (name, tags) in owned_subs.iter() {
            merged_subs
                .entry(name.clone())
                .or_default()
                .extend(tags.iter().cloned());
        }

        for (name, symbol_tags) in merged_subs {
            // MANDATORY exports are always imported regardless of requested tags
            let is_mandatory = symbol_tags.contains("MANDATORY");
            if !import_all && !is_mandatory && symbol_tags.is_disjoint(&requested) {
                continue;
            }
            // An imported operator sub (e.g. `method infix:<as> is export`'s
            // sub form) must be visible to the EVAL parser so code parsed at
            // runtime recognizes the new operator symbol.
            if matches!(
                name.split_once(":<").map(|(c, _)| c),
                Some("prefix" | "postfix" | "infix" | "circumfix" | "postcircumfix")
            ) {
                crate::runtime::cow_table_mut(&mut self.imported_operator_names)
                    .insert(name.clone());
            }
            if name.starts_with("infix:<") {
                // An EXPORTED operator becomes lexically visible in whatever
                // unit imported it, so it carries no declaring-file
                // restriction (empty set == visible everywhere). Force the
                // set empty rather than filling it in only when absent: see
                // the matching comment in
                // `runtime_module_export_sub.rs::install_export_symbol`
                // (#8008) — the declaring module's own decl-time entry is
                // never absent by the time export runs.
                crate::runtime::cow_table_mut(&mut self.user_declared_infix_ops)
                    .insert(name.clone(), HashSet::new());
                crate::vm::vm_jit::note_user_infix_decl();
            }
            let source_single = format!("{module}::{name}");
            let source_prefix = format!("{module}::{name}/");
            let target_single = format!("{target_pkg}::{name}");
            let target_prefix = format!("{target_pkg}::{name}/");
            let imported_proto = self
                .registry()
                .proto_functions
                .contains_key(&Symbol::intern(&source_single))
                || (unit_global_subs.contains_key(&name)
                    && self
                        .registry()
                        .proto_functions
                        .contains_key(&Symbol::intern(&format!("GLOBAL::{name}"))));
            if imported_proto {
                self.shadow_imported_proto_family(&target_single);
            }

            let mut function_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter_map(|(k, v)| {
                    let ks = k.resolve();
                    if ks == source_single {
                        Some((Symbol::intern(&target_single), v.clone()))
                    } else if ks.starts_with(&source_prefix) {
                        Some((
                            Symbol::intern(&ks.replacen(&source_prefix, &target_prefix, 1)),
                            v.clone(),
                        ))
                    } else {
                        None
                    }
                })
                .collect();
            // Fallback for a re-import (`use Foo; use Foo :tag`) of a `unit
            // module Foo` sub: the sub is registered under `GLOBAL::name`, not
            // `Foo::name`, and an earlier default `use Foo` already stripped the
            // `GLOBAL::name` alias for a non-DEFAULT export. Restore it from the
            // stable `EXPORT::ALL::name` alias (registered for every non-ALL
            // export) so the tagged re-import makes the sub callable again.
            if function_entries.is_empty() {
                for alias in [
                    format!("{module}::EXPORT::ALL::{name}"),
                    format!("GLOBAL::EXPORT::ALL::{name}"),
                    format!("EXPORT::ALL::{name}"),
                ] {
                    if let Some(def) = self
                        .registry()
                        .functions
                        .get(&Symbol::intern(&alias))
                        .cloned()
                    {
                        function_entries.push((Symbol::intern(&target_single), def));
                        break;
                    }
                }
            }
            for (k, v) in function_entries {
                let ks = k.resolve();
                if ks.contains('/') {
                    // A multi candidate. Two modules exporting candidates of the
                    // same multi (JSON::OptIn / JSON::Name / JSON::Class each
                    // export a `trait_mod:<is>`) land on the SAME target key
                    // (`GLOBAL::trait_mod:<is>/2`), so a plain insert would let
                    // the last `use` overwrite every earlier module's
                    // candidates. Chain into the first free `__mN` slot
                    // instead, and skip candidates already installed (a
                    // re-import of the same module must stay idempotent).
                    self.import_multi_candidate_merged(&ks, v);
                } else {
                    self.registry_mut().functions_mut().insert(k, v);
                }
            }
            // Function set changed: invalidate the name-keyed resolution caches
            // (multi_candidates_cache / fn_keys_by_base).
            self.fn_resolve_gen += 1;

            let proto_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
                .registry()
                .proto_functions
                .iter()
                .filter_map(|(k, v)| {
                    if *k == *source_single
                        || (unit_global_subs.contains_key(&name)
                            && *k == Symbol::intern(&format!("GLOBAL::{name}")))
                    {
                        Some((Symbol::intern(&target_single), v.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            for (k, v) in proto_entries {
                self.registry_mut().proto_functions_mut().insert(k, v);
            }
            // `has_proto` consults the `proto_subs` name set, not just
            // `proto_functions`, so an imported proto has to be recorded there
            // under the importing package too. (Before unit-module routines
            // registered under their own package, a module's proto was already
            // in `proto_subs` as `GLOBAL::name` and this was invisible.)
            if imported_proto {
                self.registry_mut().proto_subs_insert(target_single.clone());
            }

            // A `token`/`rule`/`regex` marked `is export` lives in
            // `Registry::token_defs`, not `functions`, and a lexical one is
            // dropped when the module's scope exits — so it is re-installed
            // here from the defs captured at declaration time. Registering it
            // under the importing package is what makes `&name` (a lazy
            // by-name Routine) and `<name>` inside the importer's own regexes
            // both resolve.
            if let Some(defs) = self
                .exported_token_defs
                .get(module)
                .and_then(|m| m.get(&name))
                .cloned()
            {
                self.registry_mut()
                    .token_defs
                    .insert(Symbol::intern(&target_single), defs);
                crate::runtime::regex_parse::TOKEN_DEFS_GEN
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }

            // If this exported sub carried a trait-modified value (e.g. a role
            // mixed in via a custom `is` trait), restore it as the `&name` env
            // binding so `&name ~~ Role` works after import.
            if let Some(val) = self
                .exported_sub_values
                .get(module)
                .and_then(|m| m.get(&name))
                .cloned()
            {
                let bare_key = format!("&{name}");
                let qualified_key = format!("&{target_pkg}::{name}");
                self.record_import_env_key(&bare_key);
                self.record_import_env_key(&qualified_key);
                self.env.insert(bare_key, val.clone());
                self.env.insert(qualified_key, val);
            }
        }

        for (name, symbol_tags) in vars {
            let is_mandatory = symbol_tags.contains("MANDATORY");
            if !import_all && !is_mandatory && symbol_tags.is_disjoint(&requested) {
                continue;
            }
            let (source, target) = if let Some(sigil) = name.chars().next()
                && matches!(sigil, '$' | '@' | '%' | '&')
            {
                let bare = &name[1..];
                (format!("{sigil}{module}::{bare}"), name.clone())
            } else {
                (format!("{module}::{name}"), name.clone())
            };
            if let Some(value) = self.env.get(&source).cloned() {
                // An imported ENUM KEY is a package symbol/term, not a `$`-scalar,
                // so it goes into the enum-key namespace rather than under its own
                // plain `env` key — which, being sigil-less, is where a same-named
                // `my $s` lives (#7914). Importing `:s<time>` from
                // `CSS::Grammar::Defs` otherwise replaced the importing scope's
                // `$s` wholesale. See `runtime::enum_bare_names`.
                let is_enum_key = !target.contains("::")
                    && !target.starts_with(['$', '@', '%', '&'])
                    && matches!(value.view(), ValueView::Enum { .. });
                let env_target = if is_enum_key {
                    crate::runtime::enum_bare_names::enum_bare_key_for_insert(&target)
                } else {
                    target.clone()
                };
                if !target.contains("::") {
                    self.unsuppress_name(&target);
                }
                // Slice F (env<->locals coherence): `import` writes the symbol
                // into env by name, but a later bare reference (e.g. an imported
                // `constant c`) may read a stale caller local slot when the
                // reverse env->locals pull is disabled. Record the imported
                // name (sigil stripped to match the local-slot key) so the
                // ImportModule opcode writes it through to the caller slot.
                //
                // An enum key is skipped: it names no caller local slot, and
                // recording it made the importing frame pull `env[<key>]` over a
                // same-named lexical's slot on the next frame reconcile — the
                // half of #7914 that turned the caller's `my $s` into `Any`.
                if !target.contains("::") && !is_enum_key {
                    let slot_name = match target.chars().next() {
                        Some('$' | '@' | '%') => target[1..].to_string(),
                        _ => target.clone(),
                    };
                    self.pending_rw_writeback_sources.push(slot_name);
                }
                // Part of the LOADING module's own lexical scope, whether or not
                // it is new to `env` (see `module_imported_names`).
                if !self.module_load_stack.is_empty() && !target.contains("::") {
                    let previous = self.env.get(&env_target).cloned();
                    self.module_imported_names
                        .push((env_target.clone(), value.clone(), previous));
                }
                self.record_import_env_key(&env_target);
                self.env.insert(env_target, value);
            }
        }
        Ok(())
    }

    /// Load a module without importing its exports (Raku `need` keyword).
    pub(crate) fn need_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        let is_nested_need = !self.module_load_stack.is_empty();
        if self.loaded_modules.contains(module) {
            return Ok(());
        }
        if self.module_load_stack.iter().any(|m| m == module) {
            let mut chain = self.module_load_stack.clone();
            chain.push(module.to_string());
            return Err(RuntimeError::new(format!(
                "circular module dependency detected: {}",
                chain.join(" -> ")
            )));
        }
        self.module_load_stack.push(module.to_string());
        let class_snapshot: HashSet<String> = self.registry().classes.keys().cloned().collect();
        let env_snapshot: HashSet<Symbol> = self.env.keys().copied().collect();
        let saved = self.suppress_exports;
        self.suppress_exports = true;
        let result = self.load_module(module);
        self.suppress_exports = saved;
        self.module_load_stack.pop();
        if result.is_ok() {
            let short_name = if let Some((_, short)) = module.rsplit_once("::") {
                short.to_string()
            } else {
                module.to_string()
            };
            let class_names: Vec<String> = self.registry().classes.keys().cloned().collect();
            for class_name in &class_names {
                if !class_snapshot.contains(class_name) {
                    crate::runtime::cow_table_mut(&mut self.need_hidden_classes)
                        .insert(class_name.clone());
                    if let Some((_, short)) = class_name.rsplit_once("::") {
                        crate::runtime::cow_table_mut(&mut self.need_hidden_classes)
                            .insert(short.to_string());
                    }
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
                if is_nested_need || key_short != short_name {
                    crate::runtime::cow_table_mut(&mut self.need_hidden_classes)
                        .insert(key_s.clone());
                    crate::runtime::cow_table_mut(&mut self.need_hidden_classes)
                        .insert(key_short.to_string());
                }
            }
            if is_nested_need {
                crate::runtime::cow_table_mut(&mut self.need_hidden_classes)
                    .insert(short_name.clone());
            }
            crate::runtime::cow_table_mut(&mut self.loaded_modules).insert(module.to_string());
        }
        result
    }

    pub(crate) fn no_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        if module == "strict" {
            self.strict_mode = false;
        } else if module == "fatal" {
            self.fatal_mode = false;
        }
        Ok(())
    }
}
