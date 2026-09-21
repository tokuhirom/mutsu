use super::*;

impl Interpreter {
    /// Record a trait-modified routine value for an exported sub, so that
    /// `import_module` can restore the `&name` env binding with the role mixed in.
    /// Install an imported multi candidate under `target_key`, merging with
    /// candidates other modules already installed there: strip any `__mN`
    /// suffix to the base key, then walk base, `__m1`, `__m2`, ... and insert
    /// at the first vacant slot. A slot already holding this exact `Arc`
    /// (same-module re-import) makes the call a no-op.
    fn import_multi_candidate_merged(&mut self, target_key: &str, def: Arc<FunctionDef>) -> Symbol {
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
                    let key = *entry.key();
                    entry.insert(def);
                    return key;
                }
                std::collections::hash_map::Entry::Occupied(entry) => {
                    if Arc::ptr_eq(entry.get(), &def) {
                        return *entry.key();
                    }
                }
            }
            idx += 1;
        }
    }

    /// The export tag a package name denotes when it names a module's export
    /// stash: `EXPORT::DEFAULT` -> `DEFAULT`, `Foo::EXPORT::ALL` -> `ALL`.
    /// Any other package (including a deeper `EXPORT::A::B`) is not one.
    ///
    /// `pub(crate)`: also consulted by `vm_register_sub_ops.rs`, which is a
    /// different module tree, to implicitly export an `our sub`/`our multi
    /// sub` declared directly inside such a package (see
    /// `exec_register_sub_op`'s `__our_scoped` branch).
    pub(crate) fn export_stash_tag(package: &str) -> Option<&str> {
        let tag = match package.strip_prefix("EXPORT::") {
            Some(tag) => tag,
            None => package.split_once("::EXPORT::")?.1,
        };
        (!tag.is_empty() && !tag.contains("::")).then_some(tag)
    }

    /// Publish a routine bound through an `OUR::` code stash entry.
    ///
    /// A binding such as `OUR::{'&trait_mod:<is>'} := &trait_mod:<is>` creates
    /// the code value in the package stash, but the normal name-based dispatcher
    /// still searches `Registry::functions`.  The RHS is a materialized routine,
    /// so copy the currently visible definitions (every candidate, for a multi)
    /// into the package's registry namespace as well. Keep those aliases in the
    /// persistent `our_scoped_functions` table: an import scope may otherwise
    /// remove the temporary `GLOBAL::` aliases installed while the binding's
    /// source module was loaded, making the stash entry callable but invisible
    /// to later users.
    ///
    /// When the binding lands in a module's own export stash — the re-export
    /// idiom `my package EXPORT::DEFAULT { OUR::{'&name'} := &name }`, which
    /// `JSON::Class` uses to re-export `JSON::Marshal`'s attribute traits — the
    /// stash contents ARE that module's export list, so the aliases go under the
    /// loading module's name and the routine is recorded as one of its exports.
    /// Without that, `use`-ing the re-exporting module imported nothing at all.
    pub(crate) fn register_our_code_alias(&mut self, name: &str, value: &Value) {
        let ValueView::Sub(data) = value.view() else {
            return;
        };
        let is_multi = data.env.contains_key("__mutsu_multi_dispatch_candidates");
        let Some(name) = name.strip_prefix("&OUR::") else {
            return;
        };
        if name.is_empty() {
            return;
        }
        let current_pkg = self.current_package();
        // `module_load_stack` names the compunit being loaded, which is the
        // namespace `import_module` reads an export back out of.
        let export_target = match (
            Self::export_stash_tag(&current_pkg),
            self.module_load_stack.last(),
        ) {
            (Some(tag), Some(module)) => Some((module.clone(), tag.to_string())),
            _ => None,
        };
        if !is_multi && export_target.is_none() {
            return;
        }
        let target_pkg = match &export_target {
            Some((module, _)) => module.clone(),
            None => current_pkg,
        };

        let candidates = if is_multi {
            self.resolve_all_multi_candidates(name)
        } else {
            Vec::new()
        };
        if is_multi && candidates.is_empty() {
            return;
        }
        let source_packages = self.bare_name_packages();
        let entries: Vec<(String, Arc<FunctionDef>)> = if is_multi {
            let source_prefixes: Vec<String> = source_packages
                .iter()
                .map(|pkg| format!("{pkg}::{name}/"))
                .collect();
            self.registry()
                .functions
                .iter()
                .filter_map(|(key, def)| {
                    let key_str = key.as_str();
                    let source_prefix = source_prefixes
                        .iter()
                        .find(|prefix| key_str.starts_with(prefix.as_str()))?;
                    if !candidates
                        .iter()
                        .any(|candidate| Arc::ptr_eq(candidate, def))
                    {
                        return None;
                    }
                    let suffix = key_str.strip_prefix(source_prefix)?;
                    Some((format!("{target_pkg}::{name}/{suffix}"), def.clone()))
                })
                .collect()
        } else {
            // A single routine has one registry entry, under whichever
            // enclosing package the binding's source resolved in.
            source_packages
                .iter()
                .find_map(|pkg| {
                    let def = self
                        .registry()
                        .functions
                        .get(&Symbol::intern(&format!("{pkg}::{name}")))?
                        .clone();
                    Some(vec![(format!("{target_pkg}::{name}"), def)])
                })
                .unwrap_or_default()
        };

        let mut changed = false;
        for (target_key, def) in entries {
            let installed_key = if target_key.contains('/') {
                self.import_multi_candidate_merged(&target_key, def.clone())
            } else {
                let key = Symbol::intern(&target_key);
                self.registry_mut().functions_mut().insert(key, def.clone());
                key
            };
            self.registry_mut()
                .our_scoped_functions
                .insert(installed_key, def);
            crate::runtime::cow_table_mut(&mut self.module_registered_functions)
                .insert(installed_key);
            changed = true;
        }
        if changed {
            self.invalidate_fn_resolution();
            if let Some((module, tag)) = export_target {
                self.register_exported_sub(module, name.to_string(), vec![tag]);
            }
        }
    }

    /// Publish a symbol bound through the `OUR::` pseudo-stash.
    ///
    /// `OUR::` names the CURRENT package's own symbol table, so `OUR::<&f> :=
    /// ...` inside `package Foo { }` binds `Foo::f` -- the same slot `our &f
    /// := ...` writes, and the one `Foo::f()` calls. mutsu stored the binding
    /// under the literal env key `&OUR::f` instead, which read back only
    /// through the identical spelling: `Foo::f()` answered "Could not find
    /// symbol '&f' in 'Foo'" and the scalar form `OUR::<$x> := 1` was simply
    /// lost, because `our_pseudo_var_read` resolves a read against the current
    /// package and so never looked where the write had landed.
    ///
    /// The case that matters in practice is the generated-export idiom, where
    /// the enclosing package is a module's export stash:
    ///
    /// ```raku
    /// my package EXPORT::DEFAULT {
    ///     for @tags -> $tag {
    ///         OUR::{'&' ~ $tag} := sub (*@inners) { do-regular-tag($tag, @inners) }
    ///     }
    /// }
    /// ```
    ///
    /// That is how `Air::Functional` exports one sub per HTML tag (`h3`, `p`,
    /// `article`, ...), and none of them existed under mutsu. It cannot go
    /// through [`Self::register_our_code_alias`], which aliases an *existing*
    /// named routine by copying its `FunctionDef`: these are fresh closures
    /// over the loop variable, so there is no `FunctionDef` to find and the
    /// captured `$tag` is exactly what must be preserved. The closure value
    /// itself is published instead, as the module's exported symbol -- the
    /// same representation an `our &f is export = sub { ... }` already uses.
    pub(crate) fn publish_our_pseudo_stash_symbol(&mut self, name: &str, value: &Value) {
        // The compiler's pseudo-var spelling: a scalar arrives sigil-less
        // (`OUR::x`), every other sigil leads (`&OUR::f`, `@OUR::a`, `%OUR::h`).
        let (sigil, rest) = match name.as_bytes().first() {
            Some(b'&' | b'@' | b'%') => (&name[..1], &name[1..]),
            _ => ("", name),
        };
        let Some(bare) = rest.strip_prefix("OUR::") else {
            return;
        };
        // A nested name is a package path, not a symbol of THIS package.
        if bare.is_empty() || bare.contains("::") {
            return;
        }
        let package = self.current_package();
        let qualified = if package.is_empty() || package == "GLOBAL" {
            format!("{sigil}{bare}")
        } else {
            format!("{sigil}{package}::{bare}")
        };
        // `our_vars` is the durable package store a qualified read consults
        // after the declaring block's env entry is gone; `env` serves the
        // reads that happen while it is still live.
        self.set_our_var(qualified.clone(), value.clone());
        self.env_mut().insert(qualified, value.clone());
        let Some(tag) = Self::export_stash_tag(&package).map(str::to_string) else {
            return;
        };
        let Some(module) = self.module_load_stack.last().cloned() else {
            return;
        };
        // `exported_var_value` reads the sigil-leading spelling, so the
        // module-qualified key is `&Mod::f`, not `Mod::&f`.
        self.env_mut()
            .insert(format!("{sigil}{module}::{bare}"), value.clone());
        self.register_exported_var(module, format!("{sigil}{bare}"), vec![tag]);
    }

    /// Register a value assigned directly into an `EXPORT::<tag>` stash.
    ///
    /// Modules such as Interval use `BEGIN EXPORT::refine::<DateTime> :=
    /// Interval` rather than an `is export` declaration.  The assignment
    /// creates the stash entry, but the import table still needs the module's
    /// exported-variable metadata and a durable module-qualified value.
    pub(crate) fn register_manual_export_var(&mut self, target: &str, value: &Value) {
        let Some((tag, name)) = (if let Some(rest) = target.strip_prefix("EXPORT::") {
            rest.split_once("::")
        } else if let Some((_, rest)) = target.split_once("::EXPORT::") {
            rest.split_once("::")
        } else {
            None
        }) else {
            return;
        };
        if tag.is_empty() || name.is_empty() || tag.contains("::") || name.contains("::") {
            return;
        }
        let Some(module) = self.module_load_stack.last().cloned() else {
            return;
        };
        self.env_mut()
            .insert(format!("{module}::{name}"), value.clone());
        self.register_exported_var(module, name.to_string(), vec![tag.to_string()]);
    }

    /// Companion to [`Self::register_our_code_alias`] for the plainer half of
    /// the same "manual EXPORT stash" idiom: an ordinary `our sub`/
    /// `our multi sub` declared directly inside `my package EXPORT::<tag>
    /// { ... }`, rather than an `OUR::{'&name'} := &name` re-export binding.
    /// `exec_register_sub_op` calls this right after installing such a sub,
    /// passing the bare name it registered under `current_package()` (e.g.
    /// `EXPORT::DEFAULT::infix:<< ip== >>` for `Net::IP::Parse`).
    ///
    /// A no-op unless `current_package()` actually names an export stash and
    /// a module is currently loading — the common case of an `our sub`
    /// declared in some other nested package.
    ///
    /// `import_module` resolves an export by looking up `{module}::{name}`
    /// (see its `source_single`), where `module` is the name on
    /// `module_load_stack` — never the literal `EXPORT::<tag>` package the
    /// sub was actually registered under. So, like `register_our_code_alias`,
    /// this aliases the installed definition(s) to that key before recording
    /// the export; skipping the alias would leave `register_exported_sub`'s
    /// own registry lookup (keyed the same way) empty-handed too.
    pub(crate) fn export_implicit_stash_sub(&mut self, resolved_name: &str, multi: bool) {
        let current_pkg = self.current_package();
        let Some(tag) = Self::export_stash_tag(&current_pkg) else {
            return;
        };
        let tag = tag.to_string();
        let Some(module) = self.module_load_stack.last().cloned() else {
            return;
        };
        let entries: Vec<(String, Arc<FunctionDef>)> = if multi {
            let source_prefix = format!("{current_pkg}::{resolved_name}/");
            self.registry()
                .functions
                .iter()
                .filter_map(|(key, def)| {
                    let suffix = key.resolve().strip_prefix(&source_prefix)?.to_string();
                    Some((format!("{module}::{resolved_name}/{suffix}"), def.clone()))
                })
                .collect()
        } else {
            let source_single = format!("{current_pkg}::{resolved_name}");
            self.registry()
                .functions
                .get(&Symbol::intern(&source_single))
                .cloned()
                .map(|def| vec![(format!("{module}::{resolved_name}"), def)])
                .unwrap_or_default()
        };
        if entries.is_empty() {
            return;
        }
        let mut changed = false;
        for (target_key, def) in entries {
            let installed_key = if target_key.contains('/') {
                self.import_multi_candidate_merged(&target_key, def.clone())
            } else {
                let key = Symbol::intern(&target_key);
                self.registry_mut()
                    .functions_mut()
                    .entry(key)
                    .or_insert_with(|| def.clone());
                key
            };
            self.registry_mut()
                .our_scoped_functions
                .insert(installed_key, def);
            crate::runtime::cow_table_mut(&mut self.module_registered_functions)
                .insert(installed_key);
            changed = true;
        }
        if changed {
            self.invalidate_fn_resolution();
        }
        self.register_exported_sub(module, resolved_name.to_string(), vec![tag]);
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
            // A top-level import has no scope snapshot because its aliases are
            // meant to persist. It still has to replace an already-visible
            // family, for example a bare-file dependency preloaded a
            // `GLOBAL::localtime` family before `Time::localtime` imported its
            // own wrapper.
            None => true,
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
            .unwrap_or_else(|| {
                self.registry()
                    .functions
                    .keys()
                    .filter(|key| **key == *target_single || key.resolve().starts_with(&prefix))
                    .copied()
                    .collect()
            });
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
            .filter(|_| self.import_scope_stack.is_empty() || proto_was_visible);
        if let Some(snapshot) = self.import_scope_stack.last_mut() {
            snapshot.shadowed_functions.extend(shadowed_functions);
            if let Some(def) = shadowed_proto {
                snapshot.shadowed_proto_functions.insert(proto_key, def);
            }
        } else {
            // There is no enclosing registry snapshot to restore at top level:
            // `shadowed_functions` (and `shadowed_proto`) are being thrown away
            // for good, replaced by whatever this import installs instead.
            //
            // `module_registered_functions` is a flat, un-scoped set of
            // registry keys "protected" from `pop_import_scope`'s cleanup
            // because some loaded module's own body once installed them
            // (`load_module_inner`'s `module_funcs` diff, keyed by symbol only
            // — see its comment). It has no notion of a key being reassigned
            // to a different owner. Once shadowed away here, the name no
            // longer names that module's definition — a later, LEXICALLY
            // SCOPED import that happens to reuse the exact same `GLOBAL::`
            // key (e.g. a same-named proto/multi family re-imported inside an
            // exported wrapper whose own name collided with it at load time)
            // must not inherit this now-stale protection, or its call-scoped
            // installs leak past `pop_import_scope` forever (#8798).
            if !shadowed_functions.is_empty() {
                let table = crate::runtime::cow_table_mut(&mut self.module_registered_functions);
                for key in shadowed_functions.keys() {
                    table.remove(key);
                }
            }
            // Replace the old proto marker along with its candidate family.
            self.registry_mut()
                .proto_subs_retain(|key| key != target_single);
        }
        self.invalidate_fn_resolution();
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
        // Multi candidates are stored under an arity-qualified key, so there
        // is no exact `package::name` entry to use for the EXPORT aliases.
        // Snapshot that family before taking mutable registry access. These
        // aliases also let imports recover a family when a distribution's
        // `unit module` name differs from its provided module path.
        let candidate_prefix = format!("{}::{}/", package, name);
        let candidate_defs: Vec<(String, Arc<FunctionDef>)> = if def.is_none() {
            self.registry()
                .functions
                .iter()
                .filter_map(|(key, candidate)| {
                    let key = key.resolve();
                    key.strip_prefix(&candidate_prefix)
                        .map(|suffix| (suffix.to_string(), candidate.clone()))
                })
                .collect()
        } else {
            Vec::new()
        };
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
                if let Some(owner) = self
                    .module_load_stack
                    .last()
                    .filter(|owner| owner.as_str() != package)
                {
                    let owner_export = format!("{}::EXPORT::{}::{}", owner, tag, name);
                    self.registry_mut()
                        .functions_mut()
                        .entry(crate::symbol::Symbol::intern(&owner_export))
                        .or_insert_with(|| def.clone());
                }
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
                    .or_insert_with(|| def.clone());
                if let Some(owner) = self
                    .module_load_stack
                    .last()
                    .filter(|owner| owner.as_str() != package)
                {
                    let owner_all = format!("{}::EXPORT::ALL::{}", owner, name);
                    self.registry_mut()
                        .functions_mut()
                        .entry(crate::symbol::Symbol::intern(&owner_all))
                        .or_insert_with(|| def.clone());
                }
            }
        } else if !candidate_defs.is_empty() {
            for tag in &tags {
                for (suffix, candidate) in &candidate_defs {
                    let bare_export = format!("EXPORT::{}::{}/{}", tag, name, suffix);
                    self.registry_mut()
                        .functions_mut()
                        .entry(crate::symbol::Symbol::intern(&bare_export))
                        .or_insert_with(|| candidate.clone());
                    let pkg_export = format!("{}::EXPORT::{}::{}/{}", package, tag, name, suffix);
                    self.registry_mut()
                        .functions_mut()
                        .entry(crate::symbol::Symbol::intern(&pkg_export))
                        .or_insert_with(|| candidate.clone());
                    if let Some(owner) = self
                        .module_load_stack
                        .last()
                        .filter(|owner| owner.as_str() != package)
                    {
                        let owner_export =
                            format!("{}::EXPORT::{}::{}/{}", owner, tag, name, suffix);
                        self.registry_mut()
                            .functions_mut()
                            .entry(crate::symbol::Symbol::intern(&owner_export))
                            .or_insert_with(|| candidate.clone());
                    }
                }
            }
            if !tags.contains(&"ALL".to_string()) {
                for (suffix, candidate) in &candidate_defs {
                    let bare_all = format!("EXPORT::ALL::{}/{}", name, suffix);
                    self.registry_mut()
                        .functions_mut()
                        .entry(crate::symbol::Symbol::intern(&bare_all))
                        .or_insert_with(|| candidate.clone());
                    let pkg_all = format!("{}::EXPORT::ALL::{}/{}", package, name, suffix);
                    self.registry_mut()
                        .functions_mut()
                        .entry(crate::symbol::Symbol::intern(&pkg_all))
                        .or_insert_with(|| candidate.clone());
                    if let Some(owner) = self
                        .module_load_stack
                        .last()
                        .filter(|owner| owner.as_str() != package)
                    {
                        let owner_all = format!("{}::EXPORT::ALL::{}/{}", owner, name, suffix);
                        self.registry_mut()
                            .functions_mut()
                            .entry(crate::symbol::Symbol::intern(&owner_all))
                            .or_insert_with(|| candidate.clone());
                    }
                }
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
        // The module load stack names the requested compunit path. Keep a
        // second metadata entry under that path when the declared unit package
        // is different, so `use Lingua::EN::Numbers :short` can validate the
        // export even though the file says `unit module Numbers`.
        if self.unit_module_loading_stack.last().is_some()
            && let Some(module) = self.module_load_stack.last().cloned()
        {
            let mirror = crate::runtime::cow_table_mut(&mut self.exported_subs)
                .entry(module)
                .or_default()
                .entry(name.clone())
                .or_default();
            for tag in &tags {
                mirror.insert(tag.clone());
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

    /// Refresh the export aliases for a multi family after a later candidate
    /// is registered. An exported proto exports its candidates too, but the
    /// proto commonly appears before those candidates in a module body. The
    /// first export registration therefore cannot create the arity-qualified
    /// aliases until the candidates exist.
    pub(crate) fn refresh_exported_multi_family(&mut self, name: &str) {
        let package = self.current_package();
        let tags = if package == "GLOBAL" {
            self.module_load_stack
                .last()
                .and_then(|module| self.module_owned_exports.get(module))
                .and_then(|exports| exports.get(name))
                .cloned()
        } else {
            self.exported_subs
                .get(&package)
                .and_then(|exports| exports.get(name))
                .cloned()
        };
        if let Some(tags) = tags {
            self.register_exported_sub(package, name.to_string(), tags.into_iter().collect());
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
        let mirror_name = name.clone();
        let mirror_tags = tags.clone();
        let entry = crate::runtime::cow_table_mut(&mut self.exported_vars)
            .entry(package)
            .or_default()
            .entry(name)
            .or_default();
        for tag in tags {
            entry.insert(tag);
        }
        // A unit module's top-level declarations execute while the runtime
        // package is still GLOBAL. Mirror variable exports under the declared
        // module name, just as exported subs are mirrored above, so a tagged
        // `our &alias is export(:tag)` is visible to a later `use Module :tag`.
        if let Some(unit_mod) = self.unit_module_loading_stack.last().cloned() {
            let mirror = crate::runtime::cow_table_mut(&mut self.exported_vars)
                .entry(unit_mod)
                .or_default()
                .entry(mirror_name.clone())
                .or_default();
            for tag in &mirror_tags {
                mirror.insert(tag.clone());
            }
        }
        // Use the requested compunit path as a second key when a file's
        // declared `unit module` name differs from that path. This mirrors
        // the variable's export metadata to the namespace import_module
        // actually receives.
        if self.unit_module_loading_stack.last().is_some()
            && let Some(module) = self.module_load_stack.last().cloned()
        {
            let mirror = crate::runtime::cow_table_mut(&mut self.exported_vars)
                .entry(module)
                .or_default()
                .entry(mirror_name.clone())
                .or_default();
            for tag in &mirror_tags {
                mirror.insert(tag.clone());
            }
        }
        // Bare-file modules execute their top-level declarations in GLOBAL,
        // so an exported `our` variable is registered under GLOBAL rather
        // than under the module path. Attribute the export to the module that
        // is loading as well, allowing `use Module :tag` to import it and to
        // expose it through the importing lexical pseudo-stash.
        if let Some(owner) = self.module_load_stack.last().cloned() {
            let mirror = crate::runtime::cow_table_mut(&mut self.exported_vars)
                .entry(owner)
                .or_default()
                .entry(mirror_name)
                .or_default();
            for tag in mirror_tags {
                mirror.insert(tag);
            }
        }
    }

    /// Resolve the value behind a statically exported variable. Most exports
    /// live in the qualified environment, but a `my constant` in a unit class
    /// is persisted in that class's package-lexical store when its body exits.
    /// Keep both import-time and EXPORT-stash reads on the same lookup path.
    pub(crate) fn exported_var_value(&self, module: &str, name: &str) -> Option<Value> {
        let (sigil, bare) = match name.chars().next() {
            Some(sigil @ ('$' | '@' | '%' | '&')) => (Some(sigil), &name[1..]),
            _ => (None, name),
        };
        let qualified = match sigil {
            Some(sigil) => format!("{sigil}{module}::{bare}"),
            None => format!("{module}::{name}"),
        };
        self.env
            .get(&qualified)
            .cloned()
            .or_else(|| self.enum_bare_value(name).cloned())
            .or_else(|| {
                self.package_lexicals
                    .get(module)
                    .and_then(|entries| entries.get(name).or_else(|| entries.get(bare)))
                    .cloned()
            })
            // `our &name` bindings are persistent package variables.  The
            // module load restores their lexical env entries after the first
            // import, so a later tagged re-import must read the durable
            // package store as well (e.g. Math::Trig's code aliases).
            .or_else(|| self.our_vars.get(&qualified).cloned())
            .or_else(|| self.our_vars.get(name).cloned())
            // Code variables use the routine registry as an additional
            // durable store.  This matters for `our &alias = &routine`: the
            // module's lexical `env` entry is restored after its first load,
            // but the qualified code value remains resolvable by name.
            .or_else(|| {
                (sigil == Some('&'))
                    .then(|| self.resolve_code_var(&format!("{module}::{bare}")))
                    .filter(|value| !value.is_nil())
            })
            // A bare-file module's `my constant &name` lives in the
            // compunit's durable module-scope lexical table after module
            // loading restores the importer's plain environment.  The first
            // import can still read the live env entry, but a later tagged
            // re-import must recover the same code value from that table.
            .or_else(|| {
                self.module_scope_lexicals
                    .get(module)
                    .and_then(|entries| entries.get(name).or_else(|| entries.get(bare)))
                    .cloned()
            })
            .or_else(|| self.env.get(name).cloned())
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
            // Rakudo exposes the C type objects through both the DEFAULT
            // export and the explicit `:types` tag.  Keep the default import
            // surface intact while allowing NativeCall consumers such as
            // SSH::LibSSH to request only the type exports.
            self.register_exported_var(
                "NativeCall".to_string(),
                name.to_string(),
                vec!["DEFAULT".to_string(), "types".to_string()],
            );
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
        let bare_file_module =
            subs.is_empty() && unit_global_subs.is_empty() && !owned_subs.is_empty();
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

        // Ordinary imports use the runtime package selected by the loading
        // compunit. A file-level operator `use` can run before its later
        // `unit class`/`unit module` declaration has registered and switched
        // `current_package`; operator candidates must instead be installed in
        // the unit package so methods in that declaration can dispatch them.
        // Keep this distinction local to operators: regular nested `use`s in
        // a `need`-loaded compunit remain in their historical GLOBAL/import
        // scope, while operator syntax needs the unit package during the
        // declaration's pre-registration window.
        let current_pkg = self.current_package().to_string();
        let unit_pkg = self
            .import_target_package
            .clone()
            .or_else(|| self.unit_module_loading_stack.last().cloned())
            .unwrap_or_else(|| current_pkg.clone());

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
        if bare_file_module {
            for (name, tags) in owned_subs.iter() {
                merged_subs
                    .entry(name.clone())
                    .or_default()
                    .extend(tags.iter().cloned());
            }
        }

        for (name, symbol_tags) in merged_subs {
            // MANDATORY exports are always imported regardless of requested tags
            let is_mandatory = symbol_tags.contains("MANDATORY");
            if !import_all && !is_mandatory && symbol_tags.is_disjoint(&requested) {
                continue;
            }
            let target_pkg = if name.contains(":<") {
                &unit_pkg
            } else {
                &current_pkg
            };
            self.record_imported_routine_alias(target_pkg, &name);
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
            let module_export_prefix = format!("{module}::EXPORT::ALL::{name}/");
            // An exported method is represented by synthetic arity-qualified
            // candidates.  A class may also contain a same-named plain sub;
            // that plain sub occupies `source_single` but is not the method's
            // export.  Prefer the method candidates when rebuilding the
            // importing scope, otherwise the exact plain sub steals the
            // export simply because it sorts first in this lookup.
            let exported_method_candidates = self
                .registry()
                .functions
                .get(&Symbol::intern(&source_single))
                .is_some_and(|def| def.declarator != crate::ast::RoutineDeclarator::Method)
                && self.registry().functions.iter().any(|(key, def)| {
                    key.resolve().starts_with(&source_prefix)
                        && def.declarator == crate::ast::RoutineDeclarator::Method
                });
            let bare_file_multi = bare_file_module
                && self
                    .registry()
                    .functions
                    .keys()
                    .any(|key| key.resolve().starts_with(&module_export_prefix));
            let imported_proto = self
                .registry()
                .proto_functions
                .contains_key(&Symbol::intern(&source_single))
                || (unit_global_subs.contains_key(&name)
                    && self
                        .registry()
                        .proto_functions
                        .contains_key(&Symbol::intern(&format!("GLOBAL::{name}"))))
                || bare_file_multi;
            let global_family_present = bare_file_module
                && self
                    .registry()
                    .functions
                    .keys()
                    .any(|key| key.resolve().starts_with(&format!("GLOBAL::{name}/")));
            // A top-level package-block import such as Zef::CLI's exported
            // `proto MAIN` has no target family to hide: the module's own
            // promoted GLOBAL candidates are the family we are importing.
            // Shadow an imported proto in a lexical scope, or a preloaded
            // GLOBAL family for a bare-file module, but do not shadow a
            // package-qualified proto merely because the source has one.
            if (imported_proto && !self.import_scope_stack.is_empty()) || global_family_present {
                self.shadow_imported_proto_family(&target_single);
            }

            let mut function_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter_map(|(k, v)| {
                    let ks = k.resolve();
                    if ks == source_single && !exported_method_candidates {
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
            // A unit module's top-level multi candidates are registered under
            // GLOBAL::name/<arity>, while the unit-module export table records
            // the public name under the module. The ordinary exact-key lookup
            // above therefore finds nothing (and the EXPORT::ALL fallback can
            // only recover a single candidate). Reuse the global candidate
            // family when this is a unit export so every dispatch alternative
            // is imported.
            if function_entries.is_empty() && unit_global_subs.contains_key(&name) {
                let global_single = format!("GLOBAL::{name}");
                let global_prefix = format!("GLOBAL::{name}/");
                function_entries = self
                    .registry()
                    .functions
                    .iter()
                    .filter_map(|(k, v)| {
                        let ks = k.resolve();
                        if ks == global_single {
                            Some((Symbol::intern(&target_single), v.clone()))
                        } else if ks.starts_with(&global_prefix) {
                            Some((
                                Symbol::intern(&ks.replacen(&global_prefix, &target_prefix, 1)),
                                v.clone(),
                            ))
                        } else {
                            None
                        }
                    })
                    .collect();
            }
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
                    }
                    let alias_prefix = format!("{alias}/");
                    let candidates: Vec<(Symbol, Arc<FunctionDef>)> = self
                        .registry()
                        .functions
                        .iter()
                        .filter_map(|(key, def)| {
                            let key = key.resolve();
                            key.strip_prefix(&alias_prefix).map(|suffix| {
                                (
                                    Symbol::intern(&format!("{target_prefix}{suffix}")),
                                    def.clone(),
                                )
                            })
                        })
                        .collect();
                    function_entries.extend(candidates);
                    if !function_entries.is_empty() {
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
            self.invalidate_fn_resolution();

            let proto_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
                .registry()
                .proto_functions
                .iter()
                .filter_map(|(k, v)| {
                    if *k == *source_single
                        || (unit_global_subs.contains_key(&name)
                            && *k == Symbol::intern(&format!("GLOBAL::{name}")))
                        || (bare_file_module && *k == Symbol::intern(&format!("GLOBAL::{name}")))
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
                let qualified_key = format!("&{current_pkg}::{name}");
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
            let target = name.clone();
            if let Some(value) = self.exported_var_value(module, &name) {
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
                crate::runtime::cow_table_mut(&mut self.need_hidden_classes).insert(short_name);
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
