use super::*;

impl Interpreter {
    /// Record a trait-modified routine value for an exported sub, so that
    /// `import_module` can restore the `&name` env binding with the role mixed in.
    pub(crate) fn record_exported_sub_value(&mut self, package: String, name: String, val: Value) {
        self.exported_sub_values
            .entry(package)
            .or_default()
            .insert(name, val);
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
                    .functions
                    .entry(crate::symbol::Symbol::intern(&bare_export))
                    .or_insert_with(|| def.clone());
                // Fully-qualified Package::EXPORT::TAG::name
                let pkg_export = format!("{}::EXPORT::{}::{}", package, tag, name);
                self.registry_mut()
                    .functions
                    .entry(crate::symbol::Symbol::intern(&pkg_export))
                    .or_insert_with(|| def.clone());
            }
            // Always register under EXPORT::ALL::name
            if !tags.contains(&"ALL".to_string()) {
                let bare_all = format!("EXPORT::ALL::{}", name);
                self.registry_mut()
                    .functions
                    .entry(crate::symbol::Symbol::intern(&bare_all))
                    .or_insert_with(|| def.clone());
                let pkg_all = format!("{}::EXPORT::ALL::{}", package, name);
                self.registry_mut()
                    .functions
                    .entry(crate::symbol::Symbol::intern(&pkg_all))
                    .or_insert_with(|| def);
            }
        }
        // Mirror this export into the unit-module export table so that
        // `import_module` can validate tags for `unit module X` files whose
        // runtime package registration used "GLOBAL".
        if let Some(unit_mod) = self.unit_module_loading_stack.last().cloned() {
            let mirror = self
                .unit_module_exported_subs
                .entry(unit_mod)
                .or_default()
                .entry(name.clone())
                .or_default();
            for tag in &tags {
                mirror.insert(tag.clone());
            }
        }
        let entry = self
            .exported_subs
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
        let entry = self
            .exported_vars
            .entry(package)
            .or_default()
            .entry(name)
            .or_default();
        for tag in tags {
            entry.insert(tag);
        }
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
        if subs.is_empty() && vars.is_empty() && unit_global_subs.is_empty() {
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

        for (name, symbol_tags) in subs {
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
                self.imported_operator_names.insert(name.clone());
            }
            let source_single = format!("{module}::{name}");
            let source_prefix = format!("{module}::{name}/");
            let target_single = format!("{target_pkg}::{name}");
            let target_prefix = format!("{target_pkg}::{name}/");

            let function_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
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
            for (k, v) in function_entries {
                self.registry_mut().functions.insert(k, v);
            }

            let proto_entries: Vec<(Symbol, Arc<FunctionDef>)> = self
                .registry()
                .proto_functions
                .iter()
                .filter_map(|(k, v)| {
                    if *k == *source_single {
                        Some((Symbol::intern(&target_single), v.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            for (k, v) in proto_entries {
                self.registry_mut().proto_functions.insert(k, v);
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
                self.env.insert(format!("&{name}"), val.clone());
                self.env.insert(format!("&{target_pkg}::{name}"), val);
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
                if !target.contains("::") {
                    self.unsuppress_name(&target);
                }
                // Slice F (env<->locals coherence): `import` writes the symbol
                // into env by name, but a later bare reference (e.g. an imported
                // `constant c`) may read a stale caller local slot when the
                // reverse env->locals pull is disabled. Record the imported
                // name (sigil stripped to match the local-slot key) so the
                // ImportModule opcode writes it through to the caller slot.
                if !target.contains("::") {
                    let slot_name = match target.chars().next() {
                        Some('$' | '@' | '%') => target[1..].to_string(),
                        _ => target.clone(),
                    };
                    self.pending_rw_writeback_sources.push(slot_name);
                }
                self.env.insert(target, value);
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
                    self.need_hidden_classes.insert(class_name.clone());
                    if let Some((_, short)) = class_name.rsplit_once("::") {
                        self.need_hidden_classes.insert(short.to_string());
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
                    self.need_hidden_classes.insert(key_s.clone());
                    self.need_hidden_classes.insert(key_short.to_string());
                }
            }
            if is_nested_need {
                self.need_hidden_classes.insert(short_name.clone());
            }
            self.loaded_modules.insert(module.to_string());
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
