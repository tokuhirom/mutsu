use super::*;

impl Interpreter {
    fn missing_symbol_name_from_failure(value: &Value) -> Option<String> {
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
        else {
            return None;
        };
        if class_name != "Failure" {
            return None;
        }
        let map = attributes.as_map();
        let exception = map.get("exception")?;
        let Value::Instance {
            attributes: ex_attrs,
            ..
        } = exception
        else {
            return None;
        };
        let message = ex_attrs.as_map().get("message")?.to_string_value();
        let prefix = "No such symbol '";
        let rest = message.strip_prefix(prefix)?;
        let symbol = rest.strip_suffix('\'')?;
        if symbol.is_empty() {
            None
        } else {
            Some(symbol.to_string())
        }
    }

    fn require_target_is_path_like(target: &str) -> bool {
        target.ends_with(".rakumod")
            || target.ends_with(".pm6")
            || target.contains('/')
            || target.contains('\\')
    }

    fn require_guess_module_name_from_path(path: &str) -> Option<String> {
        let p = Path::new(path);
        let stem = p.file_stem()?.to_string_lossy().to_string();
        if stem.is_empty() { None } else { Some(stem) }
    }

    fn resolve_require_file_path(&self, file: &str) -> Option<PathBuf> {
        let direct = PathBuf::from(file);
        if direct.exists() {
            return Some(direct);
        }
        for base in &self.lib_paths {
            let candidate = Path::new(base).join(file);
            if candidate.exists() {
                return Some(candidate);
            }
            let candidate = Path::new(base).join("lib").join(file);
            if candidate.exists() {
                return Some(candidate);
            }
        }
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            let candidate = Path::new(&cwd).join(file);
            if candidate.exists() {
                return Some(candidate);
            }
        }
        None
    }

    /// Parse a source file for require, returning the merged AST.
    fn parse_require_source(
        &mut self,
        file: &str,
        path: &std::path::Path,
    ) -> Result<Vec<crate::ast::Stmt>, RuntimeError> {
        let code = std::fs::read_to_string(path)
            .map_err(|e| RuntimeError::new(format!("Failed to read module {}: {}", file, e)))?;
        let preprocessed = Self::maybe_preprocess_roast_directives(&code);
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let result = parse_dispatch::parse_source(&preprocessed);
        crate::parser::clear_parser_lib_paths();
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
        let stmts = result.map(|(stmts, _)| stmts).map_err(|mut err| {
            err.message = format!("Failed to parse module '{}': {}", file, err.message);
            err
        })?;
        Ok(Self::merge_unit_class(stmts))
    }

    fn require_load_from_file(
        &mut self,
        file: &str,
        package_hint: Option<&str>,
    ) -> Result<(), RuntimeError> {
        let path = self
            .resolve_require_file_path(file)
            .ok_or_else(|| RuntimeError::unsatisfied_dependency(file))?;

        // Try loading from precompilation cache
        let stmts = if self.precomp_enabled {
            if let Some(cached) = crate::precomp::load_cached_ast(&path) {
                cached
            } else {
                let parsed = self.parse_require_source(file, &path)?;
                crate::precomp::save_cached_ast(&path, &parsed);
                parsed
            }
        } else {
            self.parse_require_source(file, &path)?
        };
        let saved_package = self.current_package();
        let before_function_keys: std::collections::HashSet<Symbol> =
            self.registry().functions.keys().copied().collect();
        let before_env_keys: std::collections::HashSet<Symbol> = self.env.keys().copied().collect();
        let before_class_keys: std::collections::HashSet<String> =
            self.registry().classes.keys().cloned().collect();
        if let Some(pkg) = package_hint
            && !pkg.is_empty()
        {
            self.set_current_package(pkg.to_string());
        }
        let run_result = self.run_block(&stmts);
        self.set_current_package(saved_package);
        run_result?;

        // A `sub MAIN` defined in a required module is NOT the program's MAIN
        // and must not be auto-dispatched at program end -- unless the module
        // *exported* MAIN (`proto MAIN(|) is export`, as zef does), in which case
        // it becomes the importer's MAIN. Remove only non-exported leaked MAINs.
        self.promote_exported_main_to_global();
        let main_exported = self.exported_subs.values().any(|m| m.contains_key("MAIN"));
        Self::remove_leaked_main_routines(
            &mut self.registry_mut().functions,
            &before_function_keys,
            main_exported,
        );

        if let Some(pkg) = package_hint
            && !pkg.is_empty()
        {
            let mut fn_aliases: Vec<(Symbol, std::sync::Arc<FunctionDef>)> = Vec::new();
            for (name, def) in &self.registry().functions {
                if before_function_keys.contains(name) {
                    continue;
                }
                let name_s = name.resolve();
                let tail = name_s.rsplit_once("::").map(|(_, t)| t).unwrap_or(&name_s);
                if tail.contains('/') || tail.contains(':') {
                    continue;
                }
                let alias = format!("{pkg}::{tail}");
                let alias_sym = Symbol::intern(&alias);
                if !self.registry().functions.contains_key(&alias_sym) {
                    fn_aliases.push((alias_sym, def.clone()));
                }
            }
            for (alias, def) in fn_aliases {
                self.registry_mut().functions.insert(alias, def);
            }

            let mut env_aliases: Vec<(String, Value)> = Vec::new();
            for (name, value) in &self.env {
                if before_env_keys.contains(name) {
                    continue;
                }
                if name.starts_with("$") || name.starts_with("@") || name.starts_with("%") {
                    continue;
                }
                let name_s = name.resolve();
                let tail = name_s.rsplit_once("::").map(|(_, t)| t).unwrap_or(&name_s);
                let alias = format!("{pkg}::{tail}");
                if !self.env.contains_key(&alias) {
                    env_aliases.push((alias, value.clone()));
                }
            }
            for (alias, value) in env_aliases {
                self.env.insert(alias, value);
            }

            let mut class_aliases: Vec<(String, ClassDef)> = Vec::new();
            for (name, class_def) in &self.registry().classes {
                if before_class_keys.contains(name) {
                    continue;
                }
                let tail = name.rsplit_once("::").map(|(_, t)| t).unwrap_or(name);
                let alias = format!("{pkg}::{tail}");
                if !self.registry().classes.contains_key(&alias) {
                    class_aliases.push((alias, class_def.clone()));
                }
            }
            for (alias, class_def) in class_aliases {
                self.registry_mut().classes.insert(alias, class_def);
            }
        }

        Ok(())
    }

    /// Remove top-level `MAIN` routines that were registered while loading a
    /// module (via `require`/`use`). A `sub MAIN` declared in a loaded module
    /// is not the program's MAIN and must not be auto-dispatched at program
    /// end. `before_keys` is the set of function-registry keys that existed
    /// before the module body ran; only newly-added MAIN keys are removed.
    /// Promote an *exported* `MAIN` defined in a non-GLOBAL package (e.g. zef's
    /// `package Zef::CLI { proto MAIN(|) is export … }`) to the dispatchable
    /// `GLOBAL::MAIN` slot, so the importing program's auto-MAIN dispatch finds
    /// it. Exported MAIN in a `unit module` already lands under GLOBAL; a MAIN in
    /// a nested `package` block lands under `Pkg::MAIN` and would otherwise be
    /// invisible to dispatch (and dropped as a leak).
    pub(crate) fn promote_exported_main_to_global(&mut self) {
        let pkgs_with_main: Vec<String> = self
            .exported_subs
            .iter()
            .filter(|(p, m)| p.as_str() != "GLOBAL" && m.contains_key("MAIN"))
            .map(|(p, _)| p.clone())
            .collect();
        if pkgs_with_main.is_empty() {
            return;
        }
        for pkg in pkgs_with_main {
            let prefix = format!("{pkg}::MAIN");
            // Collect `Pkg::MAIN`, `Pkg::MAIN/<arity>`, `Pkg::MAIN/<...>:<sig>`.
            let to_promote: Vec<(String, std::sync::Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(k, _)| {
                    let ks = k.resolve();
                    ks == prefix || ks.starts_with(&format!("{prefix}/"))
                })
                .map(|(k, v)| (k.resolve().to_string(), v.clone()))
                .collect();
            for (key, def) in to_promote {
                let global_key = key.replacen(&format!("{pkg}::"), "GLOBAL::", 1);
                let gsym = Symbol::intern(&global_key);
                self.registry_mut().functions.entry(gsym).or_insert(def);
            }
        }
    }

    pub(crate) fn remove_leaked_main_routines(
        functions: &mut HashMap<Symbol, std::sync::Arc<FunctionDef>>,
        before_keys: &std::collections::HashSet<Symbol>,
        main_exported: bool,
    ) {
        // A module's *exported* `proto/sub MAIN is export` IS meant to become the
        // importing program's MAIN (e.g. zef's `proto MAIN(|) is export`); its
        // candidates land under the dispatchable `GLOBAL::MAIN`. When the loaded
        // module exported MAIN, keep those; otherwise drop every leaked MAIN (a
        // non-exported `sub MAIN` in a used module is not the program's MAIN).
        let leaked: Vec<Symbol> = functions
            .keys()
            .filter(|k| {
                if before_keys.contains(*k) {
                    return false;
                }
                let ks = k.resolve();
                let after_pkg = ks.rsplit_once("::").map(|(_, t)| t).unwrap_or(&ks);
                let short = after_pkg.split(['/', ':']).next().unwrap_or(after_pkg);
                if short != "MAIN" {
                    return false;
                }
                // Keep the exported, dispatchable GLOBAL::MAIN candidates.
                !(main_exported && ks.starts_with("GLOBAL::"))
            })
            .copied()
            .collect();
        for k in leaked {
            functions.remove(&k);
        }
    }

    fn import_single_require_symbol(&mut self, module: &str, symbol: &str) -> bool {
        if symbol.is_empty() {
            return true;
        }
        if module == "Test" {
            return true;
        }
        if let Some(name) = symbol.strip_prefix('&') {
            let source_single = format!("{module}::{name}");
            let source_prefix = format!("{module}::{name}/");
            let target_single = format!("GLOBAL::{name}");
            let target_prefix = format!("GLOBAL::{name}/");
            let mut found = false;
            let function_entries: Vec<(Symbol, std::sync::Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter_map(|(k, v)| {
                    let ks = k.resolve();
                    if ks == source_single {
                        found = true;
                        Some((Symbol::intern(&target_single), v.clone()))
                    } else if ks.starts_with(&source_prefix) {
                        found = true;
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
            return found
                || self
                    .registry()
                    .functions
                    .contains_key(&Symbol::intern(&format!("GLOBAL::{name}")));
        }

        if let Some(sigil) = symbol.chars().next()
            && matches!(sigil, '$' | '@' | '%' | '&')
        {
            let bare = &symbol[1..];
            let candidates = [
                format!("{sigil}{module}::{bare}"),
                format!("{sigil}{bare}"),
                format!("{module}::{bare}"),
                bare.to_string(),
            ];
            for source in candidates {
                if let Some(value) = self.env.get(&source).cloned() {
                    self.env.insert(symbol.to_string(), value);
                    return true;
                }
            }
            return false;
        }

        let source_single = format!("{module}::{symbol}");
        if self.has_class(&source_single) || self.is_role(&source_single) {
            self.env.insert(
                symbol.to_string(),
                Value::Package(Symbol::intern(&source_single)),
            );
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return true;
        }
        if self.has_class(symbol) || self.is_role(symbol) {
            self.env
                .insert(symbol.to_string(), Value::Package(Symbol::intern(symbol)));
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return true;
        }
        if let Some(value) = self.env.get(&source_single).cloned() {
            if self
                .registry()
                .functions
                .contains_key(&Symbol::intern(&source_single))
                && matches!(value, Value::Int(_))
            {
                return false;
            }
            self.env.insert(symbol.to_string(), value);
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return true;
        }
        if let Some(value) = self.env.get(symbol).cloned() {
            let is_nil = matches!(value, Value::Nil);
            self.env.insert(symbol.to_string(), value);
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return !is_nil;
        }
        false
    }

    fn require_import_symbols(
        &mut self,
        module: &str,
        imports: &[String],
    ) -> Result<(), RuntimeError> {
        let mut missing = Vec::new();
        for symbol in imports {
            if !self.import_single_require_symbol(module, symbol) {
                missing.push(symbol.clone());
            }
        }
        if missing.is_empty() {
            Ok(())
        } else {
            Err(RuntimeError::new(format!(
                "X::Import::MissingSymbols: {}",
                missing.join(" ")
            )))
        }
    }

    pub(super) fn builtin_require(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut module_value: Option<Value> = None;
        let mut file_target: Option<String> = None;
        let mut imports: Vec<String> = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "__mutsu_require_file" => {
                    file_target = Some(value.to_string_value());
                }
                Value::Array(items, ..) => {
                    imports.extend(items.iter().map(|v| v.to_string_value()));
                }
                other => {
                    if module_value.is_none() {
                        module_value = Some(other.clone());
                    } else {
                        imports.push(other.to_string_value());
                    }
                }
            }
        }
        let module_value =
            module_value.ok_or_else(|| RuntimeError::new("require expects a module"))?;
        let module_string = match &module_value {
            Value::Package(name) => name.resolve(),
            Value::Str(name) => name.to_string(),
            value @ Value::Instance { .. } => Self::missing_symbol_name_from_failure(value)
                .unwrap_or_else(|| value.to_string_value()),
            other => other.to_string_value(),
        };
        let return_is_str = matches!(module_value, Value::Str(_));
        let mut module_name = if module_string.is_empty() {
            None
        } else {
            Some(module_string.clone())
        };

        let load_from_file = file_target.is_some()
            || module_name
                .as_ref()
                .is_some_and(|m| Self::require_target_is_path_like(m));
        if load_from_file {
            let file = file_target
                .clone()
                .or_else(|| module_name.clone())
                .ok_or_else(|| RuntimeError::new("require expects a file"))?;
            if file_target.is_none() {
                module_name = Self::require_guess_module_name_from_path(&file);
            }
            self.require_load_from_file(&file, module_name.as_deref())?;
            if let Some(module) = module_name.as_ref() {
                self.loaded_modules.insert(module.clone());
            }
        } else if let Some(module) = module_name.as_ref() {
            // For `require`, force a full reload if the module was previously loaded
            // but its functions were cleaned up by pop_import_scope (e.g. when
            // a previous `require` happened inside a block that has since exited).
            let prefix = format!("{module}::");
            if self.loaded_modules.contains(module)
                && !self
                    .registry()
                    .functions
                    .keys()
                    .any(|k| k.resolve().starts_with(&prefix))
            {
                self.loaded_modules.remove(module);
            }
            self.use_module(module)?;
        } else {
            return Err(RuntimeError::new("require expects a module name"));
        }

        let in_method_context = !self.method_class_stack.is_empty();
        let should_install_stub = !return_is_str || !in_method_context;
        if let Some(module) = module_name.as_ref()
            && should_install_stub
        {
            self.env
                .insert(module.clone(), Value::Package(Symbol::intern(module)));
            if !imports.is_empty() {
                self.require_import_symbols(module, &imports)?;
            }
        } else if let Some(module) = module_name.as_ref()
            && !imports.is_empty()
        {
            self.require_import_symbols(module, &imports)?;
        }

        if return_is_str {
            Ok(Value::str(module_string))
        } else {
            let name = module_name.unwrap_or(module_string);
            Ok(Value::Package(Symbol::intern(&name)))
        }
    }
}
