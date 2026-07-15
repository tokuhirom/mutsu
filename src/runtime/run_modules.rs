use super::*;

impl Interpreter {
    /// Resolve a module name to a file path by searching lib paths and standard locations.
    /// Returns (source_path, optional_dist_json) where optional_dist_json is Some for
    /// modules found in a CompUnit::Repository::Installation (inst# paths).
    pub(super) fn resolve_module_path(
        &self,
        module: &str,
    ) -> Option<(std::path::PathBuf, Option<String>)> {
        let base_name = module.replace("::", "/");
        let extensions = [".rakumod", ".pm6", ".pm"];

        // Check inst# paths (CompUnit::Repository::Installation) first.
        for base in &self.lib_paths {
            if let Some(prefix) = base.strip_prefix("inst#") {
                let prefix_path = Path::new(prefix);
                let dist_dir = prefix_path.join("dist");
                if !dist_dir.is_dir() {
                    continue;
                }
                let Ok(entries) = std::fs::read_dir(&dist_dir) else {
                    continue;
                };
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().and_then(|e| e.to_str()) != Some("json") {
                        continue;
                    }
                    let Ok(json_str) = std::fs::read_to_string(&path) else {
                        continue;
                    };
                    if let Some(file_id) = Self::find_module_file_id_in_dist_json(&json_str, module)
                    {
                        let source_path = prefix_path.join("sources").join(&file_id);
                        if source_path.exists() {
                            return Some((source_path, Some(json_str)));
                        }
                    }
                }
            }
        }

        let mut candidates: Vec<std::path::PathBuf> = Vec::new();
        for base in &self.lib_paths {
            if let Some(prefix) = base.strip_prefix("inst#") {
                // Installation repo: sources are stored as {prefix}/sources/{hash_id}
                // Look in dist JSON files for provides[module][file] entry
                let prefix_path = Path::new(prefix);
                let dist_dir = prefix_path.join("dist");
                let sources_dir = prefix_path.join("sources");
                if dist_dir.is_dir()
                    && let Ok(entries) = std::fs::read_dir(&dist_dir)
                {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        if path.extension().and_then(|e| e.to_str()) != Some("json") {
                            continue;
                        }
                        let Ok(json_str) = std::fs::read_to_string(&path) else {
                            continue;
                        };
                        let Ok(meta) = self.parse_json_to_value(&json_str) else {
                            continue;
                        };
                        if let Some(provides) = meta.hash_get_str("provides")
                            && let Some(entry_val) = provides.hash_get_str(module)
                        {
                            // entry_val is either {"file": "hash_id"} or just a string
                            let hash_id = match entry_val.view() {
                                ValueView::Hash(map) => map
                                    .get("file")
                                    .map(|v| v.to_string_value())
                                    .unwrap_or_default(),
                                _ => entry_val.to_string_value(),
                            };
                            if !hash_id.is_empty() {
                                let source_path = sources_dir.join(&hash_id);
                                if source_path.exists() {
                                    return Some((source_path, None));
                                }
                            }
                        }
                    }
                }
                continue; // Don't try inst# path as a filesystem path
            }
            for ext in &extensions {
                let filename = format!("{}{}", base_name, ext);
                let base_path = Path::new(base.as_str());
                candidates.push(base_path.join(&filename));
                candidates.push(base_path.join("lib").join(&filename));
            }
        }
        if candidates.is_empty()
            && let Some(path) = &self.program_path
            && let Some(parent) = Path::new(path).parent()
            && !parent.as_os_str().is_empty()
            && parent.is_dir()
        {
            for ext in &extensions {
                let filename = format!("{}{}", base_name, ext);
                candidates.push(parent.join(&filename));
            }
        }
        if let Some(path) = &self.program_path {
            let top_module = module.split("::").next().unwrap_or(module);
            for ancestor in Path::new(path).ancestors() {
                if ancestor.as_os_str().is_empty() {
                    continue;
                }
                for ext in &extensions {
                    let filename = format!("{}{}", base_name, ext);
                    candidates.push(
                        ancestor
                            .join("packages")
                            .join(top_module)
                            .join("lib")
                            .join(&filename),
                    );
                    candidates.push(
                        ancestor
                            .join("roast")
                            .join("packages")
                            .join(top_module)
                            .join("lib")
                            .join(&filename),
                    );
                }
            }
        }
        candidates
            .into_iter()
            .find(|path| path.exists())
            .map(|p| (p, None))
    }

    /// Default install directory for a well-known repository name
    /// ("site"/"home"/"vendor"/"perl"), mirroring the XDG pattern used by
    /// `precomp.rs::cache_dir()`. Returns `None` (no default available) rather
    /// than erroring when `$HOME`/`XDG_DATA_HOME` can't be determined; callers
    /// must not assume the directory exists yet.
    pub(super) fn default_repo_dir(kind: &str) -> Option<std::path::PathBuf> {
        let base = if let Ok(xdg) = std::env::var("XDG_DATA_HOME") {
            std::path::PathBuf::from(xdg)
        } else if let Ok(home) = std::env::var("HOME") {
            std::path::PathBuf::from(home).join(".local").join("share")
        } else {
            return None;
        };
        Some(base.join("mutsu").join("repo").join(kind))
    }

    /// Append the default "site" repository (see `default_repo_dir`) to the
    /// end of `lib_paths` as an `inst#` entry, so a plain `use ModuleName`
    /// finds modules installed there via
    /// `CompUnit::RepositoryRegistry.repository-for-name("site").install(...)`.
    /// Appended (not prepended) so explicit `-I`/`MUTSULIB`/project-local
    /// paths still win, matching real Raku's `-I` semantics of inserting in
    /// front of the default site/vendor/core chain. No-op if the directory
    /// can't be determined; the directory need not exist yet.
    pub fn add_default_site_repo(&mut self) {
        if let Some(dir) = Self::default_repo_dir("site") {
            self.add_lib_path(format!("inst#{}", dir.display()));

            // Hang a CompUnit::Repository::Installation for the site repo off the
            // tail of $*REPO's chain, so `$*REPO.repo-chain` exposes it as an
            // Installation repository the way real Raku always does (real Raku's
            // chain contains several Installation repos; mutsu's default was a
            // FileSystem-only chain). zef's `list-installed`/`locate` grep the
            // chain for CompUnit::Repository::Installation entries -- without an
            // Installation repo present those commands reported nothing installed
            // even when the site repo held modules.
            let mut site_attrs = HashMap::new();
            site_attrs.insert("prefix".to_string(), Value::str(dir.display().to_string()));
            let site_repo = Value::make_instance(
                Symbol::intern("CompUnit::Repository::Installation"),
                site_attrs,
            );
            let mut cursor = self.env.get("*REPO").cloned();
            while let Some(node) = cursor {
                let ValueView::Instance { attributes, .. } = node.view() else {
                    break;
                };
                // Read the current `next-repo` and release the read lock before
                // taking the write lock below (holding both on the same
                // interior-mutable cell would self-deadlock).
                let next = attributes.as_map().get("next-repo").cloned();
                match next {
                    Some(next)
                        if next.truthy() && matches!(next.view(), ValueView::Instance { .. }) =>
                    {
                        cursor = Some(next);
                    }
                    _ => {
                        attributes.insert("next-repo".to_string(), site_repo);
                        break;
                    }
                }
            }
        }
    }

    /// Parse a dist JSON string and return the file ID for the given module name.
    /// Installed distributions store provides as {"ModuleName": {"file": "hexid"}}.
    fn find_module_file_id_in_dist_json(json_str: &str, module: &str) -> Option<String> {
        let provides_pos = json_str.find("\"provides\"")?;
        let after_provides = &json_str[provides_pos + 10..];
        let colon_pos = after_provides.find(':')?;
        let after_colon = after_provides[colon_pos + 1..].trim_start();
        if !after_colon.starts_with('{') {
            return None;
        }
        let module_key = format!("\"{}\"", module);
        let module_pos = after_colon.find(&module_key)?;
        let after_module = &after_colon[module_pos + module_key.len()..];
        let colon2 = after_module.find(':')?;
        let after_colon2 = after_module[colon2 + 1..].trim_start();
        if after_colon2.starts_with('{') {
            let file_key = "\"file\"";
            let file_pos = after_colon2.find(file_key)?;
            let after_file = &after_colon2[file_pos + file_key.len()..];
            let colon3 = after_file.find(':')?;
            let after_colon3 = after_file[colon3 + 1..].trim_start();
            if let Some(stripped) = after_colon3.strip_prefix('"') {
                let end = stripped.find('"')?;
                return Some(stripped[..end].to_string());
            }
        }
        None
    }

    /// Parse a module source file, using the precompilation cache when available.
    /// Returns (stmts, was_precompiled).
    /// Extract operator sub names (infix:<..>, prefix:<..>, etc.) that a
    /// module exports with `is export` (DEFAULT or MANDATORY tag). Used by
    /// `load_module` to populate `imported_operator_names` so EVAL can see
    /// operators from imported modules without seeing non-exported subs.
    fn extract_module_exported_operator_names(source: &str) -> Vec<String> {
        let (stmts, _) = crate::parser::parse_program_partial(source);
        let mut out = Vec::new();
        for stmt in &stmts {
            if let crate::ast::Stmt::SubDecl {
                name,
                is_export,
                export_tags,
                ..
            } = stmt
                && *is_export
                && export_tags
                    .iter()
                    .any(|t| t == "DEFAULT" || t == "MANDATORY")
            {
                let n = name.resolve();
                if n.starts_with("infix:<")
                    || n.starts_with("prefix:<")
                    || n.starts_with("postfix:<")
                    || n.starts_with("circumfix:<")
                    || n.starts_with("postcircumfix:<")
                {
                    out.push(n);
                }
            }
        }
        out
    }

    pub(super) fn parse_module_source(
        &mut self,
        module: &str,
        source_path: &Path,
    ) -> Result<(Vec<crate::ast::Stmt>, bool), RuntimeError> {
        // Read source first so we can honor precompilation directives before cache lookup.
        let code = fs::read_to_string(source_path).map_err(|err| {
            RuntimeError::new(format!("Failed to read module {}: {}", module, err))
        })?;

        let has_no_precompilation = Self::source_has_no_precompilation(&code);
        let dependency_disables_precomp = self.dependency_disables_precomp(&code);
        let precomp_eligible =
            self.precomp_enabled && !has_no_precompilation && !dependency_disables_precomp;

        // Try loading from precompilation cache when eligible.
        if precomp_eligible && let Some(stmts) = crate::precomp::load_cached_ast(source_path) {
            return Ok((stmts, true));
        }

        let preprocessed = Self::maybe_preprocess_roast_directives(&code);
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let result = parse_dispatch::parse_source(&preprocessed);
        crate::parser::clear_parser_lib_paths();
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
        let stmts = result.map(|(stmts, _)| stmts).map_err(|mut err| {
            err.message = format!("Failed to parse module '{}': {}", module, err.message);
            err
        })?;
        let mut stmts = Self::merge_unit_class(stmts);
        // A module that uses NativeCall and references `Pointer` needs the
        // builtin `Pointer` prelude class too — the main-program injection only
        // sees the main source, so a NativeCall binding distributed as a module
        // would otherwise hit an undeclared `Pointer`.
        Self::inject_nativecall_prelude(&preprocessed, &mut stmts);

        // Save to precompilation cache when the module is eligible.
        if precomp_eligible {
            crate::precomp::save_cached_ast(source_path, &stmts);
        }

        Ok((stmts, precomp_eligible))
    }

    /// Return the name of a top-level `unit module/package/class` statement
    /// in `stmts`, if any. Used by `load_module` to track which unit module
    /// is currently loading so exports can be mirrored under the module name.
    fn detect_unit_package_name(stmts: &[crate::ast::Stmt]) -> Option<String> {
        for s in stmts {
            if let crate::ast::Stmt::Package {
                name,
                is_unit: true,
                ..
            } = s
            {
                return Some(name.resolve().to_string());
            }
        }
        None
    }

    /// For a module loaded from an inst# installation repo, find the distribution JSON
    /// and build a distribution Value. Returns None if the module is not from an inst# repo.
    fn detect_inst_distribution(&self, module: &str) -> Option<Value> {
        for base in &self.lib_paths {
            let prefix = base.strip_prefix("inst#")?;
            let prefix_path = Path::new(prefix);
            let dist_dir = prefix_path.join("dist");
            let resources_dir = prefix_path.join("resources");
            if !dist_dir.is_dir() {
                continue;
            }
            let Ok(entries) = std::fs::read_dir(&dist_dir) else {
                continue;
            };
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) != Some("json") {
                    continue;
                }
                let Ok(json_str) = std::fs::read_to_string(&path) else {
                    continue;
                };
                let Ok(meta_val) = self.parse_json_to_value(&json_str) else {
                    continue;
                };
                if let Some(provides) = meta_val.hash_get_str("provides")
                    && provides.hash_get_str(module).is_some()
                {
                    // Build a distribution instance with files resolved to absolute paths
                    // Remap the "files" entries to full paths
                    use std::collections::HashMap;

                    let mut resolved_files: HashMap<String, Value> = HashMap::new();
                    if let Some(files_val) = meta_val.hash_get_str("files")
                        && let ValueView::Hash(fmap) = files_val.view()
                    {
                        for (k, v) in fmap.iter() {
                            let hash_id = v.to_string_value();
                            // Determine full path based on key prefix
                            let full_path = if k.starts_with("resources/") {
                                resources_dir.join(&hash_id).to_string_lossy().to_string()
                            } else {
                                prefix_path.join(&hash_id).to_string_lossy().to_string()
                            };
                            resolved_files.insert(k.clone(), Value::str(full_path));
                        }
                    }
                    let mut meta_map = match meta_val.view() {
                        ValueView::Hash(m) => m.map.clone(),
                        _ => HashMap::new(),
                    };
                    meta_map.insert(
                        "files".to_string(),
                        Value::hash_with_data(Value::hash_arc(resolved_files)),
                    );
                    meta_map.insert("prefix".to_string(), Value::str(prefix.to_string()));
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "meta".to_string(),
                        Value::hash_with_data(Value::hash_arc(meta_map)),
                    );
                    attrs.insert("prefix".to_string(), Value::str(prefix.to_string()));
                    return Some(Value::make_instance_without_destroy(
                        crate::symbol::Symbol::intern("Distribution::Installation"),
                        attrs,
                    ));
                }
            }
        }
        None
    }

    pub(super) fn load_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        let (source_path, _inst_dist_json) = self
            .resolve_module_path(module)
            .ok_or_else(|| RuntimeError::unsatisfied_dependency(module))?;
        // Track operator subs exported by this module so EVAL can see them.
        if let Ok(source) = fs::read_to_string(&source_path) {
            for name in Self::extract_module_exported_operator_names(&source) {
                self.imported_operator_names.insert(name);
            }
        }
        // Detect distribution context for $?DISTRIBUTION.
        // For installed modules (inst# paths), use the dist JSON directly.
        // Otherwise fall back to META6.json detection.
        let saved_distribution = self.current_distribution.clone();
        // First try inst# installation repos (source files have no META6.json nearby)
        let inst_dist = self.detect_inst_distribution(module);
        // Classes/roles this module declares belong to its distribution too, so a
        // later OTF compile of one of their methods can resolve `$?DISTRIBUTION`
        // (e.g. a role method reads `$?DISTRIBUTION.meta` — zef's `Pluggable`).
        // Snapshot the pre-load names so we can record the dist for the new ones.
        let module_dist = inst_dist.or_else(|| Self::detect_distribution(&source_path));
        let (before_class_names, before_role_names): (
            std::collections::HashSet<String>,
            std::collections::HashSet<String>,
        ) = if module_dist.is_some() {
            (
                self.registry().classes.keys().cloned().collect(),
                self.registry().roles.keys().cloned().collect(),
            )
        } else {
            Default::default()
        };
        if let Some(dist) = &module_dist {
            self.current_distribution = Some(dist.clone());
            // Record the distribution for the module's package name
            // so OTF compilation can resolve $?DISTRIBUTION later.
            self.package_distributions
                .insert(module.to_string(), dist.clone());
            // Also record under the current runtime package (typically GLOBAL
            // for unit modules) since the interpreter's current_package may not
            // match the module name during function body evaluation.
            self.package_distributions
                .insert(self.current_package(), dist.clone());
        }
        // Save and restore the language version around module loading.
        // Each module may set its own `use v6.*` which should not leak
        // into the caller's language version.
        let saved_language_version = crate::parser::current_language_version();
        let (stmts, _precompiled) = self.parse_module_source(module, &source_path)?;
        // Validate any `package EXPORTHOW { ... }` directives before running the
        // module: a member named `<directive>::<declarator>` must use a known
        // directive (DECLARE/SUPERSEDE/COMPOSE), else X::EXPORTHOW::InvalidDirective.
        Self::validate_exporthow_directives(&stmts)?;
        if !Self::should_skip_runtime_for_use_only_module(&stmts) {
            // Module files should be compiled in a fresh GLOBAL scope, not
            // inheriting the caller's current_package.  Otherwise the compiler
            // would qualify top-level declarations inside the module file with
            // the caller's package (e.g. `Export_PackB::Export_PackA::foo`
            // instead of `Export_PackA::foo`).
            let saved_package = self.current_package();
            self.set_current_package("GLOBAL".to_string());
            // If the module file is a `unit module X` (or unit package/class),
            // record X so that `register_exported_sub` can mirror exports into
            // `unit_module_exported_subs` for tag validation.
            let unit_name = Self::detect_unit_package_name(&stmts);
            let pushed_unit = if let Some(name) = unit_name {
                self.unit_module_loading_stack.push(name);
                true
            } else {
                false
            };
            let before_function_keys: std::collections::HashSet<crate::symbol::Symbol> =
                self.registry().functions.keys().copied().collect();
            // Capture the module's compiled sub bodies (keyed by fingerprint) so a
            // caller can dispatch a `state`-bearing module sub through one shared
            // body across threads instead of re-OTF-compiling it per thread (which
            // severs the shared `state` cell). Compiled under GLOBAL, matching the
            // package the module body runs under here.
            self.capture_module_compiled_fns(&stmts);
            // Scope `?FILE` to the module path while its mainline runs, so
            // routine registration records the module as each sub's
            // `source_file` (module backtrace frames, error-reporting.t 15).
            let saved_qfile = self.env.get("?FILE").cloned();
            self.env.insert(
                "?FILE".to_string(),
                Value::str(source_path.to_string_lossy().to_string()),
            );
            let result = self.run_block(&stmts);
            match saved_qfile {
                Some(f) => {
                    self.env.insert("?FILE".to_string(), f);
                }
                None => {
                    self.env.remove("?FILE");
                }
            }
            if pushed_unit {
                self.unit_module_loading_stack.pop();
            }
            self.set_current_package(saved_package);
            result?;
            // A `sub MAIN` defined in a used module is NOT the program's MAIN
            // and must not be auto-dispatched at program end -- unless the module
            // *exported* MAIN (`proto MAIN(|) is export`, as zef's CLI does).
            self.promote_exported_main_to_global();
            let main_exported = self.exported_subs.values().any(|m| m.contains_key("MAIN"));
            Self::remove_leaked_main_routines(
                &mut self.registry_mut().functions,
                &before_function_keys,
                main_exported,
            );
        }
        // Record the module's distribution for every class/role it just declared,
        // so an OTF compile of one of their methods resolves `$?DISTRIBUTION`.
        if let Some(dist) = &module_dist {
            let new_names: Vec<String> = self
                .registry()
                .classes
                .keys()
                .filter(|k| !before_class_names.contains(*k))
                .chain(
                    self.registry()
                        .roles
                        .keys()
                        .filter(|k| !before_role_names.contains(*k)),
                )
                .cloned()
                .collect();
            for name in new_names {
                self.package_distributions
                    .entry(name)
                    .or_insert_with(|| dist.clone());
            }
        }
        crate::parser::set_current_language_version(&saved_language_version);
        self.current_distribution = saved_distribution;
        Ok(())
    }

    /// Compile a module's statements and record each resulting compiled sub body
    /// into `imported_compiled_fns`, keyed by its body/signature fingerprint. A
    /// caller resolving one of these subs (via the registry `FunctionDef`, which
    /// carries the same fingerprint) can then run the *shared* captured body
    /// instead of OTF-recompiling it — the key to cross-thread `state` sharing for
    /// module subs (see `imported_compiled_fns`). Pure compilation: runs no user
    /// code and touches no `env`. Compiles under the current package (GLOBAL, set
    /// by the caller) so the bodies match the module's own execution.
    ///
    /// The shared body is only *consulted* for a `state`-declaring module sub
    /// (`imported_state_body_for_def`), so a module with no such sub gains nothing
    /// from capture. Skip the whole (double-)compile in that common case — the vast
    /// majority of modules declare no `state` sub, and the scan is a cheap AST walk.
    fn capture_module_compiled_fns(&mut self, stmts: &[crate::ast::Stmt]) {
        if !Self::module_has_state_sub(stmts) {
            return;
        }
        let (_code, compiled_fns) = self.compile_block_raw(stmts);
        for cf in compiled_fns.into_values() {
            self.imported_compiled_fns
                .entry(cf.fingerprint)
                .or_insert_with(|| std::sync::Arc::new(cf));
        }
    }

    /// True if `stmts` declares at least one `sub`/`proto`/`multi` whose body
    /// declares a `state` variable (recursing through nested package blocks). Used
    /// to skip the shared-body capture compile for modules that cannot benefit.
    fn module_has_state_sub(stmts: &[crate::ast::Stmt]) -> bool {
        use crate::ast::Stmt;
        stmts.iter().any(|stmt| match stmt {
            Stmt::SubDecl { body, .. } | Stmt::ProtoDecl { body, .. } => {
                crate::runtime::Interpreter::function_body_declares_state(body)
            }
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Package { body, .. } => {
                Self::module_has_state_sub(body)
            }
            _ => false,
        })
    }
}
