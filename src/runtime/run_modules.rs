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

        // Walk `lib_paths` ONCE, in order. Plain directories (`use lib`, `-I`,
        // `MUTSULIB`) and installed repositories (`inst#`, appended by
        // `add_default_site_repo`) share a single precedence chain, exactly like
        // Raku's repository chain. Resolving every `inst#` entry up front — as
        // this used to — inverts that chain, so an installed module shadowed an
        // explicit `-I` path, which is the one thing the flag exists to prevent.
        let mut had_plain_lib_path = false;
        for base in &self.lib_paths {
            if let Some(prefix) = base.strip_prefix("inst#") {
                if let Some(found) = self.resolve_in_inst_repo(prefix, module) {
                    return Some(found);
                }
                continue; // Don't try inst# path as a filesystem path
            }
            had_plain_lib_path = true;
            let base_path = Path::new(base.as_str());
            for ext in &extensions {
                let filename = format!("{}{}", base_name, ext);
                for candidate in [
                    base_path.join(&filename),
                    base_path.join("lib").join(&filename),
                ] {
                    if candidate.exists() {
                        return Some((candidate, None));
                    }
                }
            }
        }

        let mut candidates: Vec<std::path::PathBuf> = Vec::new();
        if !had_plain_lib_path
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
        // Bundled batteries are the lowest-priority source: append their
        // candidates last so an explicit `-I`/`MUTSULIB`/project-local module or
        // an `mzef`-installed (site-repo) version always shadows the bundled copy
        // (BATTERIES.md §3/§6).
        for base in &self.bundled_lib_paths {
            let base_path = Path::new(base.as_str());
            for ext in &extensions {
                let filename = format!("{}{}", base_name, ext);
                candidates.push(base_path.join(&filename));
            }
        }
        candidates
            .into_iter()
            .find(|path| path.exists())
            .map(|p| (p, None))
    }

    /// Resolve `module` inside ONE installed repository (`inst#<prefix>`), whose
    /// sources live at `<prefix>/sources/<file_id>` and whose dist metadata lives
    /// in `<prefix>/dist/*.json`. Returns the source path plus the dist JSON it
    /// came from, or `None` when this repository cannot satisfy the request — in
    /// which case the caller moves on to the next link of the search chain.
    ///
    /// Several installed dists may provide the same short name (two JSON::Class
    /// dists exist on fez, by different authors), so every candidate *within this
    /// repository* is collected, filtered by the `use` statement's dist selectors
    /// (`:ver`/`:auth`/`:api`, in `pending_dist_selectors`), and the highest
    /// version wins. That candidate selection is per-repository and is unrelated
    /// to path precedence: it must not reach across repositories.
    fn resolve_in_inst_repo(
        &self,
        prefix: &str,
        module: &str,
    ) -> Option<(std::path::PathBuf, Option<String>)> {
        let prefix_path = Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        if !dist_dir.is_dir() {
            return None;
        }
        let entries = std::fs::read_dir(&dist_dir).ok()?;
        let mut candidates: Vec<(std::path::PathBuf, String)> = Vec::new();
        let mut unmatched_metas: Vec<String> = Vec::new();
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("json") {
                continue;
            }
            let Ok(json_str) = std::fs::read_to_string(&path) else {
                continue;
            };
            if let Some(file_id) = Self::find_module_file_id_in_dist_json(&json_str, module) {
                let source_path = prefix_path.join("sources").join(&file_id);
                if source_path.exists() {
                    candidates.push((source_path, json_str));
                    continue;
                }
            }
            unmatched_metas.push(json_str);
        }
        if !candidates.is_empty() {
            return self
                .select_dist_candidate(candidates, &self.pending_dist_selectors)
                .map(|(source_path, json_str)| (source_path, Some(json_str)));
        }
        // `find_module_file_id_in_dist_json` is a hand-rolled scan that only
        // understands `"provides": {"M": {"file": id}}`. Fall back to a real JSON
        // parse for dists that spell `provides` differently (a bare string value).
        for json_str in unmatched_metas {
            let Ok(meta) = self.parse_json_to_value(&json_str) else {
                continue;
            };
            let Some(provides) = meta.hash_get_str("provides") else {
                continue;
            };
            let Some(entry_val) = provides.hash_get_str(module) else {
                continue;
            };
            // entry_val is either {"file": "hash_id"} or just a string
            let hash_id = match entry_val.view() {
                ValueView::Hash(map) => map
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
                _ => entry_val.to_string_value(),
            };
            if hash_id.is_empty() {
                continue;
            }
            let source_path = prefix_path.join("sources").join(&hash_id);
            if source_path.exists() {
                return Some((source_path, None));
            }
        }
        None
    }

    /// Pick one installed dist among several that provide the requested module:
    /// filter by the `use` statement's dist selectors (`:auth` = exact match,
    /// `:ver`/`:api` = Version smartmatch, so `0.0.14+` means at-least), then
    /// take the highest version. Returns None when selectors exclude them all.
    fn select_dist_candidate(
        &self,
        candidates: Vec<(std::path::PathBuf, String)>,
        selectors: &[(String, String)],
    ) -> Option<(std::path::PathBuf, String)> {
        let mut best: Option<(std::path::PathBuf, String, Vec<crate::value::VersionPart>)> = None;
        for (path, json_str) in candidates {
            let meta: serde_json::Value = serde_json::from_str(&json_str).unwrap_or_default();
            let meta_str = |key: &str| -> String {
                match &meta[key] {
                    serde_json::Value::String(s) => s.clone(),
                    serde_json::Value::Number(n) => n.to_string(),
                    _ => String::new(),
                }
            };
            let mut matches = true;
            for (key, want) in selectors {
                let have = meta_str(key);
                match key.as_str() {
                    "auth" => {
                        if have != *want {
                            matches = false;
                        }
                    }
                    "ver" | "api" => {
                        // The dist JSON stores the version under "version"
                        // (zef/S22 meta), while the api is under "api".
                        let have = if key == "ver" {
                            meta_str("version")
                        } else {
                            have
                        };
                        let (want_parts, plus, minus) = Value::parse_version_string(want);
                        let (have_parts, _, _) = Value::parse_version_string(&have);
                        let have_val = Value::version(have_parts, false, false);
                        if !Self::version_smart_match(&have_val, &want_parts, plus, minus) {
                            matches = false;
                        }
                    }
                    _ => {}
                }
            }
            if !matches {
                continue;
            }
            let (ver_parts, _, _) = Value::parse_version_string(&meta_str("version"));
            let better = match &best {
                None => true,
                Some((_, _, best_parts)) => {
                    crate::runtime::version_cmp_parts(&ver_parts, best_parts)
                        == std::cmp::Ordering::Greater
                }
            };
            if better {
                best = Some((path, json_str, ver_parts));
            }
        }
        best.map(|(p, j, _)| (p, j))
    }

    /// Default install directory for a well-known repository name
    /// ("site"/"home"/"vendor"/"perl"), mirroring the XDG pattern used by
    /// `precomp.rs::cache_dir()`. Returns `None` (no default available) rather
    /// than erroring when `$HOME`/`XDG_DATA_HOME` can't be determined; callers
    /// must not assume the directory exists yet.
    /// Resolve the bundled-battery module search paths (`<bundle>/<Dist>/lib`).
    /// The bundle base is `$MUTSU_BUNDLE_DIR` if set, else discovered relative to
    /// the running binary: `share/mutsu/modules` next to `bin/` (release tarball
    /// / container layout), `modules/` two levels up (a `target/<profile>/mutsu`
    /// dev build), or `modules/` beside the binary. Each dist's `lib/` directory
    /// that exists is returned; a missing bundle yields an empty list (the
    /// interpreter simply has no bundled batteries).
    pub(crate) fn resolve_bundled_lib_paths() -> Vec<String> {
        use std::path::PathBuf;
        let base: Option<PathBuf> = if let Ok(dir) = std::env::var("MUTSU_BUNDLE_DIR") {
            Some(PathBuf::from(dir))
        } else {
            std::env::current_exe().ok().and_then(|exe| {
                let dir = exe.parent()?.to_path_buf();
                [
                    dir.join("..").join("share").join("mutsu").join("modules"),
                    dir.join("..").join("..").join("modules"),
                    dir.join("modules"),
                ]
                .into_iter()
                .find(|c| c.is_dir())
            })
        };
        let Some(base) = base.filter(|b| b.is_dir()) else {
            return Vec::new();
        };
        let Ok(entries) = std::fs::read_dir(&base) else {
            return Vec::new();
        };
        let mut paths: Vec<String> = Vec::new();
        for entry in entries.flatten() {
            let lib = entry.path().join("lib");
            if lib.is_dir() {
                paths.push(lib.display().to_string());
            }
        }
        paths.sort();
        paths
    }

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

    /// Extract operator sub names (infix:<..>, prefix:<..>, etc.) that a
    /// module exports with `is export` (DEFAULT or MANDATORY tag). Used by
    /// `load_module` to populate `imported_operator_names` so EVAL can see
    /// operators from imported modules without seeing non-exported subs.
    /// Walks the already-parsed statements (fresh or from the precomp cache)
    /// rather than re-reading and re-parsing the source file.
    fn extract_module_exported_operator_names(stmts: &[crate::ast::Stmt]) -> Vec<String> {
        let mut out = Vec::new();
        for stmt in stmts {
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

    /// Parse a module source file, using the precompilation cache when available.
    /// Returns (stmts, was_precompiled).
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

        // Try loading from precompilation cache when eligible. A hit skips the
        // parse, so the parser state the parse would have left behind must be
        // replayed from the entry — otherwise the module's mainline runs under
        // the *importer's* language revision and the module's warnings never
        // appear. See `precomp::ParseEffects`.
        if precomp_eligible
            && let Some(unit) = crate::precomp::load_cached_unit(source_path, Some(&code))
        {
            crate::parser::set_current_language_version(&unit.effects.language_version);
            for warning in &unit.effects.warnings {
                self.write_warn_to_stderr(warning);
            }
            return Ok((unit.stmts, true));
        }

        let preprocessed = Self::maybe_preprocess_roast_directives(&code);
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let result = parse_dispatch::parse_compilation_unit(&preprocessed);
        crate::parser::clear_parser_lib_paths();
        // Capture exactly what a later cache hit will have to replay, before
        // anything downstream can disturb it.
        let effects = crate::precomp::ParseEffects {
            language_version: crate::parser::current_language_version(),
            warnings: crate::parser::take_parse_warnings(),
        };
        for warning in &effects.warnings {
            self.write_warn_to_stderr(warning);
        }
        // `unit class`/`unit role`/`unit grammar` bodies are already merged at
        // parse time by the statement-list unit-capture (see
        // `parser::stmt::stmtlist`), so no post-parse surgery is needed here.
        let mut stmts = result.map(|(stmts, _)| stmts).map_err(|mut err| {
            err.message = format!("Failed to parse module '{}': {}", module, err.message);
            err
        })?;
        // A module that uses NativeCall and references `Pointer` needs the
        // builtin `Pointer` prelude class too — the main-program injection only
        // sees the main source, so a NativeCall binding distributed as a module
        // would otherwise hit an undeclared `Pointer`.
        Self::inject_nativecall_prelude(&preprocessed, &mut stmts);
        Self::inject_cglobal_prelude(&preprocessed, &mut stmts);
        Self::inject_nativecall_manage_prelude(&preprocessed, &mut stmts);
        Self::inject_iosocket_prelude(&preprocessed, &mut stmts);

        // Save to precompilation cache when the module is eligible.
        if precomp_eligible {
            crate::precomp::save_cached_unit(source_path, &stmts, &effects);
        }

        Ok((stmts, precomp_eligible))
    }

    /// Return the name of a top-level `unit module/package/class` statement
    /// in `stmts`, if any. Used by `load_module` to track which unit module
    /// is currently loading so exports can be mirrored under the module name.
    pub(crate) fn detect_unit_package_name(stmts: &[crate::ast::Stmt]) -> Option<String> {
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
            // Skip plain directories rather than giving up on the whole search:
            // `-I` paths normally sit in front of the site repository, so bailing
            // out at the first non-`inst#` entry meant this never looked at the
            // installed repositories at all.
            let Some(prefix) = base.strip_prefix("inst#") else {
                continue;
            };
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
                let Ok(meta_val) = self.parse_json_to_value(&json_str) else {
                    continue;
                };
                if let Some(provides) = meta_val.hash_get_str("provides")
                    && provides.hash_get_str(module).is_some()
                {
                    return Some(Self::build_inst_distribution(prefix, &meta_val));
                }
            }
        }
        None
    }

    /// Build a `Distribution::Installation` instance from an installed dist's
    /// meta, with its "files" entries resolved to absolute paths under `prefix`.
    fn build_inst_distribution(prefix: &str, meta_val: &Value) -> Value {
        use std::collections::HashMap;
        let prefix_path = Path::new(prefix);
        let resources_dir = prefix_path.join("resources");
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
        Value::make_instance_without_destroy(
            crate::symbol::Symbol::intern("Distribution::Installation"),
            attrs,
        )
    }

    pub(super) fn load_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        // Snapshot the `use` args (set by `exec_use_module_op`) before running
        // the module body: a transitive `use` inside the body would otherwise
        // overwrite the field. Handed to the module's `sub EXPORT`, if any.
        let export_args = self.pending_use_export_args.take();
        let (source_path, inst_dist_json) = self
            .resolve_module_path(module)
            .ok_or_else(|| RuntimeError::unsatisfied_dependency(module))?;
        // Detect distribution context for $?DISTRIBUTION.
        // For installed modules (inst# paths), use the dist JSON directly.
        // Otherwise fall back to META6.json detection.
        let saved_distribution = self.current_distribution.clone();
        let saved_distribution_floor = self.current_distribution_frame_floor;
        // Prefer the dist JSON of the distribution resolve_module_path actually
        // selected (selectors / highest-version pick): a by-name rescan could
        // land on a DIFFERENT dist that also provides this short name. The
        // source lives at <prefix>/sources/<id>, so prefix is two levels up.
        let inst_dist = inst_dist_json
            .and_then(|json_str| {
                let prefix = source_path.parent()?.parent()?;
                let meta_val = self.parse_json_to_value(&json_str).ok()?;
                Some(Self::build_inst_distribution(
                    &prefix.to_string_lossy(),
                    &meta_val,
                ))
            })
            .or_else(|| self.detect_inst_distribution(module));
        // Classes/roles this module declares belong to its distribution too, so a
        // later OTF compile of one of their methods can resolve `$?DISTRIBUTION`
        // (e.g. a role method reads `$?DISTRIBUTION.meta` — zef's `Pluggable`).
        // The same set is what the module's imported type aliases are recorded
        // against (`package_type_aliases`), so snapshot the pre-load names
        // unconditionally.
        let module_dist = inst_dist.or_else(|| Self::detect_distribution(&source_path));
        let (before_class_names, before_role_names): (
            std::collections::HashSet<String>,
            std::collections::HashSet<String>,
        ) = (
            self.registry().classes.keys().cloned().collect(),
            self.registry().roles.keys().cloned().collect(),
        );
        if let Some(dist) = &module_dist {
            self.current_distribution = Some(dist.clone());
            // Everything this module's own mainline runs from here on sits at or
            // above this frame height; frames below belong to whoever triggered
            // the load (see `build_resources_for_package`).
            self.current_distribution_frame_floor = self.routine_stack_len();
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
        // Track operator subs exported by this module so EVAL can see them.
        for name in Self::extract_module_exported_operator_names(&stmts) {
            self.imported_operator_names.insert(name);
        }
        // Validate any `package EXPORTHOW { ... }` directives before running the
        // module: a member named `<directive>::<declarator>` must use a known
        // directive (DECLARE/SUPERSEDE/COMPOSE), else X::EXPORTHOW::InvalidDirective.
        Self::validate_exporthow_directives(&stmts)?;
        let mut module_scope_names: HashMap<String, Value> = HashMap::new();
        let mut module_type_aliases: HashMap<String, String> = HashMap::new();
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
            // See `package_type_aliases`: the module body runs in the CALLER's env,
            // so the short-name type aliases its own `use` statements install are
            // only as long-lived as the frame that triggered the load. Snapshot the
            // env so the new ones can be recorded against the module itself.
            let before_env_keys: std::collections::HashSet<crate::symbol::Symbol> =
                self.env.keys().copied().collect();
            let saved_imports = std::mem::take(&mut self.module_imported_names);
            let result = self.run_block(&stmts);
            let imported = std::mem::replace(&mut self.module_imported_names, saved_imports);
            module_scope_names = self.collect_module_scope_names(&before_env_keys);
            // A re-import of a name an earlier module already installed adds
            // nothing to `env`, so the diff misses it even though it is part of
            // this module's scope (see `module_imported_names`).
            module_scope_names.extend(imported);
            module_type_aliases = self.module_type_aliases_of(&module_scope_names);
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
            // Invalidate name-keyed resolution caches.
            self.fn_resolve_gen += 1;
            // If the module defined `sub EXPORT`, call it with the `use` args and
            // install the symbols it returns into the caller's scope.
            self.apply_module_export(export_args.unwrap_or_default())?;
        }
        // Record the module's distribution for every class/role it just declared,
        // so an OTF compile of one of their methods resolves `$?DISTRIBUTION`, and
        // its own file-scope bare names (imported type aliases, `constant`s)
        // against the same set, so a routine of one of those classes can still
        // resolve them once the frame that ran the `require` is gone (see
        // `package_type_aliases` / `module_scope_lexicals`).
        if module_dist.is_some() || !module_scope_names.is_empty() {
            let mut owners: Vec<String> = self
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
            if let Some(dist) = &module_dist {
                for name in &owners {
                    self.package_distributions
                        .entry(name.clone())
                        .or_insert_with(|| dist.clone());
                }
            }
            owners.push(module.to_string());
            for owner in owners {
                if !module_type_aliases.is_empty() {
                    self.package_type_aliases
                        .entry(owner.clone())
                        .or_default()
                        .extend(
                            module_type_aliases
                                .iter()
                                .map(|(k, v)| (k.clone(), v.clone())),
                        );
                }
                self.module_scope_lexicals.entry(owner).or_default().extend(
                    module_scope_names
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone())),
                );
            }
        }
        crate::parser::set_current_language_version(&saved_language_version);
        self.current_distribution = saved_distribution;
        self.current_distribution_frame_floor = saved_distribution_floor;
        Ok(())
    }

    /// The file-scope bare names a module body just installed into `env`: entries
    /// added since `before` under a plain (sigilless, unqualified) name. Those are
    /// the module's own lexical scope — the short-name type aliases its `use`
    /// statements imported, plus its `constant`s and sigilless declarations. See
    /// `package_type_aliases` / `module_scope_lexicals` for why they cannot be left to
    /// live in `env`.
    fn collect_module_scope_names(
        &self,
        before: &std::collections::HashSet<crate::symbol::Symbol>,
    ) -> HashMap<String, Value> {
        let mut names = HashMap::new();
        for key in self.env.keys() {
            if before.contains(key) {
                continue;
            }
            let name = key.resolve();
            // Twigils, qualified names and the `__mutsu_*` / `?FILE`-style
            // metadata keys are never plain module-scope declarations. A scalar
            // `my $x` is stored sigil-less (key `x`); `@`/`%` keep their sigil.
            // `&` names are routines and have the registry, so they stay out.
            let bare = name.strip_prefix(['@', '%']).unwrap_or(name.as_str());
            if name.contains("::")
                || !bare
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
            {
                continue;
            }
            if let Some(value) = self.env.get_sym(*key) {
                names.insert(name.to_string(), value.clone());
            }
        }
        names
    }

    /// The subset of [`Self::collect_module_scope_names`] that are short-name type
    /// aliases: a `Package` naming a *different* registered type
    /// (`THING2 => Drv2::Native::THING2`).
    fn module_type_aliases_of(&self, scope: &HashMap<String, Value>) -> HashMap<String, String> {
        scope
            .iter()
            .filter_map(|(name, value)| {
                let ValueView::Package(target) = value.view() else {
                    return None;
                };
                let target = target.resolve();
                (target != *name && self.has_type_direct(&target))
                    .then(|| (name.clone(), target.to_string()))
            })
            .collect()
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
