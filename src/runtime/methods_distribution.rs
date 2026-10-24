//! CompUnit / Distribution method dispatch entry points plus the
//! `run-script` repository script loader. Candidate resolution lives in
//! `methods_distribution_cur_resolve`, install/uninstall/need in
//! `methods_distribution_cur_inst`, and the JSON parser plus small utilities in
//! `methods_distribution_helpers`.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};
use std::collections::HashMap;

use super::methods_distribution_helpers::{hash_strings, parse_json_value};

impl Interpreter {
    /// Parse a JSON string into a Hash.
    pub(crate) fn parse_json_to_value(&self, json_str: &str) -> Result<Value, RuntimeError> {
        let trimmed = json_str.trim();
        let (val, _) = parse_json_value(trimmed)
            .map_err(|e| RuntimeError::new(format!("Cannot parse JSON: {e}")))?;
        Ok(val)
    }

    /// Build the "globalish package" object exposed by a CompUnit handle. It
    /// carries the list of symbol names the CompUnit loaded so that a later
    /// `GLOBALish.WHO.merge-symbols(...)` can publish them into GLOBAL.
    pub(crate) fn make_globalish_package(&self, symbols: Option<&Value>) -> Value {
        // File-system repositories load symbols straight into GLOBAL, so there is
        // nothing to merge later: hand back GLOBAL's own stash (an Associative
        // Instance, same as `GLOBAL.WHO`) so a direct
        // `.globalish-package<Pkg>` navigates like `GLOBAL.WHO<Pkg>` does,
        // rather than the bare `GLOBAL` *type object* -- `Package<...>` is an
        // ordinary (Any-returning) postcircumfix subscript in Raku, never
        // navigation (#8295); only a real Associative stash resolves `<Pkg>`
        // to the nested package. Only the Installation repository carries
        // `globalish-symbols` (kept hidden until `merge-symbols`), which needs
        // the dedicated Stash object below instead.
        let Some(symbols) = symbols else {
            return self.globalish_stash_value();
        };
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str_from("GLOBALish"));
        attrs.insert("globalish-symbols".to_string(), symbols.clone());
        Value::make_instance(crate::symbol::Symbol::intern("Stash"), attrs)
    }

    /// Publish symbols carried by a globalish-package into GLOBAL, making them
    /// resolvable through `::('Name')`. Used by `GLOBALish.WHO.merge-symbols`.
    pub(crate) fn merge_global_symbols(&mut self, pkg: &Value) -> Value {
        let symbols = match pkg.view() {
            ValueView::Instance { attributes, .. } => {
                attributes.as_map().get("globalish-symbols").cloned()
            }
            _ => pkg.hash_get_str("globalish-symbols"),
        };
        if let Some(ValueView::Array(syms, _)) = symbols.as_ref().map(Value::view) {
            for sym in syms.iter() {
                let name = sym.to_string_value();
                self.module.cur_repo.pending_global_symbols.remove(&name);
                if let Some((_, short)) =
                    crate::qualified::split_qualified(crate::qualified::known_symbol(&name))
                        .map(|(head, tail)| (head.as_str(), tail.as_str()))
                {
                    self.module.cur_repo.pending_global_symbols.remove(short);
                }
            }
        }
        Value::NIL
    }

    /// `CompUnit::Repository::Installation.install` / `.uninstall`: both take
    /// `Distribution $distribution`; an untyped value must never reach the
    /// repository (`site` is a real store).
    pub(crate) fn cur_inst_install_checked(
        &mut self,
        prefix: &str,
        dist: &Value,
        uninstall: bool,
        force: bool,
    ) -> Result<Value, RuntimeError> {
        if !self.type_matches_value("Distribution", dist) {
            return Err(self.typecheck_binding_parameter_failure(
                "$distribution",
                "Distribution",
                dist,
            ));
        }
        if uninstall {
            self.cur_inst_uninstall(prefix, dist)
        } else {
            self.cur_inst_install(prefix, dist, force)
        }
    }

    /// `CompUnit::Repository::Installation.installed`: the installed
    /// distributions, and for the `core` repository the bundled core one.
    pub(crate) fn cur_inst_installed_with_core(
        &mut self,
        prefix: &str,
    ) -> Result<Value, RuntimeError> {
        let installed = self.cur_inst_installed(prefix)?;
        if self.is_core_repo_prefix(prefix)
            && let Some(core) = self.bundled_core_distribution()
            && let Some((items, _)) = installed.clone().into_array()
        {
            let mut items = items.to_vec();
            items.push(core);
            return Ok(Value::array(items));
        }
        Ok(installed)
    }

    /// `CompUnit::Repository::Installation.can-install`, as Rakudo's: writable
    /// if the prefix is writable, or it does not yet exist and it can be
    /// *created* -- i.e. the nearest already-existing ancestor directory is
    /// writable. Checking only the immediate parent is wrong when several path
    /// components are missing at once (mutsu's default `home`/`site` repos live
    /// under a not-yet-created `~/.local/share/mutsu/repo/`; `zef install`'s
    /// `auto` target selection then found no installable repo and died with
    /// "Need a valid installation target to continue").
    pub(crate) fn cur_inst_can_install(prefix: &str) -> bool {
        use crate::runtime::native_io::path_is_writable;
        let prefix_path = std::path::Path::new(prefix);
        if path_is_writable(prefix_path) {
            return true;
        }
        if prefix_path.exists() {
            // Exists but not writable.
            return false;
        }
        // Walk up to the nearest existing ancestor and check whether the
        // missing prefix could be created under it.
        let mut ancestor = prefix_path.parent();
        loop {
            match ancestor {
                Some(dir) if dir.exists() => return path_is_writable(dir),
                Some(dir) => ancestor = dir.parent(),
                None => return false,
            }
        }
    }

    /// `CompUnit::Repository::Installation.id`: a stable, non-empty
    /// identifier derived from the repository prefix.
    pub(crate) fn cur_inst_id(prefix: &str) -> String {
        format!("{:X}", hash_strings(&[prefix]))
    }

    /// `CompUnit::Repository::Installation.loaded`: the compunits this
    /// repository loaded.
    pub(crate) fn cur_inst_loaded(&self, prefix: &str) -> Value {
        let loaded = self
            .module
            .cur_repo
            .loaded
            .get(prefix)
            .cloned()
            .unwrap_or_default();
        Value::array(loaded)
    }

    /// `CompUnit::Repository::Installation.files($file)`: the installed
    /// distributions whose `files` include `search_path`.
    pub(crate) fn cur_inst_files(&self, prefix: &str, search_path: &str) -> Value {
        let prefix_path = std::path::Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        let mut results = Vec::new();
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
                if let Some(files) = meta.hash_get_str("files")
                    && files.hash_get_str(search_path).is_some()
                {
                    let dist_id = path
                        .file_stem()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_string();
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(prefix));
                    attrs.insert("meta".to_string(), meta);
                    attrs.insert("dist-id".to_string(), Value::str(dist_id));
                    results.push(Value::make_instance(
                        crate::symbol::Symbol::intern("CompUnit::Repository::Installation::LazyDistribution"),
                        attrs,
                    ));
                }
            }
        }
        Value::array(results)
    }

    /// `CompUnit::Repository.repo-chain`: `repo` followed by every repository
    /// its `next-repo` links reach.
    pub(crate) fn repo_chain(&self, repo: &Value) -> Value {
        let mut chain = Vec::new();
        let mut next = repo.clone();
        while next.truthy() {
            let following = match next.view() {
                ValueView::Instance { attributes, .. } => {
                    self.repo_next_link(&next, &attributes.as_map())
                }
                _ => Value::NIL,
            };
            chain.push(next);
            next = following;
        }
        Value::array(chain)
    }
    /// Implement CompUnit::RepositoryRegistry.run-script($name).
    /// Searches lib_paths for an installed script by name (in bin/ or resources/bin/),
    /// then loads and executes it.
    pub(crate) fn run_script_from_repos(
        &mut self,
        script_name: &str,
    ) -> Result<Value, RuntimeError> {
        let bin_key = format!("bin/{script_name}");
        // Collect candidate script paths from the known lib_paths
        let lib_paths_snapshot: Vec<String> = (*self.module.lib_paths).clone();
        for p in &lib_paths_snapshot {
            if let Some(prefix) = p.strip_prefix("inst#") {
                // Installation repo: scripts are stored as {prefix}/bin/{hash_id}
                // Look in the dist JSON files to find the hash for this script name
                let prefix_path = std::path::Path::new(prefix);
                let dist_dir = prefix_path.join("dist");
                let bin_dir = prefix_path.join("bin");
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
                        if let Some(files) = meta.hash_get_str("files")
                            && let Some(hash_id_val) = files.hash_get_str(&bin_key)
                        {
                            let hash_id = hash_id_val.to_string_value();
                            let script_path = bin_dir.join(&hash_id);
                            if script_path.exists() {
                                // Also add sources dir to lib_paths so 'use Module' works
                                let sources_dir =
                                    prefix_path.join("sources").to_string_lossy().to_string();
                                if !self.module.lib_paths.contains(&sources_dir) {
                                    crate::runtime::cow_table_mut(&mut self.module.lib_paths)
                                        .push(format!("inst#{prefix}"));
                                }
                                return self.load_and_run_script(&script_path);
                            }
                        }
                    }
                }
            } else {
                // FileSystem repo: look in sibling bin/ dir
                let path = std::path::Path::new(p.as_str());
                let mut candidate_dirs = Vec::new();
                if let Some(parent) = path.parent() {
                    candidate_dirs.push(parent.join("bin"));
                }
                candidate_dirs.push(path.join("bin"));
                for dir in &candidate_dirs {
                    let script_path = dir.join(script_name);
                    if script_path.exists() {
                        return self.load_and_run_script(&script_path);
                    }
                }
            }
        }
        Err(RuntimeError::new(format!(
            "Cannot find script '{script_name}' in any repository"
        )))
    }

    fn load_and_run_script(
        &mut self,
        script_path: &std::path::Path,
    ) -> Result<Value, RuntimeError> {
        let code = std::fs::read_to_string(script_path).map_err(|e| {
            RuntimeError::new(format!("Cannot read script {}: {e}", script_path.display()))
        })?;
        let path_str = script_path.to_string_lossy().to_string();
        // Add the inst# prefix to lib_paths so 'use Module' works from within the script.
        // The bin script's parent is {prefix}/bin, so parent.parent() = {prefix}.
        // We keep the inst# prefix so resolve_module_path can find installed sources.
        if let Some(bin_parent) = script_path.parent().and_then(|p| p.parent()) {
            let inst_path = format!("inst#{}", bin_parent.to_string_lossy());
            if !self.module.lib_paths.contains(&inst_path) {
                crate::runtime::cow_table_mut(&mut self.module.lib_paths).push(inst_path);
            }
        }
        // Parse the script
        let (stmts, _) = crate::parser::parse_program(&code).map_err(|e| {
            RuntimeError::new(format!("Parse error in script {path_str}: {}", e.message))
        })?;
        // Set the file context
        let saved_file = self.env.get("?FILE").cloned();
        self.env.insert("?FILE".to_string(), Value::str(path_str));
        // Snapshot the function registry before loading the script so we can restore it
        // after MAIN dispatch (preventing MAIN from leaking into the parent scope).
        let registry_snapshot = self.snapshot_routine_registry();
        // Preregister top-level subs so they're available for MAIN dispatch
        self.preregister_top_level_subs(&stmts)
            .map_err(|e| RuntimeError::new(e.message))?;
        // Run the script body (without state restoration, so MAIN stays registered)
        let result = self.run_block(&stmts);
        if let Some(prev) = saved_file {
            self.env.insert("?FILE".to_string(), prev);
        } else {
            self.env.remove("?FILE");
        }
        result?;
        // Dispatch MAIN if defined, with @*ARGS
        let empty = crate::opcode::CompiledFns::default();
        let dispatch_result = self.dispatch_main(&empty);
        // Restore the function registry to prevent MAIN from leaking into parent scope
        self.restore_routine_registry_eval(registry_snapshot);
        dispatch_result.map_err(|e| RuntimeError::new(e.message))?;
        Ok(Value::NIL)
    }
}
