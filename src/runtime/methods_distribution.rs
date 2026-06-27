//! CompUnit / Distribution method dispatch entry points plus the
//! `run-script` repository script loader. Candidate resolution lives in
//! `methods_distribution_cur_resolve`, install/uninstall/need in
//! `methods_distribution_cur_inst`, and the JSON parser plus small utilities in
//! `methods_distribution_helpers`.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

use super::methods_distribution_helpers::{hash_strings, parse_json_value};

impl Interpreter {
    /// Parse a JSON string into a Value::Hash.
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
        // nothing to merge later: hand back the live GLOBAL package so stash
        // navigation (`.globalish-package<Pkg>.WHO<...>`) resolves as before.
        // Only the Installation repository carries `globalish-symbols` (kept
        // hidden until `merge-symbols`), which needs the dedicated Stash object.
        let Some(symbols) = symbols else {
            return Value::Package(crate::symbol::Symbol::intern("GLOBAL"));
        };
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str_from("GLOBALish"));
        attrs.insert("globalish-symbols".to_string(), symbols.clone());
        Value::make_instance(crate::symbol::Symbol::intern("Stash"), attrs)
    }

    /// Publish symbols carried by a globalish-package into GLOBAL, making them
    /// resolvable through `::('Name')`. Used by `GLOBALish.WHO.merge-symbols`.
    pub(crate) fn merge_global_symbols(&mut self, pkg: &Value) -> Value {
        let symbols = match pkg {
            Value::Instance { attributes, .. } => {
                attributes.as_map().get("globalish-symbols").cloned()
            }
            _ => pkg.hash_get_str("globalish-symbols"),
        };
        if let Some(Value::Array(syms, _)) = symbols {
            for sym in syms.iter() {
                let name = sym.to_string_value();
                self.cur_repo.pending_global_symbols.remove(&name);
                if let Some((_, short)) = name.rsplit_once("::") {
                    self.cur_repo.pending_global_symbols.remove(short);
                }
            }
        }
        Value::Nil
    }

    /// Distribution method dispatch.
    pub(crate) fn dispatch_distribution_method(
        &self,
        _class_name: &str,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "prefix" => Some(Ok(attributes.get("prefix").cloned().unwrap_or(Value::Nil))),
            "meta" => Some(Ok(attributes.get("meta").cloned().unwrap_or(Value::Nil))),
            "content" => {
                let path_arg = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let prefix = attributes
                    .get("prefix")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                // Look up path in the files hash first (handles platform-specific library
                // names: resources/libraries/foo -> $prefix/resources/libraries/libfoo.so)
                let files_value = attributes
                    .get("files")
                    .cloned()
                    .or_else(|| attributes.get("meta").and_then(|m| m.hash_get_str("files")));
                let actual_path = files_value
                    .and_then(|files| files.hash_get_str(&path_arg))
                    .map(|v| v.to_string_value());
                let full_path = if let Some(abs_path) = actual_path {
                    if std::path::Path::new(&abs_path).is_absolute() {
                        abs_path
                    } else {
                        std::path::Path::new(&prefix)
                            .join(&abs_path)
                            .to_string_lossy()
                            .to_string()
                    }
                } else {
                    std::path::Path::new(&prefix)
                        .join(&path_arg)
                        .to_string_lossy()
                        .to_string()
                };
                Some(Ok(self.make_io_path_instance(&full_path)))
            }
            "Str" | "gist" => {
                let name = attributes
                    .get("meta")
                    .and_then(|m| m.hash_get_str("name"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "Distribution".to_string());
                Some(Ok(Value::str(name)))
            }
            _ => None,
        }
    }

    /// CUR::Installation method dispatch.
    pub(crate) fn dispatch_cur_installation_method(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let prefix = attributes
            .get("prefix")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        match method {
            "candidates" => {
                let depspec = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.cur_inst_candidates(&prefix, &depspec))
            }
            "install" => {
                let dist = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.cur_inst_install(&prefix, &dist))
            }
            "uninstall" => {
                let dist = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.cur_inst_uninstall(&prefix, &dist))
            }
            "installed" => Some(self.cur_inst_installed(&prefix)),
            "path-spec" => Some(Ok(Value::str(format!("inst#{prefix}")))),
            "short-id" => Some(Ok(Value::str_from("inst"))),
            "id" => {
                // A stable, non-empty identifier derived from the repo prefix.
                Some(Ok(Value::str(format!("{:X}", hash_strings(&[&prefix])))))
            }
            "loaded" => {
                let loaded = self
                    .cur_repo
                    .loaded
                    .get(&prefix)
                    .cloned()
                    .unwrap_or_default();
                Some(Ok(Value::array(loaded)))
            }
            "next-repo" => Some(Ok(attributes
                .get("next-repo")
                .cloned()
                .unwrap_or(Value::Nil))),
            "prefix" => Some(Ok(self.make_io_path_instance(&prefix))),
            "need" => Some(self.cur_inst_need(&prefix, args.first().cloned())),
            "resolve" => {
                let depspec = args.first().cloned().unwrap_or(Value::Nil);
                match self.cur_inst_candidates(&prefix, &depspec) {
                    Ok(Value::Array(arr, _)) => {
                        if arr.is_empty() {
                            Some(Ok(Value::Nil))
                        } else {
                            Some(Ok(Value::Bool(true)))
                        }
                    }
                    Ok(_) => Some(Ok(Value::Nil)),
                    Err(e) => Some(Err(e)),
                }
            }
            "files" => {
                let search_path = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let prefix_path = std::path::Path::new(&prefix);
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
                            && files.hash_get_str(&search_path).is_some()
                        {
                            let dist_id = path
                                .file_stem()
                                .unwrap_or_default()
                                .to_string_lossy()
                                .to_string();
                            let mut attrs = HashMap::new();
                            attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
                            attrs.insert("meta".to_string(), meta);
                            attrs.insert("dist-id".to_string(), Value::str(dist_id));
                            results.push(Value::make_instance(
                                crate::symbol::Symbol::intern("Distribution::Installation"),
                                attrs,
                            ));
                        }
                    }
                }
                Some(Ok(Value::array(results)))
            }
            _ => None,
        }
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
        let lib_paths_snapshot: Vec<String> = self.lib_paths.clone();
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
                                if !self.lib_paths.contains(&sources_dir) {
                                    self.lib_paths.push(format!("inst#{prefix}"));
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
            if !self.lib_paths.contains(&inst_path) {
                self.lib_paths.push(inst_path);
            }
        }
        // Parse the script
        let (stmts, _) = crate::parser::parse_program(&code).map_err(|e| {
            RuntimeError::new(format!("Parse error in script {path_str}: {}", e.message))
        })?;
        // Set the file context
        let saved_file = self.env.get("?FILE").cloned();
        self.env
            .insert("?FILE".to_string(), Value::str(path_str.clone()));
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
        let empty: std::collections::HashMap<String, crate::opcode::CompiledFunction> =
            std::collections::HashMap::new();
        let dispatch_result = self.dispatch_main(&empty);
        // Restore the function registry to prevent MAIN from leaking into parent scope
        self.restore_routine_registry_eval(registry_snapshot);
        dispatch_result.map_err(|e| RuntimeError::new(e.message))?;
        Ok(Value::Nil)
    }
}
