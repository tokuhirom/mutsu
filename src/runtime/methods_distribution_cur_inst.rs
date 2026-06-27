//! CompUnit::Repository::Installation mutations: installing, uninstalling,
//! listing installed distributions, and resolving+loading a `need`ed
//! dependency. See `methods_distribution_cur_resolve` for candidate resolution
//! and `methods_distribution` for the method dispatch entry points.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

use super::methods_distribution_helpers::{
    dist_identity, hash_strings, platform_library_name, value_to_json_string,
};

impl Interpreter {
    /// Install a distribution.
    pub(crate) fn cur_inst_install(
        &self,
        prefix: &str,
        dist: &Value,
    ) -> Result<Value, RuntimeError> {
        let prefix_path = std::path::Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        let sources_dir = prefix_path.join("sources");
        let resources_dir = prefix_path.join("resources");
        let bin_dir = prefix_path.join("bin");
        for dir in [&dist_dir, &sources_dir, &resources_dir, &bin_dir] {
            std::fs::create_dir_all(dir).ok();
        }
        let (meta, dist_prefix) = match dist {
            Value::Instance { attributes, .. } => {
                let m = attributes
                    .as_map()
                    .get("meta")
                    .cloned()
                    .unwrap_or(Value::Nil);
                let p = attributes
                    .as_map()
                    .get("prefix")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                (m, p)
            }
            _ => return Ok(Value::Bool(false)),
        };
        // Reject re-installing an identical distribution (same name/ver/auth/api).
        let new_identity = dist_identity(&meta);
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
                let Ok(existing_meta) = self.parse_json_to_value(&json_str) else {
                    continue;
                };
                if dist_identity(&existing_meta) == new_identity {
                    let name = meta
                        .hash_get_str("name")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Err(RuntimeError::new(format!(
                        "Distribution {name} is already installed"
                    )));
                }
            }
        }
        static DIST_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let counter = DIST_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let dist_id = format!(
            "{:X}{:X}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos(),
            counter
        );
        let mut installed_provides = HashMap::new();
        if let Some(provides) = meta.hash_get_str("provides")
            && let Value::Hash(map) = provides
        {
            for (k, v) in map.iter() {
                let source_path_str = v.to_string_value();
                let source_full = std::path::Path::new(&dist_prefix).join(&source_path_str);
                let source_id = format!("{:X}", hash_strings(&[k, &dist_id]));
                let dest = sources_dir.join(&source_id);
                if source_full.exists() {
                    std::fs::copy(&source_full, &dest).ok();
                }
                let mut inner = HashMap::new();
                inner.insert("file".to_string(), Value::str(source_id));
                installed_provides.insert(k.clone(), Value::Hash(Value::hash_arc(inner)));
            }
        }
        let mut installed_resources = HashMap::new();
        if let Some(resources_val) = meta.hash_get_str("resources")
            && let Value::Array(arr, _) = resources_val
        {
            for resource in arr.iter() {
                let resource_str = resource.to_string_value();
                let source_path = if resource_str.starts_with("libraries/") {
                    let lib_name = resource_str.strip_prefix("libraries/").unwrap();
                    let platform_name = platform_library_name(lib_name);
                    std::path::Path::new(&dist_prefix)
                        .join("resources")
                        .join("libraries")
                        .join(&platform_name)
                } else {
                    std::path::Path::new(&dist_prefix)
                        .join("resources")
                        .join(&resource_str)
                };
                let hash_hex = format!("{:X}", hash_strings(&[&resource_str, &dist_id]));
                // Preserve the file extension so installed paths look like HASH.ext
                let ext = std::path::Path::new(&resource_str)
                    .extension()
                    .and_then(|e| e.to_str())
                    .map(|e| format!(".{e}"))
                    .unwrap_or_default();
                let resource_id = format!("{hash_hex}{ext}");
                let dest = resources_dir.join(&resource_id);
                if source_path.exists() {
                    std::fs::copy(&source_path, &dest).ok();
                }
                installed_resources
                    .insert(format!("resources/{resource_str}"), Value::str(resource_id));
            }
        }
        let mut installed_bin = HashMap::new();
        let source_bin_dir = std::path::Path::new(&dist_prefix).join("bin");
        if source_bin_dir.is_dir()
            && let Ok(entries) = std::fs::read_dir(&source_bin_dir)
        {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                let bin_id = format!("{:X}", hash_strings(&[&name, &dist_id]));
                let dest = bin_dir.join(&bin_id);
                std::fs::copy(entry.path(), &dest).ok();
                installed_bin.insert(format!("bin/{name}"), Value::str(bin_id));
            }
        }
        let meta_inner = match &meta {
            Value::Mixin(inner, _) => inner.as_ref(),
            other => other,
        };
        let mut meta_map = match meta_inner {
            Value::Hash(map) => map.map.clone(),
            _ => HashMap::new(),
        };
        meta_map.insert(
            "provides".to_string(),
            Value::Hash(Value::hash_arc(installed_provides)),
        );
        let mut all_files = installed_resources;
        all_files.extend(installed_bin);
        meta_map.insert("files".to_string(), Value::Hash(Value::hash_arc(all_files)));
        meta_map.insert("dist-id".to_string(), Value::str(dist_id.clone()));
        let meta_value = Value::Hash(Value::hash_arc(meta_map));
        let json_str = value_to_json_string(&meta_value);
        let dist_file = dist_dir.join(format!("{dist_id}.json"));
        std::fs::write(&dist_file, json_str)
            .map_err(|e| RuntimeError::new(format!("Cannot write dist file: {e}")))?;
        Ok(Value::Bool(true))
    }

    /// Uninstall a distribution.
    pub(crate) fn cur_inst_uninstall(
        &self,
        prefix: &str,
        dist: &Value,
    ) -> Result<Value, RuntimeError> {
        let dist_id = match dist {
            Value::Instance { attributes, .. } => attributes
                .as_map()
                .get("dist-id")
                .map(|v| v.to_string_value())
                .unwrap_or_default(),
            _ => return Ok(Value::Bool(false)),
        };
        if dist_id.is_empty() {
            return Ok(Value::Bool(false));
        }
        let prefix_path = std::path::Path::new(prefix);
        let dist_file = prefix_path.join("dist").join(format!("{dist_id}.json"));
        if dist_file.exists() {
            std::fs::remove_file(&dist_file).ok();
        }
        Ok(Value::Bool(true))
    }

    /// Get installed distributions.
    pub(crate) fn cur_inst_installed(&self, prefix: &str) -> Result<Value, RuntimeError> {
        let prefix_path = std::path::Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        if !dist_dir.is_dir() {
            return Ok(Value::array(Vec::new()));
        }
        let mut results = Vec::new();
        if let Ok(entries) = std::fs::read_dir(&dist_dir) {
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
                    crate::symbol::Symbol::intern("Distribution::Installation"),
                    attrs,
                ));
            }
        }
        Ok(Value::array(results))
    }

    /// Resolve a dependency spec against an installation repository and load it,
    /// returning a `CompUnit`. The loaded symbols are kept hidden from `::('Foo')`
    /// until `GLOBALish.WHO.merge-symbols($cu.handle.globalish-package)` is run.
    pub(crate) fn cur_inst_need(
        &mut self,
        prefix: &str,
        depspec: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        let depspec = depspec.unwrap_or(Value::Nil);
        let (short_name, ..) = self.extract_depspec_fields(&depspec);
        if short_name.is_empty() {
            return Err(RuntimeError::new(
                "Could not find a CompUnit for the given dependency specification",
            ));
        }
        // Find candidates and pick the highest matching version.
        let candidates = match self.cur_inst_candidates(prefix, &depspec)? {
            Value::Array(arr, _) => arr,
            _ => crate::value::Value::array_arc(Vec::new()),
        };
        let dist_meta = |dist: &Value| -> Value {
            match dist {
                Value::Instance { attributes, .. } => attributes
                    .as_map()
                    .get("meta")
                    .cloned()
                    .unwrap_or(Value::Nil),
                _ => Value::Nil,
            }
        };
        let best = candidates
            .iter()
            .max_by(|a, b| {
                let va = dist_meta(a)
                    .hash_get_str("ver")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let vb = dist_meta(b)
                    .hash_get_str("ver")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                va.cmp(&vb)
            })
            .cloned();
        let Some(dist) = best else {
            return Err(RuntimeError::new(format!(
                "Could not find {short_name} in the installation repository"
            )));
        };
        let meta = dist_meta(&dist);
        let version_str = meta
            .hash_get_str("ver")
            .or_else(|| meta.hash_get_str("version"))
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // Locate the installed source file for the requested module.
        let source_id = meta
            .hash_get_str("provides")
            .and_then(|p| p.hash_get_str(&short_name))
            .map(|entry| match &entry {
                Value::Hash(map) => map
                    .get("file")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
                other => other.to_string_value(),
            })
            .unwrap_or_default();
        let source_path = std::path::Path::new(prefix)
            .join("sources")
            .join(&source_id);

        // Load the module source in isolation: capture the classes/roles it
        // registers so we can keep them invisible to `::()` until merged.
        let class_before: std::collections::HashSet<String> =
            self.registry().classes.keys().cloned().collect();
        let role_before: std::collections::HashSet<String> =
            self.registry().roles.keys().cloned().collect();
        if source_path.exists() {
            let saved = self.precomp_enabled;
            self.precomp_enabled = false;
            let parsed = self.parse_module_source(&short_name, &source_path);
            self.precomp_enabled = saved;
            if let Ok((stmts, _)) = parsed {
                self.run_block(&stmts)?;
            }
        }
        let mut new_symbols: Vec<String> = Vec::new();
        for k in self.registry().classes.keys() {
            if !class_before.contains(k) {
                new_symbols.push(k.clone());
            }
        }
        for k in self.registry().roles.keys() {
            if !role_before.contains(k) {
                new_symbols.push(k.clone());
            }
        }
        // Keep the freshly-loaded symbols hidden from indirect lookup until merge.
        for s in &new_symbols {
            self.cur_repo.pending_global_symbols.insert(s.clone());
        }

        // Build the CompUnit returned to the caller.
        let version = Self::parse_version_string(&version_str);
        let mut attrs = HashMap::new();
        attrs.insert("short-name".to_string(), Value::str(short_name.clone()));
        attrs.insert("version".to_string(), version);
        attrs.insert("from".to_string(), Value::str_from("Raku"));
        attrs.insert(
            "globalish-symbols".to_string(),
            Value::array(new_symbols.into_iter().map(Value::str).collect()),
        );
        let compunit = Value::make_instance(crate::symbol::Symbol::intern("CompUnit"), attrs);
        self.cur_repo
            .loaded
            .entry(prefix.to_string())
            .or_default()
            .push(compunit.clone());
        Ok(compunit)
    }
}
