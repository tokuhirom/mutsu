use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;
use std::sync::Arc;

impl Interpreter {
    /// Parse a JSON string into a Value::Hash.
    pub(crate) fn parse_json_to_value(&self, json_str: &str) -> Result<Value, RuntimeError> {
        let trimmed = json_str.trim();
        let (val, _) = parse_json_value(trimmed)
            .map_err(|e| RuntimeError::new(format!("Cannot parse JSON: {e}")))?;
        Ok(val)
    }

    /// Build the "files" hash for a distribution.
    pub(crate) fn build_dist_files_hash(&self, prefix: &str, meta: &Value) -> Value {
        let mut files = HashMap::new();
        let prefix_path = std::path::Path::new(prefix);
        if let Some(resources) = meta.hash_get_str("resources")
            && let Value::Array(arr, _) = resources
        {
            for resource in arr.iter() {
                let resource_str = resource.to_string_value();
                if resource_str.starts_with("libraries/") {
                    let lib_name = resource_str.strip_prefix("libraries/").unwrap();
                    let platform_name = platform_library_name(lib_name);
                    let actual = prefix_path
                        .join("resources")
                        .join("libraries")
                        .join(&platform_name);
                    files.insert(
                        format!("resources/{resource_str}"),
                        Value::str(actual.to_string_lossy().to_string()),
                    );
                } else {
                    let actual_path = prefix_path.join("resources").join(&resource_str);
                    files.insert(
                        format!("resources/{resource_str}"),
                        Value::str(actual_path.to_string_lossy().to_string()),
                    );
                }
            }
        }
        if let Some(provides) = meta.hash_get_str("provides")
            && let Value::Hash(map) = provides
        {
            for (_, v) in map.iter() {
                let path_str = v.to_string_value();
                files.insert(
                    path_str.clone(),
                    Value::str(prefix_path.join(&path_str).to_string_lossy().to_string()),
                );
            }
        }
        let bin_dir = prefix_path.join("bin");
        if bin_dir.is_dir()
            && let Ok(entries) = std::fs::read_dir(&bin_dir)
        {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                let path = entry.path().to_string_lossy().to_string();
                files.insert(format!("bin/{name}"), Value::str(path));
            }
        }
        Value::Hash(Arc::new(files))
    }

    /// Extract short-name and matcher fields from a depspec value.
    pub(crate) fn extract_depspec_fields(
        &self,
        depspec: &Value,
    ) -> (String, Option<String>, Option<String>, Option<String>) {
        match depspec {
            Value::CompUnitDepSpec { short_name } => (short_name.resolve(), None, None, None),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name.resolve() == "CompUnit::DependencySpecification" => {
                let sn = attributes
                    .as_map()
                    .get("short-name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes
                    .as_map()
                    .get("auth-matcher")
                    .map(|v| v.to_string_value());
                let ver = attributes
                    .as_map()
                    .get("version-matcher")
                    .map(|v| v.to_string_value());
                let api = attributes
                    .as_map()
                    .get("api-matcher")
                    .map(|v| v.to_string_value());
                (sn, auth, ver, api)
            }
            Value::Str(s) => (s.to_string(), None, None, None),
            _ => (String::new(), None, None, None),
        }
    }

    fn matches_depspec(
        meta: &Value,
        short_name: &str,
        auth_matcher: &Option<String>,
        version_matcher: &Option<String>,
        api_matcher: &Option<String>,
    ) -> bool {
        let meta_name = meta
            .hash_get_str("name")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // A distribution matches if its name equals short_name, OR if short_name
        // appears as a key in the distribution's "provides" map.
        let name_matches = meta_name == short_name || {
            meta.hash_get_str("provides")
                .and_then(|p| {
                    if let Value::Hash(map) = p {
                        Some(map)
                    } else {
                        None
                    }
                })
                .map(|map| map.contains_key(short_name))
                .unwrap_or(false)
        };
        if !name_matches {
            return false;
        }
        if let Some(auth_m) = auth_matcher {
            let meta_auth = meta
                .hash_get_str("auth")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if meta_auth != *auth_m {
                return false;
            }
        }
        if let Some(ver_m) = version_matcher {
            let meta_ver = meta
                .hash_get_str("ver")
                .or_else(|| meta.hash_get_str("version"))
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if meta_ver != *ver_m {
                return false;
            }
        }
        if let Some(api_m) = api_matcher {
            let meta_api = meta
                .hash_get_str("api")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            if meta_api != *api_m {
                return false;
            }
        }
        true
    }

    /// Get candidates for a CompUnit::Repository::FileSystem.
    pub(crate) fn cur_fs_candidates(
        &self,
        prefix: &str,
        depspec: &Value,
    ) -> Result<Value, RuntimeError> {
        let (short_name, auth_matcher, version_matcher, api_matcher) =
            self.extract_depspec_fields(depspec);
        if short_name.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let prefix_path = std::path::Path::new(prefix);
        let meta_path = prefix_path.join("META6.json");
        // When no META6.json is in prefix, also try the parent directory (handles -Ilib style).
        let parent_meta_path = prefix_path
            .parent()
            .map(|p| p.join("META6.json"))
            .filter(|p| p.exists());
        let (meta, effective_prefix) = if meta_path.exists() {
            let json_str = std::fs::read_to_string(&meta_path).map_err(|e| {
                RuntimeError::new(format!("Cannot read {}: {e}", meta_path.display()))
            })?;
            (self.parse_json_to_value(&json_str)?, prefix.to_string())
        } else if let Some(parent_meta) = parent_meta_path {
            // Parent has META6.json (e.g. prefix is dist/lib, parent is dist/)
            let parent_prefix = parent_meta.parent().unwrap().to_string_lossy().to_string();
            let json_str = std::fs::read_to_string(&parent_meta).map_err(|e| {
                RuntimeError::new(format!("Cannot read {}: {e}", parent_meta.display()))
            })?;
            let meta = self.parse_json_to_value(&json_str)?;
            // Check depspec match using parent meta
            if !Self::matches_depspec(
                &meta,
                &short_name,
                &auth_matcher,
                &version_matcher,
                &api_matcher,
            ) {
                return Ok(Value::array(Vec::new()));
            }
            // Build files hash using parent prefix (where resources/ lives)
            let files_hash = self.build_dist_files_hash(&parent_prefix, &meta);
            let meta = if let Value::Hash(map) = &meta {
                let mut m = (**map).clone();
                m.insert("files".to_string(), files_hash.clone());
                Value::Hash(Arc::new(m))
            } else {
                meta
            };
            let mut attrs = HashMap::new();
            // Use parent_prefix as the dist prefix so install can find bin/ and resources/
            // relative to the distribution root (not the lib/ subdir).
            attrs.insert(
                "prefix".to_string(),
                self.make_io_path_instance(&parent_prefix),
            );
            attrs.insert("meta".to_string(), meta);
            attrs.insert("files".to_string(), files_hash);
            return Ok(Value::array(vec![Value::make_instance(
                crate::symbol::Symbol::intern("Distribution::Path"),
                attrs,
            )]));
        } else {
            let relative = short_name.replace("::", "/");
            for ext in [".rakumod", ".pm6", ".raku", ".pm"] {
                let candidate = prefix_path.join(format!("{relative}{ext}"));
                if candidate.exists() {
                    let mut meta_map = HashMap::new();
                    meta_map.insert("name".to_string(), Value::str(short_name.clone()));
                    let mut provides_map = HashMap::new();
                    provides_map.insert(short_name.clone(), Value::str(format!("{relative}{ext}")));
                    meta_map.insert("provides".to_string(), Value::Hash(Arc::new(provides_map)));
                    let meta = Value::Hash(Arc::new(meta_map));
                    let files_hash = self.build_dist_files_hash(prefix, &meta);
                    let meta = if let Value::Hash(map) = &meta {
                        let mut m = (**map).clone();
                        m.insert("files".to_string(), files_hash.clone());
                        Value::Hash(Arc::new(m))
                    } else {
                        meta
                    };
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(prefix));
                    attrs.insert("meta".to_string(), meta);
                    attrs.insert("files".to_string(), files_hash);
                    return Ok(Value::array(vec![Value::make_instance(
                        crate::symbol::Symbol::intern("Distribution::Path"),
                        attrs,
                    )]));
                }
            }
            return Ok(Value::array(Vec::new()));
        };
        let prefix = effective_prefix.as_str();
        if !Self::matches_depspec(
            &meta,
            &short_name,
            &auth_matcher,
            &version_matcher,
            &api_matcher,
        ) {
            return Ok(Value::array(Vec::new()));
        }
        let files_hash = self.build_dist_files_hash(prefix, &meta);
        // Insert the files hash into the meta value so that $dist.meta<files> works.
        let meta = if let Value::Hash(map) = &meta {
            let mut m = (**map).clone();
            m.insert("files".to_string(), files_hash.clone());
            Value::Hash(Arc::new(m))
        } else {
            meta
        };
        let mut attrs = HashMap::new();
        attrs.insert("prefix".to_string(), self.make_io_path_instance(prefix));
        attrs.insert("meta".to_string(), meta);
        attrs.insert("files".to_string(), files_hash);
        Ok(Value::array(vec![Value::make_instance(
            crate::symbol::Symbol::intern("Distribution::Path"),
            attrs,
        )]))
    }

    /// Get candidates for a CompUnit::Repository::Installation.
    pub(crate) fn cur_inst_candidates(
        &self,
        prefix: &str,
        depspec: &Value,
    ) -> Result<Value, RuntimeError> {
        let (short_name, auth_matcher, version_matcher, api_matcher) =
            self.extract_depspec_fields(depspec);
        if short_name.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
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
                if !Self::matches_depspec(
                    &meta,
                    &short_name,
                    &auth_matcher,
                    &version_matcher,
                    &api_matcher,
                ) {
                    continue;
                }
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
                installed_provides.insert(k.clone(), Value::Hash(Arc::new(inner)));
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
            Value::Hash(map) => (**map).clone(),
            _ => HashMap::new(),
        };
        meta_map.insert(
            "provides".to_string(),
            Value::Hash(Arc::new(installed_provides)),
        );
        let mut all_files = installed_resources;
        all_files.extend(installed_bin);
        meta_map.insert("files".to_string(), Value::Hash(Arc::new(all_files)));
        meta_map.insert("dist-id".to_string(), Value::str(dist_id.clone()));
        let meta_value = Value::Hash(Arc::new(meta_map));
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
            _ => Arc::new(Vec::new()),
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
}

// ---- Simple JSON parser (no serde_json dependency) ----

fn parse_json_value(s: &str) -> Result<(Value, &str), String> {
    let s = s.trim_start();
    if s.is_empty() {
        return Err("Unexpected end of JSON".to_string());
    }
    match s.as_bytes()[0] {
        b'{' => parse_json_object(&s[1..]),
        b'[' => parse_json_array(&s[1..]),
        b'"' => parse_json_string(&s[1..]),
        b't' if s.starts_with("true") => Ok((Value::Bool(true), &s[4..])),
        b'f' if s.starts_with("false") => Ok((Value::Bool(false), &s[5..])),
        b'n' if s.starts_with("null") => Ok((Value::Nil, &s[4..])),
        b'-' | b'0'..=b'9' => parse_json_number(s),
        ch => Err(format!("Unexpected character in JSON: {}", ch as char)),
    }
}

fn parse_json_object(s: &str) -> Result<(Value, &str), String> {
    let mut map = HashMap::new();
    let mut s = s.trim_start();
    if let Some(rest) = s.strip_prefix('}') {
        return Ok((Value::Hash(Arc::new(map)), rest));
    }
    loop {
        let s2 = s.trim_start();
        if !s2.starts_with('"') {
            return Err("Expected string key in JSON object".to_string());
        }
        let (key_val, rest) = parse_json_string(&s2[1..])?;
        let key = key_val.to_string_value();
        let rest = rest.trim_start();
        if !rest.starts_with(':') {
            return Err("Expected ':' in JSON object".to_string());
        }
        let (val, rest) = parse_json_value(&rest[1..])?;
        map.insert(key, val);
        let rest = rest.trim_start();
        if let Some(after) = rest.strip_prefix('}') {
            return Ok((Value::Hash(Arc::new(map)), after));
        }
        if let Some(after) = rest.strip_prefix(',') {
            s = after;
        } else {
            return Err("Expected ',' or '}' in JSON object".to_string());
        }
    }
}

fn parse_json_array(s: &str) -> Result<(Value, &str), String> {
    let mut items = Vec::new();
    let mut s = s.trim_start();
    if let Some(rest) = s.strip_prefix(']') {
        return Ok((Value::array(items), rest));
    }
    loop {
        let (val, rest) = parse_json_value(s)?;
        items.push(val);
        let rest = rest.trim_start();
        if let Some(after) = rest.strip_prefix(']') {
            return Ok((Value::array(items), after));
        }
        if let Some(after) = rest.strip_prefix(',') {
            s = after.trim_start();
        } else {
            return Err("Expected ',' or ']' in JSON array".to_string());
        }
    }
}

fn parse_json_string(s: &str) -> Result<(Value, &str), String> {
    let mut result = String::new();
    let mut chars = s.chars();
    loop {
        match chars.next() {
            None => return Err("Unterminated JSON string".to_string()),
            Some('"') => {
                return Ok((Value::str(result), chars.as_str()));
            }
            Some('\\') => match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('/') => result.push('/'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('u') => {
                    let hex: String = chars.by_ref().take(4).collect();
                    if let Ok(cp) = u32::from_str_radix(&hex, 16)
                        && let Some(ch) = char::from_u32(cp)
                    {
                        result.push(ch);
                    }
                }
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => return Err("Unterminated escape in JSON string".to_string()),
            },
            Some(c) => result.push(c),
        }
    }
}

fn parse_json_number(s: &str) -> Result<(Value, &str), String> {
    let mut end = 0;
    let bytes = s.as_bytes();
    if end < bytes.len() && bytes[end] == b'-' {
        end += 1;
    }
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
    }
    let mut is_float = false;
    if end < bytes.len() && bytes[end] == b'.' {
        is_float = true;
        end += 1;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    if end < bytes.len() && (bytes[end] == b'e' || bytes[end] == b'E') {
        is_float = true;
        end += 1;
        if end < bytes.len() && (bytes[end] == b'+' || bytes[end] == b'-') {
            end += 1;
        }
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    let num_str = &s[..end];
    if is_float {
        let f: f64 = num_str
            .parse()
            .map_err(|_| format!("Invalid JSON number: {num_str}"))?;
        Ok((Value::Num(f), &s[end..]))
    } else {
        let i: i64 = num_str
            .parse()
            .map_err(|_| format!("Invalid JSON number: {num_str}"))?;
        Ok((Value::Int(i), &s[end..]))
    }
}

fn value_to_json_string(val: &Value) -> String {
    match val {
        Value::Hash(map) => {
            let parts: Vec<String> = map
                .iter()
                .map(|(k, v)| format!("  {:?}: {}", k, value_to_json_string(v)))
                .collect();
            format!("{{\n{}\n}}", parts.join(",\n"))
        }
        Value::Array(arr, _) => {
            let parts: Vec<String> = arr.iter().map(value_to_json_string).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Str(s) => format!("{:?}", s.to_string()),
        Value::Int(i) => format!("{i}"),
        Value::Bool(b) => format!("{b}"),
        Value::Nil => "null".to_string(),
        _ => format!("{:?}", val.to_string_value()),
    }
}

/// Identity tuple of a distribution, used to detect duplicate installs.
/// Two distributions are "the same" when name/version/auth/api all match.
fn dist_identity(meta: &Value) -> (String, String, String, String) {
    let field = |key: &str, alt: Option<&str>| {
        meta.hash_get_str(key)
            .or_else(|| alt.and_then(|a| meta.hash_get_str(a)))
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    };
    (
        field("name", None),
        field("ver", Some("version")),
        field("auth", None),
        field("api", None),
    )
}

fn platform_library_name(name: &str) -> String {
    if cfg!(target_os = "macos") {
        format!("lib{name}.dylib")
    } else if cfg!(target_os = "windows") {
        format!("{name}.dll")
    } else {
        format!("lib{name}.so")
    }
}

fn hash_strings(strings: &[&str]) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for s in strings {
        s.hash(&mut hasher);
    }
    hasher.finish()
}

impl Interpreter {
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
