//! CompUnit::Repository candidate *resolution*: building a distribution's file
//! hash, extracting/matching dependency specs, and enumerating FileSystem and
//! Installation repository candidates. See also `methods_distribution_cur_inst`
//! (install/uninstall/need) and `methods_distribution` (dispatch entry points).

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

use super::methods_distribution_helpers::platform_library_name;

impl Interpreter {
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
        Value::Hash(Value::hash_arc(files))
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
                Value::Hash(Value::hash_arc(m))
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
                    meta_map.insert(
                        "provides".to_string(),
                        Value::Hash(Value::hash_arc(provides_map)),
                    );
                    let meta = Value::Hash(Value::hash_arc(meta_map));
                    let files_hash = self.build_dist_files_hash(prefix, &meta);
                    let meta = if let Value::Hash(map) = &meta {
                        let mut m = (**map).clone();
                        m.insert("files".to_string(), files_hash.clone());
                        Value::Hash(Value::hash_arc(m))
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
            Value::Hash(Value::hash_arc(m))
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
}
