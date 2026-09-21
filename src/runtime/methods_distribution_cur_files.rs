//! `CompUnit::Repository::FileSystem.files` — the non-module-file
//! introspection query, and the version-range matching it needs.
//!
//! `.files($path, :ver, :auth, :api)` returns the META of every distribution at
//! the repository's prefix that both matches the given auth/version/api
//! and *provides a file* at `$path` (`bin/zef`, `resources/config.json`, ...).
//! Unlike `.candidates`, `$path` is a file path, not a module short-name.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueMap, ValueView, VersionPart};

impl Interpreter {
    /// `CompUnit::Repository::FileSystem.distribution`.
    ///
    /// A source repository describes the modules below its prefix as one
    /// synthetic distribution.  Rakudo uses this metadata while discovering
    /// plugins in a `use lib` directory, even when the directory has no
    /// META6.json of its own.
    pub(crate) fn cur_fs_distribution(&self, prefix: &str) -> Result<Value, RuntimeError> {
        let prefix_path = std::path::Path::new(prefix);
        let mut provides = ValueMap::default();
        Self::collect_fs_repo_provides(prefix_path, prefix_path, &mut provides);

        let mut meta = ValueMap::default();
        meta.insert("name".to_string(), Value::str(prefix.to_string()));
        meta.insert("ver".to_string(), Value::str_from("*"));
        meta.insert("api".to_string(), Value::str_from("*"));
        meta.insert("auth".to_string(), Value::str_from(""));
        meta.insert(
            "provides".to_string(),
            Value::hash_with_data(Value::hash_arc(provides)),
        );
        meta.insert("resources".to_string(), Value::array(Vec::new()));

        let distribution_prefix = prefix_path
            .parent()
            .unwrap_or(prefix_path)
            .to_string_lossy()
            .to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "prefix".to_string(),
            self.make_io_path_instance(&distribution_prefix),
        );
        attrs.insert(
            "meta".to_string(),
            Value::hash_with_data(Value::hash_arc(meta)),
        );
        attrs.insert(
            "files".to_string(),
            Value::hash_with_data(Value::hash_arc(ValueMap::default())),
        );
        Ok(Value::array(vec![Value::make_instance(
            Symbol::intern("Distribution::Hash"),
            attrs,
        )]))
    }

    fn collect_fs_repo_provides(
        root: &std::path::Path,
        directory: &std::path::Path,
        provides: &mut ValueMap,
    ) {
        let Ok(entries) = std::fs::read_dir(directory) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            if file_type.is_dir() {
                Self::collect_fs_repo_provides(root, &path, provides);
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            let Some(extension) = path.extension().and_then(|ext| ext.to_str()) else {
                continue;
            };
            if !matches!(extension, "rakumod" | "pm6" | "pm") {
                continue;
            }
            let Ok(relative) = path.strip_prefix(root) else {
                continue;
            };
            let module_path = relative.with_extension("");
            let Some(module_name) = module_path.to_str() else {
                continue;
            };
            let module_name = module_name.replace(std::path::MAIN_SEPARATOR, "::");
            let relative_path = relative.to_string_lossy().replace('\\', "/");
            provides.insert(module_name, Value::str(format!("lib/{relative_path}")));
        }
    }

    /// `CompUnit::Repository::FileSystem.files`.
    pub(crate) fn cur_fs_files(&self, prefix: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        let search_path = Self::positional_string(args, 0);
        if search_path.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let named = |name: &str| Self::named_value(args, name).map(|v| v.to_string_value());
        let (ver, auth, api) = (named("ver"), named("auth"), named("api"));

        let Some((meta, dist_prefix)) = self.fs_repo_meta(prefix) else {
            return Ok(Value::array(Vec::new()));
        };
        if !Self::meta_matches_filters(&meta, &ver, &auth, &api) {
            return Ok(Value::array(Vec::new()));
        }
        // `build_dist_files_hash` maps every file the distribution actually
        // provides (from `provides`, `bin/`, `resources/`) to its on-disk path,
        // which is exactly the set `.files` searches.
        let files_hash = self.build_dist_files_hash(&dist_prefix, &meta);
        if files_hash.hash_get_str(&search_path).is_none() {
            return Ok(Value::array(Vec::new()));
        }
        // Rakudo hands back the distribution's META hash (so `.head.<name>` is
        // the distribution name), with `files` folded in.
        let result = match meta.view() {
            ValueView::Hash(map) => {
                let mut m = (**map).clone();
                m.insert("files".to_string(), files_hash);
                Value::hash_with_data(Value::hash_arc(m))
            }
            _ => meta,
        };
        Ok(Value::array(vec![result]))
    }

    /// Read the META6.json describing the distribution rooted at (or just above)
    /// `prefix`, returning it with the distribution root it belongs to. The
    /// parent fallback is what makes a `-Ilib` style prefix find its dist.
    fn fs_repo_meta(&self, prefix: &str) -> Option<(Value, String)> {
        let prefix_path = std::path::Path::new(prefix);
        let own = prefix_path.join("META6.json");
        if own.exists() {
            let json = std::fs::read_to_string(&own).ok()?;
            return Some((self.parse_json_to_value(&json).ok()?, prefix.to_string()));
        }
        let parent_meta = prefix_path.parent().map(|p| p.join("META6.json"))?;
        if !parent_meta.exists() {
            return None;
        }
        let json = std::fs::read_to_string(&parent_meta).ok()?;
        let root = parent_meta.parent()?.to_string_lossy().to_string();
        Some((self.parse_json_to_value(&json).ok()?, root))
    }

    /// Match a distribution's META against `:ver`/`:auth`/`:api` selectors.
    /// `auth` and `api` are plain equality; `ver` is a version *range* match.
    fn meta_matches_filters(
        meta: &Value,
        ver: &Option<String>,
        auth: &Option<String>,
        api: &Option<String>,
    ) -> bool {
        let field = |key: &str, alt: &str| {
            meta.hash_get_str(key)
                .or_else(|| meta.hash_get_str(alt))
                .map(|v| v.to_string_value())
                .unwrap_or_default()
        };
        if let Some(auth) = auth
            && field("auth", "auth") != *auth
        {
            return false;
        }
        if let Some(api) = api
            && field("api", "api") != *api
        {
            return false;
        }
        if let Some(ver) = ver
            && !Self::version_selector_matches(&field("ver", "version"), ver)
        {
            return false;
        }
        true
    }

    /// Whether a concrete distribution version satisfies a selector.
    ///
    /// A trailing `+` means "this version or later", `-` means "this version or
    /// earlier", `*` in any position is a wildcard, and a bare selector must
    /// compare equal. Plain string equality (what the older depspec matcher
    /// used) got this wrong in both directions: `:ver<0.4.0+>` failed to match
    /// `0.4.0`, while `v1.0` failed to match `1.0`.
    pub(crate) fn version_selector_matches(actual: &str, selector: &str) -> bool {
        if selector.is_empty() || selector == "*" {
            return true;
        }
        let (sel_parts, sel_plus, sel_minus) = Value::parse_version_string(selector);
        let (act_parts, _, _) = Value::parse_version_string(actual);
        // A `*` part in the selector accepts anything in that position, so
        // compare only up to the wildcard.
        let compare_len = sel_parts
            .iter()
            .position(|p| matches!(p, VersionPart::Whatever))
            .unwrap_or(sel_parts.len());
        let sel_head = &sel_parts[..compare_len];
        let act_head: Vec<VersionPart> = act_parts.iter().take(compare_len).cloned().collect();
        let ordering = crate::runtime::utils::version_cmp_parts(&act_head, sel_head);
        match (sel_plus, sel_minus) {
            (true, _) => ordering != std::cmp::Ordering::Less,
            (_, true) => ordering != std::cmp::Ordering::Greater,
            _ => {
                // No range flag: an exact match, but a truncated (wildcard)
                // comparison only needs the compared prefix to agree.
                if compare_len < sel_parts.len() {
                    ordering == std::cmp::Ordering::Equal
                } else {
                    crate::runtime::utils::version_cmp_parts(&act_parts, &sel_parts)
                        == std::cmp::Ordering::Equal
                }
            }
        }
    }
}
