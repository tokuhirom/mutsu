//! CompUnit::Repository::Installation uninstallation: removing an installed
//! distribution's index entry and the files it owns (also used by a `:force`
//! install over an identical distribution).

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

use super::methods_distribution_helpers::dist_identity;

impl Interpreter {
    /// Remove the files an installed distribution owns: its `dist/<id>.json`,
    /// the sources its `provides` name and the resources and scripts its
    /// `files` name.
    // Cost: O(f), f = files of the distribution.
    pub(super) fn remove_installed_dist_files(
        prefix: &std::path::Path,
        dist_file: &std::path::Path,
        meta: &Value,
    ) {
        let mut owned: Vec<std::path::PathBuf> = Vec::new();
        if let Some(provides) = meta.hash_get_str("provides")
            && let ValueView::Hash(map) = provides.view()
        {
            for entry in map.values() {
                if let ValueView::Hash(by_path) = entry.view() {
                    for file in by_path.values() {
                        if let ValueView::Hash(h) = file.view()
                            && let Some(id) = h.get("file")
                        {
                            owned.push(prefix.join("sources").join(id.to_string_value()));
                        }
                    }
                }
            }
        }
        if let Some(files) = meta.hash_get_str("files")
            && let ValueView::Hash(map) = files.view()
        {
            for (name, id) in map.iter() {
                let dir = if name.starts_with("bin/") { "bin" } else { "resources" };
                owned.push(prefix.join(dir).join(id.to_string_value()));
                if let Some(script) = name.strip_prefix("bin/") {
                    super::bin_wrappers::remove_bin_wrapper(prefix, script);
                }
            }
        }
        for file in owned {
            std::fs::remove_file(file).ok();
        }
        std::fs::remove_file(dist_file).ok();
    }

    /// Uninstall a distribution, as Rakudo's
    /// `CompUnit::Repository::Installation.uninstall`: the distribution's
    /// `dist/<id>.json` index entry goes, and so do the sources, resources and
    /// scripts it owns, so a later `use` no longer resolves it.
    ///
    /// The installed entry is found by its dist id (the `dist-id` attribute of
    /// a distribution `.installed` returned, or the `dist-id` key of its meta,
    /// which zef's `Zef::Distribution.new(|%meta)` carries over), else by the
    /// name/ver/auth/api identity of its meta. Like Rakudo's it returns `True`
    /// whether or not anything was installed.
    // Cost: O(d + f), d = distributions installed under the prefix, f = files
    // of the removed distribution.
    pub(crate) fn cur_inst_uninstall(
        &self,
        prefix: &str,
        dist: &Value,
    ) -> Result<Value, RuntimeError> {
        let dist = dist.clone().deref_container();
        let dist = match dist.view() {
            ValueView::Mixin(inner, _) => inner.as_ref().clone(),
            _ => dist,
        };
        let ValueView::Instance { attributes, .. } = dist.view() else {
            return Ok(Value::FALSE);
        };
        let attributes = attributes.as_map();
        let meta = attributes
            .get("meta")
            .map(|m| m.clone().deref_container())
            .unwrap_or(Value::NIL);
        let dist_id = attributes
            .get("dist-id")
            .or_else(|| attributes.get("id"))
            .map(|v| v.to_string_value())
            .filter(|id| !id.is_empty())
            .or_else(|| {
                meta.hash_get_str("dist-id")
                    .map(|v| v.to_string_value())
                    .filter(|id| !id.is_empty())
            });
        let prefix_path = std::path::Path::new(prefix);
        let dist_dir = prefix_path.join("dist");
        if let Some(id) = &dist_id {
            let dist_file = dist_dir.join(format!("{id}.json"));
            if let Ok(json_str) = std::fs::read_to_string(&dist_file)
                && let Ok(installed_meta) = self.parse_json_to_value(&json_str)
            {
                Self::remove_installed_dist_files(prefix_path, &dist_file, &installed_meta);
                return Ok(Value::TRUE);
            }
        }
        if meta.is_nil() {
            return Ok(Value::TRUE);
        }
        let identity = dist_identity(&meta);
        let Ok(entries) = std::fs::read_dir(&dist_dir) else {
            return Ok(Value::TRUE);
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("json") {
                continue;
            }
            let Ok(json_str) = std::fs::read_to_string(&path) else {
                continue;
            };
            let Ok(installed_meta) = self.parse_json_to_value(&json_str) else {
                continue;
            };
            if dist_identity(&installed_meta) == identity {
                Self::remove_installed_dist_files(prefix_path, &path, &installed_meta);
                return Ok(Value::TRUE);
            }
        }
        Ok(Value::TRUE)
    }
}
