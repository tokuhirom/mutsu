//! Parse-time module lookup inside an installed repository (`inst#<prefix>`).
//!
//! The runtime resolves such a module through the dist metadata
//! (`Interpreter::resolve_in_inst_repo`); the import scan has to find the same
//! source file, or an installed module's `constant`s, types and exports are
//! invisible to the importer's parse (#12626).

use std::path::Path;

/// Find the source file for `module` in the installed repository at `prefix`
/// (`<prefix>/dist/*.json` metadata, `<prefix>/sources/<file-id>` sources).
/// When several installed dists provide the module the highest version wins;
/// the `use` statement's `:ver`/`:auth` selectors are not consulted here.
// Cost: O(d * m), d = installed dists, m = size of one dist's metadata.
pub(super) fn find_in_installed_repo(prefix: &str, module: &str) -> Option<String> {
    let prefix = Path::new(prefix);
    let entries = std::fs::read_dir(prefix.join("dist")).ok()?;
    let mut best: Option<(Vec<u64>, std::path::PathBuf)> = None;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        let Ok(text) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Ok(meta) = serde_json::from_str::<serde_json::Value>(&text) else {
            continue;
        };
        let Some(entry) = meta.get("provides").and_then(|p| p.get(module)) else {
            continue;
        };
        // A dist installed by `mzef`/zef maps the module to its source path,
        // which maps to `{ "file": <id> }`; older metadata names the id or
        // the `file` object directly.
        let Some(file_id) = entry
            .as_str()
            .or_else(|| entry.get("file").and_then(serde_json::Value::as_str))
            .or_else(|| {
                entry.as_object()?.values().find_map(|v| {
                    v.as_str()
                        .or_else(|| v.get("file").and_then(serde_json::Value::as_str))
                })
            })
        else {
            continue;
        };
        let source = prefix.join("sources").join(file_id);
        if !source.is_file() {
            continue;
        }
        let version: Vec<u64> = meta
            .get("version")
            .and_then(serde_json::Value::as_str)
            .map(|v| v.split('.').map(|p| p.parse().unwrap_or(0)).collect())
            .unwrap_or_default();
        if best.as_ref().is_none_or(|(b, _)| version > *b) {
            best = Some((version, source));
        }
    }
    best.map(|(_, source)| source.to_string_lossy().into_owned())
}
