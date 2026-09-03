//! The Raku files on disk that a workspace-wide query looks at (ADR-0065 S4).
//!
//! `workspaceSymbol` and a cross-file `definition` need more than the documents
//! the client has open. There is no index maintained in the background: files
//! are read and parsed when a query asks, and cached by modification time so
//! repeat queries are cheap. That is the right trade for this consumer — an
//! agent asks a workspace question occasionally and never while typing — and it
//! avoids a whole class of staleness bug, since the cache is validated against
//! the file rather than trusted.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Files with these extensions are analysed. `.t` is included: a test file is
/// where a routine is most often *used*, and an agent looking for a definition
/// benefits from the ones declared there too.
const RAKU_EXTENSIONS: &[&str] = &["raku", "rakumod", "rakutest", "p6", "pm6", "pl6", "t"];

/// Directories never worth walking into.
const SKIPPED_DIRECTORIES: &[&str] = &[".git", "target", "node_modules", ".precomp", ".cache"];

/// How many files one workspace scan will look at.
///
/// A bound rather than a budget: a query that walks an unbounded tree is a
/// hang, and a hung server is worse to the consumer than a truncated answer.
const MAX_FILES: usize = 4000;

#[derive(Debug, Default)]
pub struct Workspace {
    roots: Vec<PathBuf>,
    cache: HashMap<PathBuf, CachedFile>,
}

#[derive(Debug)]
struct CachedFile {
    modified: Option<SystemTime>,
    len: u64,
    text: String,
}

impl Workspace {
    /// Read the roots out of an `initialize` request's params.
    ///
    /// `workspaceFolders` is the current spelling and `rootUri`/`rootPath` the
    /// deprecated ones; clients still send the old ones, and a server that only
    /// understood the new one would silently have no workspace at all.
    pub fn from_initialize_params(params: &serde_json::Value) -> Workspace {
        let mut roots = Vec::new();
        if let Some(folders) = params.get("workspaceFolders").and_then(|v| v.as_array()) {
            for folder in folders {
                if let Some(path) = folder
                    .get("uri")
                    .and_then(|u| u.as_str())
                    .and_then(path_of_uri)
                {
                    roots.push(path);
                }
            }
        }
        if roots.is_empty()
            && let Some(path) = params
                .get("rootUri")
                .and_then(|v| v.as_str())
                .and_then(path_of_uri)
        {
            roots.push(path);
        }
        if roots.is_empty()
            && let Some(path) = params.get("rootPath").and_then(|v| v.as_str())
        {
            roots.push(PathBuf::from(path));
        }
        Workspace {
            roots,
            cache: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }

    /// Every Raku file under the roots, capped at [`MAX_FILES`].
    pub fn files(&self) -> Vec<PathBuf> {
        let mut found = Vec::new();
        for root in &self.roots {
            walk(root, &mut found);
            if found.len() >= MAX_FILES {
                break;
            }
        }
        found.truncate(MAX_FILES);
        found.sort();
        found
    }

    /// The text of `path`, from the cache when the file has not changed.
    pub fn text_of(&mut self, path: &Path) -> Option<&str> {
        let metadata = std::fs::metadata(path).ok();
        let modified = metadata.as_ref().and_then(|m| m.modified().ok());
        let len = metadata.as_ref().map_or(0, |m| m.len());
        let fresh = self
            .cache
            .get(path)
            .is_some_and(|c| c.modified == modified && c.len == len);
        if !fresh {
            let text = std::fs::read_to_string(path).ok()?;
            self.cache.insert(
                path.to_path_buf(),
                CachedFile {
                    modified,
                    len,
                    text,
                },
            );
        }
        self.cache.get(path).map(|c| c.text.as_str())
    }
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    if out.len() >= MAX_FILES {
        return;
    }
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        if out.len() >= MAX_FILES {
            return;
        }
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if path.is_dir() {
            if name.starts_with('.') || SKIPPED_DIRECTORIES.contains(&name.as_ref()) {
                continue;
            }
            walk(&path, out);
        } else if path
            .extension()
            .and_then(|e| e.to_str())
            .is_some_and(|e| RAKU_EXTENSIONS.contains(&e))
        {
            out.push(path);
        }
    }
}

/// A `file:` URI's path. Anything else (a client editing over `untitled:` or a
/// remote scheme) has no path to walk and is skipped.
fn path_of_uri(uri: &str) -> Option<PathBuf> {
    let rest = uri.strip_prefix("file://")?;
    // `file:///path` — the authority is empty for a local path.
    let rest = rest.strip_prefix('/').map(|r| format!("/{r}"))?;
    Some(PathBuf::from(percent_decode(&rest)))
}

/// Minimal percent-decoding: a workspace path with a space or a non-ASCII
/// character arrives encoded, and treating it literally would silently find no
/// files at all.
fn percent_decode(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Some(byte) = std::str::from_utf8(&bytes[i + 1..i + 3])
                .ok()
                .and_then(|hex| u8::from_str_radix(hex, 16).ok())
        {
            out.push(byte);
            i += 3;
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn workspace_folders_are_read_from_initialize_params() {
        let params = serde_json::json!({
            "workspaceFolders": [{ "uri": "file:///tmp/one", "name": "one" },
                                 { "uri": "file:///tmp/two", "name": "two" }]
        });
        let workspace = Workspace::from_initialize_params(&params);
        assert_eq!(
            workspace.roots,
            vec![PathBuf::from("/tmp/one"), PathBuf::from("/tmp/two")]
        );
    }

    #[test]
    fn the_deprecated_root_uri_is_still_understood() {
        let params = serde_json::json!({ "rootUri": "file:///tmp/only" });
        let workspace = Workspace::from_initialize_params(&params);
        assert_eq!(workspace.roots, vec![PathBuf::from("/tmp/only")]);
        let params = serde_json::json!({ "rootPath": "/tmp/older" });
        assert_eq!(
            Workspace::from_initialize_params(&params).roots,
            vec![PathBuf::from("/tmp/older")]
        );
    }

    #[test]
    fn a_client_with_no_workspace_is_not_an_error() {
        let workspace = Workspace::from_initialize_params(&serde_json::json!({}));
        assert!(workspace.is_empty());
        assert!(workspace.files().is_empty());
    }

    #[test]
    fn a_percent_encoded_path_is_decoded() {
        assert_eq!(
            path_of_uri("file:///tmp/my%20project"),
            Some(PathBuf::from("/tmp/my project"))
        );
        assert_eq!(path_of_uri("untitled:Untitled-1"), None);
    }

    #[test]
    fn the_walk_finds_raku_files_and_skips_noise() {
        let root = std::env::temp_dir().join(format!("mutsu-lsp-walk-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(root.join("lib")).unwrap();
        std::fs::create_dir_all(root.join(".git")).unwrap();
        std::fs::create_dir_all(root.join("target")).unwrap();
        std::fs::write(root.join("lib/A.rakumod"), "class A { }\n").unwrap();
        std::fs::write(root.join("script.raku"), "say 1;\n").unwrap();
        std::fs::write(root.join("notes.md"), "hello\n").unwrap();
        std::fs::write(root.join(".git/HEAD.raku"), "say 2;\n").unwrap();
        std::fs::write(root.join("target/built.raku"), "say 3;\n").unwrap();

        let mut workspace = Workspace {
            roots: vec![root.clone()],
            cache: HashMap::new(),
        };
        let files = workspace.files();
        let names: Vec<String> = files
            .iter()
            .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
            .collect();
        assert_eq!(names, vec!["A.rakumod", "script.raku"], "{files:?}");

        assert_eq!(
            workspace.text_of(&root.join("script.raku")),
            Some("say 1;\n")
        );
        // Second read comes from the cache and must be identical.
        assert_eq!(
            workspace.text_of(&root.join("script.raku")),
            Some("say 1;\n")
        );

        let _ = std::fs::remove_dir_all(&root);
    }
}
