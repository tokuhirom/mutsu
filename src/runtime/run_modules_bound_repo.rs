//! Repositories a program binds into `$*REPO` itself.
//!
//! `resolve_module_path` walks `lib_paths`, which `use lib`, `-I`, `MUTSULIB`
//! and the installed repositories all register into; the `$*REPO` chain is
//! built alongside it so the repository API can see the same links. A program
//! can also install a repository directly:
//!
//! ```raku
//! PROCESS::<$REPO> := CompUnit::Repository::FileSystem.new(
//!     :next-repo($*REPO), :prefix('packages/Fancy/lib'));
//! require Fancy::Utilities;
//! ```
//!
//! Such a link never reaches `lib_paths`, so the resolver reads it off the
//! chain: every FileSystem link ahead of the interpreter's default head that
//! is not already one of the `lib_paths` directories.

use super::*;

/// Marks the default `$*REPO` head that `runtime_init` installs. Its `.`
/// prefix is the repository API's view of the program, not a search
/// location: module resolution never searches the current directory (#11213).
pub(crate) const DEFAULT_REPO_HEAD_ATTR: &str = "__mutsu_default_repo_head";

impl Interpreter {
    /// Prefixes of the FileSystem repositories the program bound into `$*REPO`
    /// directly, in chain order (head first).
    ///
    // Cost: O(k + k·p) path canonicalizations on a hit-free walk, k = chain
    // links ahead of the default head (the program's own repositories plus
    // `use lib`/`-I` links, typically a handful), p = plain `lib_paths`.
    pub(super) fn bound_repo_prefixes(&self) -> Vec<std::path::PathBuf> {
        let Some(head) = self
            .get_process_dynamic("*REPO")
            .or_else(|| self.env.get("*REPO").cloned())
        else {
            return Vec::new();
        };
        let mut known: Option<Vec<std::path::PathBuf>> = None;
        let mut found = Vec::new();
        let mut cursor = head;
        loop {
            let next = {
                let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = cursor.view()
                else {
                    break;
                };
                let attrs = attributes.as_map();
                if attrs.contains_key(DEFAULT_REPO_HEAD_ATTR) {
                    break;
                }
                if class_name.resolve() == "CompUnit::Repository::FileSystem"
                    && let Some(prefix) = attrs.get("prefix")
                {
                    let prefix = prefix.to_string_value();
                    let canonical = std::fs::canonicalize(&prefix)
                        .unwrap_or_else(|_| std::path::PathBuf::from(&prefix));
                    let known = known.get_or_insert_with(|| {
                        self.module
                            .lib_paths
                            .iter()
                            .filter(|p| !p.starts_with("inst#"))
                            .map(|p| {
                                std::fs::canonicalize(p)
                                    .unwrap_or_else(|_| std::path::PathBuf::from(p))
                            })
                            .collect()
                    });
                    if !known.contains(&canonical) && !found.contains(&canonical) {
                        found.push(canonical);
                    }
                }
                attrs.get("next-repo").filter(|next| next.truthy()).cloned()
            };
            match next {
                Some(next) => cursor = next,
                None => break,
            }
        }
        found
    }
}
