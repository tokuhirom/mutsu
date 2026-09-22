//! The bundled batteries (`modules/<Dist>/lib`) as links of `$*REPO`'s
//! repository chain.
//!
//! `use` has always found a bundled module through `resolve_module_path`'s
//! lowest-priority fallback, but that fallback sat *outside* the
//! `CompUnit::Repository` chain, so the repository API (`$*REPO.resolve`,
//! `.need`, `repo-chain`, `repository-for-name('core')`) could not see what
//! `use` could load (#9071). Rakudo's chain ends in the repositories holding
//! its core modules; mutsu's ends in one `CompUnit::Repository::FileSystem`
//! per bundled distribution, which is exactly what each battery is on disk: a
//! source distribution whose `META6.json` sits one level above its `lib/`.
//!
//! They are FileSystem repositories, not Installation ones, on purpose: zef's
//! `list-installed` / `is-installed` only consult Installation repositories,
//! and a bundled battery must stay upgradable through `mzef install`
//! (BATTERIES.md §6) rather than read as already installed.

use super::*;

/// The bundled distribution that holds the modules Rakudo ships in its own
/// core library (`Test`, `Pod::To::Text`); `repository-for-name('core')`
/// answers its repository.
const CORE_DIST_DIR: &str = "Rakudo-Core";

/// Marks a chain link as a bundled battery; the value is the distribution's
/// directory name under the bundle.
const BUNDLED_DIST_ATTR: &str = "__mutsu_bundled_dist";

impl Interpreter {
    /// Append one FileSystem repository per bundled distribution to the tail
    /// of `$*REPO`'s chain, in the order `resolve_module_path` searches them.
    pub(super) fn add_bundled_repos(&mut self) {
        let paths = Arc::clone(&self.bundled_lib_paths);
        // Build the sub-chain back to front so each link can point at the next.
        let mut head = Value::NIL;
        for lib in paths.iter().rev() {
            head = self.bundled_repo_link(lib, head);
        }
        if head.truthy() {
            self.append_repo_to_chain_tail(head);
        }
    }

    /// Hang `repo` off the last link of `$*REPO`'s chain.
    pub(super) fn append_repo_to_chain_tail(&mut self, repo: Value) {
        let mut cursor = self.env.get("*REPO").cloned();
        while let Some(node) = cursor {
            let ValueView::Instance { attributes, .. } = node.view() else {
                break;
            };
            // Read the current `next-repo` and release the read lock before
            // taking the write lock below (holding both on the same
            // interior-mutable cell would self-deadlock).
            let next = attributes.as_map().get("next-repo").cloned();
            match next {
                Some(next)
                    if next.truthy() && matches!(next.view(), ValueView::Instance { .. }) =>
                {
                    cursor = Some(next);
                }
                _ => {
                    attributes.insert("next-repo".to_string(), repo);
                    break;
                }
            }
        }
    }

    /// One bundled distribution's repository: a FileSystem repository on its
    /// `lib/`, chained to `next`.
    fn bundled_repo_link(&self, lib: &str, next: Value) -> Value {
        let dist_dir = Path::new(lib)
            .parent()
            .and_then(|p| p.file_name())
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();
        // Canonical, as a `use lib` link is: the discovered bundle path is
        // binary-relative (`target/debug/../../modules/...`).
        let prefix = std::fs::canonicalize(lib)
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| lib.to_string());
        let mut attrs = HashMap::new();
        attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
        attrs.insert("short-id".to_string(), Value::str_from("file"));
        attrs.insert("__mutsu_precomp_enabled".to_string(), Value::TRUE);
        attrs.insert(BUNDLED_DIST_ATTR.to_string(), Value::str(dist_dir));
        attrs.insert("next-repo".to_string(), next);
        Value::make_instance(Symbol::intern("CompUnit::Repository::FileSystem"), attrs)
    }

    /// A repository for the bundled distribution holding Rakudo's own core
    /// modules (`Test`, `Pod::To::Text`), with no `next-repo`.
    ///
    /// `repository-for-name('core')` is an Installation repository in Rakudo
    /// and stays one here, but mutsu has nothing installed there: its core
    /// modules are this battery. Hanging it off the core repository as its
    /// `next-repo` lets `core.resolve(Test)` answer as Rakudo's does, while
    /// `core.candidates` / `.installed` (what zef's `list-installed` and its
    /// ignore list read) keep describing the repository itself.
    pub(crate) fn bundled_core_repo(&self) -> Option<Value> {
        let lib = self.bundled_lib_paths.iter().find(|lib| {
            Path::new(lib.as_str())
                .parent()
                .and_then(|p| p.file_name())
                .is_some_and(|n| n == CORE_DIST_DIR)
        })?;
        Some(self.bundled_repo_link(lib, Value::NIL))
    }
}
