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
//!
//! The links are built lazily, on the first read of the chain tail's
//! `next-repo` ([`Interpreter::repo_next_link`]): module loading never walks
//! the chain, so most programs never need them.

use super::*;

/// The bundled distribution that holds the modules Rakudo ships in its own
/// core library (`Test`, `Pod::To::Text`); `repository-for-name('core')`
/// answers its repository.
const CORE_DIST_DIR: &str = "Rakudo-Core";

/// Marks a chain link as a bundled battery; the value is the distribution's
/// directory name under the bundle.
const BUNDLED_DIST_ATTR: &str = "__mutsu_bundled_dist";

/// Marks the link the bundled batteries hang off until they are built: the
/// tail of the default chain. Its presence means "`next-repo` is the bundled
/// sub-chain, not yet materialized".
const BUNDLED_TAIL_PENDING_ATTR: &str = "__mutsu_bundled_tail_pending";

impl Interpreter {
    /// Reserve the tail of `$*REPO`'s chain for the bundled batteries, without
    /// building them.
    ///
    /// Module loading never walks this chain -- `use` finds a battery through
    /// `resolve_module_path`'s fallback -- so only a program that introspects
    /// the repository API reads these links. Building one FileSystem
    /// repository per distribution at every start (a `canonicalize` and an
    /// `IO::Path` each) was ~9% of `say "hello"`'s instructions; the links are
    /// built on the first read of the tail's `next-repo` instead (see
    /// [`Interpreter::repo_next_link`]).
    pub(super) fn add_bundled_repos(&mut self) {
        if self.bundled_lib_paths.is_empty() {
            return;
        }
        let Some(tail) = self.repo_chain_tail() else {
            return;
        };
        if let ValueView::Instance { attributes, .. } = tail.view() {
            attributes.insert(BUNDLED_TAIL_PENDING_ATTR, Value::TRUE);
        }
    }

    /// The `next-repo` of `repo`, whose attribute map is `attrs`.
    ///
    /// Every reader of a repository's `next-repo` goes through here, because
    /// the tail of the default chain carries the bundled batteries lazily: the
    /// first read builds them and links them in.
    // Cost: O(1); the one read that builds the bundled links is O(d),
    // d = bundled distributions.
    pub(crate) fn repo_next_link(&self, repo: &Value, attrs: &AttrMap) -> Value {
        let next = attrs.get("next-repo").cloned();
        if let Some(next) = &next
            && next.truthy()
        {
            return next.clone();
        }
        if !attrs.contains_key(BUNDLED_TAIL_PENDING_ATTR) {
            return next.unwrap_or(Value::NIL);
        }
        let head = self.build_bundled_chain();
        if let ValueView::Instance { attributes, .. } = repo.view() {
            attributes.write_keys(vec![
                (Symbol::intern("next-repo"), Some(head.clone())),
                (Symbol::intern(BUNDLED_TAIL_PENDING_ATTR), None),
            ]);
        }
        head
    }

    /// One FileSystem repository per bundled distribution, chained in the
    /// order `resolve_module_path` searches them; Nil when there are none.
    fn build_bundled_chain(&self) -> Value {
        // Build the sub-chain back to front so each link can point at the next.
        let mut head = Value::NIL;
        for lib in self.bundled_lib_paths.iter().rev() {
            head = self.bundled_repo_link(lib, head);
        }
        head
    }

    /// The last link of `$*REPO`'s chain.
    fn repo_chain_tail(&self) -> Option<Value> {
        let mut cursor = self.env.get("*REPO").cloned()?;
        loop {
            let ValueView::Instance { attributes, .. } = cursor.view() else {
                return Some(cursor);
            };
            let next = attributes.as_map().get("next-repo").cloned();
            match next {
                Some(next)
                    if next.truthy() && matches!(next.view(), ValueView::Instance { .. }) =>
                {
                    cursor = next;
                }
                _ => return Some(cursor),
            }
        }
    }

    /// Hang `repo` off the last link of `$*REPO`'s chain.
    pub(super) fn append_repo_to_chain_tail(&mut self, repo: Value) {
        if let Some(tail) = self.repo_chain_tail()
            && let ValueView::Instance { attributes, .. } = tail.view()
        {
            attributes.insert("next-repo", repo);
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
