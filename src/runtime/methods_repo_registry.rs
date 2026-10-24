//! `CompUnit::RepositoryRegistry`'s class methods `repository-for-name`,
//! `repository-for-spec` and `run-script` (ADR-11276 §9.122). The rows are in
//! `builtins/method_table/io_concurrency/class_methods.rs`; each calls one
//! method here.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// A new repository of class `class` rooted at `prefix`, with the named
    /// arguments `extra` after it.
    // Cost: as the repository's constructor.
    fn new_repository(
        &mut self,
        class: &str,
        prefix: &str,
        extra: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut args = vec![Value::pair(
            "prefix".to_string(),
            Value::str(prefix.to_string()),
        )];
        args.extend(extra);
        self.call_method_with_values(Value::package(Symbol::intern(class)), "new", args)
    }

    /// `CompUnit::RepositoryRegistry.repository-for-name($name)`: a `file#`
    /// path names a `FileSystem` repository; `site`, `home`, `vendor`, `perl`
    /// and `core` the installation repository of that name. `Nil` otherwise.
    // Cost: as the repository's constructor.
    pub(crate) fn repo_registry_for_name(&mut self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(repo) = self.module.named_repositories.get(name) {
            return Ok(repo.clone());
        }
        if let Some(prefix) = name.strip_prefix("file#") {
            return self.new_repository("CompUnit::Repository::FileSystem", prefix, Vec::new());
        }
        if matches!(name, "site" | "home" | "vendor" | "perl" | "core")
            && let Some(dir) = self.default_repo_dir(name)
        {
            let mut extra = Vec::new();
            // Rakudo's core repository holds the modules it ships
            // (`Test`, ...); mutsu bundles those as a battery, so
            // chain that battery behind it (#9071).
            if name == "core"
                && let Some(bundled) = self.bundled_core_repo()
            {
                extra.push(Value::pair("next-repo".to_string(), bundled));
            }
            return self.new_repository(
                "CompUnit::Repository::Installation",
                &dir.display().to_string(),
                extra,
            );
        }
        Ok(Value::NIL)
    }

    /// `CompUnit::RepositoryRegistry.register-name($name, $repo)`: later
    /// `repository-for-name($name)` calls answer `$repo`.
    // Cost: O(1) amortized (the table is copy-on-write).
    pub(crate) fn repo_registry_register_name(&mut self, name: &str, repo: Value) -> Value {
        crate::runtime::cow_table_mut(&mut self.module.named_repositories)
            .insert(name.to_string(), repo);
        Value::NIL
    }

    /// `CompUnit::RepositoryRegistry.repository-for-spec($spec)`: an `inst#`
    /// or `file#` spec names an installation or a `FileSystem` repository.
    /// `Nil` otherwise.
    // Cost: as the repository's constructor.
    pub(crate) fn repo_registry_for_spec(&mut self, spec: &str) -> Result<Value, RuntimeError> {
        if let Some(prefix) = spec.strip_prefix("inst#") {
            return self.new_repository("CompUnit::Repository::Installation", prefix, Vec::new());
        }
        if let Some(prefix) = spec.strip_prefix("file#") {
            return self.new_repository("CompUnit::Repository::FileSystem", prefix, Vec::new());
        }
        Ok(Value::NIL)
    }
}
