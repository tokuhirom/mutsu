//! `CompUnit::Repository::FileSystem`'s `load` and `need`: the two methods
//! that compile a module as a compunit of the repository (ADR-11276: rows of
//! `method_table::instances::compunit_repo`).

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{AttrMap, RuntimeError, Value};
use crate::meta_ns::MetaNs;
use std::collections::HashMap;

impl Interpreter {
    /// `CompUnit::Repository::FileSystem.load($file)`.
    pub(crate) fn cur_fs_load(
        &mut self,
        repo: &Value,
        attributes: &AttrMap,
        file_arg: Value,
    ) -> Result<Value, RuntimeError> {
                // `.load(IO::Path)` resolves the file relative to
                // this repository's own prefix and compiles it as a
                // compunit (a syntax error surfaces here), recording
                // the result in `.loaded` -- but, verified against
                // `raku`, it does NOT merge the file's symbols into
                // GLOBAL: `LoadMe::hi` stays unreachable after
                // `$r.load("LoadMe.rakumod".IO)` even though the
                // module ran under `unit module LoadMe`. So unlike
                // `need` below, this never calls `run_block` --
                // only the shared `parse_module_source` compile step.
                // An absolute path is loaded as given, wherever it is, as
                // Rakudo does (verified against `raku`: `$*REPO.load` of a file
                // outside every repository prefix compiles it, with the absolute
                // path as the short-name). Zef's `Staging` fallback loads the
                // core distribution's `CompUnit::Repository::Staging` this way
                // through `$*REPO`, a repository whose prefix is elsewhere
                // (#16149). The path is one the program named itself, not a
                // search location.
                let file_str = file_arg.to_string_value();
                let prefix = attributes
                    .get("prefix")
                    .map(Value::to_string_value)
                    .unwrap_or_default();
                let canonical_prefix = std::fs::canonicalize(&prefix)
                    .unwrap_or_else(|_| std::path::PathBuf::from(&prefix));
                let requested = std::path::Path::new(&file_str);
                let resolved = if requested.is_absolute() {
                    std::fs::canonicalize(requested).ok()
                } else {
                    let candidate = canonical_prefix.join(requested);
                    candidate.exists().then_some(candidate)
                };
                let Some(source_path) = resolved else {
                    return Err(RuntimeError::new(format!(
                        "Could not find {file_str} in:\n    {}",
                        canonical_prefix.display()
                    )));
                };
                // Rakudo's short-name for a `.load`ed compunit is the
                // bare relative file name, not a `::`-joined module
                // name -- verified against `raku`.
                let short_name_str = source_path
                    .strip_prefix(&canonical_prefix)
                    .unwrap_or(&source_path)
                    .to_string_lossy()
                    .to_string();
                let (stmts, _) = self.parse_module_source(&short_name_str, &source_path)?;
                // A package the file declares *inside one that already exists*
                // (`class CompUnit::Repository::Staging`) is installed into that
                // existing stash, so it is reachable by name after the load; a
                // new top-level package (`unit module LoadMe`) stays in the
                // compunit's own GLOBAL (verified against `raku` for both).
                // Zef's `Staging` fallback relies on the first.
                if stmts.iter().any(|stmt| {
                    crate::ast::package_header::declaration(stmt).is_some_and(|decl| {
                        let (crate::ast::Stmt::ClassDecl { name, .. }
                        | crate::ast::Stmt::Package { name, .. }
                        | crate::ast::Stmt::RoleDecl { name, .. }) = decl
                        else {
                            return false;
                        };
                        crate::qualified::package_parent(*name)
                            .is_some_and(|parent| self.is_known_package(parent.as_str()))
                    })
                }) {
                    self.load_module_from_path(&short_name_str, source_path.clone())?;
                }
                let mut attrs = HashMap::new();
                attrs.insert("from".to_string(), Value::str_from("Perl6"));
                attrs.insert("repo".to_string(), repo.clone());
                attrs.insert("repo-id".to_string(), Value::str(short_name_str.clone()));
                attrs.insert("short-name".to_string(), Value::str(short_name_str));
                attrs.insert("precompiled".to_string(), Value::FALSE);
                let compunit = Value::make_instance(Symbol::intern("CompUnit"), attrs);
                self.cur_repo_loaded_push(&prefix, compunit.clone());
                Ok(compunit)
    }

    /// `CompUnit::Repository::FileSystem.need($spec)`.
    pub(crate) fn cur_fs_need(
        &mut self,
        repo: &Value,
        attributes: &AttrMap,
        depspec: Value,
    ) -> Result<Value, RuntimeError> {
                // A depspec is a `CompUnit::DependencySpecification`
                // instance, or a bare short-name string. Extract the
                // short-name; the FileSystem repo resolves purely by name.
                let (short_name_str, ..) = self.extract_depspec_fields(&depspec);
                if short_name_str.is_empty() {
                    return Ok(Value::NIL);
                }
                let prefix = attributes
                    .get("prefix")
                    .map(Value::to_string_value)
                    .unwrap_or_default();
                let canonical_prefix = std::fs::canonicalize(&prefix)
                    .unwrap_or_else(|_| std::path::PathBuf::from(&prefix))
                    .to_string_lossy()
                    .to_string();
                // Check cache first
                let cache_key = MetaNs::Compunit
                    .owned_key_pair_for_strs(&canonical_prefix, &short_name_str);
                if let Some(existing) = self.env.get(&cache_key).cloned() {
                    return Ok(existing);
                }
                // The repository's loaded list outlives the frame a `need`
                // ran in: the same unit is the same compunit (#14539).
                if let Some(existing) = self
                    .module
                    .cur_repo
                    .loaded
                    .get(&prefix)
                    .and_then(|units| {
                        units.iter().find(|unit| match unit.view() {
                            crate::value::ValueView::Instance { attributes, .. } => attributes
                                .as_map()
                                .get("short-name")
                                .is_some_and(|name| name.to_string_value() == short_name_str),
                            _ => false,
                        })
                    })
                    .cloned()
                {
                    return Ok(existing);
                }
                // Try to find the module file in the prefix directory
                let relative = short_name_str.replace("::", "/");
                let mut found_path = None;
                for ext in [".rakumod", ".pm6", ".raku", ".pm"] {
                    let candidate = std::path::Path::new(&canonical_prefix)
                        .join(format!("{relative}{ext}"));
                    if candidate.exists() {
                        found_path = Some(candidate);
                        break;
                    }
                }
                // If not found in prefix, try the standard module resolution
                if found_path.is_none() {
                    found_path = self.resolve_module_path(&short_name_str).map(|(p, _)| p);
                }
                let Some(source_path) = found_path else {
                    return Ok(Value::NIL);
                };
                let repo_precomp_enabled = attributes
                    .get("__mutsu_precomp_enabled")
                    .is_none_or(Value::truthy);
                // Load the module as its own compilation unit (the same
                // machinery as a `use`, minus the import), using the
                // precompilation cache when available. Explicitly
                // constructed FileSystem repositories default to
                // precomp-disabled behavior.
                let saved_precomp = self.module.precomp_enabled;
                if !repo_precomp_enabled {
                    self.module.precomp_enabled = false;
                }
                let loaded = self.load_module_from_path(&short_name_str, source_path);
                self.module.precomp_enabled = saved_precomp;
                let precompiled = loaded?;
                crate::runtime::cow_table_mut(&mut self.module.loaded_modules)
                    .insert(short_name_str.clone());
                let mut attrs = HashMap::new();
                attrs.insert("from".to_string(), Value::str_from("Perl6"));
                // Same `.repo` / `.repo-id` / `.distribution` as a
                // `resolve` of this spec reports (a module found only
                // outside the prefix has no distribution here).
                attrs.insert("repo".to_string(), repo.clone());
                if let Some(dist) = self
                    .cur_fs_candidates(&prefix, &depspec)?
                    .as_list_items()
                    .and_then(Self::best_candidate)
                {
                    attrs.insert(
                        "repo-id".to_string(),
                        Value::str(Self::compunit_repo_id(&prefix, &dist, &short_name_str)),
                    );
                    attrs.insert("distribution".to_string(), dist);
                }
                attrs.insert("short-name".to_string(), Value::str(short_name_str));
                attrs.insert(
                    "precompiled".to_string(),
                    Value::truth(precompiled && repo_precomp_enabled),
                );
                let compunit = Value::make_instance(Symbol::intern("CompUnit"), attrs);
                self.env.insert(cache_key, compunit.clone());
                self.cur_repo_loaded_push(&prefix, compunit.clone());
                Ok(compunit)
    }
}
