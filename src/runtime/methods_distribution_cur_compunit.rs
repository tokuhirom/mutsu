//! `CompUnit` objects produced by the repository API: `resolve` on every
//! `CompUnit::Repository` kind (FileSystem / Installation), and the accessor
//! methods a `CompUnit` answers (`.repo`, `.repo-id`, `.distribution`, ...).
//!
//! Rakudo defines `resolve` per repository as "take the best candidate for the
//! depspec and describe it as a `CompUnit`, or delegate to `next-repo`":
//!
//! ```raku
//! method resolve(CompUnit::DependencySpecification $spec --> CompUnit:D) {
//!     with self!matching-dist($spec) {
//!         return CompUnit.new(:short-name($spec.short-name),
//!             :repo-id(self!comp-unit-id($spec.short-name).Str),
//!             :repo(self), :distribution($_));
//!     }
//!     return self.next-repo.resolve($spec) if self.next-repo;
//!     Nil
//! }
//! ```
//!
//! The resolved `CompUnit` is *not loaded*: its `handle` is the
//! `CompUnit::Handle` type object and `precompiled` is False, exactly as in
//! Rakudo. Loading stays the job of `need`.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};
use std::collections::HashMap;

impl Interpreter {
    /// `CompUnit::Repository::{FileSystem,Installation}.resolve($spec)`.
    ///
    /// `repo` is the invocant itself (it becomes the CompUnit's `.repo`),
    /// `attributes` its attribute map.
    pub(crate) fn cur_resolve(
        &mut self,
        repo: &Value,
        class_name: &str,
        attributes: &AttrMap,
        depspec: Value,
    ) -> Result<Value, RuntimeError> {
        let prefix = attributes
            .get("prefix")
            .map(Value::to_string_value)
            .unwrap_or_default();
        let candidates = if class_name == "CompUnit::Repository::Installation" {
            self.cur_inst_candidates(&prefix, &depspec)?
        } else {
            self.cur_fs_candidates(&prefix, &depspec)?
        };
        let best = match candidates.view() {
            ValueView::Array(arr, _) => Self::best_candidate(&arr),
            _ => None,
        };
        if let Some(distribution) = best {
            let (short_name, ..) = self.extract_depspec_fields(&depspec);
            let repo_id = Self::compunit_repo_id(&prefix, &distribution, &short_name);
            // Rakudo's FileSystem repository builds its CompUnit from the
            // short-name alone (version/auth/api stay type objects); an
            // Installation repository reports the installed distribution's.
            let from_meta = class_name == "CompUnit::Repository::Installation";
            return Ok(Self::make_resolved_compunit(
                repo.clone(),
                short_name,
                repo_id,
                distribution,
                from_meta,
            ));
        }
        match attributes.get("next-repo") {
            Some(next) if next.truthy() => {
                self.call_method_with_values(next.clone(), "resolve", vec![depspec])
            }
            _ => Ok(Value::NIL),
        }
    }

    /// The best of several matching distributions: the highest version, as
    /// Rakudo's repositories sort their candidates before taking the head.
    pub(crate) fn best_candidate(candidates: &[Value]) -> Option<Value> {
        let version_of = |dist: &Value| -> Vec<crate::value::VersionPart> {
            let ver = match dist.view() {
                ValueView::Instance { attributes, .. } => {
                    attributes.as_map().get("meta").and_then(|meta| {
                        meta.hash_get_str("ver")
                            .or_else(|| meta.hash_get_str("version"))
                    })
                }
                _ => None,
            };
            let text = ver.map(|v| v.to_string_value()).unwrap_or_default();
            Value::parse_version_string(text.trim_start_matches('v')).0
        };
        candidates
            .iter()
            .max_by(|a, b| crate::runtime::utils::version_cmp_parts(&version_of(a), &version_of(b)))
            .cloned()
    }

    /// A compunit's `repo-id`. Rakudo computes
    /// `CompUnit::PrecompilationId.new-from-string($dist.id ~ $name)`, an
    /// uppercase SHA-1 -- stable for as long as the distribution is.
    pub(crate) fn compunit_repo_id(prefix: &str, distribution: &Value, short_name: &str) -> String {
        let dist_id = Self::resolved_dist_id(prefix, distribution);
        crate::builtins::sha1::sha1_hex_uppercase(format!("{dist_id}{short_name}").as_bytes())
    }

    /// A FileSystem repository's `id`: an uppercase SHA-1 identifying its
    /// canonical prefix.
    pub(crate) fn cur_fs_id(prefix: &str) -> String {
        let canonical = std::fs::canonicalize(prefix)
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| prefix.to_string());
        crate::builtins::sha1::sha1_hex_uppercase(canonical.as_bytes())
    }

    /// The identity of the distribution a compunit was resolved from, which
    /// seeds its `repo-id`. An installed distribution carries its own
    /// `dist-id` (the `dist/<id>.json` stem); a FileSystem repository has one
    /// implicit distribution per prefix, so its canonical prefix identifies it.
    fn resolved_dist_id(prefix: &str, distribution: &Value) -> String {
        if let ValueView::Instance { attributes, .. } = distribution.view()
            && let Some(id) = attributes.as_map().get("dist-id")
        {
            return id.to_string_value();
        }
        Self::cur_fs_id(prefix)
    }

    fn make_resolved_compunit(
        repo: Value,
        short_name: String,
        repo_id: String,
        distribution: Value,
        from_meta: bool,
    ) -> Value {
        let meta = match distribution.view() {
            ValueView::Instance { attributes, .. } if from_meta => {
                attributes.as_map().get("meta").cloned()
            }
            _ => None,
        };
        let meta_field = |key: &str| meta.as_ref().and_then(|m| m.hash_get_str(key));
        let version = meta_field("ver")
            .or_else(|| meta_field("version"))
            .map(Self::version_from_value)
            .unwrap_or_else(|| Value::package(Symbol::intern("Version")));
        let api = meta_field("api")
            .map(Self::version_from_value)
            .unwrap_or_else(|| Value::package(Symbol::intern("Version")));
        let auth = meta_field("auth").unwrap_or_else(|| Value::package(Symbol::intern("Str")));
        let mut attrs = HashMap::new();
        attrs.insert("from".to_string(), Value::str_from("Perl6"));
        attrs.insert("short-name".to_string(), Value::str(short_name));
        attrs.insert("version".to_string(), version);
        attrs.insert("auth".to_string(), auth);
        attrs.insert("api".to_string(), api);
        attrs.insert("repo".to_string(), repo);
        attrs.insert("repo-id".to_string(), Value::str(repo_id));
        attrs.insert(
            "handle".to_string(),
            Value::package(Symbol::intern("CompUnit::Handle")),
        );
        attrs.insert("precompiled".to_string(), Value::FALSE);
        attrs.insert("distribution".to_string(), distribution);
        Value::make_instance(Symbol::intern("CompUnit"), attrs)
    }

    /// `$repo.loaded`: the CompUnits a repository has loaded, in load order.
    pub(crate) fn cur_repo_loaded(&self, prefix: &str) -> Vec<Value> {
        self.cur_repo
            .loaded
            .get(prefix)
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn cur_repo_loaded_push(&mut self, prefix: &str, compunit: Value) {
        self.cur_repo
            .loaded
            .entry(prefix.to_string())
            .or_default()
            .push(compunit);
    }

    /// Methods of a `CompUnit` instance (from `need` or `resolve`).
    pub(crate) fn dispatch_compunit_method(
        &mut self,
        attributes: &AttrMap,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let attr = |key: &str| attributes.get(key).cloned();
        let value = match method {
            "short-name" | "name" | "Str" | "gist" => attr("short-name").unwrap_or(Value::NIL),
            "version" | "api" => {
                attr(method).unwrap_or_else(|| Value::package(Symbol::intern("Version")))
            }
            "auth" => attr("auth").unwrap_or_else(|| Value::package(Symbol::intern("Str"))),
            "from" => attr("from").unwrap_or_else(|| Value::str_from("Perl6")),
            "repo" | "repo-id" | "distribution" => attr(method).unwrap_or(Value::NIL),
            "precompiled" => attr("precompiled").unwrap_or(Value::FALSE),
            // A resolved-but-unloaded CompUnit carries the `CompUnit::Handle`
            // type object, as in Rakudo. A loaded one (from `need`) gets a
            // handle carrying the symbols it loaded, so they can later be
            // merged into GLOBAL via `merge-symbols`.
            "handle" => match attr("handle") {
                Some(handle) => handle,
                None => {
                    let mut handle_attrs = HashMap::new();
                    if let Some(syms) = attributes.get("globalish-symbols") {
                        handle_attrs.insert("globalish-symbols".to_string(), syms.clone());
                    }
                    Value::make_instance(Symbol::intern("CompUnit::Handle"), handle_attrs)
                }
            },
            "globalish-package" => self.make_globalish_package(attributes.get("globalish-symbols")),
            _ => return None,
        };
        Some(Ok(value))
    }
}
