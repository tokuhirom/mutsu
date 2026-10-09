//! Native constructors: distribution family (lifted from `dispatch_new_unallocated`).

use super::CtorCall;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    // Encoding::Decoder::Builtin.new($encoding, :translate-nl):
    // Rakudo's `nqp::decoderconfigure(nqp::create(self), ...)`.
    /// `Encoding::Decoder::Builtin`.
    pub(super) fn ctor_encoding_decoder_builtin(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            let encoding = args
                .iter()
                .find(|a| !matches!(a.view(), ValueView::Pair(..)))
                .map(Value::to_string_value)
                .unwrap_or_default();
            let translate_nl =
                Self::named_value(&args, "translate-nl").is_some_and(|v| v.truthy());
            crate::runtime::stream_decoder_object::new_decoder(
                &encoding,
                translate_nl,
            )
    }

    // `CompUnit.new(:short-name, :repo, :repo-id, ...)`: a plain
    // record of its named arguments, read back by the accessors in
    // `methods_distribution_cur_compunit`.
    /// `CompUnit`.
    pub(super) fn ctor_compunit(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            let mut attrs = HashMap::new();
            for arg in &args {
                if let ValueView::Pair(key, value) = arg.view() {
                    attrs.insert(key.to_string(), value.clone());
                }
            }
            Ok(Value::make_instance(*class_name, attrs))
    }

    /// `CompUnit::DependencySpecification`.
    pub(super) fn ctor_compunit_dependencyspecification(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            let mut short_name: Option<String> = None;
            let mut auth_matcher: Option<String> = None;
            let mut version_matcher: Option<String> = None;
            let mut api_matcher: Option<String> = None;
            for arg in &args {
                if let ValueView::Pair(key, value) = arg.view() {
                    match key.as_str() {
                        "short-name" => {
                            if let ValueView::Str(s) = value.view() {
                                short_name = Some(s.to_string());
                            } else {
                                return Err(RuntimeError::new(
                                    "CompUnit::DependencySpecification.new: :short-name must be a Str",
                                ));
                            }
                        }
                        "auth-matcher" => {
                            if !matches!(value.view(), ValueView::Bool(true)) {
                                auth_matcher = Some(value.to_string_value());
                            }
                        }
                        "version-matcher" => {
                            if !matches!(value.view(), ValueView::Bool(true)) {
                                version_matcher = Some(value.to_string_value());
                            }
                        }
                        "api-matcher" if !matches!(value.view(), ValueView::Bool(true)) => {
                            api_matcher = Some(value.to_string_value());
                        }
                        _ => {}
                    }
                }
            }
            let short_name = short_name.ok_or_else(|| {
                RuntimeError::new(
                    "CompUnit::DependencySpecification.new: :short-name is required",
                )
            })?;
            if auth_matcher.is_some() || version_matcher.is_some() || api_matcher.is_some()
            {
                let mut attrs = HashMap::new();
                attrs.insert("short-name".to_string(), Value::str(short_name));
                if let Some(a) = auth_matcher {
                    attrs.insert("auth-matcher".to_string(), Value::str(a));
                }
                if let Some(v) = version_matcher {
                    attrs.insert("version-matcher".to_string(), Value::str(v));
                }
                if let Some(a) = api_matcher {
                    attrs.insert("api-matcher".to_string(), Value::str(a));
                }
                return Ok(Value::make_instance(
                    Symbol::intern("CompUnit::DependencySpecification"),
                    attrs,
                ));
            }
            Ok(Value::comp_unit_dep_spec(Symbol::intern(&short_name)))
    }

    /// `Distribution::Path`.
    pub(super) fn ctor_distribution_path(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            let dir_path = args
                .first()
                .map(Value::to_string_value)
                .unwrap_or_else(|| ".".to_string());
            let meta_path = std::path::Path::new(&dir_path).join("META6.json");
            if !meta_path.exists() {
                return Err(RuntimeError::new(format!(
                    "No meta file located at {}",
                    meta_path.display()
                )));
            }
            let meta_json = std::fs::read_to_string(&meta_path).map_err(|e| {
                RuntimeError::new(format!("Cannot read {}: {e}", meta_path.display()))
            })?;
            let meta_hash = self.parse_json_to_value(&meta_json)?;
            let files_hash = self.build_dist_files_hash(&dir_path, &meta_hash);
            let mut attrs = HashMap::new();
            attrs.insert("prefix".to_string(), self.make_io_path_instance(&dir_path));
            attrs.insert("meta".to_string(), meta_hash);
            attrs.insert("files".to_string(), files_hash);
            Ok(Value::make_instance(
                Symbol::intern("Distribution::Path"),
                attrs,
            ))
    }

    /// `Distribution::Hash`.
    pub(super) fn ctor_distribution_hash(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            let mut meta_hash = Value::NIL;
            let mut prefix = String::new();
            for arg in &args {
                match arg.view() {
                    ValueView::Pair(key, value) if key == "prefix" => {
                        prefix = value.to_string_value();
                    }
                    ValueView::Hash(_) => {
                        if meta_hash == Value::NIL {
                            meta_hash = arg.clone();
                        }
                    }
                    _ => {
                        if meta_hash == Value::NIL {
                            meta_hash = arg.clone();
                        }
                    }
                }
            }
            let files_hash = self.build_dist_files_hash(&prefix, &meta_hash);
            let mut attrs = HashMap::new();
            attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
            attrs.insert("meta".to_string(), meta_hash);
            attrs.insert("files".to_string(), files_hash);
            Ok(Value::make_instance(
                Symbol::intern("Distribution::Hash"),
                attrs,
            ))
    }

    /// `CompUnit::Repository::Installation`.
    pub(super) fn ctor_compunit_repository_installation(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            let mut prefix = String::new();
            let mut next_repo = None;
            for arg in &args {
                if let ValueView::Pair(key, value) = arg.view() {
                    if key == "prefix" {
                        prefix = value.to_string_value();
                    } else if key == "next-repo" && value.truthy() {
                        next_repo = Some(value.clone());
                    }
                }
            }
            let mut attrs = HashMap::new();
            attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
            attrs.insert("short-id".to_string(), Value::str_from("inst"));
            if let Some(next) = next_repo {
                attrs.insert("next-repo".to_string(), next);
            }
            Ok(Value::make_instance(
                Symbol::intern("CompUnit::Repository::Installation"),
                attrs,
            ))
    }

    /// `CompUnit::Repository::FileSystem`.
    pub(super) fn ctor_compunit_repository_filesystem(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            let mut prefix = ".".to_string();
            let mut next_repo = None;
            for arg in &args {
                if let ValueView::Pair(key, value) = arg.view() {
                    if key == "prefix" {
                        prefix = value.to_string_value();
                    } else if key == "next-repo" && value.truthy() {
                        next_repo = Some(value.clone());
                    }
                }
            }
            let prefix_path = if prefix.is_empty() { "." } else { &prefix };
            let canonical_prefix = std::fs::canonicalize(prefix_path)
                .unwrap_or_else(|_| std::path::PathBuf::from(prefix_path))
                .to_string_lossy()
                .to_string();
            let cache_key = MetaNs::RepoFs.owned_key_for_str(&canonical_prefix);
            if let Some(existing) = self.env.get(&cache_key).cloned() {
                return Ok(existing);
            }
            let mut attrs = HashMap::new();
            attrs.insert(
                "prefix".to_string(),
                self.make_io_path_instance(&canonical_prefix),
            );
            attrs.insert("short-id".to_string(), Value::str_from("file"));
            attrs.insert("__mutsu_precomp_enabled".to_string(), Value::FALSE);
            // Like Rakudo, the per-prefix instance is created once, so
            // the first construction's `next-repo` is the one that sticks.
            if let Some(next) = next_repo {
                attrs.insert("next-repo".to_string(), next);
            }
            let repo = Value::make_instance(*class_name, attrs);
            self.env.insert(cache_key, repo.clone());
            Ok(repo)
    }
}
