use super::super::*;
use super::regex_helpers::NamedRegexLookupSpec;
use crate::symbol::Symbol;

/// A resolved-and-parsed subrule candidate: (parsed pattern, dispatch package,
/// `:sym<...>` key for proto candidates).
pub(super) type ParsedTokenCandidate = (std::sync::Arc<RegexPattern>, String, Option<String>);

/// Cache slot: the `TOKEN_DEFS_GEN` the entry was built under + the candidates.
type CachedCandidates = (u64, std::sync::Arc<Vec<ParsedTokenCandidate>>);

thread_local! {
    /// Memoized argument-less subrule resolution: (pkg, name) → candidates
    /// with their patterns already parsed. Rebuilding this on every
    /// `<subrule>` reference dominated grammar matching (the registry walk in
    /// `collect_token_patterns_for_scope` scans every `token_defs` key, plus a
    /// `parse_regex` cache probe per candidate — together ~18% of a
    /// JSON-grammar parse profile). Entries record the `TOKEN_DEFS_GEN` they
    /// were built under; a stale generation rebuilds (same invalidation
    /// discipline as `REGEX_PARSE_CACHE` and the charclass cache).
    static PARSED_TOKEN_CANDIDATES: std::cell::RefCell<
        rustc_hash::FxHashMap<(String, String), CachedCandidates>,
    > = std::cell::RefCell::new(rustc_hash::FxHashMap::default());
}

impl Interpreter {
    /// Resolve an argument-less subrule to parsed candidates, memoized per
    /// (pkg, name). Returns `None` when any candidate's pattern is non-static
    /// (its parse depends on runtime variable interpolation) or fails to
    /// parse — callers fall back to the uncached per-call path.
    fn resolve_parsed_token_candidates_in_pkg(
        &self,
        name: &str,
        pkg: &str,
    ) -> Option<std::sync::Arc<Vec<ParsedTokenCandidate>>> {
        let tok_gen =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        if let Some(hit) = PARSED_TOKEN_CANDIDATES.with(|c| {
            c.borrow()
                .get(&(pkg.to_string(), name.to_string()))
                .filter(|(cached_gen, _)| *cached_gen == tok_gen)
                .map(|(_, v)| std::sync::Arc::clone(v))
        }) {
            return Some(hit);
        }
        let raw = self.resolve_token_patterns_static_in_pkg(name, pkg);
        let mut parsed_list = Vec::with_capacity(raw.len());
        for (sub_pat, sub_pkg, sym_key) in raw {
            if !crate::runtime::regex_parse::regex_pattern_is_static(&sub_pat) {
                return None;
            }
            let parsed = self.parse_candidate_in_pkg(&sub_pat, &sub_pkg)?;
            parsed_list.push((parsed, sub_pkg, sym_key));
        }
        let arc = std::sync::Arc::new(parsed_list);
        PARSED_TOKEN_CANDIDATES.with(|c| {
            c.borrow_mut().insert(
                (pkg.to_string(), name.to_string()),
                (tok_gen, std::sync::Arc::clone(&arc)),
            );
        });
        Some(arc)
    }

    /// Parse a candidate's pattern in its OWN package so nested unqualified
    /// token references (notably char-class `<+name>` items) resolve against
    /// the grammar that defines them, not the outer caller's package.
    fn parse_candidate_in_pkg(
        &self,
        sub_pat: &str,
        sub_pkg: &str,
    ) -> Option<std::sync::Arc<RegexPattern>> {
        let saved_pkg = self.current_package();
        let switch_pkg = saved_pkg.as_str() != sub_pkg;
        if switch_pkg {
            self.set_current_package_shared(sub_pkg.to_string());
        }
        let parsed = self.parse_regex(sub_pat);
        if switch_pkg {
            self.set_current_package_shared(saved_pkg);
        }
        parsed
    }

    /// Parsed candidates for a subrule reference: memoized fast path for the
    /// argument-less case, per-call resolution + parse otherwise (candidates
    /// whose pattern fails to parse are skipped, as before). The second value
    /// reports whether the RAW resolution found nothing — that (not an empty
    /// parsed list) drives the method-subrule fallback.
    pub(super) fn parsed_subrule_candidates(
        &self,
        spec: &NamedRegexLookupSpec,
        pkg: &str,
        arg_values: &[Value],
    ) -> (std::sync::Arc<Vec<ParsedTokenCandidate>>, bool) {
        if arg_values.is_empty()
            && let Some(hit) = self.resolve_parsed_token_candidates_in_pkg(&spec.lookup_name, pkg)
        {
            let raw_empty = hit.is_empty();
            return (hit, raw_empty);
        }
        let raw = self.resolve_named_regex_candidates_in_pkg(spec, pkg, arg_values);
        let raw_empty = raw.is_empty();
        let mut parsed_list = Vec::with_capacity(raw.len());
        for (sub_pat, sub_pkg, sym_key) in raw {
            if let Some(parsed) = self.parse_candidate_in_pkg(&sub_pat, &sub_pkg) {
                parsed_list.push((parsed, sub_pkg, sym_key));
            }
        }
        (std::sync::Arc::new(parsed_list), raw_empty)
    }

    pub(super) fn resolve_token_defs_in_pkg(&self, name: &str, pkg: &str) -> Vec<Arc<FunctionDef>> {
        let mut out = Vec::new();
        if name.contains("::") {
            if let Some(defs) = self.registry().token_defs.get(&Symbol::intern(name)) {
                out.extend(defs.clone());
            }
            let sym_prefix_angle = format!("{name}:sym<");
            let sym_prefix_french = format!("{name}:sym\u{ab}");
            let mut sym_keys: Vec<String> = self
                .registry()
                .token_defs
                .keys()
                .map(|key| key.resolve())
                .filter(|key| {
                    key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french)
                })
                .collect();
            sym_keys.sort();
            for key in &sym_keys {
                if let Some(defs) = self.registry().token_defs.get(&Symbol::intern(key)) {
                    out.extend(defs.clone());
                }
            }
            // Walk the MRO for qualified names, merging proto candidates from
            // every ancestor (dedup by candidate identity, derived-first).
            if let Some(pos) = name.rfind("::") {
                let qual_pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                let mut seen: std::collections::HashSet<String> = out
                    .iter()
                    .map(|d| Self::token_def_identity(&d.name.resolve(), token_name))
                    .collect();
                for ancestor in self.mro_readonly(qual_pkg) {
                    if ancestor == qual_pkg {
                        continue;
                    }
                    self.collect_token_defs_for_scope_dedup(
                        &ancestor, token_name, &mut out, &mut seen,
                    );
                }
            }
            return out;
        }
        if !pkg.is_empty() {
            // Walk the MRO of pkg, merging proto candidates from every class:
            // a derived grammar adding `rule statement:sym<repeat>` keeps the
            // base grammar's candidates (advent2009-day24).
            let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
            for scope in self.mro_readonly(pkg) {
                self.collect_token_defs_for_scope_dedup(&scope, name, &mut out, &mut seen);
            }
            if !out.is_empty() {
                return out;
            }
        }
        self.collect_token_defs_for_scope("GLOBAL", name, &mut out);
        out
    }

    pub(super) fn resolve_token_patterns_with_args_in_pkg(
        &self,
        name: &str,
        pkg: &str,
        arg_values: &[Value],
    ) -> Vec<(String, String, Option<String>)> {
        let mut out = Vec::new();
        for def in self.resolve_token_defs_in_pkg(name, pkg) {
            let mut interp = Interpreter {
                env: self.env.clone(),
                current_package: Arc::new(RwLock::new(def.package.resolve())),
                ..Self::new_regex_scratch()
            };
            self.copy_decl_registry_into(&mut interp);
            let saved_env = interp.env.clone();
            if interp
                .bind_function_args_values(&def.param_defs, &def.params, arg_values)
                .is_ok()
            {
                interp.routine_stack.push(super::super::RoutineFrame {
                    package: def.package.resolve(),
                    name: def.name.resolve(),
                    line: None,
                    file: None,
                    is_method: false,
                    is_block: false,
                    def_file: None,
                });
                let result = interp.eval_block_value(&def.body);
                interp.routine_stack.pop();
                let value = match result {
                    Ok(v) => Some(v),
                    Err(e) if e.return_value.is_some() => e.return_value,
                    Err(_) => None,
                };
                if let Some(value) = value {
                    let pattern = match value.view() {
                        ValueView::Regex(pat) => pat.to_string(),
                        ValueView::Str(s) => s.to_string(),
                        ValueView::Nil => String::new(),
                        _ => value.to_string_value(),
                    };
                    // Bake the bound parameter values into any `{ ... }` code
                    // blocks of the pattern. This is needed because regex code
                    // blocks execute in the outer interpreter env (which does
                    // not contain the subrule's bound params).
                    let param_names: Vec<String> = def
                        .param_defs
                        .iter()
                        .filter(|pd| !pd.name.is_empty() && !pd.slurpy)
                        .map(|pd| {
                            pd.name
                                .trim_start_matches([':', '@', '%', '&', '!', '.'])
                                .to_string()
                        })
                        .collect();
                    let pattern =
                        interp.bake_bound_params_into_regex_code_blocks(&pattern, &param_names);
                    let pattern = interp.interpolate_bound_regex_scalars(&pattern);
                    if let Ok(instantiated) = interp.instantiate_named_regex_arg_calls(&pattern) {
                        let sym_val = Self::extract_sym_adverb(&def.name.resolve());
                        out.push((instantiated, def.package.resolve(), sym_val));
                    }
                }
            }
            interp.env = saved_env;
        }
        out
    }

    pub(super) fn resolve_named_regex_candidates_in_pkg(
        &self,
        spec: &NamedRegexLookupSpec,
        pkg: &str,
        arg_values: &[Value],
    ) -> Vec<(String, String, Option<String>)> {
        if arg_values.is_empty() {
            self.resolve_token_patterns_static_in_pkg(&spec.lookup_name, pkg)
        } else {
            self.resolve_token_patterns_with_args_in_pkg(&spec.lookup_name, pkg, arg_values)
        }
    }

    pub(super) fn format_named_regex_arg_value(value: &Value) -> String {
        match value.view() {
            ValueView::Str(s) => {
                let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
                format!("\"{escaped}\"")
            }
            ValueView::Bool(true) => "True".to_string(),
            ValueView::Bool(false) => "False".to_string(),
            ValueView::Nil => "Nil".to_string(),
            _ => value.to_string_value(),
        }
    }
}
