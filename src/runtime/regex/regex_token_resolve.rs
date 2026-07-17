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

    /// Memoized WITH-ARGS subrule resolution: (pkg, name, rendered-args) →
    /// parsed candidates. A parameterized rule (`multi rule expr($p)` called
    /// as `<expr(3)>`) re-resolves through a fresh scratch interpreter (bind
    /// args, eval body, bake params) on EVERY match-position probe; nested
    /// precedence-climbing grammars (P47) re-enter that per position and per
    /// LR-seed iteration, which is exponential without memoization. Cached
    /// only when every candidate's body references no variables beyond its
    /// own parameters (the args are part of the key), so a body reading an
    /// outer runtime lexical is never staled. Same `TOKEN_DEFS_GEN`
    /// invalidation as above.
    static PARSED_TOKEN_ARG_CANDIDATES: std::cell::RefCell<
        rustc_hash::FxHashMap<(String, String, String), CachedCandidates>,
    > = std::cell::RefCell::new(rustc_hash::FxHashMap::default());
}

/// True when `pattern` contains no interpolation the with-args cache key does
/// not cover: no `@`/`%` refs, no `$<cap>`/`$0`/`$(...)` forms, and every
/// `$ident` names one of `params` (raku identifier rules: `-` extends a name
/// only before a letter).
fn pattern_static_modulo_params(pattern: &str, params: &[String]) -> bool {
    if pattern.contains(['@', '%']) {
        return false;
    }
    let chars: Vec<char> = pattern.chars().collect();
    let mut i = 0usize;
    while i < chars.len() {
        match chars[i] {
            '\\' => i += 2,
            '$' => {
                let j0 = i + 1;
                let mut j = j0;
                if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    j += 1;
                    while j < chars.len() {
                        let c = chars[j];
                        let kebab = c == '-'
                            && chars
                                .get(j + 1)
                                .is_some_and(|n| n.is_alphabetic() || *n == '_');
                        if c.is_alphanumeric() || c == '_' || kebab {
                            j += 1;
                        } else {
                            break;
                        }
                    }
                    let name: String = chars[j0..j].iter().collect();
                    if !params
                        .iter()
                        .any(|p| p.trim_start_matches(['$', '@', '%', '&', ':']) == name)
                    {
                        return false;
                    }
                    i = j;
                } else {
                    // `$<cap>`, `$0`, `$(...)`, bare `$` — dynamic.
                    return false;
                }
            }
            _ => i += 1,
        }
    }
    true
}

impl Interpreter {
    /// Resolve an argument-less subrule to parsed candidates, memoized per
    /// (pkg, name). Returns `None` when any candidate's pattern is non-static
    /// (its parse depends on runtime variable interpolation) or fails to
    /// parse — callers fall back to the uncached per-call path.
    fn resolve_parsed_token_candidates_in_pkg(
        &mut self,
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
        &mut self,
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
        &mut self,
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
        // With-args memoization (see PARSED_TOKEN_ARG_CANDIDATES).
        let args_fp = if arg_values.is_empty() {
            None
        } else {
            let mut fp = String::new();
            for v in arg_values {
                fp.push('\u{0}');
                fp.push_str(&Self::format_named_regex_arg_value(v));
            }
            Some(fp)
        };
        let tok_gen =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        if let Some(fp) = &args_fp
            && let Some(hit) = PARSED_TOKEN_ARG_CANDIDATES.with(|c| {
                c.borrow()
                    .get(&(pkg.to_string(), spec.lookup_name.clone(), fp.clone()))
                    .filter(|(cached_gen, _)| *cached_gen == tok_gen)
                    .map(|(_, v)| std::sync::Arc::clone(v))
            })
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
        let arc = std::sync::Arc::new(parsed_list);
        if let Some(fp) = args_fp {
            // Cache only when every candidate def's body is self-contained
            // (no variable references beyond its own params).
            let cacheable = self
                .resolve_token_defs_in_pkg(&spec.lookup_name, pkg)
                .iter()
                .all(|def| {
                    def.body.iter().all(|stmt| match stmt {
                        crate::ast::Stmt::Expr(crate::ast::Expr::Literal(v)) => match v.view() {
                            ValueView::Regex(pat) => {
                                pattern_static_modulo_params(&pat, &def.params)
                            }
                            _ => false,
                        },
                        _ => false,
                    })
                });
            if cacheable {
                PARSED_TOKEN_ARG_CANDIDATES.with(|c| {
                    c.borrow_mut().insert(
                        (pkg.to_string(), spec.lookup_name.clone(), fp),
                        (tok_gen, std::sync::Arc::clone(&arc)),
                    );
                });
            }
        }
        (arc, raw_empty)
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
        &mut self,
        name: &str,
        pkg: &str,
        arg_values: &[Value],
    ) -> Vec<(String, String, Option<String>)> {
        let mut out = Vec::new();
        let defs = self.resolve_token_defs_in_pkg(name, pkg);
        // Rakudo multi semantics: a candidate whose literal parameter values
        // exactly match the arguments is narrower than a generic (`$p`)
        // candidate and wins the dispatch outright — `<expr(0)>` must pick
        // `multi rule expr(0)` alone, not also try `expr($p)` (whose body may
        // recurse with no base case, 99problems-41-to-50.t P47).
        let literal_matches: Vec<_> = defs
            .iter()
            .filter(|def| {
                let positional: Vec<_> = def.param_defs.iter().filter(|pd| !pd.named).collect();
                positional.len() == arg_values.len()
                    && !positional.is_empty()
                    && positional
                        .iter()
                        .zip(arg_values)
                        .all(|(pd, arg)| pd.literal_value.as_ref() == Some(arg))
            })
            .cloned()
            .collect();
        let defs = if literal_matches.is_empty() {
            // No exact literal candidate: drop candidates whose literal params
            // MISMATCH the args (`<expr(1)>` must not run `multi rule expr(0)`
            // — bind_function_args_values does not enforce literal values).
            defs.into_iter()
                .filter(|def| {
                    def.param_defs
                        .iter()
                        .filter(|pd| !pd.named)
                        .zip(arg_values)
                        .all(|(pd, arg)| {
                            pd.literal_value.is_none() || pd.literal_value.as_ref() == Some(arg)
                        })
                })
                .collect()
        } else {
            literal_matches
        };
        for def in defs {
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
        &mut self,
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
