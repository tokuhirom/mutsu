use super::*;

impl Interpreter {
    pub(super) fn sort_candidates_by_specificity(
        &self,
        candidates: &mut [(String, Arc<FunctionDef>)],
    ) {
        candidates.sort_by(|a, b| {
            let a_rank = self.candidate_specificity_rank(&a.1);
            let b_rank = self.candidate_specificity_rank(&b.1);
            b_rank.cmp(&a_rank).then(a.0.cmp(&b.0))
        });
    }

    pub(super) fn resolve_function_with_alias(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<Arc<FunctionDef>> {
        self.clear_pending_dispatch_error();
        if let Some(def) = self.resolve_function_with_types(name, arg_values) {
            return Some(def);
        }
        if self.pending_dispatch_error.is_some() {
            return None;
        }
        if name.contains(':') || name.contains("::") {
            return None;
        }
        for alias in [format!("prefix:<{name}>"), format!("postfix:<{name}>")] {
            if let Some(def) = self.resolve_function_with_types(&alias, arg_values) {
                return Some(def);
            }
        }
        None
    }

    pub(super) fn resolve_function_with_arity(
        &self,
        name: &str,
        arity: usize,
    ) -> Option<Arc<FunctionDef>> {
        if name.contains("::") {
            let multi_key = format!("{}/{}", name, arity);
            if let Some(def) = self.registry().functions.get(&Symbol::intern(&multi_key)) {
                return Some(def.clone());
            }
            return self
                .registry()
                .functions
                .get(&Symbol::intern(name))
                .cloned();
        }
        // Try multi-dispatch with arity first
        let multi_local = format!("{}::{}/{}", self.current_package(), name, arity);
        if let Some(def) = self.registry().functions.get(&Symbol::intern(&multi_local)) {
            return Some(def.clone());
        }
        let multi_global = format!("GLOBAL::{}/{}", name, arity);
        if let Some(def) = self
            .registry()
            .functions
            .get(&Symbol::intern(&multi_global))
        {
            return Some(def.clone());
        }
        // Fall back to regular lookup
        self.resolve_function(name)
    }

    pub(crate) fn resolve_function_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<Arc<FunctionDef>> {
        // Arity counts only positional args, excluding named args (Pair values)
        let arity = arg_values
            .iter()
            .filter(|v| !matches!(v, Value::Pair(..)))
            .count();
        if name.contains("::") {
            if let Some(def) = self
                .registry()
                .functions
                .get(&Symbol::intern(name))
                .cloned()
            {
                // Block access to my-scoped (non-our) package items from outside
                // their declaring package.
                if self.is_my_scoped_package_item(name)
                    && let Some((pkg_prefix, _)) = name.rsplit_once("::")
                    && pkg_prefix != self.current_package()
                    && !self
                        .current_package()
                        .starts_with(&format!("{}::", pkg_prefix))
                {
                    return None;
                }
                return Some(def);
            }
            let prefix = format!("{}/{arity}:", name);
            let untyped_key = format!("{}/{}", name, arity);
            let untyped_key_sym = Symbol::intern(&untyped_key);
            let untyped_m_prefix = format!("{}__m", untyped_key);
            let mut candidates: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(key, _)| {
                    let ks = key.resolve();
                    ks.starts_with(&prefix)
                        || **key == untyped_key_sym
                        || ks.starts_with(&untyped_m_prefix)
                })
                .map(|(key, def)| (key.resolve(), def.clone()))
                .collect();
            self.sort_candidates_by_specificity(&mut candidates);
            if let Some(def) = self.choose_best_matching_candidate(name, arg_values, candidates) {
                return Some(def);
            }
            // Capture-subsignature candidates (`multi foo(|c(...))`) are registered
            // at arity 0 because the capture consumes all arguments; the real
            // dispatch parameters live in the subsignature.  Such candidates are
            // not found by the arity-keyed lookup above, so collect them
            // separately (across all arities under `name/`) and dispatch on them.
            let subsig_prefix = format!("{}/", name);
            let mut subsig_candidates: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(key, def)| {
                    key.resolve().starts_with(&subsig_prefix)
                        && def.param_defs.iter().any(|p| p.is_capture_subsignature())
                })
                .map(|(key, def)| (key.resolve(), def.clone()))
                .collect();
            if !subsig_candidates.is_empty() {
                self.sort_candidates_by_specificity(&mut subsig_candidates);
                if let Some(def) =
                    self.choose_best_matching_candidate(name, arg_values, subsig_candidates)
                {
                    return Some(def);
                }
            }
            if let Some(def) = self
                .registry()
                .functions
                .get(&Symbol::intern(name))
                .cloned()
            {
                if !self.is_my_scoped_package_item(name) {
                    return Some(def);
                } else if let Some((pkg_prefix, _)) = name.rsplit_once("::")
                    && (pkg_prefix == self.current_package()
                        || self
                            .current_package()
                            .starts_with(&format!("{}::", pkg_prefix)))
                {
                    return Some(def);
                }
            }
            // Try qualifying with the current package prefix when the
            // prefix package is visible in the current scope (i.e., exists
            // as a Package value in env).  This handles calls like
            // `Our::Package::pkg()` inside `PackageTest` where the nested
            // package was registered as `PackageTest::Our::Package`.
            if self.current_package() != "GLOBAL" {
                // Check if the prefix package (everything before the last `::`)
                // is visible in env as a Package type object.
                let prefix_visible = if let Some((pkg_prefix, _)) = name.rsplit_once("::") {
                    self.env.get(pkg_prefix).is_some()
                        || self
                            .env
                            .get(&format!("{}::{}", self.current_package(), pkg_prefix))
                            .is_some()
                } else {
                    false
                };
                if prefix_visible {
                    let qualified = format!("{}::{}", self.current_package(), name);
                    if let Some(def) = self
                        .registry()
                        .functions
                        .get(&Symbol::intern(&qualified))
                        .cloned()
                    {
                        return Some(def);
                    }
                    let q_prefix = format!("{qualified}/{arity}:");
                    let q_untyped_key = format!("{qualified}/{}", arity);
                    let q_untyped_key_sym = Symbol::intern(&q_untyped_key);
                    let q_untyped_m_prefix = format!("{}__m", q_untyped_key);
                    let mut q_candidates: Vec<(String, Arc<FunctionDef>)> = self
                        .registry()
                        .functions
                        .iter()
                        .filter(|(key, _)| {
                            let ks = key.resolve();
                            ks.starts_with(&q_prefix)
                                || **key == q_untyped_key_sym
                                || ks.starts_with(&q_untyped_m_prefix)
                        })
                        .map(|(key, def)| (key.resolve(), def.clone()))
                        .collect();
                    self.sort_candidates_by_specificity(&mut q_candidates);
                    if let Some(def) =
                        self.choose_best_matching_candidate(&qualified, arg_values, q_candidates)
                    {
                        return Some(def);
                    }
                }
            }
            return None;
        }
        let exact_local = format!("{}::{}", self.current_package(), name);
        if let Some(def) = self
            .registry()
            .functions
            .get(&Symbol::intern(&exact_local))
            .cloned()
        {
            return Some(def);
        }
        let exact_global = format!("GLOBAL::{}", name);
        if let Some(def) = self
            .registry()
            .functions
            .get(&Symbol::intern(&exact_global))
            .cloned()
        {
            return Some(def);
        }
        let prefix_local = format!("{}::{}/{}:", self.current_package(), name, arity);
        let prefix_global = format!("GLOBAL::{}/{}:", name, arity);
        let generic_keys = [
            format!("{}::{}/{}", self.current_package(), name, arity),
            format!("GLOBAL::{}/{}", name, arity),
        ];
        let mut found_multi_candidates = false;
        let mut candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(key, _)| {
                let ks = key.resolve();
                ks.starts_with(&prefix_local) || ks.starts_with(&prefix_global)
            })
            .map(|(key, def)| (key.resolve(), def.clone()))
            .collect();
        for key in &generic_keys {
            let key_sym = Symbol::intern(key);
            let m_prefix = format!("{}__m", key);
            let more: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(k, _)| **k == key_sym || k.resolve().starts_with(&m_prefix))
                .map(|(k, def)| (k.resolve(), def.clone()))
                .collect();
            if !more.is_empty() {
                found_multi_candidates = true;
            }
            candidates.extend(more);
        }
        self.sort_candidates_by_specificity(&mut candidates);
        if let Some(def) = self.choose_best_matching_candidate(name, arg_values, candidates) {
            return Some(def);
        }
        // Try optional/default candidates with different arities.
        // These can match calls with fewer positional arguments.
        let optional_prefixes = [
            format!("{}::{}/", self.current_package(), name),
            format!("GLOBAL::{}/", name),
        ];
        let mut optional_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                optional_prefixes
                    .iter()
                    .any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| !p.named && (p.optional_marker || p.default.is_some()))
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !optional_candidates.is_empty() {
            found_multi_candidates = true;
        }
        optional_candidates.sort_by(|a, b| {
            let a_has_where = a.1.param_defs.iter().any(|p| p.where_constraint.is_some());
            let b_has_where = b.1.param_defs.iter().any(|p| p.where_constraint.is_some());
            let a_has_subsig = a.1.param_defs.iter().any(|p| p.sub_signature.is_some());
            let b_has_subsig = b.1.param_defs.iter().any(|p| p.sub_signature.is_some());
            b_has_where
                .cmp(&a_has_where)
                .then(b_has_subsig.cmp(&a_has_subsig))
                .then(a.0.cmp(&b.0))
        });
        if let Some(def) =
            self.choose_best_matching_candidate(name, arg_values, optional_candidates)
        {
            return Some(def);
        }
        // Try slurpy candidates with different arities (slurpy params accept
        // variable number of args, so the registered arity may differ from call arity).
        let slurpy_prefixes = [
            format!("{}::{}/", self.current_package(), name),
            format!("GLOBAL::{}/", name),
        ];
        let mut slurpy_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                slurpy_prefixes.iter().any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| p.slurpy || p.is_capture_subsignature())
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !slurpy_candidates.is_empty() {
            found_multi_candidates = true;
        }
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        if let Some(def) = self.choose_best_matching_candidate(name, arg_values, slurpy_candidates)
        {
            return Some(def);
        }
        // Try candidates from other arities (e.g., optional/default positional params).
        // This allows calls with fewer args to match signatures like `$x = ...`.
        let any_arity_prefixes = [
            format!("{}::{name}/", self.current_package()),
            format!("GLOBAL::{name}/"),
        ];
        let mut any_arity_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, _)| {
                let ks = k.resolve();
                any_arity_prefixes
                    .iter()
                    .any(|prefix| ks.starts_with(prefix))
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !any_arity_candidates.is_empty() {
            found_multi_candidates = true;
        }
        self.sort_candidates_by_specificity(&mut any_arity_candidates);
        if let Some(def) =
            self.choose_best_matching_candidate(name, arg_values, any_arity_candidates)
        {
            return Some(def);
        }
        // Fall back to arity-only if no proto declared and no multi candidates were found.
        // When multi candidates exist but none matched (e.g., sub-signature arity mismatch),
        // falling back would bypass the sub-signature check.
        if self.has_proto(name) || found_multi_candidates {
            None
        } else {
            self.resolve_function_with_arity(name, arity)
        }
    }

    /// Collect all matching multi dispatch candidates for a function call,
    /// sorted by specificity (most specific first). Used by callsame/nextcallee.
    pub(crate) fn resolve_all_matching_candidates(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Vec<FunctionDef> {
        let arity = arg_values.len();
        let mut all_matches = Vec::new();

        // Collect from typed candidates
        for prefix_base in [
            format!("{}::{}/{}:", self.current_package(), name, arity),
            format!("GLOBAL::{}/{}:", name, arity),
        ] {
            let candidates: Vec<FunctionDef> = self
                .registry()
                .functions
                .iter()
                .filter(|(key, _)| key.resolve().starts_with(&prefix_base))
                .map(|(_, def)| (**def).clone())
                .collect();
            for def in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    all_matches.push(def);
                }
            }
        }

        // Collect from generic (untyped) candidates
        let generic_keys = [
            format!("{}::{}/{}", self.current_package(), name, arity),
            format!("GLOBAL::{}/{}", name, arity),
        ];
        for key in &generic_keys {
            let key_sym = Symbol::intern(key);
            let m_prefix = format!("{}__m", key);
            let mut candidates: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(k, _)| **k == key_sym || k.resolve().starts_with(&m_prefix))
                .map(|(k, def)| (k.resolve(), def.clone()))
                .collect();
            candidates.sort_by(|a, b| {
                let a_has_subsig = a.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                let b_has_subsig = b.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                b_has_subsig.cmp(&a_has_subsig).then(a.0.cmp(&b.0))
            });
            for (_, def) in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    let fp = crate::ast::function_body_fingerprint(
                        &def.params,
                        &def.param_defs,
                        &def.body,
                    );
                    if !all_matches.iter().any(|m: &FunctionDef| {
                        crate::ast::function_body_fingerprint(&m.params, &m.param_defs, &m.body)
                            == fp
                    }) {
                        all_matches.push((*def).clone());
                    }
                }
            }
        }

        // Collect from slurpy candidates
        let slurpy_prefixes = [
            format!("{}::{}/", self.current_package(), name),
            format!("GLOBAL::{}/", name),
        ];
        let mut slurpy_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                slurpy_prefixes.iter().any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| p.slurpy || p.is_capture_subsignature())
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        for (_, def) in slurpy_candidates {
            if self.args_match_param_types(arg_values, &def.param_defs) {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if !all_matches.iter().any(|m: &FunctionDef| {
                    crate::ast::function_body_fingerprint(&m.params, &m.param_defs, &m.body) == fp
                }) {
                    all_matches.push((*def).clone());
                }
            }
        }

        all_matches
    }
}
