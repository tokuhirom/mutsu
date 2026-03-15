use super::*;
use crate::symbol::Symbol;

type DispatchShape = (Option<String>, bool, bool, bool, bool, bool, bool);

impl Interpreter {
    fn constraint_base_name(constraint: &str) -> &str {
        let mut end = constraint.len();
        for (idx, ch) in constraint.char_indices() {
            if ch == '[' || ch == '(' || ch == ':' {
                end = idx;
                break;
            }
        }
        &constraint[..end]
    }

    fn clear_pending_dispatch_error(&mut self) {
        self.pending_dispatch_error = None;
    }

    pub(crate) fn take_pending_dispatch_error(&mut self) -> Option<RuntimeError> {
        self.pending_dispatch_error.take()
    }

    fn dispatch_visible_params<'a>(
        def: &'a FunctionDef,
    ) -> impl Iterator<Item = &'a ParamDef> + 'a {
        def.param_defs.iter().filter(|p| p.multi_invocant)
    }

    fn candidate_uses_order_sensitive_dispatch(&self, def: &FunctionDef) -> bool {
        Self::dispatch_visible_params(def).any(|p| {
            p.where_constraint.is_some()
                || p.type_constraint
                    .as_deref()
                    .map(Self::constraint_base_name)
                    .map(|base| self.subsets.contains_key(base))
                    .unwrap_or(false)
        })
    }

    fn candidate_dispatch_shape(&self, def: &FunctionDef) -> Vec<DispatchShape> {
        Self::dispatch_visible_params(def)
            .map(|p| {
                (
                    p.type_constraint.clone(),
                    p.named,
                    p.sub_signature.is_some(),
                    p.literal_value.is_some(),
                    p.traits.iter().any(|t| t == "rw"),
                    p.traits.iter().any(|t| t == "raw"),
                    p.traits.iter().any(|t| t == "copy"),
                )
            })
            .collect()
    }

    fn ambiguous_multi_dispatch_error(
        &self,
        name: &str,
        args: &[Value],
        defs: &[FunctionDef],
    ) -> RuntimeError {
        let arg_types = args
            .iter()
            .filter(|v| !matches!(v, Value::Pair(..)))
            .map(super::value_type_name)
            .collect::<Vec<_>>()
            .join(", ");
        let mut err = RuntimeError::new(format!("Ambiguous call to '{}({})'", name, arg_types));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Ambiguous call to {}({}); these signatures all match: {}",
                name,
                arg_types,
                defs.iter()
                    .map(|def| {
                        crate::value::signature::make_signature_value(
                            crate::value::signature::param_defs_to_sig_info(
                                &def.param_defs,
                                def.return_type.clone(),
                            ),
                        )
                        .to_string_value()
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        );
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Multi::Ambiguous"),
            attrs,
        )));
        err
    }

    fn choose_best_matching_candidate(
        &mut self,
        name: &str,
        args: &[Value],
        candidates: Vec<(String, FunctionDef)>,
    ) -> Option<FunctionDef> {
        let mut matches = Vec::new();
        let mut seen = std::collections::HashSet::new();
        for (_, def) in candidates {
            if self.args_match_param_types(args, &def.param_defs) {
                let fingerprint =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if !seen.insert(fingerprint) {
                    continue;
                }
                matches.push(def);
            }
        }
        if matches.len() <= 1 {
            return matches.into_iter().next();
        }

        let best_rank = self.candidate_specificity_rank(&matches[0]);
        let best_shape = self.candidate_dispatch_shape(&matches[0]);
        let tied: Vec<FunctionDef> = matches
            .iter()
            .take_while(|def| self.candidate_specificity_rank(def) == best_rank)
            .cloned()
            .collect();
        if tied.len() > 1
            && tied
                .iter()
                .all(|def| !self.candidate_uses_order_sensitive_dispatch(def))
            && tied
                .iter()
                .all(|def| self.candidate_dispatch_shape(def) == best_shape)
        {
            self.pending_dispatch_error =
                Some(self.ambiguous_multi_dispatch_error(name, args, &tied));
            return None;
        }

        Some(matches.remove(0))
    }

    fn candidate_specificity_rank(
        &self,
        def: &FunctionDef,
    ) -> (usize, usize, usize, usize, usize, usize) {
        let where_count = Self::dispatch_visible_params(def)
            .filter(|p| p.where_constraint.is_some())
            .count();
        let subset_type_count = Self::dispatch_visible_params(def)
            .filter(|p| {
                p.type_constraint
                    .as_deref()
                    .map(Self::constraint_base_name)
                    .map(|base| self.subsets.contains_key(base))
                    .unwrap_or(false)
            })
            .count();
        let typed_param_count = Self::dispatch_visible_params(def)
            .filter(|p| p.type_constraint.is_some())
            .count();
        let subsig_count = Self::dispatch_visible_params(def)
            .filter(|p| p.sub_signature.is_some())
            .count();
        let named_count = Self::dispatch_visible_params(def)
            .filter(|p| p.named)
            .count();
        let trait_count = Self::dispatch_visible_params(def)
            .filter(|p| {
                p.traits
                    .iter()
                    .any(|t| matches!(t.as_str(), "rw" | "raw" | "copy"))
            })
            .count();
        (
            where_count,
            subset_type_count,
            typed_param_count,
            subsig_count,
            named_count,
            trait_count,
        )
    }

    fn sort_candidates_by_specificity(&self, candidates: &mut [(String, FunctionDef)]) {
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
    ) -> Option<FunctionDef> {
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
    ) -> Option<FunctionDef> {
        if name.contains("::") {
            let multi_key = format!("{}/{}", name, arity);
            if let Some(def) = self.functions.get(&Symbol::intern(&multi_key)) {
                return Some(def.clone());
            }
            return self.functions.get(&Symbol::intern(name)).cloned();
        }
        // Try multi-dispatch with arity first
        let multi_local = format!("{}::{}/{}", self.current_package, name, arity);
        if let Some(def) = self.functions.get(&Symbol::intern(&multi_local)) {
            return Some(def.clone());
        }
        let multi_global = format!("GLOBAL::{}/{}", name, arity);
        if let Some(def) = self.functions.get(&Symbol::intern(&multi_global)) {
            return Some(def.clone());
        }
        // Fall back to regular lookup
        self.resolve_function(name)
    }

    pub(crate) fn resolve_function_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        // Arity counts only positional args, excluding named args (Pair values)
        let arity = arg_values
            .iter()
            .filter(|v| !matches!(v, Value::Pair(..)))
            .count();
        if name.contains("::") {
            let prefix = format!("{}/{arity}:", name);
            let untyped_key = format!("{}/{}", name, arity);
            let untyped_key_sym = Symbol::intern(&untyped_key);
            let untyped_m_prefix = format!("{}__m", untyped_key);
            let mut candidates: Vec<(String, FunctionDef)> = self
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
            return self.functions.get(&Symbol::intern(name)).cloned();
        }
        let prefix_local = format!("{}::{}/{}:", self.current_package, name, arity);
        let prefix_global = format!("GLOBAL::{}/{}:", name, arity);
        let generic_keys = [
            format!("{}::{}/{}", self.current_package, name, arity),
            format!("GLOBAL::{}/{}", name, arity),
        ];
        let mut found_multi_candidates = false;
        let mut candidates: Vec<(String, FunctionDef)> = self
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
            let more: Vec<(String, FunctionDef)> = self
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
            format!("{}::{}/", self.current_package, name),
            format!("GLOBAL::{}/", name),
        ];
        let mut optional_candidates: Vec<(String, FunctionDef)> = self
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
            format!("{}::{}/", self.current_package, name),
            format!("GLOBAL::{}/", name),
        ];
        let mut slurpy_candidates: Vec<(String, FunctionDef)> = self
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                slurpy_prefixes.iter().any(|prefix| ks.starts_with(prefix))
                    && def.param_defs.iter().any(|p| p.slurpy)
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
            format!("{}::{name}/", self.current_package),
            format!("GLOBAL::{name}/"),
        ];
        let mut any_arity_candidates: Vec<(String, FunctionDef)> = self
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
            format!("{}::{}/{}:", self.current_package, name, arity),
            format!("GLOBAL::{}/{}:", name, arity),
        ] {
            let candidates: Vec<FunctionDef> = self
                .functions
                .iter()
                .filter(|(key, _)| key.resolve().starts_with(&prefix_base))
                .map(|(_, def)| def.clone())
                .collect();
            for def in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    all_matches.push(def);
                }
            }
        }

        // Collect from generic (untyped) candidates
        let generic_keys = [
            format!("{}::{}/{}", self.current_package, name, arity),
            format!("GLOBAL::{}/{}", name, arity),
        ];
        for key in &generic_keys {
            let key_sym = Symbol::intern(key);
            let m_prefix = format!("{}__m", key);
            let mut candidates: Vec<(String, FunctionDef)> = self
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
                        all_matches.push(def);
                    }
                }
            }
        }

        // Collect from slurpy candidates
        let slurpy_prefixes = [
            format!("{}::{}/", self.current_package, name),
            format!("GLOBAL::{}/", name),
        ];
        let mut slurpy_candidates: Vec<(String, FunctionDef)> = self
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                slurpy_prefixes.iter().any(|prefix| ks.starts_with(prefix))
                    && def.param_defs.iter().any(|p| p.slurpy)
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
                    all_matches.push(def);
                }
            }
        }

        all_matches
    }

    pub(super) fn eval_token_call_values(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Result<Option<String>, RuntimeError> {
        let defs = match self.resolve_token_defs(name) {
            Some(defs) => defs,
            None => return Ok(None),
        };
        let subject = match self.env.get("_") {
            Some(Value::Str(s)) => Some(s.to_string()),
            _ => None,
        };
        // Collect all matching candidates with their declarative prefix match lengths
        let mut candidates: Vec<(usize, usize, String)> = Vec::new(); // (prefix_match_len, match_len, pattern)
        for def in defs {
            if let Some(pattern) = self.eval_token_def(&def, arg_values)? {
                if let Some(ref text) = subject {
                    if let Some(len) = self.regex_match_len_at_start(&pattern, text) {
                        let prefix_match_len = self
                            .declarative_prefix_match_len(&pattern, text)
                            .unwrap_or(len);
                        candidates.push((prefix_match_len, len, pattern));
                    }
                } else {
                    candidates.push((0, 0, pattern));
                }
            }
        }
        // Sort by declarative prefix match length (longest first), then by match length
        candidates.sort_by(|a, b| b.0.cmp(&a.0).then(b.1.cmp(&a.1)));
        if let Some((_, _, pattern)) = candidates.into_iter().next() {
            return Ok(Some(pattern));
        }
        if self.has_proto_token(name) {
            return Err(RuntimeError::new(format!(
                "No matching candidates for proto token: {}",
                name
            )));
        }
        Ok(None)
    }

    pub(super) fn eval_token_def(
        &mut self,
        def: &FunctionDef,
        arg_values: &[Value],
    ) -> Result<Option<String>, RuntimeError> {
        if def.empty_sig && !arg_values.is_empty() {
            return Err(Self::reject_args_for_empty_sig(arg_values));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        let rw_bindings =
            match self.bind_function_args_values(&def.param_defs, &def.params, arg_values) {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.env = saved_env;
                    self.restore_readonly_vars(saved_readonly);
                    return Err(e);
                }
            };
        self.routine_stack
            .push((def.package.resolve(), def.name.resolve()));
        let result = self.eval_block_value(&def.body);
        self.routine_stack.pop();
        let mut restored_env = saved_env;
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
        self.env = restored_env;
        self.restore_readonly_vars(saved_readonly);
        let value = match result {
            Ok(v) => v,
            Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
            Err(e) => return Err(e),
        };
        match value {
            Value::Regex(pat) => Ok(Some(pat.to_string())),
            Value::Str(s) => Ok(Some(s.to_string())),
            Value::Nil => Ok(None),
            other => Ok(Some(other.to_string_value())),
        }
    }

    pub(crate) fn has_proto(&self, name: &str) -> bool {
        if name.contains("::") {
            return self.proto_subs.contains(name);
        }
        let local = format!("{}::{}", self.current_package, name);
        if self.proto_subs.contains(&local) {
            return true;
        }
        self.proto_subs.contains(&format!("GLOBAL::{}", name))
    }

    /// Check if any multi candidates exist for this function name (any arity).
    pub(crate) fn has_multi_candidates(&self, name: &str) -> bool {
        let prefixes = [
            format!("{}::{}/", self.current_package, name),
            format!("GLOBAL::{}/", name),
        ];
        self.functions.keys().any(|k| {
            let ks = k.resolve();
            prefixes.iter().any(|p| ks.starts_with(p))
        })
    }

    pub(super) fn resolve_proto_function_with_alias(
        &self,
        name: &str,
    ) -> Option<(String, FunctionDef)> {
        if let Some(def) = self.resolve_proto_function(name) {
            return Some((name.to_string(), def));
        }
        if name.contains(':') || name.contains("::") {
            return None;
        }
        for alias in [format!("prefix:<{name}>"), format!("postfix:<{name}>")] {
            if let Some(def) = self.resolve_proto_function(&alias) {
                return Some((alias, def));
            }
        }
        None
    }

    fn resolve_proto_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.proto_functions.get(&Symbol::intern(name)).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        if let Some(def) = self.proto_functions.get(&Symbol::intern(&local)) {
            return Some(def.clone());
        }
        self.proto_functions
            .get(&Symbol::intern(&format!("GLOBAL::{}", name)))
            .cloned()
    }

    pub(super) fn call_proto_function(
        &mut self,
        proto_name: &str,
        def: &FunctionDef,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if def.empty_sig && !args.is_empty() {
            return Err(Self::reject_args_for_empty_sig(args));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        let rw_bindings = match self.bind_function_args_values(&def.param_defs, &def.params, args) {
            Ok(bindings) => bindings,
            Err(e) => {
                self.env = saved_env;
                self.restore_readonly_vars(saved_readonly);
                // Convert binding type-check errors to X::TypeCheck::Argument for proto calls
                if e.message
                    .contains("X::TypeCheck::Binding::Parameter: Type check failed")
                {
                    let type_names: Vec<String> = args
                        .iter()
                        .filter(|a| !matches!(a, Value::Pair(..)))
                        .map(|a| super::value_type_name(a).to_string())
                        .collect();
                    let mut err = RuntimeError::new(format!(
                        "X::TypeCheck::Argument: Calling {}({}) will never work with proto signature ({})",
                        proto_name,
                        type_names.join(", "),
                        def.param_defs
                            .iter()
                            .map(|p| {
                                if let Some(tc) = &p.type_constraint {
                                    format!("{} {}", tc, p.name)
                                } else {
                                    p.name.clone()
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(err.message.clone()));
                    err.exception = Some(Box::new(Value::make_instance(
                        Symbol::intern("X::TypeCheck::Argument"),
                        attrs,
                    )));
                    return Err(err);
                }
                return Err(e);
            }
        };
        self.routine_stack
            .push((def.package.resolve(), def.name.resolve()));
        self.proto_dispatch_stack
            .push((proto_name.to_string(), args.to_vec()));
        let result = if def.body.is_empty() {
            // Bodyless proto behaves as implicit {*} dispatch.
            self.call_proto_dispatch()
        } else {
            let rewritten = Self::rewrite_proto_dispatch_stmts(&def.body);
            self.eval_block_value(&rewritten)
        };
        self.proto_dispatch_stack.pop();
        self.routine_stack.pop();
        let mut restored_env = saved_env.clone();
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.restore_env_preserving_existing(&restored_env, &def.params);
        self.restore_readonly_vars(saved_readonly);
        match result {
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            other => other,
        }
    }

    pub(super) fn call_proto_dispatch(&mut self) -> Result<Value, RuntimeError> {
        let (proto_name, args) = self
            .proto_dispatch_stack
            .last()
            .cloned()
            .ok_or_else(|| RuntimeError::new("{*} used outside proto".to_string()))?;
        self.clear_pending_dispatch_error();
        let Some(def) = self.resolve_proto_candidate_with_types(&proto_name, &args) else {
            if let Some(err) = self.take_pending_dispatch_error() {
                return Err(err);
            }
            let mut err = RuntimeError::new(format!(
                "No matching candidates for proto sub: {}",
                proto_name
            ));
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Cannot resolve caller {}; none of these signatures matches",
                    proto_name
                )),
            );
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Multi::NoMatch"),
                attrs,
            )));
            return Err(err);
        };
        if def.empty_sig && !args.is_empty() {
            return Err(Self::reject_args_for_empty_sig(&args));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        let rw_bindings = match self.bind_function_args_values(&def.param_defs, &def.params, &args)
        {
            Ok(bindings) => bindings,
            Err(e) => {
                self.env = saved_env;
                self.restore_readonly_vars(saved_readonly);
                return Err(e);
            }
        };
        self.routine_stack
            .push((def.package.resolve(), def.name.resolve()));
        let result = self.run_block(&def.body);
        self.routine_stack.pop();
        let implicit_return = self.env.get("_").cloned().unwrap_or(Value::Nil);
        let mut restored_env = saved_env.clone();
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.restore_env_preserving_existing(&restored_env, &def.params);
        self.restore_readonly_vars(saved_readonly);
        match result {
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
            Ok(()) => Ok(implicit_return),
        }
    }

    fn rewrite_proto_dispatch_stmts(body: &[Stmt]) -> Vec<Stmt> {
        body.iter().map(Self::rewrite_proto_dispatch_stmt).collect()
    }

    fn resolve_proto_candidate_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        let arity = arg_values.len();
        if name.contains("::") {
            let prefix = format!("{}/{arity}:", name);
            let untyped_key = format!("{}/{}", name, arity);
            let untyped_key_sym = Symbol::intern(&untyped_key);
            let untyped_m_prefix = format!("{}__m", untyped_key);
            let mut candidates: Vec<(String, FunctionDef)> = self
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
            candidates.sort_by(|a, b| {
                let a_has_where = a.1.param_defs.iter().any(|p| p.where_constraint.is_some());
                let b_has_where = b.1.param_defs.iter().any(|p| p.where_constraint.is_some());
                b_has_where.cmp(&a_has_where).then(a.0.cmp(&b.0))
            });
            if let Some(def) = self.choose_best_matching_candidate(name, arg_values, candidates) {
                return Some(def);
            }
            return None;
        }
        self.resolve_function_with_types(name, arg_values)
    }

    fn rewrite_proto_dispatch_stmt(stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::Expr(Expr::Whatever) => Stmt::Expr(Expr::Call {
                name: Symbol::intern("__PROTO_DISPATCH__"),
                args: Vec::new(),
            }),
            Stmt::Expr(expr) => Stmt::Expr(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Return(expr) => Stmt::Return(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Take(expr) => Stmt::Take(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Die(expr) => Stmt::Die(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::Fail(expr) => Stmt::Fail(Self::rewrite_proto_dispatch_expr(expr)),
            Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint,
            } => Stmt::VarDecl {
                name: name.clone(),
                expr: Self::rewrite_proto_dispatch_expr(expr),
                type_constraint: type_constraint.clone(),
                is_state: *is_state,
                is_our: *is_our,
                is_dynamic: *is_dynamic,
                is_export: *is_export,
                export_tags: export_tags.clone(),
                custom_traits: custom_traits.clone(),
                where_constraint: where_constraint.clone(),
            },
            Stmt::Assign { name, expr, op } => Stmt::Assign {
                name: name.clone(),
                expr: Self::rewrite_proto_dispatch_expr(expr),
                op: *op,
            },
            Stmt::TempMethodAssign {
                var_name,
                method_name,
                method_args,
                value,
            } => Stmt::TempMethodAssign {
                var_name: var_name.clone(),
                method_name: method_name.clone(),
                method_args: method_args
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
                value: Self::rewrite_proto_dispatch_expr(value),
            },
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            } => Stmt::If {
                cond: Self::rewrite_proto_dispatch_expr(cond),
                then_branch: Self::rewrite_proto_dispatch_stmts(then_branch),
                else_branch: Self::rewrite_proto_dispatch_stmts(else_branch),
                binding_var: binding_var.clone(),
            },
            Stmt::While { cond, body, label } => Stmt::While {
                cond: Self::rewrite_proto_dispatch_expr(cond),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
            },
            Stmt::For {
                iterable,
                body,
                label,
                param,
                param_def,
                params,
                mode,
            } => Stmt::For {
                iterable: Self::rewrite_proto_dispatch_expr(iterable),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
                param: param.clone(),
                param_def: Box::new((**param_def).clone()),
                params: params.clone(),
                mode: *mode,
            },
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                label,
                repeat,
            } => Stmt::Loop {
                init: init
                    .as_ref()
                    .map(|s| Box::new(Self::rewrite_proto_dispatch_stmt(s))),
                cond: cond.as_ref().map(Self::rewrite_proto_dispatch_expr),
                step: step.as_ref().map(Self::rewrite_proto_dispatch_expr),
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
                repeat: *repeat,
            },
            Stmt::Block(stmts) => Stmt::Block(Self::rewrite_proto_dispatch_stmts(stmts)),
            Stmt::SyntheticBlock(stmts) => {
                Stmt::SyntheticBlock(Self::rewrite_proto_dispatch_stmts(stmts))
            }
            other => other.clone(),
        }
    }

    fn rewrite_proto_dispatch_expr(expr: &Expr) -> Expr {
        match expr {
            Expr::AnonSub { body, .. }
                if body.len() == 1 && matches!(body[0], Stmt::Expr(Expr::Whatever)) =>
            {
                Expr::Call {
                    name: Symbol::intern("__PROTO_DISPATCH__"),
                    args: Vec::new(),
                }
            }
            Expr::Unary { op, expr } => Expr::Unary {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::PostfixOp { op, expr } => Expr::PostfixOp {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                op: op.clone(),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
            },
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => Expr::Ternary {
                cond: Box::new(Self::rewrite_proto_dispatch_expr(cond)),
                then_expr: Box::new(Self::rewrite_proto_dispatch_expr(then_expr)),
                else_expr: Box::new(Self::rewrite_proto_dispatch_expr(else_expr)),
            },
            Expr::Call { name, args } => Expr::Call {
                name: *name,
                // Keep call arguments intact so closure literals like `{*}`
                // used as callbacks are not treated as proto dispatch.
                args: args.to_vec(),
            },
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } => Expr::MethodCall {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                name: *name,
                // Same rule as Expr::Call: don't rewrite callback arguments.
                args: args.to_vec(),
                modifier: *modifier,
                quoted: *quoted,
            },
            Expr::ArrayLiteral(items) => Expr::ArrayLiteral(
                items
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
            ),
            Expr::Index { target, index } => Expr::Index {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                index: Box::new(Self::rewrite_proto_dispatch_expr(index)),
            },
            Expr::IndexAssign {
                target,
                index,
                value,
            } => Expr::IndexAssign {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                index: Box::new(Self::rewrite_proto_dispatch_expr(index)),
                value: Box::new(Self::rewrite_proto_dispatch_expr(value)),
            },
            Expr::AssignExpr { name, expr } => Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::Block(stmts) => Expr::Block(Self::rewrite_proto_dispatch_stmts(stmts)),
            Expr::DoBlock { body, label } => Expr::DoBlock {
                body: Self::rewrite_proto_dispatch_stmts(body),
                label: label.clone(),
            },
            Expr::Try { body, catch } => Expr::Try {
                body: Self::rewrite_proto_dispatch_stmts(body),
                catch: catch
                    .as_ref()
                    .map(|b| Self::rewrite_proto_dispatch_stmts(b)),
            },
            Expr::Gather(body) => Expr::Gather(Self::rewrite_proto_dispatch_stmts(body)),
            Expr::HyperOp {
                op,
                left,
                right,
                dwim_left,
                dwim_right,
            } => Expr::HyperOp {
                op: op.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
                dwim_left: *dwim_left,
                dwim_right: *dwim_right,
            },
            Expr::MetaOp {
                meta,
                op,
                left,
                right,
            } => Expr::MetaOp {
                meta: meta.clone(),
                op: op.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: Box::new(Self::rewrite_proto_dispatch_expr(right)),
            },
            Expr::Reduction { op, expr } => Expr::Reduction {
                op: op.clone(),
                expr: Box::new(Self::rewrite_proto_dispatch_expr(expr)),
            },
            Expr::InfixFunc {
                name,
                left,
                right,
                modifier,
            } => Expr::InfixFunc {
                name: name.clone(),
                left: Box::new(Self::rewrite_proto_dispatch_expr(left)),
                right: right
                    .iter()
                    .map(Self::rewrite_proto_dispatch_expr)
                    .collect(),
                modifier: modifier.clone(),
            },
            Expr::CallOn { target, args } => Expr::CallOn {
                target: Box::new(Self::rewrite_proto_dispatch_expr(target)),
                args: args.to_vec(),
            },
            Expr::Lambda { param, body } => Expr::Lambda {
                param: param.clone(),
                body: Self::rewrite_proto_dispatch_stmts(body),
            },
            Expr::AnonSub { body, is_rw } => Expr::AnonSub {
                body: Self::rewrite_proto_dispatch_stmts(body),
                is_rw: *is_rw,
            },
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw,
            } => Expr::AnonSubParams {
                params: params.clone(),
                param_defs: param_defs.clone(),
                return_type: return_type.clone(),
                body: Self::rewrite_proto_dispatch_stmts(body),
                is_rw: *is_rw,
            },
            other => other.clone(),
        }
    }

    fn restore_env_preserving_existing(&mut self, saved_env: &Env, params: &[String]) {
        let current = self.env.clone();
        let mut restored = saved_env.clone();
        for key in saved_env.keys() {
            if params.iter().any(|p| p == key) || key == "@_" {
                continue;
            }
            if let Some(v) = current.get(key) {
                restored.insert(key.clone(), v.clone());
            }
        }
        self.env = restored;
    }
}
