use super::*;

impl Interpreter {
    pub(crate) fn call_proto_dispatch(&mut self) -> Result<Value, RuntimeError> {
        let (proto_name, args, method_ctx) = self
            .proto_dispatch_stack
            .last()
            .cloned()
            .ok_or_else(|| RuntimeError::new("{*} used outside proto".to_string()))?;
        // `proto method` body: `{*}` redispatches to the matching multi *method*
        // candidate on the invocant. The one-shot `proto_method_skip` flag makes
        // the re-entry bypass proto interception so it reaches the real candidate.
        if let Some(ctx) = method_ctx {
            // `{*}` rw-redispatch for a proto *method* (ledger §D): like the proto
            // *sub* path (`vm_call_proto_dispatch`), Rakudo redispatches with the
            // proto's CURRENT (body-mutated) parameter, and a candidate's `is rw`
            // param needs a writable argument. The interpreter binds method params
            // by name into env, so the current value lives in env (`code = None`).
            // Rebuild the rw args + arg_sources so the candidate matches and its
            // writeback chains back through the proto method param to the caller.
            let invocant_class = match &ctx.invocant {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                _ => None,
            };
            let (args, rw_sources) = match invocant_class
                .and_then(|cn| self.lookup_proto_method(&cn, &proto_name))
                .and_then(|(_, proto)| {
                    self.proto_rw_redispatch_args(&proto.param_defs, &args, None)
                }) {
                Some((rebuilt, sources)) => (rebuilt, Some(sources)),
                None => (args, None),
            };
            self.proto_method_skip = Some(proto_name.clone());
            if rw_sources.is_some() {
                self.set_pending_call_arg_sources(rw_sources);
            }
            return self.call_method_with_values(ctx.invocant, &proto_name, args);
        }
        self.clear_pending_dispatch_error();
        let Some(def) = self.resolve_proto_candidate_with_types(&proto_name, &args) else {
            if let Some(err) = self.take_pending_dispatch_error() {
                return Err(err);
            }
            // Build call profile: name(Type1, Type2, ...)
            let arg_types: Vec<String> = args
                .iter()
                .filter(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
                .map(|a| {
                    let tn = super::value_type_name(a);
                    if !matches!(a, Value::Nil) {
                        format!("{}:D", tn)
                    } else {
                        tn.to_string()
                    }
                })
                .collect();
            let call_profile = format!("{}({})", proto_name, arg_types.join(", "));

            // Collect candidate signatures from all multi candidates
            let sig_lines = self.collect_multi_candidate_signatures(&proto_name, args.len());

            let sig_list = if sig_lines.is_empty() {
                String::new()
            } else {
                format!(":\n{}", sig_lines.join("\n"))
            };

            let message = format!(
                "Cannot resolve caller {}; none of these signatures matches{}",
                call_profile, sig_list
            );
            let mut err = RuntimeError::new(format!(
                "No matching candidates for proto sub: {}",
                proto_name
            ));
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(message));
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
        // Set up multi dispatch stack so nextsame/nextwith can walk through
        // remaining candidates when called inside a proto-dispatched multi sub.
        // Get candidates in dispatch order (same sorting as resolve_function_with_types)
        // and take all candidates after the first (current) match.
        let remaining = self.resolve_remaining_proto_candidates(&proto_name, &args, &def);
        let pushed_dispatch = !remaining.is_empty();
        if pushed_dispatch {
            let rw_params =
                super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs);
            self.multi_dispatch_stack.push((
                proto_name.clone(),
                remaining,
                args.clone(),
                rw_params,
            ));
        }
        self.samewith_context_stack.push((proto_name.clone(), None));
        self.routine_stack.push(RoutineFrame {
            package: def.package.resolve(),
            name: def.name.resolve(),
            line: None,
            file: None,
            is_method: false,
            is_block: false,
        });
        let result = self.run_block(&def.body);
        self.routine_stack.pop();
        self.samewith_context_stack.pop();
        if pushed_dispatch {
            self.multi_dispatch_stack.pop();
        }
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

    /// Collect remaining multi candidates after the current one, in dispatch order.
    /// Uses the same candidate sorting as resolve_function_with_types to ensure
    /// nextsame/nextwith walk candidates in the correct order.
    fn resolve_remaining_proto_candidates(
        &mut self,
        name: &str,
        args: &[Value],
        current_def: &FunctionDef,
    ) -> Vec<Arc<FunctionDef>> {
        let arity = args
            .iter()
            .filter(|v| !matches!(v, Value::Pair(..)))
            .count();
        let prefix_local = format!("{}::{}/{}:", self.current_package(), name, arity);
        let prefix_global = format!("GLOBAL::{}/{}:", name, arity);
        let generic_keys = [
            format!("{}::{}/{}", self.current_package(), name, arity),
            format!("GLOBAL::{}/{}", name, arity),
        ];
        // Collect all candidates (typed + generic) like resolve_function_with_types
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
            candidates.extend(more);
        }
        // Sort by specificity (same as resolve_function_with_types)
        self.sort_candidates_by_specificity(&mut candidates);
        // Walk sorted candidates: find all matching ones, skip duplicates,
        // and return everything after the current candidate.
        let current_fp = crate::ast::function_body_fingerprint(
            &current_def.params,
            &current_def.param_defs,
            &current_def.body,
        );
        let mut seen_fps = std::collections::HashSet::new();
        let mut found_current = false;
        let mut remaining = Vec::new();
        for (_key, cand) in &candidates {
            let fp =
                crate::ast::function_body_fingerprint(&cand.params, &cand.param_defs, &cand.body);
            if !seen_fps.insert(fp) {
                continue; // duplicate
            }
            if !self.args_match_param_types(args, &cand.param_defs) {
                continue; // doesn't match
            }
            if !found_current {
                if fp == current_fp {
                    found_current = true;
                }
                continue;
            }
            remaining.push(cand.clone());
        }
        remaining
    }

    pub(crate) fn rewrite_proto_dispatch_stmts(body: &[Stmt]) -> Vec<Stmt> {
        body.iter().map(Self::rewrite_proto_dispatch_stmt).collect()
    }

    pub(crate) fn resolve_proto_candidate_with_types(
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
            candidates.sort_by(|a, b| {
                let a_has_where = a.1.param_defs.iter().any(|p| p.where_constraint.is_some());
                let b_has_where = b.1.param_defs.iter().any(|p| p.where_constraint.is_some());
                b_has_where.cmp(&a_has_where).then(a.0.cmp(&b.0))
            });
            if let Some(def) = self.choose_best_matching_candidate(name, arg_values, candidates) {
                return Some((*def).clone());
            }
            return None;
        }
        self.resolve_function_with_types(name, arg_values)
            .map(|a| (*a).clone())
    }

    /// Collect formatted signature lines from multi dispatch candidates.
    pub(super) fn collect_multi_candidate_signatures(
        &self,
        name: &str,
        _arity: usize,
    ) -> Vec<String> {
        let mut sig_lines = Vec::new();
        // Multi candidates are stored with keys like:
        //   GLOBAL::name/arity:Type
        //   Package::name/arity:Type
        //   GLOBAL::name/arity__mN
        // We need to match all of them.
        let local_prefix = format!("{}::{}/", self.current_package(), name);
        let global_prefix = format!("GLOBAL::{}/", name);
        let bare_prefix = format!("{}/", name);
        let mut seen_sigs = std::collections::HashSet::new();
        for (key, def) in &self.registry().functions {
            let ks = key.resolve();
            if !ks.starts_with(&local_prefix)
                && !ks.starts_with(&global_prefix)
                && !ks.starts_with(&bare_prefix)
            {
                continue;
            }
            let sig_parts: Vec<String> = def
                .param_defs
                .iter()
                .filter(|pd| !pd.traits.iter().any(|t| t == "invocant"))
                .map(|pd| {
                    if pd.name == "__type_only__" {
                        pd.type_constraint.as_deref().unwrap_or("Any").to_string()
                    } else {
                        let sigil = if pd.name.starts_with('@')
                            || pd.name.starts_with('%')
                            || pd.name.starts_with('&')
                        {
                            ""
                        } else if pd.sigilless {
                            "\\"
                        } else {
                            "$"
                        };
                        let name_part = format!("{}{}", sigil, pd.name);
                        if let Some(tc) = &pd.type_constraint {
                            format!("{} {}", tc, name_part)
                        } else {
                            name_part
                        }
                    }
                })
                .collect();
            let sig_str = format!("    ({})", sig_parts.join(", "));
            if seen_sigs.insert(sig_str.clone()) {
                sig_lines.push(sig_str);
            }
        }
        sig_lines
    }
}
