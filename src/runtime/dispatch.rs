use super::*;

type DispatchShape = (Option<String>, bool, bool, bool, bool, bool, bool);

impl Interpreter {
    pub(crate) fn constraint_base_name(constraint: &str) -> &str {
        let mut end = constraint.len();
        for (idx, ch) in constraint.char_indices() {
            if ch == '[' || ch == '(' || ch == ':' {
                end = idx;
                break;
            }
        }
        &constraint[..end]
    }

    pub(super) fn clear_pending_dispatch_error(&mut self) {
        self.pending_dispatch_error = None;
    }

    pub(crate) fn take_pending_dispatch_error(&mut self) -> Option<RuntimeError> {
        self.pending_dispatch_error.take()
    }

    pub(crate) fn set_pending_dispatch_error(&mut self, err: RuntimeError) {
        self.pending_dispatch_error = Some(err);
    }

    pub(super) fn dispatch_visible_params(def: &FunctionDef) -> Vec<&ParamDef> {
        Self::dispatch_visible_params_from(&def.param_defs)
    }

    /// Compute the parameters that participate in multi dispatch from a slice
    /// of parameter definitions.
    ///
    /// When a candidate's signature is a single capture parameter carrying a
    /// subsignature (e.g. `multi sub foo(|c(Int $x))`), dispatch must rank and
    /// tie-break on the subsignature's parameters, exactly as if they had been
    /// written as a normal positional/named signature (`multi sub foo(Int $x)`).
    /// Without flattening, every such candidate would look like a single
    /// sigilless slurpy capture and all candidates would appear tied.
    fn dispatch_visible_params_from(param_defs: &[ParamDef]) -> Vec<&ParamDef> {
        let visible: Vec<&ParamDef> = param_defs.iter().filter(|p| p.multi_invocant).collect();
        // Detect the "single capture with a subsignature" shape and descend
        // into the subsignature for dispatch purposes.
        if visible.len() == 1 {
            let p = visible[0];
            // A named capture `|c(...)` is a sigilless slurpy param; an anonymous
            // capture `|(...)` is recorded as a `__subsig__` param.  Both carry
            // the real dispatch parameters in their subsignature.
            let is_capture_subsig = (p.slurpy && p.sigilless) || p.name == "__subsig__";
            if is_capture_subsig
                && p.type_constraint.is_none()
                && p.literal_value.is_none()
                && let Some(sub) = &p.sub_signature
            {
                return Self::dispatch_visible_params_from(sub);
            }
        }
        visible
    }

    pub(super) fn candidate_uses_order_sensitive_dispatch(&self, def: &FunctionDef) -> bool {
        Self::dispatch_visible_params(def).into_iter().any(|p| {
            p.literal_value.is_some()
                || p.where_constraint.is_some()
                || p.type_constraint
                    .as_deref()
                    .map(Self::constraint_base_name)
                    .map(|base| self.registry().subsets.contains_key(base))
                    .unwrap_or(false)
        })
    }

    pub(super) fn candidate_dispatch_shape(&self, def: &FunctionDef) -> Vec<DispatchShape> {
        Self::dispatch_visible_params(def)
            .into_iter()
            .map(|p| {
                // Include sigil-based implicit type constraint in the shape
                // so that @-param and $-param are distinguishable.
                let effective_type = if p.type_constraint.is_some() {
                    p.type_constraint.clone()
                } else if !p.slurpy && p.name.starts_with('@') {
                    Some("__sigil_@__".to_string())
                } else if !p.slurpy && p.name.starts_with('%') {
                    Some("__sigil_%__".to_string())
                } else if p.name.starts_with('&') {
                    Some("__sigil_&__".to_string())
                } else {
                    None
                };
                (
                    effective_type,
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
        self.routine_stack.push(RoutineFrame {
            package: def.package.resolve(),
            name: def.name.resolve(),
            line: None,
            file: None,
            is_method: false,
            is_block: false,
        });
        let result = self.eval_block_value(&def.body);
        self.routine_stack.pop();
        // Apply <sym> instantiation for proto token :sym<> variants
        let instantiate_sym = |pat: &str| -> String { Self::instantiate_token_pattern(def, pat) };
        let rendered = match result {
            Ok(Value::Regex(pat)) => {
                let pat = instantiate_sym(&pat);
                self.instantiate_named_regex_arg_calls(&self.interpolate_bound_regex_scalars(&pat))
                    .map(Some)
            }
            Ok(Value::Str(s)) => {
                let s = instantiate_sym(&s);
                self.instantiate_named_regex_arg_calls(&self.interpolate_bound_regex_scalars(&s))
                    .map(Some)
            }
            Ok(Value::Nil) => Ok(None),
            Ok(other) => Ok(Some(other.to_string_value())),
            Err(e) if e.return_value.is_some() => match e.return_value.unwrap() {
                Value::Regex(pat) => {
                    let pat = instantiate_sym(&pat);
                    self.instantiate_named_regex_arg_calls(
                        &self.interpolate_bound_regex_scalars(&pat),
                    )
                    .map(Some)
                }
                Value::Str(s) => {
                    let s = instantiate_sym(&s);
                    self.instantiate_named_regex_arg_calls(
                        &self.interpolate_bound_regex_scalars(&s),
                    )
                    .map(Some)
                }
                Value::Nil => Ok(None),
                other => Ok(Some(other.to_string_value())),
            },
            Err(e) => Err(e),
        };
        let mut restored_env = saved_env;
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
        self.env = restored_env;
        self.restore_readonly_vars(saved_readonly);
        rendered
    }
}
