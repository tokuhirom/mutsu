use super::*;

impl Interpreter {
    fn coercion_dispatch_value(&mut self, constraint: &str, arg: &Value) -> Option<Value> {
        let (target, source) = parse_coercion_type(constraint)?;
        if let Some(src) = source
            && !self.type_matches_value(src, arg)
        {
            return None;
        }
        self.try_coerce_value_for_constraint(constraint, arg.clone())
            .ok()
            .filter(|coerced| self.type_matches_value(target, coerced))
    }

    pub(in crate::runtime) fn args_match_param_types(
        &mut self,
        args: &[Value],
        param_defs: &[ParamDef],
    ) -> bool {
        let saved_env = self.env.clone();
        let result = (|| {
            let positional_params: Vec<&ParamDef> =
                param_defs.iter().filter(|p| !p.named).collect();
            let positional_arg_count = args
                .iter()
                .filter(|arg| {
                    !matches!(
                        unwrap_varref_value((*arg).clone()),
                        Value::Pair(..) | Value::ValuePair(..)
                    )
                })
                .count();
            let mut required_positional_count = 0usize;
            let mut positional_max_count = 0usize;
            let mut has_variadic_positional = false;
            for pd in &positional_params {
                let is_capture_param = pd.name == "_capture" || (pd.slurpy && pd.sigilless);
                let is_subsig_capture = pd.name == "__subsig__" && pd.sub_signature.is_some();
                if pd.slurpy || is_capture_param || is_subsig_capture {
                    has_variadic_positional = true;
                    continue;
                }
                positional_max_count += 1;
                if pd.default.is_none()
                    && !pd.optional_marker
                    && !pd.name.starts_with('@')
                    && !pd.name.starts_with('%')
                {
                    required_positional_count += 1;
                }
            }
            if positional_arg_count < required_positional_count {
                return false;
            }
            if !has_variadic_positional && positional_arg_count > positional_max_count {
                return false;
            }
            let mut i = 0usize;
            for pd in positional_params {
                let is_capture_param = pd.name == "_capture" || (pd.slurpy && pd.sigilless);
                let is_subsig_capture = pd.name == "__subsig__" && pd.sub_signature.is_some();
                let mut arg_for_checks: Option<Value> = if pd.slurpy || is_capture_param {
                    if is_capture_param {
                        // |c capture params preserve both positional and named parts.
                        // Keep VarRef wrappers in positional values so that
                        // sub_signature_matches_value can check `is rw` traits.
                        let mut positional = Vec::new();
                        let mut named = std::collections::HashMap::new();
                        let remaining = args.get(i..).unwrap_or(&[]);
                        for arg in remaining {
                            let unwrapped = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, val) = unwrapped {
                                named.insert(key, *val);
                            } else {
                                positional.push(arg.clone());
                            }
                        }
                        Some(Value::Capture { positional, named })
                    } else {
                        // For single-star slurpy (*@), flatten list arguments but preserve
                        // itemized Arrays ($[...] / .item) as single positional values.
                        let mut items = Vec::new();
                        let remaining = args.get(i..).unwrap_or(&[]);
                        for arg in remaining {
                            let arg = unwrap_varref_value(arg.clone());
                            if !pd.double_slurpy {
                                if let Value::Array(arr, kind) = &arg
                                    && !kind.is_itemized()
                                {
                                    items.extend(arr.iter().cloned());
                                    continue;
                                }
                                if matches!(
                                    &arg,
                                    Value::Range(..)
                                        | Value::RangeExcl(..)
                                        | Value::RangeExclStart(..)
                                        | Value::RangeExclBoth(..)
                                        | Value::GenericRange { .. }
                                        | Value::Seq(..)
                                        | Value::Slip(..)
                                ) {
                                    flatten_into_slurpy(&[arg], &mut items);
                                    continue;
                                }
                            }
                            items.push(arg);
                        }
                        Some(Value::real_array(items))
                    }
                } else if pd.name.starts_with('@') || pd.name.starts_with('%') {
                    args.get(i).cloned().map(unwrap_varref_value)
                } else if is_subsig_capture {
                    let remaining = args.get(i..).unwrap_or(&[]);
                    let unwrapped: Vec<_> =
                        remaining.iter().cloned().map(unwrap_varref_value).collect();
                    // Separate Pair args into the named map only when
                    // they represent named arguments (their keys match
                    // named subsignature params), not when they are
                    // objects being destructured.
                    let named_param_names: std::collections::HashSet<&str> = pd
                        .sub_signature
                        .as_ref()
                        .map(|sub_params| {
                            sub_params
                                .iter()
                                .filter(|sp| sp.named)
                                .map(|sp| {
                                    sp.name
                                        .strip_prefix('$')
                                        .or_else(|| sp.name.strip_prefix('@'))
                                        .or_else(|| sp.name.strip_prefix('%'))
                                        .unwrap_or(sp.name.as_str())
                                })
                                .collect()
                        })
                        .unwrap_or_default();
                    let pairs_match_named = unwrapped.iter().any(|a| {
                        if let Value::Pair(key, _) = a {
                            named_param_names.contains(key.as_str())
                        } else {
                            false
                        }
                    });
                    Some(if pairs_match_named {
                        capture_target_from_remaining_args(&unwrapped)
                    } else {
                        sub_signature_target_from_remaining_args(&unwrapped)
                    })
                } else {
                    args.get(i).cloned().map(unwrap_varref_value)
                };
                if arg_for_checks.is_none()
                    && !pd.required
                    && !pd.name.is_empty()
                    && !is_capture_param
                    && !is_subsig_capture
                {
                    let missing = Self::missing_optional_param_value(pd);
                    if let Some(constraint) = &pd.type_constraint
                        && (pd.name.starts_with('@') || pd.name.starts_with('%'))
                    {
                        let info = Self::parse_container_constraint(&pd.name, constraint);
                        self.register_container_type_metadata(&missing, info);
                    }
                    arg_for_checks = Some(missing);
                }
                // For multi-dispatch: `is rw` params require a writable variable argument.
                // Check VarRef wrapping (function calls) and pending arg sources
                // (which may be set for method calls via arg_sources_idx).
                if pd.traits.iter().any(|t| t == "rw") {
                    let raw_arg = args.get(i);
                    let is_varref = raw_arg
                        .map(|a| varref_from_value(a).is_some())
                        .unwrap_or(false);
                    let has_arg_source = self
                        .pending_call_arg_sources
                        .as_ref()
                        .and_then(|sources| sources.get(i))
                        .and_then(|name| name.as_ref())
                        .is_some();
                    if !is_varref && !has_arg_source {
                        return false;
                    }
                }
                if let Some(literal) = &pd.literal_value {
                    if let Some(arg) = arg_for_checks.as_ref() {
                        if arg != literal {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                if arg_for_checks.is_none()
                    && !pd.slurpy
                    && !is_capture_param
                    && !is_subsig_capture
                    && pd.default.is_none()
                    && !pd.optional_marker
                {
                    return false;
                }
                if let Some(constraint) = &pd.type_constraint
                    && let Some(arg) = arg_for_checks.as_ref()
                {
                    let resolved_constraint = self.resolved_type_capture_name(constraint);
                    if pd.name.starts_with('&') {
                        if !self.type_matches_value("Callable", arg) {
                            return false;
                        }
                        let Some(return_type) = self.callable_return_type(arg) else {
                            return false;
                        };
                        if !self.type_matches_value(
                            &resolved_constraint,
                            &Value::Package(Symbol::intern(&return_type)),
                        ) {
                            return false;
                        }
                    }
                    let dispatch_arg = self
                        .coercion_dispatch_value(&resolved_constraint, arg)
                        .unwrap_or_else(|| arg.clone());
                    let source_name = args
                        .get(i)
                        .and_then(varref_from_value)
                        .map(|(source_name, _)| source_name)
                        .or_else(|| {
                            self.pending_call_arg_sources
                                .as_ref()
                                .and_then(|sources| sources.get(i))
                                .and_then(|name| name.as_ref())
                                .cloned()
                        });
                    let source_constraint = source_name.as_deref().and_then(|source_name| {
                        self.var_type_constraint(source_name).or_else(|| {
                            self.var_type_constraint(
                                source_name.trim_start_matches(['$', '@', '%', '&']),
                            )
                        })
                    });
                    if let Some(captured_name) = resolved_constraint.strip_prefix("::") {
                        self.bind_type_capture(captured_name, &dispatch_arg);
                    } else if pd.name == "__type_only__" {
                        // Bare identifier param (e.g., enum value) -- resolve from env and compare
                        if let Some(expected_val) = self.env.get(&resolved_constraint).cloned() {
                            if dispatch_arg != expected_val {
                                return false;
                            }
                        } else if !self.type_matches_value(&resolved_constraint, &dispatch_arg) {
                            return false;
                        }
                    } else if pd.name.starts_with('&') {
                        // Callable return-type matching was handled above.
                    } else if pd.name.starts_with('@') || pd.name.starts_with('%') {
                        if !self.typed_container_param_matches(
                            &pd.name,
                            &resolved_constraint,
                            &dispatch_arg,
                            source_name.as_deref(),
                            source_constraint.as_deref(),
                        ) && self
                            .typed_container_param_expected(&pd.name, &resolved_constraint)
                            .is_some()
                        {
                            return false;
                        }
                    } else if resolved_constraint == "Num"
                        && matches!(
                            dispatch_arg,
                            Value::Int(_)
                                | Value::Num(_)
                                | Value::Rat(_, _)
                                | Value::FatRat(_, _)
                                | Value::BigRat(_, _)
                        )
                    {
                        // Multi-dispatch numeric widening: Int/Rat/FatRat can satisfy Num.
                    } else if is_coercion_constraint(&resolved_constraint) {
                        if self
                            .coercion_dispatch_value(&resolved_constraint, arg)
                            .is_none()
                            || !self.type_matches_value(
                                Self::constraint_base_name(&resolved_constraint),
                                &dispatch_arg,
                            )
                        {
                            return false;
                        }
                    } else if !self.type_matches_value(&resolved_constraint, &dispatch_arg) {
                        return false;
                    }
                }
                // Implicit Any constraint: untyped $ parameters reject Junction type objects
                if pd.type_constraint.is_none()
                    && !pd.name.starts_with('@')
                    && !pd.name.starts_with('%')
                    && !pd.name.starts_with('&')
                    && !pd.slurpy
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !self.type_matches_value("Any", arg)
                {
                    return false;
                }
                // Sigil-based dispatch: @ params require Positional args
                if pd.name.starts_with('@')
                    && !pd.slurpy
                    && pd.type_constraint.is_none()
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !matches!(arg, Value::Array(..) | Value::Slip(..) | Value::Nil)
                    && !self.type_matches_value("Positional", arg)
                {
                    return false;
                }
                // Sigil-based dispatch: % params require Associative args
                if pd.name.starts_with('%')
                    && !pd.slurpy
                    && pd.type_constraint.is_none()
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !matches!(arg, Value::Hash(..) | Value::Nil)
                    && !self.type_matches_value("Associative", arg)
                {
                    return false;
                }
                if pd.name.starts_with('&')
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !self.type_matches_value("Callable", arg)
                {
                    return false;
                }
                if let Some(sub_params) = &pd.sub_signature {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    if !sub_signature_matches_value(self, sub_params, arg) {
                        return false;
                    }
                }
                if let Some((sig_params, sig_ret)) = &pd.code_signature {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    if !code_signature_matches_value(self, sig_params, sig_ret, arg) {
                        return false;
                    }
                }
                if let Some(where_expr) = &pd.where_constraint {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    let saved = self.env.clone();
                    self.env.insert("_".to_string(), arg.clone());
                    // Bind the parameter name so that `where {$param ...}` can
                    // reference it during dispatch matching.
                    self.env.insert(pd.name.clone(), arg.clone());
                    let ok = match where_expr.as_ref() {
                        Expr::AnonSub { body, .. } => self
                            .eval_block_value(body)
                            .map(|v| v.truthy())
                            .unwrap_or(false),
                        Expr::MethodCall { target, .. } if matches!(target.as_ref(), Expr::Var(name) if name == "_") => {
                            self.eval_block_value(&[Stmt::Expr(where_expr.as_ref().clone())])
                                .map(|v| v.truthy())
                                .unwrap_or(false)
                        }
                        expr => self
                            .eval_block_value(&[Stmt::Expr(expr.clone())])
                            .map(|v| self.smart_match(arg, &v))
                            .unwrap_or(false),
                    };
                    self.env = saved;
                    if !ok {
                        return false;
                    }
                }
                if let Some(arg) = arg_for_checks.as_ref()
                    && !pd.name.is_empty()
                    && pd.name != "_capture"
                    && pd.name != "__subsig__"
                    && !pd.name.starts_with("__type_capture__")
                {
                    self.bind_param_value(&pd.name, arg.clone());
                }
                if is_subsig_capture {
                    i = args.len();
                } else if !pd.slurpy {
                    i += 1;
                }
            }
            let has_named_slurpy = param_defs.iter().any(|pd| {
                (pd.slurpy && (pd.name.starts_with('%') || pd.sigilless))
                    || (pd.name == "__subsig__" && pd.sub_signature.is_some())
            });
            if !has_named_slurpy {
                for arg in args {
                    let arg = unwrap_varref_value(arg.clone());
                    if let Value::Pair(key, _) = arg {
                        let consumed = param_defs.iter().any(|pd| {
                            if pd.named {
                                let bare = pd.name.trim_start_matches(|c| "$@%&".contains(c));
                                bare == key
                            } else {
                                pd.name == format!(":{}", key)
                            }
                        });
                        if !consumed {
                            return false;
                        }
                    }
                }
            }
            // Build map of named args for checking required params and where constraints
            let named_args: Vec<(String, Value)> = args
                .iter()
                .filter_map(|a| {
                    let a = unwrap_varref_value(a.clone());
                    if let Value::Pair(key, val) = a {
                        Some((key, *val))
                    } else {
                        None
                    }
                })
                .collect();
            for pd in param_defs.iter().filter(|pd| pd.named) {
                let bare_name = pd.name.trim_start_matches(|c: char| "$@%&".contains(c));
                let arg_val = named_args
                    .iter()
                    .find(|(k, _)| k == bare_name)
                    .map(|(_, v)| v.clone());

                // Check required named params have corresponding args
                if pd.required && pd.default.is_none() && arg_val.is_none() {
                    return false;
                }

                // Check type constraint on named param
                if let Some(constraint) = &pd.type_constraint
                    && let Some(ref val) = arg_val
                    && !self.type_matches_value(constraint, val)
                {
                    return false;
                }

                // Check where constraint on named param
                if let Some(where_expr) = &pd.where_constraint
                    && let Some(ref val) = arg_val
                {
                    let saved = self.env.clone();
                    self.env.insert("_".to_string(), val.clone());
                    let ok = match where_expr.as_ref() {
                        Expr::AnonSub { body, .. } => self
                            .eval_block_value(body)
                            .map(|v| v.truthy())
                            .unwrap_or(false),
                        expr => self
                            .eval_block_value(&[Stmt::Expr(expr.clone())])
                            .map(|v| self.smart_match(val, &v))
                            .unwrap_or(false),
                    };
                    self.env = saved;
                    if !ok {
                        return false;
                    }
                }
            }
            true
        })();
        self.env = saved_env;
        result
    }

    pub(in crate::runtime) fn method_args_match(
        &mut self,
        args: &[Value],
        param_defs: &[ParamDef],
    ) -> bool {
        let is_invocant_param =
            |p: &ParamDef| p.is_invocant || p.traits.iter().any(|t| t == "invocant");
        let all_invocant_only = param_defs.iter().all(is_invocant_param);
        if all_invocant_only {
            // Hot path for methods like `method m { ... }` where only the implicit invocant
            // is present in signature metadata. Reject any explicit user arguments quickly.
            for arg in args {
                match arg {
                    Value::Pair(key, _) if key == TEST_CALLSITE_LINE_KEY => {}
                    Value::ValuePair(key, _) => {
                        if let Value::Str(name) = key.as_ref() {
                            if name.as_str() != TEST_CALLSITE_LINE_KEY {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                    _ => return false,
                }
            }
            return self.args_match_param_types(args, &[]);
        }

        let filtered_storage;
        let filtered_params: &[ParamDef] = if param_defs.iter().any(is_invocant_param) {
            filtered_storage = param_defs
                .iter()
                .filter(|p| !is_invocant_param(p))
                .cloned()
                .collect::<Vec<_>>();
            filtered_storage.as_slice()
        } else {
            // Common path: method signatures without explicit invocant metadata.
            param_defs
        };
        let positional_params: Vec<&ParamDef> =
            filtered_params.iter().filter(|p| !p.named).collect();
        let positional_arg_count = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(..)))
            .count();
        let mut required = 0usize;
        let mut has_positional_slurpy = false;
        let mut has_hash_slurpy = false;
        for pd in &positional_params {
            if pd.slurpy {
                if pd.sigilless {
                    // Capture parameter (|c) absorbs both positional and named args
                    has_positional_slurpy = true;
                    has_hash_slurpy = true;
                } else if pd.name.starts_with('%') {
                    has_hash_slurpy = true;
                } else {
                    has_positional_slurpy = true;
                }
            } else if pd.default.is_none() && !pd.optional_marker {
                required += 1;
            }
        }
        let max_positional = positional_params.iter().filter(|p| !p.slurpy).count();
        if has_positional_slurpy {
            if positional_arg_count < required {
                return false;
            }
        } else if positional_arg_count < required || positional_arg_count > max_positional {
            return false;
        }
        if !has_hash_slurpy {
            let named_params: std::collections::HashSet<&str> = filtered_params
                .iter()
                .filter(|p| p.named)
                .map(|p| {
                    // Strip sigil for named params with array/hash sigils: :@l -> "l", :%h -> "h"
                    p.name
                        .strip_prefix('@')
                        .or_else(|| p.name.strip_prefix('%'))
                        .unwrap_or(p.name.as_str())
                })
                .collect();
            for arg in args {
                if let Value::Pair(key, _) = arg
                    && key != TEST_CALLSITE_LINE_KEY
                    && !named_params.contains(key.as_str())
                {
                    return false;
                }
                if let Value::ValuePair(key, _) = arg
                    && let Value::Str(name) = key.as_ref()
                    && name.as_str() != TEST_CALLSITE_LINE_KEY
                    && !named_params.contains(name.as_str())
                {
                    return false;
                }
            }
        }
        if !self.args_match_param_types(args, filtered_params) {
            return false;
        }
        true
    }

    /// Create an error for calling a sub with empty signature `()` with arguments.
    pub(crate) fn reject_args_for_empty_sig(args: &[Value]) -> RuntimeError {
        if let Some(k) = args.iter().find_map(|a| match a {
            Value::Pair(key, _) if key != TEST_CALLSITE_LINE_KEY => Some(key.clone()),
            // ValuePair is a positional pair (parenthesized), not a named arg
            _ => None,
        }) {
            return RuntimeError::new(format!("Unexpected named argument '{}' passed", k));
        }
        RuntimeError::new(
            "Too many positionals passed; expected 0 arguments but got more".to_string(),
        )
    }
}
