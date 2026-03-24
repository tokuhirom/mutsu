use super::*;
use crate::symbol::Symbol;
use crate::value::signature::{extract_sig_info, make_signature_value, param_defs_to_sig_info};

impl Interpreter {
    /// Dispatch methods on callable values (Routine, Sub, WeakSub).
    /// Returns Some(result) if handled, None to fall through to common dispatch.
    pub(super) fn dispatch_callable_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match target {
            Value::Routine { name, package, .. } => self.dispatch_routine_method(
                target,
                &name.resolve(),
                &package.resolve(),
                method,
                args.to_vec(),
            ),
            Value::Sub(data) => {
                let data = data.clone();
                self.dispatch_sub_method(target, &data, method, args.to_vec())
            }
            Value::WeakSub(weak) => weak.upgrade().map(|strong| {
                self.call_method_with_values(Value::Sub(strong), method, args.to_vec())
            }),
            _ => None,
        }
    }

    /// Dispatch methods on Value::Routine (multi-dispatch handles).
    /// Returns Some(result) if handled, None to fall through.
    pub(super) fn dispatch_routine_method(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if method == "assuming" {
            // Create a wrapper Sub that delegates to the multi-dispatch routine
            let mut sub_data = crate::value::SubData {
                package: Symbol::intern(package),
                name: Symbol::intern(name),
                params: Vec::new(),
                param_defs: Vec::new(),
                body: vec![],
                is_rw: false,
                is_raw: false,
                env: self.env().clone(),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                is_bare_block: false,
                compiled_code: None,
                deprecated_message: None,
            };
            // Store the routine name so call_sub_value can dispatch
            sub_data.env.insert(
                "__mutsu_routine_name".to_string(),
                Value::str(name.to_string()),
            );
            // Apply assumed args
            for arg in &args {
                if let Value::Pair(key, boxed) = arg {
                    if key == "__mutsu_test_callsite_line" {
                        continue;
                    }
                    sub_data.assumed_named.insert(key.clone(), *boxed.clone());
                } else {
                    sub_data.assumed_positional.push(arg.clone());
                }
            }
            return Some(Ok(Value::Sub(std::sync::Arc::new(sub_data))));
        }
        if method == "candidates" && args.is_empty() {
            return Some(Ok(Value::array(self.routine_candidate_subs(package, name))));
        }
        if method == "cando" && args.len() == 1 {
            let call_args = Self::capture_to_call_args(&args[0]);
            let matching = self
                .resolve_all_matching_candidates(name, &call_args)
                .into_iter()
                .map(|def| {
                    Value::make_sub(
                        def.package,
                        def.name,
                        def.params,
                        def.param_defs,
                        def.body,
                        def.is_rw,
                        self.env.clone(),
                    )
                })
                .collect();
            return Some(Ok(Value::array(matching)));
        }
        if method == "signature" && args.is_empty() {
            let candidates = self.routine_candidate_subs(package, name);
            if candidates.is_empty() {
                let (params, param_defs) = self.callable_signature(target);
                let defs = if !param_defs.is_empty() {
                    param_defs
                } else {
                    params
                        .into_iter()
                        .map(|name| ParamDef {
                            name,
                            default: None,
                            multi_invocant: true,
                            required: false,
                            named: false,
                            slurpy: false,
                            double_slurpy: false,
                            sigilless: false,
                            type_constraint: None,
                            literal_value: None,
                            sub_signature: None,
                            where_constraint: None,
                            traits: Vec::new(),
                            optional_marker: false,
                            outer_sub_signature: None,
                            code_signature: None,
                            is_invocant: false,
                            shape_constraints: None,
                        })
                        .collect()
                };
                let info = param_defs_to_sig_info(&defs, None);
                return Some(Ok(make_signature_value(info)));
            }
            if candidates.len() == 1 {
                return Some(self.call_method_with_values(
                    candidates[0].clone(),
                    "signature",
                    Vec::new(),
                ));
            }
            let mut signatures = Vec::new();
            for candidate in candidates {
                match self.call_method_with_values(candidate, "signature", Vec::new()) {
                    Ok(sig) => signatures.push(sig),
                    Err(e) => return Some(Err(e)),
                }
            }
            return Some(Ok(Value::Junction {
                kind: JunctionKind::Any,
                values: std::sync::Arc::new(signatures),
            }));
        }
        if matches!(method, "raku" | "perl" | "gist" | "Str") && args.is_empty() {
            // For Routine handles, render using the first candidate's signature
            let candidates = self.routine_candidate_subs(package, name);
            let sig_gist = if !candidates.is_empty() {
                if let Ok(Value::Instance { attributes, .. }) =
                    self.call_method_with_values(candidates[0].clone(), "signature", Vec::new())
                {
                    attributes
                        .get("gist")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "()".to_string())
                } else {
                    "()".to_string()
                }
            } else {
                let (params, param_defs) = self.callable_signature(target);
                let defs = if !param_defs.is_empty() {
                    param_defs
                } else {
                    params
                        .into_iter()
                        .map(|name| ParamDef {
                            name,
                            default: None,
                            multi_invocant: true,
                            required: false,
                            named: false,
                            slurpy: false,
                            double_slurpy: false,
                            sigilless: false,
                            type_constraint: None,
                            literal_value: None,
                            sub_signature: None,
                            where_constraint: None,
                            traits: Vec::new(),
                            optional_marker: false,
                            outer_sub_signature: None,
                            code_signature: None,
                            is_invocant: false,
                            shape_constraints: None,
                        })
                        .collect()
                };
                let info = param_defs_to_sig_info(&defs, None);
                let sig = make_signature_value(info);
                if let Value::Instance { attributes, .. } = &sig {
                    attributes
                        .get("gist")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "()".to_string())
                } else {
                    "()".to_string()
                }
            };
            return Some(Ok(Value::str(format!(
                "sub {} {} {{ #`(Sub|0) ... }}",
                name, sig_gist
            ))));
        }
        if method == "can" {
            let method_name = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let can = matches!(
                method_name.as_str(),
                "assuming"
                    | "signature"
                    | "arity"
                    | "count"
                    | "candidates"
                    | "cando"
                    | "of"
                    | "name"
                    | "raku"
                    | "perl"
                    | "Str"
                    | "gist"
                    | "can"
            );
            return Some(Ok(Value::Bool(can)));
        }
        if matches!(method, "arity" | "count") && args.is_empty() {
            let candidates = self.routine_candidate_subs(package, name);
            if !candidates.is_empty() {
                let mut infos = Vec::new();
                for candidate in candidates {
                    if let Value::Sub(data) = candidate {
                        let sig = self.sub_signature_value(&data);
                        if let Some(info) = extract_sig_info(&sig) {
                            infos.push(info);
                        }
                    }
                }
                if infos.is_empty() {
                    return Some(Ok(Value::Int(0)));
                }
                return Some(Ok(if method == "arity" {
                    Self::candidate_arity_value(&infos)
                } else {
                    Self::candidate_count_value(&infos)
                }));
            }

            let (params, param_defs) = self.callable_signature(target);
            let defs = if !param_defs.is_empty() {
                param_defs
            } else {
                params
                    .into_iter()
                    .map(|name| ParamDef {
                        name,
                        default: None,
                        multi_invocant: true,
                        required: true,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                    })
                    .collect()
            };
            let info = param_defs_to_sig_info(&defs, None);
            return Some(Ok(if method == "arity" {
                Value::Int(Self::signature_required_positional_count(&info))
            } else {
                Self::signature_count_value(&info)
            }));
        }
        None
    }

    /// Dispatch methods on Value::Sub.
    /// Returns Some(result) if handled, None to fall through.
    pub(super) fn dispatch_sub_method(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if method == "nextwith" {
            // Tail-style dispatch: call target with caller frame and return from current frame.
            let saved_env = self.env.clone();
            if let Some(parent_env) = self.caller_env_stack.last().cloned() {
                self.env = parent_env;
            }
            let call_result = self.call_sub_value(target.clone(), args, false);
            self.env = saved_env;
            let value = match call_result {
                Ok(v) => v,
                Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                Err(e) => return Some(Err(e)),
            };
            return Some(Err(RuntimeError {
                return_value: Some(value),
                ..RuntimeError::new("")
            }));
        }
        if method == "assuming" {
            let mut next = (*data).clone();
            let make_failure =
                |sub_data: &crate::value::SubData, expected: Value, symbol: String| {
                    let mut ex_attrs = std::collections::HashMap::new();
                    ex_attrs.insert("expected".to_string(), expected.clone());
                    ex_attrs.insert("symbol".to_string(), Value::str(symbol));
                    ex_attrs.insert(
                        "payload".to_string(),
                        Value::str(expected.to_string_value()),
                    );
                    let exception =
                        Value::make_instance(Symbol::intern("X::TypeCheck::Binding"), ex_attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), exception);
                    failure_attrs.insert("handled".to_string(), Value::Bool(false));
                    let failure = Value::make_instance(Symbol::intern("Failure"), failure_attrs);
                    let mut mixins = std::collections::HashMap::new();
                    mixins.insert("Failure".to_string(), failure);
                    Value::mixin(Value::Sub(std::sync::Arc::new(sub_data.clone())), mixins)
                };
            let mut incoming_named = std::collections::HashMap::new();
            for arg in args {
                if let Value::Pair(key, boxed) = arg {
                    if key == "__mutsu_test_callsite_line" {
                        continue;
                    }
                    incoming_named.insert(key.clone(), *boxed.clone());
                    next.assumed_named.insert(key, *boxed);
                } else {
                    next.assumed_positional.push(arg);
                }
            }
            if next.param_defs.is_empty() && !incoming_named.is_empty() {
                return Some(Ok(make_failure(
                    &next,
                    Value::str_from("Unexpected"),
                    String::new(),
                )));
            }
            let mut known_named: std::collections::HashSet<String> =
                std::collections::HashSet::new();
            Self::collect_named_param_keys(&next.param_defs, &mut known_named);
            let allows_extra_named = Self::has_named_slurpy_param(&next.param_defs);
            for name in incoming_named.keys() {
                if !allows_extra_named && !known_named.contains(name) {
                    return Some(Ok(make_failure(
                        &next,
                        Value::str_from("Unexpected"),
                        String::new(),
                    )));
                }
            }
            for pd in next.param_defs.iter().filter(|pd| pd.named) {
                if let Some(value) = incoming_named.get(&pd.name)
                    && let Some(constraint) = &pd.type_constraint
                    && !self.type_matches_value(constraint, value)
                {
                    return Some(Ok(make_failure(
                        &next,
                        Value::Package(Symbol::intern("X::TypeCheck::Assignment")),
                        format!("${}", pd.name),
                    )));
                }
            }
            return Some(Ok(Value::Sub(std::sync::Arc::new(next))));
        }
        if method == "candidates" && args.is_empty() {
            return Some(Ok(Value::array(vec![target.clone()])));
        }
        if method == "cando" && args.len() == 1 {
            let call_args = Self::capture_to_call_args(&args[0]);
            let matches = if self.candidate_matches_call_args(target, &call_args) {
                vec![target.clone()]
            } else {
                Vec::new()
            };
            return Some(Ok(Value::array(matches)));
        }
        if method == "signature" && args.is_empty() {
            return Some(Ok(self.sub_signature_value(data)));
        }
        if matches!(method, "raku" | "perl" | "gist" | "Str") && args.is_empty() {
            let sig = self.sub_signature_value(data);
            let sig_gist = if let Value::Instance { attributes, .. } = &sig {
                attributes
                    .get("gist")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "()".to_string())
            } else {
                "()".to_string()
            };
            let name = data.name.resolve();
            let id = data.id;
            if method == "gist" || method == "Str" {
                if name.is_empty() || name == "<anon>" {
                    return Some(Ok(Value::str(format!(
                        "-> {} {{ #`(Block|{}) ... }}",
                        sig_gist, id
                    ))));
                }
                return Some(Ok(Value::str(format!(
                    "sub {} {} {{ #`(Sub|{}) ... }}",
                    name, sig_gist, id
                ))));
            }
            // .raku / .perl
            if name.is_empty() || name == "<anon>" {
                return Some(Ok(Value::str(format!(
                    "-> {} {{ #`(Block|{}) ... }}",
                    sig_gist, id
                ))));
            }
            return Some(Ok(Value::str(format!(
                "sub {} {} {{ #`(Sub|{}) ... }}",
                name, sig_gist, id
            ))));
        }
        if method == "of" && args.is_empty() {
            let type_name = self
                .callable_return_type(target)
                .unwrap_or_else(|| "Mu".to_string());
            return Some(Ok(Value::Package(Symbol::intern(&type_name))));
        }
        if matches!(method, "arity" | "count") && args.is_empty() {
            let sig = self.sub_signature_value(data);
            if let Some(info) = extract_sig_info(&sig) {
                return Some(Ok(if method == "arity" {
                    Value::Int(Self::signature_required_positional_count(&info))
                } else {
                    Self::signature_count_value(&info)
                }));
            }
            return Some(Ok(Value::Int(0)));
        }
        if method == "wrap" {
            // .wrap(&wrapper) — add a wrapper to this sub's wrap chain
            let wrapper = if let Some(w) = args.first() {
                w.clone()
            } else {
                return Some(Err(RuntimeError::new("wrap requires a wrapper argument")));
            };
            self.wrap_handle_counter += 1;
            let handle_id = self.wrap_handle_counter;
            // Look up original sub_id by name if already wrapped, since &foo creates fresh Sub.
            // However, if the sub has been redefined (e.g. a new `sub foo` in a different
            // block), the old wrap chain should be cleared.  We detect redefinition by
            // checking the __mutsu_callable_id for this function name in the env: if it
            // differs from what was stored at first-wrap time, the sub was redefined.
            let func_name = data.name.resolve();
            let current_callable_id = if !func_name.is_empty() {
                let key = format!(
                    "__mutsu_callable_id::{}::{}",
                    self.current_package, func_name
                );
                self.env
                    .get(&key)
                    .and_then(|v| {
                        if let Value::Int(n) = v {
                            Some(*n)
                        } else {
                            None
                        }
                    })
                    .or_else(|| {
                        let key = format!("__mutsu_callable_id::GLOBAL::{}", func_name);
                        self.env.get(&key).and_then(|v| {
                            if let Value::Int(n) = v {
                                Some(*n)
                            } else {
                                None
                            }
                        })
                    })
            } else {
                None
            };
            let sub_id = if !func_name.is_empty() {
                if let Some((&old_id, _)) =
                    self.wrap_sub_names.iter().find(|(_, n)| **n == func_name)
                {
                    // Check if the callable_id matches what was stored at wrap time.
                    // If different, the sub was redefined.
                    let stored_callable_id =
                        self.wrap_callable_ids.get(&func_name).copied().flatten();
                    let same_sub = match (stored_callable_id, current_callable_id) {
                        (Some(stored), Some(current)) => stored == current,
                        _ => true, // If we can't tell, assume same
                    };
                    if same_sub {
                        old_id
                    } else {
                        // Sub was redefined — clear old wrap chain and mappings
                        self.wrap_chains.remove(&old_id);
                        self.wrap_sub_names.remove(&old_id);
                        self.wrap_name_to_sub.remove(&func_name);
                        data.id
                    }
                } else {
                    data.id
                }
            } else {
                data.id
            };
            // Store the callable_id for this wrap chain
            if !func_name.is_empty() {
                self.wrap_callable_ids
                    .insert(func_name.clone(), current_callable_id);
            }
            self.wrap_chains
                .entry(sub_id)
                .or_default()
                .push((handle_id, wrapper));
            // Store mapping from sub_id to function name for named call dispatch
            if !func_name.is_empty() {
                self.wrap_sub_names.insert(sub_id, func_name.clone());
                // Only store the first Sub value for this name (preserves original sub_id)
                self.wrap_name_to_sub
                    .entry(func_name)
                    .or_insert_with(|| target.clone());
            }
            // Return a WrapHandle instance
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("sub-id".to_string(), Value::Int(sub_id as i64));
            attrs.insert("handle-id".to_string(), Value::Int(handle_id as i64));
            // Store a reference to the wrapped sub for .restore()
            attrs.insert("wrapped-sub".to_string(), target.clone());
            return Some(Ok(Value::make_instance(
                Symbol::intern("Routine::WrapHandle"),
                attrs,
            )));
        }
        if method == "unwrap" {
            // Look up original sub_id by name, since &foo creates a fresh Sub each time
            let func_name = data.name.resolve();
            let sub_id = self
                .wrap_sub_names
                .iter()
                .find(|(_, n)| **n == func_name)
                .map(|(id, _)| *id)
                .unwrap_or(data.id);
            if args.is_empty() {
                // unwrap with no args on a never-wrapped sub should error
                if !self.wrap_chains.contains_key(&sub_id) || self.wrap_chains[&sub_id].is_empty() {
                    return Some(Err(RuntimeError::new(
                        "Cannot unwrap a sub that has not been wrapped",
                    )));
                }
                // Pop the outermost wrapper
                if let Some(chain) = self.wrap_chains.get_mut(&sub_id) {
                    chain.pop();
                    if chain.is_empty() {
                        self.cleanup_wrap_name_entries(sub_id);
                    }
                }
                return Some(Ok(Value::Bool(true)));
            }
            // Extract handle-id from the WrapHandle argument
            let handle = &args[0];
            let handle_id = self.extract_wrap_handle_id(handle);
            let Some(handle_id) = handle_id else {
                return Some(Err(RuntimeError::new(
                    "unwrap requires a valid WrapHandle argument",
                )));
            };
            let chain = self.wrap_chains.get_mut(&sub_id);
            if let Some(chain) = chain {
                let before_len = chain.len();
                chain.retain(|(hid, _)| *hid != handle_id);
                if chain.len() == before_len {
                    return Some(Err(RuntimeError::new(
                        "Cannot unwrap: handle not found (already unwrapped?)",
                    )));
                }
                if chain.is_empty() {
                    self.cleanup_wrap_name_entries(sub_id);
                }
                return Some(Ok(Value::Bool(true)));
            }
            return Some(Err(RuntimeError::new(
                "Cannot unwrap a sub that has not been wrapped",
            )));
        }
        if method == "callwith" {
            // &sub.callwith(args) — call the sub directly with provided args
            return Some(self.call_sub_value(target.clone(), args, false));
        }
        if method == "can" {
            let method_name = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let can = matches!(
                method_name.as_str(),
                "assuming"
                    | "signature"
                    | "arity"
                    | "count"
                    | "candidates"
                    | "cando"
                    | "of"
                    | "name"
                    | "raku"
                    | "perl"
                    | "Str"
                    | "gist"
                    | "can"
                    | "wrap"
                    | "unwrap"
                    | "callwith"
            );
            return Some(Ok(Value::Bool(can)));
        }
        None
    }

    /// Extract wrap handle ID from a WrapHandle instance.
    fn extract_wrap_handle_id(&self, handle: &Value) -> Option<u64> {
        match handle {
            Value::Instance { attributes, .. } => {
                if let Some(Value::Int(id)) = attributes.get("handle-id") {
                    Some(*id as u64)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Clean up wrap_sub_names and wrap_name_to_sub when a sub's wrap chain becomes empty.
    pub(crate) fn cleanup_wrap_name_entries(&mut self, sub_id: u64) {
        if let Some(name) = self.wrap_sub_names.remove(&sub_id) {
            self.wrap_name_to_sub.remove(&name);
        }
    }
}
