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
                env: self.env().clone(),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
            };
            // Store the routine name so call_sub_value can dispatch
            sub_data.env.insert(
                "__mutsu_routine_name".to_string(),
                Value::Str(name.to_string()),
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
                    ex_attrs.insert("symbol".to_string(), Value::Str(symbol));
                    ex_attrs.insert(
                        "payload".to_string(),
                        Value::Str(expected.to_string_value()),
                    );
                    let exception =
                        Value::make_instance(Symbol::intern("X::TypeCheck::Binding"), ex_attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), exception);
                    failure_attrs.insert("handled".to_string(), Value::Bool(false));
                    let failure = Value::make_instance(Symbol::intern("Failure"), failure_attrs);
                    let mut mixins = std::collections::HashMap::new();
                    mixins.insert("Failure".to_string(), failure);
                    Value::Mixin(
                        Box::new(Value::Sub(std::sync::Arc::new(sub_data.clone()))),
                        mixins,
                    )
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
                    Value::Str("Unexpected".to_string()),
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
                        Value::Str("Unexpected".to_string()),
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
        None
    }
}
