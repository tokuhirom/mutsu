use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn call_function_fallback(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(op) = name
            .strip_prefix("infix:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            let normalized = if op == "−" { "-" } else { op };
            return self.call_infix_routine(normalized, args);
        }
        if let Some(op) = name
            .strip_prefix("prefix:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            if let Some(def) = self.resolve_function_with_alias(name, args) {
                return self.call_function_def(&def, args);
            }
            if let Some(err) = self.take_pending_dispatch_error() {
                return Err(err);
            }
            if let Some(callable) = self.env.get(&format!("&{}", name)).cloned() {
                return self.call_sub_value(callable, args.to_vec(), false);
            }
            if args.is_empty() {
                return Ok(Value::Nil);
            }
            let arg = &args[0];
            let normalized = if op == "−" { "-" } else { op };
            return match op {
                "!" => Ok(Value::Bool(!arg.truthy())),
                "+" => Ok(crate::runtime::coerce_to_numeric(arg.clone())),
                "-" | "−" => crate::builtins::arith_negate(arg.clone()),
                "~" => {
                    if let Some(err) = self.failure_to_runtime_error_if_unhandled(arg) {
                        return Err(err);
                    }
                    Ok(Value::str(crate::runtime::utils::coerce_to_str(arg)))
                }
                "?" => Ok(Value::Bool(arg.truthy())),
                "so" => Ok(Value::Bool(arg.truthy())),
                "not" => Ok(Value::Bool(!arg.truthy())),
                "++" => crate::builtins::arith_add(arg.clone(), Value::Int(1)),
                "--" => Ok(crate::builtins::arith_sub(arg.clone(), Value::Int(1))),
                _ => {
                    // Auto-generated reduction prefix: prefix:<[op]>
                    // e.g. prefix:<[**]>(2,3,4) is equivalent to [**] 2,3,4
                    if let Some(reduce_op) = normalized
                        .strip_prefix('[')
                        .and_then(|s| s.strip_suffix(']'))
                    {
                        let (actual_op, reversed) = if let Some(inner) = reduce_op.strip_prefix('R')
                        {
                            (inner, true)
                        } else {
                            (reduce_op, false)
                        };
                        let mut items: Vec<Value> = args.to_vec();
                        if reversed {
                            items.reverse();
                        }
                        if items.is_empty() {
                            return Ok(crate::runtime::reduction_identity(actual_op));
                        }
                        if items.len() == 1 {
                            return Ok(items.into_iter().next().unwrap());
                        }
                        // Check associativity: right-associative ops fold from right
                        let is_right = matches!(actual_op, "**" | "=" | ":=" | "=>" | "x" | "xx");
                        if is_right {
                            let mut acc = items.last().unwrap().clone();
                            for item in items[..items.len() - 1].iter().rev() {
                                acc = Self::apply_reduction_op(actual_op, item, &acc)?;
                            }
                            return Ok(acc);
                        }
                        let mut acc = items[0].clone();
                        for item in &items[1..] {
                            acc = Self::apply_reduction_op(actual_op, &acc, item)?;
                        }
                        return Ok(acc);
                    }
                    // Hyper prefix operator, e.g. prefix:<-«> / prefix:<-<<>:
                    // apply the base prefix element-wise (recursing into nested
                    // Iterables, like `>>`).
                    let base = normalized
                        .strip_suffix('\u{00AB}')
                        .or_else(|| normalized.strip_suffix("<<"))
                        .or_else(|| normalized.strip_suffix('\u{00BB}'))
                        .or_else(|| normalized.strip_suffix(">>"))
                        .or_else(|| normalized.strip_prefix('\u{00AB}'))
                        .or_else(|| normalized.strip_prefix("<<"))
                        .or_else(|| normalized.strip_prefix('\u{00BB}'))
                        .or_else(|| normalized.strip_prefix(">>"));
                    if let Some(base) = base
                        && !base.is_empty()
                        && base != normalized
                    {
                        let base_name = format!("prefix:<{}>", base);
                        let items = crate::runtime::value_to_list(arg);
                        let mut results = Vec::with_capacity(items.len());
                        for item in &items {
                            let v = if matches!(
                                item,
                                Value::Array(..) | Value::Seq(_) | Value::Slip(_)
                            ) {
                                self.call_function_fallback(name, std::slice::from_ref(item))?
                            } else {
                                self.call_function_fallback(&base_name, std::slice::from_ref(item))?
                            };
                            results.push(v);
                        }
                        return Ok(Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new(results)),
                            crate::value::ArrayKind::List,
                        ));
                    }
                    Err(RuntimeError::new(format!(
                        "Unknown prefix operator: {}",
                        normalized
                    )))
                }
            };
        }
        if let Some(op) = name
            .strip_prefix("postfix:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            if let Some(def) = self.resolve_function_with_alias(name, args) {
                return self.call_function_def(&def, args);
            }
            if let Some(err) = self.take_pending_dispatch_error() {
                return Err(err);
            }
            if let Some(callable) = self.env.get(&format!("&{}", name)).cloned() {
                return self.call_sub_value(callable, args.to_vec(), false);
            }
            if !args.is_empty() {
                let arg = &args[0];
                match op {
                    "i" => {
                        // For Instance/Package types, try calling .Numeric method first
                        let coerced = if matches!(arg, Value::Instance { .. } | Value::Package(..))
                        {
                            self.call_method_with_values(arg.clone(), "Numeric", vec![])
                                .unwrap_or_else(|_| arg.clone())
                        } else {
                            arg.clone()
                        };
                        let n = crate::runtime::coerce_to_numeric(coerced);
                        let num_val = match &n {
                            Value::Int(i) => *i as f64,
                            Value::Num(f) => *f,
                            Value::Rat(n, d) => *n as f64 / *d as f64,
                            _ => {
                                return Err(RuntimeError::new(
                                    "Cannot coerce to Numeric for postfix:<i>".to_string(),
                                ));
                            }
                        };
                        return Ok(Value::Complex(0.0, num_val));
                    }
                    _ => {
                        // Hyper postfix operator, e.g. postfix:<»i>: apply the
                        // base postfix element-wise (recursing into Iterables).
                        let base = op
                            .strip_prefix('\u{00BB}')
                            .or_else(|| op.strip_prefix(">>"))
                            .or_else(|| op.strip_prefix('\u{00AB}'))
                            .or_else(|| op.strip_prefix("<<"))
                            .or_else(|| op.strip_suffix('\u{00BB}'))
                            .or_else(|| op.strip_suffix(">>"))
                            .or_else(|| op.strip_suffix('\u{00AB}'))
                            .or_else(|| op.strip_suffix("<<"));
                        if let Some(base) = base
                            && !base.is_empty()
                            && base != op
                        {
                            let base_name = format!("postfix:<{}>", base);
                            let items = crate::runtime::value_to_list(arg);
                            let mut results = Vec::with_capacity(items.len());
                            for item in &items {
                                let v = if matches!(
                                    item,
                                    Value::Array(..) | Value::Seq(_) | Value::Slip(_)
                                ) {
                                    self.call_function_fallback(name, std::slice::from_ref(item))?
                                } else {
                                    self.call_function_fallback(
                                        &base_name,
                                        std::slice::from_ref(item),
                                    )?
                                };
                                results.push(v);
                            }
                            return Ok(Value::Array(
                                std::sync::Arc::new(crate::value::ArrayData::new(results)),
                                crate::value::ArrayKind::List,
                            ));
                        }
                        // Unknown postfix operator is a syntax error in Raku (X::Syntax::Confused)
                        return Err(RuntimeError::syntax_confused_with_reason(format!(
                            "Bogus postfix: {}",
                            op
                        )));
                    }
                }
            } else {
                // Unknown postfix operator with no args is still a syntax error
                return Err(RuntimeError::syntax_confused_with_reason(format!(
                    "Bogus postfix: {}",
                    op
                )));
            }
        }
        if (self.loaded_modules.contains("Test")
            || self.loaded_modules.iter().any(|m| m.starts_with("Test::")))
            && let Some(result) = self.call_test_function(name, args)?
        {
            return Ok(result);
        }
        if let Some(pattern) = self.eval_token_call_values(name, args)? {
            return Ok(Value::regex(pattern));
        }
        let variants = self.registry().enum_types.get(name).cloned();
        if let Some(variants) = variants {
            let Some(first) = args.first().cloned() else {
                return Ok(Value::Nil);
            };
            if let Some(enum_value) = self.coerce_to_enum_variant(name, &variants, first.clone()) {
                return Ok(enum_value);
            }
            // Return a Failure wrapping X::Enum::NoValue (lazy exception, like Raku)
            let value_str = first.to_string_value();
            let msg = format!("No value '{}' found in enum {}", value_str, name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("type".to_string(), Value::Package(Symbol::intern(name)));
            attrs.insert("value".to_string(), first);
            let ex = Value::make_instance(Symbol::intern("X::Enum::NoValue"), attrs);
            let mut failure_attrs = std::collections::HashMap::new();
            failure_attrs.insert("exception".to_string(), ex);
            return Ok(Value::make_instance(
                Symbol::intern("Failure"),
                failure_attrs,
            ));
        }
        // Calling a type with a type object argument constructs a coercion type
        // object (e.g. Str(Any), Int(Str), Child(Parent)).
        if args.len() == 1
            && (self.has_type(name)
                || crate::runtime::utils::is_known_type_constraint(name)
                || self.registry().subsets.contains_key(name)
                || self.registry().roles.contains_key(name))
        {
            let source = match &args[0] {
                Value::Package(sym) => Some(sym.resolve()),
                Value::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    let args_str = type_args
                        .iter()
                        .map(|arg| match arg {
                            Value::Package(n) => n.resolve(),
                            other => other.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    Some(format!("{}[{}]", base_name.resolve(), args_str))
                }
                Value::Nil => Some("Any".to_string()),
                _ => None,
            };
            if let Some(source) = source {
                return Ok(Value::Package(Symbol::intern(&format!("{name}({source})"))));
            }
        }
        // Handle zip:with — zip with a custom combining function
        if name == "zip"
            && args
                .iter()
                .any(|a| matches!(a, Value::Pair(k, _) if k == "with"))
        {
            return self.builtin_zip_with(args);
        }
        if let Some(native_result) =
            crate::builtins::native_function(crate::symbol::Symbol::intern(name), args)
        {
            return native_result;
        }
        if name == "substr"
            && let Some((target, rest)) = args.split_first()
        {
            return self.call_method_with_values(target.clone(), "substr", rest.to_vec());
        }
        if name == "substr-rw"
            && let Some((_target, rest)) = args.split_first()
        {
            // Try to get the variable name of the first argument for Proxy support
            let arg_sources = self.pending_call_arg_sources.clone().unwrap_or_default();
            if let Some(Some(var_name)) = arg_sources.first() {
                return self.make_substr_rw_proxy(var_name, rest);
            }
            // Fallback: search env for matching value to find variable name
            let target_var = {
                let target = &args[0];
                let mut found = None;
                for (k, v) in self.env.iter() {
                    if crate::runtime::values_identical(v, target) && !k.starts_with("__") {
                        found = Some(k.resolve());
                        break;
                    }
                }
                found
            };
            if let Some(ref var_name) = target_var {
                return self.make_substr_rw_proxy(var_name, rest);
            }
            // No variable name available, just return the substring
            return self.call_method_with_values(args[0].clone(), "substr-rw", rest.to_vec());
        }
        if name == "unpolar"
            && let Some((target, rest)) = args.split_first()
        {
            return self.call_method_with_values(target.clone(), "unpolar", rest.to_vec());
        }
        // Coerce user-defined types for builtin functions via .Numeric/.Bridge
        if Self::is_builtin_function(name)
            && args.iter().any(|a| matches!(a, Value::Instance { .. }))
        {
            let mut coerced_args: Vec<Value> = Vec::with_capacity(args.len());
            let mut all_ok = true;
            for arg in args {
                if matches!(arg, Value::Instance { .. }) {
                    let coerced = self
                        .call_method_with_values(arg.clone(), "Numeric", vec![])
                        .or_else(|_| self.call_method_with_values(arg.clone(), "Bridge", vec![]));
                    match coerced {
                        Ok(val) => coerced_args.push(val),
                        Err(_) => {
                            all_ok = false;
                            break;
                        }
                    }
                } else {
                    coerced_args.push(arg.clone());
                }
            }
            if all_ok
                && let Some(native_result) = crate::builtins::native_function(
                    crate::symbol::Symbol::intern(name),
                    &coerced_args,
                )
            {
                return native_result;
            }
        }
        // Check if there's a callable with CALL-ME override (from trait_mod mixin)
        // before proto dispatch, as CALL-ME takes precedence over multi dispatch.
        if let Some(callable) = self.env.get(&format!("&{}", name)).cloned()
            && let Value::Mixin(_, ref mixins) = callable
        {
            for key in mixins.keys() {
                if let Some(role_name) = key.strip_prefix("__mutsu_role__")
                    && self.role_has_method(role_name, "CALL-ME")
                {
                    return self.call_method_with_values(callable, "CALL-ME", args.to_vec());
                }
            }
        }
        if let Some((proto_name, proto_def)) = self.resolve_proto_function_with_alias(name) {
            return self.call_proto_function(&proto_name, &proto_def, args);
        }
        // Check wrap chain for named function calls
        if let Some(sub_id) = self.wrap_sub_id_for_name(name)
            && !self.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.get_wrapped_sub(name)
        {
            return self.call_sub_value(sub_val, args.to_vec(), false);
        }
        if let Some(def) = self.resolve_function_with_alias(name, args) {
            // Collect remaining candidates for callsame/nextcallee/callwith.
            // Use all multi candidates (not just matching ones) because callwith()
            // can re-dispatch with different arguments.
            let all_candidates = self.resolve_all_multi_candidates(name);
            let def_fp =
                crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
            let remaining: Vec<std::sync::Arc<FunctionDef>> = all_candidates
                .into_iter()
                .filter(|c| {
                    crate::ast::function_body_fingerprint(&c.params, &c.param_defs, &c.body)
                        != def_fp
                })
                .collect();
            let pushed_dispatch = !remaining.is_empty();
            if pushed_dispatch {
                let rw_params =
                    super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs);
                self.multi_dispatch_stack.push((
                    name.to_string(),
                    remaining,
                    args.to_vec(),
                    rw_params,
                ));
            }
            self.samewith_context_stack.push((name.to_string(), None));
            if def.empty_sig && !args.is_empty() {
                self.samewith_context_stack.pop();
                return Err(Self::reject_args_for_empty_sig(args));
            }
            let routine_is_rw = !def.is_raw;
            let return_spec = self.routine_return_spec_by_name(&def.name.resolve());
            let saved_env = self.env.clone();
            let saved_readonly = self.save_readonly_vars();
            if let Some(line) = self.test_pending_callsite_line {
                self.env.insert("?LINE".to_string(), Value::Int(line));
            }
            self.push_caller_env();
            // When the function has where constraints and there is a &name Sub
            // in env (which carries closure env), merge the Sub's captured
            // variables so where-constraint expressions can access them.
            let fn_name = def.name.resolve();
            if def
                .param_defs
                .iter()
                .any(|pd| pd.where_constraint.is_some())
            {
                let ampname = format!("&{}", fn_name);
                if let Some(Value::Sub(ref sub_data)) = self.env.get(&ampname).cloned() {
                    for (k, v) in &sub_data.env {
                        if !k.starts_with("__mutsu_")
                            && !k.starts_with("?")
                            && !k.starts_with("!")
                            && k != "_"
                            && k != "@_"
                            && k != "%_"
                        {
                            self.env.insert_sym(*k, v.clone());
                        }
                    }
                }
            }
            let rw_bindings =
                match self.bind_function_args_values(&def.param_defs, &def.params, args) {
                    Ok(bindings) => bindings,
                    Err(e) => {
                        self.pop_caller_env();
                        self.env = saved_env;
                        self.restore_readonly_vars(saved_readonly);
                        self.samewith_context_stack.pop();
                        return Err(Self::enhance_binding_error(
                            e,
                            &def.name.resolve(),
                            &def.param_defs,
                            args,
                        ));
                    }
                };
            let sub_val = Value::make_sub(
                def.package,
                def.name,
                def.params.clone(),
                def.param_defs.clone(),
                def.body.clone(),
                def.is_rw,
                self.env.clone(),
            );
            self.block_stack.push(sub_val);
            let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
            self.routine_stack.push(RoutineFrame {
                package: def.package.resolve(),
                name: def.name.resolve(),
                line: None,
                file: None,
                is_method: false,
                is_block: false,
            });
            // Set __mutsu_callable_id so blocks defined inside this routine
            // capture the correct target for non-local return.
            let callable_key = format!("__mutsu_callable_id::{}::{}", def.package, def.name);
            if let Some(Value::Int(id)) = self.env.get(&callable_key).cloned() {
                self.env
                    .insert("__mutsu_callable_id".to_string(), Value::Int(id));
            }
            // Set current_package to the function's defining package so that
            // unqualified function lookups inside the body resolve correctly
            // (e.g., imported functions from `use` inside a module).
            let saved_package = self.current_package();
            let fn_pkg = def.package.resolve();
            if !fn_pkg.is_empty() && fn_pkg != "GLOBAL" {
                self.set_current_package(fn_pkg);
            }
            self.prepare_definite_return_slot(return_spec.as_deref());
            let result = self.eval_block_value_with_pre_post(&def.body);
            self.set_current_package(saved_package);
            self.routine_stack.pop();
            self.block_stack.pop();
            self.pop_test_assertion_context(pushed_assertion);
            self.pop_caller_env();
            let mut restored_env = saved_env;
            self.pop_caller_env_with_writeback(&mut restored_env);
            let excluded_names = Self::routine_writeback_excluded_names(&def);
            for (k, v) in self.env.iter() {
                let k_str = k.resolve();
                let scalar_writeback = restored_env.contains_key_sym(*k)
                    && !excluded_names.contains(&k_str)
                    && !matches!(
                        v,
                        Value::Array(..)
                            | Value::Hash(..)
                            | Value::Sub(..)
                            | Value::WeakSub(..)
                            | Value::Routine { .. }
                    );
                if k != "_"
                    && k != "@_"
                    && k != "%_"
                    && ((restored_env.contains_key_sym(*k)
                        && !excluded_names.contains(&k_str)
                        && matches!(v, Value::Array(..) | Value::Hash(..)))
                        || scalar_writeback
                        || k.starts_with("__mutsu_var_meta::"))
                {
                    restored_env.insert_sym(*k, v.clone());
                }
                if k.starts_with("__mutsu_var_meta::") {
                    restored_env.insert_sym(*k, v.clone());
                }
            }
            self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
            self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
            let effective_return_spec = return_spec
                .as_deref()
                .map(|spec| self.resolved_type_capture_name(spec));
            self.env = restored_env;
            self.restore_readonly_vars(saved_readonly);
            self.samewith_context_stack.pop();
            if pushed_dispatch {
                self.multi_dispatch_stack.pop();
            }
            // Convert fail errors to Failure values (same as closure call path)
            if let Err(e) = &result
                && e.is_fail()
            {
                return Ok(self.fail_error_to_failure_value(e));
            }
            // Non-local return targeting a different callable: propagate
            if let Err(ref e) = result
                && e.return_value.is_some()
                && e.return_target_callable_id.is_some()
            {
                let my_id = self.env.get(&callable_key).and_then(|v| match v {
                    Value::Int(i) => Some(*i as u64),
                    _ => None,
                });
                if my_id != e.return_target_callable_id {
                    return result;
                }
            }
            let finalized =
                self.finalize_return_with_spec(result, effective_return_spec.as_deref());
            return finalized.and_then(|v| {
                let v = if def.is_raw {
                    // Mark Proxy as decontainerized so the VM's auto-FETCH doesn't strip it
                    if let Value::Proxy {
                        fetcher,
                        storer,
                        subclass,
                        ..
                    } = v
                    {
                        Value::Proxy {
                            fetcher,
                            storer,
                            subclass,
                            decontainerized: true,
                        }
                    } else {
                        v
                    }
                } else {
                    v
                };
                self.maybe_fetch_rw_proxy(v, routine_is_rw)
            });
        }
        if let Some(err) = self.take_pending_dispatch_error() {
            return Err(err);
        }
        // Check for callable in env (e.g. &name) before proto dispatch failure.
        // This handles subs with CALL-ME mixed in via trait_mod.
        let callable_from_code_sigil = self.env.get(&format!("&{}", name)).cloned();
        let callable_from_plain = self.env.get(name).cloned();
        if let Some(callable) = callable_from_code_sigil
            .filter(|v| matches!(v, Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }))
            .or_else(|| {
                callable_from_plain.filter(|v| {
                    matches!(v, Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. })
                })
            })
        {
            return self.eval_call_on_value(callable, args.to_vec());
        }
        if self.has_proto(name) {
            // Build call profile: name(Type1:D, Type2:D, ...)
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
            let call_profile = format!("{}({})", name, arg_types.join(", "));
            let sig_lines = self.collect_multi_candidate_signatures(name, args.len());
            let sig_list = if sig_lines.is_empty() {
                String::new()
            } else {
                format!(":\n{}", sig_lines.join("\n"))
            };
            let message = format!(
                "Cannot resolve caller {}; none of these signatures matches{}",
                call_profile, sig_list
            );
            let mut err =
                RuntimeError::new(format!("No matching candidates for proto sub: {}", name));
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(message));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Multi::NoMatch"),
                attrs,
            )));
            return Err(err);
        }

        if self.has_role(name) {
            // If the role has CALL-ME, dispatch to it on the type object
            if self.role_has_method(name, "CALL-ME") {
                return self.call_method_with_values(
                    Value::Package(Symbol::intern(name)),
                    "CALL-ME",
                    args.to_vec(),
                );
            }
            // If the role has COERCE or new, pun it to a class and dispatch coercion
            if (self.role_has_method(name, "COERCE") || self.role_has_method(name, "new"))
                && args.len() == 1
            {
                self.ensure_role_punned_to_class(name);
                // Try COERCE first, then fall back to new
                if self.role_has_method(name, "COERCE") {
                    let coerce_result = self.call_method_with_values(
                        Value::Package(Symbol::intern(name)),
                        "COERCE",
                        args.to_vec(),
                    );
                    if coerce_result.is_ok() {
                        return coerce_result;
                    }
                }
                // Fall back to new
                if self.role_has_method(name, "new") {
                    let new_result = self.call_method_with_values(
                        Value::Package(Symbol::intern(name)),
                        "new",
                        args.to_vec(),
                    );
                    if new_result.is_ok() {
                        return new_result;
                    }
                }
            }
            // Role called with args but no CALL-ME/COERCE/new:
            // In `does` context, return a Pair for role application.
            // Otherwise, throw X::Coerce::Impossible.
            if !args.is_empty() {
                if self.in_does_rhs {
                    return Ok(Value::Pair(
                        name.to_string(),
                        Box::new(Value::array(args.to_vec())),
                    ));
                }
                let source_type = crate::runtime::value_type_name(&args[0]).to_string();
                let msg = format!(
                    "Impossible coercion from '{}' into '{}': no acceptable coercion method found",
                    source_type, name
                );
                return Err(RuntimeError::typed(
                    "X::Coerce::Impossible",
                    std::collections::HashMap::from([
                        ("target-type".to_string(), Value::str(name.to_string())),
                        ("from-type".to_string(), Value::str(source_type)),
                        ("message".to_string(), Value::str(msg)),
                    ]),
                ));
            }
            return Ok(Value::Pair(
                name.to_string(),
                Box::new(Value::array(args.to_vec())),
            ));
        }
        if name.starts_with("X::") {
            return Ok(Value::Package(Symbol::intern(name)));
        }

        // Check if multi candidates exist for this name (no matching arity/types)
        if self.has_multi_candidates(name) {
            // Build call profile: name(Type1:D, Type2:D, ...)
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
            let call_profile = format!("{}({})", name, arg_types.join(", "));
            let sig_lines = self.collect_multi_candidate_signatures(name, args.len());
            let sig_list = if sig_lines.is_empty() {
                String::new()
            } else {
                format!(":\n{}", sig_lines.join("\n"))
            };
            let message = format!(
                "Cannot resolve caller {}; none of these signatures matches{}",
                call_profile, sig_list
            );
            let mut err =
                RuntimeError::new(format!("No matching candidates for proto sub: {}", name));
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(message));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Multi::NoMatch"),
                attrs,
            )));
            return Err(err);
        }

        if matches!(name, "DateTime" | "Date") && args.len() == 1 {
            return self.call_method_with_values(
                Value::Package(Symbol::intern(name)),
                "new",
                args.to_vec(),
            );
        }

        // Fallback: if name is a known class/role with CALL-ME, invoke it on the type object.
        // When called with no args (e.g. A()), Raku treats it as a coercion type literal
        // rather than invoking CALL-ME — only A.() (dotted form) invokes CALL-ME with no args.
        if !args.is_empty()
            && ((self.has_class(name) && self.has_user_method(name, "CALL-ME"))
                || (self.has_role(name) && self.role_has_method(name, "CALL-ME")))
        {
            return self.call_method_with_values(
                Value::Package(Symbol::intern(name)),
                "CALL-ME",
                args.to_vec(),
            );
        }

        // TypeName() with no args produces a coercion type TypeName(Any)
        if args.is_empty()
            && (self.has_type(name)
                || crate::runtime::utils::is_known_type_constraint(name)
                || self.registry().subsets.contains_key(name)
                || self.registry().roles.contains_key(name))
        {
            return Ok(Value::Package(Symbol::intern(&format!("{name}(Any)"))));
        }

        // comb($matcher, $str) or comb($matcher, $str, $limit)
        if name == "comb" && args.len() >= 2 {
            let target = args[1].clone();
            let mut method_args = vec![args[0].clone()];
            for arg in &args[2..] {
                method_args.push(arg.clone());
            }
            return self.call_method_with_values(target, "comb", method_args);
        }

        // Try stripping package prefix (e.g., "Main::foo" -> "foo")
        if let Some(pos) = name.rfind("::") {
            let short_name = &name[pos + 2..];
            return self.call_function(short_name, args.to_vec());
        }

        let suggestions = self.suggest_routine_names(name);
        Err(RuntimeError::undeclared_routine_symbols(
            name,
            format!("Unknown function: {}", name),
            suggestions,
        ))
    }
}
