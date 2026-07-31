use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

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
                return Ok(Value::NIL);
            }
            let arg = &args[0];
            let normalized = if op == "−" { "-" } else { op };
            return match op {
                "!" => Ok(Value::truth(!arg.truthy())),
                "+" => Ok(crate::runtime::coerce_to_numeric(arg.clone())),
                "-" | "−" => crate::builtins::arith_negate(arg.clone()),
                "~" => {
                    if let Some(err) = self.failure_to_runtime_error_if_unhandled(arg) {
                        return Err(err);
                    }
                    Ok(Value::str(crate::runtime::utils::coerce_to_str(arg)))
                }
                "?" => Ok(Value::truth(arg.truthy())),
                "so" => Ok(Value::truth(arg.truthy())),
                "not" => Ok(Value::truth(!arg.truthy())),
                "++" => crate::builtins::arith_add(arg.clone(), Value::int(1)),
                "--" => Ok(crate::builtins::arith_sub(arg.clone(), Value::int(1))),
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
                                item.view(),
                                ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_)
                            ) {
                                self.call_function_fallback(name, std::slice::from_ref(item))?
                            } else {
                                self.call_function_fallback(&base_name, std::slice::from_ref(item))?
                            };
                            results.push(v);
                        }
                        return Ok(Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(results)),
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
                        let coerced = if matches!(
                            arg.view(),
                            ValueView::Instance { .. } | ValueView::Package(..)
                        ) {
                            self.call_method_with_values(arg.clone(), "Numeric", vec![])
                                .unwrap_or_else(|_| arg.clone())
                        } else {
                            arg.clone()
                        };
                        // Applying `i` to an existing Complex (directly, or via a
                        // `.Numeric` coercion that returns one) rotates it 90°
                        // (multiplies by i): `(r+ei)\i == -e+ri`.
                        if let ValueView::Complex(r, i) = coerced.view() {
                            return Ok(Value::complex(-i, r));
                        }
                        let n = crate::runtime::coerce_to_numeric(coerced);
                        let num_val = match n.view() {
                            ValueView::Int(i) => i as f64,
                            ValueView::Num(f) => f,
                            ValueView::Rat(n, d) => n as f64 / d as f64,
                            _ => {
                                return Err(RuntimeError::new(
                                    "Cannot coerce to Numeric for postfix:<i>".to_string(),
                                ));
                            }
                        };
                        return Ok(Value::complex(0.0, num_val));
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
                                    item.view(),
                                    ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_)
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
                            return Ok(Value::array_with_kind(
                                crate::gc::Gc::new(crate::value::ArrayData::new(results)),
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
                return Ok(Value::NIL);
            };
            if let Some(enum_value) = self.coerce_to_enum_variant(name, &variants, first.clone()) {
                return Ok(enum_value);
            }
            // Return a Failure wrapping X::Enum::NoValue (lazy exception, like Raku)
            let value_str = first.to_string_value();
            let msg = format!("No value '{}' found in enum {}", value_str, name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("type".to_string(), Value::package(Symbol::intern(name)));
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
            let source = match args[0].view() {
                ValueView::Package(sym) => Some(sym.resolve()),
                ValueView::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    let args_str = type_args
                        .iter()
                        .map(|arg| match arg.view() {
                            ValueView::Package(n) => n.resolve(),
                            _ => arg.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    Some(format!("{}[{}]", base_name.resolve(), args_str))
                }
                ValueView::Nil => Some("Any".to_string()),
                _ => None,
            };
            if let Some(source) = source {
                return Ok(Value::package(Symbol::intern(&format!("{name}({source})"))));
            }
        }
        // Handle zip:with — zip with a custom combining function
        if name == "zip"
            && args
                .iter()
                .any(|a| matches!(a.view(), ValueView::Pair(k, _) if k == "with"))
        {
            return self.builtin_zip_with(args);
        }
        // A user-declared routine SHADOWS a same-named builtin: in raku a lexical
        // `sub abs` (declared here or imported from a module) wins over CORE's.
        // The native table is consulted below, so without this check the builtin
        // silently ran instead — and only for some shapes, which made it look
        // arbitrary: the VM's named-call path normally runs the user def directly
        // and only falls through to here when its strict builtin-shadow gate
        // (`def_is_otf_compilable`, which rejects a DEFAULT parameter for
        // name-cache reasons — PR #3546) says the def cannot be OTF-compiled. So
        // `sub rotate (Str $s, Int $n = 1) is export` lost every call while the
        // same sub without the default won. Resolution happens through the normal
        // registry lookup, so a name with no user routine costs one miss and
        // reaches the native table exactly as before.
        let user_shadows_builtin = Self::is_builtin_function(name)
            && self.resolve_function_with_alias(name, args).is_some();
        if !user_shadows_builtin
            && let Some(native_result) =
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
            && args
                .iter()
                .any(|a| matches!(a.view(), ValueView::Instance { .. }))
        {
            let mut coerced_args: Vec<Value> = Vec::with_capacity(args.len());
            let mut all_ok = true;
            for arg in args {
                if matches!(arg.view(), ValueView::Instance { .. }) {
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
            && let ValueView::Mixin(_, mixins) = callable.view()
        {
            for key in mixins.keys() {
                if let Some(role_name) = key.strip_prefix("__mutsu_role__")
                    && self.role_has_method(role_name, "CALL-ME")
                {
                    return self.call_method_with_values(
                        callable.clone(),
                        "CALL-ME",
                        args.to_vec(),
                    );
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
            let saved_readonly = self.enter_readonly_frame();
            if let Some(line) = self.test_pending_callsite_line {
                self.cur_source_line = line;
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
                if let Some(sub_val) = self.env.get(&ampname).cloned()
                    && let ValueView::Sub(sub_data) = sub_val.view()
                {
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
                        self.exit_readonly_frame(saved_readonly);
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
                def_file: None,
            });
            // Set __mutsu_callable_id so blocks defined inside this routine
            // capture the correct target for non-local return.
            let callable_key = format!("__mutsu_callable_id::{}::{}", def.package, def.name);
            if let Some(id_val) = self.env.get(&callable_key).cloned()
                && let ValueView::Int(id) = id_val.view()
            {
                self.env
                    .insert("__mutsu_callable_id".to_string(), Value::int(id));
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
            // Tell the fresh-compiler body path which parameters are sigilless
            // (`\attr`) so a nested closure captures them by name instead of
            // compiling a bare reference as a bareword (lost capture). Restored
            // after the eval; a nested call sets and restores its own.
            let saved_eval_sigilless = std::mem::replace(
                &mut self.pending_eval_sigilless,
                def.param_defs
                    .iter()
                    .filter(|pd| pd.sigilless && !pd.name.is_empty())
                    .map(|pd| pd.name.clone())
                    .collect(),
            );
            let saved_eval_placeholders = std::mem::replace(
                &mut self.pending_eval_placeholder_params,
                def.params
                    .iter()
                    .filter(|p| p.trim_start_matches(['$', '@', '%', '&']).starts_with('^'))
                    .cloned()
                    .collect(),
            );
            let result = self.eval_block_value_with_pre_post(&def.body);
            self.pending_eval_placeholder_params = saved_eval_placeholders;
            self.pending_eval_sigilless = saved_eval_sigilless;
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
                        v.view(),
                        ValueView::Array(..)
                            | ValueView::Hash(..)
                            | ValueView::Sub(..)
                            | ValueView::WeakSub(..)
                            | ValueView::Routine { .. }
                    );
                if k != "_"
                    && k != "@_"
                    && k != "%_"
                    // Per-frame non-local-return target marker: writing the callee's
                    // id back would retarget blocks the caller creates afterwards.
                    && k != "__mutsu_callable_id"
                    && ((restored_env.contains_key_sym(*k)
                        && !excluded_names.contains(&k_str)
                        && matches!(v.view(), ValueView::Array(..) | ValueView::Hash(..)))
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
            self.exit_readonly_frame(saved_readonly);
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
                && e.return_target_callable_id().is_some()
            {
                let my_id = self.env.get(&callable_key).and_then(|v| match v.view() {
                    ValueView::Int(i) => Some(i as u64),
                    _ => None,
                });
                if my_id != e.return_target_callable_id() {
                    return result;
                }
            }
            let finalized =
                self.finalize_return_with_spec(result, effective_return_spec.as_deref());
            return finalized.and_then(|v| {
                let v = if def.is_raw {
                    // Mark Proxy as decontainerized so the VM's auto-FETCH doesn't strip it
                    if matches!(v.view(), ValueView::Proxy { .. }) {
                        let (fetcher, storer, subclass, _) = v.into_proxy_parts().unwrap();
                        Value::proxy_parts(fetcher, storer, subclass, true)
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
            .filter(|v| {
                matches!(
                    v.view(),
                    ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                )
            })
            .or_else(|| {
                callable_from_plain.filter(|v| {
                    matches!(
                        v.view(),
                        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                    )
                })
            })
        {
            return self.eval_call_on_value(callable, args.to_vec());
        }
        if self.has_proto(name) {
            // Build call profile: name(Type1:D, Type2:D, ...)
            let arg_types: Vec<String> = args
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                .map(|a| {
                    let tn = super::value_type_name(a);
                    if !matches!(a.view(), ValueView::Nil) {
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

        // Invoking a *class* type object coerces: `Foo($x)` is `Foo.COERCE($x)`
        // when the class defines COERCE, else `Foo.new($x)`, else
        // X::Coerce::Impossible — the same protocol the role branch below
        // implements after punning. Built-in types (`Int("42")`), roles and enums
        // already went through their own paths; a user-declared class had none, so
        // `Locale::Dates($locale)` died with "Unknown function: Dates".
        //
        // `CALL-ME` wins over both: a type object carrying one is invocable
        // rather than coercive.
        // `!has_role`: coercing a role puns it to a class, so after the first
        // `R1("x")` the pun makes `has_class("R1")` true and this branch would
        // shadow the role branch below for every later call. The role branch owns
        // roles (it also handles the `does`-RHS Pair form), so defer to it.
        if self.has_class(name) && !self.has_role(name) && !args.is_empty() {
            if self.class_has_method(name, "CALL-ME") {
                return self.call_method_with_values(
                    Value::package(Symbol::intern(name)),
                    "CALL-ME",
                    args.to_vec(),
                );
            }
            // A coercion takes ONE value: `B("q", "r")` coerces the List, it does
            // not splat two arguments. raku shows this by rejecting
            // `class B { method new($x, $y) {…} }; B("q","r")` with "Impossible
            // coercion from 'List'" while accepting it for `new($x)`.
            let coercee = if args.len() == 1 {
                vec![args[0].clone()]
            } else {
                vec![Value::array(args.to_vec())]
            };
            // COERCE first, then `new` — and fall back from a COERCE that has no
            // matching candidate to `new`, which is what raku does (a class may
            // declare `multi method COERCE(Str)` and `multi method new(Int)` and
            // accept both spellings). Mirrors the role branch below.
            if self.class_has_method(name, "COERCE") {
                let coerced = self.call_method_with_values(
                    Value::package(Symbol::intern(name)),
                    "COERCE",
                    coercee.clone(),
                );
                if coerced.is_ok() || !self.class_has_method(name, "new") {
                    return coerced;
                }
            }
            if self.class_has_method(name, "new") {
                return self.call_method_with_values(
                    Value::package(Symbol::intern(name)),
                    "new",
                    coercee,
                );
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

        if self.has_role(name) {
            // If the role has CALL-ME, dispatch to it on the type object
            if self.role_has_method(name, "CALL-ME") {
                return self.call_method_with_values(
                    Value::package(Symbol::intern(name)),
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
                        Value::package(Symbol::intern(name)),
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
                        Value::package(Symbol::intern(name)),
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
                    return Ok(Value::pair(name.to_string(), Value::array(args.to_vec())));
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
            return Ok(Value::pair(name.to_string(), Value::array(args.to_vec())));
        }
        if name.starts_with("X::") {
            return Ok(Value::package(Symbol::intern(name)));
        }

        // Check if multi candidates exist for this name (no matching arity/types)
        if self.has_multi_candidates(name) {
            // Build call profile: name(Type1:D, Type2:D, ...)
            let arg_types: Vec<String> = args
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                .map(|a| {
                    let tn = super::value_type_name(a);
                    if !matches!(a.view(), ValueView::Nil) {
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
                Value::package(Symbol::intern(name)),
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
                Value::package(Symbol::intern(name)),
                "CALL-ME",
                args.to_vec(),
            );
        }

        // TypeName() with no args produces a coercion type TypeName(Any).
        // Also handles a type smiley: `Int:D()` -> `Int:D(Any)`.
        if args.is_empty() {
            let (base, smiley) = crate::runtime::types::strip_type_smiley(name);
            // A bound generic type parameter (`T()` / `T:D()` inside a role method
            // where `T` -> `Int`) forms `Int(Any)` / `Int:D(Any)`.
            if let Some(v) = self.env.get(base)
                && let ValueView::Package(pkg) = v.view()
            {
                let resolved = format!("{}{}(Any)", pkg.resolve(), smiley.unwrap_or(""));
                return Ok(Value::package(Symbol::intern(&resolved)));
            }
            if self.has_type(base)
                || crate::runtime::utils::is_known_type_constraint(base)
                || self.registry().subsets.contains_key(base)
                || self.registry().roles.contains_key(base)
            {
                return Ok(Value::package(Symbol::intern(&format!("{name}(Any)"))));
            }
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

        // An `nqp::` op mutsu does not implement must NOT fall through to the
        // package-prefix strip below, because Raku almost always has a same-named
        // builtin with *different* semantics — `nqp::index("hello", "z")` reached
        // Raku's `index` and returned Nil where nqp yields -1. A silent wrong
        // answer is worse than an error, and nqp code branches on exactly these
        // values (`!= -1`). The ops mutsu really does implement (`nqp::atkey`,
        // `nqp::atpos`, `nqp::ordat`, `nqp::gethostname`, `nqp::bindattr`) are
        // matched earlier under their full name, so they are unaffected.
        //
        // The `nqp::` namespace is reserved and its op set is documented, so
        // rejecting an unimplemented one is safe. (The general case —
        // `Foo::Bar::index(…)` also reaching the builtin, where raku says "Could
        // not find symbol '&index' in 'GLOBAL::Foo::Bar'" — is the same shape but
        // a wider blast radius; see todo/tickets/nqp-op-aliasing-and-sha1.md.)
        if let Some(op) = name.strip_prefix("nqp::") {
            // The supported VALUE-op table (runtime/nqp_ops.rs) — anything it
            // does not know keeps the loud unsupported error below.
            if let Some(result) = self.call_nqp_op(op, args) {
                return result;
            }
            return Err(RuntimeError::new(format!(
                "Unsupported nqp:: op: nqp::{op}"
            )));
        }

        // A qualified call retries under its short name (`Main::foo` -> `foo`) —
        // that is how a call qualified with a package mutsu did not register
        // still finds its routine.
        //
        // The retry only makes sense when mutsu has *something declared* under
        // the short name. Retrying unconditionally meant a qualified call landed
        // on Raku's same-named builtin: `Foo::Bar::index("hello", "l")` returned
        // 2, and `Test::ok(1)` ran the TAP routine, where raku says "Could not
        // find symbol '&index' in 'GLOBAL::Foo::Bar'". A package qualifier is
        // not a decoration to be discarded.
        if let Some(pos) = name.rfind("::") {
            let short_name = &name[pos + 2..];
            if self.qualified_retry_resolves(short_name, args) {
                return self.call_function(short_name, args.to_vec());
            }
            return Err(self.no_such_qualified_symbol(&name[..pos], short_name));
        }

        // NativeCall's `explicitly-manage($str)` marks a value's C-side buffer
        // as caller-managed so the GC will not free it while a native call holds
        // the pointer. mutsu copies each `Str` argument into an owned `CString`
        // that lives for the duration of the call, so there is nothing to pin —
        // treat it as an identity no-op returning its argument.
        if name == "explicitly-manage" {
            return Ok(args.first().cloned().unwrap_or(Value::NIL));
        }

        // Native JSON routines invoked as code objects (`&from-json`,
        // `$str.&from-json`) reach this generic fallback — they have no
        // declared sub for the resolver above to find.
        if let Some(result) = self.try_native_json_function(name, args) {
            return result;
        }

        let suggestions = self.suggest_routine_names(name);
        Err(RuntimeError::undeclared_routine_symbols(
            name,
            format!("Unknown function: {}", name),
            suggestions,
        ))
    }

    /// Does mutsu have anything *declared* under a qualified call's short name?
    /// This gates the package-prefix strip: the strip exists so that a call
    /// qualified with a package mutsu never registered still finds its own
    /// routine, not so that any qualifier can be dropped to reach a builtin.
    ///
    /// Deliberately asks "is something declared here" rather than "is this a
    /// builtin": the builtin question has no reliable answer, because names like
    /// `index` are dispatched by a hand-written arm of `call_function` and are
    /// not in `BUILTIN_FUNCTION_NAMES`, so `is_builtin_function` misses them.
    fn qualified_retry_resolves(&mut self, short_name: &str, args: &[Value]) -> bool {
        if self.resolve_proto_function_with_alias(short_name).is_some()
            || self.has_multi_candidates(short_name)
            || self.wrap_sub_id_for_name(short_name).is_some()
            || self.env.get(&format!("&{short_name}")).is_some()
        {
            return true;
        }
        if self.resolve_function_with_alias(short_name, args).is_some() {
            return true;
        }
        // A routine that exists but whose candidates did not match left a
        // pending dispatch error. Let the retry run so that error is what the
        // caller sees, rather than "no such symbol".
        if self.pending_dispatch_error.is_some() {
            return true;
        }
        // The strip also carries type coercion (`Foo::Bar("x")`) when mutsu
        // registered the class only under its short name.
        self.has_class(short_name)
            || self.has_role(short_name)
            || self.registry().subsets.contains_key(short_name)
            || self.registry().enum_types.contains_key(short_name)
    }

    /// raku's error for a qualified call whose short name resolves to nothing:
    /// `Could not find symbol '&index' in 'GLOBAL::Foo::Bar'` (an `X::AdHoc`).
    /// A package raku knows about is named bare — `class C {}; C::foo()` says
    /// `in 'C'` — while one it has never seen is reported under `GLOBAL::`.
    fn no_such_qualified_symbol(&self, package: &str, short_name: &str) -> RuntimeError {
        // An explicitly written `GLOBAL::` qualifier resolves through the
        // pseudo-package, and raku then names the symbol without its `&` sigil:
        // `GLOBAL::index(…)` is "Could not find symbol 'index' in 'GLOBAL'",
        // while `Foo::Bar::index(…)` is "'&index' in 'GLOBAL::Foo::Bar'".
        let (package, sigil) = match package.strip_prefix("GLOBAL::") {
            Some(rest) => (rest, ""),
            None if package == "GLOBAL" => ("", ""),
            None => (package, "&"),
        };
        let known = self.has_class(package)
            || self.has_role(package)
            || self.registry().package_kinds.contains_key(package)
            || self.registry().package_stubs.contains(package);
        let qualified = if package.is_empty() {
            "GLOBAL".to_string()
        } else if known {
            package.to_string()
        } else {
            format!("GLOBAL::{package}")
        };
        RuntimeError::new(format!(
            "Could not find symbol '{sigil}{short_name}' in '{qualified}'"
        ))
    }
}
