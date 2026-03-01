use super::*;
use crate::token_kind::TokenKind;
use num_traits::{Signed, ToPrimitive, Zero};

impl Interpreter {
    pub(super) fn call_function_fallback(
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
                "+" => Ok(Value::Int(crate::runtime::to_int(arg))),
                "-" | "−" => crate::builtins::arith_negate(arg.clone()),
                "~" => Ok(Value::Str(crate::runtime::utils::coerce_to_str(arg))),
                "?" => Ok(Value::Bool(arg.truthy())),
                "so" => Ok(Value::Bool(arg.truthy())),
                "not" => Ok(Value::Bool(!arg.truthy())),
                _ => Err(RuntimeError::new(format!(
                    "Unknown prefix operator: {}",
                    normalized
                ))),
            };
        }
        if (self.loaded_modules.contains("Test")
            || self.loaded_modules.iter().any(|m| m.starts_with("Test::")))
            && let Some(result) = self.call_test_function(name, args)?
        {
            return Ok(result);
        }
        if let Some(pattern) = self.eval_token_call_values(name, args)? {
            return Ok(Value::Regex(pattern));
        }
        if let Some(variants) = self.enum_types.get(name).cloned() {
            let Some(first) = args.first().cloned() else {
                return Ok(Value::Nil);
            };
            if let Some(enum_value) = self.coerce_to_enum_variant(name, &variants, first.clone()) {
                return Ok(enum_value);
            }
            // Throw X::Enum::NoValue
            let value_str = first.to_string_value();
            let msg = format!("No value '{}' found in enum {}", value_str, name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::Str(msg.clone()));
            attrs.insert("type".to_string(), Value::Str(name.to_string()));
            attrs.insert("value".to_string(), first);
            let ex = Value::make_instance("X::Enum::NoValue".to_string(), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        // Handle zip:with — zip with a custom combining function
        if name == "zip"
            && args
                .iter()
                .any(|a| matches!(a, Value::Pair(k, _) if k == "with"))
        {
            return self.builtin_zip_with(args);
        }
        if let Some(native_result) = crate::builtins::native_function(name, args) {
            return native_result;
        }
        if name == "substr"
            && let Some((target, rest)) = args.split_first()
        {
            return self.call_method_with_values(target.clone(), "substr", rest.to_vec());
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
                && let Some(native_result) = crate::builtins::native_function(name, &coerced_args)
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
        if let Some(def) = self.resolve_function_with_alias(name, args) {
            // Collect remaining candidates for callsame/nextcallee
            let all_candidates = self.resolve_all_matching_candidates(name, args);
            let def_fp =
                crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
            let remaining: Vec<FunctionDef> = all_candidates
                .into_iter()
                .filter(|c| {
                    crate::ast::function_body_fingerprint(&c.params, &c.param_defs, &c.body)
                        != def_fp
                })
                .collect();
            let pushed_dispatch = !remaining.is_empty();
            if pushed_dispatch {
                self.multi_dispatch_stack.push((remaining, args.to_vec()));
            }
            if def.empty_sig && !args.is_empty() {
                return Err(Self::reject_args_for_empty_sig(args));
            }
            let routine_is_rw = true;
            let return_spec = self.routine_return_spec_by_name(&def.name);
            let saved_env = self.env.clone();
            let saved_readonly = self.save_readonly_vars();
            if let Some(line) = self.test_pending_callsite_line {
                self.env.insert("?LINE".to_string(), Value::Int(line));
            }
            self.push_caller_env();
            let rw_bindings =
                match self.bind_function_args_values(&def.param_defs, &def.params, args) {
                    Ok(bindings) => bindings,
                    Err(e) => {
                        self.pop_caller_env();
                        self.env = saved_env;
                        self.restore_readonly_vars(saved_readonly);
                        return Err(e);
                    }
                };
            let sub_val = Value::make_sub(
                def.package.clone(),
                def.name.clone(),
                def.params.clone(),
                def.param_defs.clone(),
                def.body.clone(),
                def.is_rw,
                self.env.clone(),
            );
            self.block_stack.push(sub_val);
            let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
            self.routine_stack
                .push((def.package.clone(), def.name.clone()));
            self.prepare_definite_return_slot(return_spec.as_deref());
            let result = self.eval_block_value(&def.body);
            self.routine_stack.pop();
            self.block_stack.pop();
            self.pop_test_assertion_context(pushed_assertion);
            self.pop_caller_env();
            let mut restored_env = saved_env;
            self.pop_caller_env_with_writeback(&mut restored_env);
            let excluded_names = Self::routine_writeback_excluded_names(&def);
            for (k, v) in self.env.iter() {
                let scalar_writeback = restored_env.contains_key(k)
                    && !excluded_names.contains(k)
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
                    && ((restored_env.contains_key(k)
                        && matches!(v, Value::Array(..) | Value::Hash(..)))
                        || scalar_writeback
                        || k.starts_with("__mutsu_var_meta::"))
                {
                    restored_env.insert(k.clone(), v.clone());
                }
                if k.starts_with("__mutsu_var_meta::") {
                    restored_env.insert(k.clone(), v.clone());
                }
            }
            self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
            self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
            self.env = restored_env;
            self.restore_readonly_vars(saved_readonly);
            if pushed_dispatch {
                self.multi_dispatch_stack.pop();
            }
            let finalized = self.finalize_return_with_spec(result, return_spec.as_deref());
            return finalized.and_then(|v| self.maybe_fetch_rw_proxy(v, routine_is_rw));
        }
        // Check for callable in env (e.g. &name) before proto dispatch failure.
        // This handles subs with CALL-ME mixed in via trait_mod.
        let callable_from_code_sigil = self.env.get(&format!("&{}", name)).cloned();
        let callable_from_plain = self.env.get(name).cloned();
        if let Some(callable) = callable_from_code_sigil
            .filter(|v| matches!(v, Value::Sub(_) | Value::Routine { .. }))
            .or_else(|| {
                callable_from_plain.filter(|v| matches!(v, Value::Sub(_) | Value::Routine { .. }))
            })
        {
            return self.eval_call_on_value(callable, args.to_vec());
        }
        if self.has_proto(name) {
            let mut err =
                RuntimeError::new(format!("No matching candidates for proto sub: {}", name));
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::Str(format!(
                    "Cannot resolve caller {}; none of these signatures matches",
                    name
                )),
            );
            err.exception = Some(Box::new(Value::make_instance(
                "X::Multi::NoMatch".to_string(),
                attrs,
            )));
            return Err(err);
        }
        if self.has_role(name) {
            return Ok(Value::Pair(
                name.to_string(),
                Box::new(Value::array(args.to_vec())),
            ));
        }
        if name.starts_with("X::") {
            return Ok(Value::Package(name.to_string()));
        }

        // Check if multi candidates exist for this name (no matching arity/types)
        if self.has_multi_candidates(name) {
            let mut err =
                RuntimeError::new(format!("No matching candidates for proto sub: {}", name));
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::Str(format!(
                    "Cannot resolve caller {}; none of these signatures matches",
                    name
                )),
            );
            err.exception = Some(Box::new(Value::make_instance(
                "X::Multi::NoMatch".to_string(),
                attrs,
            )));
            return Err(err);
        }

        Err(RuntimeError::new(format!(
            "X::Undeclared::Symbols: Unknown function: {}",
            name
        )))
    }

    pub(super) fn call_infix_routine(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let is_set_op = matches!(
            op,
            "(-)"
                | "∖"
                | "(|)"
                | "∪"
                | "(&)"
                | "∩"
                | "(^)"
                | "⊖"
                | "(elem)"
                | "∈"
                | "(cont)"
                | "∋"
                | "(<=)"
                | "⊆"
                | "(>=)"
                | "⊇"
                | "(<)"
                | "⊂"
                | "(>)"
                | "⊃"
        );
        // 1-arg Iterable gets flattened (like +@foo slurpy), but not for set operators
        // which coerce their single argument to a QuantHash instead
        let args: Vec<Value> = if args.len() == 1 && !is_set_op {
            match &args[0] {
                Value::Array(items, ..) => items.to_vec(),
                _ => args.to_vec(),
            }
        } else {
            args.to_vec()
        };
        if op == "x" || op == "xx" {
            return self.call_repeat_infix(op, &args);
        }
        // Parser normalization fallback: `method foo { ... }` can appear as
        // InfixFunc(name="foo", left=BareWord("method"), right=[ArrayLiteral(...)]).
        // Treat this as a Method object value.
        if !args.is_empty() && args[0].to_string_value() == "method" {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::Str(op.to_string()));
            attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
            let mut sig_attrs = std::collections::HashMap::new();
            sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
            attrs.insert(
                "signature".to_string(),
                Value::make_instance("Signature".to_string(), sig_attrs),
            );
            attrs.insert("returns".to_string(), Value::Package("Mu".to_string()));
            attrs.insert("of".to_string(), Value::Package("Mu".to_string()));
            return Ok(Value::make_instance("Method".to_string(), attrs));
        }
        if args.is_empty() {
            return Ok(reduction_identity(op));
        }
        if args.len() == 1 {
            if op == "(|)" || op == "∪" {
                let is_lazy_union_input = |value: &Value| match value {
                    Value::LazyList(_) => true,
                    Value::GenericRange { start, end, .. } => {
                        let is_infinite = |bound: &Value| match bound {
                            Value::Num(n) => n.is_infinite(),
                            Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
                            Value::Mixin(inner, _) => match inner.as_ref() {
                                Value::Num(n) => n.is_infinite(),
                                Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
                                _ => false,
                            },
                            _ => false,
                        };
                        is_infinite(start) || is_infinite(end)
                    }
                    _ => false,
                };
                return match &args[0] {
                    Value::Instance { class_name, .. } if class_name == "Failure" => {
                        Err(RuntimeError::new("Exception"))
                    }
                    value if is_lazy_union_input(value) => {
                        Err(RuntimeError::new("X::Cannot::Lazy"))
                    }
                    Value::Bag(_) | Value::Mix(_) | Value::Set(_) => Ok(args[0].clone()),
                    other => Ok(Value::set(crate::runtime::utils::coerce_to_set(other))),
                };
            }
            if is_chain_comparison_op(op) {
                return Ok(Value::Bool(true));
            }
            if op == "~" {
                return Ok(Value::Str(crate::runtime::utils::coerce_to_str(&args[0])));
            }
            // Set operators with single arg: coerce to appropriate set type
            if matches!(op, "(-)" | "∖" | "(|)" | "∪" | "(&)" | "∩" | "(^)" | "⊖") {
                return Ok(coerce_value_to_quanthash(&args[0]));
            }
            return Ok(args[0].clone());
        }
        // Sequence operators: dispatch directly to eval_sequence
        match op {
            "..." | "...^" | "^..." | "^...^" => {
                let exclude_end = op == "...^" || op == "^...^";
                let exclude_start = op.starts_with('^');

                let mut result = if args.len() > 2 {
                    // Chained sequence: multiple waypoints
                    // args = [seed, waypoint1, waypoint2, ..., endpoint]
                    // Each waypoint can be a list: first element is endpoint for
                    // previous segment, whole list is seed for next segment.
                    self.eval_chained_sequence(&args, exclude_end)?
                } else {
                    let left = args[0].clone();
                    let right = args.last().cloned().unwrap_or(Value::Nil);
                    self.eval_sequence(left, right, exclude_end)?
                };

                if exclude_start {
                    // Remove the first element
                    match &result {
                        Value::Array(items, is_real) if !items.is_empty() => {
                            let new_items = items[1..].to_vec();
                            result = if *is_real {
                                Value::real_array(new_items)
                            } else {
                                Value::array(new_items)
                            };
                        }
                        Value::LazyList(ll) => {
                            let mut items = ll.cache.lock().unwrap().clone().unwrap_or_default();
                            if !items.is_empty() {
                                items.remove(0);
                            }
                            result = Value::LazyList(std::sync::Arc::new(crate::value::LazyList {
                                body: vec![],
                                env: std::collections::HashMap::new(),
                                cache: std::sync::Mutex::new(Some(items)),
                            }));
                        }
                        _ => {}
                    }
                }
                return Ok(result);
            }
            _ => {}
        }
        // Short-circuit operators need special handling
        match op {
            "andthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if !crate::runtime::types::value_is_defined(&acc) {
                        return Ok(acc);
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            "notandthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Value::Nil);
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            _ => {}
        }
        // Set operators: check for Failure and lazy list values
        if is_set_op {
            let is_lazy_value = |v: &Value| -> bool {
                match v {
                    Value::LazyList(_) => true,
                    Value::Range(_, end)
                    | Value::RangeExcl(_, end)
                    | Value::RangeExclStart(_, end)
                    | Value::RangeExclBoth(_, end) => *end == i64::MAX,
                    Value::GenericRange { end, .. } => match end.as_ref() {
                        Value::HyperWhatever => true,
                        Value::Num(n) => n.is_infinite() && n.is_sign_positive(),
                        _ => {
                            let n = end.to_f64();
                            n.is_infinite() && n.is_sign_positive()
                        }
                    },
                    _ => false,
                }
            };
            for arg in &args {
                if let Value::Instance { class_name, .. } = arg
                    && class_name == "Failure"
                {
                    return Err(RuntimeError::new("Exception"));
                }
                if is_lazy_value(arg) {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::Str("Cannot coerce a lazy list onto a Set".to_string()),
                    );
                    let ex = Value::make_instance("X::Cannot::Lazy".to_string(), attrs);
                    let mut err = RuntimeError::new("Cannot coerce a lazy list onto a Set");
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }
        let mut acc = args[0].clone();
        for rhs in &args[1..] {
            let pair_args = vec![acc.clone(), rhs.clone()];
            let infix_name = format!("infix:<{}>", op);
            if let Some(def) = self.resolve_function_with_types(&infix_name, &pair_args) {
                crate::trace::trace_log!("call", "call_infix_routine dispatch def: {}", infix_name);
                acc = self.call_function_def(&def, &pair_args)?;
                continue;
            }
            if let Some(callable) = self.env.get(&format!("&{}", infix_name)).cloned()
                && matches!(
                    callable,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Instance { .. } | Value::Mixin(..)
                )
            {
                crate::trace::trace_log!(
                    "call",
                    "call_infix_routine dispatch callable env: {}",
                    infix_name
                );
                acc = self.eval_call_on_value(callable, pair_args)?;
                continue;
            }
            crate::trace::trace_log!(
                "call",
                "call_infix_routine fallback reduce/eval: {}",
                infix_name
            );
            let mut lhs = acc.clone();
            let mut rhs = rhs.clone();
            if self.infix_uses_numeric_bridge(op) {
                lhs = self.coerce_infix_operand_numeric(lhs)?;
                rhs = self.coerce_infix_operand_numeric(rhs)?;
            }
            if let Ok(value) = Self::apply_reduction_op(op, &lhs, &rhs) {
                acc = value;
            } else {
                let (expr_op, expr_left, expr_right) =
                    if let Some(inner) = op.strip_prefix('R').filter(|inner| !inner.is_empty()) {
                        // Runtime autogen for callable reverse meta-ops (&infix:<Rop>):
                        // evaluate the inner operator with swapped operands.
                        (inner, rhs.clone(), acc)
                    } else {
                        (op, acc, rhs.clone())
                    };
                let expr_args = vec![expr_left.clone(), expr_right.clone()];
                let expr_infix_name = format!("infix:<{}>", expr_op);
                if let Some(def) = self.resolve_function_with_types(&expr_infix_name, &expr_args) {
                    crate::trace::trace_log!(
                        "call",
                        "call_infix_routine fallback dispatch def: {}",
                        expr_infix_name
                    );
                    acc = self.call_function_def(&def, &expr_args)?;
                    continue;
                }
                if let Some(callable) = self.env.get(&format!("&{}", expr_infix_name)).cloned()
                    && matches!(
                        callable,
                        Value::Sub(_)
                            | Value::WeakSub(_)
                            | Value::Instance { .. }
                            | Value::Mixin(..)
                    )
                {
                    crate::trace::trace_log!(
                        "call",
                        "call_infix_routine fallback dispatch callable env: {}",
                        expr_infix_name
                    );
                    acc = self.eval_call_on_value(callable, expr_args)?;
                    continue;
                }
                acc = self.eval_block_value(&[Stmt::Expr(Self::build_infix_expr(
                    expr_op, expr_left, expr_right,
                ))])?;
            }
        }
        Ok(acc)
    }

    pub(super) fn repeat_error(class_name: &str, message: String) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::Str(message.clone()));
        let ex = Value::make_instance(class_name.to_string(), attrs);
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        err
    }

    pub(super) fn parse_repeat_count(value: &Value) -> Result<Option<i64>, RuntimeError> {
        let mut current = value;
        while let Value::Mixin(inner, _) = current {
            current = inner;
        }
        match current {
            Value::Whatever => Ok(None),
            Value::Int(i) => Ok(Some(*i)),
            Value::BigInt(n) => {
                use num_traits::ToPrimitive;
                Ok(Some(n.to_i64().unwrap_or(i64::MAX)))
            }
            Value::Num(f) => {
                if f.is_nan() {
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        "Cannot convert NaN to Int".to_string(),
                    ));
                }
                if f.is_infinite() {
                    if f.is_sign_positive() {
                        return Ok(None);
                    }
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        "Cannot convert -Inf to Int".to_string(),
                    ));
                }
                Ok(Some(f.trunc() as i64))
            }
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n > 0 {
                        return Ok(None);
                    }
                    let msg = if *n < 0 {
                        "Cannot convert -Inf to Int"
                    } else {
                        "Cannot convert NaN to Int"
                    };
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        msg.to_string(),
                    ));
                }
                Ok(Some(n / d))
            }
            Value::FatRat(n, d) => {
                if d.is_zero() {
                    if n.is_positive() {
                        return Ok(None);
                    }
                    let msg = if n.is_negative() {
                        "Cannot convert -Inf to Int"
                    } else {
                        "Cannot convert NaN to Int"
                    };
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        msg.to_string(),
                    ));
                }
                Ok(Some((n / d).to_i64().unwrap_or(i64::MAX)))
            }
            Value::BigRat(n, d) => {
                if d.is_zero() {
                    if n.is_positive() {
                        return Ok(None);
                    }
                    let msg = if n.is_negative() {
                        "Cannot convert -Inf to Int"
                    } else {
                        "Cannot convert NaN to Int"
                    };
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        msg.to_string(),
                    ));
                }
                use num_traits::ToPrimitive;
                Ok(Some((n / d).to_i64().unwrap_or(i64::MAX)))
            }
            Value::Str(s) => {
                let parsed = s.trim().parse::<f64>().map_err(|_| {
                    Self::repeat_error(
                        "X::Str::Numeric",
                        format!("Cannot convert string '{}' to a number", s),
                    )
                })?;
                Self::parse_repeat_count(&Value::Num(parsed))
            }
            Value::Array(items, ..) => Ok(Some(items.len() as i64)),
            Value::Seq(items) => Ok(Some(items.len() as i64)),
            Value::LazyList(ll) => Ok(Some(
                ll.cache
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .as_ref()
                    .map_or(0usize, |v| v.len()) as i64,
            )),
            Value::Package(_) => Ok(Some(0)),
            _ => Ok(Some(0)),
        }
    }

    pub(super) fn make_repeat_lazy_cache(items: Vec<Value>) -> Value {
        Value::LazyList(std::sync::Arc::new(crate::value::LazyList {
            body: Vec::new(),
            env: std::collections::HashMap::new(),
            cache: std::sync::Mutex::new(Some(items)),
        }))
    }

    pub(super) fn repeat_lhs_once(&mut self, left: &Value) -> Result<Value, RuntimeError> {
        match left {
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                self.eval_call_on_value(left.clone(), Vec::new())
            }
            _ => Ok(left.clone()),
        }
    }

    pub(super) fn make_x_whatevercode(&self, left: Value) -> Value {
        let mut env = std::collections::HashMap::new();
        env.insert(
            "__mutsu_callable_type".to_string(),
            Value::Str("WhateverCode".to_string()),
        );
        let param = "__wc_0".to_string();
        let body = vec![Stmt::Expr(Expr::Binary {
            left: Box::new(Expr::Literal(left)),
            op: TokenKind::Ident("x".to_string()),
            right: Box::new(Expr::Var(param.clone())),
        })];
        Value::make_sub(
            self.current_package.clone(),
            "<whatevercode-x>".to_string(),
            vec![param],
            Vec::new(),
            body,
            false,
            env,
        )
    }

    pub(super) fn call_repeat_infix(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            if op == "xx" {
                return Err(Self::repeat_error(
                    "Exception",
                    "xx with no args throws".to_string(),
                ));
            }
            return Ok(reduction_identity(op));
        }
        if args.len() == 1 {
            return Ok(args[0].clone());
        }

        let mut acc = args[0].clone();
        for rhs in &args[1..] {
            match op {
                "x" => {
                    if let Value::Package(name) = rhs
                        && name == "Int"
                        && !self.warning_suppressed()
                    {
                        self.write_warn_to_stderr(&format!(
                            "Use of uninitialized value of type {} in numeric context",
                            name
                        ));
                    }
                    if matches!(rhs, Value::Whatever) {
                        acc = self.make_x_whatevercode(acc);
                        continue;
                    }
                    let Some(n_raw) = Self::parse_repeat_count(rhs)? else {
                        return Err(Self::repeat_error(
                            "X::Numeric::CannotConvert",
                            "Cannot convert Inf to Int".to_string(),
                        ));
                    };
                    let n = n_raw.max(0) as usize;
                    acc = Value::Str(crate::runtime::utils::coerce_to_str(&acc).repeat(n));
                }
                "xx" => {
                    const EAGER_LIMIT: usize = 10_000;
                    const LAZY_CACHE: usize = 4_096;
                    if let Value::Package(name) = rhs
                        && name == "Int"
                        && !self.warning_suppressed()
                    {
                        self.write_warn_to_stderr(&format!(
                            "Use of uninitialized value of type {} in numeric context",
                            name
                        ));
                    }
                    let count = Self::parse_repeat_count(rhs)?;
                    let (repeat, lazy) = match count {
                        Some(n) if n <= 0 => (0usize, false),
                        Some(n) if (n as usize) <= EAGER_LIMIT => (n as usize, false),
                        Some(n) => ((n as usize).min(LAZY_CACHE), true),
                        None => (LAZY_CACHE, true),
                    };
                    let mut items = Vec::with_capacity(repeat);
                    if let Value::Slip(slip_items) = &acc {
                        if slip_items.is_empty() {
                            items.extend(std::iter::repeat_n(Value::Nil, repeat));
                        } else {
                            for _ in 0..repeat {
                                items.extend(slip_items.iter().cloned());
                            }
                        }
                    } else {
                        for _ in 0..repeat {
                            items.push(self.repeat_lhs_once(&acc)?);
                        }
                    }
                    acc = if lazy {
                        Self::make_repeat_lazy_cache(items)
                    } else {
                        Value::Seq(std::sync::Arc::new(items))
                    };
                }
                _ => unreachable!(),
            }
        }
        Ok(acc)
    }

    pub(super) fn infix_uses_numeric_bridge(&self, op: &str) -> bool {
        matches!(
            op,
            "+" | "-"
                | "*"
                | "/"
                | "%"
                | "**"
                | "=="
                | "!="
                | "<"
                | ">"
                | "<="
                | ">="
                | "<=>"
                | "cmp"
                | "before"
                | "after"
                | "min"
                | "max"
        )
    }

    pub(super) fn coerce_infix_operand_numeric(
        &mut self,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if !matches!(value, Value::Instance { .. }) {
            return Ok(value);
        }
        if !(self.type_matches_value("Real", &value) || self.type_matches_value("Numeric", &value))
        {
            return Ok(value);
        }
        self.call_method_with_values(value.clone(), "Numeric", vec![])
            .or_else(|_| self.call_method_with_values(value.clone(), "Bridge", vec![]))
            .or(Ok(value))
    }

    pub(super) fn build_infix_expr(op: &str, left: Value, right: Value) -> Expr {
        if op == "∉" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(left)),
                    op: TokenKind::SetElem,
                    right: Box::new(Expr::Literal(right)),
                }),
            };
        }
        if op == "∌" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(left)),
                    op: TokenKind::SetCont,
                    right: Box::new(Expr::Literal(right)),
                }),
            };
        }
        if let Some(inner) = op.strip_prefix("![")
            && let Some(inner) = inner.strip_suffix(']')
        {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr(inner, left, right)),
            };
        }
        if let Some(inner) = op.strip_prefix('!')
            && !inner.is_empty()
        {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr(inner, left, right)),
            };
        }
        Expr::Binary {
            left: Box::new(Expr::Literal(left)),
            op: Self::infix_token(op),
            right: Box::new(Expr::Literal(right)),
        }
    }

    pub(super) fn infix_token(op: &str) -> TokenKind {
        match op {
            "+" => TokenKind::Plus,
            "-" => TokenKind::Minus,
            "*" | "×" => TokenKind::Star,
            "/" | "÷" => TokenKind::Slash,
            "**" => TokenKind::StarStar,
            "%" => TokenKind::Percent,
            "%%" => TokenKind::PercentPercent,
            "!%%" => TokenKind::BangPercentPercent,
            "==" => TokenKind::EqEq,
            "=" => TokenKind::EqEq,
            "!=" => TokenKind::BangEq,
            "===" | "=:=" => TokenKind::EqEqEq,
            "~" => TokenKind::Tilde,
            "+&" => TokenKind::BitAnd,
            "+|" => TokenKind::BitOr,
            "+^" => TokenKind::BitXor,
            "(|)" | "∪" => TokenKind::SetUnion,
            "(&)" | "∩" => TokenKind::SetIntersect,
            "(-)" | "∖" => TokenKind::SetDiff,
            "(^)" | "⊖" => TokenKind::SetSymDiff,
            "(elem)" | "∈" => TokenKind::SetElem,
            "(cont)" | "∋" => TokenKind::SetCont,
            "..." => TokenKind::DotDotDot,
            "...^" => TokenKind::DotDotDotCaret,
            ".." => TokenKind::DotDot,
            "<=>" => TokenKind::LtEqGt,
            "<" => TokenKind::Lt,
            ">" => TokenKind::Gt,
            "<=" => TokenKind::Lte,
            ">=" => TokenKind::Gte,
            "~~" => TokenKind::SmartMatch,
            "&&" => TokenKind::AndAnd,
            "||" => TokenKind::OrOr,
            "^^" => TokenKind::XorXor,
            "//" => TokenKind::SlashSlash,
            _ => TokenKind::Ident(op.to_string()),
        }
    }
}
