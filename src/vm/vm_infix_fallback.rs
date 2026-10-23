use super::*;

impl Interpreter {
    pub(super) fn call_infix_fallback(
        &mut self,
        name: &str,
        infix_name: Option<&str>,
        call_args: Vec<Value>,
        code: &CompiledCode,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        // When an infix operator is called with a single Iterable argument,
        // flatten it into elements (like a +@foo slurpy) and reduce over them.
        let call_args = if call_args.len() == 1 {
            match call_args[0].view() {
                ValueView::Hash(map) => {
                    // Break Hash into Pairs
                    map.iter()
                        .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                        .collect::<Vec<_>>()
                }
                ValueView::Array(items, ..) => items.iter().cloned().collect(),
                _ => call_args,
            }
        } else {
            call_args
        };
        // A named argument (a trailing adverb, `1 + 2 :adverb`) reaches a core
        // operator no candidate of which declares a named parameter: rakudo
        // rejects the call instead of ignoring the adverb.
        if let Some(op_name) = infix_name
            && call_args
                .iter()
                .skip(1)
                .any(|arg| matches!(arg.view(), ValueView::Pair(..)))
            && !matches!(
                crate::runtime::native_infix_dispatch::core_infix_candidates(op_name),
                crate::runtime::native_infix_dispatch::CoreInfixCandidates::Shadowing
            )
            && !self.user_infix_override(op_name)
        {
            return Err(
                crate::runtime::incdec_rw_sub::core_infix_named_argument_error(op_name, &call_args),
            );
        }
        if call_args.len() == 2
            && infix_name.is_none_or(|op| !self.is_candidate_less_proto(op))
            && let Some(value) = self.core_infix_values(name, &call_args[0], &call_args[1], code)?
        {
            return Ok(value);
        }
        if call_args.len() == 2 {
            match name {
                "(elem)" | "∈" => {
                    return self.eval_binary_with_junctions(
                        call_args[0].clone(),
                        call_args[1].clone(),
                        Self::eval_set_elem_values,
                    );
                }
                "(cont)" | "∋" => {
                    return self.eval_binary_with_junctions(
                        call_args[0].clone(),
                        call_args[1].clone(),
                        Self::eval_set_cont_values,
                    );
                }
                _ => {}
            }
        }
        // `cmp` is normally a dedicated opcode, but importing a user-defined
        // `infix:<cmp>` causes the compiler to retain the generic infix-call
        // path so that its multi candidates can participate. Once those
        // candidates decline, use the same native structural implementation
        // as the dedicated opcode; the generic reduction only knows about
        // native Range variants and would order an `is Range` instance by its
        // representation instead of its bounds.
        if call_args.len() == 2 && name == "cmp" {
            let left = Self::unwrap_var_ref_value(call_args[0].clone())
                .deref_container()
                .deitemize_element();
            let right = Self::unwrap_var_ref_value(call_args[1].clone())
                .deref_container()
                .deitemize_element();
            return self.cmp_value(left, right);
        }
        if call_args.len() == 2
            && let Some(result) = self.range_instance_arithmetic(
                name,
                &Self::unwrap_var_ref_value(call_args[0].clone()),
                &Self::unwrap_var_ref_value(call_args[1].clone()),
            )?
        {
            return Ok(result);
        }
        if call_args.len() >= 2 {
            // `apply_reduction_op` is always a NATIVE reduction (`+`, `mod`,
            // junctions, ...) — never a call that could bind an `is rw`
            // parameter — so unwrap the `WrapVarRef` tag
            // `compile_expr_infix_func` attaches to a variable operand for
            // that possibility (already tried and declined by every caller
            // of this function). Left wrapped, the native op's own type
            // matching (e.g. `arith_mod`) sees an unmatched `VarRef` view and
            // silently defaults instead of computing the real value.
            //
            // A `Proxy` operand (an `is rw` method's answer) is FETCHed here:
            // the native op reads plain values, and the operand only reaches
            // this point raw because user `multi infix:<op>` candidates with
            // `is rw` parameters were tried first and declined.
            let mut acc =
                self.fetch_proxy_for_store(Self::unwrap_var_ref_value(call_args[0].clone()))?;
            let mut reduced = true;
            for rhs in &call_args[1..] {
                let rhs = self.fetch_proxy_for_store(Self::unwrap_var_ref_value(rhs.clone()))?;
                match crate::runtime::Interpreter::apply_reduction_op(name, &acc, &rhs) {
                    Ok(value) => acc = value,
                    Err(_) => {
                        reduced = false;
                        break;
                    }
                }
            }
            if reduced {
                return Ok(acc);
            }
        }
        // Past this point every remaining candidate is looked up by name
        // (`call_user_routine_direct`, `call_function_compiled_first`, the
        // core routine below) — including a purely custom `infix:<op>` sub
        // that `try_user_infix` never saw because it only competes against a
        // CORE-recognized operator name. Such a sub may still declare an
        // `is rw` parameter (French's `&infix:<plus_égal>`), so `call_args`
        // stays wrapped here; only the native reduction above is unwrapped.
        if let Some(op_name) = infix_name
            && let Ok(v) = loan_env!(self, call_user_routine_direct(op_name, call_args.clone()))
        {
            return Ok(v);
        }
        if Self::should_retry_with_canonical_infix_name(name)
            && let Some(op_name) = infix_name
            && let Ok(v) =
                self.call_function_compiled_first(op_name, call_args.clone(), compiled_fns)
        {
            return Ok(v);
        }
        // Everything below reaches a core routine of the operator's *bare*
        // name — the core `sub cross`, not a core `&infix:<cross>`, which
        // rakudo does not have. A user declaration of such a name is a fresh
        // lexical routine that SHADOWS the operator outright (ADR-0093), so
        // once the user's own candidates have declined the call there is
        // nothing left to fall through to: the answer is X::Multi::NoMatch
        // naming only their signatures, exactly as rakudo reports it.
        // Operators rakudo really does declare (`+`, `minmax`, ...) classify as
        // `Modelled`/`Unmodelled` and keep their core candidate, per ADR-0071.
        // `has_multi_function_cached` is the same gate ADR-0071 rule 5 uses: a
        // plain `sub infix:<op>` is a lexical shadow rather than a candidate,
        // so it either resolved above and replaced the operator outright, or it
        // is not visible here at all. Only a `multi` can reach this point
        // having declined the call — and requiring one is also what keeps a
        // module-private `sub infix:<notthere>` (roast
        // S06-operator-overloading/imported-subs.t, declared but never
        // exported) falling through to rakudo's own answer for it,
        // "Two terms in a row", instead of being reported as a candidate set
        // the caller cannot see.
        if let Some(op_name) = infix_name
            && matches!(
                crate::runtime::native_infix_dispatch::core_infix_candidates(op_name),
                crate::runtime::native_infix_dispatch::CoreInfixCandidates::Shadowing
            )
            && self.user_infix_override(op_name)
            && self.has_multi_function_cached(op_name)
        {
            return Err(self.multi_no_match_error(op_name, &call_args));
        }
        // A user `proto sub infix:<op>` with no candidate: the operator parsed
        // (the proto alone registers it), but nothing can answer the call
        // (mutsu#10531).
        if let Some(op_name) = infix_name
            && self.is_candidate_less_proto(op_name)
        {
            return Err(self.multi_no_candidates_error(op_name, &call_args));
        }
        // A user `proto sub infix:<op>` whose body answers the call itself
        // (no `{*}` dispatch) is an ordinary routine (mutsu#10696); neither
        // the direct routine lookup above nor the by-name call below resolves
        // a proto, so run it here.
        // TODO: compile to bytecode (reuses the interpreter's proto runner).
        if let Some(op_name) = infix_name
            && let Some(proto) = self.resolve_proto_function(op_name)
        {
            return self.call_proto_function(op_name, &proto, &call_args);
        }
        match self.call_function_compiled_first(name, call_args.clone(), compiled_fns) {
            Ok(v) => Ok(v),
            Err(err) => {
                // `for foo-bar() -> ...` currently produces an infix AST fallback call.
                // If `foo-bar` has explicit empty signature `:()`, retry zero-arg dispatch.
                let is_empty_sig_rejection = err
                    .message
                    .starts_with("Too many positionals passed; expected 0 arguments but got more")
                    || err.message.starts_with("Unexpected named argument '");
                if is_empty_sig_rejection {
                    if let Ok(v) = self.call_function_compiled_first(name, Vec::new(), compiled_fns)
                    {
                        return Ok(v);
                    }
                    if Self::should_retry_with_canonical_infix_name(name)
                        && let Some(op_name) = infix_name
                        && let Ok(v) =
                            self.call_function_compiled_first(op_name, Vec::new(), compiled_fns)
                    {
                        return Ok(v);
                    }
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, Vec::new(), None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.env().get(&bare_env_name).cloned() {
                        return self.vm_call_on_value(code_val, Vec::new(), None);
                    }
                    let method_name = name
                        .strip_prefix("infix:<")
                        .and_then(|s| s.strip_suffix('>'))
                        .unwrap_or(name);
                    if !method_name.is_empty()
                        && !call_args.is_empty()
                        && call_args[0].to_string_value() == "method"
                    {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("name".to_string(), Value::str(method_name.to_string()));
                        attrs.insert("is_dispatcher".to_string(), Value::FALSE);
                        let mut sig_attrs = std::collections::HashMap::new();
                        sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
                        attrs.insert(
                            "signature".to_string(),
                            Value::make_instance(Symbol::intern("Signature"), sig_attrs),
                        );
                        attrs.insert("returns".to_string(), Value::package(Symbol::intern("Mu")));
                        attrs.insert("of".to_string(), Value::package(Symbol::intern("Mu")));
                        return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
                    }
                    Err(RuntimeError::syntax_confused_with_reason(
                        "Two terms in a row",
                    ))
                } else {
                    if let Some(op_name) = infix_name {
                        let op_env_name = format!("&{}", op_name);
                        if let Some(code_val) = self.env().get(&op_env_name).cloned() {
                            return self.vm_call_on_value(code_val, call_args, None);
                        }
                    }
                    let bare_env_name = format!("&{}", name);
                    if let Some(code_val) = self.env().get(&bare_env_name).cloned() {
                        self.vm_call_on_value(code_val, call_args, None)
                    } else {
                        let method_name = name
                            .strip_prefix("infix:<")
                            .and_then(|s| s.strip_suffix('>'))
                            .unwrap_or(name);
                        if !method_name.is_empty()
                            && !call_args.is_empty()
                            && call_args[0].to_string_value() == "method"
                        {
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert("name".to_string(), Value::str(method_name.to_string()));
                            attrs.insert("is_dispatcher".to_string(), Value::FALSE);
                            let mut sig_attrs = std::collections::HashMap::new();
                            sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
                            attrs.insert(
                                "signature".to_string(),
                                Value::make_instance(Symbol::intern("Signature"), sig_attrs),
                            );
                            attrs.insert(
                                "returns".to_string(),
                                Value::package(Symbol::intern("Mu")),
                            );
                            attrs.insert("of".to_string(), Value::package(Symbol::intern("Mu")));
                            return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
                        }
                        Err(RuntimeError::syntax_confused_with_reason(
                            "Two terms in a row",
                        ))
                    }
                }
            }
        }
    }

    /// Apply the core Range arithmetic candidate to an `is Range` instance.
    /// Native Range values already reach the arithmetic builtins directly, but
    /// a user-defined infix family (Math::Interval's `Rangy` operators are the
    /// representative case) routes a non-matching mixed operand through this
    /// generic fallback. Materialize the inherited/delegated bounds first so
    /// the existing Range arithmetic remains the single implementation.
    pub(crate) fn range_instance_arithmetic(
        &mut self,
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let as_range = |this: &mut Self, value: &Value| {
            let ValueView::Instance { class_name, .. } = value.view() else {
                return Ok(None);
            };
            if !this
                .class_mro(&class_name.resolve())
                .iter()
                .any(|name| name.as_str() == "Range")
            {
                return Ok(None);
            }
            let Some((min, max, excludes_min, excludes_max)) =
                this.range_parts_via_dispatch(value)?
            else {
                return Ok(None);
            };
            Ok(Some(Value::generic_range(
                min,
                max,
                excludes_min,
                excludes_max,
            )))
        };
        let left_range = as_range(self, left)?;
        let right_range = as_range(self, right)?;
        let result = match op {
            "+" => left_range
                .map(|range| crate::builtins::arith_add(range, right.clone()))
                .or_else(|| {
                    right_range.map(|range| crate::builtins::arith_add(range, left.clone()))
                })
                .transpose()?,
            "-" => left_range.map(|range| crate::builtins::arith_sub(range, right.clone())),
            "*" => left_range
                .map(|range| crate::builtins::arith_mul(range, right.clone()))
                .or_else(|| {
                    right_range.map(|range| crate::builtins::arith_mul(range, left.clone()))
                }),
            "/" => left_range
                .map(|range| crate::builtins::arith_div(range, right.clone()))
                .transpose()?,
            _ => None,
        };
        Ok(result)
    }
}
