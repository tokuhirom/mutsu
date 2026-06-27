use super::*;

impl Interpreter {
    pub(super) fn exec_hyper_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
        writeback: bool,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let name = Self::const_str(code, name_idx).to_string();
        // QuantHash (Set/Bag/Mix) operands: reuse the plain-Hash hyper logic by
        // projecting each to a `key => weight` Hash, then convert the result
        // (and any write-back value) back to the original QuantHash type. The
        // result type/mutability follows whichever operand is a QuantHash.
        let quant_result = Self::quanthash_kind(&left).or_else(|| Self::quanthash_kind(&right));
        let (left, right) = if quant_result.is_some() {
            (
                Self::quanthash_to_hash(&left),
                Self::quanthash_to_hash(&right),
            )
        } else {
            (left, right)
        };
        // Resolve a concrete code-ref value when `name` refers to a lexical
        // `&name` variable (e.g. the `&op`/`&metaop` loop variables in
        // S03-metaops/infix.t). Such calls must go through the Interpreter closure
        // dispatch (`vm_call_on_value`) so sigilless `rw` binding and the
        // return value behave the same as a named-sub call; the interpreter
        // fallback mishandles both for mutating sigilless subs.
        let func_value: Option<Value> = {
            let bare = name.trim_start_matches('&');
            let mut found = None;
            for key in [format!("&{}", bare), bare.to_string()] {
                if let Some(v @ (Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. })) =
                    self.env().get(&key)
                {
                    found = Some(v.clone());
                    break;
                }
            }
            found
        };
        // Hash operands: apply the code-ref to each value key-by-key, mirroring
        // the dwim key-set semantics of `exec_hyper_op`.
        if matches!(&left, Value::Hash(..)) || matches!(&right, Value::Hash(..)) {
            let stack_before = self.stack.len();
            self.exec_hyper_func_op_hash(
                left,
                right,
                &name,
                func_value.as_ref(),
                dwim_left,
                dwim_right,
                writeback,
                compiled_fns,
            )?;
            if let Some((kind, mutable)) = quant_result {
                let pushed: Vec<Value> = self.stack.split_off(stack_before);
                for v in pushed {
                    self.stack.push(Self::hash_to_quanthash(v, kind, mutable));
                }
            }
            return Ok(());
        }
        let both_scalar = !Self::is_listy(&left) && !Self::is_listy(&right);
        let left_list = Interpreter::value_to_list(&left);
        let right_list = Interpreter::value_to_list(&right);
        let left_len = left_list.len();
        let right_len = right_list.len();
        if left_len == 0 && right_len == 0 {
            if writeback {
                self.stack.push(Value::array(Vec::new()));
            }
            self.stack.push(Value::array(Vec::new()));
            return Ok(());
        }
        let length_mismatch = || {
            RuntimeError::new(format!(
                "Non-dwimmy hyper operator: left has {} elements, right has {}",
                left_len, right_len
            ))
        };
        let result_len = if !dwim_left && !dwim_right {
            if left_len != right_len {
                return Err(length_mismatch());
            }
            left_len
        } else if dwim_left && dwim_right {
            std::cmp::max(left_len, right_len)
        } else if dwim_right {
            // Only the right side dwims: it is cycled up to the (fixed) left
            // length, so the right must not be longer than the left.
            if right_len > left_len {
                return Err(length_mismatch());
            }
            left_len
        } else {
            // Only the left side dwims: it is cycled up to the (fixed) right
            // length, so the left must not be longer than the right.
            if left_len > right_len {
                return Err(length_mismatch());
            }
            right_len
        };
        // When the left operand is a writable lvalue and the code-ref's first
        // parameter is bindable `rw` (sigilless `\a`, `$a is rw`, or `is raw`),
        // pass each left element by writable reference so a mutating code-ref
        // (e.g. `&[+=]`) writes back. We then collect the (possibly mutated)
        // left elements and leave them on the stack for the compiler-emitted
        // store into the lvalue.
        let func_writable = self.hyper_func_first_param_writable(
            &name,
            func_value.as_ref(),
            compiled_fns,
            &left_list,
            &right_list,
        );
        // An assignment meta-op (`&[+=]`) applied to a non-lvalue left operand
        // (a literal or literal list, e.g. `3 >>[&metaop]<< @a`) cannot write
        // back and dies, just like `3 += 1` would. (A sigilless user sub that
        // does not actually assign — e.g. `cst` — must NOT die here; it dies
        // naturally only if its body assigns to the read-only bound value.)
        if !writeback && Self::is_assign_metaop_ref(func_value.as_ref()) {
            return Err(RuntimeError::new(
                "Cannot modify an immutable value".to_string(),
            ));
        }
        let do_writeback = writeback && func_writable;
        // Look up the function by name (&name variable or compiled function)
        let mut results = Vec::with_capacity(result_len);
        let mut mutated_left: Vec<Value> =
            Vec::with_capacity(if do_writeback { result_len } else { 0 });
        for i in 0..result_len {
            let l = if left_len == 0 {
                Value::Int(0)
            } else {
                left_list[i % left_len].clone()
            };
            let r = if right_len == 0 {
                &Value::Int(0)
            } else {
                &right_list[i % right_len]
            };
            if do_writeback {
                let synth = format!("__mutsu_hyperfn_lv_{}", i);
                self.env_mut().insert(synth.clone(), l.clone());
                let varref =
                    crate::runtime::types::make_varref_value(synth.clone(), l.clone(), None);
                let call_args = vec![varref, r.clone()];
                let result = self.dispatch_hyper_func_call(
                    &name,
                    func_value.as_ref(),
                    call_args,
                    compiled_fns,
                )?;
                results.push(result);
                let updated = self.env().get(&synth).cloned().unwrap_or(l);
                self.env_mut().remove(&synth);
                mutated_left.push(updated);
            } else {
                let call_args = vec![l, r.clone()];
                let result = self.dispatch_hyper_func_call(
                    &name,
                    func_value.as_ref(),
                    call_args,
                    compiled_fns,
                )?;
                results.push(result);
            }
        }
        let left_is_array = matches!(&left, Value::Array(_, crate::value::ArrayKind::Array));
        let right_is_array = matches!(&right, Value::Array(_, crate::value::ArrayKind::Array));
        let wrap = |items: Vec<Value>| -> Value {
            if both_scalar && items.len() == 1 {
                items.into_iter().next().unwrap()
            } else if !left_is_array && !right_is_array {
                Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            } else {
                Value::real_array(items)
            }
        };
        // For writeback, push the mutated-left value first (consumed by the
        // store op) and leave the function results on top as the expression
        // value.
        if writeback {
            let writeback_val = if do_writeback {
                wrap(mutated_left)
            } else {
                // No mutation happened; restore the original left value so the
                // compiler-emitted store is a harmless no-op.
                left.clone()
            };
            self.stack.push(wrap(results));
            self.stack.push(writeback_val);
        } else {
            self.stack.push(wrap(results));
        }
        Ok(())
    }

    /// Hash variant of `exec_hyper_func_op`: apply a code-ref to hash values
    /// key-by-key. Supports hash-hash (matched by key, dwim selecting the key
    /// set) and hash-scalar broadcasting, plus `rw` write-back of the mutated
    /// left hash for `%a >>[&metaop]<< %b`.
    #[allow(clippy::too_many_arguments)]
    fn exec_hyper_func_op_hash(
        &mut self,
        left: Value,
        right: Value,
        name: &str,
        func_value: Option<&Value>,
        dwim_left: bool,
        dwim_right: bool,
        writeback: bool,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let probe_left: Vec<Value> = match &left {
            Value::Hash(m) => m.values().cloned().collect(),
            other => vec![other.clone()],
        };
        let probe_right: Vec<Value> = match &right {
            Value::Hash(m) => m.values().cloned().collect(),
            other => vec![other.clone()],
        };
        let func_writable = self.hyper_func_first_param_writable(
            name,
            func_value,
            compiled_fns,
            &probe_left,
            &probe_right,
        );
        // An assignment meta-op whose left operand is not a writable hash lvalue
        // (e.g. `3 <<[&metaop]>> %a`, where the scalar would be the mutated
        // element) cannot write back and therefore dies.
        if !(writeback && matches!(&left, Value::Hash(..)))
            && Self::is_assign_metaop_ref(func_value)
        {
            return Err(RuntimeError::new(
                "Cannot modify an immutable value".to_string(),
            ));
        }
        let do_writeback = writeback && matches!(&left, Value::Hash(..)) && func_writable;
        // Determine the key set and a per-key (left, right) value source.
        let (keys, la, ra, right_scalar) = match (&left, &right) {
            (Value::Hash(la), Value::Hash(ra)) => {
                let keys: Vec<String> = match (dwim_left, dwim_right) {
                    (false, false) => {
                        let mut ks: Vec<String> = la.keys().cloned().collect();
                        for k in ra.keys() {
                            if !la.contains_key(k) {
                                ks.push(k.clone());
                            }
                        }
                        ks
                    }
                    (true, true) => la.keys().filter(|k| ra.contains_key(*k)).cloned().collect(),
                    (false, true) => la.keys().cloned().collect(),
                    (true, false) => ra.keys().cloned().collect(),
                };
                (keys, Some(la.clone()), Some(ra.clone()), None)
            }
            (Value::Hash(la), _) => {
                // `%hash OP scalar`: the scalar (right) must be on a dwim side
                // to broadcast over the hash's keys; otherwise the lengths
                // mismatch (N vs 1) and it is a non-dwimmy error.
                if !dwim_right {
                    return Err(RuntimeError::new(
                        "Non-dwimmy hyper operator: cannot apply a hash against a non-dwim scalar"
                            .to_string(),
                    ));
                }
                let keys: Vec<String> = la.keys().cloned().collect();
                (keys, Some(la.clone()), None, Some(right.clone()))
            }
            (_, Value::Hash(ra)) => {
                // `scalar OP %hash`: the scalar (left) must be on a dwim side.
                if !dwim_left {
                    return Err(RuntimeError::new(
                        "Non-dwimmy hyper operator: cannot apply a non-dwim scalar against a hash"
                            .to_string(),
                    ));
                }
                let keys: Vec<String> = ra.keys().cloned().collect();
                (keys, None, Some(ra.clone()), None)
            }
            _ => unreachable!("exec_hyper_func_op_hash called without a hash operand"),
        };
        let identity = Value::Int(0);
        let mut result: std::collections::HashMap<String, Value> =
            std::collections::HashMap::with_capacity(keys.len());
        let mut mutated: std::collections::HashMap<String, Value> =
            std::collections::HashMap::with_capacity(if do_writeback { keys.len() } else { 0 });
        for key in keys {
            let l = match &la {
                Some(m) => m.get(&key).cloned().unwrap_or_else(|| identity.clone()),
                None => left.clone(),
            };
            let r = match (&ra, &right_scalar) {
                (Some(m), _) => m.get(&key).cloned().unwrap_or_else(|| identity.clone()),
                (None, Some(s)) => s.clone(),
                (None, None) => identity.clone(),
            };
            if do_writeback {
                let synth = format!("__mutsu_hyperfn_lvh_{}", key);
                self.env_mut().insert(synth.clone(), l.clone());
                let varref =
                    crate::runtime::types::make_varref_value(synth.clone(), l.clone(), None);
                let call_args = vec![varref, r];
                let v = self.dispatch_hyper_func_call(name, func_value, call_args, compiled_fns)?;
                let updated = self.env().get(&synth).cloned().unwrap_or(l);
                self.env_mut().remove(&synth);
                result.insert(key.clone(), v);
                mutated.insert(key, updated);
            } else {
                let call_args = vec![l, r];
                let v = self.dispatch_hyper_func_call(name, func_value, call_args, compiled_fns)?;
                result.insert(key, v);
            }
        }
        let result_hash = Value::Hash(Value::hash_arc(result));
        if writeback {
            let writeback_val = if do_writeback {
                Value::Hash(Value::hash_arc(mutated))
            } else {
                left.clone()
            };
            self.stack.push(result_hash);
            self.stack.push(writeback_val);
        } else {
            self.stack.push(result_hash);
        }
        Ok(())
    }

    /// True when the resolved code-ref is a built-in assignment meta-operator
    /// (`&[+=]`, `&[~=]`, ...) — a `Routine` named `infix:<op=>`. Such a
    /// routine unconditionally mutates its first argument, so applying it to a
    /// non-lvalue is a hard error.
    fn is_assign_metaop_ref(func_value: Option<&Value>) -> bool {
        let Some(Value::Routine { name, .. }) = func_value else {
            return false;
        };
        let Some(inner) = name
            .resolve()
            .strip_prefix("infix:<")
            .and_then(|s| s.strip_suffix('>'))
            .map(str::to_string)
        else {
            return false;
        };
        inner.ends_with('=')
            && inner.len() > 1
            && !matches!(
                inner.as_str(),
                "==" | "!=" | "<=" | ">=" | "===" | "!==" | "=:=" | "!=:=" | "<=>"
            )
    }

    /// Dispatch a single per-element call of a hyper function-op. Lexical
    /// code-refs (`func_value`) go through the Interpreter closure dispatch; named subs
    /// go through the compiled-first path.
    fn dispatch_hyper_func_call(
        &mut self,
        name: &str,
        func_value: Option<&Value>,
        call_args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        if let Some(fv) = func_value {
            self.vm_call_on_value(fv.clone(), call_args, Some(compiled_fns))
        } else {
            self.call_function_compiled_first(name, call_args, compiled_fns)
        }
    }

    /// Determine whether the code-ref named `name` binds its first positional
    /// parameter in a way that can write back to the caller (sigilless raw,
    /// `is rw`, or `is raw`). Used to decide whether a hyper function-op should
    /// pass left elements by writable reference.
    fn hyper_func_first_param_writable(
        &mut self,
        name: &str,
        func_value: Option<&Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        left_list: &[Value],
        right_list: &[Value],
    ) -> bool {
        let probe = vec![
            left_list.first().cloned().unwrap_or(Value::Int(0)),
            right_list.first().cloned().unwrap_or(Value::Int(0)),
        ];
        fn writable(pd: &crate::ast::ParamDef) -> bool {
            pd.sigilless || pd.traits.iter().any(|t| t == "rw" || t == "raw")
        }
        // A code-ref held in a lexical `&name` variable: inspect its SubData.
        // An assignment meta-operator routine (`&[+=]`, `&[~=]`, ...) mutates
        // its first argument even though its synthesized signature does not
        // carry an `rw` marker. Detect it by name so the element is passed by
        // writable reference.
        if let Some(Value::Routine { name: rname, .. }) = func_value
            && let Some(inner) = rname
                .resolve()
                .strip_prefix("infix:<")
                .and_then(|s| s.strip_suffix('>'))
            && inner.ends_with('=')
            && inner.len() > 1
            && !matches!(
                inner,
                "==" | "!=" | "<=" | ">=" | "===" | "!==" | "=:=" | "!=:=" | "<=>"
            )
        {
            return true;
        }
        let sub = match func_value {
            Some(Value::Sub(data)) => Some(data.clone()),
            Some(Value::WeakSub(weak)) => weak.upgrade(),
            _ => None,
        };
        if let Some(data) = sub {
            return data.param_defs.first().map(writable).unwrap_or(false);
        }
        if let Some(cf) = self.find_compiled_function(compiled_fns, name, &probe) {
            return cf.param_defs.first().map(writable).unwrap_or(false);
        }
        if let Some(def) = loan_env!(self, resolve_function_with_types(name, &probe)) {
            return def.param_defs.first().map(writable).unwrap_or(false);
        }
        false
    }
}
