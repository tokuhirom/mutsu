use super::*;

/// Check if a type name is a core Raku type that should always be accepted.
fn is_core_raku_type(name: &str) -> bool {
    matches!(
        name,
        "Mu" | "Any"
            | "Cool"
            | "Junction"
            | "Pair"
            | "List"
            | "Seq"
            | "Range"
            | "Map"
            | "Slip"
            | "Set"
            | "Bag"
            | "Mix"
            | "SetHash"
            | "BagHash"
            | "MixHash"
            | "Capture"
            | "Signature"
            | "Parameter"
            | "Block"
            | "Code"
            | "Sub"
            | "Method"
            | "Routine"
            | "Regex"
            | "Match"
            | "Grammar"
            | "IO"
            | "Proc"
            | "Promise"
            | "Supply"
            | "Channel"
            | "Instant"
            | "Duration"
            | "Version"
            | "Exception"
            | "Failure"
            | "Nil"
            | "Int"
            | "Num"
            | "Rat"
            | "Complex"
            | "Str"
            | "Bool"
            | "Whatever"
            | "HyperWhatever"
            | "WhateverCode"
            | "Stash"
            | "Scalar"
            | "Numeric"
            | "Real"
            | "Stringy"
            | "Callable"
            | "Positional"
            | "Associative"
            | "Iterable"
            | "Iterator"
            | "Dateish"
            | "Date"
            | "DateTime"
            | "Buf"
            | "Blob"
            | "utf8"
    )
}

impl VM {
    fn array_elements_match_constraint(&mut self, constraint: &str, value: &Value) -> bool {
        match value {
            Value::Array(items, ..) => items
                .iter()
                .all(|item| self.array_elements_match_constraint(constraint, item)),
            Value::Nil => true,
            _ => self.interpreter.type_matches_value(constraint, value),
        }
    }

    pub(super) fn exec_make_range_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::Range(*a, *b),
            (Value::Int(a), Value::Num(b)) if b.is_infinite() && b.is_sign_positive() => {
                Value::Range(*a, i64::MAX)
            }
            (Value::Num(a), Value::Int(b)) if a.is_infinite() && a.is_sign_negative() => {
                Value::Range(i64::MIN, *b)
            }
            (Value::Str(a), Value::Str(b)) => Value::GenericRange {
                start: Box::new(Value::Str(a.clone())),
                end: Box::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            (l, r) if l.is_numeric() && r.is_numeric() => Value::GenericRange {
                start: Box::new(l.clone()),
                end: Box::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (Value::Str(a), r) if r.is_numeric() => Value::GenericRange {
                start: Box::new(Value::Str(a.clone())),
                end: Box::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (l, Value::Str(b)) if l.is_numeric() => Value::GenericRange {
                start: Box::new(l.clone()),
                end: Box::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            // WhateverCode endpoint: e.g. 0..*-2
            (_, Value::Sub(_)) | (Value::Sub(_), _) => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: false,
                excl_end: false,
            },
            _ => Value::Nil,
        };
        self.stack.push(result);
    }

    pub(super) fn exec_make_range_excl_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExcl(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Box::new(left.clone()),
                end: Box::new(right.clone()),
                excl_start: false,
                excl_end: true,
            },
            _ => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: false,
                excl_end: true,
            },
        };
        self.stack.push(result);
    }

    pub(super) fn exec_make_range_excl_start_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExclStart(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Box::new(left.clone()),
                end: Box::new(right.clone()),
                excl_start: true,
                excl_end: false,
            },
            _ => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: true,
                excl_end: false,
            },
        };
        self.stack.push(result);
    }

    pub(super) fn exec_make_range_excl_both_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExclBoth(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Box::new(left.clone()),
                end: Box::new(right.clone()),
                excl_start: true,
                excl_end: true,
            },
            _ => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: true,
                excl_end: true,
            },
        };
        self.stack.push(result);
    }

    pub(super) fn exec_num_coerce_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        if matches!(
            &val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            return Err(RuntimeError::new(
                "Cannot resolve caller Numeric(Sub:D: ); none of these signatures matches:\n    (Mu:U \\v: *%_)",
            ));
        }
        // If the value is an Instance, try calling the Numeric method
        if let Value::Instance { .. } = &val
            && let Ok(result) =
                self.interpreter
                    .call_method_with_values(val.clone(), "Numeric", vec![])
        {
            self.stack.push(result);
            return Ok(());
        }
        // Force LazyList before numeric coercion so we can count elements
        let val = if let Value::LazyList(ll) = &val {
            let items = self.interpreter.force_lazy_list_bridge(ll)?;
            Value::Seq(std::sync::Arc::new(items))
        } else {
            val
        };
        let result = crate::runtime::utils::coerce_to_numeric(val);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_coerce_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // If the value is an Instance, try calling the Stringy method
        if let Value::Instance { .. } = &val
            && let Ok(result) =
                self.interpreter
                    .call_method_with_values(val.clone(), "Stringy", vec![])
        {
            self.stack.push(result);
            return Ok(());
        }
        self.stack
            .push(Value::Str(crate::runtime::utils::coerce_to_str(&val)));
        Ok(())
    }

    pub(super) fn exec_upto_range_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let n = match val {
            Value::Int(i) => i,
            _ => 0,
        };
        self.stack.push(Value::RangeExcl(0, n));
    }

    pub(super) fn exec_pre_increment_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        let new_val = match val {
            Value::Int(i) => Value::Int(i + 1),
            Value::Bool(_) => Value::Bool(true),
            Value::Rat(n, d) => make_rat(n + d, d),
            _ => Value::Int(1),
        };
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
    }

    pub(super) fn exec_pre_decrement_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        let new_val = match val {
            Value::Int(i) => Value::Int(i - 1),
            Value::Bool(_) => Value::Bool(false),
            Value::Rat(n, d) => make_rat(n - d, d),
            _ => Value::Int(-1),
        };
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
    }

    pub(super) fn exec_get_capture_var_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self
            .interpreter
            .env()
            .get(name)
            .cloned()
            .unwrap_or(Value::Nil);
        self.stack.push(val);
    }

    pub(super) fn exec_get_code_var_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self.interpreter.resolve_code_var(name);
        self.stack.push(val);
    }

    pub(super) fn exec_indirect_code_lookup_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let func_name = Self::const_str(code, name_idx).to_string();
        // Pop the package name from the stack (result of evaluating the package expr)
        let package = self.stack.pop().unwrap_or(Value::Nil);
        // Construct a qualified name: "SETTING::OUTER::...::not"
        // resolve_code_var will strip pseudo-package prefixes and resolve to builtin
        let pkg_str = package.to_string_value();
        let qualified = if pkg_str.is_empty() {
            func_name
        } else {
            format!("{}::{}", pkg_str, func_name)
        };
        let val = self.interpreter.resolve_code_var(&qualified);
        self.stack.push(val);
    }

    pub(super) fn exec_assign_expr_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = match &code.constants[name_idx as usize] {
            Value::Str(s) => s.clone(),
            _ => unreachable!("AssignExpr name must be a string constant"),
        };
        if name.starts_with('&') && !name.contains("::") {
            let bare = name.trim_start_matches('&');
            let has_variable_slot = self.interpreter.env().contains_key(&name);
            let is_routine_symbol = self.interpreter.has_function(bare)
                || self.interpreter.has_multi_function(bare)
                || self.interpreter.has_proto(bare)
                || self.interpreter.resolve_token_defs(bare).is_some()
                || self.interpreter.has_proto_token(bare);
            if is_routine_symbol && !has_variable_slot {
                return Err(RuntimeError::new("X::Assignment::RO"));
            }
        }
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let val = if name.starts_with('%') {
            runtime::coerce_to_hash(raw_val)
        } else if name.starts_with('@') {
            runtime::coerce_to_array(raw_val)
        } else {
            raw_val
        };
        // When assigning Nil to a typed variable, reset to the type object
        let val = if matches!(val, Value::Nil) && !name.starts_with('@') && !name.starts_with('%') {
            if let Some(constraint) = self.interpreter.var_type_constraint(&name) {
                Value::Package(constraint)
            } else {
                val
            }
        } else {
            val
        };
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.interpreter.env().get(&readonly_key),
            Some(Value::Bool(true))
        ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::new("X::Assignment::RO"));
        }
        self.update_local_if_exists(code, &name, &val);
        self.set_env_with_main_alias(&name, val.clone());
        if let Some(alias_name) = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.clone())
            } else {
                None
            }
        }) {
            self.update_local_if_exists(code, &alias_name, &val);
            self.interpreter.env_mut().insert(alias_name, val.clone());
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr), val.clone());
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_wrap_var_ref_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let name = Self::const_str(code, name_idx).to_string();
        let mut named = std::collections::HashMap::new();
        named.insert("__mutsu_varref_name".to_string(), Value::Str(name));
        named.insert("__mutsu_varref_value".to_string(), value);
        self.stack.push(Value::Capture {
            positional: Vec::new(),
            named,
        });
    }

    pub(super) fn exec_get_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        let val = if let Some(Value::Hash(env_hash)) = self.interpreter.env().get("%*ENV") {
            env_hash.get(key).cloned().unwrap_or_else(|| {
                std::env::var_os(key)
                    .map(|v| Value::Str(v.to_string_lossy().to_string()))
                    .unwrap_or(Value::Nil)
            })
        } else if let Some(value) = std::env::var_os(key) {
            Value::Str(value.to_string_lossy().to_string())
        } else {
            Value::Nil
        };
        self.stack.push(val);
    }

    pub(super) fn exec_exists_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        let exists = if let Some(Value::Hash(env_hash)) = self.interpreter.env().get("%*ENV") {
            env_hash.contains_key(key) || std::env::var_os(key).is_some()
        } else {
            std::env::var_os(key).is_some()
        };
        self.stack.push(Value::Bool(exists));
    }

    pub(super) fn exec_exists_expr_op(&mut self) {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        self.stack.push(Value::Bool(val.truthy()));
    }

    pub(super) fn exec_reduction_op(
        &mut self,
        code: &CompiledCode,
        op_idx: u32,
    ) -> Result<(), RuntimeError> {
        let op = Self::const_str(code, op_idx).to_string();
        // Support scan/meta reduction [\op] and negated forms like [!after].
        let (scan, op_no_scan) = if let Some(stripped) = op.strip_prefix('\\') {
            (true, stripped.to_string())
        } else {
            (false, op.clone())
        };
        // Only treat '!' as negation prefix when the remaining part is a known
        // operator (e.g. [!after], [!==], [!eqv]).  Operators like '!=' are their
        // own base operators and must not be split.
        const KNOWN_BASE_OPS: &[&str] = &[
            "+", "-", "*", "/", "%", "~", "||", "&&", "//", "%%", "**", "^^", "+&", "+|", "+^",
            "+<", "+>", "~&", "~|", "~^", "~<", "~>", "?&", "?|", "?^", "==", "!=", "<", ">", "<=",
            ">=", "<=>", "===", "=:=", "=>", "eqv", "eq", "ne", "lt", "gt", "le", "ge", "leg",
            "cmp", "~~", "min", "max", "gcd", "lcm", "and", "or", "not", ",", "after", "before",
            "X", "Z", "x", "xx", "&", "|", "^", "o", "∘",
        ];
        let (negate, base_op) = if let Some(stripped) = op_no_scan.strip_prefix('!')
            && KNOWN_BASE_OPS.contains(&stripped)
        {
            (true, stripped.to_string())
        } else {
            (false, op_no_scan)
        };
        let base_op = if base_op == "∘" {
            "o".to_string()
        } else {
            base_op
        };
        let list_value = self.stack.pop().unwrap_or(Value::Nil);
        let mut list = if let Value::LazyList(ref ll) = list_value {
            self.interpreter.force_lazy_list_bridge(ll)?
        } else {
            runtime::value_to_list(&list_value)
        };
        if list.iter().any(|v| matches!(v, Value::Slip(_))) {
            let mut flattened = Vec::new();
            for item in list {
                if let Value::Slip(items) = item {
                    flattened.extend(items.iter().cloned());
                } else {
                    flattened.push(item);
                }
            }
            list = flattened;
        }
        if base_op == "," {
            if scan {
                let mut out = Vec::with_capacity(list.len());
                let mut prefix = Vec::new();
                for item in list {
                    prefix.push(item);
                    out.push(Value::array(prefix.clone()));
                }
                self.stack.push(Value::Seq(std::sync::Arc::new(out)));
            } else {
                self.stack.push(Value::array(list));
            }
            return Ok(());
        }
        if scan {
            if list.is_empty() {
                self.stack.push(Value::Seq(std::sync::Arc::new(Vec::new())));
                return Ok(());
            }
            let mut acc = list[0].clone();
            let mut out = Vec::with_capacity(list.len());
            out.push(acc.clone());
            for item in &list[1..] {
                let v = self.eval_reduction_operator_values(&base_op, &acc, item)?;
                acc = if negate { Value::Bool(!v.truthy()) } else { v };
                out.push(acc.clone());
            }
            self.stack.push(Value::Seq(std::sync::Arc::new(out)));
            return Ok(());
        }
        if list.is_empty() {
            self.stack.push(runtime::reduction_identity(&base_op));
        } else {
            let is_comparison = runtime::is_chain_comparison_op(&base_op);
            if is_comparison {
                let mut result = true;
                for i in 0..list.len() - 1 {
                    let v =
                        self.eval_reduction_operator_values(&base_op, &list[i], &list[i + 1])?;
                    let truthy = if negate { !v.truthy() } else { v.truthy() };
                    if !truthy {
                        result = false;
                        break;
                    }
                }
                self.stack.push(Value::Bool(result));
            } else {
                if base_op == "o" {
                    let mut acc = list[0].clone();
                    for item in &list[1..] {
                        acc = self.interpreter.compose_callables(acc, item.clone());
                    }
                    self.stack.push(acc);
                    return Ok(());
                }
                let acc = if base_op == "=>" {
                    let mut acc = list.last().cloned().unwrap_or(Value::Nil);
                    for item in list[..list.len() - 1].iter().rev() {
                        let v = self.eval_reduction_operator_values(&base_op, item, &acc)?;
                        acc = if negate { Value::Bool(!v.truthy()) } else { v };
                    }
                    acc
                } else {
                    let mut acc = list[0].clone();
                    for item in &list[1..] {
                        let v = self.eval_reduction_operator_values(&base_op, &acc, item)?;
                        acc = if negate { Value::Bool(!v.truthy()) } else { v };
                    }
                    acc
                };
                self.stack.push(acc);
            }
        }
        Ok(())
    }

    pub(super) fn exec_routine_magic_op(&mut self) -> Result<(), RuntimeError> {
        if let Some((package, name)) = self.interpreter.routine_stack_top() {
            self.stack.push(Value::Routine {
                package: package.clone(),
                name: name.clone(),
                is_regex: false,
            });
        } else {
            return Err(RuntimeError::new("X::Undeclared::Symbols"));
        }
        Ok(())
    }

    pub(super) fn exec_block_magic_op(&mut self) -> Result<(), RuntimeError> {
        if let Some(val) = self.interpreter.block_stack_top().cloned() {
            if matches!(val, Value::Sub(_)) {
                self.stack.push(val);
            } else {
                return Err(RuntimeError::new("X::Undeclared::Symbols"));
            }
        } else {
            return Err(RuntimeError::new("X::Undeclared::Symbols"));
        }
        Ok(())
    }

    pub(super) fn exec_take_op(&mut self) {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        self.interpreter.take_value(val);
    }

    pub(super) fn exec_package_scope_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let body_end = body_end as usize;
        let saved = self.interpreter.current_package().to_string();
        let saved_env = self.interpreter.env().clone();
        let saved_locals = self.locals.clone();
        self.interpreter.set_current_package(name);
        self.run_range(code, *ip + 1, body_end, compiled_fns)?;
        self.interpreter.set_current_package(saved);
        let current_env = self.interpreter.env().clone();
        let mut restored_env = saved_env.clone();
        for (k, v) in current_env {
            if saved_env.contains_key(&k) || k.contains("::") {
                restored_env.insert(k, v);
            }
        }
        self.locals = saved_locals;
        for (idx, local_name) in code.locals.iter().enumerate() {
            if let Some(val) = restored_env.get(local_name).cloned() {
                self.locals[idx] = val;
            }
        }
        *self.interpreter.env_mut() = restored_env;
        *ip = body_end;
        Ok(())
    }

    pub(super) fn exec_phaser_end_op(&mut self, code: &CompiledCode, idx: u32) {
        let stmt = &code.stmt_pool[idx as usize];
        if let crate::ast::Stmt::Phaser { body, .. } = stmt {
            self.interpreter.push_end_phaser(body.clone());
        }
    }

    pub(super) fn exec_type_check_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
    ) -> Result<(), RuntimeError> {
        let constraint = Self::const_str(code, tc_idx);
        let (base_constraint, _) = crate::runtime::types::strip_type_smiley(constraint);
        let declared_constraint = base_constraint
            .split_once('(')
            .map_or(base_constraint, |(target, _)| target);
        let value = self.stack.last().expect("TypeCheck: empty stack").clone();
        if let Value::Array(..) = &value {
            if !self.array_elements_match_constraint(constraint, &value) {
                return Err(RuntimeError::new("X::Syntax::Number::LiteralType"));
            }
            return Ok(());
        }
        if matches!(value, Value::Nil) && self.interpreter.is_definite_constraint(constraint) {
            return Err(RuntimeError::new(
                "X::Syntax::Variable::MissingInitializer: Definite typed variable requires initializer",
            ));
        }
        if runtime::is_known_type_constraint(base_constraint) {
            if !matches!(value, Value::Nil)
                && !self.interpreter.type_matches_value(constraint, &value)
            {
                let coerced = match base_constraint {
                    "Str" => Some(Value::Str(crate::runtime::utils::coerce_to_str(&value))),
                    _ => None,
                };
                if let Some(new_val) = coerced {
                    *self.stack.last_mut().unwrap() = new_val;
                } else {
                    return Err(RuntimeError::new("X::Syntax::Number::LiteralType"));
                }
            }
        } else if !self.interpreter.has_type(declared_constraint)
            && !is_core_raku_type(declared_constraint)
        {
            // Unknown user-defined type — reject it
            return Err(RuntimeError::new(format!(
                "Type '{}' is not declared",
                constraint
            )));
        }
        if !matches!(value, Value::Nil) && !self.interpreter.type_matches_value(constraint, &value)
        {
            return Err(RuntimeError::new(
                "X::TypeCheck::Assignment: Type check failed in assignment",
            ));
        }
        if !matches!(value, Value::Nil) {
            let coerced = self
                .interpreter
                .try_coerce_value_for_constraint(constraint, value.clone())?;
            *self.stack.last_mut().unwrap() = coerced;
        }
        Ok(())
    }

    pub(super) fn exec_eval_ast_expr_op(
        &mut self,
        code: &CompiledCode,
        stmt_idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = code.stmt_pool[stmt_idx as usize].clone();
        let result = self.interpreter.eval_block_value(&[stmt])?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_indirect_type_lookup_op(&mut self) {
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        if let Some(code_name) = name.strip_prefix('&') {
            self.stack
                .push(self.interpreter.resolve_code_var(code_name));
        } else {
            self.stack.push(Value::Package(name));
        }
    }

    pub(super) fn exec_state_var_init_op(&mut self, code: &CompiledCode, slot: u32, key_idx: u32) {
        let init_val = self.stack.pop().unwrap_or(Value::Nil);
        let key = Self::const_str(code, key_idx);
        let val = if let Some(stored) = self.interpreter.get_state_var(key) {
            stored.clone()
        } else {
            self.interpreter
                .set_state_var(key.to_string(), init_val.clone());
            init_val
        };
        let slot_idx = slot as usize;
        self.locals[slot_idx] = val.clone();
        let name = code.locals[slot_idx].clone();
        self.interpreter.env_mut().insert(name, val);
    }

    pub(super) fn exec_block_scope_op(
        &mut self,
        code: &CompiledCode,
        enter_end: u32,
        body_end: u32,
        end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let enter_start = *ip + 1;
        let body_start = enter_end as usize;
        let leave_start = body_end as usize;
        let end = end as usize;
        let routine_snapshot = self.interpreter.snapshot_routine_registry();
        let saved_env = self.interpreter.env().clone();
        let saved_locals = self.locals.clone();

        self.run_range(code, enter_start, body_start, compiled_fns)?;
        let mut body_err = None;
        if let Err(e) = self.run_range(code, body_start, leave_start, compiled_fns) {
            body_err = Some(e);
        }
        let leave_res = self.run_range(code, leave_start, end, compiled_fns);
        self.interpreter.restore_routine_registry(routine_snapshot);

        let current_env = self.interpreter.env().clone();
        let mut restored_env = saved_env.clone();
        for (k, v) in current_env {
            if saved_env.contains_key(&k) {
                // Dynamic variables (e.g. $*VAR) are scoped to the block:
                // restore to the saved value rather than propagating the inner value.
                if k.starts_with('*') {
                    continue;
                }
                restored_env.insert(k, v);
            }
        }
        self.locals = saved_locals;
        for (idx, name) in code.locals.iter().enumerate() {
            if let Some(val) = restored_env.get(name).cloned() {
                self.locals[idx] = val;
            }
        }
        *self.interpreter.env_mut() = restored_env;

        if let Err(e) = leave_res
            && body_err.is_none()
        {
            return Err(e);
        }
        if let Some(e) = body_err {
            return Err(e);
        }
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_do_block_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        label: &Option<String>,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        let label = label.clone();
        loop {
            match self.run_range(code, body_start, end, compiled_fns) {
                Ok(()) => break,
                Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => continue,
                Err(e) if e.is_next && Self::label_matches(&e.label, &label) => {
                    self.stack.push(Value::array(vec![]));
                    break;
                }
                Err(e) if e.is_last && Self::label_matches(&e.label, &label) => {
                    self.stack
                        .push(e.return_value.unwrap_or(Value::array(vec![])));
                    break;
                }
                Err(e) => return Err(e),
            }
        }
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_let_save_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        index_mode: bool,
    ) {
        let name = Self::const_str(code, name_idx).to_string();
        if index_mode {
            let _idx_val = self.stack.pop().unwrap_or(Value::Int(0));
        }
        let old_val = self
            .get_env_with_main_alias(&name)
            .or_else(|| {
                code.locals
                    .iter()
                    .position(|n| n == &name)
                    .map(|i| self.locals[i].clone())
            })
            .unwrap_or(Value::Nil);
        self.interpreter.let_saves_push(name, old_val);
    }

    pub(super) fn exec_let_block_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mark = self.interpreter.let_saves_len();
        let body_start = *ip + 1;
        let end = body_end as usize;
        match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                if Self::is_let_success(&topic) {
                    self.interpreter.discard_let_saves(mark);
                } else {
                    self.interpreter.restore_let_saves(mark);
                    self.sync_locals_from_env(code);
                }
            }
            Err(e) => {
                self.interpreter.restore_let_saves(mark);
                self.sync_locals_from_env(code);
                return Err(e);
            }
        }
        *ip = end;
        Ok(())
    }
}
