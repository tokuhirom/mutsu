use super::*;

impl Interpreter {
    /// Execute a thunk-based short-circuit reduction where the operand is
    /// an array of thunks (anonymous blocks).  Each thunk is called lazily:
    /// for `&&`/`and`:    evaluation stops on the first false result;
    /// for `||`/`or`:     evaluation stops on the first truthy result;
    /// for `//`/`orelse`: evaluation stops on the first defined result;
    /// for `andthen`:     evaluation stops on the first undefined result;
    /// for `^^`/`xor`:    evaluation stops after two truthy values are found.
    ///
    /// `scan` controls whether intermediate results are collected (triangle
    /// reduction `[\op]`) or only the final value is returned (`[op]`).
    pub(super) fn exec_scan_shortcircuit_reduction(
        &mut self,
        op: &str,
        _negate: bool,
        scan: bool,
        thunks: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        if thunks.is_empty() {
            if scan {
                self.stack.push(Value::Seq(std::sync::Arc::new(Vec::new())));
            } else {
                // Identity values
                let identity = match op {
                    "&&" | "and" => Value::Bool(true),
                    _ => Value::Bool(false),
                };
                self.stack.push(identity);
            }
            return Ok(());
        }

        let is_xor = matches!(op, "^^" | "xor");

        if is_xor {
            // xor/^^ is list-associative: exactly one element must be truthy.
            // Short-circuit once two truthy values are found.
            let mut found: Option<Value> = None;
            let mut multiple = false;
            let mut last_val = Value::Nil; // track last evaluated value for all-false case
            let mut results: Vec<Value> = Vec::new();
            let mut done = false; // whether we have short-circuited
            for thunk in thunks {
                if done {
                    // Already short-circuited: result is Nil for all remaining
                    if scan {
                        results.push(Value::Nil);
                    }
                    // don't evaluate thunk
                } else {
                    let val = self.vm_call_on_value(thunk, vec![], None)?;
                    last_val = val.clone();
                    if val.truthy() {
                        if found.is_some() {
                            // Second truthy: result is Nil, short-circuit
                            multiple = true;
                            found = None;
                            done = true;
                            if scan {
                                results.push(Value::Nil);
                            }
                        } else {
                            found = Some(val.clone());
                            if scan {
                                results.push(val);
                            }
                        }
                    } else {
                        // Falsy: current xor result is the truthy one found so far,
                        // or val itself if none found yet
                        let cur = if let Some(ref f) = found {
                            f.clone()
                        } else {
                            val
                        };
                        if scan {
                            results.push(cur);
                        }
                    }
                }
            }
            let final_result = if multiple {
                Value::Nil
            } else if let Some(v) = found {
                v
            } else {
                // All false: return last element
                last_val
            };
            if scan {
                self.stack.push(Value::Seq(std::sync::Arc::new(results)));
            } else {
                self.stack.push(final_result);
            }
            return Ok(());
        }

        // Evaluate the first thunk unconditionally.
        let mut acc = self.vm_call_on_value(thunks[0].clone(), vec![], None)?;
        let mut results: Vec<Value> = if scan { vec![acc.clone()] } else { Vec::new() };
        // For the remaining thunks, check acc before evaluating.
        for thunk in thunks.into_iter().skip(1) {
            let should_short_circuit = match op {
                "&&" | "and" => !acc.truthy(),
                "||" | "or" => acc.truthy(),
                "//" | "orelse" => runtime::types::value_is_defined(&acc),
                "andthen" => !runtime::types::value_is_defined(&acc),
                _ => false,
            };
            if should_short_circuit {
                // For `andthen`: once acc is undefined, drop remaining elements from scan.
                // For all other operators (&&/||/and/or/orelse), repeat the accumulated value.
                if scan && op != "andthen" {
                    results.push(acc.clone());
                }
                // don't evaluate thunk
            } else {
                let val = self.vm_call_on_value(thunk, vec![], None)?;
                acc = match op {
                    "&&" | "and" => {
                        if acc.truthy() {
                            val
                        } else {
                            acc
                        }
                    }
                    "||" | "or" => {
                        if acc.truthy() {
                            acc
                        } else {
                            val
                        }
                    }
                    "//" | "orelse" => {
                        if runtime::types::value_is_defined(&acc) {
                            acc
                        } else {
                            val
                        }
                    }
                    "andthen" => {
                        if runtime::types::value_is_defined(&acc) {
                            val
                        } else {
                            Value::Slip(std::sync::Arc::new(vec![]))
                        }
                    }
                    _ => val,
                };
                if scan {
                    results.push(acc.clone());
                }
            }
        }
        if scan {
            self.stack.push(Value::Seq(std::sync::Arc::new(results)));
        } else {
            self.stack.push(acc);
        }
        Ok(())
    }

    /// Lazily evaluate a triangle (scan) reduction on an infinite/lazy range.
    /// Produces a `LazyList` with a `ScanSpec` so that additional elements
    /// can be computed on demand. Pre-computes an initial batch of elements
    /// so that common operations like `.Slip` and `.join` work immediately.
    pub(super) fn exec_lazy_scan_reduction(
        &mut self,
        base_op: &str,
        negate: bool,
        list_value: &Value,
    ) -> Result<(), RuntimeError> {
        let spec = crate::value::ScanSpec {
            op: base_op.to_string(),
            negate,
            source: list_value.clone(),
            accumulator: None,
            computed_count: 0,
        };
        let ll = crate::value::LazyList::new_scan(spec);
        let ll = std::sync::Arc::new(ll);
        // Pre-compute an initial batch so that eager consumers (e.g. .Slip,
        // .join) that read the cache directly get a useful prefix.
        const INITIAL_BATCH: usize = 1_000;
        self.force_scan_lazy_list(&ll, INITIAL_BATCH)?;
        self.stack.push(Value::LazyList(ll));
        Ok(())
    }

    pub(super) fn exec_routine_magic_op(&mut self) -> Result<(), RuntimeError> {
        // Skip pointy-block entries in the routine stack so that &?ROUTINE
        // inside a pointy block sees the enclosing routine (sub/method).
        let routine_stack = self.routine_stack();
        let entry = routine_stack
            .iter()
            .rev()
            .find(|frame| frame.name != "<pointy-block>");
        if let Some(frame) = entry {
            // Anonymous subs are pushed with "<anon>" as the sentinel name.
            // Return the block_stack Sub directly so callers can invoke it.
            if frame.name.is_empty() || frame.name == "<anon>" {
                if let Some(val) = self.block_stack_top().cloned()
                    && matches!(val, Value::Sub(_))
                {
                    self.stack.push(val);
                    return Ok(());
                }
                return Err(RuntimeError::undeclared_symbols("Undeclared name"));
            }
            self.stack.push(Value::Routine {
                package: Symbol::intern(&frame.package),
                name: Symbol::intern(&frame.name),
                is_regex: false,
            });
        } else {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        Ok(())
    }

    pub(super) fn exec_block_magic_op(&mut self) -> Result<(), RuntimeError> {
        if let Some(val) = self.block_stack_top().cloned() {
            if matches!(val, Value::Sub(_)) {
                self.stack.push(val);
            } else {
                return Err(RuntimeError::undeclared_symbols("Undeclared name"));
            }
        } else {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        Ok(())
    }

    pub(super) fn exec_take_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        if self.gather_items_len() > 0 {
            self.take_value(val)
        } else {
            // No enclosing gather — raise a CX::Take control exception so a
            // CONTROL block can observe it. If unhandled, the runtime wraps
            // it as X::ControlFlow with illegal=>"take", enclosing=>"gather".
            Err(RuntimeError::take_signal(val))
        }
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
        let saved = self.current_package().to_string();
        let saved_env = self.env().clone();
        let saved_locals = self.locals.clone();
        self.set_current_package(name.clone());
        self.run_range(code, *ip + 1, body_end, compiled_fns)?;
        self.set_current_package(saved);
        let current_env = self.env().clone();
        let mut restored_env = saved_env.clone();
        // The block's own `my` lexicals (new env keys, not package-qualified or
        // internal) are dropped from the outer scope below (lexical scoping), but
        // a named sub defined in this `package Foo {...}` block closes over them and
        // is called by-name AFTER the block exits. Record them keyed by the package
        // so a `GetGlobal` miss inside Foo's subs can fall back to them. Stored
        // post-body, so the values reflect the block's assignments (e.g. zef's
        // `package Zef::CLI { my $CONFIG = preprocess-args-config-mutate(...); ... }`).
        for (k, v) in current_env.iter() {
            if !saved_env.contains_key_sym(*k) && !k.contains_str("::") && !k.starts_with("__") {
                let bare = k.resolve();
                // Record ONLY genuine `my` lexicals. An `our` package variable also
                // leaves a bare env key here, but it has a qualified twin in the
                // `our` store (`Pkg::name`) that is the authoritative value — and it
                // can be set from OUTSIDE the package (`$Pkg::msg = ...`), which this
                // bare snapshot would not see. Recording it would let the stale
                // declaration-time snapshot shadow the real `our` value on read
                // (`package_scope_lexical` is consulted before the `our` fallback).
                let qualified = format!("{name}::{bare}");
                if self.get_our_var(&qualified).is_some() || current_env.contains_key(&qualified) {
                    continue;
                }
                self.package_lexicals
                    .entry(name.clone())
                    .or_default()
                    .insert(bare, v.clone());
            }
        }
        for (k, v) in current_env.iter() {
            if saved_env.contains_key_sym(*k) || k.contains_str("::") {
                restored_env.insert_sym(*k, v.clone());
            }
        }
        self.locals = saved_locals;
        for (idx, local_name) in code.locals.iter().enumerate() {
            if let Some(val) = restored_env.get(local_name).cloned() {
                self.locals[idx] = val;
            }
        }
        *self.env_mut() = restored_env;
        *ip = body_end;
        Ok(())
    }

    pub(super) fn exec_phaser_end_op(&mut self, code: &CompiledCode, idx: u32, site_id: u64) {
        // Only register each END phaser once (by site_id), even if the
        // opcode is encountered multiple times inside a repeatedly-called closure.
        if !self.register_end_phaser_site(site_id) {
            return;
        }
        let stmt = &code.stmt_pool[idx as usize];
        if let crate::ast::Stmt::Phaser { body, .. } = stmt {
            loan_env!(self, push_end_phaser(body.clone()));
        }
    }

    pub(super) fn exec_type_check_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.exec_type_check_op_inner(code, tc_idx, var_name_idx, false)
    }

    /// Type check for `:=` binds to a typed scalar; raises X::TypeCheck::Binding
    /// on mismatch instead of X::TypeCheck::Assignment.
    pub(super) fn exec_type_check_bind_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.exec_type_check_op_inner(code, tc_idx, var_name_idx, true)
    }
}
