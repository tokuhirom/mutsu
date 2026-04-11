use super::*;

impl VM {
    /// Build a simple backtrace string from the interpreter's routine stack.
    /// Each frame is formatted as "  in sub <name>" (or "  in method <name>").
    pub(super) fn build_backtrace_string(&self) -> String {
        let stack = self.interpreter.routine_stack();
        let mut lines = Vec::new();
        for (_package, name) in stack.iter().rev() {
            if name.is_empty() || name == "<unit>" {
                lines.push("  in block <unit>".to_string());
            } else {
                lines.push(format!("  in sub {}", name));
            }
        }
        lines.join("\n")
    }

    /// If the value is a `LazyIoLines`, force it into an eager array by reading
    /// all remaining lines from the file handle. Otherwise return the value as-is.
    pub(super) fn force_if_lazy_io_lines(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyIoLines { ref handle, kv } = val {
            let forced = self.interpreter.force_lazy_io_lines(handle)?;
            if kv {
                // Apply .kv transformation on the forced array
                let items = crate::runtime::utils::value_to_list(&forced);
                let mut kv_items = Vec::with_capacity(items.len() * 2);
                for (i, v) in items.iter().enumerate() {
                    kv_items.push(Value::Int(i as i64));
                    kv_items.push(v.clone());
                }
                Ok(Value::array(kv_items))
            } else {
                Ok(forced)
            }
        } else {
            Ok(val)
        }
    }

    fn thread_right_first(
        left: &crate::value::JunctionKind,
        right: &crate::value::JunctionKind,
    ) -> bool {
        use crate::value::JunctionKind::{All, Any, None, One};
        matches!(left, Any | One) && matches!(right, All | None)
    }

    pub(super) fn label_matches(error_label: &Option<String>, loop_label: &Option<String>) -> bool {
        error_label.as_deref() == loop_label.as_deref() || error_label.is_none()
    }

    /// Check if a method on LazyList requires forcing the list first.
    pub(super) fn lazy_list_needs_forcing(method: &str) -> bool {
        matches!(
            method,
            "list"
                | "Array"
                | "Numeric"
                | "Int"
                | "elems"
                | "hyper"
                | "race"
                | "first"
                | "grep"
                | "map"
                | "sort"
                | "reverse"
                | "join"
                | "head"
                | "tail"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "flat"
                | "unique"
                | "repeated"
                | "squish"
                | "classify"
                | "categorize"
                | "produce"
                | "rotor"
                | "batch"
                | "reduce"
                | "combinations"
                | "permutations"
                | "values"
                | "List"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "Seq"
                | "item"
                | "cache"
                | "pick"
                | "roll"
                | "keys"
                | "kv"
                | "pairs"
                | "antipairs"
        )
    }

    /// Force a LazyList by running its compiled bytecode in the VM.
    /// Falls back to interpreter if no compiled code is available.
    pub(super) fn force_lazy_list_vm(
        &mut self,
        list: &LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Check cache first
        if let Some(cached) = list.cache.lock().unwrap().clone() {
            return Ok(cached);
        }

        // If no compiled code, fall back to interpreter
        let (cc, fns) = match (&list.compiled_code, &list.compiled_fns) {
            (Some(cc), Some(fns)) => (cc.clone(), fns.clone()),
            _ => return self.interpreter.force_lazy_list_bridge(list),
        };

        // Save current VM state.
        // Flush any pending locals→env writes so env is consistent.
        // We do a manual sync rather than ensure_env_synced since we don't
        // have the outer code here -- we'll restore locals directly.
        let saved_env = self.interpreter.clone_env();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_env_dirty = self.env_dirty;
        let saved_locals_dirty = self.locals_dirty;

        // Set up lazy list's environment
        *self.interpreter.env_mut() = list.env.clone();

        // Push gather items collector
        let saved_gather_len = self.interpreter.gather_items_len();
        self.interpreter.push_gather_items(Vec::new());
        self.interpreter.push_gather_take_limit(None);

        // Initialize locals for the compiled code
        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.env_dirty = false;
        self.locals_dirty = false;
        self.stack = Vec::new();

        // Run the compiled code using the lazy list's own compiled_fns.
        // Outer scope subs are available via the env as Value::Sub.
        let run_fns = fns.as_ref();

        let mut ip = 0;
        let mut run_result = Ok(());
        while ip < cc.ops.len() {
            match self.exec_one(&cc, &mut ip, run_fns) {
                Ok(()) => {}
                Err(e) if e.is_warn => {
                    if !self.interpreter.warning_suppressed() {
                        self.interpreter.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    ip += 1;
                    continue;
                }
                Err(e) => {
                    run_result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        // Collect gather items
        let items = self.interpreter.pop_gather_items().unwrap_or_default();
        self.interpreter.pop_gather_take_limit();

        // Clean up extra gather items if needed
        while self.interpreter.gather_items_len() > saved_gather_len {
            self.interpreter.pop_gather_items();
            self.interpreter.pop_gather_take_limit();
        }

        // Sync locals back to env before reading the result environment.
        // During VM execution, variable assignments go to self.locals, not
        // to the interpreter env. We must flush them so the merge logic below
        // can see the changes made by the gather body.
        for (i, name) in cc.locals.iter().enumerate() {
            if cc.simple_locals[i] {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), self.locals[i].clone());
            }
        }

        // Restore the outer environment, selectively merging changes from
        // the gather body. Only propagate variables that:
        // 1. Existed in the outer scope, AND
        // 2. Were actually modified during gather body execution
        //    (i.e., their value changed from the gather body's initial env).
        // This prevents nested gather closures from corrupting each other's
        // captured variables (e.g., `$n` in nested grep-div calls), while
        // still propagating genuine side effects (e.g., `$x += 1`).
        let gather_result_env = self.interpreter.env().clone();
        let mut merged_env = saved_env.clone();
        for (k, v) in gather_result_env.iter() {
            if !saved_env.contains_key(k) {
                continue;
            }
            if let Some(initial) = list.env.get(k) {
                // Variable existed in both outer and gather env.
                // Only propagate if the value actually changed during execution.
                // Compare string representations as a proxy for value equality.
                if v.to_string_value() != initial.to_string_value() {
                    merged_env.insert(k.clone(), v.clone());
                }
            } else {
                // Variable existed in outer scope but not in gather's initial env;
                // always propagate changes.
                merged_env.insert(k.clone(), v.clone());
            }
        }
        *self.interpreter.env_mut() = merged_env;

        // Check whether the merged env actually changed any outer-scope variables.
        let env_actually_changed = {
            let merged = self.interpreter.env();
            saved_env.iter().any(|(k, old_val)| {
                merged
                    .get(k)
                    .is_some_and(|new_val| new_val.to_string_value() != old_val.to_string_value())
            })
        };

        // Restore VM state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = if env_actually_changed {
            true
        } else {
            saved_env_dirty
        };
        self.locals_dirty = saved_locals_dirty;

        // Check for errors
        run_result?;

        // Cache the result
        *list.cache.lock().unwrap() = Some(items.clone());
        Ok(items)
    }

    /// Force a LazyList into a Seq by evaluating the gather body.
    fn force_lazy_if_needed(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyList(ll) = &val {
            let items = self.force_lazy_list_vm(ll)?;
            Ok(Value::Seq(std::sync::Arc::new(items)))
        } else {
            Ok(val)
        }
    }

    pub(super) fn eval_binary_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        f: fn(&mut VM, Value, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        // Auto-FETCH Proxy containers in binary operations
        let left = self.interpreter.auto_fetch_proxy(&left)?;
        let right = self.interpreter.auto_fetch_proxy(&right)?;
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(right_kind.clone(), results?));
        }
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        // Force LazyList values before arithmetic/comparison operations
        let left = self.force_lazy_if_needed(left)?;
        let right = self.force_lazy_if_needed(right)?;
        f(self, left, right)
    }

    /// Smartmatch with junction threading but WITHOUT forcing lazy values.
    /// For `!~~` (negate=true), we compute `~~` first and then negate the
    /// collapsed result.  Raku defines `$x !~~ $y` as `not ($x ~~ $y)`,
    /// where `not` collapses junctions before negating.
    #[allow(dead_code)]
    pub(super) fn eval_smartmatch_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
    ) -> Result<Value, RuntimeError> {
        self.eval_smartmatch_with_junctions_ex(left, right, negate, false)
    }

    /// Extended smartmatch with junction threading.
    /// `rhs_is_match_regex` indicates the RHS was originally `m//`, which
    /// changes the failure return from Nil to False.
    pub(super) fn eval_smartmatch_with_junctions_ex(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
        rhs_is_match_regex: bool,
    ) -> Result<Value, RuntimeError> {
        // For !~~, compute ~~ first, then negate the collapsed boolean.
        if negate {
            let match_result =
                self.eval_smartmatch_with_junctions_ex(left, right, false, rhs_is_match_regex)?;
            let bool_val = match_result.truthy();
            return Ok(Value::Bool(!bool_val));
        }
        // When RHS is the Junction type object, don't auto-thread LHS.
        // $junction ~~ Junction should return True (a Junction isa Junction).
        // Also applies to Mu (the supertype of Junction).
        if matches!(&right, Value::Package(name) if matches!(name.resolve().as_str(), "Junction" | "Mu"))
            && matches!(&left, Value::Junction { .. })
        {
            return self.smart_match_op(left, right, rhs_is_match_regex);
        }
        // Helper: check if a value is a regex (for junction collapse decisions)
        let is_regex_value = |v: &Value| {
            matches!(
                v,
                Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
                    | Value::Routine { is_regex: true, .. }
            )
        };
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| {
                    self.eval_smartmatch_with_junctions_ex(
                        left.clone(),
                        v,
                        false,
                        rhs_is_match_regex,
                    )
                })
                .collect();
            // Smartmatch collapses junctions to Bool
            let junction = Value::junction(right_kind.clone(), results?);
            return Ok(Value::Bool(junction.truthy()));
        }
        if let Value::Junction { kind, values } = left {
            // When RHS is a non-junction regex and LHS is a junction,
            // return the Junction of Match/Nil results without collapsing.
            // For all other cases, collapse to Bool.
            let keep_junction = is_regex_value(&right);
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| {
                    self.eval_smartmatch_with_junctions_ex(
                        v,
                        right.clone(),
                        false,
                        rhs_is_match_regex,
                    )
                })
                .collect();
            let junction = Value::junction(kind, results?);
            if keep_junction {
                return Ok(junction);
            }
            return Ok(Value::Bool(junction.truthy()));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| {
                    self.eval_smartmatch_with_junctions_ex(
                        left.clone(),
                        v,
                        false,
                        rhs_is_match_regex,
                    )
                })
                .collect();
            // Smartmatch collapses junctions to Bool
            let junction = Value::junction(kind, results?);
            return Ok(Value::Bool(junction.truthy()));
        }
        self.smart_match_op(left, right, rhs_is_match_regex)
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
        rhs_is_match_regex: bool,
    ) -> Result<Value, RuntimeError> {
        // When RHS is Whatever, autoprime: return a WhateverCode that takes
        // one argument and smartmatches LHS against it.
        // In Raku, `$x ~~ *` produces `-> $a { $x ~~ $a }`.
        if matches!(&right, Value::Whatever) {
            use crate::ast::{Expr, Stmt};
            use crate::env::Env;
            let mut env = Env::new();
            env.insert(
                "__mutsu_callable_type".to_string(),
                Value::str_from("WhateverCode"),
            );
            // Capture the LHS value in the closure environment
            env.insert("__wc_sm_lhs".to_string(), left);
            let param = "__wc_0".to_string();
            let body = vec![Stmt::Expr(Expr::Binary {
                left: Box::new(Expr::Var("__wc_sm_lhs".to_string())),
                op: crate::token_kind::TokenKind::SmartMatch,
                right: Box::new(Expr::Var(param.clone())),
            })];
            return Ok(Value::make_sub(
                Symbol::intern("GLOBAL"),
                Symbol::intern("<whatevercode-smartmatch>"),
                vec![param],
                Vec::new(),
                body,
                false,
                env,
            ));
        }
        let is_regex = matches!(
            &right,
            Value::Regex(_)
                | Value::RegexWithAdverbs { .. }
                | Value::Routine { is_regex: true, .. }
        );
        let matched = self.vm_smart_match(&left, &right);
        // Check for pending regex security error (set by regex parse/match)
        if let Some(err) = crate::runtime::Interpreter::take_pending_regex_error() {
            return Err(err);
        }
        // Check for pending dispatch error (e.g., from Any ~~ Pair method call)
        if let Some(err) = self.interpreter.take_pending_dispatch_error() {
            return Err(err);
        }
        if is_regex {
            // When $/ is a Junction (from :nth with junction argument),
            // the ~~ operator collapses the result to a Bool.
            let slash = self
                .interpreter
                .env()
                .get("/")
                .cloned()
                .unwrap_or(Value::Nil);
            if matches!(&slash, Value::Junction { .. }) {
                Ok(Value::Bool(matched))
            } else if matched {
                // For regex smartmatch, return the Match object (from $/) or Nil
                Ok(slash)
            } else if rhs_is_match_regex && matches!(&slash, Value::Nil) {
                // Failed m// (non-global) returns False, not Nil.
                // But m:g// returns an empty list from $/, so we check that
                // $/ is Nil before returning False.
                Ok(Value::Bool(false))
            } else {
                // Failed bare // returns Nil; m:g// returns $/ (empty list)
                Ok(slash)
            }
        } else {
            Ok(Value::Bool(matched))
        }
    }
}
