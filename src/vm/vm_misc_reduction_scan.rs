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
                self.stack.push(Value::seq(Vec::new()));
            } else {
                // Identity values
                let identity = match op {
                    "&&" | "and" => Value::TRUE,
                    _ => Value::FALSE,
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
            let mut last_val = Value::NIL; // track last evaluated value for all-false case
            let mut results: Vec<Value> = Vec::new();
            let mut done = false; // whether we have short-circuited
            for thunk in thunks {
                if done {
                    // Already short-circuited: result is Nil for all remaining
                    if scan {
                        results.push(Value::NIL);
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
                                results.push(Value::NIL);
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
                Value::NIL
            } else if let Some(v) = found {
                v
            } else {
                // All false: return last element
                last_val
            };
            if scan {
                self.stack.push(Value::seq(results));
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
                            Value::slip_arc(std::sync::Arc::new(vec![]))
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
            self.stack.push(Value::seq(results));
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
            source: crate::runtime::unbounded_range::pipe_source(list_value.clone()),
            accumulator: None,
            computed_count: 0,
        };
        let ll = crate::value::LazyList::new_scan(spec);
        let ll = crate::gc::Gc::new(ll);
        // Pre-compute an initial batch so that eager consumers (e.g. .Slip,
        // .join) that read the cache directly get a useful prefix.
        //
        // NOT over a lazy pipe (`(1..*).map(...)`, `.grep(...)`, a `gather`):
        // stepping the scan 1000 times runs the pipe's user closure 1000
        // times, which is observable — `([\+] (1..*).map({ die if $_ > 5; $_
        // }))[^3]` must answer `[1 3 6]`, not explode. A `Range` source has no
        // such body, and every consumer that needs more than the cache holds
        // goes back through `force_scan_lazy_list` for it.
        const INITIAL_BATCH: usize = 1_000;
        if !matches!(list_value.view(), ValueView::LazyList(_)) {
            self.force_scan_lazy_list(&ll, INITIAL_BATCH)?;
        }
        self.stack.push(Value::lazy_list(ll));
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
                if let Some(val) = self.block_stack_top()
                    && matches!(val.view(), ValueView::Sub(_))
                {
                    self.stack.push(val);
                    return Ok(());
                }
                return Err(RuntimeError::undeclared_symbols("Undeclared name"));
            }
            self.stack
                .push(Value::routine_parts(frame.package, frame.name, false));
        } else {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        Ok(())
    }

    pub(super) fn exec_block_magic_op(&mut self) -> Result<(), RuntimeError> {
        if let Some(val) = self.block_stack_top() {
            if matches!(val.view(), ValueView::Sub(_)) {
                self.stack.push(val);
            } else {
                return Err(RuntimeError::undeclared_symbols("Undeclared name"));
            }
        } else {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        Ok(())
    }

    /// Cost: O(1) amortized (a push onto the enclosing gather's buffer), so a
    /// gather that takes e values builds its list in O(e); taking an unforced
    /// gather coroutine forces it whole first.
    pub(super) fn exec_take_op(&mut self) -> Result<(), RuntimeError> {
        let mut val = self.stack.pop().unwrap_or(Value::NIL);
        // `take <gather>` (a coroutine-backed lazy `Seq`, e.g. the recursive
        // `take internals($child)` in the P62 binary-tree walk) is reified to a
        // concrete `Seq` here, at the point it is taken. The gather was just
        // produced in the current (still-valid) evaluation context; deferring
        // its force until the enclosing gather's result is flattened later
        // re-runs it in a torn-down context and yields nothing. A later
        // `flat`/`.flat` on the enclosing gather still flattens the reified
        // `Seq` — the observable result is identical to raku's lazy `take`.
        if let ValueView::LazyList(ll) = val.view()
            && ll.coroutine.is_some()
            && ll.cache.lock().unwrap().is_none()
        {
            let items = self.force_lazy_list_vm(&ll)?;
            val = Value::seq(items);
        }
        // `take` of a lazy-tail Slip adds the elements of its ending lazy
        // items (#13117).
        let val = self.reify_lazy_slip_tail(val)?;
        if self.gather_items_len() > 0 {
            // Raku's `take` ALWAYS raises a `CX::Take` control exception, and
            // `gather` is only its outermost handler: a lexically nearer
            // `CONTROL { when CX::Take {...} }` (or a catch-all `default`)
            // intercepts it first and may `.resume` it, which discards the
            // value. mutsu appends straight into the gather buffer for speed,
            // so route through the control exception only when the innermost
            // active CONTROL handler actually declares an arm that can match a
            // `CX::Take` (`OpCode::TryCatch::control_handles_take`). Handlers
            // that cannot match it keep the direct path, so the common
            // `gather`+`CATCH`/`CONTROL` combination is unaffected.
            if self
                .control
                .control_handlers
                .last()
                .is_some_and(|h| h.handles_take)
            {
                return Err(RuntimeError::take_signal(val));
            }
            self.take_value(val)
        } else {
            // No enclosing gather — raise a CX::Take control exception so a
            // CONTROL block can observe it. If unhandled, the runtime wraps
            // it as X::ControlFlow with illegal=>"take", enclosing=>"gather".
            Err(RuntimeError::take_signal(val))
        }
    }

    pub(super) fn exec_phaser_end_op(&mut self, code: &CompiledCode, idx: u32, site_id: u64) {
        let stmt = &code.stmt_pool[idx as usize];
        let crate::ast::Stmt::Phaser {
            body, end_index, ..
        } = stmt
        else {
            return;
        };
        let end_index = *end_index;
        // The main compunit's ENDs were all installed before the body ran (see
        // `end_phasers`), so reaching one only records the scope it closes
        // over — into the slot it already owns, which is what keeps its
        // source-order install position. Re-reaching it re-captures, matching
        // rakudo, where the last execution's frame is the one the END sees.
        //
        // Indices are unique per process, so a hit here is always this
        // declaration and never a same-numbered one from a module. (The
        // precompilation cache would break that, since it outlives the process
        // whose counter produced the index — which is why `end_index` is
        // `#[serde(skip)]` and a cache hit carries none.)
        if loan_env!(self, capture_end_phaser_env(end_index)) {
            return;
        }
        // No pre-installed slot: a module's END, an `EVAL`'s, an rvalue one, or
        // a main-compunit one in a nesting form `end_phasers` did not walk
        // into. Those install where execution reaches them, so register each
        // site only once even when the opcode sits inside a repeatedly-called
        // closure.
        if !self.register_end_phaser_site(site_id) {
            return;
        }
        // An `EVAL`'d snippet is compiled at run time in rakudo, so its ENDs
        // install after everything the main compunit declared rather than at a
        // source position. Withhold the index for those; `push_end_phaser`
        // then keeps them in the RUNTIME class. (A module body needs no such
        // treatment: `push_end_phaser` reads the load-order stack itself.)
        let end_index = if self.env().get("__mutsu_in_eval").is_some() {
            None
        } else {
            end_index
        };
        let body = body.clone();
        loan_env!(self, push_end_phaser(body, end_index));
    }

    pub(super) fn exec_type_check_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
        has_explicit_initializer: bool,
        smiley_from_pragma: bool,
    ) -> Result<(), RuntimeError> {
        self.exec_type_check_op_inner(
            code,
            tc_idx,
            var_name_idx,
            false,
            has_explicit_initializer,
            smiley_from_pragma,
        )
    }

    /// Type check for `:=` binds to a typed scalar; raises X::TypeCheck::Binding
    /// on mismatch instead of X::TypeCheck::Assignment.
    pub(super) fn exec_type_check_bind_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.exec_type_check_op_inner(code, tc_idx, var_name_idx, true, false, false)
    }
}
