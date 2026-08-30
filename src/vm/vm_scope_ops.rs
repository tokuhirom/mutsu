//! Scope ops: `subtest`, `react`, and `whenever` scope execution.
use super::*;

impl Interpreter {
    pub(super) fn exec_subtest_scope_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let end = body_end as usize;
        let body_start = *ip + 1;
        let label = self.stack.pop().unwrap_or(Value::NIL).to_string_value();
        let ctx = self.begin_subtest();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);
        self.finish_subtest(ctx, &label, run_result)?;
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_react_scope_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let end = body_end as usize;
        let body_start = *ip + 1;

        // `whenever` callbacks run as compiled bytecode (the drive loop lives on
        // `impl Interpreter`, see `vm_react_loop.rs`) but still capture their lexicals from
        // env. First pull any pending env updates into locals (e.g. instance
        // attribute mutations written into the shared cell after bind-stdin), then
        // flush all locals to env so captured vars are visible/mutable from the
        // whenever callbacks.
        self.sync_env_from_locals(code);

        // Enter react mode: whenever blocks will register subscriptions
        self.enter_react();
        let saved_depth = self.stack.len();
        // A bare `done` written directly in the react body's own top level
        // (not inside a `whenever`) is handled by the `body_done` check just
        // below — see `runtime::react_done_handler_depth`.
        let _react_done_handler =
            crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        drop(_react_done_handler);
        self.stack.truncate(saved_depth);

        // If `done;` was called in the react body, skip the event loop —
        // the body already signaled that no further events should be processed.
        let body_done = matches!(&run_result, Err(e) if e.is_react_done());
        // The react/supply drive loop runs Interpreter-side and dispatches every
        // whenever / LAST / QUIT / CLOSE callback as compiled bytecode
        // (Stage 2 #3038/#3039; QUIT handlers Interpreter-native in the Stage 3 follow-up).
        // No drive-loop callback routes back through the tree-walk interpreter.
        // The `whenever` callbacks mutate captured-outer lexicals by name in env,
        // with no per-write record this site can drain. Snapshot the caller frame's
        // slot-backing env values right before the event loop so that, after it,
        // only the slots whose env value actually changed are written through.
        let pre_env: Vec<Option<Value>> = code
            .locals
            .iter()
            .map(|n| {
                self.env().get(n).cloned().or_else(|| {
                    n.strip_prefix('$')
                        .or_else(|| n.strip_prefix('@'))
                        .or_else(|| n.strip_prefix('%'))
                        .or_else(|| n.strip_prefix('&'))
                        .and_then(|b| self.env().get(b).cloned())
                })
            })
            .collect();
        let event_result = if body_done {
            // Drain any queued subscriptions so they don't leak
            self.run_react_event_loop_drain();
            Ok(())
        } else {
            self.run_react_event_loop()
        };
        // Slice F (react/whenever coherence): the `whenever` callbacks ran as
        // compiled bytecode on *this* VM (synchronous `from-list` emit) and
        // mutated captured-outer caller lexicals (`my $i; whenever ... { $i++ }`)
        // straight into `env` by name. Reconcile the caller's local slots from
        // env so the slot stays coherent (same HashEntryRef / `!attr` per-slot
        // skips); this is what keeps `$i` correct.
        for (i, name) in code.locals.iter().enumerate() {
            if name.starts_with('!')
                || matches!(self.locals[i].view(), ValueView::HashEntryRef { .. })
            {
                continue;
            }
            let cur = self.env().get(name).cloned().or_else(|| {
                name.strip_prefix('$')
                    .or_else(|| name.strip_prefix('@'))
                    .or_else(|| name.strip_prefix('%'))
                    .or_else(|| name.strip_prefix('&'))
                    .and_then(|b| self.env().get(b).cloned())
            });
            if let Some(cur) = cur
                && pre_env.get(i).map(|p| p.as_ref()) != Some(Some(&cur))
            {
                self.locals[i] = cur;
            }
        }

        *ip = end;
        if let Err(err) = run_result
            && !err.is_react_done()
        {
            return Err(err);
        }
        if let Err(err) = event_result
            && !err.is_react_done()
        {
            // Wrap in X::React::Died if not already wrapped
            return Err(crate::runtime::Interpreter::wrap_react_died_if_needed(err));
        }
        Ok(())
    }

    /// Promote the `supply` block's own `my` lexicals to shared container cells
    /// so every callback the block registers reads and writes ONE binding.
    ///
    /// A `whenever`/`LAST`/`QUIT` callback captures the live env by value, and
    /// each callback then persists its own writes against its own `Sub` id. That
    /// makes the block's lexical a per-callback snapshot: a sibling `whenever`
    /// never saw the first one's writes, and a `LAST` phaser read the value the
    /// variable had when the block started — `supply { my $acc = ''; whenever $s
    /// { $acc ~= $_; LAST emit $acc } }` emitted the empty string, which is how
    /// Cro's `application/x-www-form-urlencoded` body parser decoded every
    /// request body as empty. A cell is captured by reference and overwrites a
    /// same-named caller lexical on entry (see the `ContainerRef` arm of the
    /// closure-env merge in `resolution_call_sub.rs`), so it fixes the sharing
    /// without giving up the lexical-scoping vouch `owned_lexicals` provides.
    ///
    /// Only names the block itself declared with `my` are promoted. The emitter
    /// parameter is not (it is dispatched on as an object), and neither are
    /// captured outer lexicals — those belong to the declaring frame.
    fn share_supply_block_lexicals(&mut self, code: &CompiledCode) {
        for sym in &code.my_declared_sym {
            if code.free_var_syms.contains(sym)
                || code.supply_emitter_sym == Some(*sym)
                // A `my enum`'s type and variant names are `my`-declared too,
                // but they are not variables: they must keep resolving to the
                // enum binding, and a cell in their slot hides it
                // (t/supply-block-enum-lexical.t).
                || code.my_declared_enum_sym.contains(sym)
            {
                continue;
            }
            let Some(current) = self.env().get_sym(*sym).cloned() else {
                continue;
            };
            if matches!(current.view(), ValueView::ContainerRef(_)) {
                continue;
            }
            self.env_mut()
                .insert_sym(*sym, current.into_container_ref());
        }
    }

    pub(super) fn exec_whenever_scope_op(
        &mut self,
        code: &CompiledCode,
        body_idx: u32,
        analysis_cc_idx: u32,
        param_idx: &Option<u32>,
        yields_value: bool,
        param_type_idx: &Option<u32>,
    ) -> Result<(), RuntimeError> {
        let supply_val = self.stack.pop().unwrap_or(Value::NIL);
        let param = param_idx.map(|idx| Self::const_str(code, idx).to_string());
        let param_type = param_type_idx.map(|idx| Self::const_str(code, idx).to_string());
        let stmt = &code.stmt_pool[body_idx as usize];
        if let Stmt::Block(body) = stmt {
            // Box captured-and-mutated lexicals the whenever body reads into
            // shared ContainerRef cells BEFORE run_whenever_with_value clones
            // the env for the callback closures below: those closures are
            // dispatched later (possibly cross-thread, e.g. a `start` inside
            // the body), so a by-value copy would miss the parent frame's
            // later writes and vice versa. Mirrors MakeGather's identical
            // precondition (`exec_make_gather_op`) for the same reason: the
            // analysis closure compiled by `surface_stashed_body_free_vars`
            // (Case B, cross-thread lexicals) names the free vars; the boxing
            // rules are exactly the closure-capture ones.
            let analysis_cc = Self::resolve_closure_code(code, Some(analysis_cc_idx));
            self.box_captured_lexicals(code, &analysis_cc);
            // Lexicals the enclosing `supply { … }` body declared with `my`. The
            // `whenever` callbacks built below capture the live env and are
            // dispatched later from the emitting thread, whose ambient env is
            // the main script's — these names must resolve to the block's own
            // binding, not to a caller lexical that happens to share the name.
            // A name that is also a free var of this frame refers to the OUTER
            // binding for its pre-declaration uses, so it is not owned here.
            //
            // Restricted to a supply body on purpose: a `react { … }` block
            // compiles inline into the enclosing frame, so `my_declared_sym`
            // there is the WHOLE frame's declarations — including the lexicals
            // sibling `whenever`s are supposed to share (t/react-whenever-
            // shared-lexical.t). See `CompiledCode::is_supply_block_body`.
            //
            // A `whenever` registered from inside ANOTHER `whenever`'s body runs
            // in a chunk re-compiled from that callback's AST, which is not the
            // supply body and so computes nothing below. The enclosing callback's
            // own owned set rides in on `inherited_owned_lexicals` and applies to
            // both shapes.
            let mut owned_lexicals: Vec<crate::symbol::Symbol> =
                code.inherited_owned_lexicals.clone();
            if code.is_supply_block_body {
                let mut owned: Vec<crate::symbol::Symbol> = code
                    .my_declared_sym
                    .iter()
                    .filter(|sym| !code.free_var_syms.contains(sym))
                    .copied()
                    .collect();
                // The block's own emitter is its PARAMETER, not a `my`, so the
                // filter above never sees it — yet it is the one name that MUST
                // resolve to this instance. The generated name is unique per
                // parse site but shared by every runtime instance of that site,
                // so a supply chain built by calling the same routine twice
                // (`$s = xform($s); $s = xform($s)`, exactly what Cro's
                // middleware pipeline does) has two live instances of one block:
                // tapping the outer one runs the inner one's body, whose binding
                // then answered the OUTER body's `emit`, feeding the outer supply
                // its own output forever.
                if let Some(sym) = code.supply_emitter_sym
                    && !owned.contains(&sym)
                {
                    owned.push(sym);
                }
                // The body's never-written free variables are owned too. A supply
                // block body is a scope its caller never re-enters, and its
                // `whenever` callbacks are dispatched much later from an arbitrary
                // frame — so a lexical it captured must not be re-resolved against
                // whoever happens to dispatch it. Two live instances of one parse
                // site make this concrete: with
                // `Cro::HTTP::Router::RouteSet.transformer`'s
                // `supply { whenever $requests { … } }` instantiated for both the
                // outer route set and a delegated inner one, the inner body's
                // callback saw the OUTER `$requests`.
                //
                // Only `authoritative_free_vars` qualify — the captures the
                // *creating frame* vouched for as never written after the capture.
                // A capture the supply body writes is shared state whose updates
                // must reach the declaring frame (`whenever $s.on-close({ $closed
                // = True })`), and one the declaring frame reassigns after the
                // block was built must still be read live (`my $gate = 0; my $sup
                // = supply { … emit $gate … }; $gate = 9`) — freezing either here
                // would be a by-value snapshot that silently goes stale.
                for sym in &code.authoritative_free_vars {
                    if !owned.contains(sym) {
                        owned.push(*sym);
                    }
                }
                for sym in owned {
                    if !owned_lexicals.contains(&sym) {
                        owned_lexicals.push(sym);
                    }
                }
                self.share_supply_block_lexicals(code);
            }
            let tap = loan_env!(
                self,
                run_whenever_with_value(
                    supply_val,
                    yields_value,
                    &param,
                    &param_type,
                    body,
                    &owned_lexicals
                )
            )?;
            if yields_value {
                self.stack.push(tap);
            }
            Ok(())
        } else {
            Err(RuntimeError::new("WheneverScope expects Block body"))
        }
    }

    /// Walk the MRO of `class_name` to find a parameterized Array or Hash parent.
    /// Returns the element type if found (e.g. "Str" for `Array[Str]`).
    pub(super) fn find_parameterized_container_parent(&self, class_name: &str) -> Option<String> {
        let parents = self.class_parents_readonly(class_name);
        for parent in &parents {
            if let Some(inner) = parent
                .strip_prefix("Array[")
                .or_else(|| parent.strip_prefix("List["))
                .and_then(|s| s.strip_suffix(']'))
            {
                return Some(inner.trim().to_string());
            }
        }
        // Also check the class itself in case it IS a parameterized type
        if let Some(inner) = class_name
            .strip_prefix("Array[")
            .or_else(|| class_name.strip_prefix("List["))
            .and_then(|s| s.strip_suffix(']'))
        {
            return Some(inner.trim().to_string());
        }
        None
    }
}
