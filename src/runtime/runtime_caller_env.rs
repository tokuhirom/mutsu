use super::*;

impl Interpreter {
    pub(crate) fn push_caller_env(&mut self) {
        self.push_caller_env_with_code(None);
    }

    /// Push caller env with an explicit code (Sub) value for the current frame.
    pub(crate) fn push_caller_env_with_code(&mut self, code: Option<Value>) {
        self.caller_env_stack.push(self.env.clone());
        let file = self
            .env
            .get("?FILE")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let line = self.cur_source_line;
        let code = code.or_else(|| self.block_stack.last().cloned());
        self.callframe_stack.push(CallFrameEntry {
            file,
            line,
            code,
            env: self.env.clone(),
        });
    }

    pub(crate) fn pop_caller_env(&mut self) {
        self.caller_env_stack.pop();
        // Restore the caller's current line (recorded at push time). The
        // env-based `?LINE` got this for free from the `saved_env` restore;
        // the field must be rolled back explicitly.
        if let Some(entry) = self.callframe_stack.pop() {
            self.cur_source_line = entry.line;
        }
    }

    /// Push the caller frames an `EVAL` inserts between the EVAL'd unit's
    /// mainline and the scope that invoked EVAL. Rakudo runs the compiled unit
    /// behind two intermediate frames (the EVAL multi candidate and its proto),
    /// so from EVAL'd mainline code `CALLER::` / `CALLER::CALLER::` see frames
    /// with no user lexicals (lookups yield Nil) and the invoking scope is
    /// reached at `CALLER::CALLER::CALLER::`. Mirror that: push the invoking
    /// frame's env (the depth-3 target), then two empty frames.
    pub(crate) fn push_eval_caller_frames(&mut self) {
        self.push_caller_env();
        for _ in 0..2 {
            self.caller_env_stack.push(Env::new());
            self.callframe_stack.push(CallFrameEntry {
                file: "EVAL".to_string(),
                line: 0,
                code: None,
                env: Env::new(),
            });
        }
    }

    pub(crate) fn pop_eval_caller_frames(&mut self) {
        for _ in 0..3 {
            self.pop_caller_env();
        }
    }

    /// Pop caller env and apply any dynamic variable writes back to the given env.
    /// Use this instead of `pop_caller_env()` at function return sites that restore `saved_env`.
    pub(crate) fn pop_caller_env_with_writeback(&mut self, restored_env: &mut Env) {
        if let Some(popped) = self.caller_env_stack.pop() {
            for (key, value) in &popped {
                key.with_str(|key_str| {
                    // `$_`, `$/`, `$!` report as dynamic for `.VAR.dynamic` /
                    // CALLER:: purposes, but they are *not* propagated back to the
                    // caller on return: each routine gets its own topic/match/error
                    // and the caller's value is restored from `saved_env`. Writing
                    // the callee frame's stale copy back here clobbers the caller's
                    // live `$_` (e.g. a `block.($_)` call inside a `for` loop would
                    // revert the loop topic to the previous iteration's value).
                    let bare = Self::normalize_var_meta_name(key_str);
                    if matches!(bare, "_" | "/" | "!") {
                        return;
                    }
                    if self.is_var_dynamic(key_str) && restored_env.get_sym(*key) != Some(value) {
                        restored_env.insert_sym(*key, value.clone());
                    }
                });
            }
        }
        if let Some(entry) = self.callframe_stack.pop() {
            self.cur_source_line = entry.line;
        }
    }

    pub(crate) fn get_caller_line(&self, depth: usize) -> Option<Value> {
        let stack_len = self.callframe_stack.len();
        if depth == 0 || depth > stack_len {
            return None;
        }
        self.callframe_stack
            .get(stack_len - depth)
            .map(|entry| Value::int(entry.line))
    }

    /// Look up a variable through the $CALLER:: chain.
    /// `depth` is the number of CALLER:: levels (1 for $CALLER::x, 2 for $CALLER::CALLER::x).
    /// `name` is the bare variable name without sigil (e.g. "a" for $a).
    pub(crate) fn get_caller_var(&self, name: &str, depth: usize) -> Result<Value, RuntimeError> {
        let stack_len = self.caller_env_stack.len();
        if depth == 0 || depth > stack_len {
            // Walking past the top of the call stack is not an error in Raku:
            // the lookup just misses and yields Nil.
            return Ok(Value::NIL);
        }
        let env = &self.caller_env_stack[stack_len - depth];
        if let Some(val) = env.get(name) {
            // Check that the variable is declared `is dynamic`
            if !self.is_var_dynamic(name) {
                return Err(crate::runtime::utils::caller_not_dynamic_error(name));
            }
            Ok(val.clone())
        } else {
            // A name the target frame does not have resolves to Nil quietly
            // (rakudo's runtime pad lookup misses; only a *present* non-dynamic
            // symbol throws X::Caller::NotDynamic).
            Ok(Value::NIL)
        }
    }

    /// Set a variable through the $CALLER:: chain.
    pub(crate) fn set_caller_var(
        &mut self,
        name: &str,
        depth: usize,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let stack_len = self.caller_env_stack.len();
        if depth > stack_len {
            return Err(RuntimeError::new(format!(
                "Cannot access caller variable '${name}' — not enough caller frames"
            )));
        }
        if !self.is_var_dynamic(name) {
            return Err(crate::runtime::utils::caller_not_dynamic_error(name));
        }
        let idx = stack_len - depth;
        self.caller_env_stack[idx].insert(name.to_string(), value.clone());
        // Also update current env if the variable exists there
        if self.env.contains_key(name) {
            self.env.insert(name.to_string(), value);
        }
        // Single-store coherence: this write targets a *caller frame's* lexical by
        // name (`callframe(d).my.<$x> = v` / `$CALLER::x = v`). `pop_caller_env_
        // with_writeback` propagates it into the restored caller env on return, but
        // the caller's local *slot* is not refreshed when blanket reconcile is off,
        // so the caller reads the stale slot. Record the bare name so the call site
        // drains `env[name]` into that slot. Uses the caller-var list (retain-on-
        // miss) rather than the rw list, so an intervening deeper call the writer
        // makes before returning does not consume it one frame too soon. No-op when
        // no such slot exists; the default build's blanket reconcile makes it
        // redundant = byte-identical.
        self.record_caller_var_writeback(name);
        Ok(())
    }

    /// Record a by-name env write to a caller-frame lexical (retain-on-miss list).
    /// The owning local slot may live several frames up — the writer can make an
    /// intervening deeper call before returning to the owner — so the source is
    /// retained until a frame whose `code` actually has the slot drains it (see
    /// `apply_pending_caller_var_writeback`). Use this (not
    /// `pending_rw_writeback_sources`, which is drop-on-miss / single-frame) when
    /// the write happens inside a nested callee/closure (e.g. a Proxy STORE whose
    /// referent lexical is owned by the caller of the `$proxy = v` assignment).
    pub(crate) fn record_caller_var_writeback(&mut self, name: &str) {
        if !self.pending_caller_var_writeback.iter().any(|n| n == name) {
            self.pending_caller_var_writeback.push(name.to_string());
        }
    }

    /// Look up a variable through $DYNAMIC:: — searches the entire caller stack.
    pub(crate) fn get_dynamic_var(&self, name: &str) -> Result<Value, RuntimeError> {
        // Search from the most recent caller to the oldest
        for env in self.caller_env_stack.iter().rev() {
            if let Some(val) = env.get(name)
                && self.is_var_dynamic(name)
            {
                return Ok(val.clone());
            }
        }
        // Also check current env
        if let Some(val) = self.env.get(name)
            && self.is_var_dynamic(name)
        {
            return Ok(val.clone());
        }
        Err(RuntimeError::new(format!(
            "Cannot find dynamic variable '${name}'"
        )))
    }

    /// Bind a caller variable to a local variable ($CALLER::target := $source).
    /// This creates an alias so that future reads/writes of target go through source.
    pub(crate) fn bind_caller_var(
        &mut self,
        target_name: &str,
        source_name: &str,
        depth: usize,
    ) -> Result<(), RuntimeError> {
        let stack_len = self.caller_env_stack.len();
        if depth > stack_len {
            return Err(RuntimeError::new(format!(
                "Cannot access caller variable '${target_name}' — not enough caller frames"
            )));
        }
        if !self.is_var_dynamic(target_name) {
            return Err(RuntimeError::new(format!(
                "Cannot access '${target_name}' through CALLER, because it is not declared as dynamic"
            )));
        }
        // Copy the current value to the caller env and set up the binding alias
        let source_val = self.env.get(source_name).cloned().unwrap_or(Value::NIL);
        let idx = stack_len - depth;
        self.caller_env_stack[idx].insert(target_name.to_string(), source_val.clone());
        // Set up binding alias so reads of target_name resolve to source_name
        self.var_bindings
            .insert(target_name.to_string(), source_name.to_string());
        // Also update current env
        self.env.insert(target_name.to_string(), source_val);
        Ok(())
    }

    /// Resolve a variable name through bindings (follow aliases).
    pub(crate) fn resolve_binding(&self, name: &str) -> Option<&str> {
        // Empty-map fast exit: this runs on every GetLocal, and the common
        // program never creates a `$CALLER::x := ...` binding.
        if self.var_bindings.is_empty() {
            return None;
        }
        self.var_bindings.get(name).map(|s| s.as_str())
    }

    /// Save and clear var_bindings, returning the saved state.
    pub(crate) fn take_var_bindings(&mut self) -> HashMap<String, String> {
        std::mem::take(&mut self.var_bindings)
    }

    /// Restore previously saved var_bindings.
    pub(crate) fn restore_var_bindings(&mut self, bindings: HashMap<String, String>) {
        self.var_bindings = bindings;
    }
}
