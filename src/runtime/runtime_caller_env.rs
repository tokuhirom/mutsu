use super::*;
use crate::value::ValueMap;

impl Interpreter {
    pub(crate) fn push_caller_env(&mut self) {
        self.push_caller_env_with_code(None);
    }

    /// Copy the slot-only locals of the frame that is making this call (returned, for the `caller_env_stack` / `callframe_stack` entries pushed right
    /// after), so a by-name read of the immediate caller (`CALLER::<$x>`, `CALLER::`, `callframe`)
    /// sees them although no store mirrored them into `env` (#14337, #15048).
    ///
    /// `caller_code` is the address of the calling frame's live `CompiledCode`
    /// and `caller_base` the base of its slot window (`VmCallFrame::caller_code`
    /// / `saved_locals_base` for a call that already opened its frame, else the
    /// executing frame). Nothing is copied when the caller's chunk mirrors every
    /// store anyway. The entries hold them in a scoped overlay, so the flush
    /// costs O(k) in the copied locals, never a copy of the whole env.
    // Cost: O(k), k = the calling frame's slot-only locals; O(1) when none exist.
    fn flush_calling_frame_slots(
        &self,
        caller_code: usize,
        caller_base: usize,
    ) -> Vec<(crate::symbol::Symbol, Value)> {
        if caller_code == 0 {
            return Vec::new();
        }
        // SAFETY: `caller_code` is the address of the calling frame's
        // `CompiledCode`, which stays alive while the frame waits for this call to
        // return (see `VmCallFrame::caller_code`, `Interpreter::running_code`).
        let code = unsafe { &*(caller_code as *const crate::opcode::CompiledCode) };
        if code.mirrors_all_params() {
            return Vec::new();
        }
        let mut flushed: Vec<(crate::symbol::Symbol, Value)> = Vec::new();
        for (slot, sym) in code.locals_sym.iter().enumerate() {
            if code.needs_env_sync.get(slot).copied().unwrap_or(true) {
                continue;
            }
            let Some(value) = self.locals.absolute_slot(caller_base + slot) else {
                break;
            };
            // A slot nothing has written yet is a declaration still to come: by
            // name it must not hide an outer variable of the same name.
            if value.is_nil() {
                continue;
            }
            // Only the names a store would have mirrored: plain user variables.
            // `self`, `?CLASS`, `$_`, `$!`, `$/` and the internal keys are the
            // frame's own state, and an env key by their name would mislead the
            // readers that look those up env-first (ADR-14792).
            if !sym.with_str(|name| name != "self" && crate::opcode::is_plain_user_var_name(name)) {
                continue;
            }
            flushed.push((*sym, value.clone()));
        }
        flushed
    }

    /// Push caller env with an explicit code (Sub) value for the current frame.
    pub(crate) fn push_caller_env_with_code(&mut self, code: Option<Value>) {
        let frame = (self.current_code, self.locals.base());
        let flushed = if crate::opcode::caller_frame_read_possible() {
            self.flush_calling_frame_slots(frame.0, frame.1)
        } else {
            Vec::new()
        };
        self.push_caller_env_entry(code, flushed, frame);
    }

    /// [`Self::push_caller_env`] for a call whose callee frame is already open:
    /// the calling frame is the one `caller_code` / `caller_base` name (the
    /// callee's `VmCallFrame`), not the executing top of the slot stack.
    pub(crate) fn push_caller_env_of_frame(&mut self, caller_code: usize, caller_base: usize) {
        let flushed = if crate::opcode::caller_frame_read_possible() {
            self.flush_calling_frame_slots(caller_code, caller_base)
        } else {
            Vec::new()
        };
        self.push_caller_env_entry(None, flushed, (caller_code, caller_base));
    }

    fn push_caller_env_entry(
        &mut self,
        code: Option<Value>,
        flushed: Vec<(crate::symbol::Symbol, Value)>,
        frame: (usize, usize),
    ) {
        // The flushed locals sit in a scoped overlay on the entries' env, never
        // on the live env: a callee or closure resolving a free name through the
        // live chain must not meet the caller's variables there.
        let entry_env = if flushed.is_empty() {
            self.env.clone()
        } else {
            // A block tier, not a frame root: the entry is the calling frame,
            // whose root (and static link, which `CALLER::OUTER::` reads,
            // #18787) lies below the overlay.
            let mut env = match Env::open_block_tier(self.env.clone()) {
                Ok((env, _)) => env,
                Err(env) => Env::scoped_child(env),
            };
            for (sym, value) in &flushed {
                env.insert_sym(*sym, value.clone());
            }
            env
        };
        self.caller_env_stack.push(entry_env.clone());
        // The entry describes the frame this call is being made *from*, so its
        // file is the file the currently-executing routine was defined in.
        let file = self
            .executing_source_file_sym()
            .unwrap_or_else(|| Symbol::intern(""));
        let line = self.cur_source_line;
        let code = code
            .map(CodeFrame::Ready)
            .or_else(|| self.block_stack.last().cloned());
        self.callframe_stack.push(CallFrameEntry {
            file,
            line,
            code,
            env: entry_env,
            package: self.current_package_sym(),
            routine: self.routine_stack.last().copied(),
            synthetic: false,
            eval_frame: false,
            routine_depth: self.routine_stack.len(),
            flushed_frame: (!flushed.is_empty()).then_some(frame),
            flushed,
        });
    }

    /// Depth of the `callframe_stack`.
    // Cost: O(1).
    pub(crate) fn callframe_stack_len(&self) -> usize {
        self.callframe_stack.len()
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

    /// Current depth of the (paired) `caller_env_stack`/`callframe_stack`,
    /// for panic-unwind recovery (`Interpreter::recover_call_frames_after_panic`).
    /// This is a SEPARATE push/pop stack from `call_frames` -- pushed by
    /// `push_caller_env`/`push_caller_env_with_code`, popped by
    /// `pop_caller_env`/`pop_caller_env_with_writeback` -- so a panic caught
    /// mid-call leaves it holding every entry pushed since the boundary too,
    /// for exactly the reason `call_frames` did before
    /// `recover_call_frames_after_panic` existed. See
    /// `todo/deep/panic-unwind-leaks-side-channel-call-state.md`.
    pub(crate) fn caller_env_stack_depth(&self) -> usize {
        self.caller_env_stack.len()
    }

    /// `key`'s binding in the innermost caller frame that binds it in its
    /// own tiers, or `None`.
    // Cost: O(c * d), c = caller env entries, d = each frame's tiers.
    pub(crate) fn caller_frame_binding(&self, key: Symbol) -> Option<&Value> {
        self.caller_env_stack
            .iter()
            .rev()
            .find_map(|env| env.get_sym_in_frame(key))
    }

    /// Truncate `caller_env_stack`/`callframe_stack` back to `depth` (see
    /// [`Self::caller_env_stack_depth`]). The two are always pushed/popped
    /// together in lockstep (`push_caller_env_with_code`,
    /// `pop_caller_env`/`pop_caller_env_with_writeback`,
    /// `push_eval_caller_frames`/`pop_eval_caller_frames`), so a single depth
    /// recovers both. Deliberately a bare truncate (discard), NOT
    /// `pop_caller_env_with_writeback`'s dynamic-variable writeback: the env
    /// each discarded entry would have written into belongs to a callee frame
    /// that panic recovery has already discarded (via the `call_frames`
    /// pop-loop), so replaying those writes would write stale values into the
    /// now-restored caller env instead.
    pub(crate) fn truncate_caller_env_stack(&mut self, depth: usize) {
        self.caller_env_stack
            .truncate(depth.min(self.caller_env_stack.len()));
        self.callframe_stack
            .truncate(depth.min(self.callframe_stack.len()));
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
        if let Some(invoker) = self.callframe_stack.last_mut() {
            invoker.eval_frame = true;
        }
        for _ in 0..2 {
            self.caller_env_stack.push(Env::new());
            self.callframe_stack.push(CallFrameEntry {
                file: Symbol::intern("EVAL"),
                line: 0,
                code: None,
                env: Env::new(),
                package: self.current_package_sym(),
                routine: None,
                synthetic: true,
                eval_frame: true,
                routine_depth: self.routine_stack.len(),
                flushed: Vec::new(),
                flushed_frame: None,
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
        if let Some(popped) = self.caller_env_stack.pop()
            // The walk below writes back only keys `is_var_dynamic` answers
            // true for, and -- past the `$_`/`$/`/`$!` trio it skips first --
            // that is exactly the `is dynamic` flag table. With no such
            // declaration in the program the walk is a guaranteed no-op over
            // every key of the caller's env (a whole lexical scope per return
            // once a method dispatch has flattened it), so skip it outright.
            && !self.var_dynamic_flags_is_empty()
        {
            // A key can be dynamic only by the `is dynamic` flag table or the
            // builtin `$*` list (`is_var_dynamic`). Every builtin and every
            // twigil name is a `*` key, which the symbol's memoized flags answer
            // without a string lookup; so unless some flagged name lacks the
            // twigil (`my $x is dynamic`), every other key is skipped before
            // the per-key string work. The walk is once per routine return,
            // over the caller's whole visible env.
            let plain_dynamic = self
                .lexicals
                .var_dynamic_flags
                .iter()
                .any(|(name, &dynamic)| dynamic && !name.starts_with('*'));
            for (key, value) in &popped {
                if !plain_dynamic && !key.is_dynamic_var_env_key() {
                    continue;
                }
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
        self.caller_var_lookup(name, depth, true)
    }

    /// [`Self::get_caller_var`]; `caller_topic_strict` says whether the
    /// caller's `$_` is refused as not dynamic (`CALLER::`, #18988) rather
    /// than read (`CALLERS::` reads it).
    // Cost: O(t), t = the frame's tiers.
    pub(super) fn caller_var_lookup(
        &self,
        name: &str,
        depth: usize,
        caller_topic_strict: bool,
    ) -> Result<Value, RuntimeError> {
        // A name the target frame does not have resolves to Nil quietly
        // (rakudo's runtime pad lookup misses; only a *present* non-dynamic
        // symbol throws X::Caller::NotDynamic). Walking past the top of the
        // call stack is not an error either.
        Ok(self
            .caller_frame_var(name, depth, caller_topic_strict, false)?
            .unwrap_or(Value::NIL))
    }

    /// [`Self::caller_var_lookup`], `None` when the frame at `depth` (or a
    /// frame there at all) does not have the name.
    // Cost: O(t), t = the frame's tiers.
    /// `own_frame` looks only in that frame's own tiers, not through the
    /// frames that called it (a cascading search visits those itself, and
    /// reads each one's flushed slots there).
    pub(super) fn caller_frame_var(
        &self,
        name: &str,
        depth: usize,
        caller_topic_strict: bool,
        own_frame: bool,
    ) -> Result<Option<Value>, RuntimeError> {
        let stack_len = self.caller_env_stack.len();
        if depth == 0 || depth > stack_len {
            return Ok(None);
        }
        // `CALLER::OUTER::x` / `CALLER::UNIT::x`: the caller's lexical scopes.
        if let Some(found) = self.caller_lexical_chain_var(name, depth) {
            return Ok(Some(found));
        }
        let env = &self.caller_env_stack[stack_len - depth];
        // A sigil-less key is looked up as a term (a constant's term key,
        // #9962) first, then as itself (a lexical type), #17846.
        // A sigil-less key is reported as spelled, without a `$`.
        let reported = name;
        let lookup = |key: &str| {
            if own_frame {
                env.get_sym_in_frame(Symbol::intern(key))
            } else {
                env.get_through_callers(key)
            }
        };
        let (name, found) = match lookup(name) {
            Some(val) => (name, Some(val)),
            None => match name.strip_prefix(crate::runtime::term_names::TERM_PREFIX) {
                Some(bare) => (bare, lookup(bare)),
                None => (name, None),
            },
        };
        if let Some(val) = found {
            // Check that the variable is declared `is dynamic`
            if !self.is_var_dynamic(name)
                || (caller_topic_strict && self.caller_topic_is_not_dynamic(name))
            {
                return Err(crate::runtime::utils::caller_not_dynamic_error(reported));
            }
            Ok(Some(val.clone()))
        } else {
            Ok(None)
        }
    }

    /// Snapshot the lexical names in one caller frame for `CALLER::LEXICAL::`.
    // Cost: O(v), v = visible entries in the selected caller environment.
    pub(crate) fn caller_lexical_stash_entries(&self, depth: usize) -> ValueMap {
        let stack_len = self.caller_env_stack.len();
        let Some(env) = depth
            .checked_sub(1)
            .and_then(|offset| stack_len.checked_sub(1 + offset))
            .and_then(|index| self.caller_env_stack.get(index))
        else {
            return ValueMap::default();
        };
        let merged = env.filtered_flat(&|_, _| true);
        let mut entries = ValueMap::default();
        let mut block_subs = Vec::new();
        for (key, value) in merged.iter() {
            let key = key.resolve();
            // A block-scoped `sub` lives in the frame's slot; the env holds
            // its escape-hatch copy under the block-sub prefix. The caller's
            // pad names it `&name`, as `MY::` does in that frame.
            if let Some(name) = key.strip_prefix(crate::env::BLOCK_LEXICAL_SUB_PREFIX) {
                block_subs.push((format!("&{name}"), value.clone()));
                continue;
            }
            if self.should_hide_from_my_global_stash(&key) {
                continue;
            }
            entries.insert(Self::add_sigil_prefix(&key), value.clone());
        }
        for (key, value) in block_subs {
            entries.entry(key).or_insert(value);
        }
        // The caller's file-scope and package routines are not in its env
        // snapshot (#17733).
        self.add_caller_scope_routines(&mut entries, depth);
        entries
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
        if !self.is_var_dynamic(name) || self.caller_topic_is_not_dynamic(name) {
            return Err(crate::runtime::utils::caller_not_dynamic_error(name));
        }
        if name == "_" && depth == 1 {
            // `$CALLER::_ = v` / `$CALLER::_++`: the caller's topic, which
            // the return-time writeback deliberately skips (each routine has
            // its own `$_`), and which is not the callee's own `$_` either.
            self.write_caller_topic(value);
            return Ok(());
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

    /// `CALLER::LEXICAL::<$x> = v`: [`Self::set_caller_var`] without the
    /// dynamic-variable requirement (`LEXICAL::` reaches any lexical).
    // Cost: O(1).
    pub(crate) fn set_caller_lexical_var(
        &mut self,
        name: &str,
        depth: usize,
        value: Value,
    ) -> Result<(), RuntimeError> {
        // `caller_env_stack` omits light/inlined call paths while each CALLER
        // component counts a semantic routine frame, so validate against
        // `routine_stack` and let the runtime-name carrier cross the physical
        // frames (the same scheme as `CALLER::<$x> := v`, `bind_stash_key`).
        if depth == 0 || depth > self.routine_stack.len() {
            return Err(RuntimeError::new(format!(
                "Cannot access caller variable '${name}' - not enough caller frames"
            )));
        }
        if name == "_" {
            self.write_caller_topic(value);
        } else {
            self.env_mut().insert(name.to_string(), value);
            self.record_runtime_name_write(name);
        }
        Ok(())
    }

    /// Write `value` into the calling routine's topic `$_`. The topic is
    /// per-frame, so a general runtime-name write would replay the callee's
    /// topic through every intervening frame; this writes directly into the
    /// current call frame's saved caller env instead, and the caller-var drain
    /// updates the caller's authoritative local slot once when this frame
    /// returns.
    // Cost: O(1).
    fn write_caller_topic(&mut self, value: Value) {
        if let Some(frame) = self.call_frames.last_mut() {
            if let Some(ValueView::ContainerRef(cell)) =
                frame.saved_env.get("_").map(Value::view)
            {
                cell.store(value);
            } else {
                frame.saved_env.insert("_".to_string(), value);
            }
            self.record_caller_var_writeback("_");
        }
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
        if !self.pending_caller_var_writeback.contains(name) {
            self.pending_caller_var_writeback.insert(name.to_string());
        }
    }

    /// [`Self::record_caller_var_writeback`] for a write whose TARGET NAME was
    /// resolved at RUN TIME (`$::($n) = v`, `::('$x') = v`, an assignment inside an
    /// `EVAL`'d snippet). Such a name is invisible to the compiler, so on top of
    /// the slot refresh it also needs its VALUE carried across each frame exit
    /// between the writer and the frame that declares the lexical — see
    /// [`Self::pending_runtime_name_writes`] and `propagate_pending_caller_writes`.
    ///
    /// Plain user lexicals qualify, and so do dynamics (`$*x`): a callee's
    /// rebind (`$*x := ...`) or by-name bind (`nqp::bindlexdyn`) of a dynamic
    /// its caller declared is carried the same way to the declaring frame
    /// (ADR-12529 §3, slice 40); the op sites record only a dynamic the running
    /// code does not declare itself. A system name (`&?BLOCK`, the topic, `$/`,
    /// `__mutsu_*`) is per-frame by construction and replaying it into a caller
    /// corrupts that caller's own binding.
    pub(crate) fn record_runtime_name_write(&mut self, name: &str) {
        if !crate::env::is_plain_user_lexical(name) && !crate::env::is_dynamic_var_env_key(name) {
            return;
        }
        self.record_caller_var_writeback(name);
        if !self.pending_runtime_name_writes.iter().any(|n| n == name) {
            self.pending_runtime_name_writes.push(name.to_string());
        }
    }

    /// Collect the dynamic variables (`$*x` / `@*x` / `%*x`) visible in the
    /// current dynamic scope — the whole caller chain plus the current frame —
    /// keyed by their full `$*name` spelling. Backs the `DYNAMIC::` pseudo-stash
    /// (`DYNAMIC::<$*x>`, `DYNAMIC::<$*x>:exists`, `DYNAMIC::.keys`), which must
    /// find a dynamic bound in ANY caller — including one `:=`-bound to a value
    /// with no Scalar container (roast pseudo-6c "Bound dynamics"). Innermost
    /// frame wins: the current env overlays older callers.
    pub(crate) fn dynamic_pseudo_stash_entries(&self) -> ValueMap {
        let mut entries: ValueMap = ValueMap::default();
        // oldest caller first, current env last, so the nearest binding wins.
        let frames = self
            .caller_env_stack
            .iter()
            .chain(std::iter::once(&self.env));
        for env in frames {
            // Chain-aware: walk this frame's WHOLE overlay-parent chain
            // (outermost tier first, nearer tiers overwriting, tombstoned
            // keys suppressed) instead of `env.iter()`, which sees only the
            // top overlay tier. Any frame that runs under a scoped overlay
            // without pushing its own caller-env entry (every compiled
            // method body, and two latent sub-side shapes -- a sub with a
            // positional param, and a frameless overlay intermediate between
            // writer and reader) otherwise hides dynamics living in its
            // parent tiers from this walk even though `Env::get()` would
            // find them. See ADR-0035 Mechanism 1 (docs/adr/0035-method-calls-observe-caller-frames.md).
            let merged = env.filtered_flat(&|_, _| true);
            // The built-in dynamics live in the per-interpreter base tier, not
            // in any env's own map (ADR-0086), and `iter()` is map-only — so
            // ask for them explicitly, before the map's entries, which shadow
            // them. Without this, `DYNAMIC::<$*OUT>` / `PROCESS::<$OUT>` would
            // stop finding what the interpreter seeded.
            for (k, v) in merged
                .visible_base_dynamics()
                .iter()
                .map(|(k, v)| (k, v))
                .chain(merged.iter())
            {
                let key = k.resolve();
                let spelled = if let Some(n) = key.strip_prefix("@*") {
                    format!("@*{n}")
                } else if let Some(n) = key.strip_prefix("%*") {
                    format!("%*{n}")
                } else if let Some(n) = key.strip_prefix('*') {
                    format!("$*{n}")
                } else {
                    continue;
                };
                entries.insert(spelled, v.clone().into_deref());
            }
        }
        entries
    }

    /// The `PROCESS::` stash's entries: the process-level dynamics, keyed by
    /// their stash spelling (`$OUT`, `@ARGS`, `%ENV`). That is the
    /// interpreter-seeded base tier, every binding a caller frame made without
    /// declaring it lexically (`$*OUT = $fh`), and the durable
    /// `PROCESS::<$name> = ...` installs -- but never a frame's own
    /// `my $*OUT`, which shadows the process value only for `$*OUT` lookups
    /// (Tee reads `PROCESS::<$OUT>` while a caller holds `my $*OUT`).
    // Cost: O(f * e), f = caller frames, e = entries of a frame's env chain.
    pub(crate) fn process_stash_entries(&self) -> ValueMap {
        fn stash_key(env_key: &str) -> Option<String> {
            if let Some(n) = env_key.strip_prefix("@*") {
                Some(format!("@{n}"))
            } else if let Some(n) = env_key.strip_prefix("%*") {
                Some(format!("%{n}"))
            } else if let Some(n) = env_key.strip_prefix("&*") {
                Some(format!("&{n}"))
            } else {
                env_key.strip_prefix('*').map(|n| format!("${n}"))
            }
        }
        let mut entries: ValueMap = ValueMap::default();
        if let Some(base) = self.env.dyn_base() {
            for (k, v) in base.iter() {
                if let Some(key) = stash_key(&k.resolve()) {
                    entries.insert(key, v.clone().into_deref());
                }
            }
        }
        let frames = self
            .caller_env_stack
            .iter()
            .chain(std::iter::once(&self.env));
        for env in frames {
            let merged = env.filtered_flat(&|_, _| true);
            for (k, v) in merged.iter() {
                let env_key = k.resolve();
                let Some(key) = stash_key(&env_key) else {
                    continue;
                };
                if merged
                    .contains_key(crate::meta_ns::MetaNs::LexicalDynamic.str_key_for_str(&env_key))
                {
                    continue;
                }
                entries.insert(key, v.clone().into_deref());
            }
        }
        // The process-level routines rakudo installs (`&*chdir`).
        for name in crate::runtime::process_routines::PROCESS_ROUTINES {
            entries
                .entry(format!("&{name}"))
                .or_insert_with(|| self.resolve_code_var(&format!("*{name}")));
        }
        for (env_key, v) in self.lexicals.process_dynamics.entries() {
            if let Some(key) = stash_key(&env_key) {
                entries.insert(key, v.clone().into_deref());
            }
        }
        entries
    }

    /// The client of the running code: the code that called into it, as
    /// `CLIENT::` names it. `own_file` is the running code's own unit. A
    /// light call pushes no routine frame, so the frames of that unit, the
    /// running routine's included when it has one, are skipped by their
    /// declaring file; the first frame declared elsewhere is the client, and
    /// its unit's language revision is what `CORE-SETTING-REV` reports. With
    /// no such frame the client is the mainline (`GLOBAL`, the revision of
    /// the file that made the outermost call). Returns the client's package
    /// and revision letter.
    /// The fallback of `client_code` (`vm_client_code`), for code with no
    /// recorded package or a call chain that ends before the client.
    // Cost: O(f), f = routine frames, plus one revision-table probe.
    pub(crate) fn client_frame(&self, own_file: Option<Symbol>) -> (Symbol, char) {
        let frames: &[crate::runtime::RoutineFrame] = &self.routine_stack;
        let revision_of = Self::unit_revision_letter;
        let declared_in = |frame: &crate::runtime::RoutineFrame| frame.def_file.or(frame.file);
        match frames
            .iter()
            .rev()
            .find(|frame| own_file.is_none_or(|own| declared_in(frame) != Some(own)))
        {
            Some(client) => (
                client.package,
                revision_of(declared_in(client)).unwrap_or_else(|| self.running_revision_letter()),
            ),
            None => (
                crate::symbol::wk::global_package(),
                revision_of(frames.first().and_then(|frame| frame.file).or(own_file))
                    .unwrap_or_else(|| self.running_revision_letter()),
            ),
        }
    }

    /// The letter of the language revision the unit `file` is compiled
    /// under (`d` for `6.d`), when it recorded one.
    // Cost: O(1), one revision-table probe.
    pub(crate) fn unit_revision_letter(file: Option<Symbol>) -> Option<char> {
        file.and_then(crate::parser::unit_revisions::unit_language_revision)
            .and_then(|version| {
                version
                    .as_str()
                    .strip_prefix("6.")
                    .and_then(|r| r.chars().next())
            })
    }

    /// The letter of the language revision the running code is under.
    // Cost: O(1).
    pub(crate) fn running_revision_letter(&self) -> char {
        ['e', 'd', 'c']
            .into_iter()
            .find(|r| crate::parser::current_language_version_starts_with(&format!("6.{r}")))
            .unwrap_or('d')
    }

    /// The `GLOBAL::<$x> = ...` stores of the dynamic chain, keyed as the
    /// `$*x` they are a 6.e fallback for: such a store keeps the qualified
    /// spelling in the scope that ran it, which may be any frame of the chain.
    // Cost: O(f * e), f = caller frames, e = entries of a frame's env chain.
    pub(crate) fn global_qualified_dynamics(&self) -> Vec<(String, Value)> {
        let mut qualified: Vec<(String, Value)> = Vec::new();
        let frames = self
            .caller_env_stack
            .iter()
            .chain(std::iter::once(&self.env));
        for env in frames {
            for (key, value) in env.flattened().iter() {
                let key = key.resolve();
                let (sigil, rest) = match key.chars().next() {
                    Some(c @ ('@' | '%')) => (c, &key[1..]),
                    _ => ('$', key.as_str()),
                };
                if let Some(bare) = rest.strip_prefix("GLOBAL::")
                    && bare.starts_with(|c: char| c.is_alphabetic() || c == '_')
                    && !crate::qualified::is_qualified_str(bare)
                {
                    qualified.push((format!("{sigil}*{bare}"), value.clone()));
                }
            }
        }
        qualified
    }

    /// Store `value` under the qualified env key `key` (`GLOBAL::x`) in the
    /// nearest frame of the dynamic chain that holds it, as the store that
    /// put it there did. Returns whether some frame held it.
    // Cost: O(f), f = caller frames, one env probe each.
    pub(crate) fn set_in_holding_frame(&mut self, key: &str, value: Value) -> bool {
        if self.env.contains_key(key) {
            self.env.insert(key.to_string(), value);
            return true;
        }
        for env in self.caller_env_stack.iter_mut().rev() {
            if env.contains_key(key) {
                env.insert(key.to_string(), value);
                return true;
            }
        }
        false
    }

    /// Look up a variable through $DYNAMIC:: — searches the entire caller stack.
    pub(crate) fn get_dynamic_var(&self, name: &str) -> Result<Value, RuntimeError> {
        // Search from the most recent caller to the oldest
        for env in self.caller_env_stack.iter().rev() {
            if let Some(val) = env.get(name)
                && self.is_var_dynamic(name)
            {
                return Ok(val.deref_container());
            }
        }
        // Also check current env
        if let Some(val) = self.env.get(name)
            && self.is_var_dynamic(name)
        {
            return Ok(val.deref_container());
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
        // The caller's own `$_` is not dynamic, but a bind reaches it
        // (`$CALLER::_ := $v`), as `CALLER::<$_> := $v` does (#18988).
        if target_name == "_" && depth == 1 {
            let source_val = self.env.get(source_name).cloned().unwrap_or(Value::NIL);
            self.bind_caller_implicit_lexical("_", source_val);
            return Ok(());
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
        // (latched process-wide: the Tier B inline GetLocal fast path relies
        // on a zero counter to skip resolve_binding — see CALLER_VAR_BINDS).
        crate::vm::vm_jit::note_caller_var_binding();
        self.lexicals
            .var_bindings
            .insert(target_name.to_string(), source_name.to_string());
        // Also update current env
        self.env.insert(target_name.to_string(), source_val);
        Ok(())
    }

    /// Resolve a variable name through bindings (follow aliases).
    pub(crate) fn resolve_binding(&self, name: &str) -> Option<&str> {
        // Empty-map fast exit: this runs on every GetLocal, and the common
        // program never creates a `$CALLER::x := ...` binding.
        if self.lexicals.var_bindings.is_empty() {
            return None;
        }
        self.lexicals.var_bindings.get(name).map(|s| s.as_str())
    }

    /// Save and clear var_bindings, returning the saved state.
    pub(crate) fn take_var_bindings(&mut self) -> HashMap<String, String> {
        std::mem::take(&mut self.lexicals.var_bindings)
    }

    /// Restore previously saved var_bindings.
    pub(crate) fn restore_var_bindings(&mut self, bindings: HashMap<String, String>) {
        self.lexicals.var_bindings = bindings;
    }
}
