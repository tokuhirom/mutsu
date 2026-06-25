use super::*;

impl Interpreter {
    /// Collapse the interpreter's env to a flat (`parent=None`) env if it is
    /// currently a scoped overlay. No-op (O(1)) for a flat env. Used at every
    /// boundary that would otherwise let a transient scoped env survive into code
    /// that captures or iterates it overlay-only. See docs/vm-dual-store.md (Slice 6).
    #[inline]
    pub(super) fn flatten_scoped_env(&mut self) {
        if self.env().is_scoped() {
            let flat = self.env().flattened();
            *self.env_mut() = flat;
        }
    }

    /// Save the current env, locals, stack depth, and readonly vars into a new
    /// call frame.
    pub(super) fn push_call_frame(&mut self) {
        crate::vm::vm_stats::record_clone_env();
        // Save the caller env by an O(1) clone (an Arc bump, even when scoped):
        // it is only restored on return, so it need not be flattened. The callee
        // chains a fresh overlay over it (multi-tier), so nested calls no longer
        // pay the per-call O(env) flatten. Long-lived captures (Sub closures, END
        // phasers, threads) still flatten via `clone_env` at the capture site.
        let frame = VmCallFrame {
            saved_env: self.env().clone(),
            saved_readonly: Some(self.save_readonly_vars()),
            readonly_added: Vec::new(),
            saved_locals: std::mem::take(&mut self.locals),
            saved_stack_depth: self.stack.len(),
            saved_local_bind_pairs: std::mem::take(&mut self.local_bind_pairs),
        };
        self.call_frames.push(frame);
    }

    /// Lightweight call frame for simple methods: skips saving readonly vars
    /// since simple methods don't use `:=` binding.
    pub(super) fn push_light_call_frame(&mut self) {
        crate::vm::vm_stats::record_clone_env();
        let frame = VmCallFrame {
            saved_env: self.env().clone(),
            saved_readonly: None,
            readonly_added: Vec::new(),
            saved_locals: std::mem::take(&mut self.locals),
            saved_stack_depth: self.stack.len(),
            saved_local_bind_pairs: std::mem::take(&mut self.local_bind_pairs),
        };
        self.call_frames.push(frame);
    }

    /// Pop the most recent call frame and restore locals and readonly vars.
    /// Returns the frame so callers can access `saved_env` for site-specific merge logic.
    pub(super) fn pop_call_frame(&mut self) -> VmCallFrame {
        let mut frame = self
            .call_frames
            .pop()
            .expect("pop_call_frame: no frame to pop");
        self.locals = std::mem::take(&mut frame.saved_locals);
        self.local_bind_pairs = std::mem::take(&mut frame.saved_local_bind_pairs);
        if let Some(readonly) = std::mem::take(&mut frame.saved_readonly) {
            // Slow path: restore the whole snapshot (covers any `:=` marking).
            self.restore_readonly_vars(readonly);
        } else if !frame.readonly_added.is_empty() {
            // Light-frame method path: drop only the param names this frame added.
            for name in &frame.readonly_added {
                self.unmark_readonly(name);
            }
        }
        frame
    }

    fn twigil_dynamic_alias(name: &str) -> Option<String> {
        if let Some(rest) = name.strip_prefix("$*") {
            return Some(format!("*{}", rest));
        }
        if let Some(rest) = name.strip_prefix('*') {
            return Some(format!("$*{}", rest));
        }
        None
    }

    fn main_unqualified_name(name: &str) -> Option<String> {
        for sigil in ["$", "@", "%", "&"] {
            let prefix = format!("{sigil}Main::");
            if let Some(rest) = name.strip_prefix(&prefix) {
                return Some(format!("{sigil}{rest}"));
            }
        }
        None
    }

    /// Look up an `our`-scoped variable by trying the bare (unqualified) name
    /// after stripping pseudo-package prefixes like GLOBAL::, OUR::, etc.
    pub(super) fn our_var_pseudo_unqualified(&self, name: &str) -> Option<Value> {
        Self::pseudo_package_unqualified_name(name)
            .and_then(|bare| self.get_our_var(&bare).cloned())
    }

    /// Strip pseudo-package qualifiers (GLOBAL::, OUR::, MY::) from a
    /// sigiled variable name, returning the bare variable name.
    /// e.g. "$GLOBAL::x" → Some("x"), "$OUR::x" → Some("x")
    fn pseudo_package_unqualified_name(name: &str) -> Option<String> {
        let pseudo = ["GLOBAL", "OUR", "MY"];
        for sigil in ["$", "@", "%", "&"] {
            if let Some(rest) = name.strip_prefix(sigil) {
                for pkg in &pseudo {
                    let prefix = format!("{pkg}::");
                    if let Some(bare) = rest.strip_prefix(&prefix) {
                        return Some(format!("{sigil}{bare}"));
                    }
                }
            }
        }
        // Also handle unsigiled forms (e.g. "GLOBAL::x")
        for pkg in &pseudo {
            let prefix = format!("{pkg}::");
            if let Some(bare) = name.strip_prefix(&prefix) {
                return Some(bare.to_string());
            }
        }
        None
    }

    fn main_qualified_name(name: &str) -> Option<String> {
        for sigil in ["$", "@", "%", "&"] {
            if let Some(rest) = name.strip_prefix(sigil)
                && !rest.contains("::")
            {
                return Some(format!("{sigil}Main::{rest}"));
            }
        }
        None
    }

    /// Look up a sigiled variable name (e.g. "@z") in the locals array by its
    /// bare (sigil-stripped) name.  This handles function parameters that are
    /// stored without sigils in the compiled-function locals.
    pub(super) fn get_local_by_bare_name(&self, code: &CompiledCode, name: &str) -> Option<Value> {
        // Strip the leading sigil (@, %)
        let bare = name.strip_prefix('@').or_else(|| name.strip_prefix('%'))?;
        // Respect lexical shadowing by resolving the most recently-declared local.
        let idx = code.locals.iter().rposition(|n| n == bare)?;
        Some(self.locals.get(idx)?.clone())
    }

    pub(super) fn get_env_with_main_alias(&self, name: &str) -> Option<Value> {
        // Raku allows underscore variants of kebab-case identifiers
        // (e.g. $*EXECUTABLE_NAME is equivalent to $*EXECUTABLE-NAME).
        // Try the kebab-case equivalent first if the name contains underscores.
        if name.contains('_') {
            let kebab = name.replace('_', "-");
            if let Some(val) = self.get_env_with_main_alias_inner(&kebab) {
                return Some(val);
            }
        }
        self.get_env_with_main_alias_inner(name)
    }

    fn get_env_with_main_alias_inner(&self, name: &str) -> Option<Value> {
        // Atomic array CAS stores the authoritative copy under an internal key.
        // Always check it first so both thread-clone and non-clone reads
        // observe the latest CAS'd value.
        if name.starts_with('@') {
            let atomic_key = format!("__mutsu_atomic_arr::{name}");
            if let Some(v) = self.get_shared_var(&atomic_key) {
                return Some(v);
            }
        }
        // Thread-clone @/% lookups must prefer the shared copy. Child thread
        // env snapshots can lag behind sibling mutations even when Lock::Async
        // serializes the writes through shared_vars.
        if self.is_thread_clone()
            && (name.starts_with('@') || name.starts_with('%'))
            && let Some(v) = self.get_shared_var(name)
        {
            return Some(v);
        }
        // Otherwise check local env first so that function parameters and
        // lexical variables take precedence over shared_vars. Without this,
        // recursive `start` blocks can read stale parameter values from
        // shared_vars instead of the locally-bound ones.
        if let Some(val) = self.env().get(name) {
            return Some(val.clone());
        }
        // Anonymous scalar placeholders (from bare `$`) are invocation-local.
        if name.starts_with("__ANON_STATE_") {
            return None;
        }
        // Fall back to shared_vars for cross-thread visibility of variables
        // that were explicitly updated by other threads.
        if let Some(v) = self.get_shared_var(name) {
            return Some(v);
        }
        // Follow binding aliases ($CALLER::target := $source)
        if let Some(resolved) = self.resolve_binding(name)
            && let Some(val) = self.env().get(resolved)
        {
            return Some(val.clone());
        }
        if let Some(alias) = Self::twigil_dynamic_alias(name) {
            return self.env().get(&alias).cloned();
        }
        if let Some(alias) = Self::main_unqualified_name(name) {
            return self.env().get(&alias).cloned();
        }
        if let Some(qualified) = Self::main_qualified_name(name) {
            return self.env().get(&qualified).cloned();
        }
        // Strip GLOBAL::, OUR::, MY:: pseudo-package qualifiers to find
        // the variable under its bare name in the environment.
        if let Some(bare) = Self::pseudo_package_unqualified_name(name) {
            return self.env().get(&bare).cloned();
        }
        // Placeholder block parameters are stored as "^name". Allow lexical
        // access by the de-careted name inside the same block.
        if !name.starts_with('^') {
            let placeholder = format!("^{name}");
            if let Some(val) = self.env().get(&placeholder) {
                return Some(val.clone());
            }
            if let Some(val) = self.get_shared_var(&placeholder) {
                return Some(val);
            }
        }
        None
    }

    pub(super) fn set_env_with_main_alias(&mut self, name: &str, value: Value) {
        self.set_env_with_main_alias_sym(name, None, value);
    }

    /// Like `set_env_with_main_alias` but accepts a pre-interned Symbol
    /// to avoid Symbol::intern() overhead on hot paths.
    pub(super) fn set_env_with_main_alias_sym(
        &mut self,
        name: &str,
        _name_sym: Option<Symbol>,
        value: Value,
    ) {
        // Slice B (docs/vm-single-store.md): while a carrier (EVAL / interpreter
        // fallback) is active, log every by-name env write so the carrier-return
        // writeback can reconcile exactly these names into the caller's slots
        // (replacing the blanket `env_dirty` pull). Logging a superset is safe —
        // the writeback filters by the caller's compiled-local slots.
        if let Some(set) = self.carrier_writes.as_mut() {
            set.insert(name.to_string());
        }
        if name.starts_with("__ANON_STATE_") {
            self.env_mut().insert(name.to_string(), value);
            return;
        }
        if !name.starts_with('^') {
            let placeholder = format!("^{name}");
            if self.env().contains_key(&placeholder) {
                loan_env!(self, set_shared_var(&placeholder, value.clone()));
                self.env_mut().insert(placeholder, value);
                return;
            }
        }
        loan_env!(self, set_shared_var(name, value.clone()));
        if let Some(alias) = Self::twigil_dynamic_alias(name) {
            self.env_mut().insert(alias, value.clone());
        }
        if let Some(inner) = name
            .strip_prefix("&infix:<")
            .or_else(|| name.strip_prefix("&prefix:<"))
            .or_else(|| name.strip_prefix("&postfix:<"))
            && let Some(op_name) = inner.strip_suffix('>')
        {
            self.env_mut()
                .insert(format!("&{}", op_name), value.clone());
        }
        if let Some(alias) = Self::main_unqualified_name(name) {
            self.env_mut().insert(alias, value);
            return;
        }
        if let Some(qualified) = Self::main_qualified_name(name)
            && self.env().contains_key(&qualified)
        {
            self.env_mut().insert(qualified, value);
            return;
        }
        // Write through GLOBAL::, OUR::, MY:: pseudo-package qualifiers to the
        // bare variable name in the environment.
        if let Some(bare) = Self::pseudo_package_unqualified_name(name) {
            self.env_mut().insert(bare, value);
        } else if let Some(bare) = name.strip_prefix("GLOBAL::")
            && self.env().contains_key(bare)
        {
            self.env_mut().insert(bare.to_string(), value);
        }
    }

    /// Record a by-name caller-lexical env write that did NOT flow through
    /// `set_env_with_main_alias` (the single by-name writer that auto-logs). Some
    /// ops mutate a caller lexical with a direct `env_mut().insert` — e.g. the
    /// `$x ~~ s///` writeback to its LHS variable. For the reverse sync to stay
    /// precise *and* complete (Slice C', open-question #2), every such write must
    /// log into the active carrier set so the carrier-return
    /// `writeback_carrier_writes` reconciles the caller slot — without this, an
    /// EVAL/carrier that dropped its blanket net (a fully-reconciled bareword
    /// carrier, #3227/#3231) would silently lose the write. Call this right after
    /// the direct insert, for each caller-visible name written.
    pub(crate) fn note_caller_env_write(&mut self, name: &str) {
        if let Some(set) = self.carrier_writes.as_mut() {
            set.insert(name.to_string());
        }
    }

    /// Begin a carrier region: start logging by-name env writes. Returns the
    /// previously-active log (if a carrier was already running, e.g. nested
    /// EVAL) so the caller can restore + merge on `end_carrier`. See Slice B.
    pub(super) fn begin_carrier(&mut self) -> Option<std::collections::HashSet<String>> {
        self.carrier_writes
            .replace(std::collections::HashSet::new())
    }

    /// End a carrier region: take this carrier's logged names and restore the
    /// outer carrier's log (merging this carrier's names into it so an enclosing
    /// carrier also reconciles any of its own frame's slots the inner carrier
    /// wrote). Returns the names this carrier wrote, for the caller to hand to
    /// `writeback_carrier_writes` — but only on the success path: if the carrier
    /// errored, the frame unwinds, so reconciling its slots is moot (yet the log
    /// state must still be restored here so a `Some` does not leak into a CATCH).
    pub(super) fn end_carrier(
        &mut self,
        saved: Option<std::collections::HashSet<String>>,
    ) -> std::collections::HashSet<String> {
        let written = self.carrier_writes.take().unwrap_or_default();
        self.carrier_writes = saved.map(|mut s| {
            s.extend(written.iter().cloned());
            s
        });
        written
    }

    /// Resolve the env value backing local `name` (with the sigil-stripped
    /// fallback the blanket reconcile uses). Raw — no type filter.
    fn carrier_env_value(&self, name: &str) -> Option<Value> {
        self.env().get(name).cloned().or_else(|| {
            name.strip_prefix('$')
                .or_else(|| name.strip_prefix('@'))
                .or_else(|| name.strip_prefix('%'))
                .or_else(|| name.strip_prefix('&'))
                .and_then(|b| self.env().get(b).cloned())
        })
    }

    /// Whether the carrier writeback may overwrite a slot *currently* holding `v`.
    /// Eligibility is keyed on what the slot WOULD LOSE, not on the incoming value
    /// (so an `Int` slot that a carrier turns into a `Mixin` via `does` is fine).
    /// Excludes the binding cells (`HashEntryRef`/`ContainerRef`) and a plain
    /// `Array`/`Hash` slot — env may hold a COW-detached copy whose write would
    /// clobber a live interior `:=` element cell. Scalars, Set/Bag/Mix, Mixin,
    /// Instance and the rest are safe targets (the whole slot value is replaced).
    /// (Note: an Instance mutated *in place* through its shared `Arc<RwLock>`
    /// attribute cell snapshots equal pre/post, so the diff never fires for it —
    /// only a genuine value/type change writes through.)
    fn slot_carrier_overwritable(v: &Value) -> bool {
        !matches!(
            v,
            Value::HashEntryRef { .. }
                | Value::ContainerRef(_)
                | Value::Array(..)
                | Value::Hash(..)
        )
    }

    /// Snapshot the slot-backing env values before a `lives-ok`/`dies-ok` carrier
    /// runs, for the locals whose slot is overwritable. `None` entries (cell /
    /// Array / Hash slots, or absent env keys) are never written back.
    pub(super) fn snapshot_carrier_overwritable_env(
        &self,
        code: &CompiledCode,
    ) -> Vec<Option<Value>> {
        code.locals
            .iter()
            .enumerate()
            .map(|(i, name)| {
                if name.starts_with('!') || !Self::slot_carrier_overwritable(&self.locals[i]) {
                    None
                } else {
                    self.carrier_env_value(name)
                }
            })
            .collect()
    }

    /// Post-carrier (`lives-ok { … }` / block Test fn) precise writeback: for each
    /// overwritable slot whose env value changed during the carrier, write the new
    /// env value through to the slot. The double-OFF replacement for the blanket
    /// reconcile, restricted to slots that carry no live `:=` cell.
    pub(super) fn carrier_writeback_changed_aggregates(
        &mut self,
        code: &CompiledCode,
        pre_env: &[Option<Value>],
    ) {
        for (i, name) in code.locals.iter().enumerate() {
            if name.starts_with('!')
                || matches!(
                    self.locals[i],
                    Value::HashEntryRef { .. } | Value::ContainerRef(_)
                )
            {
                continue;
            }
            let Some(cur) = self.carrier_env_value(name) else {
                continue;
            };
            if !Self::slot_carrier_overwritable(&self.locals[i]) {
                // A plain Array/Hash slot: overwriting from env normally risks
                // clobbering a live interior `:=` element cell (env may hold a
                // COW-detached copy). The one safe case is a *type change away* from
                // the container — `$a does Role` turns a Hash `$a` into a `Mixin`,
                // discarding the old container wholesale — so write through only
                // when the env value's variant differs from the slot's.
                if std::mem::discriminant(&self.locals[i]) != std::mem::discriminant(&cur) {
                    self.locals[i] = cur;
                }
                continue;
            }
            // "Changed" must catch a *variant* change even when `PartialEq` treats
            // the two values as equal: `Value::Mixin(Int(0), …)` compares EQUAL to
            // `Int(0)` (Mixin PartialEq delegates to its inner value), so a
            // `$a does Role` that turns an `Int` slot into an allomorphic `Mixin`
            // would otherwise be missed. Compare the enum discriminant first, then
            // the value.
            let changed = match pre_env.get(i) {
                Some(Some(prev)) => {
                    std::mem::discriminant(prev) != std::mem::discriminant(&cur) || *prev != cur
                }
                _ => true,
            };
            if changed {
                self.locals[i] = cur;
            }
        }
    }

    /// A local value that is safe to overwrite from env during a carrier
    /// writeback: an immutable, cell-free scalar. A container (Array/Hash/...),
    /// an Instance, or a binding cell/ref may hold *live* `:=` cells whose
    /// authoritative state lives in the slot, not env — env may hold a
    /// COW-detached copy (autovivified during the carrier's reads). Overwriting
    /// such a slot from env destroys the live cell (regressed
    /// S03-binding/nested.t). Those names fall back to the `env_dirty` barrier
    /// pull, whose HashEntryRef skip + deferred timing handled them before. The
    /// common EVAL case (`$x = scalar`) is a plain scalar and stays precise.
    pub(crate) fn is_writeback_safe_scalar(v: &Value) -> bool {
        matches!(
            v,
            Value::Int(_)
                | Value::BigInt(_)
                | Value::Num(_)
                | Value::Str(_)
                | Value::Bool(_)
                | Value::Rat(..)
                | Value::FatRat(..)
                | Value::BigRat(..)
                | Value::Complex(..)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::Package(_)
                | Value::Enum { .. }
                | Value::Version { .. }
                | Value::Uni(_)
                | Value::Nil
                | Value::Whatever
        )
    }

    /// Reconcile the *plain-scalar* names a carrier wrote into the current
    /// frame's slots (Slice B). Reads the current env value for each written
    /// scalar name that has a slot in `code.locals`, so the subsequent
    /// `env_dirty` barrier pull finds it already coherent (the reverse sync for
    /// carrier scalar writes becomes precise). A slot holding a
    /// container/instance/binding cell is left to the barrier pull — overwriting
    /// it from a possibly COW-detached env copy would clobber a live `:=` cell
    /// (regressed S03-binding/nested.t). `env_dirty` is still set by the carrier
    /// site as the safety net for those (and for implicit reconcile dependencies
    /// the carrier did not itself write, e.g. a prior `:=` bind); Slice F removes
    /// it once those are explicit. Mirrors `sync_locals_from_env`'s per-slot
    /// skips (HashEntryRef / `!attr`).
    /// Slice F write-through for a `~~` regex match: copy the env values the match
    /// produced — the match variable `$/`, numbered captures (`$0`/`$1`/…), and
    /// any embedded `{ }` / `:my` / `:let` block writes (`extra` = the
    /// `pending_local_updates` names) — straight into the caller's local slots, so
    /// the slots stay coherent without the reverse `sync_locals_from_env` pull.
    ///
    /// Unlike `writeback_carrier_writes` (conservative — never overwrites a
    /// container slot, to protect live `:=` cells), a match RESULT is a freshly
    /// produced value (Match / capture list) with no shared interior cell, so it
    /// is always safe to overwrite the slot — mirroring exactly what the reverse
    /// pull does (which copies unconditionally except for `HashEntryRef`/`!attr`).
    pub(super) fn writeback_match_locals(
        &mut self,
        code: &CompiledCode,
        extra: &std::collections::HashSet<String>,
    ) {
        for (i, name) in code.locals.iter().enumerate() {
            let is_match_name =
                name == "/" || (!name.is_empty() && name.bytes().all(|b| b.is_ascii_digit()));
            if !is_match_name && !extra.contains(name) {
                continue;
            }
            // Mirror the reverse pull's invariants: never clobber a live `:=`
            // binding cell or an attribute slot managed via GetLocal/SetLocal.
            if matches!(self.locals[i], Value::HashEntryRef { .. }) || name.starts_with('!') {
                continue;
            }
            if let Some(val) = self.env().get(name).cloned().or_else(|| {
                name.strip_prefix('$')
                    .or_else(|| name.strip_prefix('@'))
                    .or_else(|| name.strip_prefix('%'))
                    .or_else(|| name.strip_prefix('&'))
                    .and_then(|b| self.env().get(b).cloned())
            }) {
                self.locals[i] = val;
            }
        }
    }

    pub(super) fn writeback_carrier_writes(
        &mut self,
        code: &CompiledCode,
        written: &std::collections::HashSet<String>,
    ) -> bool {
        if written.is_empty() {
            return true;
        }
        // Tracks whether every written name with a caller slot was made coherent
        // here (so the caller can drop the blanket `env_dirty` net). A name left
        // to the barrier pull flips this to false.
        let mut fully = true;
        for (i, name) in code.locals.iter().enumerate() {
            if name.starts_with('!') {
                continue;
            }
            let bare = name
                .strip_prefix('$')
                .or_else(|| name.strip_prefix('@'))
                .or_else(|| name.strip_prefix('%'))
                .or_else(|| name.strip_prefix('&'));
            // Only reconcile slots the carrier actually wrote by name.
            if !written.contains(name) && !bare.is_some_and(|b| written.contains(b)) {
                continue;
            }
            let env_val = self
                .env()
                .get(name)
                .cloned()
                .or_else(|| bare.and_then(|b| self.env().get(b).cloned()));
            let Some(env_val) = env_val else {
                // The carrier logged the name but env no longer holds it (e.g. a
                // scoped key dropped on block exit). Nothing to reconcile.
                continue;
            };
            // A plain scalar is safe to copy from env: it carries no live `:=`
            // cell, so overwriting the slot cannot clobber shared state. This is
            // the common EVAL case (`$x = scalar`).
            //
            // The eligibility is on the *new env value*, not the old slot value:
            // when the carrier reassigned the variable to a plain scalar (the env
            // now holds e.g. `Int(1)`), writing it through is safe regardless of
            // what the slot held before — there is no container COW-copy hazard in
            // a scalar value. This covers `my $a = []; EVAL q'$a = 1'` (slot held
            // an Array, env now holds a scalar), which the old "slot is a scalar"
            // test missed, leaving it to the blanket reconcile (a no-op under
            // double-OFF, so `$a` stayed the stale Array). The only excluded slots
            // are live `:=` binding cells (ContainerRef/HashEntryRef), where the
            // slot must keep routing through the bound cell.
            let new_is_scalar = Self::is_writeback_safe_scalar(&env_val);
            let slot_is_bind_cell = matches!(
                &self.locals[i],
                Value::ContainerRef(_) | Value::HashEntryRef { .. }
            );
            if Self::is_writeback_safe_scalar(&self.locals[i])
                || (new_is_scalar && !slot_is_bind_cell)
            {
                self.locals[i] = env_val;
                continue;
            }
            // A container/instance slot is NEVER overwritten from env: env may
            // hold a COW-detached copy whose write would destroy a live interior
            // `:=` cell (regressed S03-binding/nested.t). It is coherent already
            // iff env and the slot share the same Arc (an in-place mutation the
            // slot observes through the shared container). If they diverge (a
            // whole-container reassignment, or a detached copy), leave it to the
            // `env_dirty` barrier pull and signal "not fully reconciled".
            if !crate::vm::vm_method_dispatch::cheaply_unchanged(&self.locals[i], &env_val) {
                fully = false;
            }
        }
        fully
    }

    pub(super) fn sync_env_from_locals(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            self.set_env_with_main_alias(name, self.locals[i].clone());
        }
    }

    pub(super) fn sync_regex_interpolation_env_from_locals(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            if name == "_"
                || name == "/"
                || name == "!"
                || name == "\u{a2}"
                || name.chars().all(|ch| ch.is_ascii_digit())
            {
                continue;
            }
            // Only sync locals that already exist in the env.  This prevents
            // compile-time-allocated but not-yet-declared locals (from later
            // block scopes) from being prematurely introduced into the env,
            // which would cause bare-word class name resolution to fail when
            // a later block declares `my $x` and the class is named `x`.
            if !self.env().contains_key(name) {
                continue;
            }
            self.set_env_with_main_alias(name, self.locals[i].clone());
        }
    }

    /// Check if a local name looks like a bare function parameter (no sigil).
    /// These are stored by the compiler for function params like `$n` → `n`.
    fn is_bare_param_name(name: &str) -> bool {
        !name.is_empty()
            && !name.starts_with('$')
            && !name.starts_with('@')
            && !name.starts_with('%')
            && !name.starts_with('&')
            && !name.starts_with('.')
            && !name.starts_with('!')
            && !name.starts_with('^')
            && name != "_"
            && !name.contains("::")
            && !name.starts_with("__mutsu_")
    }

    /// Write-through mirror of a single local slot into the interpreter env.
    ///
    /// Replaces the lazy `ensure_env_synced` batch flush (Slice 6.2): instead of
    /// marking the slot dirty and flushing all dirty slots at the next barrier, a
    /// local write mirrors to env *immediately*, so a name-based reader
    /// (GetGlobal-family op, closure capture, interpreter bridge) always observes
    /// the current value with no stale window. Only mirrors slots that such a
    /// reader can actually name (`needs_env_sync`) and that the old flush would
    /// have mirrored (simple-scalar locals + bare-name params); a slot-only local
    /// (read via GetLocal, e.g. `fib`'s `$n`) never reaches env.
    #[inline]
    pub(super) fn flush_local_to_env(&mut self, code: &CompiledCode, idx: usize) {
        // Slot-only locals (no name-based reader) never need to reach env.
        if !code.needs_env_sync.get(idx).copied().unwrap_or(true) {
            return;
        }
        let Some(name) = code.locals.get(idx) else {
            return;
        };
        // Mirror simple scalar locals (the SetLocal fast path) and bare-name
        // params. Skip topic (_), attributes (.x, !x), dynamic vars ($*x), and
        // package-qualified names to avoid corrupting outer scope.
        if code.simple_locals.get(idx).copied().unwrap_or(false) || Self::is_bare_param_name(name) {
            let sym = code.locals_sym.get(idx).copied();
            crate::vm::vm_stats::record_env_flush(1);
            self.set_env_with_main_alias_sym(name, sym, self.locals[idx].clone());
        }
    }

    /// Slice F (env<->locals coherence): drain the `is rw` / aliased-container
    /// parameter (and captured-outer-write) sources recorded by the call
    /// dispatch paths and write each caller variable's new `env` value straight
    /// through to its local slot in the caller's frame (`code`). This keeps the
    /// slot coherent at the call site. Invariant: never clobber a live
    /// `HashEntryRef` binding slot. Sources whose name is not a local of this
    /// frame are dropped here; cross-frame propagation up to an ancestor's slot
    /// is handled by the env_dirty-gated `reconcile_locals_from_env_at_site` that
    /// `drain_and_reconcile_after_cached_call` (and the slow call path) runs at
    /// each frame's call site.
    pub(super) fn apply_pending_rw_writeback(&mut self, code: &CompiledCode) {
        if !self.pending_rw_writeback_sources.is_empty() {
            let sources = std::mem::take(&mut self.pending_rw_writeback_sources);
            for source in sources {
                if let Some(slot) = self.find_local_slot(code, &source) {
                    if !matches!(self.locals[slot], Value::HashEntryRef { .. })
                        && let Some(val) = self.env().get(&source).cloned()
                    {
                        self.locals[slot] = val;
                    }
                } else if !self.pending_caller_var_writeback.contains(&source) {
                    // The owning slot is not in THIS frame. Don't drop the source:
                    // it belongs to a frame further up the stack (e.g. a sibling
                    // `submethod BUILD`'s `$counter++` queued for the outer `.new`
                    // site, consumed here by a *nested* call inside another BUILD —
                    // S12-construction/BUILD.t). Move it to the retain-on-miss
                    // caller-var list so the owning frame's drain refreshes its slot
                    // instead of leaving it stale.
                    self.pending_caller_var_writeback.push(source);
                }
            }
        }
        self.apply_pending_caller_var_writeback(code);
    }

    /// Drain caller-frame-targeted writes (`$CALLER::x = v` / `callframe(d).my.<$x>
    /// = v`). Unlike `pending_rw_writeback_sources`, a source whose slot is NOT in
    /// this frame's `code` is RETAINED rather than dropped, because the target slot
    /// lives an unknown number of frames up: the writer may make an intervening
    /// *deeper* call (whose code lacks the slot) before returning to the frame that
    /// owns it, and that deeper call's drain must not consume the pending write.
    /// Each matched source is removed so it is applied exactly once.
    pub(super) fn apply_pending_caller_var_writeback(&mut self, code: &CompiledCode) {
        if self.pending_caller_var_writeback.is_empty() {
            return;
        }
        let sources = std::mem::take(&mut self.pending_caller_var_writeback);
        let mut retained = Vec::new();
        for source in sources {
            if let Some(slot) = self.find_local_slot(code, &source) {
                if !matches!(self.locals[slot], Value::HashEntryRef { .. })
                    && let Some(val) = self.env().get(&source).cloned()
                {
                    self.locals[slot] = val;
                }
                // matched (slot exists in this frame) → applied, do not retain
            } else {
                retained.push(source);
            }
        }
        self.pending_caller_var_writeback = retained;
    }

    /// Slice F (multi-frame coherence): drain after a *cached fast-call* dispatch
    /// (positional-light / light / 0-arg fast / OTF), mirroring the slow path's
    /// post-`dispatch_func_call_inner` reconcile.
    ///
    /// `apply_pending_rw_writeback` is precise but single-frame: it writes only
    /// the sources THIS caller frame recorded. A captured-outer write performed by
    /// a *nested* callee (`via()` -> `bump-outer()` -> `$acc = $acc + 10`) is
    /// recorded against the intermediate `via` frame, whose code has no `$acc`
    /// slot, so the drain there discards it (`mem::take`) one frame too deep. The
    /// write still lands in the shared env and propagates up, but the top-level
    /// `$acc` slot stays stale across calls — so `via(); via()` fails to accumulate
    /// (10, not 20) once the reverse `sync_locals_from_env` pull is gone.
    ///
    /// The slow `exec_call_func_op` path already handles this with an env_dirty-
    /// gated `reconcile_locals_from_env_at_site`; the cached fast paths returned
    /// early without it. Add the same gated reconcile here. It is gated on
    /// `env_dirty` exactly like the slow path: the light merges set `env_dirty`
    /// only when the callee wrote a captured-outer (non-local) name, so a pure
    /// call (e.g. `fib`) never trips it and pays nothing.
    pub(super) fn drain_and_reconcile_after_cached_call(&mut self, code: &CompiledCode) {
        self.apply_pending_rw_writeback(code);
    }

    /// Box each captured-outer scalar that a carrier body (`EVAL`, an embedded
    /// regex `{...}` block) WRITES into a `ContainerRef` cell shared across the
    /// live `env` and every saved call frame's `saved_env`, so the carrier's
    /// by-name write reaches the owner frame even after the owner's env is
    /// restored on return (the multi-frame accumulation wall — a captured-outer
    /// write hidden inside an `EVAL` string is invisible to the static
    /// `free_var_writes` analysis at the owner's decl site, so it cannot be boxed
    /// there; box it here at carrier-run time instead).
    ///
    /// Cell boxing is the permanent coherence mechanism (the env→locals reconcile
    /// is retired), so this always boxes when there are free-var writes.
    pub(crate) fn box_carrier_free_var_writes(&mut self, code: &CompiledCode) {
        if code.free_var_writes.is_empty() {
            return;
        }
        // Restrict to the EVAL carrier (`__mutsu_in_eval`). The other
        // eval_block_value users — supply/whenever/gather bodies — have their own
        // concurrent-cell coherence (Track C, the hardest/last cluster); boxing
        // their captured vars here conflicts with that machinery, so leave them be.
        if self.env().get("__mutsu_in_eval").is_none() {
            return;
        }
        let names: Vec<String> = code.free_var_writes.iter().map(|s| s.resolve()).collect();
        for name in names {
            if name == "_" || name == "$_" || name == "@_" || name == "%_" {
                continue;
            }
            // Scalars only (containers share via Arc already); never hide a
            // type object / sub / instance behind a cell.
            if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
                continue;
            }
            // A sigilless alias (`sub swap(\x,\y){ y=x }`) is NOT a captured-outer
            // lexical: `x` resolves through `__mutsu_sigilless_alias::x` to the
            // caller's `$a`, and writes propagate via that chain (#3318). Boxing
            // `x` into a cell detaches it from `$a` — skip it.
            if self
                .env()
                .get(&format!("__mutsu_sigilless_alias::{}", name))
                .is_some()
            {
                continue;
            }
            let cur = match self.env().get(&name) {
                Some(v) => v.clone(),
                None => continue,
            };
            if cur.is_container_ref() {
                continue;
            }
            if matches!(
                cur,
                Value::Package(_)
                    | Value::Array(..)
                    | Value::Hash(..)
                    | Value::Sub(..)
                    | Value::Instance { .. }
                    | Value::Proxy { .. }
            ) {
                continue;
            }
            // A genuine type/`where` constraint must keep flowing through the
            // assignment chokepoint (write-through bypasses the re-check); `Mu`
            // is universal so it is safe to box (mirrors slice 1.5).
            let mut tc = loan_env!(self, var_type_constraint(&name));
            if tc.is_none() {
                tc = loan_env!(self, var_type_constraint(name.trim_start_matches('$')));
            }
            if matches!(tc.as_deref(), Some(t) if t != "Mu") {
                continue;
            }
            let container = cur.into_container_ref();
            self.set_env_with_main_alias(&name, container.clone());
            if let Some(idx) = code.locals.iter().rposition(|n| n == &name) {
                self.locals[idx] = container.clone();
            }
            for frame in self.call_frames.iter_mut().rev() {
                if frame.saved_env.contains_key(&name) {
                    frame.saved_env.insert(name.clone(), container.clone());
                }
            }
            if self.shared_vars_active {
                loan_env!(self, set_shared_var(&name, container.clone()));
            }
        }
    }

    pub(super) fn find_local_slot(&self, code: &CompiledCode, name: &str) -> Option<usize> {
        code.locals.iter().position(|n| n == name)
    }

    pub(super) fn update_local_if_exists(&mut self, code: &CompiledCode, name: &str, val: &Value) {
        if let Some(slot) = self.find_local_slot(code, name) {
            self.locals[slot] = val.clone();
        }
    }

    pub(super) fn locals_get_by_name(&self, code: &CompiledCode, name: &str) -> Option<Value> {
        self.find_local_slot(code, name)
            .map(|slot| self.locals[slot].clone())
    }

    pub(super) fn locals_set_by_name(&mut self, code: &CompiledCode, name: &str, val: Value) {
        if let Some(slot) = self.find_local_slot(code, name) {
            self.locals[slot] = val;
        }
    }
}
