use super::*;

impl Interpreter {
    /// Sync locals from env if the dirty flag is set, then clear the flag.
    /// If locals are also dirty, flush them to env first so we don't lose
    /// values that were only written to the locals array (fast-path SetLocal).
    pub(super) fn ensure_locals_synced(&mut self, code: &CompiledCode) {
        if self.env_dirty {
            // Flush locals→env first so env has the latest simple-local values
            // before we pull env→locals (which may overwrite them with stale data).
            self.sync_locals_from_env(code);
            self.env_dirty = false;
        }
    }

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

    /// Save the current env, locals, stack depth, readonly vars, and env_dirty flag
    /// into a new call frame. Resets env_dirty to false for the new frame.
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
            saved_env_dirty: self.env_dirty,
            saved_local_bind_pairs: std::mem::take(&mut self.local_bind_pairs),
        };
        self.env_dirty = false;
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
            saved_env_dirty: self.env_dirty,
            saved_local_bind_pairs: std::mem::take(&mut self.local_bind_pairs),
        };
        self.env_dirty = false;
        self.call_frames.push(frame);
    }

    /// Pop the most recent call frame and restore locals, readonly vars, and env_dirty flag.
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
        self.env_dirty = frame.saved_env_dirty;
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

    pub(super) fn sync_locals_from_env(&mut self, code: &CompiledCode) {
        crate::vm::vm_stats::record_locals_pull();
        for (i, name) in code.locals.iter().enumerate() {
            // Don't overwrite HashSlotRef locals with env values.
            // These are live container references from `:=` binding and must
            // not be replaced by stale env copies.
            if matches!(self.locals[i], Value::HashSlotRef { .. }) {
                continue;
            }
            // Attribute locals (!attr) are managed exclusively via GetLocal/SetLocal.
            // Never overwrite them from env, which may hold stale method-entry values.
            if name.starts_with('!') {
                continue;
            }
            if let Some(val) = self.env().get(name) {
                self.locals[i] = val.clone();
                continue;
            }
            if let Some(bare) = name
                .strip_prefix('$')
                .or_else(|| name.strip_prefix('@'))
                .or_else(|| name.strip_prefix('%'))
                .or_else(|| name.strip_prefix('&'))
                && let Some(val) = self.env().get(bare)
            {
                self.locals[i] = val.clone();
            }
        }
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
