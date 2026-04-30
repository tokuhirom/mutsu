use super::*;

impl VM {
    /// Sync locals from env if the dirty flag is set, then clear the flag.
    /// If locals are also dirty, flush them to env first so we don't lose
    /// values that were only written to the locals array (fast-path SetLocal).
    pub(super) fn ensure_locals_synced(&mut self, code: &CompiledCode) {
        if self.env_dirty {
            // Flush locals→env first so env has the latest simple-local values
            // before we pull env→locals (which may overwrite them with stale data).
            self.ensure_env_synced(code);
            self.sync_locals_from_env(code);
            self.env_dirty = false;
        }
    }

    /// Save the current env, locals, stack depth, readonly vars, and env_dirty flag
    /// into a new call frame. Resets env_dirty to false for the new frame.
    pub(super) fn push_call_frame(&mut self) {
        let frame = VmCallFrame {
            saved_env: self.interpreter.clone_env(),
            saved_readonly: self.interpreter.save_readonly_vars(),
            saved_locals: std::mem::take(&mut self.locals),
            saved_stack_depth: self.stack.len(),
            saved_env_dirty: self.env_dirty,
            saved_locals_dirty: self.locals_dirty,
            saved_local_bind_pairs: std::mem::take(&mut self.local_bind_pairs),
        };
        self.env_dirty = false;
        self.locals_dirty = false;
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
        self.interpreter
            .restore_readonly_vars(std::mem::take(&mut frame.saved_readonly));
        self.env_dirty = frame.saved_env_dirty;
        self.locals_dirty = frame.saved_locals_dirty;
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
            .and_then(|bare| self.interpreter.get_our_var(&bare).cloned())
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
        // Atomic array CAS stores the authoritative copy under an internal key.
        // Always check it first so both thread-clone and non-clone reads
        // observe the latest CAS'd value.
        if name.starts_with('@') {
            let atomic_key = format!("__mutsu_atomic_arr::{name}");
            if let Some(v) = self.interpreter.get_shared_var(&atomic_key) {
                return Some(v);
            }
        }
        // Thread-clone @/% lookups must prefer the shared copy. Child thread
        // env snapshots can lag behind sibling mutations even when Lock::Async
        // serializes the writes through shared_vars.
        if self.interpreter.is_thread_clone()
            && (name.starts_with('@') || name.starts_with('%'))
            && let Some(v) = self.interpreter.get_shared_var(name)
        {
            return Some(v);
        }
        // Otherwise check local env first so that function parameters and
        // lexical variables take precedence over shared_vars. Without this,
        // recursive `start` blocks can read stale parameter values from
        // shared_vars instead of the locally-bound ones.
        if let Some(val) = self.interpreter.env().get(name) {
            return Some(val.clone());
        }
        // Anonymous scalar placeholders (from bare `$`) are invocation-local.
        if name.starts_with("__ANON_STATE_") {
            return None;
        }
        // Fall back to shared_vars for cross-thread visibility of variables
        // that were explicitly updated by other threads.
        if let Some(v) = self.interpreter.get_shared_var(name) {
            return Some(v);
        }
        // Follow binding aliases ($CALLER::target := $source)
        if let Some(resolved) = self.interpreter.resolve_binding(name)
            && let Some(val) = self.interpreter.env().get(resolved)
        {
            return Some(val.clone());
        }
        if let Some(alias) = Self::twigil_dynamic_alias(name) {
            return self.interpreter.env().get(&alias).cloned();
        }
        if let Some(alias) = Self::main_unqualified_name(name) {
            return self.interpreter.env().get(&alias).cloned();
        }
        if let Some(qualified) = Self::main_qualified_name(name) {
            return self.interpreter.env().get(&qualified).cloned();
        }
        // Strip GLOBAL::, OUR::, MY:: pseudo-package qualifiers to find
        // the variable under its bare name in the environment.
        if let Some(bare) = Self::pseudo_package_unqualified_name(name) {
            return self.interpreter.env().get(&bare).cloned();
        }
        // Placeholder block parameters are stored as "^name". Allow lexical
        // access by the de-careted name inside the same block.
        if !name.starts_with('^') {
            let placeholder = format!("^{name}");
            if let Some(val) = self.interpreter.env().get(&placeholder) {
                return Some(val.clone());
            }
            if let Some(val) = self.interpreter.get_shared_var(&placeholder) {
                return Some(val);
            }
        }
        None
    }

    pub(super) fn set_env_with_main_alias(&mut self, name: &str, value: Value) {
        if name.starts_with("__ANON_STATE_") {
            self.interpreter.env_mut().insert(name.to_string(), value);
            return;
        }
        if !name.starts_with('^') {
            let placeholder = format!("^{name}");
            if self.interpreter.env().contains_key(&placeholder) {
                self.interpreter.set_shared_var(&placeholder, value.clone());
                self.interpreter.env_mut().insert(placeholder, value);
                return;
            }
        }
        self.interpreter.set_shared_var(name, value.clone());
        if let Some(alias) = Self::twigil_dynamic_alias(name) {
            self.interpreter.env_mut().insert(alias, value.clone());
        }
        if let Some(inner) = name
            .strip_prefix("&infix:<")
            .or_else(|| name.strip_prefix("&prefix:<"))
            .or_else(|| name.strip_prefix("&postfix:<"))
            && let Some(op_name) = inner.strip_suffix('>')
        {
            self.interpreter
                .env_mut()
                .insert(format!("&{}", op_name), value.clone());
        }
        if let Some(alias) = Self::main_unqualified_name(name) {
            self.interpreter.env_mut().insert(alias, value);
            return;
        }
        if let Some(qualified) = Self::main_qualified_name(name)
            && self.interpreter.env().contains_key(&qualified)
        {
            self.interpreter.env_mut().insert(qualified, value);
            return;
        }
        // Write through GLOBAL::, OUR::, MY:: pseudo-package qualifiers to the
        // bare variable name in the environment.
        if let Some(bare) = Self::pseudo_package_unqualified_name(name) {
            self.interpreter.env_mut().insert(bare, value);
        } else if let Some(bare) = name.strip_prefix("GLOBAL::") {
            // For unsigiled GLOBAL-qualified names (e.g. "GLOBAL::x" from
            // `$x` at top level), also update the bare name in the env so
            // that lookups by bare name see the latest value.
            if self.interpreter.env().contains_key(bare) {
                self.interpreter.env_mut().insert(bare.to_string(), value);
            }
        }
    }

    pub(super) fn sync_locals_from_env(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
                continue;
            }
            if let Some(bare) = name
                .strip_prefix('$')
                .or_else(|| name.strip_prefix('@'))
                .or_else(|| name.strip_prefix('%'))
                .or_else(|| name.strip_prefix('&'))
                && let Some(val) = self.interpreter.env().get(bare)
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
            if !self.interpreter.env().contains_key(name) {
                continue;
            }
            self.set_env_with_main_alias(name, self.locals[i].clone());
        }
    }

    /// Sync only simple locals to env (the ones whose SetLocal fast path
    /// skips env writes). Only runs when locals_dirty is set.
    pub(super) fn ensure_env_synced(&mut self, code: &CompiledCode) {
        if self.locals_dirty {
            for (i, name) in code.locals.iter().enumerate() {
                if code.simple_locals[i] {
                    self.set_env_with_main_alias(name, self.locals[i].clone());
                }
            }
            self.locals_dirty = false;
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
