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

    /// Grab a locals vector from the recycle pool (or allocate the first time),
    /// sized to `num_locals` with `Nil` slots. Pair with [`Self::recycle_locals`].
    #[inline]
    pub(super) fn take_locals_from_pool(&mut self, num_locals: usize) -> Vec<Value> {
        let mut v = self.locals_pool.pop().unwrap_or_default();
        v.clear();
        v.resize(num_locals, Value::NIL);
        v
    }

    /// Return a used locals vector to the recycle pool (bounded; excess is
    /// simply dropped). Clearing here also drops the callee's slot values at a
    /// well-defined point instead of inside the pool.
    #[inline]
    pub(super) fn recycle_locals(&mut self, mut used: Vec<Value>) {
        const LOCALS_POOL_MAX: usize = 64;
        if self.locals_pool.len() < LOCALS_POOL_MAX {
            used.clear();
            self.locals_pool.push(used);
        }
    }

    /// Save the current env, locals, stack depth, and readonly vars into a new
    /// call frame.
    pub(super) fn push_call_frame(&mut self) {
        // GC safepoint (§9.2a `call`): the frame-push boundary holds no
        // container borrow. Self-gated — one cached load unless `MUTSU_GC`
        // arms a trigger.
        crate::gc::gc_safepoint(crate::gc::SafepointKind::Call);
        crate::vm::vm_stats::record_clone_env();
        // Save the caller env by an O(1) clone (an Arc bump, even when scoped):
        // it is only restored on return, so it need not be flattened. The callee
        // chains a fresh overlay over it (multi-tier), so nested calls no longer
        // pay the per-call O(env) flatten. Long-lived captures (Sub closures, END
        // phasers, threads) still flatten via `clone_env` at the capture site.
        let frame = VmCallFrame {
            saved_env: self.env().clone(),
            saved_cur_line: self.cur_source_line,
            readonly_mark: self.enter_readonly_frame(),
            saved_locals: std::mem::take(&mut self.locals),
            saved_upvalues: std::mem::take(&mut self.upvalues),
            saved_stack_depth: self.stack.len(),
            saved_local_bind_pairs: std::mem::take(&mut self.local_bind_pairs),
            saved_loop_local_vars: std::mem::take(&mut self.loop_local_vars),
            saved_loop_local_saved_env: std::mem::take(&mut self.loop_local_saved_env),
            saved_block_declared_vars: std::mem::take(&mut self.block_declared_vars),
            saved_frame_authoritative: std::mem::take(&mut self.frame_authoritative),
        };
        self.call_frames.push(frame);
    }

    /// Lightweight call frame for simple methods. Historically this skipped
    /// the whole-set readonly snapshot and tracked param deltas by hand; the
    /// journal (`enter_readonly_frame`) made that split obsolete — both frame
    /// flavors now open a scope for two integer ops.
    pub(super) fn push_light_call_frame(&mut self) {
        // GC safepoint (§9.2a `call`) — see `push_call_frame`.
        crate::gc::gc_safepoint(crate::gc::SafepointKind::Call);
        crate::vm::vm_stats::record_clone_env();
        let frame = VmCallFrame {
            saved_env: self.env().clone(),
            saved_cur_line: self.cur_source_line,
            readonly_mark: self.enter_readonly_frame(),
            saved_locals: std::mem::take(&mut self.locals),
            saved_upvalues: std::mem::take(&mut self.upvalues),
            saved_stack_depth: self.stack.len(),
            saved_local_bind_pairs: std::mem::take(&mut self.local_bind_pairs),
            saved_loop_local_vars: std::mem::take(&mut self.loop_local_vars),
            saved_loop_local_saved_env: std::mem::take(&mut self.loop_local_saved_env),
            saved_block_declared_vars: std::mem::take(&mut self.block_declared_vars),
            saved_frame_authoritative: std::mem::take(&mut self.frame_authoritative),
        };
        self.call_frames.push(frame);
    }

    /// Pop the most recent call frame and restore locals and readonly vars.
    /// Returns the frame so callers can access `saved_env` for site-specific merge logic.
    pub(super) fn pop_call_frame(&mut self) -> VmCallFrame {
        // GC safepoint (§9.2a `return`): the frame-pop / return-merge boundary
        // holds no container borrow (the return value is an owned stack root).
        crate::gc::gc_safepoint(crate::gc::SafepointKind::Return);
        let mut frame = self
            .call_frames
            .pop()
            .expect("pop_call_frame: no frame to pop");
        self.cur_source_line = frame.saved_cur_line;
        self.locals = std::mem::take(&mut frame.saved_locals);
        self.upvalues = std::mem::take(&mut frame.saved_upvalues);
        self.local_bind_pairs = std::mem::take(&mut frame.saved_local_bind_pairs);
        self.loop_local_vars = std::mem::take(&mut frame.saved_loop_local_vars);
        self.loop_local_saved_env = std::mem::take(&mut frame.saved_loop_local_saved_env);
        self.block_declared_vars = std::mem::take(&mut frame.saved_block_declared_vars);
        self.frame_authoritative = std::mem::take(&mut frame.saved_frame_authoritative);
        self.exit_readonly_frame(frame.readonly_mark);
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
        // Sigils are single-byte ASCII, so `name[1..]` is a valid boundary once
        // the first char is one of them. Check the constant `Main::` prefix
        // directly instead of building `"{sigil}Main::"` for all four sigils on
        // every call -- this runs on the variable-access hot path.
        let sigil = name.chars().next()?;
        if !matches!(sigil, '$' | '@' | '%' | '&') {
            return None;
        }
        let rest = name[1..].strip_prefix("Main::")?;
        Some(format!("{sigil}{rest}"))
    }

    /// Look up an `our`-scoped variable by trying the bare (unqualified) name
    /// after stripping pseudo-package prefixes like GLOBAL::, OUR::, etc.
    pub(super) fn our_var_pseudo_unqualified(&self, name: &str) -> Option<Value> {
        Self::pseudo_package_unqualified_name(name)
            .and_then(|bare| self.get_our_var(&bare).cloned())
    }

    /// Reconstruct the package-qualified key for a bare free-variable name
    /// referenced from inside one of `cur`'s named subs (where the compiler
    /// leaves the name unqualified because the sub-body compile scope is a
    /// mangled `Pkg::&sub/arity` name). e.g. `("X", "P")` -> `Some("P::X")`,
    /// `("@a", "P")` -> `Some("@P::a")`. Returns `None` for names that are
    /// already qualified, sigil-stripped twigils, or positional captures —
    /// mirroring the `GetGlobal` bare-name read fallback so reads and writes
    /// resolve to the same canonical store.
    fn package_qualified_candidate(name: &str, cur: &str) -> Option<String> {
        if name.contains("::") || cur.is_empty() || cur == "GLOBAL" || cur.contains("::&") {
            return None;
        }
        let bare_first = name.trim_start_matches(['$', '@', '%', '&']);
        let first_ch = bare_first.chars().next()?;
        if matches!(first_ch, '_' | '/' | '!' | '?' | '*' | '.' | '=') || first_ch.is_ascii_digit()
        {
            return None;
        }
        let candidate = if let Some(rest) = name.strip_prefix('$') {
            format!("${cur}::{rest}")
        } else if let Some(rest) = name.strip_prefix('@') {
            format!("@{cur}::{rest}")
        } else if let Some(rest) = name.strip_prefix('%') {
            format!("%{cur}::{rest}")
        } else if let Some(rest) = name.strip_prefix('&') {
            format!("&{cur}::{rest}")
        } else {
            format!("{cur}::{name}")
        };
        Some(candidate)
    }

    /// Resolve a bare enum-member name through the current package chain
    /// (`EnumC::Lists` for `Lists` read with `current_package == "EnumC"`).
    /// Qualified enum-member env keys survive a loading module's package-block
    /// rollback (only bare keys are dropped), so this recovers members that a
    /// nested `use` made invisible under their bare name. Walks up the package
    /// chain like `resolve_type_in_current_package`.
    pub(super) fn resolve_enum_member_in_current_package(&self, name: &str) -> Option<Value> {
        if name.is_empty() || name.contains("::") {
            return None;
        }
        let probe_chain = |root: &str| -> Option<Value> {
            let mut pkg: &str = root;
            loop {
                if pkg.is_empty() || pkg == "GLOBAL" {
                    return None;
                }
                let qualified = format!("{pkg}::{name}");
                if let Some(v) = self.env().get(&qualified)
                    && matches!(v.view(), ValueView::Enum { .. })
                {
                    return Some(v.clone());
                }
                match pkg.rsplit_once("::") {
                    Some((parent, _)) => pkg = parent,
                    None => return None,
                }
            }
        };
        if let Some(v) = probe_chain(&self.current_package().to_string()) {
            return Some(v);
        }
        // Inside a method body the runtime package is often GLOBAL; the
        // declaring scope is the invocant's class (`self`), so probe its
        // package chain too (`unit class EnumC; our enum HF <… Lists>;
        // method new(:$h = Lists)` reads `Lists` as `EnumC::Lists`).
        let invocant_class = match self.env().get("self").map(|v| v.view()) {
            Some(ValueView::Instance { class_name, .. }) => Some(class_name.to_string()),
            Some(ValueView::Package(p)) => Some(p.resolve().to_string()),
            _ => None,
        }?;
        probe_chain(&invocant_class)
    }

    /// Read a bare free-variable name from the enclosing package's variable
    /// store: the `our` store via the reconstructed `Pkg::name` key, or the
    /// package-block `my` lexical store (`package_lexicals`). Mirrors the
    /// `GetGlobal` read fallbacks so the fused `AtomicCompoundVar` / inc-dec
    /// RMW paths see the same value a plain `Var` read would. Returns the raw
    /// stored value (the caller decont's if needed).
    pub(super) fn read_package_scope_var(&self, name: &str) -> Option<Value> {
        let cur = self.current_package();
        if let Some(candidate) = Self::package_qualified_candidate(name, &cur)
            && let Some(v) = self.get_our_var(&candidate)
        {
            return Some(v.clone());
        }
        self.package_lexicals
            .get(&cur)
            .and_then(|m| m.get(name))
            .cloned()
    }

    /// Return the value of a bare package-block `my` lexical (`package P { my $x;
    /// sub f { $x } }`) recorded in `package_lexicals` by `exec_package_scope_op`.
    /// This store is the *authoritative* lexical scope a package's named subs close
    /// over, so callers consult it BEFORE `env`: two stale `env` shadows would
    /// otherwise win — a boxed lexical's prior-call return-merge copy, and the
    /// package block's own `my $x` top-level local slot flushed to `env` as the
    /// type object after the block exits. A returned `ContainerRef` is the live
    /// shared cell (writes mutate it in place); a plain value is the current
    /// snapshot (writes go via `writeback_package_scope_var`). Gated on a real
    /// (non-GLOBAL, non-mangled) `current_package`, so a bare reference after the
    /// block (running under GLOBAL) never resolves here. A name shadowed by the
    /// sub's own `my`/param is a local slot (GetLocal), so it never reaches here.
    pub(super) fn package_scope_lexical(&self, name: &str) -> Option<Value> {
        let cur = self.current_package();
        if cur.is_empty() || cur == "GLOBAL" || cur.contains("::&") {
            return None;
        }
        // `package_lexicals` is keyed by the package's own env name for the
        // lexical: scalars sigil-less (`CONFIG`), `@`/`%`/`&` keep their sigil
        // (`@a`). A free-var read reaches this with the name in one of two shapes:
        //   * BARE (`CONFIG` / `@a`) — emitted when the body compiles under the
        //     mangled `Pkg::&sub/arity` scope (`compile_sub_body`), which does not
        //     qualify names. Looked up directly.
        //   * QUALIFIED (`MyCLI::CONFIG` / `@MyCLI::a`) — emitted when the body
        //     compiles under the PLAIN package name (`compile_block_raw`, used by
        //     the `call_function_def` slow path that runs an exported `MAIN`), which
        //     auto-qualifies free vars. Resolve ONLY when the qualifier is the
        //     current package, so `$Other::x` from outside never reaches another
        //     package's `my` lexical (Raku: `my` lexicals are not package-public).
        let key: std::borrow::Cow<str> = if name.contains("::") {
            let (sigil, rest) = match name.as_bytes().first() {
                Some(b @ (b'$' | b'@' | b'%' | b'&')) => (Some(*b as char), &name[1..]),
                _ => (None, name),
            };
            let (pkg, bare) = rest.rsplit_once("::")?;
            if pkg != cur {
                return None;
            }
            // A class-body `my` static lives in `package_lexicals` for BARE-name
            // reuse, but a QUALIFIED `$C::x` is a distinct package variable, not
            // the static — do not resolve it here (t/package-lookup.t).
            if self
                .class_body_static_names
                .get(pkg)
                .is_some_and(|s| s.contains(bare))
            {
                return None;
            }
            match sigil {
                // `@`/`%`/`&` lexicals are stored WITH their sigil; a scalar (`$`
                // or sigil-less) is stored sigil-less.
                Some(s @ ('@' | '%' | '&')) => std::borrow::Cow::Owned(format!("{s}{bare}")),
                _ => std::borrow::Cow::Borrowed(bare),
            }
        } else {
            std::borrow::Cow::Borrowed(name)
        };
        self.package_lexicals
            .get(&cur)
            .and_then(|m| m.get(key.as_ref()))
            .cloned()
    }

    /// Write-back companion of [`read_package_scope_var`]: if a bare free
    /// variable resolves to an existing package-scope store entry (`our` var
    /// or package-block `my` lexical), update it in place so a mutation made
    /// from inside a named sub persists across calls. Returns `true` when an
    /// entry was found and updated. A SetGlobal / RMW of a name that is NOT a
    /// package-scope var (a fresh global, a dynamic var, `$_`, ...) returns
    /// `false` and the caller's normal store path applies unchanged.
    pub(super) fn writeback_package_scope_var(&mut self, name: &str, val: &Value) -> bool {
        // Both branches below require a hit in `our_vars` (package-qualified `our`
        // var) or `package_lexicals` (package-block `my` static). When both stores
        // are empty — the common case for a plain-lexical hot loop — there is
        // nothing to write back, so skip the `current_package()` RwLock read +
        // `String` clone entirely. This is the dominant cost of the inc-dec
        // write-back path once the type-constraint check is gated away.
        if self.our_vars_is_empty() && self.package_lexicals.is_empty() {
            return false;
        }
        let cur = self.current_package();
        if let Some(candidate) = Self::package_qualified_candidate(name, &cur)
            && self.get_our_var(&candidate).is_some()
        {
            self.set_our_var(candidate.clone(), val.clone());
            // Keep an existing qualified env entry coherent for a same-frame
            // read by the qualified name (`$P::X`).
            if self.env().contains_key(&candidate) {
                self.set_env_with_main_alias(&candidate, val.clone());
            }
            return true;
        }
        if let Some(m) = self.package_lexicals.get_mut(&cur)
            && let Some(slot) = m.get_mut(name)
        {
            // A boxed lexical's cell is shared with every reader; mutate it in
            // place rather than replacing the entry with a plain value (which
            // would sever the sharing). A plain entry is replaced directly.
            if let ValueView::ContainerRef(arc) = slot.view() {
                arc.lock().unwrap().clone_from(val);
            } else {
                *slot = val.clone();
            }
            return true;
        }
        false
    }

    /// Strip pseudo-package qualifiers (GLOBAL::, OUR::, MY::) from a
    /// sigiled variable name, returning the bare variable name.
    /// e.g. "$GLOBAL::x" → Some("x"), "$OUR::x" → Some("x")
    fn pseudo_package_unqualified_name(name: &str) -> Option<String> {
        // The pseudo-package prefixes are constants -- match them directly
        // rather than re-formatting `"{pkg}::"` on every variable access.
        const PSEUDO: [&str; 3] = ["GLOBAL::", "OUR::", "MY::"];
        let sigil = name.chars().next();
        if matches!(sigil, Some('$' | '@' | '%' | '&')) {
            // Sigils are single-byte ASCII, so `name[1..]` is a valid boundary.
            let rest = &name[1..];
            for prefix in PSEUDO {
                if let Some(bare) = rest.strip_prefix(prefix) {
                    return Some(format!("{}{bare}", sigil.unwrap()));
                }
            }
        }
        // Also handle unsigiled forms (e.g. "GLOBAL::x")
        for prefix in PSEUDO {
            if let Some(bare) = name.strip_prefix(prefix) {
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
        // Atomic array/hash CAS stores the authoritative copy under an internal
        // key. Always check it first so both thread-clone and non-clone reads
        // observe the latest CAS'd value. Without the hash arm, a thread's own
        // `%h{$k} = $v` (routed through `shared_hash_elem_set`) was invisible
        // to its own re-read: the base-key snapshot below is stale.
        if name.starts_with('@') {
            let atomic_key = format!("__mutsu_atomic_arr::{name}");
            if let Some(v) = self.get_shared_var(&atomic_key) {
                return Some(v);
            }
        } else if name.starts_with('%') {
            let atomic_key = format!("__mutsu_atomic_hash::{name}");
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
        // access by the de-careted name inside the same block. Gated on the
        // process-global latch: without a `^name` key anywhere, the `format!` +
        // interning probe can only miss.
        if crate::env::placeholder_var_possible() && !name.starts_with('^') {
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

    /// The by-name env write for a *plain lexical* (see
    /// [`CompiledCode::plain_locals`]): everything
    /// [`set_env_with_main_alias_sym`] does minus the alias maintenance, every
    /// branch of which is unreachable for such a name.
    ///
    /// The name has no sigil, so it cannot be a `Main::`/`GLOBAL::`/`OUR::`/`MY::`
    /// qualified or unqualified form (all of which need a `::` this name cannot
    /// contain), nor an `&infix:<...>` operator alias; it has no `*` twigil, so it
    /// has no `$*x`/`*x` dynamic-alias pair; and it is not a compiler-internal
    /// `__ANON_STATE_*`. What is left is the carrier log, the (latched)
    /// placeholder probe, and one Symbol-keyed insert -- which is what a
    /// `my $x = ...` in a hot loop should cost.
    pub(super) fn set_env_plain_lexical(&mut self, name: &str, sym: Option<Symbol>, value: Value) {
        if let Some(set) = self.carrier_writes.as_mut() {
            set.insert(name.to_string());
        }
        if crate::env::placeholder_var_possible() {
            let placeholder = format!("^{name}");
            if self.env().contains_key(&placeholder) {
                loan_env!(self, set_shared_var(&placeholder, value.clone()));
                self.env_mut().insert(placeholder, value);
                return;
            }
        }
        loan_env!(self, set_shared_var_sym(name, sym, value));
    }

    /// Like `set_env_with_main_alias` but accepts a pre-interned Symbol
    /// to avoid Symbol::intern() overhead on hot paths.
    pub(super) fn set_env_with_main_alias_sym(
        &mut self,
        name: &str,
        name_sym: Option<Symbol>,
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
        // A placeholder param (`$^a`) is bound under its careted name, so a write
        // to the de-careted lexical must land on it. Skipped outright unless some
        // `^name` key has ever been created (the common program), which otherwise
        // costs a `format!` + an interning env probe on every mirrored local store.
        if crate::env::placeholder_var_possible() && !name.starts_with('^') {
            let placeholder = format!("^{name}");
            if self.env().contains_key(&placeholder) {
                loan_env!(self, set_shared_var(&placeholder, value.clone()));
                self.env_mut().insert(placeholder, value);
                return;
            }
        }
        loan_env!(self, set_shared_var_sym(name, name_sym, value.clone()));
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
            v.view(),
            ValueView::HashEntryRef { .. }
                | ValueView::ContainerRef(_)
                | ValueView::Array(..)
                | ValueView::Hash(..)
        )
    }

    /// Snapshot the slot-backing env values before a `lives-ok`/`dies-ok` carrier
    /// runs. Overwritable slots use it for the changed-value diff; plain
    /// Array/Hash slots also snapshot (their writeback additionally requires
    /// the pre-carrier env to have been in sync with the slot — see
    /// `carrier_writeback_changed_aggregates`). `None` entries (binding cells,
    /// `!attr`, absent env keys) are never written back.
    pub(super) fn snapshot_carrier_overwritable_env(
        &self,
        code: &CompiledCode,
    ) -> Vec<Option<Value>> {
        code.locals
            .iter()
            .enumerate()
            .map(|(i, name)| {
                if name.starts_with('!')
                    || matches!(
                        self.locals[i].view(),
                        ValueView::HashEntryRef { .. } | ValueView::ContainerRef(_)
                    )
                {
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
                    self.locals[i].view(),
                    ValueView::HashEntryRef { .. } | ValueView::ContainerRef(_)
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
                // COW-detached copy). Two safe cases:
                // - a *type change away* from the container (`$a does Role`
                //   turns a Hash `$a` into a `Mixin`), discarding the old
                //   container wholesale;
                // - a clean observed REBIND: the slot and env were in sync
                //   before the carrier and the carrier produced a different
                //   value (`lives-ok { $parsed = %h{$k} }` re-run — the second
                //   Hash assignment was silently dropped, JSON::Marshal
                //   t/030-trait.t / t/080-type-constraints.t). A pre-carrier
                //   divergence (a live-cell copy) keeps the skip.
                if !self.locals[i].same_variant(&cur) {
                    self.locals[i] = cur;
                } else if let Some(Some(prev)) = pre_env.get(i)
                    && *prev == self.locals[i]
                    && *prev != cur
                {
                    self.locals[i] = cur;
                }
                continue;
            }
            // "Changed" must catch a *variant* change even when `PartialEq` treats
            // the two values as equal: `Mixin(Int(0), …)` compares EQUAL to
            // `Int(0)` (Mixin PartialEq delegates to its inner value), so a
            // `$a does Role` that turns an `Int` slot into an allomorphic `Mixin`
            // would otherwise be missed. Compare the enum discriminant first, then
            // the value.
            let changed = match pre_env.get(i) {
                Some(Some(prev)) => !prev.same_variant(&cur) || *prev != cur,
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
            v.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..)
                | ValueView::Complex(..)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::Package(_)
                | ValueView::Enum { .. }
                | ValueView::Version { .. }
                | ValueView::Uni(_)
                | ValueView::Nil
                | ValueView::Whatever
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
            if matches!(self.locals[i].view(), ValueView::HashEntryRef { .. })
                || name.starts_with('!')
            {
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
                self.locals[i].view(),
                ValueView::ContainerRef(_) | ValueView::HashEntryRef { .. }
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
            // §1.4 shadow slots: a name occupying several slots (an inner-block
            // shadow under MUTSU_SHADOW_SLOTS) cannot be broadcast — env holds
            // one value per name, and pushing an arbitrary (last-iterated)
            // same-named slot clobbers the live value with an uninitialized
            // sibling's. The per-write mirror (`flush_local_to_env`) keeps env
            // tracking the live slot instead. All-false with the gate off.
            if code.dup_named_locals.get(i).copied().unwrap_or(false) {
                continue;
            }
            self.set_env_with_main_alias(name, self.locals[i].clone());
        }
    }

    /// Like [`Self::sync_env_from_locals`], but skips slots whose name was
    /// never introduced into env — a compile-time-allocated local whose
    /// declaration has not run yet (e.g. a `state $b` later in the same loop
    /// body). Prematurely seeding env with such a name makes the declaration,
    /// when it does run, mistake the seeded entry for a live outer binding and
    /// record it for the loop-local shadow restore — which then wipes the
    /// variable at loop exit (advent2012-day15 FIRST/NEXT/LAST state loss).
    /// Mirrors the identical guard in
    /// [`Self::sync_regex_interpolation_env_from_locals`].
    ///
    /// Used by the I/O ops (Say/Put/Print/Note), whose pre-sync exists so a
    /// user `$*OUT` override / `.gist` method sees fresh values of *live*
    /// variables: any name such an override can legitimately reach via env is
    /// either already present in env (declared, captured, `our`, dynamic) or
    /// forced there by the reflective-access flag — never slot-only.
    pub(super) fn sync_env_from_locals_declared(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            if code.dup_named_locals.get(i).copied().unwrap_or(false) {
                continue;
            }
            if !self.env().contains_key(name) {
                continue;
            }
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
            // A shadow-duplicated name cannot be broadcast by name — see
            // `sync_env_from_locals` above.
            if code.dup_named_locals.get(i).copied().unwrap_or(false) {
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
            // Never clobber a name that is currently bound as a readonly param
            // (a `for ... -> $x` / sub param). The compiler allocates one slot
            // per *name* across the whole unit, so a sibling scope's `my $x`
            // produces a `code.locals` entry that shares this name but whose
            // slot is an uninitialised stale value here. The live binding lives
            // in `env` (params are env-authoritative), so pushing the stale
            // slot would overwrite the param with Nil. Skip it.
            if self.is_readonly(name) {
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
    /// have mirrored (bare-name lexicals and params); a slot-only local (read via
    /// GetLocal, e.g. `fib`'s `$n`) never reaches env.
    #[inline]
    pub(super) fn flush_local_to_env(&mut self, code: &CompiledCode, idx: usize) {
        // Slot-only locals (no name-based reader) never need to reach env.
        if !code.needs_env_sync.get(idx).copied().unwrap_or(true) {
            return;
        }
        let Some(name) = code.locals.get(idx) else {
            return;
        };
        let sym = code.locals_sym.get(idx).copied();
        // A plain lexical (the overwhelmingly common local: `my $x`, a scalar
        // param) has no aliases to maintain, so it takes the cheap writer. The
        // predicate is precomputed per slot; `plain_locals` is a strict subset of
        // `is_bare_param_name`, so the remaining mirrored names (dynamic `*x`,
        // `__ANON*`) still take the full alias-maintaining path below.
        if code.plain_locals.get(idx).copied().unwrap_or(false) {
            crate::vm::vm_stats::record_env_flush(1);
            self.set_env_plain_lexical(name, sym, self.locals[idx].clone());
            return;
        }
        // Skip topic (_), attributes (.x, !x), and package-qualified names to
        // avoid corrupting outer scope.
        if Self::is_bare_param_name(name) {
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
                // §1.4/§1.5: prefer the compiler-baked caller slot for this source
                // (folded in at arg-binding time) so the write lands on the LIVE
                // (inner shadow) slot, not the by-name `position` (outer) slot. Fall
                // back to the name search for sources with no baked slot (a
                // captured-outer free-var write, undefine env-identity, …).
                // Remove (not just read) this source's baked slot: it is consumed by
                // this drain. A blanket clear would wrongly drop an OUTER call's
                // still-pending slot when a NESTED call drains first (`f($a)` whose
                // body calls `g($b)` — g's drain must not lose f's `a` slot).
                let baked_raw = self
                    .pending_rw_writeback_slots
                    .remove(&source)
                    .map(|s| s as usize);
                // A baked slot index is only meaningful in the frame it was baked
                // in. When a NESTED call drains a source whose slot was baked for an
                // ANCESTOR frame (`f(@a, %h)` whose body calls `@a.map(...)` — the
                // `.map` mut-op drains f's still-pending `@a`/`%h` writeback), that
                // caller-frame index can collide with THIS frame's `locals` range
                // (`s < locals.len()`) yet name a *different* variable — writing the
                // wrong value into an unrelated slot (a Hash param clobbered by an
                // Array source). Trust the baked slot only when THIS frame's `code`
                // actually holds `source` at it; otherwise the source belongs to a
                // frame further up the stack, so retain it for that frame's own
                // drain rather than mis-applying (or draining early by name) here.
                let baked = baked_raw.filter(|&s| {
                    s < self.locals.len() && code.locals.get(s).is_some_and(|n| n == &source)
                });
                let slot = if baked.is_some() {
                    baked
                } else if baked_raw.is_some() {
                    // Baked for a different frame: retain for the owning frame.
                    None
                } else {
                    // No baked slot (a captured-outer free-var write, …): resolve
                    // the owning slot by name within this frame's code.
                    self.find_local_slot(code, &source)
                };
                if let Some(slot) = slot {
                    if !matches!(self.locals[slot].view(), ValueView::HashEntryRef { .. })
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
                if !matches!(self.locals[slot].view(), ValueView::HashEntryRef { .. })
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

    /// Drain `pending_local_updates` logged by an embedded regex `{ ... }` block
    /// (via `execute_regex_code_blocks`) into the caller's local slots after a
    /// method call. A `Grammar.parse` runs its rule's embedded code blocks inside
    /// this same interpreter, writing caller lexicals straight into `env` (e.g.
    /// `regex TOP { x { $x = 42 } }` mutating an outer `$x`); like the smartmatch
    /// path (`vm_smartmatch_ops`), write those through to the caller's local slots
    /// so the slot stays coherent, and record the miss for a caller-var writeback
    /// when the owning slot lives further up the stack. No-op unless a block wrote.
    pub(super) fn drain_pending_local_updates_after_call(&mut self, code: &CompiledCode) {
        if self.pending_local_updates.is_empty() {
            return;
        }
        // Only reconcile plain user variable names; internal/magic keys the block
        // touches (`?LINE`, `__mutsu_*`, `$/`-derived, …) are not caller lexicals.
        let is_user_var = |n: &str| -> bool {
            !n.is_empty()
                && !n.starts_with('?')
                && !n.starts_with("__")
                && n.bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b':')
        };
        let written: std::collections::HashSet<String> = self
            .pending_local_updates
            .iter()
            .map(|(n, _)| n.clone())
            .filter(|n| is_user_var(n))
            .collect();
        self.writeback_match_locals(code, &written);
        for name in &written {
            let is_match_name =
                name == "/" || (!name.is_empty() && name.bytes().all(|b| b.is_ascii_digit()));
            if is_match_name || self.find_local_slot(code, name).is_some() {
                continue;
            }
            self.record_caller_var_writeback(name);
        }
        self.pending_local_updates.clear();
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
                cur.view(),
                ValueView::Package(_)
                    | ValueView::Array(..)
                    | ValueView::Hash(..)
                    | ValueView::Sub(..)
                    | ValueView::Instance { .. }
                    | ValueView::Proxy { .. }
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

    /// Resolve a local slot for `name`, preferring the compile-time-baked `slot`
    /// (scope-correct even once a name occupies several `code.locals` slots) and
    /// falling back to the by-name search when the caller has no baked slot.
    /// §1.5 helper — see docs/lexical-scope-slot-campaign.md.
    pub(super) fn resolve_local_slot(
        &self,
        code: &CompiledCode,
        slot: Option<u32>,
        name: &str,
    ) -> Option<usize> {
        match slot {
            Some(s) if (s as usize) < self.locals.len() => Some(s as usize),
            // A baked slot that is out of range for this frame is treated as
            // "not a local here" (do not fall back to a by-name match, which
            // could pick a different variable's slot).
            Some(_) => None,
            None => self.find_local_slot(code, name),
        }
    }

    /// Read the local resolved by [`Self::resolve_local_slot`] when it exists.
    /// §1.5 slot-preferring mirror of [`Self::locals_get_by_name`].
    pub(super) fn read_local_slot_or_name(
        &self,
        code: &CompiledCode,
        slot: Option<u32>,
        name: &str,
    ) -> Option<Value> {
        self.resolve_local_slot(code, slot, name)
            .map(|s| self.locals[s].clone())
    }

    /// Write `val` into the slot resolved by [`Self::resolve_local_slot`] when it
    /// exists (no-op otherwise). §1.5 slot-preferring mirror write.
    pub(super) fn write_local_slot_or_name(
        &mut self,
        code: &CompiledCode,
        slot: Option<u32>,
        name: &str,
        val: Value,
    ) {
        if let Some(s) = self.resolve_local_slot(code, slot, name) {
            self.locals[s] = val;
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
