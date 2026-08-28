//! Routine / block / gather execution-stack accessors and current-package state.
use super::*;

impl Interpreter {
    pub(crate) fn routine_stack_top(&self) -> Option<&super::RoutineFrame> {
        self.routine_stack.last()
    }

    pub(crate) fn routine_stack(&self) -> &[super::RoutineFrame] {
        &self.routine_stack
    }

    /// Whether an actual **routine** (sub/method) encloses the running code —
    /// not merely some frame. A bare `{ ... }` block, a `for` body and a
    /// closure all push a `RoutineFrame` with `is_block: true`, so
    /// `!routine_stack.is_empty()` answers "is any frame live", which is a
    /// different question.
    ///
    /// It matters for `return`: a `return` in a non-routine block does a
    /// *non-local* return when a routine lexically encloses it, and throws
    /// `X::ControlFlow::Return` when none does. Deciding that from
    /// `is_empty()` made an `EVAL` run inside a mainline `{ ... }` block
    /// compile its snippet as "inside a routine", so a `return` in the
    /// snippet's own pointy block returned from whatever sub later called it
    /// instead of throwing (`roast/S04-statements/return.t` test 15).
    pub(crate) fn enclosing_routine_exists(&self) -> bool {
        self.routine_stack.iter().any(|f| !f.is_block)
    }

    /// Whether a `return`'s captured `return_target_callable_id` still names
    /// a routine frame actually on the dynamic call stack right now — the
    /// general, "right at the return site" liveness check `EvalContextRoutineState`
    /// (`classify_eval_context_routine`, ADR-0037 §2.3) already applies for an
    /// `EVAL ..., context => $ctx` unit, generalized here for an ordinary
    /// (non-EVAL) closure's captured `return`.
    ///
    /// A `return` inside a closure lexically written inside routine `R`
    /// always compiles to propagate a `CX::Return` signal (the compiler
    /// cannot know at compile time whether `R`'s call frame will still be
    /// live when the closure is eventually invoked — that is a dynamic
    /// question). Every routine-call boundary the signal passes through
    /// (`vm_call_named_inner.rs`, `vm_closure_dispatch.rs`) checks "is this
    /// frame the target" and keeps propagating on a miss — correct as far as
    /// it goes, but it never asks "could ANY live frame still be the
    /// target", so a signal whose target already exited keeps propagating
    /// uncaught straight through every `try`/`CATCH` boundary along the way,
    /// instead of being caught by the nearest one (raku: the nearest
    /// enclosing `CATCH` sees a real `X::ControlFlow::Return`).
    ///
    /// This walks every live (non-block) routine frame and resolves its
    /// CURRENT registration id via `registration_clone_id` — the same id
    /// space `RuntimeError::return_target_callable_id` and `SubData::id`
    /// live in (see that field's doc comment) — so it answers precisely
    /// "does `target_id` still belong to something on the stack", not just
    /// "is the stack non-empty" (which `enclosing_routine_exists` answers,
    /// too coarse here: an unrelated routine frame, e.g. a `subtest`
    /// wrapper, does not make a DIFFERENT routine's captured return live).
    pub(crate) fn return_target_is_live(&self, target_id: u64) -> bool {
        self.routine_stack.iter().any(|f| {
            !f.is_block
                && self.registration_clone_id(&f.package.resolve(), &f.name.resolve())
                    == Some(target_id)
        })
    }

    /// Push a new routine frame. `line` and `file` record the call-site
    /// in the *caller* (the line/file where this function was called from);
    /// `def_file` is the file the routine's body lives in (None = main
    /// script), used by backtrace rendering.
    ///
    /// Takes `Symbol`s (`RoutineFrame`'s fields are all interned) so the push
    /// itself never allocates; a caller holding only a `&str`/`String` should
    /// intern via `Symbol::intern` (a thread-local cache hit after the first
    /// call for a given call site, since the same name/package is reused on
    /// every repeat call).
    pub(crate) fn push_routine_with_location(
        &mut self,
        package: Symbol,
        name: Symbol,
        line: Option<u32>,
        file: Option<Symbol>,
        def_file: Option<Symbol>,
    ) {
        self.routine_stack.push(super::RoutineFrame {
            package,
            lexical_package: None,
            name,
            line,
            file,
            is_method: false,
            is_submethod: false,
            is_block: false,
            def_file,
            invocation_id: crate::runtime::next_invocation_id(),
        });
    }

    /// Push a method routine frame. `line`/`file` record the call-site (as for
    /// `push_routine_with_location`); `def_file` is the file the method body was
    /// *declared* in (the class's `use`d module, or `None` for the main script
    /// or a synthetic/native method) — see `MethodDef::source_file`. Without
    /// this, `executing_source_file()`'s frame walk always fell through past a
    /// method frame to the dynamically-scoped `?FILE`, which had already
    /// reverted to the main script by the time the method ran.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn push_method_routine_with_location(
        &mut self,
        package: Symbol,
        lexical_package: Symbol,
        name: Symbol,
        line: Option<u32>,
        file: Option<Symbol>,
        def_file: Option<Symbol>,
        is_submethod: bool,
    ) {
        self.routine_stack.push(super::RoutineFrame {
            package,
            lexical_package: Some(lexical_package),
            name,
            line,
            file,
            is_method: true,
            is_submethod,
            is_block: false,
            def_file,
            invocation_id: crate::runtime::next_invocation_id(),
        });
    }

    /// Push a block/closure routine frame. `def_file` is the file the block's
    /// body was written in, known when the frame comes from a closure value
    /// (`SubData::source_file`); an inlined bare block passes `None` and is
    /// attributed to the routine that lexically encloses it.
    pub(crate) fn push_block_routine_with_location(
        &mut self,
        package: Symbol,
        name: Symbol,
        line: Option<u32>,
        file: Option<Symbol>,
        def_file: Option<Symbol>,
    ) {
        self.routine_stack.push(super::RoutineFrame {
            package,
            lexical_package: None,
            name,
            line,
            file,
            is_method: false,
            is_submethod: false,
            is_block: true,
            def_file,
            invocation_id: crate::runtime::next_invocation_id(),
        });
    }

    pub(crate) fn pop_routine(&mut self) {
        self.routine_stack.pop();
    }

    /// The package of the frame `CALLER::` names — the one below the frame
    /// currently executing. A block frame carries the package its closure was
    /// created in, so a `subtest { ... }` body written in the test script
    /// answers the script's package rather than the module's.
    ///
    /// With no caller frame the caller is the mainline, whose package is the
    /// compilation unit's; a script's is `GLOBAL`. (A `unit module` mainline
    /// calling into another module would want that module's name here, but
    /// `current_package` has already moved on to the callee by this point, so
    /// it is not recoverable from the stack.)
    pub(crate) fn caller_frame_package(&self) -> String {
        let len = self.routine_stack.len();
        if len >= 2 {
            return self.routine_stack[len - 2].package.resolve();
        }
        "GLOBAL".to_string()
    }

    /// `package::name` of the routine that dynamically encloses the frame
    /// `CALLER::` names — walking down past any *block* frames (a bare
    /// `{ ... }`, a `for` body, a closure) starting at `caller_frame_package`'s
    /// same frame (`routine_stack[len - 2]`) to the nearest actual routine
    /// (`is_block == false`). `None` when no routine encloses it at all: the
    /// frame `CALLER::` names is the mainline, or (with fewer than two frames
    /// live) there is no caller frame to walk from in the first place.
    ///
    /// This is ADR-0037 §2.2's control-flow identity, stamped onto the
    /// pseudo-stash alongside the package so `EVAL ..., context => $ctx` can
    /// later classify the snippet's `return` (§2.3) instead of only naming
    /// its package (`caller_frame_package`, `stamp_stash_origin_package`).
    pub(crate) fn caller_frame_enclosing_routine(&self) -> Option<String> {
        let len = self.routine_stack.len();
        if len < 2 {
            return None;
        }
        self.routine_stack[..len - 1]
            .iter()
            .rev()
            .find(|f| !f.is_block)
            .map(|f| format!("{}::{}", f.package, f.name))
    }

    /// The file the code currently executing was *defined* in — the module path
    /// for a routine that came from a `use`d module, the script otherwise.
    ///
    /// `?FILE` in env only tracks the unit being *loaded*, so once a module's
    /// mainline has finished it reads the script again; a routine's own file
    /// survives on its frame as `def_file`. Backtrace rendering already prefers
    /// `def_file` the same way (`vm_helpers.rs`); `callframe` needs it so a frame
    /// running inside a module reports the module, which is how a test framework
    /// walks past its own frames to find the caller's failure site.
    ///
    /// An *inlined* bare block records no `def_file` of its own — it belongs to
    /// the routine that lexically encloses it — so such a frame is skipped in
    /// favour of the frame below it. A block that came from a closure value does
    /// carry one and is answered directly, which is what keeps a block written
    /// in the caller's file attributed there even while a module invokes it.
    pub(crate) fn executing_source_file(&self) -> Option<String> {
        for frame in self.routine_stack.iter().rev() {
            match frame.def_file {
                Some(file) => return Some(file.resolve()),
                None if frame.is_block => continue,
                None => break,
            }
        }
        self.current_source_file()
    }

    /// Current routine-stack depth. Paired with [`truncate_routine_stack`] so a
    /// structured execution boundary (block scope, try/catch) can record its
    /// entry depth and restore it on exit, exception-safely.
    pub(crate) fn routine_stack_len(&self) -> usize {
        self.routine_stack.len()
    }

    /// Drop routine frames down to `len`. Used by block/try executors to remove
    /// the bare-block callframe they pushed (and reclaim any frames a nested
    /// bare block leaked when its body threw past its own cleanup).
    pub(crate) fn truncate_routine_stack(&mut self, len: usize) {
        self.routine_stack.truncate(len);
    }

    pub(crate) fn block_stack_top(&self) -> Option<&Value> {
        self.block_stack.last()
    }

    pub(crate) fn push_block(&mut self, val: Value) {
        self.block_stack.push(val);
    }

    pub(crate) fn pop_block(&mut self) {
        self.block_stack.pop();
    }

    /// Stringify a value, calling the `.Str` method for Instance and Package types.
    pub(crate) fn stringify_value(&mut self, value: Value) -> Result<String, RuntimeError> {
        match value.view() {
            ValueView::Instance { .. } | ValueView::Package(_) => {
                let result = self.call_method_with_values(value, "Str", vec![])?;
                Ok(result.to_string_value())
            }
            _ => Ok(value.to_string_value()),
        }
    }

    /// Check if a value can respond to a given method name.
    pub(crate) fn value_can_method(&mut self, value: &Value, method: &str) -> bool {
        // ADR-0019 Phase E box E11: the arity-cascade catalog
        // (`Interpreter::e2_native_method_exists`) replaces a dummy-0-arg-only
        // `native_method_0arg` probe here, which missed every 1-arg-or-later
        // native method entirely (`can-ok "abc", "substr"` / `"index"` failed
        // even though `raku` passes both -- a real gap, not a style choice).
        let method_sym = crate::symbol::Symbol::intern(method);
        if self.e2_native_method_exists(value, method_sym.as_str()) {
            return true;
        }
        // For instances, check class methods
        if let ValueView::Instance { class_name, .. } = value.view()
            && self.class_has_method(&class_name.resolve(), method)
        {
            return true;
        }
        // For type objects (`Chemistry::Elements.^can(...)` / `can-ok $type,
        // ...`), resolve methods against the named class's MRO too — a type
        // object can do any of its class's methods, not just the universal set.
        if let ValueView::Package(class_name) = value.view()
            && self.class_has_method(&class_name.resolve(), method)
        {
            return true;
        }
        // Universal methods available on all values
        matches!(
            method,
            "WHAT"
                | "say"
                | "print"
                | "put"
                | "gist"
                | "Str"
                | "Int"
                | "Num"
                | "Bool"
                | "Numeric"
                | "Real"
                | "so"
                | "not"
                | "defined"
                | "isa"
                | "can"
                | "does"
                | "ACCEPTS"
                | "raku"
                | "perl"
                | "clone"
                | "new"
        )
    }

    pub(crate) fn take_value(&mut self, val: Value) -> Result<(), RuntimeError> {
        if let Some(items) = self.gather_items.last_mut() {
            // `take` of a Slip flattens it into the gather (`take Empty` /
            // `take slip(1,2)` add zero / two elements — Rakudo semantics);
            // every other value, including a List/Seq, is added as one element
            // (a later `flat`/`.flat` on the gather result flattens those).
            if let ValueView::Slip(elems) = val.view() {
                items.extend(elems.iter().cloned());
            } else {
                items.push(val);
            }
            if let Some(Some(limit)) = self.gather_take_limits.last()
                && items.len() >= *limit
            {
                // A take inside a routine call NESTED under the lazy-pull
                // driver cannot suspend soundly (the driver snapshots only its
                // own frame; the signal would unwind the callee and corrupt
                // the saved ip/stack — see `lazy_pull_entry_call_depth`). Keep
                // collecting eagerly instead; over-production is correct.
                if self
                    .lazy_pull_entry_call_depth
                    .is_some_and(|entry| self.call_frames.len() > entry)
                {
                    return Ok(());
                }
                if self.lazy_take_boundary_defer {
                    // Inside a condition-driven loop: defer the suspension to
                    // the loop's iteration boundary (`gather_suspend_pending`)
                    // — suspending at the take itself replays the statements
                    // between the take and the iteration end on resume. The
                    // overshoot backstop still signals here if no boundary is
                    // ever reached.
                    self.gather_suspend_pending = true;
                    if items.len() >= limit.saturating_add(64) {
                        self.gather_suspend_pending = false;
                        return Err(RuntimeError::new(
                            "__mutsu_lazy_gather_take_limit_reached__",
                        ));
                    }
                } else {
                    return Err(RuntimeError::new(
                        "__mutsu_lazy_gather_take_limit_reached__",
                    ));
                }
            }
        }
        Ok(())
    }

    pub(crate) fn gather_items_len(&self) -> usize {
        self.gather_items.len()
    }

    pub(crate) fn push_gather_items(&mut self, items: Vec<Value>) {
        self.gather_items.push(items);
    }

    pub(crate) fn pop_gather_items(&mut self) -> Option<Vec<Value>> {
        self.gather_items.pop()
    }

    pub(crate) fn current_gather_items(&self) -> Vec<Value> {
        self.gather_items.last().cloned().unwrap_or_default()
    }

    pub(crate) fn push_gather_take_limit(&mut self, limit: Option<usize>) {
        self.gather_take_limits.push(limit);
    }

    pub(crate) fn pop_gather_take_limit(&mut self) {
        self.gather_take_limits.pop();
    }

    /// The package currently in scope, read out of the shared `Arc<RwLock>`
    /// handle as an owned `String`. Returns owned (not `&str`) because the value
    /// lives behind a lock guard that must not escape the call — the guard is
    /// dropped before returning, so no lock is held across the caller's work
    /// (re-entry safe, mirroring the registry accessors).
    pub(crate) fn current_package(&self) -> String {
        self.current_package.read().unwrap().clone()
    }

    /// The current package as an interned `Symbol`, read from the atomic mirror
    /// of `current_package`. Cheap enough (one relaxed load) for per-call use on
    /// the hot dispatch path, where `current_package()`'s `String` clone is not.
    pub(crate) fn current_package_sym(&self) -> Symbol {
        Symbol::from_id(
            self.current_package_sym
                .load(std::sync::atomic::Ordering::Relaxed),
        )
    }

    pub(crate) fn set_current_package(&mut self, pkg: String) {
        self.current_package_sym.store(
            Symbol::intern(&pkg).id(),
            std::sync::atomic::Ordering::Relaxed,
        );
        *self.current_package.write().unwrap() = pkg;
    }

    /// Switch `current_package` to `pkg`, returning an RAII guard that
    /// restores the previous value when dropped -- on normal control flow OR
    /// when a Rust panic unwinds through the guarded call.
    ///
    /// Several call-dispatch functions (`call_compiled_closure_with_topic`,
    /// `call_compiled_function_named_inner`) temporarily switch
    /// `current_package` to the callee's declaring package for the duration
    /// of the call, then restore it with a plain `self.set_current_package(saved)`
    /// statement near the end of the function. A Rust panic caught at an
    /// outer `catch_unwind` boundary (`run_inner_guarded`/`run_range_guarded`)
    /// skips straight past that statement -- only `Drop` runs on unwind -- so
    /// `current_package` was left as the panicking callee's own package
    /// instead of the caller's, and the very next unqualified call resolved
    /// against the wrong package ("Unknown function: ..."). See
    /// `todo/deep/panic-unwind-leaks-side-channel-call-state.md`.
    ///
    /// Returns a guard rather than fixing the field via the `call_frames`
    /// recovery pop-loop (`recover_call_frames_after_panic`) because the
    /// switch does not happen at `push_call_frame()` time in either caller --
    /// moving it there would require restructuring both dispatch functions.
    /// An RAII guard self-heals regardless of what a future unwind boundary
    /// looks like.
    pub(crate) fn enter_package_guarded(&mut self, pkg: String) -> CurrentPackageGuard {
        let saved_str = self.current_package();
        let saved_sym_id = self.current_package_sym().id();
        self.set_current_package(pkg);
        CurrentPackageGuard {
            pkg_lock: std::sync::Arc::clone(&self.current_package),
            pkg_sym: std::sync::Arc::clone(&self.current_package_sym),
            saved_str,
            saved_sym_id,
        }
    }

    /// The packages a *bare* (unqualified) routine name is looked up in. A
    /// method's declaring compunit package follows its owning class, before
    /// unrelated enclosing namespaces; the walk always ends at `GLOBAL`.
    ///
    /// A `class` declared inside a `module` is registered under the
    /// module-qualified name (`NL::Searcher` for `class Searcher` in
    /// `unit module NL`), so stripping one `::` segment at a time reproduces the
    /// lexical nesting the declaration came from: a method of `NL::Searcher`
    /// calling a bare `cannon-name` must find `NL`'s `cannon-name`, exactly as
    /// raku's lexical lookup does. Before this existed, bare-name lookup jumped
    /// straight from the current package to `GLOBAL`, so the module's own subs
    /// were invisible to its classes' methods (the `NativeLibs`/`DBIish`
    /// blocker).
    ///
    /// The common case — mainline code under `GLOBAL` — returns a single
    /// element, so callers pay one small `Vec` for what used to be two
    /// hard-coded `format!`s.
    pub(crate) fn bare_name_packages(&self) -> Vec<String> {
        let cur = self.current_package();
        let lexical = self
            .routine_stack
            .last()
            .and_then(|frame| frame.lexical_package)
            .map(|s| s.as_str());
        if cur == "GLOBAL" {
            return match lexical {
                Some(pkg) if pkg != "GLOBAL" => vec![pkg.to_string(), cur],
                _ => vec![cur],
            };
        }
        // A `state`-variable scope key is not a package at all; treat it as
        // GLOBAL-only rather than walking its mangled segments.
        if cur.starts_with("__state_") {
            return vec![cur, "GLOBAL".to_string()];
        }
        // A mangled sub/closure scope (`Pkg::&name/2`, `Pkg::&<closure>/7`)
        // carries its real package as the part before `::&`. Walk outwards from
        // that, not from the mangled key.
        let head = cur.split("::&").next().unwrap_or("").to_string();
        let mut out = vec![cur];
        if let Some(pkg) = lexical
            && pkg != "GLOBAL"
            && !out.iter().any(|candidate| candidate == pkg)
        {
            out.push(pkg.to_string());
        }
        let mut probe = head;
        while !probe.is_empty() && probe != "GLOBAL" {
            if probe != out[0] {
                out.push(probe.clone());
            }
            match probe.rsplit_once("::") {
                Some((outer, _)) => probe = outer.to_string(),
                None => break,
            }
        }
        out.push("GLOBAL".to_string());
        out
    }

    /// Interior-mutable variant for the `&self` regex matcher: the package is
    /// stored behind a RwLock, so a temporary switch (e.g. into a cross-package
    /// grammar subrule's defining package while parsing its body) does not need
    /// `&mut self`.
    pub(crate) fn set_current_package_shared(&self, pkg: String) {
        self.current_package_sym.store(
            Symbol::intern(&pkg).id(),
            std::sync::atomic::Ordering::Relaxed,
        );
        *self.current_package.write().unwrap() = pkg;
    }
}

/// RAII guard returned by [`Interpreter::enter_package_guarded`]. Restores
/// `current_package` on drop, including on a Rust panic unwind.
///
/// `current_package`/`current_package_sym` are already interior-mutable
/// (`Arc<RwLock<String>>` / `Arc<AtomicU32>`, the same handles
/// [`Interpreter::set_current_package_shared`] uses), so this guard just
/// holds cloned `Arc` handles and writes through them directly on drop -- no
/// `&mut Interpreter` borrow is needed, so it stays fully safe (no raw
/// pointers) even though it is typically constructed deep inside a large
/// `&mut self` dispatch function and lives across many further `self.*`
/// calls before being dropped.
pub(crate) struct CurrentPackageGuard {
    pkg_lock: std::sync::Arc<std::sync::RwLock<String>>,
    pkg_sym: std::sync::Arc<std::sync::atomic::AtomicU32>,
    saved_str: String,
    saved_sym_id: u32,
}

impl Drop for CurrentPackageGuard {
    fn drop(&mut self) {
        *self.pkg_lock.write().unwrap() = std::mem::take(&mut self.saved_str);
        self.pkg_sym
            .store(self.saved_sym_id, std::sync::atomic::Ordering::Relaxed);
    }
}
