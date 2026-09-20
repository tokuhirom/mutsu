//! Routine / block / gather execution-stack accessors and current-package state.
use super::*;

impl Interpreter {
    /// Announce that something a name-keyed dispatch cache depends on changed,
    /// **other than** the registry's functions map itself — a routine was
    /// wrapped or unwrapped, a lexical import scope popped, a proto marker
    /// moved, an `our`-scoped alias was installed.
    ///
    /// Such a change leaves no trace in `Registry::functions`, so the map's
    /// version stamp (see `runtime::function_table`) cannot retire the affected
    /// answers and this call has to drop them outright. That is why it is the
    /// expensive form: every generation-tagged memo is emptied, not just the
    /// entries for one name, and the base-name key index goes with it.
    ///
    /// **Prefer [`Self::invalidate_fn_resolution_for_keys`]** for the ordinary
    /// case of "I inserted or removed these registry keys". That form keeps
    /// every memo whose generation the map itself will retire, which is what
    /// makes a per-call `my sub` re-registration affordable
    /// ([#8314](https://github.com/tokuhirom/mutsu/issues/8314)).
    ///
    /// `MUTSU_VM_STATS=1`'s `fn-resolve-gen-bumps` line attributes every
    /// generation *change* to its source location, so a new one is visible.
    #[track_caller]
    pub(crate) fn invalidate_fn_resolution(&mut self) {
        // Drop the generation-tagged memos outright. Retiring them by version
        // movement would not be enough here: the functions map may well be
        // about to travel back to a version these entries are tagged with (a
        // scope restore reinstalls a map this program has already run under),
        // and the change being announced would then be undone for the caches
        // but not for the interpreter -- a wrapped routine answering with its
        // pre-wrap resolution. Emptying them has no such failure mode.
        self.fn_resolve_cache.clear();
        self.multi_compiled_key_cache.clear();
        self.multi_candidates_cache.clear();
        self.declared_fn_cache.clear();
        self.multi_fn_cache.clear();
        crate::vm::vm_stats::record_fn_keys_base_invalidation(self.fn_keys_by_base.len());
        self.fn_keys_by_base.clear();
        // ...and give the map a version it has never had, so that the caches
        // which self-refresh off the generation rather than being cleared here
        // -- `light_call_cache`, `pos_light_call_cache`, `otf_call_cache`,
        // `func_multi_resolve_cache`, the ADR-0066 callsite inline-cache epoch --
        // see the change too. Mirroring the unchanged version instead left a
        // `&wrapped.wrap(...)` invisible to a call site that had already run
        // (`t/routines/call/call-inline-cache.t` test 6): nothing about the
        // functions map moved, so nothing retired the resolution those caches
        // were holding. Keeping `fn_resolve_gen == functions_version()` as a
        // whole-program invariant is also what makes the mirror easy to reason
        // about: there is exactly one name for "which map is installed".
        self.registry_mut().renew_functions_version();
        self.sync_fn_resolve_gen();
    }

    /// [`Self::invalidate_fn_resolution`] for the ordinary case: the caller
    /// wrote `Registry::functions` and knows exactly which registry keys it
    /// inserted or removed.
    ///
    /// Nothing is cleared wholesale here. The map stamped itself with a fresh
    /// version when the caller wrote it, so every memo tagged with the previous
    /// version is retired by that stamp alone — and, crucially, becomes live
    /// again if the map is later restored to the state it named. Entering
    /// `unjsonify-string` installs `JSON::Fast::fetch-codepoint` and leaving it
    /// removes that one key again, so the steady-state map is back after every
    /// call; the memos describing it now survive the round trip instead of
    /// being rebuilt from a full registry scan on the far side.
    ///
    /// The keys are still needed for `fn_keys_by_base`, which is a per-base-name
    /// index rather than a generation-tagged memo: only the base names the
    /// change actually touched are evicted.
    ///
    /// Passing a key that did NOT change is harmless (a spurious eviction);
    /// MISSING one that did is a stale index, which the debug-only audit in
    /// [`Self::fn_base_name_registered`] turns into a located panic on the next
    /// resolution rather than a silent mis-dispatch.
    #[track_caller]
    pub(crate) fn invalidate_fn_resolution_for_keys(
        &mut self,
        keys: impl IntoIterator<Item = Symbol>,
    ) {
        let mut evicted = 0usize;
        for key in keys {
            let spelled = key.resolve();
            let base = crate::runtime::dispatch_resolve::function_key_base_name(&spelled);
            let base_sym = Symbol::intern(base);
            if self.fn_keys_by_base.remove(&base_sym).is_some() {
                evicted += 1;
            }
        }
        crate::vm::vm_stats::record_fn_keys_base_invalidation(evicted);
        self.sync_fn_resolve_gen();
    }

    /// Re-read `fn_resolve_gen` from the functions map's own version stamp.
    ///
    /// `fn_resolve_gen` is a mirror of `Registry::functions_version()`, not a
    /// counter of its own: the map names its content and this field just
    /// carries that name to the memo read sites, which cannot take a registry
    /// guard on every probe. Mirroring rather than incrementing is what lets a
    /// generation *recur* -- a scope restore reinstalls the very `Arc` the
    /// snapshot took, version included, so the value here goes back to what it
    /// was before the excursion and the memos from before it are valid again.
    ///
    /// A call that finds the version unmoved is a no-op, which quietly absorbs
    /// the several places that announce one registry write twice (the
    /// `RegisterSub` opcode invalidates after `register_compiled_sub_decl`,
    /// which already invalidated on whichever install path it took).
    #[track_caller]
    fn sync_fn_resolve_gen(&mut self) {
        let version = self.registry().functions_version();
        if self.fn_resolve_gen == version {
            return;
        }
        self.fn_resolve_gen = version;
        crate::vm::vm_stats::record_fn_resolve_gen_bump(std::panic::Location::caller());
    }

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
    ///
    /// `name` is stripped of any `Pkg::` qualification before it is stored.
    /// A qualified callsite (`P::f()`) hands every call path here whatever
    /// text named the routine at the callsite -- some (the general dispatch
    /// entry) already resolve that back down to the routine's own short
    /// name first, others (the light-call fast paths, which push their own
    /// frame directly) do not, so a qualified call could reach this point
    /// carrying `P::f` as its OWN name. The qualification belongs on
    /// `package` alone: `resolve_code_var`'s `?ROUTINE` arm reads this
    /// frame's `name` straight into `&?ROUTINE.name`, so an unstripped
    /// qualification leaked there directly, and repeat calls (served by the
    /// light-call caches, which push their frame without going through the
    /// general entry at all) kept leaking it even once the general entry's
    /// own call was fixed to resolve the short name first (#8347).
    pub(crate) fn push_routine_with_location(
        &mut self,
        package: Symbol,
        name: Symbol,
        line: Option<u32>,
        file: Option<Symbol>,
        def_file: Option<Symbol>,
    ) {
        let invocation_id = self.take_invocation_id();
        let lexical_package = self.lexical_package_for_frame(def_file);
        let name = match name.as_str().rsplit_once("::") {
            Some((_, short)) => Symbol::intern(short),
            None => name,
        };
        let frame = super::RoutineFrame {
            package,
            lexical_package,
            name,
            line,
            file,
            is_method: false,
            is_submethod: false,
            is_block: false,
            def_file,
            invocation_id,
        };
        self.record_profile_routine_frame(&frame);
        self.routine_stack.push(frame);
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
        let invocation_id = self.take_invocation_id();
        let frame = super::RoutineFrame {
            package,
            lexical_package: Some(lexical_package),
            name,
            line,
            file,
            is_method: true,
            is_submethod,
            is_block: false,
            def_file,
            invocation_id,
        };
        self.record_profile_routine_frame(&frame);
        self.routine_stack.push(frame);
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
        let invocation_id = self.take_invocation_id();
        let frame = super::RoutineFrame {
            package,
            lexical_package: None,
            name,
            line,
            file,
            is_method: false,
            is_submethod: false,
            is_block: true,
            def_file,
            invocation_id,
        };
        self.record_profile_routine_frame(&frame);
        self.routine_stack.push(frame);
    }

    /// Record the exact routine entry only when the profiler consumer is
    /// armed.  Keeping this at the common frame-push boundary covers the fast,
    /// light, method, and closure dispatch paths with one implementation.
    ///
    /// `frame.file` is already the call site's lexical file
    /// (`Self::executing_source_file_sym`, resolved at push time by every
    /// `push_*_routine_with_location`), so this no longer needs its own
    /// outward walk over `routine_stack` to reconstruct it (#8743).
    pub(crate) fn record_profile_routine_frame(&self, frame: &super::RoutineFrame) {
        if crate::vm::vm_poll::profiler_armed() {
            crate::profile::record_routine_frame(frame);
        }
    }

    /// Find the unit-module package owning a routine or closure's body. EVAL
    /// units are not registered as modules themselves, but their parent unit
    /// is, so walk the same parent chain used by compilation-unit scoping.
    fn lexical_package_for_frame(&self, def_file: Option<Symbol>) -> Option<Symbol> {
        // An empty module metadata table decides the answer on its own: no
        // module has been declared or loaded anywhere, therefore no frame has
        // a lexical package. Answering that here rather than by walking the
        // chain is what avoids work on the hot path -- EVERY routine call runs
        // this check, and the
        // walk below costs a `Symbol::resolve` (a heap copy of the declaring
        // path), a re-intern of that copy, and a read of the process-global
        // `EVAL_UNIT_PARENTS` lock. On `benchmarks/bench-fib.raku`, which
        // declares no module and no `EVAL`, that dead work was 4.9% of the run
        // (#7788).
        if self.unit_module_packages.is_empty() && self.module_source_packages.is_empty() {
            return None;
        }
        // `def_file` is already interned; hand the `Symbol` straight to the
        // unit lookup instead of resolving it back into a `String` and making
        // `unit_of_source` re-intern that. Round-tripping a name through a
        // string is the pattern ADR-0037 removed from this very call path once
        // already.
        let mut unit = def_file
            .map(|file| self.unit_of_source_sym(Some(file)))
            .unwrap_or(self.current_unit);
        loop {
            if let Some(package) = self.unit_module_packages.get(&unit) {
                return Some(*package);
            }
            let Some(parent) = crate::runtime::eval_unit_parent(unit) else {
                break;
            };
            unit = parent;
        }
        def_file.and_then(|file| self.module_source_packages.get(&file).copied())
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

    /// The compilation unit the frame `CALLER::` names belongs to — the
    /// compunit-scoping counterpart of [`Self::caller_frame_package`], stamped
    /// onto the pseudo-stash so `EVAL ..., context => $ctx` can compile the
    /// snippet with the *caller's* import visibility rather than that of the
    /// module which happens to call `EVAL` (#7837).
    ///
    /// Same walk as [`Self::executing_unit_sym`], started one frame lower: a
    /// block frame carries no `def_file` of its own and belongs to whatever
    /// encloses it, so it is skipped. With no caller frame at all the caller is
    /// a mainline, whose unit is the one currently being loaded or run — which
    /// is what `?FILE` (`current_source_file_sym`) names, exactly as
    /// `executing_unit_sym` falls back to.
    pub(crate) fn caller_frame_unit(&self) -> Symbol {
        let len = self.routine_stack.len();
        if len >= 2 {
            for frame in self.routine_stack[..len - 1].iter().rev() {
                match frame.def_file {
                    Some(file) => return self.unit_of_source_sym(Some(file)),
                    None if frame.is_block => continue,
                    None => break,
                }
            }
        }
        self.unit_of_source_sym(self.current_source_file_sym())
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

    /// `Symbol` form of [`Self::executing_source_file`] — allocation-free,
    /// like [`Self::current_source_file_sym`] is for [`Self::current_source_file`].
    ///
    /// This is the file a `RoutineFrame` push records as its CALL SITE
    /// (`push_routine_with_location` and its method/block siblings): the site
    /// is in the body of whichever frame is on top of `routine_stack` right
    /// now (the caller, before the new frame is pushed), so it is the same
    /// walk `executing_source_file` does for "what file is running", not
    /// [`Self::current_source_file_sym`]'s dynamically-scoped `?FILE`. `?FILE`
    /// only tracks the unit currently *loading*, so it had already reverted to
    /// the importer's path by the time a `use`d module's own routine called
    /// another — filing the callee's call site under the script instead of the
    /// module that actually made the call ([#8743](https://github.com/tokuhirom/mutsu/issues/8743)).
    pub(crate) fn executing_source_file_sym(&self) -> Option<Symbol> {
        for frame in self.routine_stack.iter().rev() {
            match frame.def_file {
                Some(file) => return Some(file),
                None if frame.is_block => continue,
                None => break,
            }
        }
        self.current_source_file_sym()
    }

    /// [`Self::executing_source_file`], corrected for code running directly in
    /// a module's own top-level mainline while an unrelated routine call is
    /// still on `routine_stack` — the file-level counterpart of
    /// [`Self::executing_unit_sym_for_module_load`], and correct for exactly
    /// the same reason (see `module_loading_unit_stack`'s doc comment): a
    /// module body runs via `run_block`, which pushes no routine frame, so the
    /// plain frame walk answers whichever routine is still below it.
    ///
    /// Without it, `use`ing a module from inside an `EVAL` that itself runs
    /// inside a routine stamped every routine the module declares with the
    /// *calling* module's file — which then anchored the loaded module's own
    /// `sub EXPORT` to the wrong compunit and made its qualified
    /// self-reference (`Terminal::ANSI::OO.new` inside
    /// `Terminal/ANSI/OO.rakumod`'s own `EXPORT`) fail the #7797 visibility
    /// gate. That is the shape `Test.rakumod`'s `use-ok` produces (#7837).
    ///
    /// The corrected answer is `?FILE` ([`Self::current_source_file`]), NOT the
    /// unit symbol on `module_loading_unit_stack`, even though the two name the
    /// same file for a plain module mainline. `?FILE` is the dynamically-scoped
    /// "unit being compiled" marker, so [`Self::enter_source_file`] can retarget
    /// it — which it does for a deferred **role body**, re-run at each
    /// composition from the composing scope (`RoleDef::decl_file`). The unit
    /// stack still names the composing module there, so reading it would stamp
    /// the role's own lexical subs with the wrong file and make the role's
    /// methods unable to call them (zef's `role Plugin`'s `sub DEBUG`, caught by
    /// the bundled-library gate).
    pub(crate) fn executing_source_file_for_module_load(&self) -> Option<String> {
        if let Some(&(_, depth_at_push)) = self.module_loading_unit_stack.last()
            && self.routine_stack.len() == depth_at_push
        {
            return self.current_source_file();
        }
        self.executing_source_file()
    }

    /// Current routine-stack depth. Paired with [`Self::truncate_routine_stack`] so a
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

    /// The innermost frame's code object, built on demand for a lazy routine
    /// frame (see `CodeFrame`).
    pub(crate) fn block_stack_top(&self) -> Option<Value> {
        self.block_stack
            .last()
            .map(|frame| self.code_frame_value(frame))
    }

    pub(crate) fn push_block(&mut self, val: Value) {
        self.block_stack.push(CodeFrame::Ready(val));
    }

    /// Push a named routine's frame without building its `Sub`; a reader
    /// materializes it (`Interpreter::code_frame_value`).
    pub(crate) fn push_lazy_block(&mut self, code: LazyRoutineCode) {
        self.block_stack
            .push(CodeFrame::Lazy(std::sync::Arc::new(code)));
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
        // For instances, check class methods -- and the auto-generated public
        // accessor of a `has $.x`, which in Raku is an ordinary method of its
        // declaring class and so is exactly as `can`-able as a written one.
        // `class_has_method` only walks the method tables, so `can-ok $obj,
        // 'attr'` answered False for every accessor while `$obj.can('attr')`
        // (which goes through `resolve_user_method_or_accessor`) answered True
        // -- the two disagreed about the same question. That is what left
        // `Template::Nest::Fast` failing its `can-ok` sweep on six of seven
        // names.
        if let ValueView::Instance { class_name, .. } = value.view()
            && (self.class_has_method(&class_name.resolve(), method)
                || self.has_public_accessor(&class_name.resolve(), method))
        {
            return true;
        }
        // For type objects (`Chemistry::Elements.^can(...)` / `can-ok $type,
        // ...`), resolve methods against the named class's MRO too — a type
        // object can do any of its class's methods, not just the universal set.
        if let ValueView::Package(class_name) = value.view()
            && (self.class_has_method(&class_name.resolve(), method)
                || self.has_public_accessor(&class_name.resolve(), method))
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
        let call_depth = self.call_frames.len();
        let routine_depth = self.routine_stack_len();
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
                // driver cannot suspend soundly AT THE TAKE (the driver
                // snapshots only its own frame; the signal would unwind the
                // callee and corrupt the saved ip/stack — see
                // `lazy_pull_entry_call_depth` or
                // `lazy_pull_entry_routine_depth`). It can still suspend at
                // the next iteration boundary of a condition-driven loop that
                // lives in the driver's OWN frame, which is reached only after
                // the callee has returned: park the deferred-suspension flag
                // and let that boundary consume it (the consumption sites
                // check the frame depth themselves). Without this the pull
                // collected eagerly and forever whenever the gather body's
                // only takes came from a nested call under an infinite loop
                // (`gather { loop { self!bitmap(...) } }`, EuclideanRhythm).
                let nested_vm_call = self
                    .lazy_pull_entry_call_depth
                    .is_some_and(|entry| call_depth > entry);
                let nested_interpreter_call = self
                    .lazy_pull_entry_routine_depth
                    .is_some_and(|entry| routine_depth > entry);
                if nested_vm_call || nested_interpreter_call {
                    self.gather_suspend_pending = true;
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

    /// Whether a deferred lazy-pull suspension (`gather_suspend_pending`) may
    /// be taken at the iteration boundary the caller has just reached.
    ///
    /// The pull driver can only snapshot and resume its OWN frame, so only a
    /// loop running at (or above) the driver's entry VM/routine depth is a
    /// sound suspension point. A loop inside a routine the gather body called
    /// must leave the flag set and keep running: the flag survives the
    /// callee's return and the gather body's own loop consumes it one boundary
    /// later.
    pub(crate) fn gather_suspend_boundary_reached(&self) -> bool {
        if !self.gather_suspend_pending {
            return false;
        }
        let outside_vm_call = self
            .lazy_pull_entry_call_depth
            .is_none_or(|entry| self.call_frames.len() <= entry);
        let outside_interpreter_call = self
            .lazy_pull_entry_routine_depth
            .is_none_or(|entry| self.routine_stack_len() <= entry);
        outside_vm_call && outside_interpreter_call
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

    /// Switch `current_package` to the package a gather body was WRITTEN in
    /// (`__mutsu_gather_package`, captured by `exec_make_gather_op`), and hand
    /// back the caller's own package to restore once the pull is done.
    /// `None` when the env carries no such marker (an `EVAL`-built gather, or
    /// one predating this capture) — the caller then leaves `current_package`
    /// untouched, exactly as it did before this existed.
    pub(crate) fn enter_gather_package(&mut self, list_env: &crate::env::Env) -> Option<String> {
        let pkg = match list_env.get("__mutsu_gather_package")?.view() {
            ValueView::Str(pkg) => pkg.as_str().to_string(),
            _ => return None,
        };
        let saved = self.current_package();
        self.set_current_package(pkg);
        Some(saved)
    }

    /// The current package as an interned `Symbol`, read from the atomic mirror
    /// of `current_package`. Cheap enough (one relaxed load) for per-call use on
    /// the hot dispatch path, where `current_package()`'s `String` clone is not.
    /// The next routine-invocation id (see `RoutineFrame::invocation_id`), taken
    /// from this interpreter's claimed block. Refills from the process-global
    /// block allocator only when the block runs out.
    #[inline]
    pub(crate) fn take_invocation_id(&mut self) -> u64 {
        if self.next_invocation_id == self.invocation_id_block_end {
            let base = crate::runtime::claim_invocation_id_block();
            self.next_invocation_id = base;
            self.invocation_id_block_end = base + crate::runtime::INVOCATION_ID_BLOCK;
        }
        let id = self.next_invocation_id;
        self.next_invocation_id += 1;
        id
    }

    pub(crate) fn current_package_sym(&self) -> Symbol {
        Symbol::from_id(
            self.current_package_sym
                .load(std::sync::atomic::Ordering::Relaxed),
        )
    }

    /// Whether `current_package` is the "no package" case — unset, or the
    /// default top-level `GLOBAL`.
    ///
    /// Read from the interned mirror: one relaxed atomic load and two id
    /// compares. The spelling this replaces — `let cur = self.current_package();
    /// cur.is_empty() || cur == "GLOBAL"` — takes the `RwLock` and clones the
    /// package name onto the heap to compare it against two literals, and the
    /// `GetGlobal` fast-hit gate in `exec_one_dispatch` asks it on every
    /// unqualified global read: 1,091,175 of the 1,449,126 `String` clones
    /// `current_package()` made in a ten-decode `JSON::Fast` profile came from
    /// that one line.
    #[inline]
    pub(crate) fn current_package_is_global(&self) -> bool {
        let sym = self.current_package_sym();
        sym == crate::symbol::wk::empty_package() || sym == crate::symbol::wk::global_package()
    }

    pub(crate) fn set_current_package(&mut self, pkg: String) {
        let sym = Symbol::intern(&pkg);
        self.set_current_package_with_sym(pkg, sym);
    }

    /// [`Self::set_current_package`] for a caller that already holds the
    /// package's `Symbol` (a `CompiledFunction`'s cached `package_sym`). The
    /// by-name entry point re-hashed the package name on every named call
    /// (#7736).
    pub(crate) fn set_current_package_with_sym(&mut self, pkg: String, sym: Symbol) {
        // `lookup`, not `intern` -- see `baked_param_name_sym` (#7766).
        debug_assert_eq!(Symbol::lookup(&pkg), Some(sym));
        self.current_package_sym
            .store(sym.id(), std::sync::atomic::Ordering::Relaxed);
        *self.current_package.write().unwrap() = pkg;
    }

    /// Switch `current_package` to `sym`'s package, returning an RAII guard
    /// that restores the previous value when dropped -- on normal control flow
    /// OR when a Rust panic unwinds through the guarded call.
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
    ///
    /// Takes the target package as a `Symbol` only: the `String` the backing
    /// store holds is recoverable from it (`Symbol::as_str` is an indexed read
    /// out of the per-thread resolve table), so the `pkg: String` this used to
    /// take was pure per-call allocation at every call site — and the sites
    /// that had only a `Symbol` were spelling it `sym.as_str().to_string()`
    /// just to satisfy the signature, then paying `Symbol::intern` to get the
    /// symbol back. Three call sites, all of which hold the symbol already
    /// (`CompiledFunction::package_sym`, `SubData::package`); see #8686 Phase 1.
    ///
    /// **Entering the package that is already current is free.** The switch and
    /// its restore are then both no-ops, so neither is performed: the common
    /// shape on the hot path is a routine calling a sibling in its own package
    /// (every `JSON::Fast` helper calling the next), which used to pay two
    /// `String` allocations and three `RwLock` acquisitions to write back the
    /// value that was already there. The guard is still returned, and still
    /// restores on drop if the *body* moved `current_package` — only the
    /// redundant writes are skipped, so no caller has to reason about whether
    /// the switch happened.
    pub(crate) fn enter_package_guarded_sym(&mut self, sym: Symbol) -> CurrentPackageGuard {
        let saved_sym_id = self.current_package_sym().id();
        if sym.id() != saved_sym_id {
            self.set_current_package_with_sym(sym.as_str().to_owned(), sym);
        }
        CurrentPackageGuard {
            pkg_lock: std::sync::Arc::clone(&self.current_package),
            pkg_sym: std::sync::Arc::clone(&self.current_package_sym),
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
    ///
    /// NOTE: for a *compound declared name* (`class HTTP::HPACK::Decoder { ...
    /// }` written at file scope), the `::` segments are not real lexical scopes
    /// in raku — but this walk must still cross them, because they are also how
    /// mutsu models a module compunit's *file-scope* lexicals: `HTTP::HPACK`'s
    /// own `sub decode-int` is registered under `HTTP::HPACK` and reached from
    /// `HTTP::HPACK::Decoder`'s methods only by stripping a segment. Cutting the
    /// walk there breaks every bundled module written in that (very common)
    /// shape. The compound-name distinction is enforced for *type* names, where
    /// a precise registration-time record exists — see
    /// `Registry::compound_declared_types`.
    pub(crate) fn bare_name_packages(&self) -> Vec<String> {
        self.bare_name_packages_syms()
            .iter()
            .map(|s| s.as_str().to_string())
            .collect()
    }

    /// [`Self::bare_name_packages`] as interned symbols, memoized on the only
    /// two inputs it reads (see [`Interpreter::bare_name_packages_memo`]).
    ///
    /// This is the form every hot caller should use: the answer is an `Arc`
    /// clone rather than a fresh `Vec<String>` with a `String` per enclosing
    /// package, and the symbols compare by id instead of by `memcmp`. The
    /// `Vec<String>` wrapper above stays for the handful of callers that hand
    /// the names to something wanting owned strings.
    pub(crate) fn bare_name_packages_syms(&self) -> crate::runtime::BareNamePackages {
        let cur_sym = self.current_package_sym();
        let lexical_sym = self
            .routine_stack
            .last()
            .and_then(|frame| frame.lexical_package);
        if let Some(hit) = self
            .bare_name_packages_memo
            .borrow()
            .get(&(cur_sym, lexical_sym))
        {
            return hit.clone();
        }
        let computed: crate::runtime::BareNamePackages =
            Self::compute_bare_name_packages(cur_sym, lexical_sym);
        self.bare_name_packages_memo
            .borrow_mut()
            .insert((cur_sym, lexical_sym), computed.clone());
        computed
    }

    /// The pure derivation behind [`Self::bare_name_packages_syms`]: everything
    /// it reads is in its two arguments, which is what makes the memo need no
    /// invalidation.
    fn compute_bare_name_packages(
        cur_sym: Symbol,
        lexical_sym: Option<Symbol>,
    ) -> crate::runtime::BareNamePackages {
        let global = Symbol::intern("GLOBAL");
        let cur = cur_sym.as_str();
        let lexical = lexical_sym.map(|s| s.as_str());
        if cur == "GLOBAL" {
            return match (lexical_sym, lexical) {
                (Some(sym), Some(pkg)) if pkg != "GLOBAL" => vec![sym, cur_sym].into(),
                _ => vec![cur_sym].into(),
            };
        }
        // A `state`-variable scope key is not a package at all; treat it as
        // GLOBAL-only rather than walking its mangled segments.
        if cur.starts_with("__state_") {
            return vec![cur_sym, global].into();
        }
        // A mangled sub/closure scope (`Pkg::&name/2`, `Pkg::&<closure>/7`)
        // carries its real package as the part before `::&`. Walk outwards from
        // that, not from the mangled key.
        let head = cur.split("::&").next().unwrap_or("");
        // A parameterised role's current-package spelling carries its type
        // argument in brackets (`Bar::Holder[Bar::Comparable]`), and that
        // argument can itself be a namespaced type name with its own `::`
        // (`Algorithm::MinMaxHeap[Algorithm::MinMaxHeap::Comparable]`).
        // Walking outward with a plain `rsplit_once("::")` would then split
        // *inside* the bracket instead of at the real enclosing-package
        // boundary, producing garbage segments and never yielding the
        // role's own bare name (`Bar::Holder`) as a search package -- so a
        // bare routine imported into the role's file (a custom `infix:<...>`
        // operator, say) became unreachable from the role's own methods the
        // moment it was instantiated with a `::`-qualified type argument.
        // The bracket is a type argument, never a nested package, so the
        // walk must start from the part before the first `[`.
        let head = head.split('[').next().unwrap_or(head);
        let mut out = vec![cur_sym];
        if let (Some(sym), Some(pkg)) = (lexical_sym, lexical)
            && pkg != "GLOBAL"
            && !out.contains(&sym)
        {
            out.push(sym);
        }
        let mut probe = head;
        while !probe.is_empty() && probe != "GLOBAL" {
            if probe != cur {
                out.push(Symbol::intern(probe));
            }
            match probe.rsplit_once("::") {
                Some((outer, _)) => probe = outer,
                None => break,
            }
        }
        out.push(global);
        out.into()
    }

    /// Interior-mutable variant for the `&self` regex matcher: the package is
    /// stored behind a RwLock, so a temporary switch (e.g. into a cross-package
    /// grammar subrule's defining package while parsing its body) does not need
    /// `&mut self`.
    ///
    /// Takes the package as an interned `Symbol`: the matcher threads one
    /// (rather than a `&str`) down its whole call chain, and this switch is the
    /// one place on that chain that has to materialize the text
    /// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
    pub(crate) fn set_current_package_shared_sym(&self, sym: Symbol) {
        self.current_package_sym
            .store(sym.id(), std::sync::atomic::Ordering::Relaxed);
        *self.current_package.write().unwrap() = sym.as_str().to_owned();
    }
}

/// RAII guard returned by [`Interpreter::enter_package_guarded_sym`]. Restores
/// `current_package` on drop, including on a Rust panic unwind.
///
/// `current_package`/`current_package_sym` are already interior-mutable
/// (`Arc<RwLock<String>>` / `Arc<AtomicU32>`, the same handles
/// [`Interpreter::set_current_package_shared_sym`] uses), so this guard just
/// holds cloned `Arc` handles and writes through them directly on drop -- no
/// `&mut Interpreter` borrow is needed, so it stays fully safe (no raw
/// pointers) even though it is typically constructed deep inside a large
/// `&mut self` dispatch function and lives across many further `self.*`
/// calls before being dropped.
/// Only the saved package's `Symbol` id is held, not its text: the two are kept
/// in lockstep by every writer (`set_current_package_with_sym` asserts it,
/// `set_current_package_shared_sym` derives the text *from* the symbol, and the
/// `Interpreter` clones that build a fresh pair — a thread snapshot, a regex
/// scratch — copy both together, which [#7576](https://github.com/tokuhirom/mutsu/issues/7576)
/// is the record of), so the string is recoverable from the id and does not
/// need saving. That is what lets the guard be free to *construct*: it used to
/// clone the package out from behind its `RwLock` on every guarded call.
pub(crate) struct CurrentPackageGuard {
    pkg_lock: std::sync::Arc<std::sync::RwLock<String>>,
    pkg_sym: std::sync::Arc<std::sync::atomic::AtomicU32>,
    saved_sym_id: u32,
}

impl Drop for CurrentPackageGuard {
    fn drop(&mut self) {
        // Restore only if something actually moved. The `swap` both reads and
        // writes the mirror in one operation, so the guarded region ends with
        // the saved package current either way; the `RwLock` write and the
        // `String` allocation behind it are what the check is for, and they are
        // skipped for every guard whose region never left its own package.
        let previous = self
            .pkg_sym
            .swap(self.saved_sym_id, std::sync::atomic::Ordering::Relaxed);
        if previous != self.saved_sym_id {
            *self.pkg_lock.write().unwrap() =
                Symbol::from_id(self.saved_sym_id).as_str().to_owned();
        }
    }
}

#[cfg(test)]
mod call_site_file_tests {
    use super::*;
    use crate::runtime::Interpreter;

    fn frame(name: &str, def_file: Option<Symbol>, is_block: bool) -> super::super::RoutineFrame {
        super::super::RoutineFrame {
            package: Symbol::intern("Module"),
            lexical_package: None,
            name: Symbol::intern(name),
            line: Some(1),
            file: None,
            is_method: false,
            is_submethod: false,
            is_block,
            def_file,
            invocation_id: 1,
        }
    }

    /// The call site of a routine invoked from inside a `use`d module's own
    /// body is that module's file -- not the dynamically-scoped `?FILE`,
    /// which has already reverted to whatever script `use`d the module by
    /// the time one of the module's OWN routines calls another (#8743).
    /// `executing_source_file_sym` is what every `push_*_routine_with_location`
    /// call site now uses instead of `current_source_file_sym` for exactly
    /// this reason.
    #[test]
    fn call_site_is_the_lexical_file_of_the_calling_routine() {
        let mut interp = Interpreter::new();
        let module = Symbol::intern("Module.rakumod");
        interp
            .routine_stack
            .push(frame("outer", Some(module), false));
        assert_eq!(interp.executing_source_file_sym(), Some(module));
    }

    /// A block frame with no `def_file` of its own (an inlined bare block)
    /// belongs to whichever routine lexically encloses it, so a call made
    /// from inside it is still attributed to that routine's file.
    #[test]
    fn an_inlined_block_inherits_its_enclosing_routines_file() {
        let mut interp = Interpreter::new();
        let module = Symbol::intern("Module.rakumod");
        interp
            .routine_stack
            .push(frame("outer", Some(module), false));
        interp.routine_stack.push(frame("", None, true));
        assert_eq!(interp.executing_source_file_sym(), Some(module));
    }

    /// With no enclosing routine at all, the call site is whatever mainline
    /// is currently running -- exactly what the dynamically-scoped `?FILE`
    /// tracks correctly (module loading scopes it for the duration of that
    /// module's own mainline), so the walk falls back to it.
    #[test]
    fn with_no_enclosing_routine_the_call_site_is_the_running_mainline() {
        let interp = Interpreter::new();
        assert!(interp.routine_stack().is_empty());
        assert_eq!(
            interp.executing_source_file_sym(),
            interp.current_source_file_sym()
        );
    }
}
