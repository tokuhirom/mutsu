use super::*;
use crate::compiler::Compiler;
use crate::symbol::Symbol;

impl Interpreter {
    /// Get the current source line number from the interpreter.
    pub(super) fn current_source_line(&self) -> Option<u32> {
        Some(self.cur_source_line as u32)
    }

    /// Get the current source file from the interpreter env.
    pub(crate) fn current_source_file(&self) -> Option<String> {
        self.env().get("?FILE").and_then(|v| match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        })
    }

    /// `Symbol` variant of [`Self::current_source_file`]: interns instead of
    /// allocating a fresh `String`. `?FILE` is the same value for the whole
    /// run of a compilation unit, so after the first call this is a
    /// thread-local cache hit — no allocation — which is what makes it cheap
    /// enough for `RoutineFrame` pushes on a hot call path (see
    /// `vm_call_fast.rs`).
    pub(crate) fn current_source_file_sym(&self) -> Option<Symbol> {
        self.env().get("?FILE").and_then(|v| match v.view() {
            ValueView::Str(s) => Some(Symbol::intern(s.as_str())),
            _ => None,
        })
    }

    /// Attach the defining scope to an interpolating regex literal
    /// (`OpCode::LoadRegexClosure`).
    ///
    /// Reads each listed name out of the creating frame — its local slot when
    /// the compiler baked one, otherwise `env` — and hands back a
    /// `Value::RegexCaptured` (or an adverb-bearing regex with its `captured`
    /// field filled). A name that resolves nowhere is simply left out, so the
    /// value is byte-equivalent to today's plain constant when nothing is
    /// captured. A capture that is already a shared `ContainerRef` cell is kept
    /// AS the cell, so later writes through it stay visible to the regex.
    ///
    /// A name flagged in `code.needs_cell_regex` (own, and mutated after this
    /// regex literal is constructed — see that field's doc comment) is boxed
    /// into a shared cell via `box_decl_local_cell` BEFORE being read, so the
    /// captured value IS the cell: later writes to the defining frame's local
    /// flow through it, and the stored regex observes them at match time
    /// (raku-verified same-scope mutation semantics; bug 1 of
    /// `todo/tickets/stored-regex-loses-its-defining-scope-lexicals.md`).
    /// Unmutated captures stay cheap by-value snapshots.
    pub(super) fn capture_regex_closure(
        &mut self,
        code: &CompiledCode,
        base: &Value,
        captures: &[(Symbol, u32)],
    ) -> Value {
        let mut scope: HashMap<String, Value> = HashMap::new();
        for (sym, slot) in captures {
            let name = sym.resolve();
            if *slot != crate::opcode::NOT_A_LOCAL && code.needs_cell_regex.contains(sym) {
                self.box_decl_local_cell(code, *slot as usize);
            }
            let from_local = (*slot != crate::opcode::NOT_A_LOCAL)
                .then(|| self.locals.get(*slot as usize))
                .flatten()
                .filter(|v| !v.is_nil())
                .cloned();
            let Some(v) = from_local.or_else(|| self.env().get(name.as_str()).cloned()) else {
                continue;
            };
            if v.is_nil() {
                continue;
            }
            scope.insert(name.to_string(), v);
        }
        if scope.is_empty() {
            return base.clone();
        }
        let scope = std::sync::Arc::new(scope);
        match base.view() {
            ValueView::Regex(p) => Value::regex_closure(std::sync::Arc::clone(&p), scope),
            ValueView::RegexWithAdverbs(a) => {
                let mut adv = a.clone();
                adv.captured = Some(scope);
                Value::regex_with_adverbs(adv)
            }
            _ => base.clone(),
        }
    }

    pub(super) fn exec_make_gather_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            // Box captured-and-mutated lexicals the gather body reads into shared
            // ContainerRef cells BEFORE snapshotting the env: the body pulls
            // lazily after this frame moves on, so a by-value copy would miss
            // later writes (`my $x = 1; my $s = gather { take $x }; $x = 2` must
            // take 2). The analysis closure (compiled from the same body by
            // surface_stashed_body_free_vars) names the free vars; the boxing
            // rules are exactly the closure-capture ones.
            let analysis_cc = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &analysis_cc);
            let mut env = self.env().clone();
            env.insert("__mutsu_lazylist_from_gather".to_string(), Value::TRUE);
            // A `samewith` in the body redispatches the routine the gather was
            // WRITTEN in, but the body runs after that routine has returned and
            // its dynamic dispatch frame has been popped. Capture the context
            // with the env snapshot so it survives (Digest::SHA3's `Keccak`
            // ends in `gather for samewith $inputBytes, ...`).
            self.capture_samewith_context_into(&mut env);
            // A forward aggregate free var with no baked parent slot is the
            // self-reference in a binding initializer (`my @a := gather {
            // ... @a ... }`). Tag it before the LazyList is built; GetArrayVar
            // then exposes the live take collector while this gather is being
            // forced, instead of reading the pre-declaration Any snapshot.
            if let Some(analysis) = &analysis_cc {
                for (i, sym) in analysis.free_var_syms.iter().enumerate() {
                    let has_baked_slot = analysis
                        .free_var_parent_slots
                        .get(i)
                        .copied()
                        .flatten()
                        .is_some();
                    let is_forward_aggregate = !has_baked_slot
                        && sym.with_str(|name| {
                            (name.starts_with('@') || name.starts_with('%'))
                                && code.locals.iter().any(|local| local == name)
                        });
                    if is_forward_aggregate {
                        sym.with_str(|name| {
                            env.insert(format!("__mutsu_gather_self_ref::{name}"), Value::TRUE);
                        });
                    }
                }
            }
            // Compile the gather body to bytecode for Interpreter-native forcing.
            // A `sub` declared in the body is lexical to it, so compile through a
            // `Stmt::Block` (whose `BlockScope` restores the routine registry) when there
            // is one. Without it two sibling `gather { sub foo {...} }` blocks collided
            // with X::Redeclaration, and the first block's `foo` stayed callable outside.
            let compiler = Compiler::new();
            let scoped_body: Vec<Stmt>;
            let compile_target: &[Stmt] = if Compiler::stmts_declare_routines(body) {
                scoped_body = vec![Stmt::Block(body.clone())];
                &scoped_body
            } else {
                body
            };
            let (compiled_code, compiled_fns) = compiler.compile(compile_target);
            let list = LazyList {
                body: body.clone(),
                env,
                cache: std::sync::Mutex::new(None),
                generation_state: std::sync::Mutex::new(None),
                compiled_code: Some(std::sync::Arc::new(compiled_code)),
                compiled_fns: Some(std::sync::Arc::new(compiled_fns)),
                elems_count: None,
                scan_spec: None,
                sequence_spec: None,
                coroutine: Some(std::sync::Mutex::new(crate::value::GatherCoroutineState {
                    ip: 0,
                    locals: Vec::new(),
                    stack: Vec::new(),
                    env: crate::env::Env::new(),
                    finished: false,
                    started: false,
                    for_loop_resume: None,
                    state_scope_id: crate::value::next_instance_id(),
                })),
                lazy_pipe: None,
                closure_seq: None,
                walk_pending: None,
                cat_pull: None,
                array_context: false,
                list_context: false,
                cached_no_sink: false,
                itemized: false,
            };
            let val = Value::lazy_list(crate::gc::Gc::new(list));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeGather expects Block"))
        }
    }

    pub(super) fn resolve_closure_code(
        code: &CompiledCode,
        cc_idx: Option<u32>,
    ) -> Option<std::sync::Arc<CompiledCode>> {
        cc_idx.map(|i| code.closure_compiled_codes[i as usize].clone())
    }

    /// Resolve the creating frame's local slot for a captured free var / upvalue
    /// (`parent_slots[i]` parallel to the sym list, baked at the closure's emit
    /// point by `Compiler::add_closure_code_baked`). With `MUTSU_SHADOW_SLOTS`
    /// active the baked slot wins — a name can occupy several creator slots and
    /// the `rposition` name search always picks the innermost shadow, which is
    /// wrong for a closure created outside that shadow's block. Gated: with the
    /// gate off (default / CI) this is byte-identical to the pre-campaign
    /// `rposition` search. The baked slot is validated against the slot's name
    /// (stale/hand-built chunks fall back to the search). §1.3 closure-capture
    /// slot bake.
    fn resolve_capture_slot(
        code: &CompiledCode,
        parent_slots: &[Option<u32>],
        i: usize,
        sym: crate::symbol::Symbol,
    ) -> Option<usize> {
        if crate::compiler::shadow_slots_active()
            && let Some(baked) = parent_slots.get(i).copied().flatten()
            && let Some(name) = code.locals.get(baked as usize)
            && sym.with_str(|s| name == s)
        {
            return Some(baked as usize);
        }
        sym.with_str(|s| code.locals.iter().rposition(|n| n == s))
    }

    /// Overwrite `env`'s entry for each free variable of `callee` that the
    /// CURRENTLY RUNNING frame (`code`) owns as a local slot, with the live
    /// slot value.
    ///
    /// This is the by-name half of [`Self::capture_closure_env`]'s "upvalue
    /// read" step, factored out for the one env-capture site that cannot use
    /// that function: a *named* sub returned as the trailing value of its
    /// declaring routine (`sub outer { my $a = 2; sub inner {...} }`). That path
    /// flattens the frame's env with `clone_env()`, but a `my` in a routine body
    /// lives in the frame's local SLOT and is not necessarily mirrored into
    /// `env` — so the flattened snapshot either lacks the name entirely or, when
    /// an enclosing scope declared the same name, still holds the OUTER
    /// binding's value. Either way the escaping Sub then resolved the name
    /// against its caller instead of its declaration scope: lexical scoping
    /// degrading into dynamic scoping.
    /// Returns the names it installed, so the caller can vouch for them as
    /// `SubData::authoritative_captures`: the declaring frame is gone by the
    /// time such a Sub is called, so its snapshot can never be stale, and the
    /// call-time merge must install it with OVERWRITE rather than losing to a
    /// same-named lexical in whatever frame invokes it.
    pub(crate) fn inject_frame_locals_for_free_vars(
        &self,
        code: &CompiledCode,
        callee: &CompiledCode,
        env: &mut Env,
    ) -> Vec<crate::symbol::Symbol> {
        let mut installed = Vec::new();
        for (i, sym) in callee.free_var_syms.iter().enumerate() {
            // Dynamics (`$*x`), the topic, attribute twigils and `__mutsu_*`
            // metadata resolve through their own stores against the LIVE frame
            // by design — never freeze one into a lexical snapshot.
            if !sym.with_str(crate::env::is_plain_user_lexical) {
                continue;
            }
            if let Some(slot) =
                Self::resolve_capture_slot(code, &callee.free_var_parent_slots, i, *sym)
                && let Some(val) = self.locals.get(slot)
            {
                env.insert_sym(*sym, val.clone());
                installed.push(*sym);
            }
        }
        installed
    }

    pub(super) fn exec_make_anon_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_block: bool,
    ) -> Result<(), RuntimeError> {
        // See `closures_created` doc comment: a routine-registry restore gate
        // consults this to detect a closure literal escaping via a side
        // channel (not just the return value).
        self.closures_created += 1;
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let params = crate::ast::collect_placeholders_shallow(body);
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let authoritative_captures = self.compute_authoritative_captures(&compiled_code);
            let mut upvalues = self.capture_upvalues(code, &compiled_code);
            let mut captured_env = self.capture_closure_env(code, &compiled_code);
            self.freeze_readonly_owned_captures(
                code,
                &compiled_code,
                &owned_captures,
                &mut captured_env,
                &mut upvalues,
            );
            // A bare block never declares a return type of its own, so a
            // lexically-inherited `__mutsu_return_type` (from the routine or
            // pointy block it is written inside) must not be enforced on the
            // block's own result — the same guard the `MakeLambda` /
            // `MakeAnonSubParams` arms already apply. Without it, a block
            // argument written inside e.g. `-> $x --> Pair { (@k.map({ … })
            // .join: $sep) => $x }` failed the *outer* `Pair` check on its own
            // inner value.
            captured_env.remove("__mutsu_return_type");
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let compiled_fns = compiled_code
                .as_ref()
                .and_then(|cc| cc.compiled_fns.clone());
            let val = Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
                package: Symbol::intern(&self.lexical_closure_package()),
                name: Symbol::intern(""),
                params,
                param_defs: Vec::new(),
                body: body.clone(),
                is_rw: false,
                is_raw: false,
                // Upvalue snapshot (single-store Slice E): capture only free vars,
                // shadow-meta, and system names; see `capture_closure_env`.
                env: captured_env,
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                is_bare_block: is_block,
                owned_captures,
                authoritative_captures,
                upvalues,
                compiled_code,
                compiled_fns,
                compiled_routine: None,
                is_decl_expr_thunk: false,
                deprecated_message: None,
                source_line: cc_source_line,
                // Not `current_source_file()`: that reads the dynamically-scoped
                // `?FILE` env var, which only tracks the unit currently being
                // *loaded* (see `run_modules.rs`) — correct for a closure built
                // while its module loads, but wrong for one built later, each
                // time an already-loaded module's routine runs and constructs
                // this literal afresh (`?FILE` has reverted to the caller's own
                // file by then). `executing_source_file()` instead reads the
                // file baked onto the innermost enclosing routine frame's own
                // `def_file`, which stays correct regardless of who is calling.
                source_file: self.executing_source_file(),
                captured_fatal_mode: self.fatal_mode,
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeAnonSub expects Block"))
        }
    }

    pub(super) fn exec_make_anon_sub_params_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_whatever_code: bool,
    ) -> Result<(), RuntimeError> {
        // See `closures_created` doc comment.
        self.closures_created += 1;
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            name,
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            ..
        } = stmt
        {
            self.check_param_custom_traits(param_defs)?;
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let authoritative_captures = self.compute_authoritative_captures(&compiled_code);
            let mut upvalues = self.capture_upvalues(code, &compiled_code);
            // Upvalue snapshot (single-store Slice E); see `capture_closure_env`.
            let mut env = self.capture_closure_env(code, &compiled_code);
            self.freeze_readonly_owned_captures(
                code,
                &compiled_code,
                &owned_captures,
                &mut env,
                &mut upvalues,
            );
            // See the note in `vm_register_sub_ops`: a lexically-inherited
            // `__mutsu_return_type` would be enforced on this closure's return.
            env.remove("__mutsu_return_type");
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if is_whatever_code {
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("WhateverCode"),
                );
            }
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let compiled_fns = compiled_code
                .as_ref()
                .and_then(|cc| cc.compiled_fns.clone());
            let val = Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
                package: Symbol::intern(&self.lexical_closure_package()),
                // Anonymous closures pool a SubDecl with an empty name; a
                // named `anon sub NAME` decl carries its name through here.
                name: *name,
                params: params.clone(),
                param_defs: param_defs.clone(),
                body: body.clone(),
                is_rw: *is_rw,
                is_raw: *is_raw,
                env,
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: params.is_empty() && param_defs.is_empty(),
                // A pointy block (`-> $x {...}`) is a `Block`, not a `Sub`. Named
                // anonymous subs (`sub {...}`) have `is_pointy_block == false` and
                // stay `Sub`. (`WhateverCode` already overrides via callable_type.)
                is_bare_block: compiled_code.as_ref().is_some_and(|cc| cc.is_pointy_block),
                owned_captures,
                authoritative_captures,
                upvalues,
                captured_fatal_mode: self.fatal_mode,
                compiled_code,
                compiled_fns,
                compiled_routine: None,
                is_decl_expr_thunk: false,
                deprecated_message: None,
                source_line: cc_source_line,
                // See the comment on the equivalent `MakeAnonSub` arm above:
                // `executing_source_file()` (not `current_source_file()`)
                // keeps this correct for a closure literal that is (re)built
                // each time an already-loaded module's routine runs, after
                // the module's own `?FILE` scope has long since reverted.
                source_file: self.executing_source_file(),
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeAnonSubParams expects SubDecl"))
        }
    }

    /// Free variables of a closure being created that were declared in an
    /// enclosing loop body (see `Interpreter::loop_local_vars`). These become the
    /// closure's `owned_captures`: read at call time from its own frozen captured
    /// env so each loop iteration's closure sees its own value (Raku
    /// per-iteration binding), immune to the dual-store slot re-injection.
    /// Value-freeze a *read-only* `:=`-bound loop capture in a just-built closure
    /// env. `my $in := @a[$i]` makes `$in` a `ContainerRef` aliasing an element;
    /// the loop re-points the same lexical each iteration, so a closure that only
    /// READS `$in` (an `owned_capture` not in `captured_mutated_locals`) would
    /// otherwise freeze the shared/re-pointed cell and every iteration's closure
    /// resolves to the loop's final binding (roast S17-lowlevel/lock.t cue tests,
    /// `$out = $in * 10`). A *mutated* capture is boxed into a genuinely-shared
    /// cell by `box_captured_lexicals` and must keep its live `ContainerRef`, so
    /// only the read-only ones are snapshotted to a plain value.
    pub(super) fn freeze_readonly_owned_captures(
        &self,
        code: &CompiledCode,
        cc: &Option<std::sync::Arc<CompiledCode>>,
        owned_captures: &[Symbol],
        env: &mut Env,
        upvalues: &mut [Option<Value>],
    ) {
        let cc_upvalue_syms: &[Symbol] = cc
            .as_ref()
            .map(|c| c.upvalue_syms.as_slice())
            .unwrap_or(&[]);
        for sym in owned_captures {
            // A *mutated* capture is boxed into a genuinely-shared cell by
            // `box_captured_lexicals` and must keep its live `ContainerRef`.
            if code.captured_mutated_locals.contains(sym) {
                continue;
            }
            if !matches!(
                env.get_sym(*sym).map(Value::view),
                Some(ValueView::ContainerRef(_))
            ) {
                continue;
            }
            // Deep-deref: the per-iteration binding can nest (`$in`'s own cell
            // wrapping the element cell, plus prior iterations' wrappers).
            let mut v = env.get_sym(*sym).cloned().unwrap();
            let mut guard = 0;
            while let ValueView::ContainerRef(_) = v.view() {
                v = v.into_deref();
                guard += 1;
                if guard > 64 {
                    break;
                }
            }
            env.insert_sym(*sym, v.clone());
            // The read path resolves a free var from the upvalue snapshot (Slice
            // E), captured from the creating frame's slot — which holds the same
            // shared `ContainerRef`. Freeze that entry to the snapshot too.
            if let Some(uv_idx) = cc_upvalue_syms.iter().position(|s| s == sym)
                && let Some(slot) = upvalues.get_mut(uv_idx)
            {
                *slot = Some(v);
            }
        }
    }

    pub(super) fn compute_owned_captures(
        &self,
        compiled_code: &Option<std::sync::Arc<CompiledCode>>,
    ) -> Vec<Symbol> {
        if self.loop_local_vars.is_empty() && self.frame_owned.is_empty() {
            return Vec::new();
        }
        let Some(cc) = compiled_code else {
            return Vec::new();
        };
        // Per-iteration loop captures (Raku fresh-binding): a free var declared in
        // an enclosing loop body froze a distinct value per iteration.
        cc.free_var_syms
            .iter()
            .filter(|sym| {
                self.loop_local_vars.iter().any(|set| set.contains(*sym))
                    // ADR-0027: cascade an inherited loop-frozen vouch from the
                    // creating frame (a closure created one closure-CALL deep
                    // from the loop body, e.g. an IIFE factory's returned
                    // block) — but ONLY when the name's currently captured
                    // value is plain, never when it is a `ContainerRef`. A
                    // cell is a live shared binding (already force-installed
                    // by the unconditional cell-overwrite capture-env merge);
                    // re-freezing it here would reintroduce the
                    // `roast/S17-lowlevel/lock.t` stale-snapshot hazard that
                    // `frame_authoritative` deliberately excludes
                    // `owned_captures` from.
                    || (self.frame_owned.contains(sym)
                        && !matches!(
                            self.env().get_sym(**sym).map(Value::view),
                            Some(ValueView::ContainerRef(_))
                        ))
            })
            .copied()
            .collect()
    }

    /// Free vars this closure captures that the CREATING frame vouches for
    /// (`frame_authoritative`) — a never-written, lexically-authoritative value.
    /// Stored in `SubData::authoritative_captures`, installed with overwrite at
    /// call time, and re-seeded into the callee's `frame_authoritative` so the
    /// vouch cascades to deeper closures. This is the runtime counterpart of the
    /// compile-time `propagate_authoritative_down`, which does not reach a closure
    /// the inline `.map`/`.grep` fast path re-compiles (its runtime CompiledCode is
    /// a distinct copy from the one the compile-time pass mutates). Kept separate
    /// from loop `owned_captures`, which may be concurrently-mutated shared cells
    /// and must NOT be propagated as authoritative (a reader thread would freeze a
    /// stale snapshot — `roast/S17-lowlevel/lock.t`'s condition-variable busy-wait).
    pub(super) fn compute_authoritative_captures(
        &self,
        compiled_code: &Option<std::sync::Arc<CompiledCode>>,
    ) -> Vec<Symbol> {
        if self.frame_authoritative.is_empty() {
            return Vec::new();
        }
        let Some(cc) = compiled_code else {
            return Vec::new();
        };
        cc.free_var_syms
            .iter()
            .filter(|sym| self.frame_authoritative.contains(sym))
            .copied()
            .collect()
    }

    /// Capture the closure's environment as an *upvalue snapshot* (single-store
    /// Slice E): instead of flattening the whole lexical env into the closure
    /// (`clone_env`), capture only the names the closure body (and its nested
    /// closures) can actually observe.
    ///
    /// The invariant that makes this safe: a closure body references an outer
    /// **user lexical** only through a `GetGlobal`-family opcode, so the compiler's
    /// `free_var_syms` set already lists every such name. The *other* names a body
    /// can read — `self` (attribute access), special vars (`$_`, `$/`, `$!`,
    /// `$?FILE`, …), dynamic vars (`$*…`), match captures, `&?ROUTINE`/`&?BLOCK`,
    /// and type names — go through dedicated opcodes the free-var scan cannot see,
    /// but they are all *system* names rather than plain user lexicals. So the
    /// capture keeps: free variables, the `__mutsu_*` shadow-meta, and every name
    /// that is not a plain user lexical ([`crate::env::is_plain_user_lexical`]). It
    /// drops only the bulk non-free plain user lexicals, which the body provably
    /// cannot reference.
    ///
    /// Reflective programs (`EVAL` / `CALLER::` / symbolic deref) keep the
    /// whole-env snapshot because they can read a caller lexical under any name.
    /// Inline/stored-body consumers no longer widen closure capture: their exact
    /// dependencies are represented by `free_var_syms` and the per-consumer slot
    /// sets from ADR-0018.
    ///
    /// **Slice E Part 2 (the upvalue read):** a free variable that is one of *this*
    /// frame's own locals is read straight from the slot store
    /// (`self.locals[slot]`, the live upvalue), not from `env`. This is what lets
    /// `compute_needs_env_sync` drop its closure-driven flush (branch #2): the
    /// closure no longer depends on the parent frame mirroring that local into
    /// `env` before capture. Ancestor free variables (no slot here) and the system
    /// names still come from the flattened env, where they always live. Mutation
    /// *propagation* back to the parent is unchanged — it flows through the reverse
    /// `env_dirty` path and, for captured-and-mutated locals, the shared
    /// `ContainerRef` cell that `box_captured_lexicals` installs in both the slot
    /// and `env`.
    pub(super) fn capture_closure_env(
        &self,
        code: &CompiledCode,
        cc: &Option<std::sync::Arc<CompiledCode>>,
    ) -> Env {
        let Some(cc) = cc else {
            return self.clone_env();
        };
        if crate::opcode::reflective_name_access_possible() {
            let mut flat = self.clone_env();
            // Even when capturing the whole env by name, a slot-only local (a
            // pointy-block/sub parameter that this frame never mirrors into `env`,
            // e.g. `-> $r { * ~~ /<$r>/ }` where `$r` is read only inside a stored
            // regex) can be missing from the cloned env. Pull this frame's
            // free-var slots from the live local store so such captures survive.
            for (i, sym) in cc.free_var_syms.iter().enumerate() {
                if let Some(slot) =
                    Self::resolve_capture_slot(code, &cc.free_var_parent_slots, i, *sym)
                    && let Some(val) = self.locals.get(slot)
                {
                    flat.insert_sym(*sym, val.clone());
                }
            }
            // ADR-0024 §4: a closure created while a mainline named sub's frame
            // is running must capture the SAME cells the sub itself resolves
            // free variables through, overriding whatever the (possibly
            // shadowed) env/slot capture above just wrote.
            self.inject_mainline_lexical_captures(cc, &mut flat);
            // `$OUTER::x` inside this closure reads the *enclosing* binding of `x`,
            // which is captured into `flat` right now. But when the closure runs,
            // its own frame may overwrite that name in the live env (most commonly
            // the topic `$_`: a bare `sub {...}` establishes a fresh `$_ = Any`).
            // OUTER:: is lexical, so snapshot the captured enclosing value under a
            // reserved key the running frame never touches; `get_outer_var` reads
            // it back regardless of any later same-name overwrite.
            for name in &cc.outer_ref_names {
                if let Some(val) = flat.get(name).cloned() {
                    flat.insert(format!("__mutsu_outer::{name}"), val);
                }
            }
            // Drop the closure's own params/locals (e.g. a WhateverCode's `_`
            // topic param) so a stale enclosing `for`/map topic is not inherited
            // and later leaked back to the caller.
            for name in &cc.locals {
                if !cc.free_var_syms.iter().any(|s| s.with_str(|x| x == name)) {
                    flat.remove_sym(Symbol::intern(name));
                }
            }
            // `__mutsu_callable_type` is closure-IDENTITY metadata (e.g. the
            // WhateverCode marker), set on the genuine closure's own env AFTER
            // capture (see the `is_whatever_code` insert in the caller). It must
            // never be inherited: an ordinary inner block created inside a
            // WhateverCode body would otherwise capture the marker and be
            // mis-treated as a WhateverCode itself — e.g. the `.map` loop would
            // then hold `$_` at the outer topic instead of binding it to the
            // element (`*.map({ $_ })` saw the whole list, not each item).
            flat.remove_sym(Symbol::intern("__mutsu_callable_type"));
            // Attribute-twigil keys are per-frame materializations of `self`'s
            // attributes — never snapshot them (see the filtered branch below).
            flat.retain(|k, _| !k.with_str(Self::is_attr_twigil_env_key));
            self.capture_bare_callees(cc, &mut flat);
            self.materialize_frame_self_into_capture(code, &mut flat);
            return flat;
        }
        let free: std::collections::HashSet<Symbol> = cc.free_var_syms.iter().copied().collect();
        // The closure's own parameters/locals (e.g. a WhateverCode's `_` param)
        // shadow any same-named enclosing binding, so they must NOT be inherited
        // from the creating frame's env. Capturing the enclosing `_` (a `for`/map
        // topic) into a `_`-param WhateverCode would leak that stale topic back to
        // the caller on return (`* ~~ /<$r>/` invoked inside a grep-in-`for`).
        let own_locals: std::collections::HashSet<&str> =
            cc.locals.iter().map(|s| s.as_str()).collect();
        // Keep only the upvalue set, shadow-meta, and system names, walking the
        // env tiers directly (`filtered_flat`) — flattening first (`clone_env`)
        // deep-cloned the entire parent-chain map per lambda creation. The
        // filter is key-pure, so the tier walk's shadow/tombstone handling is
        // exactly the flattened view. `__mutsu_callable_type` is
        // closure-identity metadata (the WhateverCode marker), (re)installed on
        // the genuine closure's own env after capture — never inherit it, or an
        // ordinary inner block would be mis-detected as a WhateverCode (see the
        // by-name path above).
        let mut env = self.env().filtered_flat(&|k, _v| {
            if k.with_str(|s| s == "__mutsu_callable_type") {
                return false;
            }
            // Attribute-twigil keys (`!x`, `@!x`, `%.x`, …) are per-frame
            // materializations of `self`'s attributes, not lexicals: the
            // closure must read them through its captured `self` at RUN time.
            // A creation-time snapshot goes stale the moment the instance
            // mutates — a `start` block reading `@!before` inside
            // Cro::CompositeConnector.connect saw an empty pre-mutation copy.
            if k.with_str(Self::is_attr_twigil_env_key) {
                return false;
            }
            free.contains(&k)
                || k.with_str(|s| !crate::env::is_plain_user_lexical(s) && !own_locals.contains(s))
        });
        // Upvalue read: override this frame's own free-var slots with the live
        // local value. Authoritative even after the closure-driven env flush is
        // gone (a slot-only local is no longer mirrored into `env`).
        for (i, sym) in cc.free_var_syms.iter().enumerate() {
            if let Some(slot) = Self::resolve_capture_slot(code, &cc.free_var_parent_slots, i, *sym)
                && let Some(val) = self.locals.get(slot)
            {
                env.insert_sym(*sym, val.clone());
            }
        }
        // ADR-0024 §4: see the identical override in the reflective path above.
        self.inject_mainline_lexical_captures(cc, &mut env);
        // A bare call records only its sigilless callee in bytecode, so it is
        // not normally part of `free_var_syms`. Preserve an existing lexical
        // code binding for each callee the closure (or a nested closure it may
        // create later) actually references. This is the escape gate for an
        // imported sub installed by `use` inside EVAL: PopImportScope may remove
        // the registry alias, while the escaping closure still owns `&name`.
        self.capture_bare_callees(cc, &mut env);
        self.materialize_frame_self_into_capture(code, &mut env);
        env
    }

    /// ADR-0024 §4: while [`Self::mainline_lexical_frame_active`] holds (a
    /// mainline named sub's frame is running), override each of `cc`'s free
    /// variables that has a `unit_lexicals[MAINLINE_UNIT_KEY]` entry with its
    /// shared cell, in place of whatever the ordinary env/local-slot capture
    /// wrote for it. Without this, a closure created inside the sub (e.g.
    /// `.map({ $y })`) would capture whatever the CALLING frame's env holds
    /// under that name — the shadow, if the sub was called from inside a
    /// shadowing block — instead of the sub's own true lexical binding
    /// (ADR-0024 row 3). A closure created inside a plain (non-mainline)
    /// frame is unaffected: the predicate is false there, so this is a no-op.
    fn inject_mainline_lexical_captures(&self, cc: &CompiledCode, env: &mut Env) {
        if !self.mainline_lexical_frame_active() {
            return;
        }
        let Some(mainline) = self.unit_lexicals.get(crate::runtime::MAINLINE_UNIT_KEY) else {
            return;
        };
        for sym in &cc.free_var_syms {
            if let Some(cell) = sym.with_str(|s| mainline.get(s).cloned()) {
                env.insert_sym(*sym, cell);
            }
        }
    }

    fn capture_bare_callees(&self, cc: &CompiledCode, env: &mut Env) {
        // Import aliases only need this escape gate for re-entrant source EVAL:
        // ordinary module/package execution retains its lexical registry state
        // through the existing module-scope machinery.
        if self.env().get("__mutsu_in_eval").is_none() {
            return;
        }
        for name in cc.bare_callee_names() {
            let resolved_name = name.resolve();
            if self.has_proto(&resolved_name) || self.has_multi_candidates(&resolved_name) {
                continue;
            }
            let Some(def) = self
                .resolve_function(&resolved_name)
                .map(|def| (*def).clone())
            else {
                continue;
            };
            // Same-package routines continue to use normal compiled/registry
            // dispatch. A different defining package means this bare name is a
            // lexical import alias; pinning just those aliases avoids changing
            // ordinary closure dispatch throughout the program.
            if def
                .package
                .with_str(|package| package == self.current_package())
            {
                continue;
            }
            let code_name = name.with_str(|name| format!("&{name}"));
            let code_sym = Symbol::intern(&code_name);
            if !env.contains_key_sym(code_sym)
                && let Some(value) = self.env().get_sym(code_sym)
            {
                env.insert_sym(code_sym, value.clone());
            } else if !env.contains_key_sym(code_sym) {
                env.insert_sym(code_sym, self.sub_value_from_function_def(def));
            }
        }
    }

    /// Attribute-twigil env keys (`!x`, `@!x`, `%.x`, …): per-frame
    /// materializations of `self`'s attributes that a closure capture must NOT
    /// snapshot (see the capture filter).
    fn is_attr_twigil_env_key(s: &str) -> bool {
        let bare = match s.as_bytes().first() {
            Some(b'@' | b'%' | b'&' | b'$') => &s[1..],
            _ => s,
        };
        let b = bare.as_bytes();
        matches!(b.first(), Some(b'!') | Some(b'.')) && b.len() > 1 && b[1].is_ascii_alphabetic()
    }

    /// `self` is lexical: a closure created inside a method body must capture
    /// that method's invocant. On the fast method path (skip_env_setup) `self`
    /// lives ONLY in a local slot, so the env-based capture above misses it and
    /// a later `$.attr`/`$!attr` in the closure body resolved against whatever
    /// `self` the *invoking* frame happened to carry — a supply block created
    /// in `Sink.sinker` and tapped from another object's method read `$!sum`
    /// off that other object (Cro::Service.start's assembled pipeline).
    fn materialize_frame_self_into_capture(&self, code: &CompiledCode, env: &mut Env) {
        if env.get("self").is_none()
            && let Some(slot) = code.locals.iter().position(|n| n == "self")
            && let Some(val) = self.locals.get(slot)
            && !val.is_nil()
        {
            env.insert("self".to_string(), val.clone());
        }
    }

    /// Build the closure's upvalue array (aligned with `cc.upvalue_syms`) from the
    /// creating frame's current bindings. Each entry resolves from the creating
    /// frame's own local slot first (authoritative in the single-store model —
    /// after `box_captured_lexicals` a mutated/escaping lexical's slot holds the
    /// shared `ContainerRef` cell, so the snapshot clones the live cell), then the
    /// enclosing env (transitive captures / outer-frame lexicals). A read-only
    /// scalar that is never mutated resolves to its plain value. `GetUpvalue` later
    /// dereferences a `ContainerRef`, so a boxed capture stays coherent with the
    /// creator.
    pub(super) fn capture_upvalues(
        &self,
        code: &CompiledCode,
        cc: &Option<std::sync::Arc<CompiledCode>>,
    ) -> Vec<Option<Value>> {
        let Some(cc) = cc else {
            return Vec::new();
        };
        if cc.upvalue_syms.is_empty() {
            return Vec::new();
        }
        cc.upvalue_syms
            .iter()
            .enumerate()
            .map(|(i, sym)| {
                // Resolve the creating frame's current binding: own local slot
                // first (authoritative in the single-store model), then env.
                let resolved = if let Some(slot) =
                    Self::resolve_capture_slot(code, &cc.upvalue_parent_slots, i, *sym)
                    && let Some(val) = self.locals.get(slot)
                {
                    val.clone()
                } else {
                    self.env().get_sym(*sym).cloned().unwrap_or(Value::NIL)
                };
                // Freeze ONLY a shared `ContainerRef` cell into the upvalue array:
                // reading it always tracks the creator's container, so it is
                // unconditionally correct (and skips an env HashMap lookup). A
                // non-cell value is NOT frozen (`None`) -> `GetUpvalue` reads it
                // live from env, exactly preserving the env-capture behavior.
                //
                // We deliberately do NOT snapshot a non-cell "constant" capture by
                // value: mutsu's compile-time mutation analysis is incomplete (it
                // does not see writes from separately-registered role/class methods
                // or rw-arg sinks like `cas`), so a value that merely *looks*
                // read-only can in fact be mutated by another scope/thread
                // (S12-construction/roles-6e.t). Promoting such constants to
                // by-value snapshots requires a complete mutation analysis and is
                // deferred to a later phase.
                resolved.is_container_ref().then_some(resolved)
            })
            .collect()
    }

    /// Whether the scalar env-key name `s` carries a type/`where` constraint
    /// that must NOT be boxed into a `ContainerRef`: the assignment chokepoint
    /// re-checks such a constraint BY NAME on every mutation, and a
    /// `ContainerRef` write-through bypasses that check. `Mu` (the universal
    /// type — every value satisfies it) and the native/builtin scalar value
    /// types (`int`/`num`/`str` families, `Int`/`UInt`/`Num`/`Str`/`Rat` with
    /// or without a `:D`/`:U` smiley) ARE boxable: their check also runs at
    /// the assignment op by name, so the write-through bypasses nothing extra
    /// for them (see the historical `cas`/`thread_escaping` detail in
    /// [`Self::box_captured_lexicals`], which does not apply to this shared
    /// predicate). Shared between closure-capture boxing
    /// (`box_captured_lexicals`) and mainline `my` capture at named-sub
    /// registration (ADR-0024, `exec_register_sub_op`).
    pub(super) fn type_constrained_unboxable(&mut self, s: &str) -> bool {
        let mut tc = loan_env!(self, var_type_constraint(s));
        if tc.is_none() {
            tc = loan_env!(self, var_type_constraint(s.trim_start_matches('$')));
        }
        let value_type_boxable = tc.as_deref().is_some_and(|t| {
            crate::runtime::native_types::is_native_int_type(t)
                || matches!(t, "num" | "num32" | "num64" | "str")
                || matches!(
                    crate::runtime::types::strip_type_smiley(t).0,
                    "Int" | "UInt" | "Num" | "Str" | "Rat"
                )
        });
        !value_type_boxable && matches!(tc.as_deref(), Some(t) if t != "Mu")
    }

    /// Box-on-capture (lever C Slice 2): a closure captures the *container* of a
    /// closed-over lexical scalar, not a frozen value — but only for the lexicals
    /// that actually need it: an enclosing-scope local that is BOTH captured by a
    /// closure AND mutated after declaration (`code.captured_mutated_locals`,
    /// computed by the compiler). Before snapshotting the env into the new
    /// closure's `data.env`, replace such a free variable (which has a slot in
    /// `code.locals`) with a shared `ContainerRef` in BOTH the slot and the env.
    /// The env snapshot then shares the same `Arc`, so:
    ///
    /// - mutation of the lexical *after* capture is visible to the closure
    ///   (`my $x=1; my $c={$x}; $x=2; $c()` -> 2, in or out of a loop), and
    /// - sibling closures share one cell
    ///   (`my $v=0; my $g={$v}; my $s=->$n{$v=$n}; $s(42); $g()` -> 42).
    ///
    /// Per-iteration freshness is preserved because a loop-body `my` redeclaration
    /// resets the stale ContainerRef in the slot+env each iteration (see
    /// exec_set_local_op vardecl handling), so the next closure boxes a fresh
    /// cell. Read-only / declaration-only captures are deliberately NOT boxed:
    /// they don't need container identity, and boxing them (e.g. Test's
    /// `lives-ok {...}` closing over a surrounding `$obj` / type object / Mix)
    /// would hide the value behind a ContainerRef and trip the many code paths
    /// that don't yet deref one (immutability, type-object dispatch, `.kv` rw
    /// writeback). Arrays / hashes / subs / type objects are reference-shared
    /// already and untouched.
    pub(super) fn box_captured_lexicals(
        &mut self,
        code: &CompiledCode,
        cc: &Option<std::sync::Arc<CompiledCode>>,
    ) {
        let Some(cc) = cc else { return };
        // Box captured-and-mutated `$` scalar locals into a shared `ContainerRef`
        // cell so the closure observes mutations and siblings share one cell. Two
        // narrow triggers (deliberately NOT "every captured-mutated local" — that
        // broad form regressed perf and correctness, see #2749 / docs):
        //   (A) loop-body locals (`loop_local_vars`): per-iteration binding, the
        //       original lever-C path — kept byte-for-byte.
        //   (B) `needs_cell_locals`: locals captured by a child closure whose
        //       value ESCAPES the creating frame (escape analysis — stored,
        //       returned, or bound, not immediately invoked). These genuinely
        //       need a shared cell even in non-loop frames (e.g. a getter+setter
        //       factory, or a single `&f = sub {...}` assigned closure). The
        //       escape signal excludes the immediately-invoked closure
        //       (`lives-ok {...}` / `map {...}`, call args / control blocks),
        //       bounding boxing cost and avoiding the broad-boxing perf blowup.
        // Read-only loop captures are handled by `owned_captures` (value-freeze).
        // §1.3 (shadow slots only): a captured-and-mutated local whose name
        // occupies MORE THAN ONE slot (a genuine inner-block shadow under
        // `MUTSU_SHADOW_SLOTS`) must get a cell even when the closure does not
        // escape: the non-cell coherence path writes the mutation back BY NAME
        // (position = the outer slot), so a non-escaping closure over the inner
        // shadow would update the wrong slot (S06-advanced/wrap.t). With the
        // gate off `alloc_local` get-or-creates by name, so duplicates are
        // structurally impossible and this is byte-identical.
        let dup_shadow_possible =
            crate::compiler::shadow_slots_active() && code.dup_named_locals.iter().any(|d| *d);
        if code.captured_mutated_locals.is_empty()
            || (self.loop_local_vars.is_empty()
                && code.needs_cell_locals.is_empty()
                && !dup_shadow_possible)
        {
            return;
        }
        for (fv_i, sym) in cc.free_var_syms.iter().enumerate() {
            if !code.captured_mutated_locals.contains(sym) {
                continue;
            }
            let needs_cell = code.needs_cell_locals.contains(sym);
            // Resolve to an owned String instead of `with_str`: `with_str` holds
            // the global symbol table's READ lock across its closure, and the
            // checks below (`var_type_constraint`, env access) can intern a NEW
            // string — a same-thread read→write reacquire of the RwLock, which
            // deadlocks. (Surfaced by the MakeGather boxing path; the closure
            // creation ops share this code, so keep it lock-free for all.)
            let s = sym.resolve();
            if s.starts_with('@') || s.starts_with('%') || s.starts_with('&') {
                continue;
            }
            let is_loop_local = self.loop_local_vars.iter().any(|set| set.contains(sym));
            // Emit-point slot (§1.3 slot bake, gated): the creator slot this
            // closure actually captures. Falls back to the rposition name
            // search (byte-identical with the gate off / for hand-built cc).
            let baked_idx = Self::resolve_capture_slot(code, &cc.free_var_parent_slots, fv_i, *sym);
            // Shadow trigger (C): the captured slot is one of several
            // same-named slots — by-name writeback cannot disambiguate, so a
            // cell is required regardless of the escape analysis (see the
            // `dup_shadow_possible` gate above).
            let is_dup_shadow = dup_shadow_possible
                && baked_idx
                    .is_some_and(|b| code.dup_named_locals.get(b).copied().unwrap_or(false));
            if !is_loop_local {
                // Non-loop escaping path (B) / shadow path (C) only.
                if !needs_cell && !is_dup_shadow {
                    continue;
                }
                // HISTORY: a type/`where`-constrained scalar used to be skipped
                // here, because the `ContainerRef` write-through bypassed the
                // constraint re-check at the assignment chokepoint, and because
                // a cell hid `cas`'s by-name target resolution
                // (roast/S17-lowlevel/cas.t does `cas` on a
                // `my LittleNodey $head` captured by a same-frame
                // `throws-like` block). Both halves closed independently:
                // ADR-0042 made the constraint a property of the CONTAINER, so
                // a write reaching the scalar through its cell re-checks it
                // (`my Str $s; my $t := $s; $t = 42` dies correctly), and
                // ADR-0062 anchored the atomic lane to the published value with
                // the root store as the lane's only authority. ADR-0055 slice 1
                // therefore RETIRED the skip: it was the last thing keeping a
                // captured-and-mutated class-typed scalar unboxed, which is
                // exactly the residue an overwrite-install cannot tolerate.
                // The exceptions the skip had accumulated (`Mu`, thread-escaping
                // closures, the native/builtin scalar value types — pins
                // `t/thread-shared-scalar-visibility.t` and `t/nqp-cbor-ops.t`)
                // are subsumed: everything is boxed now.
            }
            let Some(idx) = baked_idx else {
                continue;
            };
            // Already a shared cell -> a sibling closure (or earlier capture)
            // boxed it; reuse the same Arc.
            if self.locals[idx].is_container_ref() {
                continue;
            }
            // The name-keyed legacy atomic lane owns this binding's value right
            // now (an earlier `cas` was refused a cell and parked the value
            // there): promoting here would seed a cell from the stale slot and
            // fork the binding in two. Decline — see `legacy_atomic_lane_owns`.
            if self.legacy_atomic_lane_owns(s.trim_start_matches('$')) {
                continue;
            }
            let cur = &self.locals[idx];
            // Only box plain scalar containers. Reference types share already;
            // type objects / proxies must not be hidden behind a ContainerRef.
            // Seq/HyperSeq/RaceSeq/Slip are Arc-backed the same way Array/Hash
            // are Gc-backed, so the founding rationale ("reference-shared
            // already, left untouched", commit 5cedcfe60) applies to them
            // equally -- they were simply not in the original list (see
            // `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`, resolved
            // by adding them here). Excluding them also removes the specific
            // hazard that ticket documented: a var whose value transitions
            // from a refused shape (Array) to a Seq mid-sequence (e.g. via
            // `flat`) no longer triggers a MID-SEQUENCE promotion here, so it
            // stays on one lane (the general shared_vars reconcile) for its
            // whole lifetime instead of switching mechanisms partway through.
            // EXCEPTION: the Any type object is the uninitialized-scalar seed
            // (PLAN 8.5 step 3) — box it exactly like the old Nil seed, so a
            // captured-then-reassigned lexical stays a shared cell
            // (t/then-captured-lexical-cross-thread.t).
            // ADR-0055 slice 1 (2026-08-28): `Package`, `Array` and `Hash` left
            // this list. A `$`-held Array/Hash and a Package (type object) in a
            // captured-and-mutated scalar were the other two unboxed residues;
            // under closure-wins (slice 2) an unboxed mutated capture is a
            // staleness bug, so the cell is mandatory. Note this boxes the `$`
            // SCALAR container, not the Array/Hash itself — `@a`/`%h` sigil
            // locals still take `box_decl_local_container_cell`.
            if !cur.is_any_type_object()
                && matches!(
                    cur.view(),
                    ValueView::Sub(..)
                        | ValueView::Proxy { .. }
                        | ValueView::Seq(..)
                        | ValueView::HyperSeq(..)
                        | ValueView::RaceSeq(..)
                        | ValueView::Slip(..)
                )
            {
                continue;
            }
            let container = cur.clone().into_container_ref();
            self.locals[idx] = container.clone();
            self.env_mut().insert(s.clone(), container.clone());
            // Track C: if a thread is already running (shared_vars active) and a
            // stale plain snapshot of this name lives in `shared_vars` (seeded
            // by an earlier `start` before this local was boxed), replace it
            // with the cell. Otherwise the stale value, marked dirty, would be
            // written back over the cell by `sync_shared_vars_to_env` after the
            // next await — disconnecting the parent from the shared cell.
            // `set_shared_var` only updates entries that already exist, so this
            // is a no-op when the name was never snapshotted.
            if self.shared_vars_active {
                // The cell now OWNS this binding, which is exactly what the
                // re-declaration mask was standing in for, so the mask must not
                // block the replacement below: leaving the stale plain snapshot
                // in place lets `sync_shared_vars_to_env` write it back over the
                // cell after the next await and disconnect the parent.
                self.thread_redeclared_vars.borrow_mut().remove(&s);
                self.thread_redeclared_vars
                    .borrow_mut()
                    .remove(s.trim_start_matches('$'));
                loan_env!(self, set_shared_var(&s, container.clone()));
            }
        }
    }
}
