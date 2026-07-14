use super::*;
use crate::compiler::Compiler;
use crate::symbol::Symbol;

impl Interpreter {
    /// Get the current source line number from the interpreter.
    pub(super) fn current_source_line(&self) -> Option<u32> {
        Some(self.cur_source_line as u32)
    }

    /// Get the current source file from the interpreter env.
    pub(super) fn current_source_file(&self) -> Option<String> {
        self.env().get("?FILE").and_then(|v| match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        })
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
                })),
                lazy_pipe: None,
                closure_seq: None,
                walk_pending: None,
                cat_pull: None,
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

    pub(super) fn exec_make_anon_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_block: bool,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let params = crate::ast::collect_placeholders_shallow(body);
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let mut upvalues = self.capture_upvalues(code, &compiled_code);
            let mut captured_env = self.capture_closure_env(code, &compiled_code);
            self.freeze_readonly_owned_captures(
                code,
                &compiled_code,
                &owned_captures,
                &mut captured_env,
                &mut upvalues,
            );
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
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
                upvalues,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
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
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            ..
        } = stmt
        {
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
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
            let val = Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(""),
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
                upvalues,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
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
        if self.loop_local_vars.is_empty() {
            return Vec::new();
        }
        let Some(cc) = compiled_code else {
            return Vec::new();
        };
        cc.free_var_syms
            .iter()
            .filter(|sym| self.loop_local_vars.iter().any(|set| set.contains(*sym)))
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
    /// Two cases keep the whole-env snapshot (`clone_env`) because `free_var_syms`
    /// is *not* a complete account of the names the body reads:
    /// - **reflective** programs (`EVAL` / `CALLER::` / symbolic deref) can read a
    ///   caller lexical under any name (process-global flag, set at finalize);
    /// - **`captures_env_by_name`** frames run an inline body that reads lexicals
    ///   by name through a path the op-scan misses (`whenever`/`gather` bodies
    ///   stashed in the `stmt_pool`, loop/block control temps) — see the field on
    ///   [`CompiledCode`].
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
        if cc.captures_env_by_name || crate::opcode::reflective_name_access_possible() {
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
        // Flatten once (same as `clone_env`), then keep only the upvalue set,
        // shadow-meta, and system names. The base tier (GLOBAL_BASE) is never in
        // the overlay and stays reachable through the flat env's tail lookup.
        let flat = self.clone_env();
        let mut map: std::collections::HashMap<Symbol, Value> = std::collections::HashMap::new();
        for (k, v) in flat.iter() {
            // `__mutsu_callable_type` is closure-identity metadata (the
            // WhateverCode marker), (re)installed on the genuine closure's own env
            // after capture. Never inherit it, or an ordinary inner block would be
            // mis-detected as a WhateverCode (see the by-name path above).
            if k.with_str(|s| s == "__mutsu_callable_type") {
                continue;
            }
            let keep = free.contains(k)
                || k.with_str(|s| !crate::env::is_plain_user_lexical(s) && !own_locals.contains(s));
            if keep {
                map.insert(*k, v.clone());
            }
        }
        // Upvalue read: override this frame's own free-var slots with the live
        // local value. Authoritative even after the closure-driven env flush is
        // gone (a slot-only local is no longer mirrored into `env`).
        for (i, sym) in cc.free_var_syms.iter().enumerate() {
            if let Some(slot) = Self::resolve_capture_slot(code, &cc.free_var_parent_slots, i, *sym)
                && let Some(val) = self.locals.get(slot)
            {
                map.insert(*sym, val.clone());
            }
        }
        crate::env::Env::from_symbol_map(map)
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
                // A type/`where`-constrained scalar must keep flowing through
                // the assignment chokepoint so each mutation re-checks the
                // constraint; the ContainerRef write-through bypasses it. Skip
                // boxing it (inline `where` desugars to an anonymous subset, so
                // var_type_constraint catches block/whatever/`&pred` forms).
                // Applied to (B) only — the loop path (A) is left unchanged.
                // EXCEPTION: `Mu` is the universal type — every value satisfies
                // it, so the ContainerRef write-through bypasses no real check.
                // Box `my Mu $s` so captured-outer thunks (metaop `Xxx`/`Zand`)
                // share its cell and stay coherent without the blanket reconcile.
                let mut tc = loan_env!(self, var_type_constraint(&s));
                if tc.is_none() {
                    tc = loan_env!(self, var_type_constraint(s.trim_start_matches('$')));
                }
                if matches!(tc.as_deref(), Some(t) if t != "Mu") {
                    continue;
                }
            }
            let Some(idx) = baked_idx else {
                continue;
            };
            let cur = &self.locals[idx];
            // Already a shared cell -> a sibling closure (or earlier capture)
            // boxed it; reuse the same Arc.
            if cur.is_container_ref() {
                continue;
            }
            // Only box plain scalar containers. Reference types share already;
            // type objects / proxies must not be hidden behind a ContainerRef.
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
                loan_env!(self, set_shared_var(&s, container.clone()));
            }
        }
    }
}
