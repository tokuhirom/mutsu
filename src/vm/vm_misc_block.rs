use super::*;

impl Interpreter {
    pub(crate) fn is_exceptional_block_exit(err: &RuntimeError) -> bool {
        if err.is_fail() {
            return true;
        }
        if err.return_value.is_some() {
            return false;
        }
        !(err.is_last()
            || err.is_next()
            || err.is_redo()
            || err.is_goto()
            || err.is_proceed()
            || err.is_succeed()
            || err.is_leave
            || err.is_resume()
            || err.is_react_done())
    }

    /// KEEP/UNDO (and LEAVE's success/failure queue split) are decided by the
    /// trailing value's DEFINEDNESS, not its truthiness: `{ 0 }` / `{ False }`
    /// / `{ "" }` / `{ () }` all still run KEEP in real Raku (they are
    /// falsy-but-defined), while `{ Any }` / `{ Nil }` / a phaser-only block
    /// with no value statement at all (implicit `Nil`) run UNDO. An
    /// exceptional exit (an exception, `fail`, or an interrupted loop control
    /// flow like `last`/`next`/`redo`, whose `return_value` is `None` and so
    /// reads as undefined `Nil`) also runs UNDO. See
    /// `todo/tickets/keep-undo-decided-by-value-truthiness-not-completion.md`.
    pub(super) fn should_run_success_queue(
        body_result: &Result<(), RuntimeError>,
        current_value: Option<Value>,
    ) -> bool {
        match body_result {
            Ok(()) => runtime::types::value_is_defined(&current_value.unwrap_or(Value::NIL)),
            Err(e) if !Self::is_exceptional_block_exit(e) => {
                runtime::types::value_is_defined(&e.return_value.clone().unwrap_or(Value::NIL))
            }
            Err(_) => false,
        }
    }

    /// Execute the CheckPhaser opcode: pop TOS, throw X::Phaser::PrePost if falsy.
    /// `condition` is the phaser condition's source text (e.g. `0`) when known.
    pub(super) fn exec_check_phaser_op(
        &mut self,
        is_pre: bool,
        condition: Option<String>,
    ) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::NIL);
        if !val.truthy() {
            return Err(crate::runtime::phaser_prepost_error(
                is_pre,
                &condition.unwrap_or_default(),
            ));
        }
        Ok(())
    }

    // Cost: O(1) plus the body; a scope-isolating block (string-interpolation `{...}`)
    // adds O(w + k + s), w = names it wrote by name (its block tier), k = names it
    // declares, s = the chunk's special/internal local slots (saved and reverted);
    // `scope_routines` adds O(R), R = routine-registry entries. Rakudo: O(1) -- see #9170.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_do_block_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        label: &Option<String>,
        scope_isolate: bool,
        isolate_decls_idx: u32,
        scope_routines: bool,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        let label = label.clone();
        let stack_base = self.stack.len();
        let saved_when_matched = self.when_matched();
        let once_scope = self.next_once_scope_id();
        self.push_once_scope(once_scope);
        self.push_enum_scope();
        // A `do { ... }` block is a block, so a routine declared directly in it
        // is lexical to it: it must not leak out, and an outer routine of the
        // same name must come back when the block exits. `OpCode::BlockScope`
        // (the statement-form bare block), every routine call and every
        // for-loop body that declares routines already take this
        // snapshot/restore pair; the value-position `do` form was the one that
        // did not, so `sub foo() {...}` inside it permanently replaced an outer
        // `foo`. The compiler sets `scope_routines` only when the body actually
        // declares a routine, so the common case pays nothing.
        let routine_snapshot = scope_routines.then(|| self.snapshot_routine_registry());
        // A scope-isolating block runs over a block tier (see `vm_block_env`),
        // so its exit reads back only what it wrote by name, and saves just the
        // local slots it may have to revert rather than the whole frame (#9170).
        let saved_env = if scope_isolate {
            let isolate_set = Self::isolate_decl_names(code, isolate_decls_idx);
            let revert_slots: Vec<(usize, Value)> = Self::isolate_revert_slots(code, &isolate_set)
                .into_iter()
                .filter_map(|idx| self.locals.get(idx).map(|v| (idx, v.clone())))
                .collect();
            Some((self.open_block_env_tier(), isolate_set, revert_slots))
        } else {
            None
        };
        // A bare block — labelled or not — is not a loop construct in rakudo:
        // `last`/`next`/`redo` (even `last LAB` naming this block's own label)
        // must NOT be caught here, and entering this block must not raise
        // `loop_handler_depth` either, or `LAB: { next }` would look handled
        // and silently leave the block instead of raising
        // `X::ControlFlow` ("labeled next without loop construct")
        // (`todo/tickets/labelled-bare-block-is-not-a-loop-construct.md`).
        // Only `leave` (a block-exit statement, not a loop-control one) is
        // caught by an enclosing block regardless of label.
        let result = match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => Ok(()),
            Err(e)
                if e.is_leave
                    && e.leave_callable_id().is_none()
                    && e.leave_routine().is_none()
                    && Self::label_matches(&e.label, &label) =>
            {
                self.stack.truncate(stack_base);
                self.stack.push(
                    e.return_value
                        .unwrap_or(Value::slip_arc(std::sync::Arc::new(vec![]))),
                );
                Ok(())
            }
            // A `when`/`default` succeed exits its innermost enclosing
            // block — this one. `do { when Int { "i" } }` yields the matched
            // body's value and execution continues after the `do`; raku does
            // NOT let the succeed travel on to an outer `given`/`with`
            // (DBIish's execute loses its whole bind-setup `given` when the
            // preceding `do { when ... }` escapes it). The `when_matched`
            // flag is reset too — an enclosing `given` breaks its body on it
            // after every op, which would skip the rest of the given.
            Err(e) if e.is_succeed() => {
                self.stack.truncate(stack_base);
                self.stack.push(e.return_value.unwrap_or(Value::NIL));
                loan_env!(self, set_when_matched(saved_when_matched));
                Ok(())
            }
            Err(e) => Err(e),
        };
        self.pop_enum_scope();
        self.pop_once_scope();
        if let Some(routine_snapshot) = routine_snapshot {
            self.restore_routine_registry(routine_snapshot);
        }
        // Restore scope if scope_isolate is true
        if let Some((saved_env, isolate_set, revert_slots)) = saved_env {
            let block_result = self.stack.pop().unwrap_or(Value::NIL);
            // Scope-isolating exit. The block isolates its OWN scalar/array `my`/
            // `state` declarations (reverted to the pre-block value so they don't
            // leak), but a mutation of an OUTER variable must persist —
            // `"{ $x = 1 }"` / `"{ foo() }"` where `foo` writes an outer/`our`
            // var. New hashes still leak (the `:into(my %h := :{})` idiom).
            //   - declared scalar/array name (isolate set) -> revert (skip).
            //   - other plain user var that changed            -> keep (outer mutation).
            //   - new hash                                      -> keep (`:into`).
            //   - internal `__mutsu_*` / specials / dynamics    -> revert.
            // Isolate-set keyed by BARE name (sigil stripped) so it matches an env
            // key regardless of whether that scalar/array is stored with or
            // without its sigil (e.g. a `state $a` whose env mirror would
            // otherwise be re-carried and pollute the next evaluation's init).
            //
            // Only the block's own writes are candidates: every other binding
            // is, by construction, still the entry env's.
            let closed = self.close_block_env_tier(saved_env);
            let mut restored_env = closed.base;
            let mut new_vars: Vec<(Symbol, Value)> = Vec::new();
            for (name, value) in closed.writes.iter() {
                let nm = name.resolve();
                if isolate_set.contains(Self::strip_isolate_sigil(&nm)) {
                    continue;
                }
                let keep = match restored_env.get_sym(*name) {
                    None => nm.starts_with('%') && !nm.starts_with("%*"),
                    Some(saved_val) => Self::is_plain_isolate_user_var(&nm) && value != saved_val,
                };
                if keep {
                    new_vars.push((*name, value.clone()));
                }
            }
            // Re-insert the kept writes
            for (name, value) in new_vars {
                restored_env.insert_sym(name, value);
            }
            *self.env_mut() = restored_env;
            // (B) per-store env-write: the env mirror is suppressed, so re-seeding
            // every slot from the restored env would clobber a propagating outer
            // var's LIVE in-block value with its stale decl-time seed — e.g.
            // `my $ct = CT.new(...)` whose slot never mirrored into env, so a second
            // `$ct.x` inside one interpolation read `Any`. The live in-block
            // `self.locals` already holds the correct value for a propagating outer
            // name (the block body read/mutated the slot directly). Revert ONLY the
            // block's OWN isolated declarations (reverted to their pre-block value so
            // they don't leak) and internal/special/dynamic slots; leave every other
            // slot at its live value. This mirrors the env-side keep/revert decision
            // above without depending on the env mirror.
            for (idx, val) in revert_slots {
                self.locals[idx] = val;
            }
            self.stack.push(block_result);
        }
        *ip = end;
        result
    }

    /// A scope-isolating block's own declarations, by bare (sigil-stripped)
    /// name, from the `isolate_decls_idx` constant of `OpCode::DoBlockExpr`.
    // Cost: O(k), k = names the block declares.
    fn isolate_decl_names(
        code: &CompiledCode,
        isolate_decls_idx: u32,
    ) -> std::collections::HashSet<String> {
        if isolate_decls_idx == u32::MAX {
            return std::collections::HashSet::new();
        }
        match code.constants[isolate_decls_idx as usize].view() {
            ValueView::Array(items, ..) => items
                .iter()
                .filter_map(|v| match v.view() {
                    ValueView::Str(s) => Some(Self::strip_isolate_sigil(s.as_ref()).to_string()),
                    _ => None,
                })
                .collect(),
            _ => std::collections::HashSet::new(),
        }
    }

    fn strip_isolate_sigil(name: &str) -> &str {
        name.strip_prefix(['$', '@', '%', '&']).unwrap_or(name)
    }

    fn is_plain_isolate_user_var(name: &str) -> bool {
        crate::opcode::is_plain_user_var_name(name)
    }

    /// The local slots a scope-isolating block reverts on exit: its own
    /// declarations (`isolate_set`) and every slot that is not a plain user
    /// variable. Found through the chunk's indexes rather than by scanning the
    /// frame's locals.
    // Cost: O(k + s), k = names the block declares, s = the chunk's non-plain
    // (special/internal/dynamic) local slots.
    fn isolate_revert_slots(
        code: &CompiledCode,
        isolate_set: &std::collections::HashSet<String>,
    ) -> Vec<usize> {
        let mut slots: Vec<usize> = code
            .non_plain_local_slots()
            .iter()
            .map(|&i| i as usize)
            .collect();
        for bare in isolate_set {
            slots.extend(code.local_slots_of_bare(bare).iter().map(|&i| i as usize));
        }
        slots.sort_unstable();
        slots.dedup();
        slots
    }

    /// `BEGIN <expr>` that stayed inside a routine (see `compile_expr_phaser`).
    /// Same memo mechanism as `once`, but keyed by the compile-time site id
    /// alone: a BEGIN is one value shared by every clone of the enclosing code,
    /// not one per clone.
    pub(super) fn exec_begin_once_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        site_id: u64,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let cache_key = format!("BEGIN#{site_id}");
        let store = std::sync::Arc::clone(self.once_store());
        match store.claim(&cache_key) {
            crate::runtime::once_store::OnceClaim::Cached(value) => {
                self.stack.push(value);
                *ip = body_end as usize;
                Ok(())
            }
            crate::runtime::once_store::OnceClaim::Claimed => {
                let body_start = *ip + 1;
                let end = body_end as usize;
                let stack_base = self.stack.len();
                // ADR-0041 §9: a value-position `BEGIN` is as much BEGIN time
                // as the statement form, so its body sees only the sub
                // declarations the program has textually reached. (Unlike
                // `CheckPhaserStart` this does NOT raise `check_phaser_depth`:
                // whether a throw here should surface as `X::Comp::BeginTime`
                // is a separate question this opcode has always answered "no".)
                self.begin_time_enter();
                let ran = self.run_range(code, body_start, end, compiled_fns);
                self.begin_time_leave();
                match ran {
                    Ok(()) => {
                        let value = if self.stack.len() > stack_base {
                            self.stack.pop().unwrap_or(Value::NIL)
                        } else {
                            Value::NIL
                        };
                        store.fulfill(&cache_key, value.clone());
                        self.stack.push(value);
                        *ip = end;
                        Ok(())
                    }
                    Err(e) => {
                        store.abandon(&cache_key);
                        Err(e)
                    }
                }
            }
        }
    }

    pub(super) fn exec_once_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        // The op's own bytecode position is the deterministic per-site id (stable
        // across recompiles); the scope prefix is the enclosing code-object clone.
        let cache_key = loan_env!(self, once_scope_key(*ip));
        let store = std::sync::Arc::clone(self.once_store());
        // Claim the site: get the cached result, or take ownership to run the body
        // (blocking until a peer thread that owns the claim publishes its result).
        match store.claim(&cache_key) {
            crate::runtime::once_store::OnceClaim::Cached(value) => {
                self.stack.push(value);
                *ip = body_end as usize;
                Ok(())
            }
            crate::runtime::once_store::OnceClaim::Claimed => {
                let body_start = *ip + 1;
                let end = body_end as usize;
                let stack_base = self.stack.len();
                match self.run_range(code, body_start, end, compiled_fns) {
                    Ok(()) => {
                        let value = if self.stack.len() > stack_base {
                            self.stack.pop().unwrap_or(Value::NIL)
                        } else {
                            Value::NIL
                        };
                        store.fulfill(&cache_key, value.clone());
                        self.stack.push(value);
                        *ip = end;
                        Ok(())
                    }
                    Err(e) => {
                        // The body threw: release the claim so a retry or a waiting
                        // peer can run it, then propagate the error.
                        store.abandon(&cache_key);
                        Err(e)
                    }
                }
            }
        }
    }

    // Cost: O(1) for a `$` variable; O(e) for an `@`/`%` variable,
    // e = the container's elements (one-level `snapshot_container_for_temp`).
    pub(super) fn exec_let_save_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_temp: bool,
        slot: Option<u32>,
    ) {
        let name = Self::const_str(code, name_idx).to_string();
        // §1.4/§1.5: when the compiler baked THIS frame's slot for `name`, read the
        // pre-scope value straight from the live slot rather than from `env` by
        // name. The slot is always current; `env` is only a mirror and can be stale
        // under the (B) per-store env-write — a plain-lexical assignment
        // `$x = ...` before this `temp`/`let` may not have mirrored into `env`, so
        // an env-first read would snapshot the decl-seed `Any` instead of the real
        // value. The matching restore side (`restore_let_value`) already prefers the
        // baked slot.
        // An ATTRIBUTE is the one name whose source of truth is neither of those:
        // it lives in `self`'s shared attribute cell, which `exec_get_local_op`
        // re-reads on every `$!x`, and which the restore side writes back
        // (`restore_let_value`). Reading the slot or `env` for one can snapshot a
        // value the attribute never held — `temp $!logger.level` emits a `LetSave`
        // naming `!logger`, and in a method body that has not otherwise touched it
        // both the slot and `env` are empty, so the snapshot was `Nil` and the
        // restore wrote that `Nil` over a live object. Save and restore have to
        // read and write the same store.
        let old_val = match crate::value::attr_twigil_base(&name)
            .and_then(|_| self.read_self_attr_cell(&name))
        {
            Some(attr_val) => attr_val,
            None => match slot {
                Some(s) if (s as usize) < self.locals.len() => self.locals[s as usize].clone(),
                _ => self
                    .get_env_with_main_alias(&name)
                    .or_else(|| {
                        code.locals
                            .iter()
                            .position(|n| n == &name)
                            .map(|i| self.locals[i].clone())
                    })
                    .unwrap_or(Value::NIL),
            },
        };
        // A boxed (shared-cell) scalar saves its INNER value, decoupled from the
        // cell: otherwise the snapshot would be the same Arc that the dynamic-scope
        // write mutates, so the restore would see the modified value, not the
        // original (named-sub captured-outer boxing — see
        // docs/captured-outer-cell-sharing.md). The matching write-through restore
        // is in `restore_let_value`. Gated on the same toggle as the boxing it
        // supports, so the default build is byte-identical to before.
        let old_val = match old_val.view() {
            ValueView::ContainerRef(arc) => arc.lock().unwrap().clone(),
            _ => old_val.clone(),
        };
        // Copy an Array/Hash one level deep so the snapshot is independent of
        // future element writes: those go through the shared backing node
        // (container identity §3), so sharing it would observe every later
        // `@a[i] = v` / `%h<k> = v` and the restore would be a no-op. `let`
        // needs this as much as `temp` (it restores on block failure).
        //
        // A plain `$` variable is not copied at all: its snapshot is the value
        // it held -- the same Array/Hash object, so `temp $s; $s[0] = 7` keeps
        // the element write after the restore, exactly as in raku. Only the
        // `@`/`%` container itself is snapshot as a fresh node. An element temp
        // saves just that element (`exec_let_save_elem_op`).
        let save_val = if name.starts_with(['@', '%']) {
            Self::snapshot_container_for_temp(&old_val)
        } else {
            old_val
        };
        self.let_saves_push(name, save_val, is_temp, slot);
    }

    /// Snapshot an Array/Hash for `temp`/`let`: a fresh backing node holding
    /// the container's element CONTAINERS as they are, and preserving its
    /// embedded metadata (type parameters, defaults, shape, object-hash keys)
    /// so a restored `my %h{Pair}` / `my @a is default(...)` keeps its identity.
    /// Nested containers are shared, not copied (`temp @a; @a[0][0] = 99`
    /// survives the restore in raku too).
    ///
    /// A bound element (`@a[0] := $x`) stays bound in the snapshot, while the
    /// LIVE container is decontainerized in place: inside the scope `@a[0] = 5`
    /// must not reach `$x`, and after it the restore puts the bound element
    /// back, so `$x` and `@a[0]` are one container again -- both as in raku
    /// (#9435).
    fn snapshot_container_for_temp(val: &Value) -> Value {
        fn decont(v: &mut Value) {
            let inner = match v.view() {
                ValueView::ContainerRef(arc) => match arc.lock() {
                    Ok(guard) => guard.clone(),
                    Err(poisoned) => poisoned.into_inner().clone(),
                },
                _ => return,
            };
            *v = inner;
        }
        match val.view() {
            ValueView::Array(arc_vec, kind) => {
                let snapshot =
                    Value::array_with_kind(crate::gc::Gc::new((**arc_vec).clone()), kind);
                // SAFETY: aliased in-place element edit of the live array (see
                // `gc_contents_mut`); `decont` never re-enters the interpreter.
                unsafe { crate::value::gc_contents_mut(&arc_vec) }
                    .items_mut()
                    .iter_mut()
                    .for_each(decont);
                snapshot
            }
            ValueView::Hash(arc_map) => {
                let snapshot = Value::hash_with_data(crate::gc::Gc::new((**arc_map).clone()));
                // SAFETY: as above, for the live hash's values.
                unsafe { crate::value::gc_contents_mut(&arc_map) }
                    .map
                    .values_mut()
                    .for_each(decont);
                snapshot
            }
            _ => val.clone(),
        }
    }

    pub(super) fn exec_let_block_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        value_on_stack: bool,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let mark = self.let_saves_len();
        let body_start = *ip + 1;
        let end = body_end as usize;
        match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => {
                // A value-position block left its value on the stack for the
                // enclosing expression; peek it rather than reading the topic,
                // which it never wrote (see `OpCode::LetBlock`).
                let result = if value_on_stack {
                    self.stack.last().cloned().unwrap_or(Value::NIL)
                } else {
                    self.env().get("_").cloned().unwrap_or(Value::NIL)
                };
                let success = Self::is_let_success(&result);
                loan_env!(self, resolve_let_saves_on_success(mark, success));
                // `let`/`temp` restore writes the saved value back into `env` only;
                // the matching local slot still holds the in-block value. The restore
                // recorded each restored name precisely (`restore_let_value`); drain
                // it so the frame's slots refresh.
                self.apply_pending_rw_writeback(code);
            }
            Err(e) => {
                loan_env!(self, restore_let_saves(mark));
                self.apply_pending_rw_writeback(code);
                return Err(e);
            }
        }
        *ip = end;
        Ok(())
    }
}
