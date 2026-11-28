//! `GetGlobal` / `GetGlobalDeferred`: a scalar read by name.
//!
//! The opcode's whole arm, moved out of `exec_ops_group_00` so the slot form of a
//! routine-nested sub's free-variable read (`GetLexFree`, `vm_lexsub_free_slot.rs`)
//! runs exactly this resolution when its slot holds no binding yet.

use super::*;
use crate::meta_ns::MetaNs;

impl Interpreter {
    /// Push the value of the variable the constant `name_idx` names, resolved by name.
    // Cost: O(d) on a hit, d = env tiers walked (overlay -> parent chain -> base);
    // a miss runs the fallback chain, O(d + p*|name|), p = enclosing packages probed.
    pub(super) fn exec_get_global_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        // `GetGlobalDeferred` hands a pending `HashEntryRef` bind token
        // over whole; every other value reads exactly like `GetGlobal`.
        if matches!(code.ops[*ip], OpCode::GetGlobalDeferred(_))
            && let Some(held) = self.get_env_with_main_alias(name)
        {
            // The caller's binding may be a shared cell that holds the token.
            let token = match held.view() {
                ValueView::ContainerRef(cell) => cell.load(),
                _ => held,
            };
            if matches!(token.view(), ValueView::HashEntryRef { .. }) {
                self.stack.push(token);
                *ip += 1;
                return Ok(());
            }
        }
        // mutsu#17971: a routine-nested sub's free variable this call
        // already resolved to its alias cell (`vm_lexsub_free_cache.rs`).
        if let Some(val) = self.lexsub_free_cache_read(name, code.const_sym(name_idx)) {
            self.stack.push(val);
            *ip += 1;
            return Ok(());
        }
        if self.rw_return_context
            && !name.starts_with(['@', '%', '&', '*', '!', '?'])
            && !crate::qualified::is_qualified(code.const_sym(name_idx))
        {
            self.exec_get_scalar_container_op(code, name_idx, None);
            *ip += 1;
            return Ok(());
        }
        if name == "?CALLER::LINE" {
            let line = self.get_caller_line(1).unwrap_or(Value::NIL);
            self.stack.push(line);
            *ip += 1;
            return Ok(());
        }
        // $*THREAD: the current thread's own Thread object.
        if name == "*THREAD" || name == "$*THREAD" {
            self.stack.push(crate::runtime::current_thread_value());
            *ip += 1;
            return Ok(());
        }
        // Atomic-variable read: only possible once some `atomicint`/atomic
        // storage has been registered under this name. Skip the whole check
        // (a `format!` plus two `var_type_constraint` lookups) on the hot
        // read path otherwise, which is the overwhelmingly common case.
        // The interpreter's own flag stays the first half: a thread cloned
        // before the atomic was registered reads its own variable directly.
        if self.atomic_var_seen() && Self::atomic_name_possible(name) {
            let atomic_name = name.strip_prefix('$').unwrap_or(name);
            let atomic_name_key = MetaNs::AtomicName.owned_key_for_str(atomic_name);
            let is_atomic_int = loan_env!(self, var_type_constraint(name)).as_deref()
                == Some("atomicint")
                || loan_env!(self, var_type_constraint(atomic_name)).as_deref()
                    == Some("atomicint")
                || self.get_shared_var(&atomic_name_key).is_some();
            if is_atomic_int {
                let fetched = self.vm_call_function(
                    "__mutsu_atomic_fetch_var",
                    vec![Value::str(atomic_name.to_string())],
                )?;
                self.stack.push(fetched);
                *ip += 1;
                return Ok(());
            }
        }
        // Phase 3 Stage 2c (ii): a sigilless attribute (`has $x`) compiles
        // to a bare `Var("x")` that reads via GetGlobal (it is not a method
        // local), so route it to `self`'s shared cell here too — otherwise
        // a read after a nested-frame mutation sees the stale entry copy.
        // `read_self_attr_cell` is gated on `sigilless_attrs_active`, so
        // non-sigilless programs pay only a string check.
        if let Some(cell_val) = self.read_self_attr_cell(name) {
            self.stack.push(cell_val);
            *ip += 1;
            return Ok(());
        }
        // Rakudo parity: a private-attribute read on a concrete invocant
        // whose class does not carry the attribute throws (P6opaque
        // no-such-attribute) instead of yielding Nil.
        if name.starts_with('!')
            && name.len() > 1
            && !name.starts_with("__")
            && let Some(err) = self.missing_private_attr_read_error(name)
        {
            return Err(err);
        }
        // Fast scalar read (J4 helper hot path): the dominant GetGlobal
        // shape is a plain env hit (topic `$_` reads in loop bodies,
        // dynamic vars). Serve it through the memoized per-slot Symbol —
        // no per-read intern, and no kebab-alias re-probe for the 1-char
        // topic name `_` (whose underscore otherwise sends every read
        // through the `_`->`-` pre-pass of get_env_with_main_alias).
        // Gate order reproduces the slow chain's precedence: the stores
        // consulted before env must be provably inactive (escaping-our
        // captures empty, no real current package, no ADR-0024 mainline
        // capture active), @/% names keep the slow path's atomic/
        // thread-clone arms, and Nil / LazyThunk / ContainerRef hits
        // fall through for the slow tail's default/type-object, force
        // and deref handling.
        //
        // ADR-0024: `mainline_lexical_frame_active()` must also gate
        // this shortcut, for a reason `is_container_ref()` alone does
        // NOT cover: a mainline named sub's captured cell is installed
        // into BOTH its own local slot and the env key at
        // registration, but an ordinary later `my $name = ...` in an
        // unrelated (shadow-slot) scope — even one the sub was NOT
        // declared inside — explicitly clears a stale `ContainerRef`
        // sitting in env under its own name before writing its fresh
        // value (`exec_set_local_op_inner`'s redeclaration guard), so
        // by the time the sub runs the env key can hold a perfectly
        // plain (non-cell) value that is NOT the sub's own captured
        // binding. `unit_lexical_slot`/`get_env_with_main_alias`
        // consult `unit_lexicals[MAINLINE_UNIT_KEY]` before env
        // precisely to survive that; this shortcut must not bypass it.
        // A pointy `for` parameter of the running call is the
        // innermost binding of its name, held in `env` while its loop
        // runs: it shadows a same-named file-scope or package-block
        // lexical and an `our` mirror, every store the slow chain
        // below asks before `env` (#17429 found `$x` in a module
        // routine's `for 1, 2 -> $x { ... }` reading the module's
        // `my $x` instead).
        let loop_bound = {
            let b0 = name.as_bytes().first().copied();
            !matches!(b0, Some(b'@' | b'%' | b'*'))
                && (name == "_" || !name.contains('_'))
                && !crate::qualified::is_qualified(code.const_sym(name_idx))
                && self.lexicals.escaping_our_lexical_names.is_empty()
                && self.topic_state.name_bound_by_loop_param(name)
        };
        let loop_param_val = if loop_bound {
            self.env().get_sym(code.const_sym(name_idx)).cloned()
        } else {
            None
        };
        let fast_hit = if let Some(val) = &loop_param_val {
            (!val.is_nil()
                && !val.is_container_ref()
                && !matches!(val.view(), ValueView::LazyThunk(_)))
            .then(|| val.clone())
        } else {
            let b0 = name.as_bytes().first().copied();
            if !matches!(b0, Some(b'@' | b'%'))
                // A dynamic or a `PROCESS::` name may resolve to the
                // process stash instead (ADR-11318); the slow chain
                // asks it first.
                && !(self.process_dynamics_published()
                    && (b0 == Some(b'*') || name.starts_with("PROCESS::")))
                && (name == "_" || !name.contains('_'))
                // A package-qualified name reads `our_vars` first.
                && !crate::qualified::is_qualified(code.const_sym(name_idx))
                && self.lexicals.escaping_our_lexical_names.is_empty()
                // The topic is every routine's own (`env::routine_owned`):
                // never a mainline capture, a routine-nested sub's alias,
                // a package-block or file-scope lexical, so the four stores
                // these gates protect cannot hold it, and a module routine's
                // `$_` reads take this path too (#18274).
                && (name == "_" || self.env_only_free_read_possible(name))
            {
                debug_assert!(
                    name != "_" || self.topic_outside_env_stores_miss(code),
                    "a store ahead of env holds the topic"
                );
                match self.env().get_sym(code.const_sym(name_idx)) {
                    Some(val)
                        if !val.is_nil()
                            && !val.is_container_ref()
                            && !matches!(val.view(), ValueView::LazyThunk(_)) =>
                    {
                        Some(val.clone())
                    }
                    _ => None,
                }
            } else {
                None
            }
        };
        if let Some(val) = fast_hit {
            self.stack.push(val);
            *ip += 1;
            return Ok(());
        }
        let val = self.get_global_chain_value(code, name_idx, name, loop_param_val)?;
        // `OUR::`-qualified variable reads are scoped to the CURRENT
        // package (`$OUR::x` inside `package A {}` is `A::x`, bare `x`
        // at file scope). Authoritative: a miss is an undefined package
        // variable, so it overrides any same-named GLOBAL `our` the
        // generic chain above may have leaked (`our_pseudo_var_read`
        // returns None for non-`OUR::` names, leaving `val` untouched).
        let val = self.our_pseudo_var_read(name).unwrap_or(val);
        // When the value is Nil and the variable has a type constraint,
        // return the type object (consistent with GetLocal behavior).
        // No constraint probe here (ADR-0042 slice 3): a typed scalar's
        // type-object seed comes from its declaration and its Nil-reset
        // from the store path, so a Nil that reaches here — a `= Nil`
        // parameter default — is genuinely Nil. See the matching
        // comment in `vm_var_assign_local_get.rs`.
        let val = if val.is_nil() {
            match self.var_default(name) {
                Some(def) => def.clone(),
                None => val,
            }
        } else {
            val
        };
        // Force lazy thunks transparently on access. Tag-probed first: a
        // `view()` would materialize a lazy Match (see `exec_get_local_op`).
        let val = if val.is_lazy_thunk_value()
            && let ValueView::LazyThunk(thunk_data) = val.view()
        {
            self.force_lazy_thunk(&thunk_data)?
        } else {
            val
        };
        // Auto-deref ContainerRef for stack use (ContainerRef axis of the
        // decont family; moves through for the common non-container case).
        let val = val.into_deref();
        if matches!(code.ops[*ip], OpCode::GetGlobal(_)) {
            self.lexsub_free_cache_fill(code, name, code.const_sym(name_idx), &val);
        }
        self.stack.push(val);
        *ip += 1;
        Ok(())
    }
}
