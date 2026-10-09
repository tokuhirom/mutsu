use super::vm_call_method_ops::MethodName;
use super::*;
use crate::symbol::Symbol;
use crate::value::ValueMap;
use crate::value::types::is_stash_class_name;

impl Interpreter {
    /// The invocant of a `.&sub(...)` call is always bound positionally to the
    /// sub's first parameter, even when it is a literal colonpair (`:42foo.&f`).
    /// A bare `Pair` in an argument list is otherwise splatted into a
    /// named argument, so containerize a Pair invocant into a positional
    /// `ValuePair` (the same conversion `OpCode::ContainerizePair` performs).
    fn invocant_as_positional(target: Value) -> Value {
        match target.view() {
            ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
            _ => target,
        }
    }

    /// Derive the method name for an indirect call `$obj.$name`. A **type
    /// object** used as the name specifier (`$string.$type` with `$type = Int`)
    /// dispatches the method named by its short name (`.Int`), so use that name
    /// rather than the type object's gist (`(Int)`). Any other value falls back
    /// to its string form (mutsu treats a plain `Str` as a method name).
    pub(super) fn dynamic_method_name(name_val: &Value) -> String {
        match name_val.view() {
            ValueView::Package(name) => name.resolve(),
            _ => name_val.to_string_value(),
        }
    }

    /// Take the run-time method name out of a dynamic call's operands
    /// (`[.., target, name, args...]` becomes `[.., target, args...]`) and
    /// decide how it dispatches. `None` means the name is a Callable to invoke
    /// with the target as its first argument (`$obj.$code(...)`,
    /// `$obj."&f"`-style Sub values); the value is then returned in `Err`.
    ///
    /// Cost: O(a), a = arguments (the name slot is removed from under them).
    fn take_dynamic_method_name(
        &mut self,
        arity: usize,
        quoted: bool,
        opcode: &str,
    ) -> Result<Result<String, Value>, RuntimeError> {
        let Some(name_pos) = self.stack.len().checked_sub(arity + 1).filter(|&p| p >= 1) else {
            return Err(RuntimeError::new(format!(
                "Interpreter stack underflow in {opcode}"
            )));
        };
        let name_val = self.stack.remove(name_pos);
        let is_callable = (!quoted && !matches!(name_val.view(), ValueView::Package(_)))
            || matches!(
                name_val.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            );
        Ok(if is_callable {
            Err(name_val)
        } else {
            Ok(Self::dynamic_method_name(&name_val))
        })
    }

    /// `$obj.$code(args)` / `$obj.&code(args)`: call `callable` with the
    /// receiver as its first positional argument. Stack: `[.., target,
    /// args...]`. The `.+`/`.*` modifiers wrap the single result in an Array.
    fn exec_dynamic_callable_method(
        &mut self,
        code: &CompiledCode,
        callable: Value,
        arity: usize,
        modifier_idx: Option<u32>,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // ADR-0054 S3: spread only the `|EXPR` positions.
        let (args, _arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, None);
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in dynamic method call target")
        })?;
        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(Self::invocant_as_positional(target));
        call_args.extend(args);
        let result = self.vm_call_on_value(callable, call_args, None)?;
        match modifier_idx.map(|idx| Self::const_str(code, idx)) {
            Some("+") | Some("*") => self.stack.push(Value::array(vec![result])),
            _ => self.stack.push(result),
        }
        Ok(())
    }

    /// `OpCode::CallMethodDynamic` (`$obj."$name"(...)`, `$obj.$code(...)`).
    /// Owns only the name resolution: a Callable is invoked on the receiver,
    /// and a method name dispatches through the `CallMethod` body with the
    /// run-time spelling, so the two forms cannot drift (#9454). A run-time
    /// name is never a compile-time macro, so it dispatches as a quoted name
    /// (`$obj."$m"()` with `$m = "WHAT"` calls a user `WHAT` method, as
    /// `$obj."WHAT"()` does).
    pub(super) fn exec_call_method_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        match self.take_dynamic_method_name(arity as usize, quoted, "CallMethodDynamic")? {
            Err(callable) => {
                // A code-object "name" is not an accessor read: drop any
                // accessor-ref request so it cannot leak to a later dispatch.
                self.accessor_ref_pending = false;
                self.exec_dynamic_callable_method(
                    code,
                    callable,
                    arity as usize,
                    modifier_idx,
                    arg_sources_idx,
                )
            }
            Ok(method) => self.exec_call_method_named_op(
                code,
                MethodName::dynamic(&method),
                arity,
                modifier_idx,
                arg_sources_idx,
            ),
        }
    }

    /// `OpCode::CallMethodDynamicMut` (`$var."$name"(...)` on a named
    /// receiver): as [`Self::exec_call_method_dynamic_op`], delegating a method
    /// name to the `CallMethodMut` body.
    pub(super) fn exec_call_method_dynamic_mut_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        match self.take_dynamic_method_name(arity as usize, quoted, "CallMethodDynamicMut")? {
            Err(callable) => {
                // A code-object "name" is not an accessor read: drop any
                // accessor-ref request so it cannot leak to a later dispatch.
                self.accessor_ref_pending = false;
                self.exec_dynamic_callable_method(
                    code,
                    callable,
                    arity as usize,
                    modifier_idx,
                    arg_sources_idx,
                )
            }
            Ok(method) => self.exec_call_method_mut_named_op(
                code,
                MethodName::dynamic(&method),
                arity,
                target_name_idx,
                modifier_idx,
                arg_sources_idx,
            ),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_call_method_mut_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.exec_call_method_mut_named_op(
            code,
            MethodName::from_const(code, name_idx, quoted),
            arity,
            target_name_idx,
            modifier_idx,
            arg_sources_idx,
        )
    }

    /// The `CallMethodMut` body for an already-resolved method name (see
    /// `exec_call_method_named_op`; `CallMethodDynamicMut` owns only the name
    /// resolution). Stack: `[.., target, args...]`.
    pub(super) fn exec_call_method_mut_named_op(
        &mut self,
        code: &CompiledCode,
        name: MethodName<'_>,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let quoted = name.quoted;
        // Whether the receiver is `Nil`, read before the impl consumes the
        // operands (the stack is `[.., target, args...]` here, so the target is
        // `arity` slots below the top). Used for the Nil-absorb fallback below.
        let receiver_is_nil = self
            .stack
            .len()
            .checked_sub(arity as usize + 1)
            .and_then(|i| self.stack.get(i))
            .is_some_and(Value::is_nil);
        // A front mutation of a lazy `@`-array over an infinite sequence runs on
        // a reified prefix and is stitched back in front of the live tail
        // afterwards (see `vm_lazy_front_mutate`).
        let front_mutation = if modifier_idx.is_none() {
            self.lazy_seq_front_mutation_prepare(
                code,
                name.raw,
                arity,
                Self::const_str(code, target_name_idx),
            )
            .transpose()?
        } else {
            None
        };
        let result = self.exec_call_method_mut_op_impl(
            code,
            name,
            arity,
            target_name_idx,
            modifier_idx,
            quoted,
            arg_sources_idx,
        );
        if let Some(pending) = front_mutation
            && result.is_ok()
        {
            self.lazy_seq_front_mutation_finish(
                code,
                Self::const_str(code, target_name_idx),
                pending,
            );
        }
        // Nil absorbs a method it does not define (raku's `Nil.FALLBACK`), the
        // same verdict the scalar `CallMethod` opcode and the hyper path reach.
        // This opcode -- a method call on a *named* receiver -- never applied
        // it, so `$?DISTRIBUTION.meta<ver>` outside a distribution died with
        // "No such method 'meta'" where raku answers Nil.
        //
        // Applied only *after* normal dispatch fails to find the method, not as
        // a pre-dispatch shortcut: `Nil` really does define control-flow and
        // introspection methods (`&?BLOCK.leave` on a Nil block, the exception
        // accessors), and short-circuiting those to Nil silently skipped them
        // (S04-statements/leave.t, S32-exceptions/misc.t). Falling back on the
        // not-found error is what `FALLBACK` means. `is_nil` is strictly `Nil`,
        // so an uninitialised `Any` receiver still errors as before.
        let result = match result {
            Err(e) if receiver_is_nil && Self::is_method_not_found_error(&e) => {
                self.stack.push(Value::NIL);
                Ok(())
            }
            other => other,
        };
        // The pending arg-source names/slots are scoped to THIS dispatch: a
        // callee signature bind consumes them, but a native/builtin dispatch
        // never binds and would leave them behind. A later bind with no
        // interleaving call opcode (e.g. the next chunk call of a Rust-driven
        // `.map` loop) would then re-resolve its sigilless params from the
        // leftover names against stale env keys (S32-hash/multislice-6e.t:
        // `-> \k, \v { Pair.new(k,v) }` repeated the first chunk). Clear on
        // every exit, mirroring the CallFunc/CallOnCodeVar set-then-clear pair.
        self.set_pending_call_arg_sources(None);
        self.pending_call_arg_source_slots.clear();
        // The constructor-lane candidate is scoped to this dispatch too: one a
        // probe claimed must not be installed by a later, unrelated arrival at
        // the native constructor.
        self.caches.ctor_lane_candidate = None;
        result
    }

    /// Method names a branch between the top of `exec_call_method_mut_op_impl`
    /// and its env-pure gate inspects by name for receivers of every kind (or
    /// whose receiver test is not obviously closed to a plain scalar), so the
    /// early scalar lane leaves them to the full path. Over-listing only costs
    /// a missed shortcut.
    // Cost: O(1).
    pub(super) fn scalar_early_lane_skips(method: &str) -> bool {
        matches!(
            method,
            "raku"
                | "perl"
                | "gist"
                | "say"
                | "note"
                | "put"
                | "print"
                | "VAR"
                | "WHAT"
                | "^name"
                | "substr-rw"
                | "subbuf-rw"
                | "BIND-KEY"
                | "value"
                | "ACCEPTS"
                | "combinations"
                | "int-bounds"
                | "message"
                | "freeze"
                | "so"
                | "not"
                | "Bool"
                | "pairs"
                | "antipairs"
                | "kv"
                | "cache"
                | "List"
                | "list"
                | "values"
                | "skip"
                | "rotor"
                | "batch"
                | "unique"
                | "repeated"
                | "squish"
                | "produce"
                | "flat"
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn exec_call_method_mut_op_impl(
        &mut self,
        code: &CompiledCode,
        name: MethodName<'_>,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        // ADR-0121 D3: a generated accessor already resolved to a slot of the
        // receiver's layout reads it before the probe chain runs (see
        // `vm_accessor_lane`).
        if arity == 0
            && modifier_idx.is_none()
            && !quoted
            && !self.accessor_ref_pending
            && !crate::runtime::find_method_intercept::any_user_find_method()
            && let Some(val) = self.try_accessor_lane(name.sym)
        {
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "accessor");
            self.stack.pop();
            self.stack.push(val);
            return Ok(());
        }
        // A no-argument pure native method on an immutable scalar receiver
        // (`$chunk.chars`, `$s.defined`, `$n.Int`): the gate further down
        // (`try_env_pure_mut_dispatch`) answers exactly this shape, and every
        // branch between here and there is keyed on an argument, a modifier, a
        // `^find_method` override, a method name the gate itself refuses or
        // `scalar_early_lane_skips` lists, or a receiver kind (`Instance`,
        // `Package`, a lazy list, a Seq, a container view, ...) that a plain
        // `Str`/`Int`/`Num`/`Bool` is not. So the same verdict is reached here,
        // without the ~700 lines of probes in between, which cost more than
        // the native method itself (#9494: ~100 such calls per parsed CSV row).
        if arity == 0
            && modifier_idx.is_none()
            && !quoted
            && arg_sources_idx.is_none()
            && !self.accessor_ref_pending
            && !Self::scalar_early_lane_skips(name.raw)
            && !crate::runtime::find_method_intercept::any_user_find_method()
            && let Some(target) = self.stack.last()
            && matches!(
                target.view(),
                ValueView::Str(_) | ValueView::Int(_) | ValueView::Num(_) | ValueView::Bool(_)
            )
        {
            let target = target.clone();
            if let Some(result) = self.try_env_pure_scalar_native_dispatch(
                "callmethodmut",
                &target,
                name.raw,
                name.sym,
                &[],
                None,
                false,
            ) {
                // The bookkeeping the full path's argument decode performs for
                // a call without argument sources.
                self.pending_call_arg_source_slots.clear();
                self.set_pending_call_arg_sources(None);
                self.stack.pop();
                self.stack.push(result?);
                return Ok(());
            }
        }
        // `.elems` / `.end` on an array (`$i < @ch.elems` in a C-style loop
        // condition): the same native answer the general path's probe gives,
        // without the receiver probes in between. See `try_array_count_lane`.
        if arity == 0
            && modifier_idx.is_none()
            && !quoted
            && arg_sources_idx.is_none()
            && !self.accessor_ref_pending
            && let Some(result) = self.try_array_count_lane(name.raw, name.sym)
        {
            self.pending_call_arg_source_slots.clear();
            self.set_pending_call_arg_sources(None);
            self.stack.pop();
            self.stack.push(result?);
            return Ok(());
        }
        // Consume (and unconditionally clear) the accessor-ref marker: it is
        // emitted immediately before this opcode and scoped to this one dispatch.
        let want_ref = std::mem::take(&mut self.accessor_ref_pending);
        crate::alloc_scope_named!(_sc_cmm_dec, "cmm:decode-sources");
        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
        crate::alloc_scope_end!(_sc_cmm_dec);
        crate::alloc_scope_named!(_sc_cmm_names, "cmm:names");
        let method_raw = name.raw;
        let target_name: &str = Self::const_str(code, target_name_idx);
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        // `rewrite_method_name` allocated a fresh `String` for the method name on
        // EVERY method call, even the overwhelmingly common no-modifier case where
        // the name is already a `&str` in the constant pool. The `_cow` variant
        // (already used by the sibling call paths) borrows it instead; only `.^`/`.!`
        // still allocate. Together with `target_name` below this was 2.0 of the
        // ~12.6 allocations `benchmarks/bench-ctor.raku` spends per method call
        // outside the fast-path scopes (#7561).
        let method_cow = Self::rewrite_method_name_cow(method_raw, modifier);
        let method: &str = &method_cow;
        // Interned once per call: the unrewritten name comes from the per-chunk
        // constant-symbol table, so the hot path pays no re-intern.
        let method_sym = match modifier {
            Some("^") | Some("!") => crate::symbol::Symbol::intern(method),
            _ => name.sym,
        };
        crate::alloc_scope_end!(_sc_cmm_names);
        crate::alloc_scope_named!(_sc_cmm_args, "cmm:args");
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallMethodMut",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let has_varref = raw_args
            .iter()
            .any(|a| matches!(a.view(), ValueView::VarRef { .. }));
        // ADR-0054 S3: spread only the positions the caller wrote as `|EXPR`
        // -- decided by call-site syntax, not by a value merely evaluating to
        // a Slip (`.method(@a.Slip)` stays one argument).
        let (args, arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, decoded_sources);
        self.set_pending_call_arg_sources(arg_sources.clone());
        // Elements appended to a NATIVE integer array store through the native
        // slot, so each one wraps to the element width exactly as an assignment
        // does (`my uint8 @e; @e.push(1, 300, 2)` stores 1, 44, 2). Done here,
        // before the several push/append dispatch branches below, so every one
        // of them sees already-wrapped values.
        let args =
            if matches!(method, "push" | "unshift" | "append" | "prepend") && !args.is_empty() {
                // Dual store: a scalar-held container (`my $a := array[uint8].new`)
                // keeps its live value — including the `array[uint8]` element-type
                // metadata `wrap_native_int_items` below reads via
                // `element_constraint_for` — in the local slot only, leaving the env
                // mirror at the `my`-declaration seed until some later sync point
                // (an I/O op, a frame boundary, ...) republishes it. Without this,
                // `native_int_element_constraint`'s `self.env().get(target_name)`
                // read the STALE, untagged env copy and silently skipped the wrap
                // (`$a.push(-1)` stored `-1` instead of wrapping to `255`), even
                // though `$a[0]`/`.of` — which read the authoritative slot — already
                // reported the array as `uint8`. Same fix as the sibling
                // element-assignment/`:delete` handlers (`seed_env_from_scalar_slot`).
                self.seed_env_from_scalar_slot(code, None, target_name);
                self.wrap_native_int_items(target_name, args)
            } else {
                args
            };
        // ADR-0040's store boundary, Proxy half — see
        // `Interpreter::fetch_proxy_mutator_args`.
        let args = self.fetch_proxy_mutator_args(method, args)?;
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallMethodMut target".to_string())
        })?;
        // A deferred hash-entry token held by a variable that keeps the
        // container on purpose (the `with`/`without` topic temp over an rw
        // routine's result, `without f() { ... }` where `f` is
        // `sub f is rw { %h<absent><key> }`) reads as the entry's current value
        // -- `Any` while the key does not exist -- as it does on the
        // `CallMethod` path. `.VAR` is the one method that wants the token.
        // The variable read decontainerizes the token, so a push-family call
        // on a variable bound to a MISSING entry (`my $e := f('x'); $e.push: 5`
        // with `sub f($k) is rw { %h{$k} }`) reads the slot's token itself to
        // autovivify the entry (`hash_entry_invocant`).
        let target = if matches!(method, "push" | "append" | "unshift" | "prepend")
            && !target_name.is_empty()
            && (target.is_nil() || matches!(target.view(), ValueView::Package(_)))
            && let Some(slot) = self.resolve_local_slot(code, None, target_name)
            && matches!(self.locals[slot].view(), ValueView::HashEntryRef { .. })
        {
            self.locals[slot].clone()
        } else {
            target
        };
        let target = if method != "VAR" && matches!(target.view(), ValueView::HashEntryRef { .. }) {
            Self::hash_entry_invocant(method, &target)
        } else {
            target
        };
        // `.VAR` reflects the variable's container. When the slot holds a
        // shared cell (an `is rw` / `is raw` parameter aliasing the caller's
        // variable), the reflector must see that cell -- it carries the
        // container's descriptor name (#11196) and identity -- so publish the
        // slot to the by-name mirror the reflector reads.
        if method == "VAR" && arity == 0 && !target_name.is_empty() {
            self.seed_env_from_scalar_slot(code, None, target_name);
            // A slot that holds a plain value is authoritative over a shared
            // cell the by-name mirror still holds from an earlier binding of
            // the same name (a light call reuses its caller's env), so the
            // reflector must not read that cell's descriptor.
            if !target_name.starts_with(['@', '%'])
                && let Some(slot) = self.resolve_local_slot(code, None, target_name)
                && !self.locals[slot].is_container_ref()
                && self
                    .env()
                    .get(target_name)
                    .is_some_and(|v| v.is_container_ref())
            {
                let current = self.locals[slot].clone();
                self.set_env_with_main_alias(target_name, current);
            }
        }
        // A user `method ^find_method` answers every call on its type
        // (`find_method_intercept`). `.+`/`.*` keep the candidate walk.
        // A raw invocant (`multi method handler(Object::Trampoline:D \SELF:
        // |args)`) receives the named receiver's container, as on the
        // ordinary dispatch below.
        if matches!(modifier, None | Some("?"))
            && let Some(found) = self.user_find_method_lookup(&target, method)
        {
            let found = found?;
            let armed = self.arm_raw_invocant_for_found_method(code, target_name, &target, &found);
            let result = self.invoke_user_found_method(found, &target, &args);
            self.disarm_raw_invocant_arrival(armed);
            self.stack.push(result?);
            return Ok(());
        }
        if method == "raku"
            && crate::builtins::methods_0arg::raku_repr::raku_scalar_itemized(&target)
        {
            let rendered = self
                .raku_repr_with_dispatch(&target)
                .unwrap_or_else(|| crate::builtins::methods_0arg::raku_repr::raku_value(&target));
            self.stack.push(Value::str(rendered));
            return Ok(());
        }
        crate::alloc_scope_end!(_sc_cmm_args);
        // `$b.subbuf-rw(...)` outside an assignment: a write-through Proxy over
        // the buffer, the `Buf` counterpart of the `substr-rw` arm above
        // (#9216). A user class's own `subbuf-rw` is untouched.
        // Cost: O(1) beyond resolving the window against the buffer's length.
        if method == "subbuf-rw"
            && modifier.is_none()
            && let ValueView::Instance { class_name, .. } = target.descalarize().view()
            && crate::runtime::utils::is_buf_like_class(&class_name.resolve())
        {
            let proxy = self.make_subbuf_rw_proxy(target.descalarize().clone(), &args)?;
            self.stack.push(proxy);
            return Ok(());
        }
        let stash_target = if method == "BIND-KEY" && args.len() == 2 {
            match target.view() {
                ValueView::Instance { class_name, .. }
                    if is_stash_class_name(class_name.as_str()) =>
                {
                    Some(target.clone())
                }
                _ => Self::caller_stash_depth(target_name)
                    .map(|depth| self.caller_stash_value(target_name, depth)),
            }
        } else {
            None
        };
        if let Some(stash_target) = stash_target {
            let key = args[0].to_string_value();
            let bind_source = arg_sources
                .as_ref()
                .and_then(|sources| sources.get(1))
                .and_then(|source| source.as_deref())
                .filter(|source| !source.contains('\0'));
            let result =
                self.bind_stash_key(code, &stash_target, &key, args[1].clone(), bind_source)?;
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "stash-bind-key");
            self.stack.push(result);
            return Ok(());
        }
        // `Pair.new($k, $v)` compiles its value argument tagged with `WrapVarRef`
        // (see `compile_expr_method_on_var`): when the receiver is the native
        // Pair type, box the source local into a shared `ContainerRef` cell so
        // the built Pair's value aliases `$v` (write-through, the same capture
        // the fat-arrow `MakePair` path performs). Any other receiver (a
        // shadowing user class, a rebound name) unwraps to the plain value —
        // identical to an untagged compile.
        let args = if has_varref {
            let native_pair_new = method == "new"
                && matches!(target.view(), ValueView::Package(cn) if cn == "Pair")
                && !self.has_user_method("Pair", "new");
            args.into_iter()
                .map(|a| match a.view() {
                    ValueView::VarRef { name, value, .. } => {
                        let inner = value.clone();
                        if native_pair_new {
                            let slot_hint = a.varref_slot();
                            // `box_type_objects`, exactly as the fat-arrow
                            // `MakePair` path does: an UNINITIALIZED declared
                            // scalar holds a bare type object but is still a
                            // container, and raku aliases it --
                            // `my Int $x; my $p = Pair.new("k", $x);
                            // $p.value = 5` leaves `$x` at 5.
                            self.capture_var_cell_boxing_type_objects(
                                code,
                                &name.resolve(),
                                inner,
                                slot_hint,
                            )
                        } else {
                            inner
                        }
                    }
                    _ => a,
                })
                .collect()
        } else {
            args
        };
        // `X::Foo.throw`/`.fail`/..., `IO::Path.e`/... on a type object (compiled
        // here because the bareword target routes through CallMethodMut) require
        // a concrete invocant: X::Parameter::InvalidConcreteness.
        if let Some(err) = self.type_object_concreteness_error(method, &args, &target) {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "exception-concreteness",
            );
            return Err(err);
        }
        // Reify/consume a deferred Seq (ADR-0034 §2.3) — including an
        // `IO::Handle.lines`/`.words` source (formerly the separate
        // `LazyIoLines` special case, forced here with a name-keyed env
        // writeback band-aid for `.cache` — ADR-0034 §1.3/§2.4). Reification
        // now fills the SAME `Arc<SeqBody>` every alias of the receiver
        // shares, so no writeback is needed: every alias (this frame's
        // variable, a second alias, a value passed to a sub one call frame
        // away) observes it for free.
        let target = match self.take_seq_prefix(&target, method, &args)? {
            Some(prefix) => prefix,
            None => target,
        };
        let target = self.reify_or_consume_seq_target(target, method)?;
        // A `does Sequence` class with its own `iterator`: see
        // `vm_sequence_role_delegate.rs`.
        if let Some(result) = self.try_sequence_role_delegate(&target, method_sym, &args) {
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
            self.stack.push(result?);
            return Ok(());
        }
        if method == "message"
            && args.is_empty()
            && let ValueView::Instance { attributes, .. } = target.view()
            && let Some(msg) = attributes.as_map().get("__mutsu_thrown_message")
        {
            self.stack.push(msg.clone());
            return Ok(());
        }
        // ADR-0070 at the mutable OPCODE entry. The push/append/unshift/prepend
        // branches below (and the `call_method_mut_with_values` arms they lead
        // to) read `args` positionally, so an adverb none of them declares was
        // stored as an ELEMENT: `@a.push(:zzz)` left `[1, 2, 3, :zzz]` where
        // raku's implicit `*%_` swallows it and the array is untouched.
        // Restricted to a native container receiver -- an `Instance`/`Package`
        // may be a user class whose own `push` declares a named parameter, and a
        // `Mixin` may carry a role method, so neither is touched here.
        let args = if matches!(target.view(), ValueView::Array(..) | ValueView::Hash(_)) {
            crate::builtins::strip_undeclared_nameds(method, &args).unwrap_or(args)
        } else {
            args
        };
        // ADR-0058: a mutating method reads its ARGUMENTS' elements through
        // pure code (`@a.splice(1, 1, (7,8).map({...}))` flattens the Seq into
        // the array), so a still-deferred `.map` argument has to run first.
        self.reify_map_grep_seq_args(&args)?;
        // Mutating methods reached through `.VAR` must retain the underlying
        // cell so the established container writeback paths can update it.
        let target = if !matches!(method, "WHAT" | "^name" | "VAR")
            && let ValueView::ContainerView(cell) = target.view()
        {
            Value::container_ref(cell.clone())
        } else {
            target
        };
        // A lexical receiver uses CallMethodMut even for a read-only method.
        // Read through scalar itemization for Range methods, while retaining the
        // wrapper for the renderers that expose itemization.
        let target = if matches!(method, "ACCEPTS" | "combinations" | "int-bounds")
            && target.descalarize().is_range()
        {
            target.descalarize().clone()
        } else {
            target
        };
        // ADR-0040 §9.2: a renderer resolves its receiver's `Proxy` elements
        // first — the third entry that needs this guard, alongside the
        // `CallMethod` opcode and `call_method_with_values_inner`. A method call
        // on a *variable* compiles to `CallMethodMut` (see
        // `compile_expr_method_on_var`), so `@a.gist` and `$l.raku` arrive here
        // and nowhere else. Placed with the other receiver-deciding steps above,
        // and after them, so it sees the receiver they settled on.
        let target = Self::gist_receiver(method, target);
        let target = if Self::renders_receiver_elements(method) && Self::holds_nested_proxy(&target)
        {
            loan_env!(self, resolve_proxies_in_value(&target))?
        } else {
            target
        };
        // Unhandled Failure explosion: calling a non-Failure method on an
        // unhandled Failure should throw before native dispatch can mistake
        // the Failure for an ordinary Cool value (for example, `Failure.lines`
        // reaches the native Str/Cool method table).
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name.as_str() == "Failure"
            && !target.is_failure_handled()
            && !matches!(
                method,
                "exception"
                    | "handled"
                    | "self"
                    | "defined"
                    | "Bool"
                    | "so"
                    | "not"
                    | "gist"
                    | "Str"
                    | "raku"
                    | "perl"
                    | "WHICH"
                    | "WHERE"
                    | "HOW"
                    | "WHY"
                    | "WHO"
                    | "backtrace"
                    | "is-handling"
                    | "WHAT"
                    | "DEFINITE"
                    | "VAR"
                    | "^name"
                    | "isa"
                    | "does"
                    | "ACCEPTS"
                    | "Failure"
                    | "sink"
            )
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&target)
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "failure-explode",
            );
            return Err(err);
        }
        if method == "value"
            && args.is_empty()
            && let Some(weight) = self.quanthash_weight_pair_value(target.unwrap_varref())
        {
            self.stack.push(weight);
            return Ok(());
        }
        if let Some(failure) = self.exception_failure_method(&target, method, &args) {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "Failure");
            self.stack.push(failure);
            return Ok(());
        }
        // Mutating a lazy `@`-array (infinite source). raku rejects operations
        // that touch the (non-existent) end — push/pop/append — with
        // `X::Cannot::Lazy`, but allows front operations (unshift/prepend/shift/
        // splice). An array over an infinite sequence spec, an endpoint-less
        // closure sequence or a triangle reduce keeps its laziness across
        // those (`vm_lazy_front_mutate`, run before this impl); the finite
        // shapes below (a closure sequence with an endpoint, a `lazy`-marked
        // finite list) still reify to a real Array first, and an infinite one
        // reaching here through another route answers the strict force's
        // `X::Cannot::Lazy`. (L2)
        if let ValueView::LazyList(ll) = target.view()
            && ll.in_array_context()
            && ll.is_genuinely_lazy()
            && let Some(action) = match method {
                "push" => Some("push to"),
                "pop" => Some("pop from"),
                "append" => Some("append to"),
                _ => None,
            }
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lazy-array-mutate-reject",
            );
            return Err(RuntimeError::cannot_lazy_with_action(action, "Array"));
        }
        let target = if let ValueView::LazyList(ll) = target.view()
            && ll.in_array_context()
            && (ll.closure_seq.is_some()
                || ll.scan_spec.is_some()
                // A `lazy`-marked list that is known finite and runs no user
                // code to reify (`my @a = lazy <b c d>`, `my @a = <a>, |lazy
                // <b c d>`): Rakudo reifies it for these mutators too.
                || (ll.is_lazy_marked() && !ll.eqv_would_hang()))
            && matches!(method, "shift" | "unshift" | "prepend" | "splice")
        {
            let items = self.force_lazy_list_vm(&ll)?;
            let reified = Value::real_array(items);
            self.env_mut()
                .insert(target_name.to_string(), reified.clone());
            reified
        } else {
            target
        };
        // `Pair.freeze`: decontainerize the value (severing any Scalar-container
        // alias), make it read-only, and return the value (Pair.rakudoc).
        if method == "freeze"
            && args.is_empty()
            && matches!(
                target.view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            )
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "pair-freeze");
            let frozen = self.pair_freeze(&target, target_name);
            self.stack.push(frozen);
            return Ok(());
        }
        // Regex.Bool / Regex.so: match against the regex's topic (see
        // `vm_regex_bool`), the same answer the `CallMethod` form gives.
        if let Some(result) = self.try_regex_bool_method(&target, method, &args) {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "regex-bool-topic",
            );
            self.stack.push(result);
            return Ok(());
        }
        // #8880: the plain-method lane. Everything from here to this opcode's
        // user-method dispatch is a chain of probes speculating that the
        // receiver might be something other than an ordinary object -- a proto,
        // an exception, an attribute accessor, a scalar, an `IO::Handle` -- and
        // for a plain `class C { method m() {...} }` every one of them answers
        // "no" on every call. Once the chain has been observed inert for this
        // receiver class and method name, skip it. The gate is re-evaluated (and
        // the install candidate cleared) on every dispatch, so a nested call run
        // from inside a probe cannot leave its key behind for an outer one.
        // See `vm_call_method_plain_lane` for what the key has to hold constant.
        // ADR-0121 D3: the constructor twin of the plain-method lane --
        // `Class.new(named...)` on a class whose `.new` has been observed to
        // walk the whole chain into the native default constructor goes
        // straight there. See `vm_ctor_lane`.
        self.caches.ctor_lane_candidate =
            Self::ctor_lane_key(&target, &args, modifier, quoted, want_ref, method_sym);
        if let Some(class_sym) = self.caches.ctor_lane_candidate
            && let Some(result) = self.try_ctor_lane(class_sym, &args)
        {
            self.caches.ctor_lane_candidate = None;
            self.stack.push(result?);
            return Ok(());
        }
        match self.plain_method_lane_key(&target, &args, modifier, quoted, want_ref, method_sym) {
            Some(lane_key) if self.plain_method_lane_hit(&lane_key) => {
                self.caches.plain_method_lane_candidate = None;
                return self.run_plain_method_lane(
                    code,
                    target_name,
                    target,
                    method,
                    method_sym,
                    args,
                );
            }
            other => self.caches.plain_method_lane_candidate = other,
        }
        // `proto method` body dispatch (see try_proto_method_body).
        if let Some(result) = self.try_proto_method_body(&target, method, &args) {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "proto");
            let v = result?;
            // Drain captured-outer writeback recorded by the dispatched multi
            // candidate (see exec_call_method_op). No-op in default builds.
            self.apply_pending_rw_writeback(code);
            self.stack.push(v);
            return Ok(());
        }
        // `Exception.Str`/`.gist` delegate to a user `message` *method* (e.g. from a
        // parameterized role). `$e.Str` on a variable compiles to CallMethodMut, so
        // the mut path needs the same interception as CallMethod.
        if let Some(out) = self.try_exception_str_via_user_message(&target, method, &args)? {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "exception-str-message",
            );
            self.stack.push(out);
            return Ok(());
        }
        // gist/Str/raku/perl of a genuinely-lazy list renders raku's placeholder
        // (`[...]` in `@` array context, `(...)` for a bare Seq, `...` for Str)
        // rather than forcing the (possibly infinite) sequence. Must run before
        // the gather-coroutine force below, which would hang on an infinite list.
        if let ValueView::LazyList(ll) = target.view()
            && matches!(method, "gist" | "Str" | "raku" | "perl")
            && ll.renders_lazy_placeholder()
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lazy-placeholder",
            );
            if matches!(method, "raku" | "perl") && Interpreter::lazy_seq_raku_applies(&ll) {
                let text = self.lazy_seq_raku(&ll)?;
                self.stack.push(Value::str(text));
                return Ok(());
            }
            self.stack
                .push(Value::str(crate::value::lazy_list_placeholder(
                    method,
                    ll.in_array_context(),
                )));
            return Ok(());
        }
        // Lazy `.first` over a gather coroutine: pull incrementally instead of
        // forcing the (possibly infinite) list to completion.
        if let ValueView::LazyList(ll) = target.view()
            && ll.needs_vm_lazy_dispatch()
            && method == "first"
            && let Some(result) = self.try_lazy_gather_first(&ll, &args)
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "lazy-first");
            self.stack.push(result?);
            return Ok(());
        }
        // Lazy `.pairs`/`.antipairs`/`.kv` over a genuinely-lazy source: build a
        // lazy index-pipe stage instead of forcing the (possibly infinite)
        // source. Matches Rakudo where these are `.is-lazy` over a lazy list.
        if args.is_empty()
            && let Some(pipe) = crate::builtins::lazy_scan::index_pipe_method(&target, method, true)
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lazy-index-pipe",
            );
            self.stack.push(pipe);
            return Ok(());
        }
        // `.skip`/`.rotor`/`.batch`/`.unique`/`.repeated`/`.squish`/`.produce`/
        // `.flat` over a lazy invocant: stream through an adaptor stage
        // instead of forcing the (possibly infinite) source (#9159).
        if let Some(pipe) = self.try_lazy_adaptor_method(&target, method, &args)? {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "lazy-adaptor");
            self.stack.push(pipe);
            return Ok(());
        }
        // `.cache` on a genuinely-lazy list stays lazy (caches on demand); see
        // the matching note in the non-mut dispatch path.
        if let ValueView::LazyList(ll) = target.view()
            && method == "cache"
            && let Some(result) = ll.cache_lazy_view()
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "lazy-cache");
            self.stack.push(result);
            return Ok(());
        }
        // A named-variable receiver reaches CallMethodMut. Its list coercion
        // must keep a live gather pullable, as the CallMethod path does for an
        // inline receiver; forcing here runs a later die before `for` can see
        // the elements already taken.
        if let ValueView::LazyList(ll) = target.view()
            && ll.coroutine.is_some()
            && args.is_empty()
            && matches!(method, "List" | "list" | "values")
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "gather-list-context",
            );
            self.stack
                .push(Value::lazy_list(crate::gc::Gc::new(ll.with_list_context())));
            return Ok(());
        }
        let target = if let ValueView::LazyList(ll) = target.view()
            && ll.needs_vm_lazy_dispatch()
            && Self::lazy_list_needs_forcing(method)
            && !(method == "join" && crate::builtins::is_join_lazy(&target))
            // A `.map`/`.grep` on a lazy pipeline, an infinite sequence/closure
            // spec, OR a gather coroutine appends another lazy stage (interpreter
            // dispatch via `is_lazy_pipe_source`) — it must not force the source
            // here, or a `gather { … }.grep(…)[^3]` would run the whole gather body
            // (and its trailing side effects) instead of pulling on demand.
            // Laziness-preserving coercions return the list unchanged (native
            // dispatch) — neither forces.
            && !(matches!(method, "map" | "grep") && ll.map_grep_appends_stage())
            && !((ll.lazy_pipe.is_some() || ll.is_infinite_spec())
                && Self::lazy_pipe_preserving_coercion(method))
            // On an infinite sequence/closure spec — OR an explicitly `lazy`-marked
            // (`lazy gather {…}`) list — the count/numeric coercions produce a
            // *soft* X::Cannot::Lazy Failure (recoverable with `//`), emitted by
            // the 0-arg native dispatch — they must not be hard-forced/reified.
            // A plain (non-`lazy`) finite gather stays forceable and reifies.
            && !((ll.is_infinite_spec() || ll.is_lazy_marked())
                && matches!(method, "elems" | "Int" | "Numeric"))
        {
            let saved_env = self.env().clone();
            // `.head(n)` only needs the first `n` elements: pull them lazily so
            // an infinite gather does not hang.
            let items = match Self::gather_head_bound(method, &args) {
                Some(n) => self.force_lazy_list_vm_n(&ll, n)?,
                // A strict force of an infinite list (lazy pipeline / infinite
                // sequence / closure spec) cannot terminate: raise
                // X::Cannot::Lazy with this method's name. A pipe whose source
                // chain provably bottoms out finite (`gather {...}.map(*+1)`)
                // DOES terminate, so it forces like any other finite list --
                // raku answers `.elems` there rather than throwing.
                None if (ll.lazy_pipe.is_some() && !ll.pipe_bottoms_out_finite())
                    || ll.is_infinite_spec() =>
                {
                    return Err(RuntimeError::cannot_lazy(method));
                }
                None => self.force_lazy_list_vm(&ll)?,
            };
            // A lazy map/grep pipeline runs its callback via `vm_call_on_value`
            // in this Interpreter, so its side effects on enclosing variables are
            // legitimate and must persist (unlike gather coroutine corruption,
            // which the env restore undoes).
            if !matches!(method, "elems" | "hyper" | "race") && ll.lazy_pipe.is_none() {
                *self.env_mut() = saved_env;
            }
            // A list-context view (`(gather {...}).List`, `.cache`) records
            // that the finite result must render as a List, not a Seq; the
            // non-mut dispatch path already honours it (`vm_call_method_ops.rs`)
            // and this one silently did not, so whether `.raku`/`eqv` saw a
            // List or a Seq depended purely on which of the two opcodes the
            // call compiled to (`CallMethod` for an inline receiver,
            // `CallMethodMut` for a named-variable one). That made
            // `my $a = (gather {...}).List; $a.raku` render `(1, 2).Seq` while
            // the two-statement spelling rendered `$(1, 2)`.
            ll.reified_value(items)
        } else {
            target
        };
        // Fast path: 0-arg attribute accessor read on an Instance (e.g.
        // `$obj.x`). A method call on a *variable* compiles to CallMethodMut for
        // potential invocant write-back, so accessor reads land here -- without
        // this they all fell back to the interpreter. The read does not mutate
        // the invocant, so no write-back to `target_name` is needed.
        if let Some(val) = self.try_fast_accessor_read(
            &target,
            method_sym,
            &args,
            modifier.is_some(),
            quoted,
            want_ref,
        ) {
            // Pure attribute read: does not mutate the invocant (see comment
            // above), so it does not dirty the caller's locals (Slice 6.3).
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "accessor");
            if !want_ref && modifier.is_none() && !quoted && args.is_empty() {
                self.note_accessor_lane(&target, method_sym);
            }
            self.stack.push(val);
            return Ok(());
        }
        // A `.wrap`ped accessor declines the fast path above (its wrappers must
        // run); when a container was asked for, run the chain with the
        // container request carried to its terminal accessor.
        if want_ref
            && args.is_empty()
            && let Some(result) = self.try_wrapped_accessor_container(&target, method)
        {
            self.stack.push(result?);
            return Ok(());
        }
        // `.so` / `.not` on a value whose type defines a user `Bool` method must
        // dispatch through that method (Mu.so / Mu.not are defined in terms of
        // .Bool) rather than the native truthiness fast path. A type that defines
        // `.so` / `.not` itself is left to full dispatch.
        if matches!(method, "so" | "not") && args.is_empty() {
            let user_bool_owner = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
                ValueView::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = user_bool_owner
                && loan_env!(self, resolve_method_with_owner(&cn, "Bool", &[])).is_some()
                && loan_env!(self, resolve_method_with_owner(&cn, method, &[])).is_none()
            {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "so-not-user-bool",
                );
                let t = self.eval_truthy(&target);
                self.stack
                    .push(Value::truth(if method == "not" { !t } else { t }));
                return Ok(());
            }
        }
        // Two more dispatch shapes that touch no env -- a pure native method on
        // an immutable scalar receiver, and text output to a native
        // `IO::Handle` -- are answered here, before the flatten below, for the
        // same reason the accessor read is. See `try_env_pure_mut_dispatch`.
        if let Some(result) =
            self.try_env_pure_mut_dispatch(&target, method, method_sym, &args, modifier, quoted)
        {
            self.stack.push(result?);
            return Ok(());
        }
        // The plain-array mutators (`@a.push(...)`, `@!fields.push: $f`) mutate
        // the array's shared backing node in place and reach the env only by
        // name, through `env_root_descended_mut` -- whose `get_mut` promotes a
        // parent-tier entry into the overlay, so a scoped env serves it as well
        // as a flat one. Between the flatten below and this helper's usual call
        // site the only branches a plain `Array` receiver with a `push`-family
        // name can meet are the junction-argument autothreading (excluded here)
        // and the shared-array lane, which bails on exactly the condition the
        // helper itself bails on. So answer it before the flatten: one push per
        // parsed CSV field paid a whole-scope env clone for nothing (#9494).
        //
        // The array mutators are rows (ADR-11276 §9.23): the same handler answers
        // here, in front of the flatten, for the plain array receiver, and later
        // for every other shape. A thread-shared name keeps its lane further
        // down, which routes it through the atomic store.
        if modifier.is_none()
            && matches!(target.view(), ValueView::Array(..))
            && !args.iter().any(Value::is_junction_value)
            && !(self.threads.shared_vars_active && !self.container_name_is_redeclared(target_name))
        {
            let mut place =
                crate::builtins::method_table::ReceiverPlace::var_in(target_name, &target, code)
                    .with_arg_sources(arg_sources.as_deref().unwrap_or(&[]));
            if let Some(result) =
                crate::builtins::method_table::invoke_mut(self, &mut place, method_sym, &args)
            {
                crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                self.shadow_check_native_row_candidate(
                    &target,
                    method,
                    method_sym,
                    args.len(),
                    true,
                );
                self.stack.push(result?);
                return Ok(());
            }
        }
        // Beyond the pure-read accessor fast path above, full method dispatch may
        // capture/iterate the env; collapse a transient scoped overlay env to a
        // flat env so the full lexical view is seen. Placed after the accessor
        // read so a `$.attr` read inside a scoped method body does not collapse
        // the overlay (defeating the per-method-call deep-copy elimination).
        self.flatten_scoped_env();
        // Detect calls on undeclared type names: when a BareWord resolved to a Str
        // (because the name wasn't a known type/class), and .new() is called on it,
        // this means the user tried to instantiate a nonexistent class.
        if method == "new"
            && let ValueView::Str(s) = target.view()
            && **s == target_name
            && target_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            && !self.has_type(target_name)
            && !Self::is_builtin_type(target_name)
            && !self.has_class(target_name)
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "undeclared-type-new",
            );
            let suggestions = self.suggest_type_names(target_name);
            return Err(RuntimeError::undeclared_type_symbols(
                target_name,
                format!("Undeclared name:\n    {} used at line 1", target_name),
                suggestions,
            ));
        }
        // Junction auto-threading: thread method calls over junction values
        if let ValueView::Junction { kind, values } = target.view()
            && !matches!(
                method,
                "Bool"
                    | "so"
                    | "WHAT"
                    | "WHICH"
                    | "^name"
                    | "gist"
                    | "Str"
                    | "defined"
                    | "THREAD"
                    | "raku"
                    | "perl"
                    | "say"
                    | "note"
            )
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "junction-invocant",
            );
            let mut results = Vec::new();
            // env_dirty substrate (docs/captured-outer-cell-sharing.md §10):
            // accumulate EVERY eigenstate's by-name caller write. Each eigenstate's
            // method dispatch records its captured-outer / `our` write into
            // `pending_rw_writeback_sources`, but the NEXT eigenstate's dispatch
            // overwrites that buffer, so only the last eigenstate's source survived
            // to the post-loop drain — a var written only by an EARLIER eigenstate
            // (`$cnt1` while the last writes `$cnt2`) was lost (double-OFF). Drain
            // each eigenstate's sources into the retain-on-miss
            // `pending_caller_var_writeback` so all of them persist; the post-loop
            // drain then writes every owning caller slot precisely from env (which
            // already holds the accumulated value).
            for v in values.iter() {
                let r = if let Some(threaded) =
                    self.maybe_autothread_method_args(v, method, &args)?
                {
                    threaded
                } else if let Some(nr) = self.try_native_method(v, method_sym, &args) {
                    nr?
                } else {
                    self.try_compiled_method_or_interpret_sym(v.clone(), method_sym, args.clone())?
                };
                results.push(r);
                let pending: Vec<String> = self
                    .pending_rw_writeback_sources
                    .drain(..)
                    .chain(self.pending_caller_var_writeback.drain())
                    .collect();
                for name in pending {
                    self.record_caller_var_writeback(&name);
                }
            }
            let junction_result = Value::junction(kind, results);
            self.stack.push(junction_result);
            // Slice F (env<->locals coherence): an invocant junction that threads
            // a *user* method mutating a captured outer / `our` variable (e.g.
            // `$junc.a` with `method a { $cnt++ }`) accumulates each eigenstate's
            // write into env correctly, but the per-call pending writeback only
            // carries the *last* eigenstate's source — so a different variable
            // written by an earlier eigenstate (`$cnt1` vs the last `$cnt2`) never
            // reaches the caller's local slot. This junction path returns before
            // the normal post-dispatch reconcile, so drain the accumulated
            // per-eigenstate writebacks into the caller's slots here; env already
            // holds every eigenstate's accumulated value.
            self.apply_pending_caller_var_writeback(code);
            return Ok(());
        }

        // Junction auto-threading for method arguments (mut variant)
        if let Some(result) = self.maybe_autothread_method_args(&target, method, &args)? {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "junction-args");
            self.stack.push(result);
            return Ok(());
        }

        // .WHO on pseudo-package Package values: build the stash in the Interpreter
        // where we have access to locals (which the interpreter doesn't have).
        if method == "WHO"
            && args.is_empty()
            && matches!(target.view(), ValueView::Package(name) if Self::is_pseudo_package_bare(&name.resolve()))
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "who-pseudo-package",
            );
            if let ValueView::Package(pkg_name) = target.view() {
                let stash = self.build_pseudo_stash(code, &pkg_name.resolve());
                self.stack.push(stash);
            }
            return Ok(());
        }

        // `Lock.protect` / `Lock::Async.protect` require a defined invocant and a
        // single Callable block. The type object (`Lock.protect: …`) or a
        // non-Callable arg (`.protect: %()`) matches no candidate and must throw
        // X::Multi::NoMatch (roast .../multi-no-match.t).
        if method == "protect" {
            let is_lock_type_object = matches!(target.view(), ValueView::Package(name)
                if matches!(name.resolve().as_str(),
                    "Lock" | "Lock::Async" | "Lock::Soft"));
            let is_lock_instance_bad_arg = matches!(target.view(),
                ValueView::Instance { class_name, .. }
                if matches!(class_name.resolve().as_str(),
                    "Lock" | "Lock::Async" | "Lock::Soft"))
                && (args.len() != 1
                    || !matches!(args[0].view(), ValueView::Sub(..) | ValueView::WeakSub(..)));
            if is_lock_type_object || is_lock_instance_bad_arg {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "lock-protect-nomatch",
                );
                return Err(
                    crate::runtime::methods_signature_errors::make_multi_no_match_error("protect"),
                );
            }
        }
        // Fast path for Lock::Async.protect — execute block inline in current Interpreter
        if method == "protect"
            && args.len() == 1
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && (class_name.as_str() == "Lock::Async" || class_name.as_str() == "Lock")
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "lock-protect");
            let lock_id = match attributes.as_map().get("lock-id").map(Value::view) {
                Some(ValueView::Int(id)) if id > 0 => id as u64,
                _ => {
                    return Err(RuntimeError::new(
                        "Lock.protect called on Lock without lock-id",
                    ));
                }
            };
            let lock = crate::runtime::native_methods::lock_runtime_by_id(lock_id)
                .ok_or_else(|| RuntimeError::new("Lock.protect could not find lock state"))?;
            let me = crate::runtime::native_methods::current_thread_id();
            crate::runtime::native_methods::acquire_lock(&lock, me)?;
            // Entering the critical section: pull the latest value of any
            // shared scalar a previous holder committed inside its own
            // critical section (mirrors Semaphore.acquire).
            self.enter_critical_section();
            let code_val = args.into_iter().next().unwrap_or(Value::NIL);
            let result = match self.try_exec_simple_shared_protect_block(code, &code_val)? {
                Some(value) => Ok(value),
                None => self.exec_protect_block_inline(code, &code_val),
            };
            self.leave_critical_section();
            let _ = crate::runtime::native_methods::release_lock(&lock, me);
            self.stack.push(result?);
            return Ok(());
        }

        // `Lock::Async.protect-or-queue-on-recursion` /
        // `.with-lock-hidden-from-recursion-check`: the recursion-aware
        // siblings of `.protect`. See `runtime::lock_async_recursion`.
        if matches!(
            method,
            "protect-or-queue-on-recursion" | "with-lock-hidden-from-recursion-check"
        ) && args.len() == 1
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && class_name.as_str() == "Lock::Async"
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lock-async-recursion",
            );
            let lock_id = match attributes.as_map().get("lock-id").map(Value::view) {
                Some(ValueView::Int(id)) if id > 0 => id as u64,
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Lock::Async.{method} called on a Lock without lock-id"
                    )));
                }
            };
            let code_val = args.into_iter().next().unwrap_or(Value::NIL);
            let result = if method == "protect-or-queue-on-recursion" {
                self.exec_lock_protect_or_queue_on_recursion(lock_id, code_val)?
            } else {
                self.exec_lock_with_lock_hidden_from_recursion_check(lock_id, code_val)?
            };
            self.stack.push(result);
            return Ok(());
        }

        // Fast path for mutating array methods on shared @-arrays.
        // Bypasses the full method dispatch chain (try_native_method →
        // call_method_mut_with_values → push_to_shared_var) for the common case
        // of pushing simple values to a shared array inside a tight loop
        // (e.g. Lock::Async.protect { push @target, $i }).
        // ...but not for a name this lineage RE-DECLARED: the store's entry under
        // it belongs to the shadowed outer binding, so funnelling a routine's own
        // `my @a` through it makes the two the same array (a nested sub's `push
        // @components` landing in the caller's `@components` broke every
        // multi-server Cro::HTTP test — see
        // `news/2026-08/threaded-array-mutation-escapes-to-the-caller.md`).
        if target_name.starts_with('@')
            && matches!(target.view(), ValueView::Array(..))
            && self.threads.shared_vars_active
            && !self.container_name_is_redeclared(target_name)
        {
            // Only a plain *lexical* `@name` is a single variable shared across
            // threads. Instance-attribute arrays (`@!order` / `@.order`) and
            // other twigil'd forms (`@*dyn`) have per-instance / per-context
            // identity, so they must NOT funnel into the global atomic store
            // keyed by name — that would accumulate pushes across every object
            // (roles-6e.t DESTROY: each `C1` instance's `@!order` doubled). They
            // keep the original base-key / interior-mutation path.
            let plain = Self::is_plain_lexical_array_name(target_name);
            match method {
                // Route through the atomic shared store. The base-key
                // `push_to_existing_shared_array`/`push_to_shared_var` write the
                // plain `@a` shared entry, which `set_shared_var` can clobber with
                // a stale empty snapshot during env sync — losing concurrent
                // `start { @a.push(...) }` updates from sibling threads. (The
                // base-key path also `extend`ed for `unshift`, appending instead
                // of prepending.) The `__mutsu_atomic_arr::` store is exempt from
                // that clobber, so concurrent push/unshift serialize and all land.
                //
                // append/prepend MUST funnel here too: once a push created the
                // atomic entry, reads prefer it, so an append applied to the
                // stale base/env copy is silently invisible — the zef
                // `populate-distributions` bug (`push @idx, ...; append @idx,
                // ...` on a hyper worker lost every appended element).
                "push" | "unshift" | "append" | "prepend" if plain && !args.is_empty() => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-push-atomic",
                    );
                    let items = if matches!(method, "push" | "unshift") {
                        crate::runtime::Interpreter::normalize_push_unshift_args(args.clone())
                    } else {
                        crate::runtime::flatten_append_args(args.clone())
                    };
                    // Stored through a native slot, so each element wraps to the
                    // element width (`my uint8 @e; @e.push(1, 300, 2)` -> 1, 44, 2).
                    let items = self.wrap_native_int_items(target_name, items);
                    let front = matches!(method, "unshift" | "prepend");
                    let result = self.shared_array_extend(target_name, items, front);
                    self.stack.push(result);
                    return Ok(());
                }
                "push" | "unshift" if !args.is_empty() => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-push-legacy",
                    );
                    let result = loan_env!(
                        self,
                        push_to_existing_shared_array(target_name, args.clone())
                    )
                    .unwrap_or_else(|| {
                        loan_env!(self, push_to_shared_var(target_name, args, &target))
                    });
                    self.stack.push(result);
                    return Ok(());
                }
                // Removal ops only lose updates once the atomic entry shadows
                // the base copy, so gate on its existence and keep the richer
                // slow path (arity/lazy/immutable errors, callable splice
                // args) for the unshadowed case.
                "pop" | "shift"
                    if plain
                        && args.is_empty()
                        && matches!(
                            target.view(),
                            ValueView::Array(_, crate::value::ArrayKind::Array)
                        )
                        && self.atomic_array_entry_exists(target_name) =>
                {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-pop-shift",
                    );
                    let (result, _) = self.shared_array_mutate(target_name, |data, _| {
                        if data.items().is_empty() {
                            crate::runtime::utils::make_empty_array_failure_what(method, "Array")
                        } else if method == "shift" {
                            data.remove(0)
                        } else {
                            data.pop().unwrap_or(Value::NIL)
                        }
                    });
                    self.stack.push(result);
                    return Ok(());
                }
                "splice"
                    if plain
                        && matches!(
                            target.view(),
                            ValueView::Array(_, crate::value::ArrayKind::Array)
                        )
                        && self.atomic_array_entry_exists(target_name) =>
                {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-splice",
                    );
                    let (removed, _) = self.shared_array_mutate(target_name, |data, _| {
                        crate::runtime::Interpreter::splice_array_data(data, &args)
                    });
                    self.stack.push(Value::real_array(removed));
                    return Ok(());
                }
                _ => {}
            }
        }

        let mut skip_native = quoted
            && matches!(
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            );
        let is_junction_target = match target.view() {
            ValueView::Junction { .. } => true,
            ValueView::Scalar(inner) => matches!(inner.view(), ValueView::Junction { .. }),
            _ => false,
        };
        if matches!(method, "gist" | "raku" | "perl") && is_junction_target {
            skip_native = true;
        }
        // Also skip native if the target has a user-defined method with this name,
        // but NOT for pseudo-methods like DEFINITE, WHAT, etc. which are macros.
        // WHICH/WHY are exceptions: unlike the other six, raku treats them as
        // ordinary, overridable methods in every call form (not just quoted),
        // so a user override must win here too.
        if !skip_native
            && !matches!(
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHERE" | "VAR"
            )
        {
            let class_name = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                ValueView::Package(name) => Some(name),
                _ => None,
            };
            if let Some(cn) = class_name
                && match target.view() {
                    ValueView::Package(_) => {
                        self.grammar_has_user_method_memo(cn, method_sym)
                            || self.package_has_applicable_user_method(&target, method, &args)
                    }
                    _ => {
                        self.grammar_has_user_method_memo(cn, method_sym)
                            // `has ObjAt $.WHICH` overrides `.WHICH` (Rake), and
                            // a quoted or run-time name (`$obj."$n"()` with
                            // `$n = "hash"`) reaches `has %.hash`'s accessor
                            // too: the plain-name lanes that answer an
                            // accessor read are skipped for a quoted name.
                            || ((quoted || matches!(method, "WHICH" | "WHY"))
                                && self.has_public_accessor(&cn.resolve(), method))
                    }
                }
            {
                skip_native = true;
            }
        }
        // A `role R is Hash` pun (a Mixin around its storage instance) whose
        // role declares `method keys`/`elems`/...: the role's method wins over
        // the native Hash one.
        if !skip_native
            && let inner = Self::hash_pun_inner(&target).unwrap_or_else(|| target.clone())
            && let ValueView::Instance { class_name, .. } = inner.view()
            && self
                .registry()
                .role_associative_base(&class_name.resolve())
                .is_some()
            && self.has_user_method_including_role(&class_name.resolve(), method)
        {
            skip_native = true;
        }
        if !skip_native
            && matches!(method, "AT-KEY" | "keys" | "values")
            && matches!(target.view(), ValueView::Instance { class_name, .. } if is_stash_class_name(class_name.as_str()))
        {
            skip_native = true;
        }
        if !skip_native
            && method == "keys"
            && target_name.starts_with('%')
            && loan_env!(self, var_hash_key_constraint(target_name)).is_some()
        {
            skip_native = true;
        }
        if !skip_native
            && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Proc::Async")
            && matches!(
                method,
                "start"
                    | "kill"
                    | "write"
                    | "close-stdin"
                    | "bind-stdin"
                    | "bind-stdout"
                    | "bind-stderr"
                    | "ready"
                    | "print"
                    | "say"
                    | "command"
                    | "started"
                    | "w"
                    | "pid"
                    | "stdout"
                    | "stderr"
                    | "Supply"
            )
        {
            skip_native = true;
        }
        if !skip_native
            && crate::runtime::nqp_ops_list::is_iteration_buffer(&target)
            && matches!(
                method,
                "elems"
                    | "AT-POS"
                    | "BIND-POS"
                    | "push"
                    | "unshift"
                    | "List"
                    | "Slip"
                    | "Seq"
                    | "iterator"
                    | "append"
                    | "prepend"
                    | "clear"
            )
        {
            skip_native = true;
        }
        // `skip_pseudo_method_native` exists for exactly one purpose: a *quoted*
        // MOP pseudo-method call (`$obj."WHAT"()`) must dispatch a user-defined
        // method of that name instead of the reflection macro
        // (`dispatch_method_by_name_1` consumes it). It is NOT a general
        // "this receiver skips native dispatch" signal, so it must be gated the
        // same way its `CallMethod` twin gates it (`vm_call_method_ops.rs`).
        // Setting it for every `skip_native` leaked the flag into the *first*
        // nested dispatch of the same method name: `my $r = any("5","6");
        // $r.raku` set it to `"raku"` (junction receiver), so the junction
        // renderer's first `"5".raku` bypassed the native repr and fell to the
        // stringifying catch-all, printing `any(5, "6")`.
        if quoted
            && skip_native
            && matches!(
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            self.dispatch.skip_pseudo_method_native = Some(method.to_string());
        }
        // Handle Match.make — must mutate the Match instance's `ast` attribute
        // and write the modified Match back to the variable.
        if method == "make" && target.is_match_instance() {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "match-make");
            let value = args.into_iter().next().unwrap_or(Value::NIL);
            if let Some(updated) = target.match_with_ast_keeping_id(value.clone()) {
                self.env_mut().insert(target_name.to_string(), updated);
                self.env_mut().insert("made".to_string(), value.clone());
                self.regex_state.action_made = Some(value.clone());
            }
            self.stack.push(value);
            return Ok(());
        }
        // .hyper/.race with named arguments in mut path
        if matches!(method, "hyper" | "race") && !args.is_empty() {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "hyper-race-config",
            );
            let mut batch: Option<i64> = None;
            let mut degree: Option<i64> = None;
            for arg in &args {
                let (key, val) = match arg.view() {
                    ValueView::Pair(k, v) => (k.clone(), crate::runtime::to_int(v)),
                    ValueView::ValuePair(k, v) => (k.to_string_value(), crate::runtime::to_int(v)),
                    _ => continue,
                };
                match key.as_str() {
                    "batch" => batch = Some(val),
                    "degree" => degree = Some(val),
                    _ => {}
                }
            }
            if let Some(b) = batch
                && b <= 0
            {
                let mut attrs = ValueMap::default();
                attrs.insert("method".to_string(), Value::str(method.to_string()));
                attrs.insert("name".to_string(), Value::str("batch".to_string()));
                attrs.insert("value".to_string(), Value::int(b));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Invalid value '{}' for 'batch' on '{}'", b, method)),
                );
                return Err(RuntimeError::typed("X::Invalid::Value", attrs));
            }
            if let Some(d) = degree
                && d <= 0
            {
                let mut attrs = ValueMap::default();
                attrs.insert("method".to_string(), Value::str(method.to_string()));
                attrs.insert("name".to_string(), Value::str("degree".to_string()));
                attrs.insert("value".to_string(), Value::int(d));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Invalid value '{}' for 'degree' on '{}'",
                        d, method
                    )),
                );
                return Err(RuntimeError::typed("X::Invalid::Value", attrs));
            }
            let items = crate::runtime::value_to_list(&target);
            let body = crate::value::SeqBody::reified(items);
            // Remember the requested batch/degree so `.configuration` can report
            // them (the HyperSeq/RaceSeq does not carry the config).
            body.set_hyper_config(batch, degree);
            let result = if method == "hyper" {
                Value::hyper_seq_body(body)
            } else {
                Value::race_seq_body(body)
            };
            self.stack.push(result);
            return Ok(());
        }
        // HyperSeq/RaceSeq delegation in mut path
        if matches!(
            target.view(),
            ValueView::HyperSeq(_) | ValueView::RaceSeq(_)
        ) {
            let is_hyper = matches!(target.view(), ValueView::HyperSeq(_));
            match method {
                "hyper" | "race" | "is-lazy" | "^name" | "WHAT" | "defined" => {
                    let items_arc = match target.view() {
                        ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let result = match method {
                        "hyper" => Value::hyper_seq_body(items_arc),
                        "race" => Value::race_seq_body(items_arc),
                        "is-lazy" => Value::FALSE,
                        "defined" => Value::TRUE,
                        "^name" => {
                            let name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                            Value::str(name.to_string())
                        }
                        "WHAT" => {
                            let name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                            Value::package(Symbol::intern(name))
                        }
                        _ => unreachable!(),
                    };
                    let arm = match method {
                        "hyper" => "hyperseq-hyper",
                        "race" => "hyperseq-race",
                        "is-lazy" => "hyperseq-is-lazy",
                        "defined" => "hyperseq-defined",
                        "^name" => "hyperseq-name",
                        "WHAT" => "hyperseq-what",
                        _ => unreachable!(),
                    };
                    crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", arm);
                    self.stack.push(result);
                    return Ok(());
                }
                "map" | "grep" => {
                    // Delegate to array, then wrap result
                    let items_arc = match target.view() {
                        ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let array_target = Value::array_with_kind(
                        crate::value::Value::array_arc(items_arc.to_vec()),
                        crate::value::ArrayKind::List,
                    );
                    let call_result = if let Some(native_result) =
                        self.try_native_method(&array_target, method_sym, &args)
                    {
                        native_result
                    } else {
                        self.try_compiled_method_mut_or_interpret_sym(
                            target_name,
                            array_target,
                            method_sym,
                            args,
                        )
                    };
                    let result_val = call_result?;
                    // ADR-0058: the delegated `.map`/`.grep` hands back a Seq
                    // whose callback has not run, and `value_to_list` is a
                    // pure reader that would see ADR-0034's empty seed -- so
                    // `@a.hyper.map({ ... })` came out empty
                    // (`t/hyper-map-implicit-named-slurpy-leak.t`). A HyperSeq
                    // is eager by construction, so pulling here is the whole
                    // contract, not a compromise.
                    self.reify_map_grep_seq(&result_val)?;
                    let result_items = crate::runtime::value_to_list(&result_val);
                    // ... and so may each ELEMENT, when the block itself
                    // returned a `.map`/`.grep` Seq
                    // (`@a.hyper.map({ @ids.map({...}) })`). The HyperSeq is
                    // built eagerly from these, and every later reader of it
                    // (`.flat`, `.gist`) is pure, so this is the last place
                    // that can run them. Mirrors the recursive pull the
                    // `MapGrep` arm of `pull_seq_source` does for its own
                    // nested results (ADR-0058 §9.3).
                    self.reify_map_grep_seq_args(&result_items)?;
                    let wrapped = if is_hyper {
                        Value::hyper_seq(result_items)
                    } else {
                        Value::race_seq(result_items)
                    };
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "hyperseq-map-grep",
                    );
                    self.stack.push(wrapped);
                    return Ok(());
                }
                "iterator" if args.is_empty() => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "hyperseq-iterator",
                    );
                    // A HyperSeq/RaceSeq allows only a single iterator (rakudo #4413):
                    // a second `.iterator` throws X::Seq::Consumed. The consumed-state
                    // is tracked on the inner Arc via the shared Seq registry.
                    let items_arc = match target.view() {
                        ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let type_name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                    // Atomic check-and-mark under one lock, so concurrent workers
                    // racing for the single iterator resolve to exactly one winner
                    // (rakudo #4413 concurrency contract). Orthogonal to
                    // ADR-0034's reify/consume split — see `claim_hyper_iterator_once`.
                    if items_arc.claim_hyper_iterator_once().is_err() {
                        return Err(crate::value::seq_consumed_error_for(type_name));
                    }
                    let array_target = Value::array_with_kind(
                        crate::value::Value::array_arc(items_arc.to_vec()),
                        crate::value::ArrayKind::List,
                    );
                    let iter =
                        crate::builtins::iterator_construct::build_iterator_instance(&array_target);
                    self.stack.push(iter);
                    return Ok(());
                }
                _ => {
                    // For all other methods, convert to List and delegate
                }
            }
        }
        // Convert HyperSeq/RaceSeq to List for remaining method dispatch
        let target = match target.view() {
            ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => Value::array_with_kind(
                crate::value::Value::array_arc(items.to_vec()),
                crate::value::ArrayKind::List,
            ),
            _ => target,
        };

        // Fast paths for xxKEY methods on Hash/Set/Bag/Mix types
        match method {
            "AT-KEY" if args.len() == 1 => {
                let inner_target = match target.view() {
                    ValueView::Scalar(inner) => inner,
                    _ => &target,
                };
                if let ValueView::Hash(map) = inner_target.view() {
                    // An object hash stores `.WHICH` keys.
                    let key = if map.key_type.is_some() {
                        crate::runtime::utils::value_which_key(&args[0])
                    } else {
                        args[0].to_string_value()
                    };
                    let raw = self.resolve_hash_entry(&map, &key);
                    // ADR-0049 slice 5 (row 25): `resolve_hash_entry` returns
                    // the raw `Value::NIL` absent-key sentinel with no
                    // compensation of its own -- every OTHER hash-key reader
                    // (`vm_var_index_ops.rs`) substitutes the container's own
                    // default (`is default(...)` -> typed element type object
                    // -> `Any`) when the key is missing; `AT-KEY` had none at
                    // all, so `%h.AT-KEY("missing")` answered a bare `Nil`
                    // instead of `(Any)`/`(Int)`/the declared default.
                    let result = if raw.is_nil() {
                        self.typed_container_default(inner_target)
                    } else {
                        raw
                    };
                    crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "at-key");
                    self.stack.push(result);
                    return Ok(());
                }
            }
            // `ASSIGN-KEY` / `DELETE-KEY` on a hash or a quant hash are rows
            // (`method_table::mutating::subscript`), answered by the generic
            // arm below; what stays is the undefined receiver, which has no
            // owner: `ASSIGN-KEY` vivifies a hash, `DELETE-KEY` answers `Nil`.
            "ASSIGN-KEY" | "DELETE-KEY"
                if matches!(target.view(), ValueView::Nil | ValueView::Package(_)) =>
            {
                let result = if method == "ASSIGN-KEY" && args.len() == 2 {
                    let mut hash = ValueMap::default();
                    hash.insert(args[0].to_string_value(), args[1].clone());
                    self.env_mut().insert(
                        target_name.to_string(),
                        Value::hash_with_data(Value::hash_arc(hash)),
                    );
                    args[1].clone()
                } else {
                    Value::NIL
                };
                crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "assign-key");
                self.stack.push(result);
                return Ok(());
            }
            // `BIND-KEY` on an undefined receiver vivifies a hash holding the
            // bound pair; every owner is a row (`method_table::mutating::subscript_bind`).
            "BIND-KEY"
                if args.len() == 2
                    && matches!(target.view(), ValueView::Nil | ValueView::Package(_)) =>
            {
                let key = args[0].to_string_value();
                let value = args[1].clone();
                let source_var = arg_sources
                    .as_ref()
                    .and_then(|s| s.get(1))
                    .and_then(|s| s.clone());
                let mut new_map = ValueMap::default();
                let mut bind_source_install: Option<(String, Value)> = None;
                if let Some(var_name) = source_var {
                    let cell = match self.env().get(&var_name).map(Value::view) {
                        Some(ValueView::ContainerRef(cell)) => cell.clone(),
                        _ => {
                            let cell =
                                crate::gc::Gc::new(crate::value::ContainerCell::new(value.clone()));
                            bind_source_install =
                                Some((var_name, Value::container_ref(cell.clone())));
                            cell
                        }
                    };
                    new_map.insert(key, Value::container_ref(cell));
                } else {
                    new_map.insert(key, value.clone());
                }
                self.env_mut().insert(
                    target_name.to_string(),
                    Value::hash_with_data(Value::hash_arc(new_map)),
                );
                if let Some((source_name, cell_val)) = bind_source_install {
                    self.set_env_with_main_alias(&source_name, cell_val.clone());
                    self.update_local_if_exists(code, &source_name, &cell_val);
                }
                crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "bind-key");
                self.stack.push(value);
                return Ok(());
            }
            _ => {}
        }

        // Pre-dispatch Nil special cases — the same verdicts the scalar
        // `MethodCall` opcode reaches: warn-and-resume coercions (`$v.Int` /
        // `$v.Str` on a variable *bound* to Nil warn like `Nil.Int`) and
        // element-mutator errors. Everything else falls through to normal
        // dispatch, and the post-dispatch FALLBACK absorb in
        // `exec_call_method_mut_op` keeps handling genuinely-unknown methods.
        if modifier.is_none() && target.is_nil() {
            match crate::vm::vm_call_method_ops::nil_predispatch_verdict(method, args.is_empty()) {
                Some(crate::vm::vm_call_method_ops::NilPredispatchVerdict::Error(err)) => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "nil-predispatch",
                    );
                    return Err(err);
                }
                Some(crate::vm::vm_call_method_ops::NilPredispatchVerdict::Warn {
                    message,
                    resume,
                }) => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "nil-predispatch",
                    );
                    let resumed = self.raise_resumable_warning(message, resume)?;
                    self.stack.push(resumed);
                    return Ok(());
                }
                None => {}
            }
        }
        // Auto-vivify undefined values (Nil, Any, Mu type objects) to empty Arrays
        // for mutating list methods. In Raku, calling push/unshift/append/prepend on
        // an undefined variable auto-vivifies it to an Array.
        let target = if matches!(method, "push" | "unshift" | "append" | "prepend")
            && (target.is_nil()
                || matches!(
                    target.view(),
                    ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu" | "Array")
                )) {
            // A `$` variable holds the vivified Array in its Scalar
            // container (raku: `my $x; $x.push(1); $x.raku` is `$[1]`), and so
            // does a never-written package-qualified `@`/`%` slot, which reads
            // as `Any` (`@GLOBAL::a.push(1); @GLOBAL::a.raku` is `$[1]`, #10962).
            let target_sym = Symbol::intern(target_name);
            let package_slot = crate::qualified::is_package_array(target_sym)
                || crate::qualified::is_package_hash(target_sym);
            let empty_array = if target_name.starts_with(['@', '%', '&']) && !package_slot {
                Value::real_array(vec![])
            } else {
                Value::real_array(vec![]).item()
            };
            // A writable loop parameter such as `for @rows <-> $row` names a
            // `ContainerRef` cell for the source element. Replacing the env
            // binding here detaches the vivified array from that element, so
            // later reads still see Any. Descend through the alias when one
            // exists; ordinary lexical variables retain the old env fallback.
            if let Some(slot) = self.env_root_descended_mut(target_name) {
                *slot = empty_array.clone();
            } else {
                self.env_mut()
                    .insert(target_name.to_string(), empty_array.clone());
                // A package-qualified name (`$GLOBAL::n.push(1)`) is a package
                // variable. The env write reaches only the running frame, whose
                // exit drops a key it did not hold on entry, so persist the
                // vivified array in `our_vars` as `SetGlobal` and the
                // read-modify-write store do (#10620). The two share one
                // array, so the mutation below lands in both.
                if crate::qualified::is_qualified(target_sym) {
                    self.set_our_var(target_name.to_string(), empty_array.clone());
                }
            }
            empty_array
        } else {
            target
        };
        // For .* and .+ modifiers, skip the single-dispatch call and go
        // directly to the all-methods-in-MRO path to avoid double execution.
        match modifier {
            Some("+") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "modifier-plus",
                );
                let vals =
                    self.call_method_all_with_fallback(&target, method, &args, skip_native)?;
                self.stack.push(Value::array(vals));
            }
            Some("*") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "modifier-star",
                );
                match self.call_method_all_with_fallback(&target, method, &args, skip_native) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
            }
            _ => {
                // A receiver-mutating built-in method answered from its row
                // (ADR-11276 §9.23): `BagHash.add`/`remove`, the QuantHash
                // mutators, `Hash.push`/`append` and `Str`'s
                // `subst-mutate`/`substr-rw` so far. The row writes through the
                // receiver's shared node (or replaces the variable's value) and
                // re-seats the dual store itself, so there is nothing to write
                // back here. By this point the receiver is settled: a lazy
                // array reified, an undefined one vivified, a user method of the
                // same name on an instance already answered.
                if modifier.is_none() {
                    let mut place = crate::builtins::method_table::ReceiverPlace::var_in(
                        target_name,
                        &target,
                        code,
                    )
                    .with_arg_sources(arg_sources.as_deref().unwrap_or(&[]));
                    if let Some(result) = crate::builtins::method_table::invoke_mut(
                        self, &mut place, method_sym, &args,
                    ) {
                        crate::vm::vm_stats::record_dispatch_entry_outcome(
                            "callmethodmut",
                            "native",
                        );
                        self.shadow_check_native_row_candidate(
                            &target,
                            method,
                            method_sym,
                            args.len(),
                            true,
                        );
                        self.stack.push(result?);
                        return Ok(());
                    }
                }
                // Native fast path for mutating Buf write methods on a mutable Buf
                // instance (ledger §1: native receiver dispatch -> Interpreter-native).
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_buf_mut(target_name, &target, method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // Native fast path for the Iterator protocol on a self-contained
                // array-backed iterator (ledger §1: native receiver dispatch ->
                // Interpreter-native). `$it.pull-one` etc. compile to CallMethodMut, so the
                // index-advancing dispatch lands here.
                if modifier.is_none()
                    && let Some(result) = self.try_native_iterator(&target, method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // Array-subclass instance delegation (mut path): when the Instance's
                // class inherits from Array, delegate mutating Array methods to the
                // backing __mutsu_array_storage attribute and write back.
                if let ValueView::Instance {
                    class_name: inst_class,
                    attributes,
                    id: inst_id,
                } = target.view()
                {
                    let cn = inst_class.resolve();
                    let is_array_method = matches!(
                        method,
                        "push"
                            | "pop"
                            | "shift"
                            | "unshift"
                            | "append"
                            | "prepend"
                            | "splice"
                            | "join"
                            | "elems"
                            | "end"
                            | "List"
                            // `.list` is `.List`'s lower-case sibling and was
                            // missing here, so `$v.list` on an `is Array`
                            // subclass wrapped the instance in a one-element
                            // list while the non-mut `CallMethod` path (which
                            // delegates unconditionally) returned the elements.
                            | "list"
                            | "Array"
                            | "Seq"
                            | "Slip"
                            | "sort"
                            | "reverse"
                            | "rotate"
                            | "unique"
                            | "squish"
                            | "flat"
                            | "map"
                            | "grep"
                            | "first"
                            | "head"
                            | "tail"
                            | "AT-POS"
                            | "ASSIGN-POS"
                            | "EXISTS-POS"
                            | "DELETE-POS"
                            | "BIND-POS"
                            // Numeric reductions / list folds over the elements.
                            | "min"
                            | "max"
                            | "minmax"
                            | "sum"
                            | "reduce"
                            | "produce"
                            // Index/element views.
                            | "kv"
                            | "pairs"
                            | "antipairs"
                            | "keys"
                            | "values"
                            // Grouping and combinatorics.
                            | "classify"
                            | "categorize"
                            | "combinations"
                            | "permutations"
                            | "rotor"
                            | "batch"
                            // Element selection (non-mutating).
                            | "pick"
                            | "roll"
                            // Junction constructors over the elements.
                            | "all"
                            | "any"
                            | "none"
                            | "one"
                    );
                    if is_array_method
                        && !self.has_user_method(&cn, method)
                        && attributes.contains_key("__mutsu_array_storage")
                        && self
                            .mro_readonly(&cn)
                            .iter()
                            .any(|n| Self::is_positional_base(n))
                    {
                        // A subclass that overrides `iterator` decides every
                        // method raku defines through the Iterable protocol, so
                        // those answer from what the override yields (twins in
                        // `vm_call_method_ops.rs` and `methods_call_dispatch.rs`).
                        // Returned directly rather than by substituting
                        // `storage` below: that binding is also what the mut
                        // path WRITES BACK, and persisting a reordered view as
                        // the instance's storage would move `[0]`/`.join`/`|`
                        // too -- which rakudo keeps on the reified elements.
                        if let Some(source) =
                            self.positional_subclass_iteration_source(&target, method)
                        {
                            let r = self.call_method_with_values(source?, method, args);
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "user",
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        let mut storage = attributes
                            .as_map()
                            .get("__mutsu_array_storage")
                            .cloned()
                            .unwrap_or(Value::real_array(Vec::new()));
                        // Interpreter-native fast path: simple mutators on the plain
                        // untyped backing array are performed in Rust and the
                        // updated storage written back, with no interpreter
                        // dispatch. Richer methods fall through below.
                        if let Some(result) =
                            self.native_array_storage_mut(&mut storage, method, &args)
                        {
                            let result = result?;
                            let updated_instance = self.write_back_array_storage_instance(
                                target_name,
                                &inst_class,
                                &attributes,
                                inst_id,
                                storage,
                            );
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            // ADR-0019 E6b step 1: shadow-check against the actual
                            // opcode receiver `target` (the Instance), not the
                            // backing `storage` value the Tier-A helper above was
                            // fed — `target` is what a real E6b cutover would
                            // query resolve_sequence with.
                            self.shadow_check_native_row_candidate(
                                &target,
                                method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(
                                if matches!(method, "push" | "append" | "prepend" | "unshift") {
                                    updated_instance
                                } else {
                                    result
                                },
                            );
                            return Ok(());
                        }
                        // Non-mutating block list methods (`.first`/`.minmax`)
                        // dispatch through the same native helpers a *plain*
                        // array uses on the backing storage, so an `is Array`
                        // instance gets the same VM-native coverage instead of
                        // bouncing to the tree-walk interpreter (ledger §D /
                        // §C Phase-3). They borrow `&storage` and return a
                        // fresh value, so they never mutate the instance.
                        // `.grep` returns rw views into the source (a
                        // `for @s.grep { $_++ }` writes back), and
                        // `.splice`/`ASSIGN-POS`/… mutate, so those keep the
                        // fallback — they need the first-class element-cell
                        // write-back the interpreter owns. (`.map` used to be
                        // in this list; it is deferred now, and its rw loop
                        // runs from the Seq's pull — ADR-0058 §9.4.)
                        if let Some(r) = self.try_native_first(&storage, method, &args) {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            self.shadow_check_native_row_candidate(
                                &target,
                                method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        if let Some(r) = self.try_native_minmax(&storage, method, &args) {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            self.shadow_check_native_row_candidate(
                                &target,
                                method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        // Other non-mutating, non-rw-view list methods
                        // (`.sort`/`.reverse`/`.unique`/`.elems`/…) go through the
                        // umbrella native dispatch on the backing storage. Gated to
                        // a whitelist of methods that return fresh values (never an
                        // rw view into, nor a mutation of, the source), so the
                        // by-value `&storage` borrow is correct for the instance.
                        if Self::is_array_storage_native_safe(method)
                            && let Some(r) = self.try_native_method(&storage, method_sym, &args)
                        {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            self.shadow_check_native_row_candidate(
                                &target,
                                method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        // Perform the operation on the backing array
                        // TODO: compile to bytecode — Array-backed instance method
                        // (non-simple methods on `is Array` storage). See ledger §1.
                        crate::vm::vm_stats::record_method_fallback(method);
                        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
                        self.shadow_check_native_row_candidate(
                            &target,
                            method,
                            method_sym,
                            args.len(),
                            false,
                        );
                        // Seed the synthetic binding with the current storage
                        // BEFORE dispatching: methods like `ASSIGN-POS`/`BIND-POS`/
                        // `DELETE-POS` mutate by scanning `self.env` for a binding
                        // whose Array Arc pointer identity matches the receiver
                        // (`overwrite_array_bindings_by_identity`) rather than
                        // returning an updated value through `target_var`. A real
                        // named `@a.ASSIGN-POS(...)` call works because `@a` is
                        // already bound in `self.env` with that same Arc; without
                        // this seed, `"__mutsu_array_tmp"` was never in `self.env`
                        // at call time, so the identity scan found nothing and the
                        // mutation silently no-op'd.
                        self.env_mut()
                            .insert("__mutsu_array_tmp".to_string(), storage.clone());
                        let result = loan_env!(
                            self,
                            call_method_mut_with_values(
                                "__mutsu_array_tmp",
                                storage.clone(),
                                method,
                                args,
                            )
                        )
                        .or_else(|_| {
                            // Try non-mut dispatch for read-only methods
                            self.vm_call_method_with_values(storage.clone(), method, vec![])
                        })?;
                        // Read back the (potentially mutated) storage
                        if let Some(updated_storage) = self.env().get("__mutsu_array_tmp").cloned()
                        {
                            storage = updated_storage;
                        }
                        self.env_mut().remove("__mutsu_array_tmp");
                        // Update the instance with the new storage
                        self.write_back_array_storage_instance(
                            target_name,
                            &inst_class,
                            &attributes,
                            inst_id,
                            storage,
                        );
                        self.stack.push(result);
                        return Ok(());
                    }
                }
                // Hash-subclass instance delegation (mut path): the Associative
                // twin of the Array-subclass delegation block just above. See
                // `vm_hash_subclass_delegate.rs` for why this reuses the
                // existing native Hash dispatch (via a synthetic env binding)
                // instead of hand-written Rust mutators.
                // QuantHash-subclass instance delegation (mut path): the
                // `Set`/`Bag`/`Mix` twin, see `vm_baggy_subclass_delegate.rs`.
                if let Some(result) =
                    self.try_baggy_storage_delegate_mut(target_name, &target, method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.stack.push(result?);
                    return Ok(());
                }
                if let Some(result) =
                    self.try_hash_storage_delegate_mut(target_name, &target, method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // NOTE: No Nil absorber here for CallMethodMut. Unlike CallMethod
                // (which handles direct Nil.method calls), CallMethodMut targets
                // are from variables. Uninitialized variables in mutsu are Nil
                // (should be Any), so absorbing here would break methods like
                // .end, .elems, etc. on uninitialized containers.
                // The CallMethod path has the Nil absorber for direct Nil.method calls.
                // Slice 6.3: assume the dispatch dirties the caller env; only a
                // proven-pure compiled method path clears this.
                self.dispatch.method_dispatch_pure = false;
                if !skip_native
                    && !self.native_lever_a_user_override_sym(&target, method_sym)
                    && let Some(produced) =
                        self.try_quanthash_weight_pair_producer(&target, target_name, method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome(
                        "callmethodmut",
                        "quanthash-weight-pair-producer",
                    );
                    self.dispatch.method_dispatch_pure = true;
                    self.stack.push(produced);
                    return Ok(());
                }
                // ADR-0036 slice 3 / ADR-0045 slice 4: `.pairs`/`.kv`/
                // `.antipairs`/`.values`/`.reverse`/`.sort` on a real mutable
                // container hand out the elements' own `Scalar` containers, not
                // clones. This must run BEFORE the sentinel-resolved copy below,
                // which is a fresh Hash and therefore has no identity to
                // promote into. `try_element_container_producer` declines every
                // receiver that must keep the snapshot producer.
                if !skip_native
                    // An `augment`ed native type's own `.sort`/`.pairs`/... must
                    // still win: this routing changes how a *native* producer
                    // builds its result, and there is no native producer to
                    // change when the user has replaced the method.
                    && !self.native_lever_a_user_override_sym(&target, method_sym)
                    && let Some(produced) = self.try_element_container_producer(&target, method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome(
                        "callmethodmut",
                        "element-container-producer",
                    );
                    self.dispatch.method_dispatch_pure = true;
                    self.stack.push(produced);
                    return Ok(());
                }
                let call_result = if !skip_native {
                    if let Some(native_result) = self.try_native_method(&target, method_sym, &args)
                    {
                        // A native method reaching this tail returns a value and
                        // does not write the receiver back into env (mutating
                        // array/hash natives are handled by the dedicated
                        // writeback branches above and return early). So it is
                        // env-pure w.r.t. the caller -> no per-call locals pull.
                        self.dispatch.method_dispatch_pure = true;
                        crate::vm::vm_stats::record_dispatch_entry_outcome(
                            "callmethodmut",
                            "native",
                        );
                        self.shadow_check_native_row_candidate(
                            &target,
                            method,
                            method_sym,
                            args.len(),
                            true,
                        );
                        native_result
                    } else {
                        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
                        self.shadow_check_native_row_candidate(
                            &target,
                            method,
                            method_sym,
                            args.len(),
                            false,
                        );
                        self.dispatch_compiled_method_mut_with_raw_invocant(
                            code,
                            target_name,
                            target,
                            method,
                            method_sym,
                            args,
                        )
                    }
                } else {
                    // ADR-0019 E6b step 1: NOT shadow-checked here. `skip_native`
                    // means the arity cascade was deliberately never consulted for
                    // this call (a user-defined method override, a pseudo-method
                    // like WHAT/HOW, junction .gist, Stash AT-KEY, ...) -- there is
                    // no "did the cascade serve this call" outcome to compare the
                    // resolver's Native candidate against, unlike the genuine
                    // native/user completions above.
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
                    self.dispatch_compiled_method_mut_with_raw_invocant(
                        code,
                        target_name,
                        target,
                        method,
                        method_sym,
                        args,
                    )
                };
                match modifier {
                    Some("?") => match call_result {
                        Ok(val) => {
                            self.stack.push(val);
                        }
                        Err(e) if Self::is_method_not_found_error(&e) => {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "notfound",
                            );
                            self.stack.push(Value::NIL);
                        }
                        Err(e) => return Err(e),
                    },
                    _ => {
                        if let Err(e) = &call_result
                            && Self::is_method_not_found_error(e)
                        {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "notfound",
                            );
                        }
                        self.stack.push(call_result?);
                    }
                }
            }
        }
        Ok(())
    }

    /// Interpreter-native mutating list methods (`append`/`prepend`/`unshift`/`pop`/`shift`)
    /// on a plain, untyped `@`-array stored in env. Mirrors the interpreter's
    /// primary (`env.get_mut` + `Arc::make_mut`) branch in `methods_mut.rs`
    /// exactly for this narrow case, so the result is behavior-invariant.
    ///
    /// Returns:
    /// - `Some(Ok(v))` — handled natively (env already mutated); `v` is the
    ///   method's return value (the array for append/prepend/unshift, the removed
    ///   element for pop/shift).
    /// - `Some(Err(_))` — handled natively but errored.
    /// - `None` — not eligible; the caller must fall through to the interpreter.
    ///
    /// Intentionally conservative: bails out (returns `None`) for typed/shaped/
    /// lazy arrays, type-constrained or metadata-bearing containers, shared
    /// arrays, and any receiver that is not the exact array currently bound to
    /// `target_name` in env. Those richer semantics stay owned by the interpreter.
    /// Is `name` a plain lexical `@array` variable (sigil `@` immediately
    /// followed by an identifier char), as opposed to an attribute (`@!x` /
    /// `@.x`), a dynamic (`@*x`), or other twigil'd form? Only plain lexicals
    /// have a single shared identity across threads, so only they may be routed
    /// through the name-keyed atomic shared store.
    pub(crate) fn is_plain_lexical_array_name(name: &str) -> bool {
        let mut bytes = name.bytes();
        bytes.next() == Some(b'@')
            && matches!(bytes.next(), Some(c) if c.is_ascii_alphabetic() || c == b'_')
    }

    /// Sigil-agnostic form of `is_plain_lexical_array_name`: a plain lexical
    /// container variable (`@name` / `%name`) whose second character is an
    /// identifier start — i.e. not a twigil'd attribute (`@!`, `%.`), dynamic
    /// (`@*`), or other special form. Used to gate atomic-shared-store routing
    /// (those non-plain forms share a name across instances and must not funnel
    /// into the global name-keyed store).
    pub(crate) fn is_plain_lexical_name(name: &str) -> bool {
        let mut bytes = name.bytes();
        matches!(bytes.next(), Some(b'@') | Some(b'%'))
            && matches!(bytes.next(), Some(c) if c.is_ascii_alphabetic() || c == b'_')
    }

    /// Non-mutating, non-rw-view list methods that are safe to dispatch on an
    /// `is Array` instance's backing storage via `try_native_method` (which
    /// borrows the storage immutably and returns a fresh value). Excludes:
    /// `map`/`first`/`minmax` (handled by their own helpers above), `grep` (its
    /// result shares rw element cells with the source — `for @s.grep { $_++ }`
    /// must write back), and the mutating methods (`splice`/`ASSIGN-POS`/
    /// `DELETE-POS`/`BIND-POS`), which must update the instance via the fallback.
    fn is_array_storage_native_safe(method: &str) -> bool {
        matches!(
            method,
            "sort"
                | "reverse"
                | "rotate"
                | "unique"
                | "squish"
                | "flat"
                | "join"
                | "elems"
                | "end"
                | "List"
                | "Array"
                | "Seq"
                | "Slip"
                | "AT-POS"
                | "EXISTS-POS"
                | "head"
                | "tail"
        )
    }

    /// The simple array mutators (`push`/`pop`/`shift`/`unshift`/`append`/`prepend`,
    /// and `splice`) applied directly to an `is Array`-backed instance's backing
    /// storage `Value`: the row's `ReceiverPlace::Detached` form, so the same
    /// handler that answers `@a.push` answers it (ADR-11276 §9.23). `storage` is
    /// mutated in place through its shared node and the method's result value
    /// is returned; the caller writes the instance back.
    ///
    /// `pub(crate)`: also reused by the `nextsame`/`callsame` synthesized native
    /// fallback (`native_array_storage_base` in
    /// `runtime/builtins_dispatch_next.rs`) so a deferred call from a user
    /// override reaches the same mutation as the direct `$a.push(...)` path,
    /// instead of silently no-op'ing through the non-mutating
    /// `try_native_method` dispatch.
    // Cost: see the row's handler (`method_table::mutating::array`).
    pub(crate) fn native_array_storage_mut(
        &mut self,
        storage: &mut Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let mut place = crate::builtins::method_table::ReceiverPlace::detached(storage);
        crate::builtins::method_table::invoke_mut(
            self,
            &mut place,
            crate::symbol::Symbol::intern(method),
            args,
        )
    }

    /// Rebuild an `is Array`-backed instance with its `__mutsu_array_storage`
    /// attribute replaced by `storage` and write it back into `target_name`.
    /// Shared by the Interpreter-native mutator fast path and the interpreter fallback.
    fn write_back_array_storage_instance(
        &mut self,
        target_name: &str,
        inst_class: &Symbol,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        inst_id: u64,
        storage: Value,
    ) -> Value {
        let new_attrs = crate::value::InstanceAttrs::clone(attributes);
        new_attrs.insert("__mutsu_array_storage".to_string(), storage);
        let updated_instance = Value::instance_parts(
            *inst_class,
            crate::gc::Gc::new(crate::value::InstanceAttrs::new(
                *inst_class,
                new_attrs.to_map(),
                inst_id,
                true,
            )),
            inst_id,
        );
        // Through the shared cell, not over it: see `Env::insert_through`.
        self.env_mut()
            .insert_through(target_name.to_string(), updated_instance.clone());
        updated_instance
    }

    /// Interpreter-native mutating Buf write methods (`write-bits`/`write-ubits`/
    /// `write-num*`/`write-int*`/`write-uint*`) on a mutable `Buf` instance bound
    /// to `target_name` (ledger §1: native receiver dispatch -> Interpreter-native). Mirrors
    /// the interpreter's instance-mutate branches in `methods_mut.rs` exactly: the
    /// byte transforms are the single shared pure implementations in `builtins/`
    /// (`buf_bits`/`buf_write_num`/`buf_write_int`), and the writeback goes
    /// straight into the receiver's shared cell (`Value::write_back_sharing`) so
    /// aliases of the same buf observe the mutation — so the result is
    /// behavior-invariant.
    ///
    /// Returns `None` (fall through to the interpreter) for type-object receivers
    /// (`buf8.write-...` on the type returns a fresh buf), immutable `Blob`, and
    /// malformed arity/arguments, leaving the interpreter to own those
    /// error/construction semantics.
    fn try_native_buf_mut(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let is_write_bits = matches!(method, "write-ubits" | "write-bits");
        let is_write_num = crate::builtins::buf_write_num::write_num_size(method).is_some();
        let is_write_int = crate::builtins::buf_write_int::write_int_info(method).is_some();
        if !(is_write_bits || is_write_num || is_write_int) {
            return None;
        }
        let ValueView::Instance {
            class_name,
            attributes,
            id,
        } = target.view()
        else {
            return None;
        };
        let cn = class_name.resolve();
        if !crate::runtime::utils::is_buf_or_blob_class(&cn) {
            return None;
        }
        // Immutable Blob: let the interpreter raise "Cannot modify immutable Blob".
        if crate::runtime::utils::is_blob_like_class(&cn) && !cn.starts_with("utf") {
            return None;
        }
        let mut bytes = crate::value::value_buf::buf_raw_bytes_or_empty(&attributes);
        // Compute the new bytes via the shared pure transform.
        let new_bytes: Vec<u8> = if is_write_bits {
            if args.len() != 3 {
                return None; // interpreter handles non-3-arg forms
            }
            let (Some(from), Some(bits)) = (
                crate::runtime::Interpreter::value_to_non_negative_i64(&args[0]),
                crate::runtime::Interpreter::value_to_non_negative_i64(&args[1]),
            ) else {
                return None; // let the interpreter raise the offset/bits parse error
            };
            match crate::builtins::buf_bits::write_bits(&bytes, from, bits, &args[2]) {
                Ok(b) => b,
                Err(e) => return Some(Err(e)),
            }
        } else {
            // write-num* / write-int*: 2 or 3 args (interpreter raises on others).
            if args.len() < 2 || args.len() > 3 {
                return None;
            }
            let offset_i64 = match args[0].view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                _ => 0,
            };
            let endian_val = if args.len() == 3 {
                crate::builtins::buf_write_num::decode_endian(&args[2])
            } else {
                0
            };
            let width = crate::value::value_buf::buf_elem_width(&cn);
            let res = if is_write_num {
                crate::builtins::buf_write_num::apply_write_num(
                    &mut bytes, method, offset_i64, &args[1], endian_val, width,
                )
            } else {
                crate::builtins::buf_write_int::apply_write_int(
                    &mut bytes, method, offset_i64, &args[1], endian_val, width,
                )
            };
            if let Err(e) = res {
                return Some(Err(e));
            }
            bytes
        };
        // Write the updated bytes straight into the receiver's live shared cell
        // (so aliases observing the same buf see the mutation), then refresh the
        // receiver binding to match the interpreter's `env.insert_through(target_var, ...)`.
        let mut updated_attrs = attributes.to_map();
        crate::value::value_buf::set_buf_raw_bytes(&mut updated_attrs, class_name, new_bytes);
        let updated = Value::write_back_sharing(&attributes, class_name, updated_attrs, id);
        self.env_mut()
            .insert_through(target_name.to_string(), updated.clone());
        Some(Ok(updated))
    }
}
