//! The **light closure call** (#8335).
//!
//! `call_compiled_closure_in_unit` pays, on every call of every closure, for
//! what a closure body *might* do: it pushes a full call frame and a caller
//! frame, installs `&?BLOCK`, the callable id, `@_` and the parameters into a
//! fresh env overlay, force-installs the captured bindings a by-name read must
//! see, snapshots the free variables, and on return scans the overlay to write
//! the body's mutations back to the caller. For `-> $a { $a + 1 }` none of that
//! is observable, and it cost ~12k instructions a call against ~4k for the
//! same body as a named sub.
//!
//! This module runs such a body with only what it can observe. Whether a body
//! qualifies is a property of its bytecode, settled once per code object
//! ([`LightClosureShape::of`]): every op must come from a short list of ops
//! that read their operands from the stack, a local slot or the closure's
//! upvalue array and nothing else — no by-name read or write, no call, no
//! declaration, no control exception, no `$_`. Such a body cannot reach the
//! env overlay, the call frames or the caller's variables, so there is nothing
//! to install, snapshot or write back. What remains is what any value op can
//! still reach indirectly (a user `infix:<+>`, a `.Str` on an object): such a
//! callee runs on its own capture over the caller's env, as a callee of a
//! positional light call does — no frame of the closure's sits between them,
//! since the body has nothing of its own to put in one.
//!
//! The call itself is checked too ([`Interpreter::try_call_closure_light`]):
//! the arguments must bind as the light binds (`closure_light_bind`,
//! `closure_light_def_bind`) would bind them, and each bound value must be one
//! `GetLocal` reads straight out of its slot, without the by-name probes it
//! makes for a `Nil` or a container. Anything else declines, and the caller
//! takes the untyped entry.

use super::vm_closure_light_def_bind::light_def_arg_is_plain_value;
use super::*;

/// A closure body the light call can run, and what the call must supply.
#[derive(Debug, Clone)]
pub(crate) struct LightClosureShape {
    /// The upvalue indices the body reads. Each must be a captured cell or
    /// value of the closure being called; a missing one is read by name on
    /// the untyped entry.
    upvalues: Box<[u32]>,
}

impl LightClosureShape {
    /// The shape of `cc`'s body, or `None` when the light call cannot run it.
    // Cost: O(n + l), n = ops, l = locals.
    pub(crate) fn of(cc: &CompiledCode) -> Option<Self> {
        if !cc.is_pointy_block
            || cc.is_routine
            || cc.is_supply_block_body
            || cc.static_name_lookup
            || cc.has_calls
            || cc.has_env_writes
            || cc.uses_capture
            || cc.reads_args_array
            || cc.reads_args_hash
            || cc.reads_topic
            || cc.writes_topic
            || cc.uses_routine_state
            || cc.sets_routine_state
            || !cc.state_locals.is_empty()
            || !cc.free_var_writes.is_empty()
            || !cc.free_var_container_writes.is_empty()
        {
            return None;
        }
        // Every local is a parameter (checked per call against the signature),
        // and every read of one is the plain slot read.
        for (i, name) in cc.locals.iter().enumerate() {
            if name.is_empty() || name.starts_with('!') || !cc.local_read_plain(i) {
                return None;
            }
        }
        let mut upvalues = Vec::new();
        for op in &cc.ops {
            match op {
                OpCode::GetUpvalue { index, .. } => upvalues.push(*index),
                OpCode::GetLocal(idx) if (*idx as usize) < cc.locals.len() => {}
                op if Self::op_is_pure(op) => {}
                _ => return None,
            }
        }
        upvalues.sort_unstable();
        upvalues.dedup();
        Some(LightClosureShape {
            upvalues: upvalues.into_boxed_slice(),
        })
    }

    /// The ops that take their operands from the stack and push a result,
    /// touching no variable, frame or control flow outside the body.
    fn op_is_pure(op: &OpCode) -> bool {
        matches!(
            op,
            OpCode::LoadConst(_)
                | OpCode::LoadNil
                | OpCode::LoadTrue
                | OpCode::LoadFalse
                | OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Mod
                | OpCode::Pow
                | OpCode::Negate
                | OpCode::IntDiv
                | OpCode::IntMod
                | OpCode::InfixMin
                | OpCode::InfixMax
                | OpCode::Not
                | OpCode::BoolCoerce
                | OpCode::NumCoerce
                | OpCode::StrCoerce
                | OpCode::Concat
                | OpCode::NumEq
                | OpCode::NumNe
                | OpCode::NumLt
                | OpCode::NumLe
                | OpCode::NumGt
                | OpCode::NumGe
                | OpCode::StrEq
                | OpCode::StrNe
                | OpCode::StrLt
                | OpCode::StrGt
                | OpCode::StrLe
                | OpCode::StrGe
                | OpCode::Spaceship
                | OpCode::Cmp
                | OpCode::Leg
                | OpCode::BitAnd
                | OpCode::BitOr
                | OpCode::BitXor
                | OpCode::BitShiftLeft
                | OpCode::BitShiftRight
                | OpCode::IntBitNeg
                | OpCode::Jump(_)
                | OpCode::JumpIfFalse(_)
                | OpCode::JumpIfTrue(_)
                | OpCode::Dup
                | OpCode::Pop
        )
    }
}

/// How a qualifying call's arguments bind (`try_call_closure_light`).
#[derive(Clone, Copy)]
enum LightBind<'a> {
    /// A bare-identifier signature: `closure_light_bind`'s item binding.
    Legacy,
    /// A plain `ParamDef` signature: `closure_light_def_bind`'s.
    Def(&'a [crate::vm::LightDefParam]),
}

impl Interpreter {
    /// Run the closure `data` light (see the module doc), or answer `None`
    /// with nothing changed when the body or the call does not qualify.
    // Cost: O(p + u), p = parameters, u = upvalues the body reads; plus the
    // body itself.
    pub(super) fn try_call_closure_light(
        &mut self,
        data: &crate::gc::Gc<crate::value::SubData>,
        cc: &CompiledCode,
        args: &[Value],
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        let shape = cc
            .light_closure_shape
            .get_or_init(|| LightClosureShape::of(cc));
        let shape = shape.as_ref()?;
        let param_syms = data.param_name_syms();
        let sources_any = self.pending_call_arg_sources().is_some_and(|sources| {
            sources.len() == args.len() && sources.iter().any(Option::is_some)
        });
        let bind = self.closure_light_admits(data, cc, shape, param_syms, args, sources_any)?;
        let base = self.bind_closure_light(bind, args.len(), |_, i| args[i].clone())?;
        Some(self.run_closure_light(data, cc, shape, base, compiled_fns))
    }

    /// The `CallOnValue` door: the target is `stack[start - 1]` and the
    /// arguments `stack[start..]`. On `Some`, both are consumed; `None` leaves
    /// the stack as it was, for the ordinary value call. Running here, before
    /// the call site decodes its argument sources, is what spares a light call
    /// that decode: such a body binds no `is rw` parameter, so nothing reads
    /// them.
    // Cost: O(p + u + a), as `try_call_closure_light`, a = the call site's
    // argument descriptors.
    pub(super) fn try_value_call_closure_light(
        &mut self,
        code: &CompiledCode,
        start: usize,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Sub(data) = self.stack.get(start.checked_sub(1)?)?.view() else {
            return None;
        };
        let cc = data.compiled_code.as_ref()?;
        let shape = cc
            .light_closure_shape
            .get_or_init(|| LightClosureShape::of(cc));
        shape.as_ref()?;
        let (data, cc) = (data.clone(), cc.clone());
        let shape = cc.light_closure_shape.get()?.as_ref()?;
        // The checks the value call makes on the way to the compiled closure
        // entry: a wrapped closure runs its wrappers, a multi's dispatcher
        // value dispatches, and an assignment through the call wants the
        // container the untyped entry hands back.
        let arity = self.stack.len() - start;
        if self.in_lvalue_assignment
            || data.is_decl_expr_thunk
            || self.has_wrap_chain(data.id)
            || data
                .env
                .contains_key_sym(crate::symbol::well_known::multi_dispatch_candidates())
            || (arity > 0 && Self::sub_multi_method_dispatcher_name(&data).is_some())
        {
            return None;
        }
        let kinds = Self::stack_arg_kinds(code, arg_sources_idx);
        if kinds.named || kinds.slip {
            return None;
        }
        let sources_any = Self::arg_sources_name_any(code, arg_sources_idx);
        let param_syms = data.param_name_syms();
        let bind = self.closure_light_admits(
            &data,
            &cc,
            shape,
            param_syms,
            &self.stack[start..],
            sources_any,
        )?;
        let base = self.bind_closure_light(bind, arity, |me, i| me.stack[start + i].clone())?;
        self.stack.truncate(start - 1);
        // The closure entry's own unit switch (`call_compiled_closure_with_topic`)
        // and `use fatal` scope (`call_compiled_closure_at`).
        let unit = self.unit_of_source_sym(data.source_file_sym());
        let saved_unit = std::mem::replace(&mut self.current_unit, unit);
        let mut guard = crate::vm::vm_call_state_guard::PragmaGuard::new(self);
        guard.module.fatal_mode = data.captured_fatal_mode;
        guard.module.lexical_fatal_mode = data.captured_fatal_mode;
        let result = guard.run_closure_light(&data, &cc, shape, base, compiled_fns);
        let result = if guard.module.fatal_mode {
            result.and_then(
                |val| match guard.failure_to_runtime_error_if_unhandled(&val) {
                    Some(err) => Err(err),
                    None => Ok(val),
                },
            )
        } else {
            result
        };
        drop(guard);
        self.current_unit = saved_unit;
        Some(result)
    }

    /// Whether the call qualifies, and how its arguments bind. Reads only.
    fn closure_light_admits<'p>(
        &self,
        data: &crate::value::SubData,
        cc: &CompiledCode,
        shape: &LightClosureShape,
        param_syms: &'p crate::value::ParamNameSyms,
        args: &[Value],
        sources_any: bool,
    ) -> Option<LightBind<'p>> {
        // Per-process latches the slot read and the upvalue read consult by
        // name once set, and the lvalue context a `GetLocal` serves a
        // container for.
        if !crate::vm::vm_jit::local_read_unspoiled()
            || self.rw_return_context
            || !self.lexicals.escaping_our_lexical_names.is_empty()
            || !data.assumed_positional.is_empty()
            || !data.assumed_named.is_empty()
            || args.len() != cc.locals.len()
        {
            return None;
        }
        for index in shape.upvalues.iter() {
            if !matches!(data.upvalues.get(*index as usize), Some(Some(_))) {
                return None;
            }
        }
        // A return spec captured from the enclosing routine applies to the
        // block's value on the untyped entry.
        if data
            .env
            .get_sym(crate::symbol::well_known::return_type())
            .is_some()
        {
            return None;
        }
        let bind = if param_syms.light_bindable {
            LightBind::Legacy
        } else {
            LightBind::Def(param_syms.light_def_params.as_deref()?)
        };
        // The parameters are the frame, in order.
        let arity = match bind {
            LightBind::Legacy => param_syms.params.len(),
            LightBind::Def(params) => params.len(),
        };
        if arity != args.len() {
            return None;
        }
        for i in 0..arity {
            let sym = match bind {
                LightBind::Legacy => param_syms.params[i],
                LightBind::Def(params) => params[i].sym(),
            };
            if cc.local_sym(i) != Some(sym) {
                return None;
            }
        }
        let admitted = match bind {
            // `closure_light_def_bind`'s argument gate.
            LightBind::Def(_) => args.iter().all(light_def_arg_is_plain_value) && !sources_any,
            // `closure_light_bind`'s.
            LightBind::Legacy => !args.iter().any(|a| {
                matches!(
                    a.unwrap_varref().view(),
                    ValueView::Pair(..) | ValueView::ValuePair(..)
                )
            }),
        };
        admitted.then_some(bind)
    }

    /// Open the closure's frame and bind the arguments into it. A value
    /// `GetLocal` would not read straight out of its slot declines (the frame
    /// is closed again), before anything else is touched.
    fn bind_closure_light(
        &mut self,
        bind: LightBind<'_>,
        arity: usize,
        arg_at: impl Fn(&Self, usize) -> Value,
    ) -> Option<crate::runtime::locals::CallerFrame> {
        let base = self.locals.push_frame(arity);
        for i in 0..arity {
            let arg = arg_at(self, i);
            let val = match bind {
                LightBind::Def(params) => Self::apply_scalar_param_bind(params[i].bind(), arg),
                LightBind::Legacy => Self::itemize_scalar_store_value(
                    crate::runtime::types::unwrap_varref_value(arg).hllize_vm_collection(),
                ),
            };
            if !val.is_plain_local_read() {
                self.locals.pop_frame(base);
                return None;
            }
            self.locals[i] = val;
        }
        Some(base)
    }

    fn run_closure_light(
        &mut self,
        data: &crate::gc::Gc<crate::value::SubData>,
        cc: &CompiledCode,
        shape: &LightClosureShape,
        locals_base: crate::runtime::locals::CallerFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        // GC safepoint (§9.2a `call`): this path skips `push_call_frame`.
        crate::vm::vm_poll::poll(crate::gc::SafepointKind::Call, 0);
        // What the binders consume on every call, so a later writeback in the
        // caller's frame cannot pick this call's sources up.
        self.pending_rw_writeback_sources.clear();
        self.take_pending_call_arg_sources();
        self.pending_call_arg_source_slots.clear();
        self.static_call_args = false;

        let saved_line = self.cur_source_line;
        let saved_stack_depth = self.stack.len();
        let saved_upvalues = (!shape.upvalues.is_empty())
            .then(|| std::mem::replace(&mut self.upvalues, data.upvalues.clone()));
        let routine_base = self.routine_stack_len();
        self.push_block_routine_with_location(
            data.package,
            crate::symbol::wk::pointy_block(),
            self.current_source_line(),
            self.executing_source_file_sym(),
            data.source_file_sym(),
        );

        // Restore everything above on every exit, a Rust panic in the body
        // included (as the positional light call does).
        let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let mut ip = 0;
            let polls = crate::vm::vm_poll::DispatchPolls::current();
            while ip < cc.ops.len() {
                let step = if ip == 0
                    && let Some(r) = crate::vm::vm_jit::try_enter(self, cc, compiled_fns)
                {
                    ip = cc.ops.len();
                    r
                } else {
                    self.exec_one_backedge_polled(cc, &mut ip, compiled_fns, polls)
                };
                step?;
                if self.is_halted() {
                    break;
                }
            }
            Ok(())
        }));
        let result = match outcome {
            Ok(Ok(())) if self.stack.len() > saved_stack_depth => {
                Ok(self.stack.pop().unwrap_or(Value::NIL))
            }
            Ok(Ok(())) => Ok(Value::NIL),
            Ok(Err(e)) => Err(e),
            Err(payload) => {
                self.finish_closure_light(
                    saved_stack_depth,
                    routine_base,
                    saved_upvalues,
                    locals_base,
                    saved_line,
                );
                std::panic::resume_unwind(payload);
            }
        };
        self.finish_closure_light(
            saved_stack_depth,
            routine_base,
            saved_upvalues,
            locals_base,
            saved_line,
        );
        result
    }

    fn finish_closure_light(
        &mut self,
        saved_stack_depth: usize,
        routine_base: usize,
        saved_upvalues: Option<Vec<Option<Value>>>,
        locals_base: crate::runtime::locals::CallerFrame,
        saved_line: i64,
    ) {
        self.stack.truncate(saved_stack_depth.min(self.stack.len()));
        self.truncate_routine_stack(routine_base);
        if let Some(upvalues) = saved_upvalues {
            self.upvalues = upvalues;
        }
        self.locals.pop_frame(locals_base);
        self.cur_source_line = saved_line;
    }
}
