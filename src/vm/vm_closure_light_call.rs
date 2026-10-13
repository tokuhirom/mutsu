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
//! still reach indirectly (a user `infix:<+>`, a `.Str` on an object): those
//! run under an env whose parent is the caller's and whose fallback tier is
//! the closure's capture, exactly as on the untyped entry.
//!
//! The call itself is checked too ([`Interpreter::try_call_closure_light`]):
//! the arguments must bind as the light binds (`closure_light_bind`,
//! `closure_light_def_bind`) would bind them, and each bound value must be one
//! `GetLocal` reads straight out of its slot, without the by-name probes it
//! makes for a `Nil` or a container. Anything else declines, and the caller
//! takes the untyped entry.

use super::*;
use super::vm_closure_light_def_bind::light_def_arg_is_plain_value;

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
        let shape = cc.light_closure_shape.get_or_init(|| LightClosureShape::of(cc));
        let shape = shape.as_ref()?;
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
        let param_syms = data.param_name_syms();
        let def_params = param_syms.light_def_params.as_deref();
        let arity = if param_syms.light_bindable {
            param_syms.params.len()
        } else {
            def_params?.len()
        };
        if arity != args.len() {
            return None;
        }
        // The parameters are the frame, in order.
        for i in 0..arity {
            let sym = match def_params {
                Some(params) if !param_syms.light_bindable => params[i].sym(),
                _ => param_syms.params[i],
            };
            if cc.local_sym(i) != Some(sym) {
                return None;
            }
        }
        if !param_syms.light_bindable {
            // `closure_light_def_bind`'s argument gate.
            if !args.iter().all(light_def_arg_is_plain_value)
                || self.pending_call_arg_sources().is_some_and(|sources| {
                    sources.len() == args.len() && sources.iter().any(Option::is_some)
                })
            {
                return None;
            }
        } else if args.iter().any(|a| {
            matches!(
                a.unwrap_varref().view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            )
        }) {
            return None;
        }
        // Bind into a frame of their own; a value `GetLocal` would not read
        // straight out of the slot declines before anything is installed.
        let base = self.locals.push_frame(arity);
        for (i, arg) in args.iter().enumerate() {
            let val = match def_params {
                Some(params) if !param_syms.light_bindable => {
                    Self::apply_scalar_param_bind(params[i].bind(), arg.clone())
                }
                _ => Self::itemize_scalar_store_value(
                    crate::runtime::types::unwrap_varref_value(arg.clone())
                        .hllize_vm_collection(),
                ),
            };
            if !val.is_plain_local_read() {
                self.locals.pop_frame(base);
                return None;
            }
            self.locals[i] = val;
        }
        Some(self.run_closure_light(data, cc, shape, base, compiled_fns))
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
        // The env a value op's callee resolves names through: the caller's,
        // with the closure's capture as the fallback tier.
        let mut frame = crate::env::Env::scoped_child(self.env().clone());
        frame.set_capture_fallback(data.env.capture_view());
        let caller_env = std::mem::replace(self.env_mut(), frame);
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
                    caller_env,
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
            caller_env,
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
        caller_env: crate::env::Env,
        saved_line: i64,
    ) {
        self.stack.truncate(saved_stack_depth.min(self.stack.len()));
        self.truncate_routine_stack(routine_base);
        if let Some(upvalues) = saved_upvalues {
            self.upvalues = upvalues;
        }
        self.locals.pop_frame(locals_base);
        *self.env_mut() = caller_env;
        self.cur_source_line = saved_line;
    }
}
