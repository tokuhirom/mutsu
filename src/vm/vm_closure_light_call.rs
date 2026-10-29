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
//! The bodies are those of pointy blocks, of WhateverCodes (`* + 1`, whose
//! single `*` is the topic parameter `_`) and of placeholder blocks
//! (`{ $^a + $^b }`) (#13286).
//!
//! The call itself is checked too ([`Interpreter::try_call_closure_light`]):
//! the arguments must bind as the light binds (`closure_light_bind`,
//! `closure_light_def_bind`) would bind them, and each bound value must be one
//! `GetLocal` reads straight out of its slot, without the by-name probes it
//! makes for a `Nil` or a container. Anything else declines, and the caller
//! takes the untyped entry.

use super::vm_closure_light_def_bind::light_def_arg_is_plain_value;
use super::*;
use crate::light_closure_shape::LightClosureShape;
use crate::value::LightCallLegacy;

/// How a qualifying call's arguments bind (`try_call_closure_light`).
#[derive(Clone, Copy)]
enum LightBind<'a> {
    /// A bare-identifier signature: `closure_light_bind`'s item binding.
    Legacy,
    /// A plain `ParamDef` signature: `closure_light_def_bind`'s.
    Def(&'a [crate::vm::LightDefParam]),
    /// `$^name` placeholders: the general binder's `ParamDef` path, which
    /// itemizes each as a plain `$x` (#13286).
    Placeholders,
    /// A single-`*` WhateverCode's topic parameter `_`, bound as the argument
    /// is (#13286).
    Topic,
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
            match param_syms.light_call_legacy {
                Some(LightCallLegacy::Placeholders) => LightBind::Placeholders,
                Some(LightCallLegacy::Topic) if cc.is_whatever_code => LightBind::Topic,
                Some(LightCallLegacy::Topic) => return None,
                None => LightBind::Def(param_syms.light_def_params.as_deref()?),
            }
        };
        // The parameters are the frame, in order.
        let arity = match bind {
            LightBind::Def(params) => params.len(),
            _ => param_syms.params.len(),
        };
        if arity != args.len() {
            return None;
        }
        for i in 0..arity {
            let sym = match bind {
                LightBind::Def(params) => params[i].sym(),
                _ => param_syms.params[i],
            };
            if cc.local_sym(i) != Some(sym) {
                return None;
            }
            // The body runs on the caller's env overlay, so a container the
            // caller holds under the parameter's name (a `for @a` loop's `$_`
            // cell, under a WhateverCode's `_`) is one this frame's slot does
            // not hold: the env/slot invariant `GetLocal` reads the slot under
            // (ADR-0097 §15) would not hold.
            if self.env().overlay_get_sym(sym).is_some_and(|v| {
                matches!(
                    v.view(),
                    ValueView::ContainerRef(_) | ValueView::Proxy { .. }
                )
            }) {
                return None;
            }
        }
        let admitted = match bind {
            // `closure_light_def_bind`'s argument gate, less its source-name
            // half for a plain parameter: a source hands a plain parameter
            // the caller variable's type constraint, which only an assignment
            // reads, and the body assigns nothing. An `is raw` parameter
            // aliases the variable itself, which a callee the body reaches
            // (a user `infix:<+>`) could write between two reads.
            LightBind::Def(params) => {
                args.iter().all(light_def_arg_is_plain_value)
                    && !(sources_any
                        && params
                            .iter()
                            .any(|p| p.bind() == crate::vm::ScalarParamBind::Keep))
            }
            // A placeholder takes the general binder's `ParamDef` path, as
            // a plain `$` parameter does above (a block's `Mu` parameter, so
            // a Junction binds whole and declines here only as a non-plain
            // value).
            LightBind::Placeholders => args.iter().all(light_def_arg_is_plain_value),
            // `closure_light_bind`'s. A Junction binds whole there as well.
            LightBind::Legacy => !args.iter().any(|a| {
                matches!(
                    a.unwrap_varref().view(),
                    ValueView::Pair(..) | ValueView::ValuePair(..)
                )
            }),
            // A WhateverCode's `_` is `Any`-typed: the full entry autothreads
            // a Junction over the call, which the light call does not do.
            LightBind::Topic => !args.iter().any(|a| {
                matches!(
                    a.unwrap_varref().view(),
                    ValueView::Pair(..) | ValueView::ValuePair(..) | ValueView::Junction { .. }
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
                LightBind::Placeholders => Self::itemize_scalar_store_value(arg),
                LightBind::Legacy => Self::itemize_scalar_store_value(
                    crate::runtime::types::unwrap_varref_value(arg).hllize_vm_collection(),
                ),
                LightBind::Topic => {
                    crate::runtime::types::unwrap_varref_value(arg).hllize_vm_collection()
                }
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
        // The frame the full entry pushes: a pointy block's carries the
        // marker `&?ROUTINE` skips, a WhateverCode's its own name.
        let frame_name = if cc.is_pointy_block {
            crate::symbol::wk::pointy_block()
        } else {
            data.name
        };
        self.push_block_routine_with_location(
            data.package,
            frame_name,
            self.current_source_line(),
            self.executing_source_file_sym(),
            data.source_file_sym(),
        );
        self.attach_closure_fixtures(data);

        // Restore everything above on every exit, a Rust panic in the body
        // included (as the positional light call does).
        let frame_caller_code = self.current_code;
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
        self.current_code = frame_caller_code;
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
