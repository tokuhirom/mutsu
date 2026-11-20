//! Slot method frame, stage 1 prototype (ADR-18393, #18393).
//!
//! A planned lean method call (`vm_method_lean_frame`) whose body provably
//! never reaches the frame's name-keyed state runs here without that state:
//! no `Env` overlay (`Env::scoped_child`, the saved caller handle and its
//! drop), no `$CALLER::` alias save/restore, no class-body statics or
//! parameters copied into an overlay by name, no readonly marks for its
//! parameters, and no per-call `method_body_package` / `state` scope.
//!
//! **Prototype only.** It is off unless `MUTSU_SLOT_FRAME=1` is set, so the
//! shipped behavior is unchanged; it exists to measure what the frame shape
//! of ADR-18393 is worth before the staged PRs are written.
//!
//! Soundness rests on two halves, one decided once per plan and one per call:
//!
//! - [`slot_frame_body_ok`] (per plan): every op of the body is one of a
//!   short list of slot/constant/arithmetic ops whose execution reads no
//!   name and writes no name, every local the body reads is a plain local
//!   (`local_read_plain`), the body declares nothing, calls nothing and
//!   creates no closure, and no frame-plan slot is seeded from the env by
//!   name. An unknown op makes the plan ineligible, so a new op can only
//!   make this frame rarer, never wrong.
//! - [`Interpreter::slot_frame_entry_ok`] (per call): the process-wide
//!   latches the `GetLocal` fast path depends on are still clear and no
//!   `$CALLER::` alias is live, so every `GetLocal` the body runs takes the
//!   slot-only fast path. A refusal runs the ordinary lean frame.

use super::*;
use crate::vm::vm_method_frame_plan::LocalInit;

/// Whether the prototype is switched on (`MUTSU_SLOT_FRAME=1`).
// Cost: O(1).
pub(super) fn slot_frame_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| {
        matches!(
            std::env::var("MUTSU_SLOT_FRAME").ok().as_deref(),
            Some("1") | Some("2")
        )
    })
}

/// Whether the slim-record measurement is switched on (`MUTSU_SLOT_FRAME=2`).
// Cost: O(1).
fn slot_frame_slim() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_SLOT_FRAME").ok().as_deref() == Some("2"))
}

/// Whether the body of `def` (compiled as `cc`) can run in a slot frame:
/// see the module docs. Decided once per plan.
// Cost: O(n + l), n = ops, l = local slots.
pub(super) fn slot_frame_body_ok(
    def: &crate::runtime::MethodDef,
    cc: &CompiledCode,
    owner: Symbol,
) -> bool {
    if !slot_frame_enabled()
        // `reconcile_captured_readonly_ex` has work only for a code that
        // writes a free variable or an `is rw` method; the op list below
        // admits no write at all.
        || def.is_rw
        || !cc.free_var_writes.is_empty()
        || !cc.free_var_container_writes.is_empty()
        || !cc.nested_sub_written_free.is_empty()
        || cc.static_name_lookup
        || cc.needs_reflective_capture
        || cc.reads_topic
        || cc.writes_topic
        || cc.uses_routine_state
        || cc.sets_routine_state
    {
        return false;
    }
    let ops_ok = cc.ops.iter().all(|op| match op {
        OpCode::LoadConst(_)
        | OpCode::LoadNil
        | OpCode::LoadTrue
        | OpCode::LoadFalse
        | OpCode::Add
        | OpCode::Sub
        | OpCode::Mul => true,
        OpCode::GetLocal(idx) => cc.local_read_plain(*idx as usize),
        _ => false,
    });
    ops_ok
        && def
            .frame_plan(owner, cc)
            .locals
            .iter()
            .all(|init| !matches!(init, LocalInit::Outer))
}

impl Interpreter {
    /// The per-call half of the slot frame's admission (module docs).
    // Cost: O(1).
    #[inline]
    pub(super) fn slot_frame_entry_ok(&self) -> bool {
        crate::vm::vm_jit::local_read_unspoiled() && self.lexicals.var_bindings.is_empty()
    }

    /// Run a planned lean call in a slot frame. The caller checked
    /// [`Self::lean_args_ok`] and [`Self::slot_frame_entry_ok`].
    // Cost: O(p + l), p = parameters, l = local slots, plus the body.
    pub(super) fn call_compiled_method_slot(
        &mut self,
        call: &super::vm_method_lean_frame::LeanMethodCall<'_>,
        args: Vec<Value>,
        base: Value,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        let unit = self.unit_of_source_sym(call.cc.source_file);
        let saved_unit = std::mem::replace(&mut self.current_unit, unit);
        let result = self.call_compiled_method_slot_in_unit(call, args, base);
        self.current_unit = saved_unit;
        result
    }

    fn call_compiled_method_slot_in_unit(
        &mut self,
        call: &super::vm_method_lean_frame::LeanMethodCall<'_>,
        args: Vec<Value>,
        base: Value,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        crate::guard_native_stack!(
            self,
            self.call_compiled_method_slot_in_unit(call, args, base)
        );
        let &super::vm_method_lean_frame::LeanMethodCall {
            owner_sym,
            method_sym,
            method_def,
            cc,
            compiled_fns,
            ..
        } = call;
        self.pending_skip_constraint_recheck = false;
        let owner_class: &str = owner_sym.as_str();
        let (role_attrs_cell, inner_attrs_cell) =
            Self::method_attr_cells_for(&base, owner_class, false);
        let attrs_cell = role_attrs_cell.or_else(|| inner_attrs_cell.clone());
        let fallback_attrs_cell = inner_attrs_cell.filter(|cell| {
            attrs_cell.as_ref().is_some_and(|primary| {
                crate::gc::Gc::as_ptr(primary) != crate::gc::Gc::as_ptr(cell)
            })
        });
        let _mark_context_guard = crate::vm::vm_call_state_guard::MarkContextGuard::new(self);

        // The frame record: slots window, stack depth, caller code, line.
        // No env overlay: the body reads and writes no name (module docs).
        // `MUTSU_SLOT_FRAME=2` measures the slim record of D1 as an upper
        // bound: only the slot window and the stack depth, no `VmCallFrame`.
        // Prototype only -- a real slim record stays on the call-frame
        // stack so panic recovery and `CALLER::` from a callee can find it;
        // this body makes no call and cannot be observed from one.
        let slim = slot_frame_slim();
        let mut slim_record = None;
        let saved_stack_depth = if slim {
            crate::vm::vm_poll::poll(crate::gc::SafepointKind::Call, 0);
            slim_record = Some((self.locals.push_frame(0), self.cur_source_line));
            self.stack.len()
        } else {
            self.push_light_call_frame();
            self.call_frames.last().unwrap().saved_stack_depth
        };
        self.push_method_class_sym(owner_sym, Some(false));

        // Bind into slots: the invocant, then each positional argument.
        let class_val = Value::package(owner_sym);
        self.discard_pending_call_arg_sources();
        self.locals.refill_slots(cc.locals.len());
        {
            // Lean parameters are the plain invocant, positional `$`
            // scalars in order, and the implicit `*%_` (`lean_param_ok`).
            let inv_pd = method_def.param_defs.iter().find(|pd| pd.is_invocant);
            let invocant = inv_pd
                .and_then(|pd| {
                    self.take_implicit_self_invocant_arrival(method_sym.as_str(), Some(pd))
                })
                .unwrap_or_else(|| base.clone());
            let inv_offset = usize::from(inv_pd.is_some());
            let param_value = |idx: usize| -> Value {
                match method_def.param_defs.get(idx) {
                    Some(pd) if pd.is_invocant => invocant.clone(),
                    Some(pd) if pd.slurpy => Value::NIL,
                    pd => args
                        .get(idx - inv_offset)
                        .map(|v| Self::itemize_fast_method_arg(pd, v.clone()))
                        .unwrap_or(Value::NIL),
                }
            };
            let attrs_guard = attrs_cell.as_ref().map(|c| c.as_map());
            let fallback_attrs_guard = fallback_attrs_cell.as_ref().map(|c| c.as_map());
            let attr_get = |key: Symbol| -> Option<Value> {
                attrs_guard
                    .as_ref()
                    .and_then(|g| g.get(key).cloned())
                    .or_else(|| {
                        fallback_attrs_guard
                            .as_ref()
                            .and_then(|g| g.get(key).cloned())
                    })
            };
            let plan = method_def.frame_plan(owner_sym, cc);
            for (i, init) in plan.locals.iter().enumerate() {
                self.locals[i] = match *init {
                    LocalInit::Param(idx) => param_value(idx as usize),
                    LocalInit::Invocant => base.clone(),
                    LocalInit::Class => class_val.clone(),
                    LocalInit::Role | LocalInit::Nil => Value::NIL,
                    LocalInit::Topic => Value::package(crate::symbol::wk::any()),
                    LocalInit::AttrPrivate { qualified, bare } => attr_get(qualified)
                        .or_else(|| attr_get(bare))
                        .unwrap_or(Value::NIL),
                    LocalInit::AttrPublic { bare } => attr_get(bare).unwrap_or(Value::NIL),
                    LocalInit::AttrAggregate { sigil_key, bare } => attr_get(sigil_key)
                        .or_else(|| attr_get(bare))
                        .unwrap_or(Value::NIL),
                    // Refused by `slot_frame_body_ok`.
                    LocalInit::Outer => Value::NIL,
                };
            }
        }

        let method_callable_id = crate::value::next_instance_id();
        self.push_method_routine_with_location(
            owner_sym,
            method_def.lexical_package,
            method_sym,
            self.current_source_line(),
            self.executing_source_file_sym(),
            method_def.source_file_sym(),
            method_def.is_submethod,
            method_def.is_hidden_from_backtrace,
            method_callable_id,
            crate::runtime::routine_stack::FrameFixtures {
                invocant: Some(base.clone()),
                class: Some(class_val),
                role: None,
            },
        );
        let saved_lexical_fatal_mode =
            std::mem::replace(&mut self.module.lexical_fatal_mode, cc.method_fatal_pragma);
        let let_mark = self.let_saves_len();
        let super::vm_method_body_run::MethodBodyOutcome {
            result,
            explicit_return,
            fail_bypass,
        } = self.run_compiled_method_body(
            cc,
            compiled_fns,
            super::vm_method_body_run::MethodBodyFrame {
                owner_sym,
                method_sym,
                method_callable_id,
                saved_stack_depth,
                let_mark,
            },
        );
        self.module.lexical_fatal_mode = saved_lexical_fatal_mode;
        let ret_val = if result.is_ok() && self.stack.len() > saved_stack_depth {
            self.stack.pop().unwrap_or(Value::NIL)
        } else {
            Value::NIL
        };
        self.stack.truncate(saved_stack_depth);
        self.pop_routine();
        self.pop_method_class();
        self.dispatch.method_dispatch_pure = true;
        // The env handle was never replaced, so there is nothing to restore.
        match slim_record {
            Some((caller, line)) => {
                crate::vm::vm_poll::poll(crate::gc::SafepointKind::Return, 0);
                self.locals.pop_frame(caller);
                self.cur_source_line = line;
            }
            None => {
                let _frame = self.pop_call_frame();
            }
        }

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };
        let final_result = if fail_bypass {
            final_result
        } else {
            self.finalize_method_result_fast(cc, final_result, None, None, None)
        };
        final_result.map(|v| (Self::method_frame_result_sharing(&base, v), None))
    }
}
