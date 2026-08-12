//! Native dispatch for the internal `__ScheduledTapPump` class — the
//! callback-shim target `Supply.schedule-on()` taps invoke instead of the
//! real tap/done/quit callback (ADR-0028 Slice 1).
//!
//! An instance carries either a `pump_id` (the `ThreadPoolScheduler` fork,
//! ADR §2 — forward the event into the pump channel a pooled drain worker is
//! reading from) or a `scheduler` + `real_cb` pair (the any-other-scheduler
//! fork, ADR §3 — stash the call and hand the scheduler a thunk it can `.cue`
//! whenever it likes), plus, for the `__mutsu_scheduled_run_cue` thunk
//! itself, a `cue_thunk_id` to look the stashed call back up by.

use super::state_scheduled_pump::{
    ScheduledCueKind, register_cue_thunk, scheduled_pump_send, take_cue_thunk,
};
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::{AttrMap, ValueView};

impl Interpreter {
    pub(in crate::runtime) fn native_scheduled_tap_pump(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "__mutsu_scheduled_emit" => {
                self.scheduled_tap_pump_dispatch(attributes, ScheduledCueKind::Emit, args)
            }
            "__mutsu_scheduled_done" => {
                self.scheduled_tap_pump_dispatch(attributes, ScheduledCueKind::Done, args)
            }
            "__mutsu_scheduled_quit" => {
                self.scheduled_tap_pump_dispatch(attributes, ScheduledCueKind::Quit, args)
            }
            "__mutsu_scheduled_run_cue" => self.scheduled_tap_pump_run_cue(attributes),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on '__ScheduledTapPump'",
                method
            ))),
        }
    }

    /// Handle one emit/done/quit callback invocation. A `pump_id` attribute
    /// selects the pooled-drain fork (§2): the event is forwarded into the
    /// channel and this call returns immediately, so the emitting thread is
    /// never blocked on the real callback. Otherwise the instance carries a
    /// `scheduler` + `real_cb` pair (§3): the call is stashed and a thunk is
    /// handed to the scheduler's own `.cue`.
    fn scheduled_tap_pump_dispatch(
        &mut self,
        attributes: &AttrMap,
        kind: ScheduledCueKind,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(ValueView::Int(pump_id)) = attributes.get("pump_id").map(Value::view) {
            let event = match kind {
                ScheduledCueKind::Emit => {
                    super::state::SupplyEvent::Emit(args.into_iter().next().unwrap_or(Value::NIL))
                }
                ScheduledCueKind::Done => super::state::SupplyEvent::Done,
                ScheduledCueKind::Quit => {
                    super::state::SupplyEvent::Quit(args.into_iter().next().unwrap_or(Value::NIL))
                }
            };
            scheduled_pump_send(pump_id as u64, event);
            return Ok(Value::NIL);
        }
        let scheduler = attributes.get("scheduler").cloned().unwrap_or(Value::NIL);
        let real_cb = attributes.get("real_cb").cloned().unwrap_or(Value::NIL);
        let thunk_id = register_cue_thunk(kind, real_cb, args);
        let thunk = Self::build_scheduled_cue_run_thunk(thunk_id);
        self.call_method_with_values(scheduler, "cue", vec![thunk])?;
        Ok(Value::NIL)
    }

    /// Run one stashed cue-thunk entry: the target scheduler decided it is
    /// time to deliver this emit/done/quit call to the real callback.
    fn scheduled_tap_pump_run_cue(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        if let Some(ValueView::Int(id)) = attributes.get("cue_thunk_id").map(Value::view)
            && let Some((kind, real_cb, mut payload)) = take_cue_thunk(id as u64)
        {
            match kind {
                ScheduledCueKind::Emit => {
                    self.call_sub_value(real_cb, payload, true)?;
                }
                ScheduledCueKind::Done => {
                    self.invoke_done_callback(real_cb)?;
                }
                ScheduledCueKind::Quit => {
                    let reason = payload.pop().unwrap_or(Value::NIL);
                    self.call_supply_quit_handler(real_cb, reason)?;
                }
            }
        }
        Ok(Value::NIL)
    }

    /// Build the zero-arg thunk `Value` handed to a `Scheduler.cue` call: a
    /// synthesized `SubData` whose body is a single `MethodCall` on a literal
    /// `__ScheduledTapPump` instance carrying only `cue_thunk_id` — the same
    /// synthesized-callable idiom `cue_scheduler_interval` uses for
    /// `__mutsu_interval_tick`. Empty env and no captures make invoking it
    /// from whatever thread the scheduler chooses trivially safe.
    fn build_scheduled_cue_run_thunk(thunk_id: u64) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("cue_thunk_id".to_string(), Value::int(thunk_id as i64));
        let instance = Value::make_instance(Symbol::intern("__ScheduledTapPump"), attrs);
        let body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Literal(instance)),
            name: Symbol::intern("__mutsu_scheduled_run_cue"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        })];
        Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern(""),
            params: Vec::new(),
            param_defs: Vec::new(),
            body,
            is_rw: false,
            is_raw: false,
            env: crate::runtime::Env::new(),
            assumed_positional: Vec::new(),
            assumed_named: std::collections::HashMap::new(),
            id: crate::value::next_instance_id(),
            empty_sig: false,
            is_bare_block: true,
            compiled_code: None,
            compiled_fns: None,
            compiled_routine: None,
            is_decl_expr_thunk: false,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            authoritative_captures: Vec::new(),
            upvalues: Vec::new(),
            captured_fatal_mode: false,
        }))
    }
}
