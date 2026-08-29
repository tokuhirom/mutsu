//! Native dispatch for the internal `__SupplyCollector` class — the
//! emit/done/quit shim `Interpreter::supply_collect_values` (ADR-0031
//! Decision B / Slice 2) taps a `Supply` with, instead of the old
//! synchronous-replay helpers (`replay_cold_whenever_capture` /
//! `replay_static_whenever_promise`) that could not see a value emitted
//! after the tap call itself returned.
//!
//! An instance carries a `collector_id` looked up in
//! `state_supply_collector`'s process-global map to find the `ReactWaker`
//! the drain loop is reading from. Invoking the shim just pushes an event
//! and returns, so the emitting thread (which may be a completely different
//! thread than the one draining) is never blocked on real user code.

use super::state_supply_collector::supply_collector_waker;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::ValueView;
use crate::value::waker::SinkEvent;

impl Interpreter {
    pub(in crate::runtime) fn native_supply_collector(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let Some(ValueView::Int(collector_id)) = attributes.get("collector_id").map(Value::view)
        else {
            return Ok(Value::NIL);
        };
        let Some(waker) = supply_collector_waker(collector_id as u64) else {
            // The drain already gave up (deadline/disconnect) and unregistered
            // this collector; a late event has nowhere left to go.
            return Ok(Value::NIL);
        };
        match method {
            "__mutsu_collector_emit" => {
                waker.push(
                    0,
                    SinkEvent::Emit(args.into_iter().next().unwrap_or(Value::NIL)),
                );
            }
            "__mutsu_collector_done" => {
                waker.push(0, SinkEvent::Done);
            }
            "__mutsu_collector_quit" => {
                waker.push(
                    0,
                    SinkEvent::Quit(args.into_iter().next().unwrap_or(Value::NIL)),
                );
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "No native method '{}' on '__SupplyCollector'",
                    method
                )));
            }
        }
        Ok(Value::NIL)
    }

    /// A zero-or-one-param synthesized callable whose body is a single
    /// `MethodCall` on a literal `__SupplyCollector` instance carrying
    /// `collector_id` — the same synthesized-callable idiom
    /// `build_scheduled_shim_sub` uses for the `__ScheduledTapPump` shims.
    /// `"done"` takes no parameter (Raku's `done()` is zero-arg); `"emit"`/
    /// `"quit"` take one (the emitted value / quit exception) and forward it
    /// verbatim.
    pub(in crate::runtime) fn build_supply_collector_shim(collector_id: u64, kind: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("collector_id".to_string(), Value::int(collector_id as i64));
        let instance = Value::make_instance(Symbol::intern("__SupplyCollector"), attrs);
        let method_name = match kind {
            "emit" => "__mutsu_collector_emit",
            "done" => "__mutsu_collector_done",
            "quit" => "__mutsu_collector_quit",
            other => unreachable!("unknown supply collector shim kind {other:?}"),
        };
        let has_param = kind != "done";
        let (params, param_defs, call_args) = if has_param {
            (
                vec!["v".to_string()],
                vec![crate::ast::ParamDef {
                    name: "v".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: false,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                    block_param: false,
                }],
                vec![crate::ast::Expr::Var("v".to_string())],
            )
        } else {
            (Vec::new(), Vec::new(), Vec::new())
        };
        let body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Literal(instance)),
            name: Symbol::intern(method_name),
            args: call_args,
            modifier: None,
            quoted: false,
        })];
        Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern(""),
            params,
            param_defs,
            body: std::sync::Arc::new(body),
            is_rw: false,
            is_raw: false,
            env: Env::new(),
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
