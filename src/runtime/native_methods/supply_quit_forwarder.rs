//! Native dispatch for the internal `__SupplyQuitForwarder` class — the
//! `quit =>` callback the `"tap" | "act"` dispatch's b3 branch ("chain a REAL
//! tap" for a `whenever` whose source is itself an on-demand `supply { ... }`
//! block) hands to the inner tap.
//!
//! ADR-0031 Decision A fixed quit ownership on the enclosing supply block's
//! own emitter, and Slice 1 made that true for a `whenever` *body* die
//! (`call_supply_tap`) and for a supplier-backed source's own `.quit()`
//! (`take_supplier_quit_callbacks_via_group`). A source that is itself an
//! on-demand supply block had no such route: b3 registered a `quit =>` on the
//! inner tap only when the `whenever` declared its own `QUIT` phaser, so a
//! quit arriving two or more levels down was silently swallowed and the
//! pipeline never terminated at all.
//!
//! This forwarder is registered unconditionally instead, exactly the way b3
//! already registers `done` (`done_chain` always carries the done-group
//! marker whether or not a `LAST` phaser was declared). It re-derives its
//! destination from first principles — the enclosing block's
//! `emitter_supplier_id` — rather than relying on a serialize-group hop that
//! only exists for supplier-backed sources, and because the enclosing block's
//! own tap installed the same kind of forwarder one level further out,
//! propagation is transitive to any nesting depth.
//!
//! It is a real synthesized callable (the `__ScheduledTapPump` /
//! `__SupplyCollector` idiom: an empty-env `SubData` whose body is one
//! `MethodCall` on a literal internal instance), not a marker value, so every
//! quit-delivery path dispatches it uniformly through `call_sub_value` —
//! including the channel-backed act loop, which invokes a tap's quit callback
//! directly rather than through `Interpreter::call_supply_quit_handler`.

use super::state::supplier_snapshot;
use crate::runtime::native_supply_methods::QuitOutcome;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::ValueView;

impl Interpreter {
    pub(in crate::runtime) fn native_supply_quit_forwarder(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if method != "__mutsu_supply_forward_quit" {
            return Err(RuntimeError::new(format!(
                "No native method '{}' on '__SupplyQuitForwarder'",
                method
            )));
        }
        let reason = args.into_iter().next().unwrap_or(Value::NIL);
        self.forward_quit_to_supply_emitter(attributes, reason)?;
        Ok(Value::NIL)
    }

    /// Deliver a source quit that arrived on a chained inner tap to the
    /// enclosing supply block. Mirrors the `Supplier."quit"` protocol exactly:
    /// run this `whenever`'s own `QUIT` phasers first; if one handles the
    /// exception the block completes with `done` instead of `quit` (unless the
    /// phaser already called `done` itself), otherwise the block's emitter is
    /// quit — which is what runs its downstream `quit =>` handlers, and what
    /// makes the next level up see a quit in turn.
    fn forward_quit_to_supply_emitter(
        &mut self,
        attributes: &AttrMap,
        reason: Value,
    ) -> Result<(), RuntimeError> {
        let Some(ValueView::Int(sid)) = attributes.get("emitter_supplier_id").map(Value::view)
        else {
            return Ok(());
        };
        let phasers: Vec<Value> = match attributes.get("quit_phasers").map(Value::view) {
            Some(ValueView::Array(cbs, ..)) => cbs.iter().cloned().collect(),
            _ => Vec::new(),
        };
        let mut handled = false;
        let mut via_done = false;
        for qcb in phasers {
            match self.run_whenever_quit_phaser(qcb, reason.clone()) {
                QuitOutcome::HandledViaDone => {
                    handled = true;
                    via_done = true;
                }
                QuitOutcome::Handled => handled = true,
                QuitOutcome::Unhandled => {}
            }
        }
        // A supply terminates exactly once. When a block has several chained
        // sources and more than one of them quits, only the first delivery
        // tears the block down; `Supplier."quit"`'s own callback drain already
        // gives run-once for the downstream handler, but the `done` branch
        // below has no such drain of its own.
        let (_, already_done, already_quit) = supplier_snapshot(sid as u64);
        if already_done || already_quit.is_some() {
            return Ok(());
        }
        let emitter = Value::make_instance(Symbol::intern("Supplier"), {
            let mut a = HashMap::new();
            a.insert("emitted".to_string(), Value::array(Vec::new()));
            a.insert("done".to_string(), Value::FALSE);
            a.insert("supplier_id".to_string(), Value::int(sid));
            a
        });
        if handled {
            if !via_done {
                self.call_method_with_values(emitter, "done", vec![])?;
            }
        } else {
            self.call_method_with_values(emitter, "quit", vec![reason])?;
        }
        Ok(())
    }

    /// A one-param synthesized callable whose body is a single `MethodCall` on
    /// a literal `__SupplyQuitForwarder` instance carrying the enclosing supply
    /// block's `emitter_supplier_id` plus this `whenever`'s own `QUIT` phaser
    /// callbacks — the same synthesized-callable idiom
    /// `build_supply_collector_shim` uses.
    pub(in crate::runtime) fn build_supply_quit_forwarder(
        emitter_supplier_id: u64,
        quit_phasers: Vec<Value>,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "emitter_supplier_id".to_string(),
            Value::int(emitter_supplier_id as i64),
        );
        attrs.insert("quit_phasers".to_string(), Value::array(quit_phasers));
        let instance = Value::make_instance(Symbol::intern("__SupplyQuitForwarder"), attrs);
        let body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Literal(instance)),
            name: Symbol::intern("__mutsu_supply_forward_quit"),
            args: vec![crate::ast::Expr::Var("v".to_string())],
            modifier: None,
            quoted: false,
        })];
        Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern(""),
            params: vec!["v".to_string()],
            param_defs: vec![crate::ast::ParamDef {
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
            body,
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
