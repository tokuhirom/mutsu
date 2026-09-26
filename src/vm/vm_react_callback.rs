//! Dispatch of `whenever` bodies and their `LAST` / `QUIT` / `CLOSE` phaser
//! callbacks from the react/supply drive loop (`vm_react_loop.rs`).

use super::*;

impl Interpreter {
    /// Dispatch a `whenever` body or one of its `LAST` / `QUIT` / `CLOSE` phaser
    /// callbacks as **compiled bytecode** (Stage 2). The first argument, when
    /// present, is the triggering value.
    ///
    /// The callbacks `run_whenever_with_value` builds carry only their AST, so
    /// they are compiled here — as a *block*, which is what a `whenever` body
    /// and a phaser are in raku. A routine compile would give the body its own
    /// fresh `$_` (`Any`) instead of the enclosing lexical topic.
    ///
    /// How the value reaches the body depends on the signature:
    ///
    /// * No declared signature (a bare block, or the `QUIT` phaser's implicit
    ///   `$_`): the value is the block's topic `$_`, bound through the
    ///   explicit-topic path so an emitted `Pair` is still the topic rather
    ///   than a named argument.
    /// * A declared signature (`-> $x`, `-> ($_, $n?)`, or a placeholder
    ///   `$^v`): the value binds **only** through that signature, and `$_`
    ///   stays the enclosing lexical topic, exactly like the same block passed
    ///   to `.tap`. An emitted `Pair` is handed over in its positional flavour
    ///   (the conversion `OpCode::ContainerizePair` performs for `f((a => 1))`),
    ///   so the binder does not mistake it for a named argument.
    ///
    /// Loop-control signals (`done` / `next` / `last`) surface as `Err`, so the
    /// drive loop's signal mapping (`run_react_consumer` etc.) is unchanged.
    pub(super) fn call_react_callback(
        &mut self,
        cb: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // A `whenever`/`LAST`/`QUIT` callback shares the enclosing react block's
        // lexicals. Each closure call persists its captured-outer free vars as
        // per-instance state (keyed by the callback's Sub id) and restores that
        // snapshot on re-entry. For a react callback that is wrong: on re-entry it
        // would restore a *stale* snapshot of a shared lexical (e.g. `my $order`
        // that a sibling `whenever` just updated), clobbering the sibling's write.
        // Drop this callback's per-instance state so it reads the shared lexical
        // from the live caller env — which every sibling writes back to.
        if let ValueView::Sub(data) = cb.view()
            && !self.nested_react_callbacks.contains(&data.id)
        {
            self.clear_closure_captured_state_for(data.id);
        }
        // Every `whenever`/`LAST`/`QUIT`/`CLOSE` callback body dispatches
        // through here, on whichever thread actually runs it, so a `done`
        // raised anywhere in its dynamic extent (directly or via a nested
        // sub call) has a react/supply drive loop to terminate — see
        // `runtime::react_done_handler_depth`.
        let _react_done_handler =
            crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
        let ValueView::Sub(data) = cb.view() else {
            let topic = args.first().cloned();
            return self.vm_call_map_block(cb, args, topic, false);
        };
        // A callback that already carries bytecode (a user code object handed
        // to a supply helper) keeps its own compile and the topic path.
        if data.compiled_code.is_some() || data.compiled_routine.is_some() || data.body.is_empty() {
            let topic = args.first().cloned();
            return self.vm_call_map_block(cb, args, topic, false);
        }
        let data = data.clone();
        let has_signature = !data.params.is_empty() && data.params.as_slice() != ["_"];
        let (args, topic) = if has_signature {
            let args: Vec<Value> = args.into_iter().map(Self::containerize_pair_item).collect();
            (args, None)
        } else {
            let topic = args.first().cloned();
            (args, topic)
        };
        // TODO: compile to bytecode once per `whenever` registration rather than
        // per dispatch; the body is only split into main/LAST/QUIT at runtime
        // (`whenever_body_split`), so there is no compile-time code object yet.
        let (cc, fns) = {
            let mut compiler = crate::compiler::Compiler::new();
            // A `return` in the body is a non-local return to the routine that
            // encloses the `react` (it still becomes X::ControlFlow::Return when
            // no routine is on the dynamic call stack). The body's lexical
            // context is not known here, so assume one may exist.
            compiler.lexically_in_routine = true;
            let mut cc = compiler.compile_closure_body(&data.params, &data.param_defs, &data.body);
            cc.is_pointy_block = true;
            (cc, compiler.take_compiled_functions())
        };
        self.call_compiled_closure_with_topic(&data, &cc, args, topic, false, &fns)
    }
}
