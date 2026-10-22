//! Dispatch of `whenever` bodies and their `LAST` / `QUIT` / `CLOSE` phaser
//! callbacks from the react/supply drive loop (`vm_react_loop.rs`).

use super::*;

impl Interpreter {
    /// Dispatch a `whenever` body or one of its `LAST` / `QUIT` / `CLOSE` phaser
    /// callbacks as **compiled bytecode** (Stage 2). The first argument, when
    /// present, is the triggering value.
    ///
    /// A `whenever` body and its phasers are closures compiled at the
    /// `whenever` site (#12872) and run as such. A callback built at run time
    /// from bare AST is still compiled here — as a *block*, which is what a
    /// `whenever` body and a phaser are in raku. A routine compile would give
    /// the body its own fresh `$_` (`Any`) instead of the enclosing lexical
    /// topic.
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
    pub(crate) fn call_react_callback(
        &mut self,
        cb: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
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
        let has_signature = !data.params.is_empty() && data.params.as_slice() != ["_"];
        let (args, topic) = if has_signature {
            let args: Vec<Value> = args.into_iter().map(Self::containerize_pair_item).collect();
            (args, None)
        } else {
            let topic = args.first().cloned();
            (args, topic)
        };
        // A `whenever` body and its phasers are closures compiled at their
        // definition site (`exec_whenever_scope_op`), as is any user code
        // object handed to a supply helper.
        if data.compiled_code.is_some() || data.compiled_routine.is_some() || data.body.is_empty() {
            return self.vm_call_map_block(cb, args, topic, false);
        }
        let data = data.clone();
        // TODO: compile to bytecode where the callback is built. A code object
        // made from bare AST at run time has no compile-time code object, so it
        // is compiled per dispatch here.
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
        self.call_compiled_closure_with_topic(
            &data,
            &cc,
            args,
            super::vm_closure_dispatch::ClosureTopic::from_loop(topic, false),
            &fns,
        )
    }
}
