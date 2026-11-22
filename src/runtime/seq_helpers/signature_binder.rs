//! `Signature.ACCEPTS(Capture)` of a signature whose parameter code reads
//! its own scope (#18570, #18661).

use super::super::*;

impl Interpreter {
    /// Whether `args` bind to the parameters of `routine`, the routine the
    /// signature belongs to (`SigInfo::binder`), with its `where` clauses and
    /// computed defaults run in the routine's own scope -- its captured
    /// variables -- and its body not run, as rakudo's trial bind does. For a
    /// routine with a body this is a dry run in its captured scope: what the
    /// binder declares is dropped with the scratch scope.
    // Cost: one bind of the parameters, O(p) plus their code.
    pub(in crate::runtime) fn signature_binds_in_scope(&mut self, routine: &Value, args: &[Value]) -> bool {
        let ValueView::Sub(data) = routine.view() else {
            return false;
        };
        // A signature literal is an empty routine: calling it binds in its
        // scope, and what its clauses write there stays written (#18570).
        if data.body.is_empty() && data.compiled_routine.is_none() {
            return self.call_sub_value(routine.clone(), args.to_vec()).is_ok();
        }
        let saved = std::mem::replace(&mut self.env, data.env.clone());
        let bound = self.bind_function_args_values(&data.param_defs, &data.params, args);
        self.env = saved;
        bound.is_ok()
    }
}
