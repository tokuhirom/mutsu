//! The env a named routine's code object (`&f`) carries.
use super::*;

impl Interpreter {
    /// The captured env for a code object built from the registered routine
    /// whose compiled body is `compiled` (`&f`, a routine escaping as a value).
    ///
    /// A compiled routine reaches this env only through
    /// `call_compiled_closure`, which installs its leaf tier as the call's
    /// capture fallback -- the same role a closure's captured env plays. So it
    /// is built by the same key filter as a closure capture
    /// ([`super::vm_register_ops::capture_keeps`]): the routine's free
    /// variables, the shadow metadata of those, and every system name. The
    /// plain user lexicals it drops are names the body provably cannot
    /// reference.
    ///
    /// Capturing the whole live env instead (`self.env().clone()`) cost nothing
    /// up front but shared the running frame's leaf tier, so the frame's next
    /// by-name write copied that whole tier: every `&f` read cost O(v), v =
    /// env entries of the current scope (#9169).
    ///
    /// Kept whole where the filter cannot be trusted: a body that can name a
    /// lexical dynamically (`EVAL`, `CALLER::`, symbolic deref -- the same
    /// `needs_reflective_capture` gate the closure capture uses), and a def
    /// with no compiled body, whose AST carrier merges the entire captured env
    /// into the call.
    // Cost: O(s + f), s = visible env names that are not plain user lexicals,
    // f = free vars of the routine (see `capture_closure_env`).
    pub(crate) fn routine_code_object_env(
        &self,
        compiled: Option<&std::sync::Arc<crate::opcode::CompiledFunction>>,
    ) -> Env {
        let Some(cf) = compiled else {
            return self.env().clone();
        };
        let cc = &cf.code;
        if crate::opcode::reflective_name_access_possible() && cc.needs_reflective_capture {
            return self.env().clone();
        }
        let free = cc.capture_free_var_set();
        let own_locals = cc.capture_local_set();
        let mut env = self.env().filtered_flat_capture(
            &|k, _v| super::vm_register_ops::capture_keeps(k, free, own_locals),
            cc.capture_probe_keys(),
        );
        self.capture_bare_callees(cc, &mut env);
        env
    }
}
