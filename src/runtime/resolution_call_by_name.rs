//! Calling a user routine family by name from a first-class code value.

use super::*;

impl Interpreter {
    /// Calls `name` as the user routine family a code value stands for.
    ///
    /// [`Interpreter::call_function`] is the builtin funnel: its arms run a
    /// core routine (`reverse`, `sort`, ...) before any user declaration is
    /// looked at. A code value captured from a user `proto`/`multi` named like
    /// a core routine -- `&reverse` imported from P5reverse -- must reach the
    /// user family instead, so a builtin name goes to
    /// `call_function_fallback`, which ranks user candidates ahead of the
    /// native table, exactly as a direct `reverse(...)` call does.
    // Cost: O(1) beyond the dispatched routine's own cost.
    pub(crate) fn call_user_family_by_name(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if !Self::is_builtin_function(name) {
            return self.call_function(name, args);
        }
        let (args, callsite_line) = self.sanitize_call_args_owned(args);
        self.test_pending_callsite_line = callsite_line;
        self.call_function_fallback(name, &args)
    }
}
