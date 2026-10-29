//! `RakuAST::CompUnit.add-check-code`: the closures queued on the unit being
//! compiled run once its CHECK phasers have (the compiler-internal call
//! `__mutsu_run_check_code` the main unit's prologue marker is followed by).

use super::*;

impl Interpreter {
    /// Run, last queued first, each closure `$*CU.add-check-code` queued on the
    /// innermost unit. Each runs with no arguments, as rakudo's does.
    // Cost: O(c) calls plus the closures' own cost, c = queued closures.
    pub(crate) fn builtin_run_check_code(&mut self) -> Result<Value, RuntimeError> {
        for (closure, _cu) in crate::unit_source_text::take_check_code() {
            self.vm_call_on_value(closure, Vec::new(), None)?;
        }
        Ok(Value::NIL)
    }
}
