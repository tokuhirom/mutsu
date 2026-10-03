//! The lexical `MONKEY-SEE-NO-EVAL` pragma.
//!
//! `use MONKEY-SEE-NO-EVAL` (or `use MONKEY`) lets a string interpolated
//! into a regex assertion (`/<$re>/`, `/<{$code}>/`) carry code blocks, which
//! is otherwise refused with `X::SecurityPolicy`. The pragma is lexical: the
//! `use` records it in the env of the scope it runs in, which every nested
//! block and closure of that scope inherits and a `no MONKEY-SEE-NO-EVAL`
//! inner block overrides.

use super::*;
use crate::meta_ns::MetaNs;
use crate::value::ValueView;

const PRAGMA: &str = "MONKEY-SEE-NO-EVAL";

impl Interpreter {
    /// Record `use`/`no MONKEY-SEE-NO-EVAL` (`on` = `use`) in this scope.
    // Cost: O(1) expected, one env insert.
    pub(crate) fn set_monkey_see_no_eval(&mut self, on: bool) {
        // At a module's top level the pragma covers the whole compunit, so
        // its routines keep it when another unit calls them.
        if on
            && let Some(&(unit, depth_at_push)) = self.module_loading_unit_stack.last()
            && self.routine_stack.len() == depth_at_push
        {
            self.registry_mut().monkey_eval_units.insert(unit);
        }
        self.env_mut().insert_sym(
            MetaNs::Pragma.key_for_str(PRAGMA),
            if on { Value::TRUE } else { Value::FALSE },
        );
    }

    /// Whether the scope running now has `MONKEY-SEE-NO-EVAL` in effect: the
    /// lexical env mark when the scope has one, else whether the running
    /// routine's compunit enabled it at its top level.
    // Cost: O(r) for the unit fallback, r = routine frames on the stack.
    pub(crate) fn monkey_see_no_eval(&self) -> bool {
        match self.env().get_sym(MetaNs::Pragma.key_for_str(PRAGMA)) {
            Some(v) => matches!(v.view(), ValueView::Bool(true)),
            None => {
                // The innermost frame that names its declaring file: a regex
                // (`my token T { <{$code}> }`) runs in a frame of its own that
                // records none.
                let unit = self
                    .routine_stack
                    .iter()
                    .rev()
                    .find_map(|frame| frame.def_file)
                    .map(|file| self.unit_of_source_sym(Some(file)))
                    .unwrap_or_else(|| self.executing_unit_sym());
                self.registry().monkey_eval_units.contains(&unit)
            }
        }
    }

    /// The importing scope's `MONKEY-SEE-NO-EVAL` state, saved around a
    /// module body: the body runs in the importer's env, and its own
    /// `use MONKEY-SEE-NO-EVAL` must not reach the importer.
    // Cost: O(1) expected, one env probe.
    pub(crate) fn monkey_see_no_eval_snapshot(&self) -> Option<Value> {
        self.env()
            .get_sym(MetaNs::Pragma.key_for_str(PRAGMA))
            .cloned()
    }

    /// Put back a [`Self::monkey_see_no_eval_snapshot`].
    // Cost: O(1) expected, one env write.
    pub(crate) fn restore_monkey_see_no_eval(&mut self, saved: Option<Value>) {
        let key = MetaNs::Pragma.key_for_str(PRAGMA);
        match saved {
            Some(value) => {
                self.env_mut().insert_sym(key, value);
            }
            None => {
                self.env_mut().remove_sym(key);
            }
        }
    }
}
