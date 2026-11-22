//! The routine form of the topicalizing operators `orelse`, `andthen` and
//! `notandthen` (`&infix:<orelse>(...)`, `[orelse] ...`), as Rakudo's
//! `multi sub infix:<orelse>(+a)` and kin fold their operands.

use super::*;
use crate::value::SeqClosureCount;

impl Interpreter {
    /// Fold `args` left to right with `op`. A value stops the fold when it is
    /// defined (`orelse`, which answers it; `notandthen`, which answers
    /// `Empty`) or undefined (`andthen`, which answers `Empty`); otherwise the
    /// next operand becomes the value, a `Callable` one called with the value
    /// so far when it takes an argument (`*.self`, `-> $x { ... }`) and with
    /// none when it takes none.
    // Cost: O(n) plus the Callables' calls, n = operands.
    pub(crate) fn fold_topicalizing_infix(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Some((first, rest)) = args.split_first() else {
            return Ok(Value::NIL);
        };
        let mut current = first.clone();
        for next in rest {
            // Rakudo asks `.defined`, which handles a Failure.
            if crate::builtins::method_table::failure::is_failure(&current) {
                current.mark_failure_handled();
            }
            let defined = crate::runtime::types::value_is_defined(&current);
            match op {
                "orelse" if defined => return Ok(current),
                "andthen" if !defined => return Ok(Value::slip(Vec::new())),
                "notandthen" if defined => return Ok(Value::slip(Vec::new())),
                _ => {}
            }
            current = if next.as_sub().is_some() {
                let takes_argument = match self.sequence_closure_count(next) {
                    SeqClosureCount::Fixed(count) => count > 0,
                    SeqClosureCount::All => true,
                };
                let call_args = if takes_argument {
                    vec![current]
                } else {
                    Vec::new()
                };
                self.call_sub_value(next.clone(), call_args)?
            } else {
                next.clone()
            };
        }
        Ok(current)
    }
}
