//! `nqp::findmethod` / `nqp::tryfindmethod` (#11499): method lookup by name
//! through the same resolver `.^find_method` uses (`classhow_find_method`), so
//! the two cannot disagree. The other object-model ops compile to Raku method
//! calls (`compiler/nqp_object_forms.rs`).

use crate::runtime::{Interpreter, RuntimeError};
use crate::value::Value;

impl Interpreter {
    /// Run `findmethod` / `tryfindmethod`; `None` for any other name.
    pub(crate) fn call_nqp_op_object(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let try_only = match op {
            "findmethod" => false,
            "tryfindmethod" => true,
            _ => return None,
        };
        let obj = args.first().cloned().unwrap_or(Value::NIL);
        let name = args.get(1).map(Value::to_string_value).unwrap_or_default();
        // nqp::findmethod($obj, $name) — the method object, dying when there
        // is none; nqp::tryfindmethod answers null (Nil) instead.
        // Cost: as `.^find_method`: O(1) on a method-cache hit, O(d) on a
        // miss, d = the MRO's depth.
        Some(match self.classhow_find_method(&obj, &name) {
            Some(method) => Ok(method),
            None if try_only => Ok(Value::NIL),
            None => Err(RuntimeError::new(format!(
                "Cannot find method '{name}' on object of type {}",
                crate::runtime::utils::value_type_name(&obj)
            ))),
        })
    }
}
