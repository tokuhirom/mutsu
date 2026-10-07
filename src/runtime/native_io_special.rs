use super::*;
use crate::value::AttrMap;
use std::collections::HashMap;

impl Interpreter {
    /// Create an IO::Special instance for standard handles (STDIN, STDOUT, STDERR).
    pub(super) fn make_io_special_instance(name: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(format!("<{}>", name)));
        Value::make_instance(Symbol::intern("IO::Special"), attrs)
    }

    /// Handle method dispatch on IO::Special instances: `new` is the
    /// constructor; every other method `IO::Special` declares is a row of the
    /// method table, reached through its owner (ADR-11276 §9.22).
    // Cost: O(1) to find the row, plus the handler's own cost.
    pub(super) fn native_io_special(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // `Mu.perl` is `self.raku`.
        let row_method = if method == "perl" { "raku" } else { method };
        if let Some(result) = crate::builtins::method_table::invoke_owner(
            self,
            &["IO::Special", "Mu"],
            row_method,
            &args,
            || Value::make_instance(Symbol::intern("IO::Special"), attributes.clone()),
        ) {
            return result;
        }
        match method {
            "new" => {
                // IO::Special.new("<STDOUT>")
                if let Some(arg) = args.first() {
                    let w = arg.to_string_value();
                    let mut new_attrs = HashMap::new();
                    new_attrs.insert("what".to_string(), Value::str(w));
                    Ok(Value::make_instance(
                        Symbol::intern("IO::Special"),
                        new_attrs,
                    ))
                } else {
                    Err(RuntimeError::new(
                        "IO::Special.new requires a string argument",
                    ))
                }
            }
            "Bool" => Ok(Value::TRUE),
            "defined" => Ok(Value::TRUE),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Special",
                method
            ))),
        }
    }
}
