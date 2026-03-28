use super::*;
use std::collections::HashMap;

impl Interpreter {
    /// Create an IO::Special instance for standard handles (STDIN, STDOUT, STDERR).
    pub(super) fn make_io_special_instance(name: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(format!("<{}>", name)));
        Value::make_instance(Symbol::intern("IO::Special"), attrs)
    }

    /// Handle method dispatch on IO::Special instances.
    pub(super) fn native_io_special(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let what = attributes
            .get("what")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // Determine which standard stream this represents
        let is_stdin = what.contains("STDIN");
        let is_stdout = what.contains("STDOUT");
        let is_stderr = what.contains("STDERR");

        match method {
            "Str" | "gist" | "what" => Ok(Value::str(what.clone())),
            "IO" => {
                // .IO returns self
                Ok(Value::make_instance(
                    Symbol::intern("IO::Special"),
                    attributes.clone(),
                ))
            }
            "e" => Ok(Value::Bool(true)),
            "d" | "f" | "l" | "x" => Ok(Value::Bool(false)),
            "s" => Ok(Value::Int(0)),
            "r" => Ok(Value::Bool(is_stdin)),
            "w" => Ok(Value::Bool(is_stdout || is_stderr)),
            "modified" | "accessed" | "changed" => {
                // Return the Instant type object
                Ok(Value::Package(Symbol::intern("Instant")))
            }
            "mode" => Ok(Value::Nil),
            "raku" | "perl" => Ok(Value::str(format!("IO::Special.new(\"{}\")", what))),
            "WHICH" => Ok(Value::str(format!("IO::Special|{}", what))),
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
            "Bool" => Ok(Value::Bool(true)),
            "defined" => Ok(Value::Bool(true)),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Special",
                method
            ))),
        }
    }
}
