use crate::runtime::*;
use crate::symbol::Symbol;

use super::state::SupplyEvent;

impl Interpreter {
    pub(in crate::runtime) fn native_encoding_builtin(
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Value {
        match method {
            "name" => attributes
                .get("name")
                .cloned()
                .unwrap_or(Value::str(String::new())),
            "alternative-names" => attributes
                .get("alternative-names")
                .cloned()
                .unwrap_or_else(|| Value::array(Vec::new())),
            "encoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::str(enc_name));
                // Extract :replacement named arg from args
                for arg in args {
                    if let Value::Pair(key, value) = arg
                        && key == "replacement"
                    {
                        attrs.insert("replacement".to_string(), *value.clone());
                    }
                }
                Value::make_instance(Symbol::intern("Encoding::Encoder"), attrs)
            }
            "decoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::str(enc_name));
                Value::make_instance(Symbol::intern("Encoding::Decoder"), attrs)
            }
            "gist" | "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("Encoding::Builtin<{}>", name))
            }
            "WHAT" => Value::Package(Symbol::intern("Encoding::Builtin")),
            _ => Value::Nil,
        }
    }

    pub(in crate::runtime) fn native_encoding_encoder(
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match method {
            "encode-chars" => {
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let enc_name = attributes
                    .get("encoding")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let replacement = attributes.get("replacement");
                let is_ascii = matches!(enc_name.to_lowercase().as_str(), "ascii" | "us-ascii");

                let mut bytes: Vec<Value> = Vec::new();
                if is_ascii {
                    for ch in input.chars() {
                        if ch as u32 > 127 {
                            if let Some(repl) = replacement {
                                let repl_str = if matches!(repl, Value::Bool(true)) {
                                    // :replacement (Bool True) -> default replacement char '?'
                                    "?".to_string()
                                } else {
                                    repl.to_string_value()
                                };
                                for b in repl_str.bytes() {
                                    bytes.push(Value::Int(b as i64));
                                }
                            } else {
                                return Err(RuntimeError::new(format!(
                                    "Cannot encode character '{}' (U+{:04X}) in ASCII",
                                    ch, ch as u32
                                )));
                            }
                        } else {
                            bytes.push(Value::Int(ch as u32 as i64));
                        }
                    }
                } else {
                    // UTF-8 encoding
                    for b in input.as_bytes() {
                        bytes.push(Value::Int(*b as i64));
                    }
                }

                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                Ok(Value::make_instance(Symbol::intern("Blob[uint8]"), attrs))
            }
            "WHAT" => Ok(Value::Package(Symbol::intern("Encoding::Encoder"))),
            _ => Ok(Value::Nil),
        }
    }

    pub(in crate::runtime) fn native_encoding_decoder(
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Value {
        match method {
            "decode-chars" => {
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(input)
            }
            "WHAT" => Value::Package(Symbol::intern("Encoding::Decoder")),
            _ => Value::Nil,
        }
    }

    /// Background event loop for Supply.act on live supplies (e.g., signal).
    /// Receives events from the channel and calls the callback.
    /// If the callback calls `exit`, terminates the entire process.
    pub(in crate::runtime) fn run_supply_act_loop(
        interp: &mut Interpreter,
        rx: &std::sync::mpsc::Receiver<SupplyEvent>,
        cb: &Value,
        delay_seconds: f64,
    ) {
        use std::io::Write;
        while let Ok(SupplyEvent::Emit(value)) = rx.recv() {
            Self::sleep_for_supply_delay(delay_seconds);
            let result = interp.call_sub_value(cb.clone(), vec![value], true);
            // Flush stdout (check both the per-interpreter buffer and the
            // shared thread output buffer used by thread clones).
            if !interp.output.is_empty() {
                print!("{}", interp.output);
                let _ = std::io::stdout().flush();
                interp.output.clear();
            }
            if let Some(ref shared) = interp.shared_thread_output {
                let drained = std::mem::take(&mut *shared.lock().unwrap());
                if !drained.is_empty() {
                    print!("{}", drained);
                    let _ = std::io::stdout().flush();
                }
            }
            // Flush stderr
            if !interp.stderr_output.is_empty() {
                eprint!("{}", interp.stderr_output);
                let _ = std::io::stderr().flush();
                interp.stderr_output.clear();
            }
            if let Some(ref shared) = interp.shared_thread_stderr {
                let drained = std::mem::take(&mut *shared.lock().unwrap());
                if !drained.is_empty() {
                    eprint!("{}", drained);
                    let _ = std::io::stderr().flush();
                }
            }
            // If the callback called exit, terminate the process
            if interp.halted {
                std::process::exit(interp.exit_code as i32);
            }
            // If the callback threw an unhandled exception, terminate
            if let Err(err) = result {
                eprintln!(
                    "Unhandled exception in code scheduled on thread\n{}",
                    err.message
                );
                let _ = std::io::stderr().flush();
                std::process::exit(1);
            }
        }
    }
}
