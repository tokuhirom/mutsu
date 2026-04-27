use crate::runtime::*;
use crate::symbol::Symbol;

use super::state::SupplyEvent;

fn decoder_buffer(attrs: &HashMap<String, Value>) -> Vec<u8> {
    let mut out = Vec::new();
    if let Some(v) = attrs.get("buffer") {
        extend_buffer_from_value(&mut out, v);
    }
    out
}

fn extend_buffer_from_value(out: &mut Vec<u8>, v: &Value) {
    match v {
        Value::Array(items, ..) | Value::Slip(items) => {
            for item in items.iter() {
                if let Some(b) = value_to_byte(item) {
                    out.push(b);
                }
            }
        }
        Value::Instance { attributes, .. } => {
            if let Some(bytes_val) = attributes.get("bytes") {
                extend_buffer_from_value(out, bytes_val);
            }
        }
        _ => {}
    }
}

fn value_to_byte(v: &Value) -> Option<u8> {
    match v {
        Value::Int(n) => Some((*n & 0xff) as u8),
        _ => None,
    }
}

/// Decode bytes using the appropriate encoding.
/// For utf8-c8, invalid bytes are preserved as synthetic codepoints.
/// For other encodings, invalid bytes are replaced with U+FFFD.
fn decode_bytes(bytes: &[u8], translate_nl: bool, encoding: &str) -> String {
    let s = if encoding == "utf8-c8" {
        crate::runtime::utf8_c8::decode_utf8_c8(bytes)
    } else {
        String::from_utf8_lossy(bytes).into_owned()
    };
    if translate_nl {
        s.replace("\r\n", "\n")
    } else {
        s
    }
}

/// Decode as many complete UTF-8 characters as possible, returning
/// (decoded_string, remaining_bytes).
fn decode_available(bytes: &[u8], encoding: &str) -> (String, Vec<u8>) {
    if encoding == "utf8-c8" {
        // utf8-c8 can always decode all bytes (invalid ones become synthetics)
        let s = crate::runtime::utf8_c8::decode_utf8_c8(bytes);
        return (s, Vec::new());
    }
    let mut end = bytes.len();
    while end > 0 {
        if std::str::from_utf8(&bytes[..end]).is_ok() {
            break;
        }
        end -= 1;
    }
    let s = std::str::from_utf8(&bytes[..end]).unwrap_or("").to_string();
    (s, bytes[end..].to_vec())
}

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
                attrs.insert("buffer".to_string(), Value::array(Vec::new()));
                let mut translate_nl = false;
                for arg in args {
                    if let Value::Pair(key, value) = arg
                        && key == "translate-nl"
                    {
                        translate_nl = value.truthy();
                    }
                }
                attrs.insert("translate-nl".to_string(), Value::Bool(translate_nl));
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
                let enc_lower = enc_name.to_lowercase();
                let is_ascii = matches!(enc_lower.as_str(), "ascii" | "us-ascii");
                let is_utf8_c8 = enc_lower == "utf8-c8";

                let mut bytes: Vec<Value> = Vec::new();
                if is_utf8_c8 {
                    for b in crate::runtime::utf8_c8::encode_utf8_c8(&input) {
                        bytes.push(Value::Int(b as i64));
                    }
                } else if is_ascii {
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
        attributes: &HashMap<String, Value>,
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
            "bytes-available" => {
                let buf = decoder_buffer(attributes);
                Value::Int(buf.len() as i64)
            }
            "is-empty" => {
                let buf = decoder_buffer(attributes);
                Value::Bool(buf.is_empty())
            }
            "WHAT" => Value::Package(Symbol::intern("Encoding::Decoder")),
            _ => Value::Nil,
        }
    }

    pub(in crate::runtime) fn native_encoding_decoder_mut(
        mut attributes: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "add-bytes" => {
                let mut buf = decoder_buffer(&attributes);
                if let Some(arg) = args.first() {
                    extend_buffer_from_value(&mut buf, arg);
                }
                attributes.insert(
                    "buffer".to_string(),
                    Value::array(buf.into_iter().map(|b| Value::Int(b as i64)).collect()),
                );
                Ok((Value::Nil, attributes))
            }
            "consume-all-chars" => {
                let buf = decoder_buffer(&attributes);
                let enc_name = attributes
                    .get("encoding")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let translate_nl = attributes
                    .get("translate-nl")
                    .map(|v| v.truthy())
                    .unwrap_or(false);
                let s = decode_bytes(&buf, translate_nl, &enc_name);
                attributes.insert("buffer".to_string(), Value::array(Vec::new()));
                Ok((Value::str(s), attributes))
            }
            "consume-available-chars" => {
                let buf = decoder_buffer(&attributes);
                let enc_name = attributes
                    .get("encoding")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let (decoded, remaining) = decode_available(&buf, &enc_name);
                let translate_nl = attributes
                    .get("translate-nl")
                    .map(|v| v.truthy())
                    .unwrap_or(false);
                let final_s = if translate_nl {
                    decoded.replace("\r\n", "\n")
                } else {
                    decoded
                };
                attributes.insert(
                    "buffer".to_string(),
                    Value::array(
                        remaining
                            .into_iter()
                            .map(|b| Value::Int(b as i64))
                            .collect(),
                    ),
                );
                Ok((Value::str(final_s), attributes))
            }
            "set-line-separators" => Ok((Value::Nil, attributes)),
            "bytes-available" => {
                let buf = decoder_buffer(&attributes);
                Ok((Value::Int(buf.len() as i64), attributes))
            }
            "is-empty" => {
                let buf = decoder_buffer(&attributes);
                Ok((Value::Bool(buf.is_empty()), attributes))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on 'Encoding::Decoder'",
                method
            ))),
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
