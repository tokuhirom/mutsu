use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

use super::state::SupplyEvent;
use crate::value::AttrMap;

fn decoder_buffer(attrs: &AttrMap) -> Vec<u8> {
    let mut out = Vec::new();
    if let Some(v) = attrs.get("buffer") {
        extend_buffer_from_value(&mut out, v);
    }
    out
}

fn extend_buffer_from_value(out: &mut Vec<u8>, v: &Value) {
    match v.view() {
        ValueView::Array(items, ..) => {
            for item in items.iter() {
                if let Some(b) = value_to_byte(item) {
                    out.push(b);
                }
            }
        }
        ValueView::Slip(items) => {
            for item in items.iter() {
                if let Some(b) = value_to_byte(item) {
                    out.push(b);
                }
            }
        }
        ValueView::Instance { attributes, .. } => {
            for item in crate::value::value_buf::buf_elems_or_empty(&attributes) {
                if let Some(b) = value_to_byte(&item) {
                    out.push(b);
                }
            }
        }
        _ => {}
    }
}

fn value_to_byte(v: &Value) -> Option<u8> {
    match v.view() {
        ValueView::Int(n) => Some((n & 0xff) as u8),
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
        attributes: &AttrMap,
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
                    if let ValueView::Pair(key, value) = arg.view()
                        && key == "replacement"
                    {
                        attrs.insert("replacement".to_string(), value.clone());
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
                    if let ValueView::Pair(key, value) = arg.view()
                        && key == "translate-nl"
                    {
                        translate_nl = value.truthy();
                    }
                }
                attrs.insert("translate-nl".to_string(), Value::truth(translate_nl));
                Value::make_instance(Symbol::intern("Encoding::Decoder"), attrs)
            }
            "gist" | "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("Encoding::Builtin<{}>", name))
            }
            "WHAT" => Value::package(Symbol::intern("Encoding::Builtin")),
            _ => Value::NIL,
        }
    }

    pub(in crate::runtime) fn native_encoding_encoder(
        attributes: &AttrMap,
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
                        bytes.push(Value::int(b as i64));
                    }
                } else if is_ascii {
                    for ch in input.chars() {
                        if ch as u32 > 127 {
                            if let Some(repl) = replacement {
                                let repl_str = if matches!(repl.view(), ValueView::Bool(true)) {
                                    // :replacement (Bool True) -> default replacement char '?'
                                    "?".to_string()
                                } else {
                                    repl.to_string_value()
                                };
                                for b in repl_str.bytes() {
                                    bytes.push(Value::int(b as i64));
                                }
                            } else {
                                return Err(RuntimeError::new(format!(
                                    "Cannot encode character '{}' (U+{:04X}) in ASCII",
                                    ch, ch as u32
                                )));
                            }
                        } else {
                            bytes.push(Value::int(ch as u32 as i64));
                        }
                    }
                } else {
                    // UTF-8 encoding
                    for b in input.as_bytes() {
                        bytes.push(Value::int(*b as i64));
                    }
                }

                Ok(crate::value::value_buf::make_buf(
                    Symbol::intern("Blob[uint8]"),
                    bytes,
                ))
            }
            "WHAT" => Ok(Value::package(Symbol::intern("Encoding::Encoder"))),
            _ => Ok(Value::NIL),
        }
    }

    pub(in crate::runtime) fn native_encoding_decoder(
        attributes: &AttrMap,
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
                Value::int(buf.len() as i64)
            }
            "is-empty" => {
                let buf = decoder_buffer(attributes);
                Value::truth(buf.is_empty())
            }
            "WHAT" => Value::package(Symbol::intern("Encoding::Decoder")),
            _ => Value::NIL,
        }
    }

    pub(in crate::runtime) fn native_encoding_decoder_mut(
        mut attributes: AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        match method {
            "add-bytes" => {
                let mut buf = decoder_buffer(&attributes);
                if let Some(arg) = args.first() {
                    extend_buffer_from_value(&mut buf, arg);
                }
                attributes.insert(
                    "buffer".to_string(),
                    Value::array(buf.into_iter().map(|b| Value::int(b as i64)).collect()),
                );
                Ok((Value::NIL, attributes))
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
                            .map(|b| Value::int(b as i64))
                            .collect(),
                    ),
                );
                Ok((Value::str(final_s), attributes))
            }
            "set-line-separators" => {
                // Store the separator strings for consume-line-chars.
                let seps: Vec<Value> = args
                    .first()
                    .map(|v| match v.view() {
                        ValueView::Array(items, ..) => items
                            .iter()
                            .map(|s| Value::str(s.to_string_value()))
                            .collect(),
                        _ => vec![Value::str(v.to_string_value())],
                    })
                    .unwrap_or_default();
                attributes.insert("line-separators".to_string(), Value::array(seps));
                Ok((Value::NIL, attributes))
            }
            "consume-line-chars" => {
                let mut chomp = false;
                let mut eof = false;
                for arg in &args {
                    if let ValueView::Pair(key, value) = arg.view() {
                        match &*key.to_string() {
                            "chomp" => chomp = value.truthy(),
                            "eof" => eof = value.truthy(),
                            _ => {}
                        }
                    }
                }
                let separators: Vec<String> =
                    match attributes.get("line-separators").map(Value::view) {
                        Some(ValueView::Array(items, ..)) if !items.is_empty() => {
                            items.iter().map(|s| s.to_string_value()).collect()
                        }
                        _ => vec!["\r\n".to_string(), "\n".to_string()],
                    };
                let buf = decoder_buffer(&attributes);
                let enc_name = attributes
                    .get("encoding")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                // Find the earliest separator occurrence (byte-wise: the
                // separators are ASCII in practice). At equal positions the
                // longest separator wins, so "\r\n" is not split by "\n".
                let mut hit: Option<(usize, usize)> = None; // (index, sep_len)
                for sep in &separators {
                    let sep_bytes = sep.as_bytes();
                    if sep_bytes.is_empty() {
                        continue;
                    }
                    if let Some(idx) = buf.windows(sep_bytes.len()).position(|w| w == sep_bytes) {
                        let better = match hit {
                            None => true,
                            Some((best_idx, best_len)) => {
                                idx < best_idx || (idx == best_idx && sep_bytes.len() > best_len)
                            }
                        };
                        if better {
                            hit = Some((idx, sep_bytes.len()));
                        }
                    }
                }
                let translate_nl = attributes
                    .get("translate-nl")
                    .map(|v| v.truthy())
                    .unwrap_or(false);
                match hit {
                    Some((idx, sep_len)) => {
                        let end = if chomp { idx } else { idx + sep_len };
                        let line = decode_bytes(&buf[..end], translate_nl, &enc_name);
                        let rest: Vec<Value> = buf[idx + sep_len..]
                            .iter()
                            .map(|b| Value::int(*b as i64))
                            .collect();
                        attributes.insert("buffer".to_string(), Value::array(rest));
                        Ok((Value::str(line), attributes))
                    }
                    None if eof && !buf.is_empty() => {
                        let line = decode_bytes(&buf, translate_nl, &enc_name);
                        attributes.insert("buffer".to_string(), Value::array(Vec::new()));
                        Ok((Value::str(line), attributes))
                    }
                    // No complete line yet: an undefined Str.
                    None => Ok((Value::package(Symbol::intern("Str")), attributes)),
                }
            }
            "consume-exactly-bytes" => {
                let n = args
                    .first()
                    .and_then(|v| v.as_int())
                    .and_then(|n| usize::try_from(n).ok())
                    .unwrap_or(0);
                let buf = decoder_buffer(&attributes);
                if buf.len() < n {
                    // Not enough bytes buffered: an undefined Blob, buffer kept.
                    return Ok((Value::package(Symbol::intern("Blob")), attributes));
                }
                let taken: Vec<Value> = buf[..n].iter().map(|b| Value::int(*b as i64)).collect();
                let rest: Vec<Value> = buf[n..].iter().map(|b| Value::int(*b as i64)).collect();
                attributes.insert("buffer".to_string(), Value::array(rest));
                Ok((
                    crate::value::value_buf::make_buf(Symbol::intern("Buf[uint8]"), taken),
                    attributes,
                ))
            }
            "bytes-available" => {
                let buf = decoder_buffer(&attributes);
                Ok((Value::int(buf.len() as i64), attributes))
            }
            "is-empty" => {
                let buf = decoder_buffer(&attributes);
                Ok((Value::truth(buf.is_empty()), attributes))
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
        rx: &super::supply_channel::SupplyReceiver,
        cb: &Value,
        delay_seconds: f64,
    ) {
        use std::io::Write;
        while let Ok(SupplyEvent::Emit(value)) = rx.recv() {
            Self::sleep_for_supply_delay(delay_seconds);
            let result = interp.call_sub_value(cb.clone(), vec![value], true);
            // Flush stdout (check both the per-interpreter buffer and the
            // shared thread output buffer used by thread clones).
            if !interp.output_sink().output.is_empty() {
                print!("{}", interp.output_sink().output);
                let _ = std::io::stdout().flush();
                interp.output_sink_mut().output.clear();
            }
            if let Some(ref shared) = interp.output_sink().shared_thread_output {
                let drained = std::mem::take(&mut *shared.lock().unwrap());
                if !drained.is_empty() {
                    print!("{}", drained);
                    let _ = std::io::stdout().flush();
                }
            }
            // Flush stderr
            if !interp.output_sink().stderr_output.is_empty() {
                eprint!("{}", interp.output_sink().stderr_output);
                let _ = std::io::stderr().flush();
                interp.output_sink_mut().stderr_output.clear();
            }
            if let Some(ref shared) = interp.output_sink().shared_thread_stderr {
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
