use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// The owners whose rows answer an instance of the I/O and concurrency
    /// classes found by owner rather than by shape: `Proc::Async`, `Supply`,
    /// `IO::CatHandle`, `Supplier` and the schedulers, the encoding objects and
    /// the handle subclasses (ADR-11276 §9.128, §9.130, §9.132, §9.133, §9.157).
    // Cost: O(1) for each class probed; O(d) for another instance, d = MRO depth.
    pub(crate) fn io_concurrency_owners_of(
        &mut self,
        target: &Value,
    ) -> Option<&'static [&'static str]> {
        crate::builtins::method_table::proc_async_owners_of(target)
            .or_else(|| crate::builtins::method_table::supply_owners_of(target))
            .or_else(|| crate::builtins::method_table::cathandle_owners_of(target))
            .or_else(|| crate::builtins::method_table::supplier_scheduler_owners_of(target))
            .or_else(|| self.encoding_owners_of(target))
            .or_else(|| crate::builtins::method_table::handle_owners_of_instance(target))
    }

    /// The owners whose rows answer an encoding object (ADR-11276 §9.156):
    /// `Encoding::Builtin`, its encoder and its decoder, and the decoder for a
    /// user subclass of it.
    // Cost: O(1) for one of the three classes; O(d) for another instance,
    // d = MRO depth.
    pub(crate) fn encoding_owners_of(&mut self, target: &Value) -> Option<&'static [&'static str]> {
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let class = class_name.resolve();
        crate::builtins::method_table::encoding_owners_of(&class).or_else(|| {
            self.is_stream_decoder_class(&class)
                .then_some(crate::builtins::method_table::ENCODING_DECODER_OWNERS)
        })
    }

    /// `Encoding::Encoder::Builtin.encode-chars`: `input` in the encoding
    /// `encoding`, a character it cannot represent replaced by `replacement`
    /// (`True` is `?`) or, without one, an error.
    // Cost: O(n), n = bytes of the encoding.
    pub(crate) fn encode_chars(
        encoding: &str,
        replacement: Option<&Value>,
        input: &str,
    ) -> Result<Value, RuntimeError> {
        let enc_lower = encoding.to_lowercase();
        let mut bytes: Vec<Value> = Vec::new();
        match enc_lower.as_str() {
            "utf8-c8" => {
                for b in crate::runtime::utf8_c8::encode_utf8_c8(input) {
                    bytes.push(Value::int(b as i64));
                }
            }
            "ascii" | "us-ascii" => {
                for ch in input.chars() {
                    if ch as u32 <= 127 {
                        bytes.push(Value::int(ch as u32 as i64));
                        continue;
                    }
                    let Some(repl) = replacement else {
                        return Err(RuntimeError::new(format!(
                            "Cannot encode character '{}' (U+{:04X}) in ASCII",
                            ch, ch as u32
                        )));
                    };
                    let repl_str = if matches!(repl.view(), ValueView::Bool(true)) {
                        "?".to_string()
                    } else {
                        repl.to_string_value()
                    };
                    for b in repl_str.bytes() {
                        bytes.push(Value::int(b as i64));
                    }
                }
            }
            _ => {
                for b in input.as_bytes() {
                    bytes.push(Value::int(*b as i64));
                }
            }
        }
        // A UTF-8 encoder answers a `utf8` buffer, as Rakudo's does.
        let class = if matches!(enc_lower.as_str(), "utf8" | "utf-8") {
            "utf8"
        } else {
            "Blob[uint8]"
        };
        Ok(crate::value::value_buf::make_buf(Symbol::intern(class), bytes))
    }
}
