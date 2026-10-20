use crate::runtime::*;
use crate::value::ValueView;

impl Interpreter {
    /// The owners whose rows answer an instance of the I/O and concurrency
    /// classes found by owner rather than by shape: `Proc::Async`, `Supply`,
    /// `IO::CatHandle`, `Supplier`, `IterationBuffer` (and user subclasses carrying
    /// their state) and the schedulers, the encoding objects and
    /// the handle subclasses (ADR-11276 §9.128, §9.130, §9.132, §9.133, §9.157).
    // Cost: O(1) for each class probed; O(d) for another instance, d = MRO depth.
    pub(crate) fn carried_state_owners_of(
        &mut self,
        target: &Value,
    ) -> Option<&'static [&'static str]> {
        crate::builtins::method_table::opaque_owners_of(target)
            .or_else(|| crate::builtins::method_table::carried_state_owners_of(target))
            .or_else(|| self.encoding_owners_of(target))
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
        crate::builtins::method_table::opaque_class_owners(&class).or_else(|| {
            self.is_stream_decoder_class(&class)
                .then_some(crate::builtins::method_table::ENCODING_DECODER_OWNERS)
        })
    }

    /// `Encoding::Encoder::Builtin.encode-chars`: `input` in the encoding
    /// `encoding`, a character it cannot represent replaced by `replacement`
    /// (`True` is `?`) or, without one, an error.
    // Cost: O(n + p + r), n = input bytes, p = replacement bytes, r = output bytes.
    pub(crate) fn encode_chars(
        &self,
        encoding: &str,
        replacement: Option<&Value>,
        input: &str,
    ) -> Result<Value, RuntimeError> {
        let replacement = Self::encoding_replacement(replacement);
        self.encode_string_buffer(input, encoding, replacement.as_deref())
    }
}
