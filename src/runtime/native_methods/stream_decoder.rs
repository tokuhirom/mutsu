//! `Encoding::Decoder::Builtin` (#11503): its methods are rows
//! (`method_table::io_concurrency::encoding`, ADR-11276 §9.156) over the
//! routines the `nqp::decoder*` ops run ([`crate::runtime::stream_decoder_object`]).

use crate::runtime::Interpreter;
use crate::runtime::stream_decoder_object::DECODER_CLASS;

impl Interpreter {
    /// Whether `class_name` is (or inherits from) the built-in decoder.
    pub(in crate::runtime) fn is_stream_decoder_class(&mut self, class_name: &str) -> bool {
        class_name == DECODER_CLASS
            || self
                .class_mro(class_name)
                .iter()
                .any(|c| *c == DECODER_CLASS)
    }
}
