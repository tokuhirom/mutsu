//! Codecs for op payloads that carry a run-time resolution cache.

use super::DecodeCtx;
use crate::symbol::Symbol;
use crate::trir::class_operand::ClassOperandSite;
use bincode::de::Decoder;
use bincode::enc::Encoder;
use bincode::error::{DecodeError, EncodeError};
use bincode::{Decode, Encode};

/// The site's identity only; its resolution cache starts cold, as a freshly
/// compiled (or cloned) site does.
impl Encode for ClassOperandSite {
    // Cost: O(1).
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        self.name.encode(encoder)?;
        self.effect_only.encode(encoder)
    }
}

impl Decode<DecodeCtx> for ClassOperandSite {
    // Cost: O(1).
    fn decode<D: Decoder<Context = DecodeCtx>>(decoder: &mut D) -> Result<Self, DecodeError> {
        let name = Symbol::decode(decoder)?;
        let effect_only = bool::decode(decoder)?;
        Ok(if effect_only {
            ClassOperandSite::new(name)
        } else {
            ClassOperandSite::term(name)
        })
    }
}
bincode::impl_borrow_decode_with_context!(ClassOperandSite, DecodeCtx);

impl Encode for crate::static_str::StaticStr {
    // Cost: O(1) amortized (one symbol-table probe).
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        Symbol::intern(self.0).encode(encoder)
    }
}

impl Decode<DecodeCtx> for crate::static_str::StaticStr {
    // Cost: O(1).
    fn decode<D: Decoder<Context = DecodeCtx>>(decoder: &mut D) -> Result<Self, DecodeError> {
        Ok(crate::static_str::StaticStr(
            Symbol::decode(decoder)?.as_str(),
        ))
    }
}
bincode::impl_borrow_decode_with_context!(crate::static_str::StaticStr, DecodeCtx);
