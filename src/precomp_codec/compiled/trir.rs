//! The codec of `TrChunk`: every field, in declaration order. Its `id` keys a
//! per-process cache, so decoding mints a fresh one, as compiling does; the
//! run-time `def_file` stamp starts empty.

use crate::precomp_codec::DecodeCtx;
use crate::trir::TrChunk;
use bincode::de::Decoder;
use bincode::enc::Encoder;
use bincode::error::{DecodeError, EncodeError};
use bincode::{Decode, Encode};

impl Encode for TrChunk {
    // Cost: O(n), n = size of the chunk.
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let TrChunk {
            id: _,
            ops,
            constants,
            n_native,
            n_obj,
            params,
            outers,
            name,
            calls,
            methods,
            def_file: _,
            captured_fatal_mode,
        } = self;
        ops.encode(encoder)?;
        constants.encode(encoder)?;
        n_native.encode(encoder)?;
        n_obj.encode(encoder)?;
        params.encode(encoder)?;
        outers.encode(encoder)?;
        name.encode(encoder)?;
        calls.encode(encoder)?;
        methods.encode(encoder)?;
        captured_fatal_mode.encode(encoder)?;
        Ok(())
    }
}

impl Decode<DecodeCtx> for TrChunk {
    // Cost: O(n), n = size of the chunk.
    fn decode<D: Decoder<Context = DecodeCtx>>(decoder: &mut D) -> Result<Self, DecodeError> {
        Ok(TrChunk {
            // A fresh identity: the per-chunk free-variable cache keys on it.
            id: crate::trir::next_chunk_id(),
            ops: Decode::decode(decoder)?,
            constants: Decode::decode(decoder)?,
            n_native: Decode::decode(decoder)?,
            n_obj: Decode::decode(decoder)?,
            params: Decode::decode(decoder)?,
            outers: Decode::decode(decoder)?,
            name: Decode::decode(decoder)?,
            calls: Decode::decode(decoder)?,
            methods: Decode::decode(decoder)?,
            def_file: Default::default(),
            captured_fatal_mode: Decode::decode(decoder)?,
        })
    }
}
bincode::impl_borrow_decode_with_context!(TrChunk, DecodeCtx);
