//! A byte stream that arrives in chunks, decoded as it arrives: the text
//! side of a `Proc::Async` output stream and of an `IO::Socket::Async`
//! connection's `Supply` (#11783). Rakudo hands both to the same decoder
//! (`Rakudo::Internals.BYTE_SUPPLY_DECODER`), so a chunk boundary is handled
//! exactly as the streaming decoder handles it:
//!
//! * a final grapheme a later chunk could extend (a combining mark) is held
//!   back, and delivered with the next chunk or alone once the stream ends;
//! * with `translate_nl`, a trailing `\r` is held until the byte after it is
//!   known, so the `\r\n` -> `\n` translation never sees a split pair;
//! * a malformed byte, or an incomplete sequence at the end of the stream, is
//!   the decoder's error, which ends the `Supply`.

use super::{Codec, DecoderConfig, StreamDecoder, TextQueues};
use crate::value::{BufBytes, RuntimeError};

/// One chunked stream's decoder state.
pub(crate) struct ChunkDecoder {
    cfg: DecoderConfig,
    bytes: BufBytes,
    queues: TextQueues,
}

impl ChunkDecoder {
    // Cost: O(1).
    pub(crate) fn new(codec: Codec, translate_nl: bool) -> Self {
        ChunkDecoder {
            cfg: DecoderConfig {
                codec,
                translate_nl,
                line_separators: Vec::new(),
            },
            bytes: BufBytes::new(),
            queues: TextQueues::default(),
        }
    }

    // Cost: O(1).
    fn decoder(&mut self) -> StreamDecoder<'_> {
        StreamDecoder {
            cfg: &self.cfg,
            bytes: &mut self.bytes,
            q: &mut self.queues,
        }
    }

    /// Add a chunk and take the text that is final now (possibly empty).
    // Cost: O(b + c), b = undecoded bytes, c = chars taken.
    pub(crate) fn feed(&mut self, chunk: &[u8]) -> Result<String, RuntimeError> {
        self.bytes.extend_from_slice(chunk);
        self.decoder().take_available_chars()
    }

    /// The stream has ended: take the held-back text. Bytes left over that do
    /// not form a whole character are an error.
    // Cost: O(b + c), b = undecoded bytes, c = chars taken.
    pub(crate) fn finish(&mut self) -> Result<String, RuntimeError> {
        self.decoder().take_all_chars()
    }
}
