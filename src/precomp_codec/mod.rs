//! The encoding of compiled bytecode for the precompilation cache
//! (ADR-11756 §2.4).
//!
//! Plain data is encoded by bincode 2's derive. The boundaries where the
//! policy lives are written here by hand:
//!
//! - **`Symbol`** is an index into a per-entry string table. Encoding collects
//!   the table in a thread-local scoped to one [`encode`] call (bincode's
//!   `Encode` has no context argument). Decoding interns every table entry once
//!   and hands the resulting symbols to the decoder as its context
//!   ([`DecodeCtx`]), so a name used a thousand times is interned once per load.
//! - **`Value`** is encoded through [`PortableValue`], which refuses a value that
//!   carries an object identity (ADR-11756 §2.3). A refusal makes the whole
//!   entry uncacheable, and the module keeps today's compile-on-load path.
//! - **Runtime caches** inside `CompiledCode` / `CompiledFunction` are not
//!   encoded; those types have hand-written codecs that rebuild them empty.
//!
//! The format is private to one build: the precompilation cache already
//! discards every entry when the binary changes, so nothing here is versioned
//! beyond that.

mod compiled;
mod sites;

use crate::symbol::Symbol;
use crate::value::{PortableValue, Value};
use bincode::de::{BorrowDecoder, Decoder};
use bincode::enc::Encoder;
use bincode::error::{DecodeError, EncodeError};
use bincode::{BorrowDecode, Decode, Encode};
use std::cell::RefCell;

pub(crate) use compiled::{decode_compiled, encode_compiled};

/// What the decoder carries: the entry's symbol table, already interned.
pub(crate) struct DecodeCtx {
    symbols: Vec<Symbol>,
}

/// The string table an [`encode`] call is building.
#[derive(Default)]
struct EncodeTable {
    index: rustc_hash::FxHashMap<Symbol, u32>,
    strings: Vec<&'static str>,
}

thread_local! {
    static ENCODE_TABLE: RefCell<Option<EncodeTable>> = const { RefCell::new(None) };
}

/// bincode configuration shared by both directions.
fn config() -> impl bincode::config::Config {
    bincode::config::standard()
}

/// Encode `value` as `[symbol table][payload]`.
///
/// Fails if anything inside refuses to be cached (an identity-carrying
/// constant), in which case the caller stores no compiled section.
// Cost: O(n), n = size of the encoded value.
pub(crate) fn encode<T: Encode>(value: &T) -> Result<Vec<u8>, EncodeError> {
    struct Reset;
    impl Drop for Reset {
        fn drop(&mut self) {
            ENCODE_TABLE.with(|t| *t.borrow_mut() = None);
        }
    }
    ENCODE_TABLE.with(|t| {
        let mut t = t.borrow_mut();
        assert!(t.is_none(), "precomp_codec::encode is not reentrant");
        *t = Some(EncodeTable::default());
    });
    let _reset = Reset;
    let payload = bincode::encode_to_vec(value, config())?;
    let strings =
        ENCODE_TABLE.with(|t| t.borrow_mut().take().map(|t| t.strings).unwrap_or_default());
    let mut out = bincode::encode_to_vec(&strings, config())?;
    out.extend_from_slice(&payload);
    Ok(out)
}

/// Decode a value written by [`encode`].
// Cost: O(n + s), n = size of the payload, s = symbols in the table (each
// interned once).
pub(crate) fn decode<T: Decode<DecodeCtx>>(bytes: &[u8]) -> Result<T, DecodeError> {
    let (strings, used): (Vec<String>, usize) = bincode::decode_from_slice(bytes, config())?;
    let ctx = DecodeCtx {
        symbols: strings.iter().map(|s| Symbol::intern(s)).collect(),
    };
    let (value, _) = bincode::decode_from_slice_with_context(&bytes[used..], config(), ctx)?;
    Ok(value)
}

impl Encode for Symbol {
    // Cost: O(1) amortized (one hash probe into the entry's table).
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let idx = ENCODE_TABLE.with(|t| {
            let mut t = t.borrow_mut();
            let t = t.as_mut().ok_or(EncodeError::Other(
                "Symbol encoded outside precomp_codec::encode",
            ))?;
            let next = t.strings.len() as u32;
            let idx = *t.index.entry(*self).or_insert(next);
            if idx == next {
                t.strings.push(self.as_str());
            }
            Ok::<u32, EncodeError>(idx)
        })?;
        idx.encode(encoder)
    }
}

impl Decode<DecodeCtx> for Symbol {
    // Cost: O(1).
    fn decode<D: Decoder<Context = DecodeCtx>>(decoder: &mut D) -> Result<Self, DecodeError> {
        let idx = u32::decode(decoder)? as usize;
        decoder
            .context()
            .symbols
            .get(idx)
            .copied()
            .ok_or(DecodeError::Other("symbol index out of range"))
    }
}
bincode::impl_borrow_decode_with_context!(Symbol, DecodeCtx);

impl Encode for Value {
    // Cost: O(n), n = size of the value.
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let portable = PortableValue::from_value(self).map_err(EncodeError::OtherString)?;
        bincode::serde::Compat(portable).encode(encoder)
    }
}

impl<C> Decode<C> for Value {
    // Cost: O(n), n = size of the value.
    fn decode<D: Decoder<Context = C>>(decoder: &mut D) -> Result<Self, DecodeError> {
        let bincode::serde::Compat(portable): bincode::serde::Compat<PortableValue> =
            Decode::decode(decoder)?;
        Ok(portable.into_value())
    }
}

impl<'de, C> BorrowDecode<'de, C> for Value {
    fn borrow_decode<D: BorrowDecoder<'de, Context = C>>(
        decoder: &mut D,
    ) -> Result<Self, DecodeError> {
        Decode::decode(decoder)
    }
}

/// `MUTSU_PRECOMP_ROUNDTRIP=1`: every compile's result is encoded, decoded and
/// re-encoded, the two encodings must match byte for byte, and the decoded copy
/// is what runs. Running a suite this way checks that the codec loses nothing
/// the program can observe, before any entry is written to disk (ADR-11756 §5,
/// step 2). A compile the codec refuses (an identity-carrying constant) runs as
/// compiled.
// Cost: O(1) after the first call.
pub(crate) fn roundtrip_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_PRECOMP_ROUNDTRIP").is_ok_and(|v| v != "0"))
}

/// See [`roundtrip_enabled`].
// Cost: O(n), n = size of the compiled code.
pub(crate) fn roundtrip(
    code: crate::opcode::CompiledCode,
    fns: crate::opcode::CompiledFns,
) -> (crate::opcode::CompiledCode, crate::opcode::CompiledFns) {
    let Ok(first) = encode_compiled(&code, &fns) else {
        return (code, fns);
    };
    let decoded = decode_compiled(&first)
        .unwrap_or_else(|e| panic!("precomp codec: cannot decode its own encoding: {e}"));
    let second = encode_compiled(&decoded.0, &decoded.1)
        .unwrap_or_else(|e| panic!("precomp codec: cannot re-encode a decoded chunk: {e}"));
    if first != second {
        let at = first
            .iter()
            .zip(&second)
            .position(|(a, b)| a != b)
            .unwrap_or(0);
        let lo = at.saturating_sub(48);
        panic!(
            "precomp codec: a round trip changed the encoding ({} vs {} bytes), first difference at {at}:\n{:?}\n{:?}",
            first.len(),
            second.len(),
            String::from_utf8_lossy(&first[lo..(at + 48).min(first.len())]),
            String::from_utf8_lossy(&second[lo..(at + 48).min(second.len())]),
        );
    }
    decoded
}

#[cfg(test)]
mod tests;
