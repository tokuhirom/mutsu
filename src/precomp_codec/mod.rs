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

pub(crate) use compiled::diff::first_difference;
pub(crate) use compiled::{decode_compiled, encode_compiled};

/// What the decoder carries: the entry's symbol table, already interned, and
/// shared with every nested decode of the same entry.
pub(crate) struct DecodeCtx {
    symbols: std::rc::Rc<[Symbol]>,
}

/// The string table an [`encode`] call is building.
#[derive(Default)]
struct EncodeTable {
    index: rustc_hash::FxHashMap<Symbol, u32>,
    strings: Vec<&'static str>,
}

thread_local! {
    static ENCODE_TABLE: RefCell<Option<EncodeTable>> = const { RefCell::new(None) };
    /// The symbols of the entry being decoded, for a value that reaches the
    /// codec through a serde impl and so cannot see the decoder's context
    /// (see [`decode_nested`]).
    static DECODE_SYMBOLS: RefCell<Option<std::rc::Rc<[Symbol]>>> = const { RefCell::new(None) };
}

/// Whether an [`encode`] call is running on this thread.
// Cost: O(1).
pub(crate) fn encoding_active() -> bool {
    ENCODE_TABLE.with(|t| t.borrow().is_some())
}

/// Encode `value` inside a running [`encode`] call, sharing its symbol table:
/// for compiled data that sits inside a serde-encoded AST node (a parameter's
/// precompiled chunks).
// Cost: O(n), n = size of the value.
pub(crate) fn encode_nested<T: Encode>(value: &T) -> Result<Vec<u8>, EncodeError> {
    bincode::encode_to_vec(value, config())
}

/// Decode what [`encode_nested`] wrote, inside a running [`decode`] call.
/// `None` outside one, or if the bytes do not decode.
// Cost: O(n), n = size of the value.
pub(crate) fn decode_nested<T: Decode<DecodeCtx>>(bytes: &[u8]) -> Option<T> {
    let symbols = DECODE_SYMBOLS.with(|s| s.borrow().clone())?;
    let ctx = DecodeCtx { symbols };
    bincode::decode_from_slice_with_context(bytes, config(), ctx)
        .ok()
        .map(|(value, _)| value)
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
    let reentered = ENCODE_TABLE.with(|t| {
        let mut t = t.borrow_mut();
        let reentered = t.is_some();
        *t = Some(EncodeTable::default());
        reentered
    });
    if reentered {
        return Err(EncodeError::Other("precomp_codec::encode is not reentrant"));
    }
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
    let (strings, used): (Vec<&str>, usize) = bincode::borrow_decode_from_slice(bytes, config())?;
    let symbols: std::rc::Rc<[Symbol]> = strings.iter().map(|s| Symbol::intern(s)).collect();
    struct Restore(Option<std::rc::Rc<[Symbol]>>);
    impl Drop for Restore {
        fn drop(&mut self) {
            let outer = self.0.take();
            DECODE_SYMBOLS.with(|s| *s.borrow_mut() = outer);
        }
    }
    let outer = DECODE_SYMBOLS.with(|s| s.borrow_mut().replace(symbols.clone()));
    let _restore = Restore(outer);
    let ctx = DecodeCtx { symbols };
    let (value, _) = bincode::decode_from_slice_with_context(&bytes[used..], config(), ctx)?;
    Ok(value)
}

/// The table index of `sym` in the entry an [`encode`] call is building, or
/// `None` outside one. A `Symbol` that reaches the codec through a serde impl
/// (an AST fragment inside compiled code) serializes as this index instead of
/// its text, the way a natively encoded one does, so decoding it costs a table
/// lookup instead of an intern (see `Symbol`'s serde impls).
// Cost: O(1) amortized (one hash probe into the entry's table).
pub(crate) fn serde_symbol_index(sym: Symbol) -> Option<u32> {
    ENCODE_TABLE.with(|t| {
        let mut t = t.borrow_mut();
        let t = t.as_mut()?;
        Some(t.index_of(sym))
    })
}

/// Whether a [`decode`] call is running on this thread, so a serde-decoded
/// `Symbol` is a [`serde_symbol_index`] index rather than text.
// Cost: O(1).
pub(crate) fn serde_symbols_active() -> bool {
    DECODE_SYMBOLS.with(|s| s.borrow().is_some())
}

/// The symbol at `idx` of the entry a [`decode`] call is reading.
// Cost: O(1).
pub(crate) fn serde_symbol_at(idx: u32) -> Option<Symbol> {
    DECODE_SYMBOLS.with(|s| s.borrow().as_ref()?.get(idx as usize).copied())
}

impl EncodeTable {
    /// The index of `sym`, adding it to the table on first use.
    // Cost: O(1) amortized.
    fn index_of(&mut self, sym: Symbol) -> u32 {
        let next = self.strings.len() as u32;
        let idx = *self.index.entry(sym).or_insert(next);
        if idx == next {
            self.strings.push(sym.as_str());
        }
        idx
    }
}

impl Encode for Symbol {
    // Cost: O(1) amortized (one hash probe into the entry's table).
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let idx = ENCODE_TABLE.with(|t| {
            let mut t = t.borrow_mut();
            let t = t.as_mut().ok_or(EncodeError::Other(
                "Symbol encoded outside precomp_codec::encode",
            ))?;
            Ok::<u32, EncodeError>(t.index_of(*self))
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

/// See [`roundtrip_enabled`]. `Err` describes how the codec failed its own
/// round trip; a chunk it refuses to encode is returned unchanged.
// Cost: O(n), n = size of the compiled code.
pub(crate) fn roundtrip(
    code: crate::opcode::CompiledCode,
    fns: crate::opcode::CompiledFns,
) -> Result<(crate::opcode::CompiledCode, crate::opcode::CompiledFns), String> {
    let Ok(first) = encode_compiled(&code, &fns) else {
        return Ok((code, fns));
    };
    let decoded = decode_compiled(&first)
        .map_err(|e| format!("precomp codec: cannot decode its own encoding: {e}"))?;
    let second = encode_compiled(&decoded.0, &decoded.1)
        .map_err(|e| format!("precomp codec: cannot re-encode a decoded chunk: {e}"))?;
    if first != second {
        return Err(format!(
            "precomp codec: a round trip changed the encoding: {}",
            describe_difference(&first, &second)
        ));
    }
    Ok(decoded)
}

/// Where two encodings first differ, with the bytes around it, for a
/// diagnostic.
// Cost: O(n), n = encoding length.
pub(crate) fn describe_difference(a: &[u8], b: &[u8]) -> String {
    let at = a
        .iter()
        .zip(b)
        .position(|(x, y)| x != y)
        .unwrap_or(a.len().min(b.len()));
    let lo = at.saturating_sub(64);
    format!(
        "{} vs {} bytes, first difference at {at}:\n{:?}\n{:?}",
        a.len(),
        b.len(),
        String::from_utf8_lossy(&a[lo..(at + 64).min(a.len())]),
        String::from_utf8_lossy(&b[lo..(at + 64).min(b.len())]),
    )
}

#[cfg(test)]
#[path = "tests.rs"]
mod tests;
