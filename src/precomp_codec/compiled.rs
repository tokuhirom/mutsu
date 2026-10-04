//! Hand-written codecs for the compiled-code containers (ADR-11756 §2.4).
//!
//! `CompiledCode` and `CompiledFunction` hold run-time caches (`OnceLock`
//! indexes, inline-cache site tables, the JIT state, the memo cache) next to
//! the compiled data, and bincode's derive cannot skip a field. Their codecs are
//! therefore written out field by field. Both destructure `self` without a `..`
//! rest pattern, so adding a field to either struct is a compile error here
//! until it is given a codec line.
//!
//! - Run-time caches are not encoded, and decode as their empty default, as a
//!   freshly compiled chunk has them.
//! - Compile-time scaffolding that a finished chunk has already emptied
//!   (`const_index`, `pending_declared_constraints`) must be empty to encode.
//! - Hash sets and maps are encoded in sorted order, so encoding is
//!   deterministic and a round trip reproduces the same bytes.
//! - AST fragments (`stmt_pool`, parameter definitions, doc comments) reuse
//!   their serde implementations.

mod code;
mod function;
mod trir;

use super::DecodeCtx;
use crate::opcode::{CompiledCode, CompiledFns, CompiledFunction};
use crate::symbol::Symbol;
use bincode::de::Decoder;
use bincode::enc::Encoder;
use bincode::error::{DecodeError, EncodeError};
use bincode::serde::Compat;
use bincode::{Decode, Encode};
use std::sync::Arc;

/// Encode a module's compiled mainline and its routine table.
// Cost: O(n), n = size of the compiled code.
pub(crate) fn encode_compiled(
    code: &CompiledCode,
    fns: &CompiledFns,
) -> Result<Vec<u8>, EncodeError> {
    super::encode(&(code, fns))
}

/// Decode what [`encode_compiled`] wrote.
// Cost: O(n), n = size of the compiled code.
pub(crate) fn decode_compiled(bytes: &[u8]) -> Result<(CompiledCode, CompiledFns), DecodeError> {
    super::decode(bytes)
}

/// Write a set's length, then its elements in `order`.
// Cost: O(k log k), k = elements.
fn encode_sorted<'a, T: Encode + 'a, E: Encoder>(
    items: impl Iterator<Item = &'a T>,
    order: impl Fn(&&T, &&T) -> std::cmp::Ordering,
    encoder: &mut E,
) -> Result<(), EncodeError> {
    let mut items: Vec<&T> = items.collect();
    items.sort_by(order);
    (items.len() as u64).encode(encoder)?;
    for item in items {
        item.encode(encoder)?;
    }
    Ok(())
}

// Cost: O(k log k), k = elements.
fn encode_sym_set<E: Encoder>(
    set: &rustc_hash::FxHashSet<Symbol>,
    encoder: &mut E,
) -> Result<(), EncodeError> {
    encode_sorted(set.iter(), |a, b| a.as_str().cmp(b.as_str()), encoder)
}

// Cost: O(k log k), k = elements.
fn encode_u32_set<E: Encoder>(
    set: &std::collections::HashSet<u32>,
    encoder: &mut E,
) -> Result<(), EncodeError> {
    encode_sorted(set.iter(), |a, b| a.cmp(b), encoder)
}

// Cost: O(k log k), k = elements.
fn encode_string_set<E: Encoder>(
    set: &std::collections::HashSet<String>,
    encoder: &mut E,
) -> Result<(), EncodeError> {
    encode_sorted(set.iter(), |a, b| a.cmp(b), encoder)
}

// Cost: O(k), k = elements.
fn decode_set<T, S, D>(decoder: &mut D) -> Result<std::collections::HashSet<T, S>, DecodeError>
where
    T: Decode<DecodeCtx> + Eq + std::hash::Hash,
    S: std::hash::BuildHasher + Default,
    D: Decoder<Context = DecodeCtx>,
{
    let len = u64::decode(decoder)? as usize;
    let mut set = std::collections::HashSet::with_capacity_and_hasher(len, S::default());
    for _ in 0..len {
        set.insert(T::decode(decoder)?);
    }
    Ok(set)
}

type ProtectedSlots = rustc_hash::FxHashMap<u32, Box<[(Symbol, u32)]>>;

// Cost: O(k log k), k = entries.
fn encode_map<E: Encoder>(map: &ProtectedSlots, encoder: &mut E) -> Result<(), EncodeError> {
    let mut entries: Vec<(&u32, &Box<[(Symbol, u32)]>)> = map.iter().collect();
    entries.sort_by_key(|(k, _)| **k);
    (entries.len() as u64).encode(encoder)?;
    for (k, v) in entries {
        k.encode(encoder)?;
        v.encode(encoder)?;
    }
    Ok(())
}

// Cost: O(k), k = entries.
fn decode_map<D: Decoder<Context = DecodeCtx>>(
    decoder: &mut D,
) -> Result<ProtectedSlots, DecodeError> {
    let len = u64::decode(decoder)? as usize;
    let mut map = ProtectedSlots::default();
    for _ in 0..len {
        let k = u32::decode(decoder)?;
        let v = Decode::decode(decoder)?;
        map.insert(k, v);
    }
    Ok(map)
}

// Cost: O(n), n = size of the value.
fn decode_serde<T: serde::de::DeserializeOwned, D: Decoder<Context = DecodeCtx>>(
    decoder: &mut D,
) -> Result<T, DecodeError> {
    let Compat(value) = Compat::<T>::decode(decoder)?;
    Ok(value)
}

// Cost: O(1).
fn require_empty(empty: bool, what: &'static str) -> Result<(), EncodeError> {
    if empty {
        Ok(())
    } else {
        Err(EncodeError::OtherString(format!(
            "{what} is not empty in a finished chunk"
        )))
    }
}

/// The routine table, sorted by key; each routine is decoded into a fresh
/// table, which mints its own version token.
impl Encode for CompiledFns {
    // Cost: O(n), n = size of the routines.
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let mut entries: Vec<(&Symbol, &Arc<CompiledFunction>)> = self.iter().collect();
        entries.sort_by(|a, b| a.0.as_str().cmp(b.0.as_str()));
        (entries.len() as u64).encode(encoder)?;
        for (key, func) in entries {
            key.encode(encoder)?;
            func.as_ref().encode(encoder)?;
        }
        Ok(())
    }
}

impl Decode<DecodeCtx> for CompiledFns {
    // Cost: O(n), n = size of the routines.
    fn decode<D: Decoder<Context = DecodeCtx>>(decoder: &mut D) -> Result<Self, DecodeError> {
        let len = u64::decode(decoder)? as usize;
        let mut fns = CompiledFns::default();
        for _ in 0..len {
            let key = Symbol::decode(decoder)?;
            let func = CompiledFunction::decode(decoder)?;
            fns.insert_shared(key, Arc::new(func));
        }
        Ok(fns)
    }
}
bincode::impl_borrow_decode_with_context!(CompiledFns, DecodeCtx);
