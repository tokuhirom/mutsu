//! Resolving a TRIR routine's free variables — ADR-0110 §3.1.
//!
//! `nom-ws`'s `GetGlobal("ws")` runs per loop ITERATION today, and the lookup
//! it performs is three string-keyed hash probes plus two thread-local
//! interner hits. TRIR resolves the BINDING once per routine (memoized on the
//! interpreter, invalidated by `unit_lexical_gen`) and reads the binding's
//! value once per invocation.
//!
//! Reading once per invocation is exactly right only while nothing can write
//! the variable during the call. A body with no calls cannot — it has no way
//! to reach any other code — so Stage 1's snapshot was sound by construction.
//! A Stage 2 body can call out, so the snapshot is taken only for a free
//! variable that resolved to a shared CELL, whose reads follow the cell and
//! therefore see any write the call made. A free variable that resolves to a
//! plain environment value is re-resolved on each read instead, which needs
//! no per-chunk "does it call" flag: the distinction is a property of the
//! binding, not of the body.

use super::TrChunk;
use super::frame::TrFrame;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::Value;

impl Interpreter {
    /// Push this invocation's free variables onto the frame, answering
    /// whether all of them resolved.
    pub(crate) fn trir_seed_outers(&mut self, chunk: &TrChunk, frame: TrFrame) -> bool {
        debug_assert_eq!(
            self.trir.outers.len(),
            frame.outer_base as usize,
            "a frame's free variables are seeded before anything else pushes any"
        );
        if chunk.outers.is_empty() {
            return true;
        }
        let key = chunk.id;
        let cache_gen = self.unit_lexical_gen;
        // One probe on the hot path. The frame stacks borrow nothing from the
        // cache, so the entry can stay borrowed while the values are pushed.
        if let Some((g, bindings)) = self.trir_outer_cache.get(&key)
            && *g == cache_gen
        {
            for v in bindings {
                let v = super::entry::deref_cell(v);
                self.trir.outers.push(v);
            }
            return true;
        }
        let mut bindings = Vec::with_capacity(chunk.outers.len());
        let mut all_celled = true;
        for o in &chunk.outers {
            match self.trir_outer_binding(chunk.name, o.name.as_str()) {
                Some(v) => {
                    all_celled &= v.is_container_ref();
                    bindings.push(v);
                }
                None => return false,
            }
        }
        for v in &bindings {
            let v = super::entry::deref_cell(v);
            self.trir.outers.push(v);
        }
        // Only a shared CELL may be cached: caching a plain environment value
        // would freeze it. (The capture pass gives a mainline `my` a cell as
        // soon as a named sub reads it, so this is the uncommon shape.)
        if all_celled {
            self.trir_outer_cache.insert(key, (cache_gen, bindings));
        }
        true
    }

    /// Re-read this frame's free variables in place.
    ///
    /// Emitted after every call in a body that makes one: a callee may have
    /// written a free variable, and re-reading the bindings is the sound
    /// alternative to proving it did not. One cell read per CALL, where the
    /// untyped path pays a full by-name lookup per ACCESS.
    pub(crate) fn trir_reseed_outers(&mut self, chunk: &TrChunk, frame: TrFrame) -> bool {
        if chunk.outers.is_empty() {
            return true;
        }
        let base = frame.outer_base as usize;
        let key = chunk.id;
        if let Some((g, bindings)) = self.trir_outer_cache.get(&key)
            && *g == self.unit_lexical_gen
        {
            for (i, v) in bindings.iter().enumerate() {
                let v = super::entry::deref_cell(v);
                self.trir.outers[base + i] = v;
            }
            return true;
        }
        // The memo went stale (a compunit lexical was added somewhere), so
        // resolve again from scratch into the same region.
        let mut fresh = Vec::with_capacity(chunk.outers.len());
        for o in &chunk.outers {
            match self.trir_outer_binding(chunk.name, o.name.as_str()) {
                Some(v) => fresh.push(v),
                None => return false,
            }
        }
        for (i, v) in fresh.iter().enumerate() {
            let v = super::entry::deref_cell(v);
            self.trir.outers[base + i] = v;
        }
        true
    }

    /// The BINDING (the `unit_lexicals` cell, or the environment entry) a
    /// TRIR chunk's free variable names.
    ///
    /// Resolved against the CALLEE's own captured-lexical bucket (ADR-0024's
    /// `mainline_lexical_subs`), not against the running frame:
    /// `Interpreter::unit_lexical_slot` asks the routine stack which bucket
    /// is active, and TRIR pushes no routine frame, so it would answer for
    /// the caller. Asking by the callee's name is what the frame would have
    /// said, without the frame.
    fn trir_outer_binding(&self, callee: Symbol, name: &str) -> Option<Value> {
        if let Some(bucket) = self.mainline_lexical_subs.get(callee.as_str())
            && let Some(v) = self.unit_lexicals.get(bucket).and_then(|m| m.get(name))
        {
            return Some(v.clone());
        }
        // A free variable the capture pass did not put in a bucket is an
        // ordinary environment name (a mainline `my` the sub reads while the
        // mainline frame is still live). It may well be a plain value rather
        // than a cell, which is why the caller refuses to cache one that is
        // not celled.
        self.env().get(name).cloned()
    }
}
