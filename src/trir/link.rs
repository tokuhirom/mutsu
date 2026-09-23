//! [`TrLink`]: a statically linked TRIR callee (#9072).

use super::TrChunk;
use crate::symbol::Symbol;

/// A callee resolved at compile time (ADR-0110 §3.3's static linkage): the
/// chunk itself, not a name to look it up by.
///
/// The link used to be only `key` + `fingerprint`, looked up at run time in
/// whatever `CompiledFns` the executing frame had in hand. For a routine in
/// a module that table is the routine's OWN nested-sub table
/// (`vm_call_dispatch.rs`), which does not contain its siblings — so every
/// statically linked call inside a module missed, bailed, and re-ran the
/// routine untyped (#9072). The chunk is immutable and the key names one
/// body, so holding it is exactly what "resolved at compile time" means.
///
/// Linking only ever points at a routine declared EARLIER in the same
/// compile (a forward or self reference is a generic call), so these `Arc`s
/// cannot form a cycle.
#[derive(Debug, Clone)]
pub(crate) struct TrLink {
    /// The callee's `CompiledFns` key, as its declaration produced it.
    pub(crate) key: Symbol,
    /// The callee's body fingerprint at compile time.
    pub(crate) fingerprint: u64,
    /// The callee's chunk.
    pub(crate) chunk: std::sync::Arc<TrChunk>,
    /// The package the callee's body resolves names in
    /// (`entry::trir_body_package`), settled when it was linked.
    pub(crate) pkg: Option<Symbol>,
}

impl TrLink {
    /// Link to `cf`, registered under `key`, when it has a chunk.
    pub(crate) fn to(key: Symbol, cf: &crate::opcode::CompiledFunction) -> Option<Self> {
        Some(TrLink {
            key,
            fingerprint: cf.fingerprint,
            chunk: cf.trir.clone()?,
            pkg: super::entry::trir_body_package(cf),
        })
    }

    /// Whether the table in hand still agrees with this link. A table that
    /// holds the key under a different body means the routine has been
    /// replaced since it was linked; a table that does not hold the key at
    /// all (a module routine's nested-sub table) has nothing to say, and the
    /// link stands.
    #[inline]
    pub(crate) fn current_in(&self, fns: &crate::opcode::CompiledFns) -> bool {
        fns.get(&self.key)
            .is_none_or(|cf| cf.fingerprint == self.fingerprint)
    }
}
