//! Identities that own persistent routine/block state.

use crate::symbol::Symbol;

/// The counters for callable clones and invocations, and the interned type
/// IDs, are separate domains. They must never alias in the state store.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum StateScopeId {
    Routine(u64),
    Block(u64),
    Invocation(u64),
    /// A type's interned symbol ID, used only for equality and hashing.
    Type(u32),
}

impl From<u64> for StateScopeId {
    fn from(id: u64) -> Self {
        Self::Routine(id)
    }
}

impl StateScopeId {
    pub(crate) fn for_callable(id: u64, is_block: bool) -> Self {
        if is_block {
            Self::Block(id)
        } else {
            Self::Routine(id)
        }
    }
}

pub(crate) type StateKey = (Symbol, Option<StateScopeId>);
