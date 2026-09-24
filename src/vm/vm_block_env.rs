//! Block-scope env tiers (#9170).
//!
//! A lexical block used to save its env on entry (`self.env().clone()`, an
//! `Arc` bump that the block's first by-name write turned into an O(v) deep
//! copy) and, on exit, walk the *whole* current env against the saved copy to
//! decide what propagates. Both halves scaled with the number of names visible
//! in the enclosing scope rather than with what the block did.
//!
//! Instead the block now runs over a tier of its own ([`Env::open_block_tier`]):
//! its writes land in an initially empty overlay chained over the untouched
//! enclosing env, and the exit reads back only that overlay. When the tier
//! cannot be used (the chain is already at its depth cap), or the block body
//! replaced the env wholesale (a flatten, an unrelated env swap), the exit sees
//! the whole current env instead — exactly what it walked before — so the
//! fallback is the old behavior, not a guess.

use super::*;
use crate::env::{BlockTier, Env};

/// What a block saved of the env on entry. See [`Interpreter::open_block_env_tier`].
pub(crate) enum BlockEnvSave {
    /// The block runs over a tier of its own, chained over this enclosing env.
    Tier(std::sync::Arc<Env>),
    /// No tier (chain at its depth cap): a plain copy of the entry env.
    Whole(Env),
}

/// The env a block leaves behind, split for its exit merge.
pub(crate) struct ClosedBlockEnv {
    /// The enclosing env as it was on entry, to merge the block's writes into.
    pub(crate) base: Env,
    /// Every binding the block may have written. For a tier this is exactly its
    /// overlay; on the fallback path it is the whole current env.
    pub(crate) writes: BlockWrites,
    /// Keys the block removed that `base` still binds (tier path only; on the
    /// fallback path a removal is simply absent from `writes`).
    pub(crate) removed: Option<rustc_hash::FxHashSet<Symbol>>,
}

pub(crate) enum BlockWrites {
    Tier(std::sync::Arc<crate::env::Tier>),
    Whole(Env),
}

impl BlockWrites {
    pub(crate) fn iter(&self) -> std::collections::hash_map::Iter<'_, Symbol, Value> {
        match self {
            BlockWrites::Tier(tier) => tier.iter(),
            BlockWrites::Whole(env) => env.iter(),
        }
    }
}

impl Interpreter {
    /// Enter a block's env scope: run what follows over a fresh tier chained
    /// over the current env. Pair with [`Self::close_block_env_tier`].
    // Cost: O(1) (an `Env` handle clone and one `Arc` allocation).
    pub(crate) fn open_block_env_tier(&mut self) -> BlockEnvSave {
        let entry = self.env().clone();
        match Env::open_block_tier(entry) {
            Ok((child, parent)) => {
                *self.env_mut() = child;
                BlockEnvSave::Tier(parent)
            }
            Err(entry) => BlockEnvSave::Whole(entry),
        }
    }

    /// Leave a block's env scope opened by [`Self::open_block_env_tier`], taking
    /// the current env out. The caller merges `writes` into `base` and installs
    /// the result.
    // Cost: O(1) when the tier is intact and nothing else holds the enclosing
    // env; O(v) on the fallback path (the whole env is handed back as writes).
    pub(crate) fn close_block_env_tier(&mut self, save: BlockEnvSave) -> ClosedBlockEnv {
        let current = std::mem::replace(self.env_mut(), Env::empty_placeholder());
        match save {
            BlockEnvSave::Tier(parent) => match current.close_block_tier(parent) {
                Ok(BlockTier {
                    overlay,
                    tombstones,
                    parent,
                }) => ClosedBlockEnv {
                    base: parent,
                    writes: BlockWrites::Tier(overlay),
                    removed: tombstones,
                },
                Err(replaced) => {
                    let (current, parent) = *replaced;
                    ClosedBlockEnv {
                        base: parent,
                        writes: BlockWrites::Whole(current),
                        removed: None,
                    }
                }
            },
            BlockEnvSave::Whole(saved) => ClosedBlockEnv {
                base: saved,
                writes: BlockWrites::Whole(current),
                removed: None,
            },
        }
    }
}
