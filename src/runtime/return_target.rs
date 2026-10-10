//! Which routine a non-local `return` inside a bare or pointy block targets.
//!
//! A block is not a return boundary: its `return` leaves the routine that
//! lexically encloses it. The signal is stamped with that routine's invocation
//! id from the block's captured env when it leaves the block
//! (`vm_closure_dispatch.rs`, `resolution_call_sub.rs`, the lazy `map`/`gather`
//! bridges).
//!
//! The callable id alone cannot carry that id through *nested* blocks:
//! every closure invocation, block or routine, rebinds it to its own id (`once`,
//! `leave` and flip-flop state key on the running block), so a block created
//! while an outer block runs captured the outer *block's* id. Its `return` then
//! targeted a callable no routine frame answers to, and escaped the routine as
//! "Attempt to return outside of immediately-enclosing Routine" — directly
//! (`sub f { { -> { return 5 }() }() }`) and for every `whenever` written in a
//! `supply { }` block, whose body is itself a block (#9630).
//!
//! So a block invocation also records the routine its own `return` targets,
//! tagged with the block's id. The tag makes the record self-invalidating: a
//! routine invoked from inside the block rebinds the callable id to its
//! own id, so the inherited record no longer matches and the routine's id wins.
//!
//! The ids live in the env's [`crate::env::FrameIds`], not in its name map
//! (ADR-12529 phase 1).

use crate::env::Env;

/// The invocation id a `return` executed in `env` (the running frame's env,
/// or a block's captured env) should target: the enclosing routine recorded
/// by [`record_block_return_target`] when `env` belongs to a running block,
/// otherwise the running callable's own id.
// Cost: O(1).
pub(crate) fn return_target_in_env(env: &Env) -> Option<u64> {
    let current = env.callable_id()? as u64;
    if let Some((owner, target)) = env.block_return()
        && owner == current
    {
        return Some(target);
    }
    Some(current)
}

/// Record, in the env of a block invocation `block_id` about to run, the
/// routine its `return` targets — the one its captured env `captured` resolves
/// to. Called before the block's own id is bound as its callable id.
// Cost: O(1).
pub(crate) fn record_block_return_target(env: &mut Env, captured: &Env, block_id: u64) {
    if let Some(target) = return_target_in_env(captured) {
        env.set_block_return(block_id, target);
    }
}
