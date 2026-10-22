//! How deep a recursive walk over a RakuAST tree may go.

/// Whether a walk `depth` levels down has nested too far. A thread with a
/// native stack floor is bounded by its real headroom, so an ordinary program
/// (a few hundred `~` operands in one expression, `roast/S32-num/int.t`) is
/// not refused by an arbitrary counter; a thread without one keeps
/// `unguarded_limit`.
// Cost: O(1).
pub(super) fn exceeded(depth: usize, unguarded_limit: usize) -> bool {
    if crate::native_stack::stack_floor_armed() {
        crate::native_stack::headroom_exhausted()
    } else {
        depth >= unguarded_limit
    }
}
