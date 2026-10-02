//! `OpCode::MarkArrayShareSource`: the mark a `$s = $src` store carries so
//! that `$s = @a` / `$s = %h` (and a chained `$r = $q` whose `$q` holds one)
//! shares the source container by reference (`docs/scalar-array-sharing.md`,
//! Slice 2a/2b). One implementation, shared by the interpreter's dispatch arm
//! and the JIT's dedicated shim.

use super::*;

impl Interpreter {
    /// Set the array-share mark for the store that follows, naming the
    /// source variable `code.constants[name_idx]`.
    ///
    /// The RHS is already on the stack, and both consumers (`SetLocal`,
    /// `AssignExpr`) share only a value that derefs to an `Array`/`Hash`. For
    /// a value that certainly cannot (`Value::is_never_array_share_source`),
    /// the mark would be consumed as a no-op, so it is left unset: a pending
    /// mark sends the store down the full `SetLocal` cascade, and setting it
    /// copies the source name. That was most of a plain `$s = $i` store
    /// (#10955).
    // Cost: O(1), plus a copy of the name when the mark is set.
    pub(super) fn exec_mark_array_share_source_op(&mut self, code: &CompiledCode, name_idx: u32) {
        if self
            .stack
            .last()
            .is_some_and(Value::is_never_array_share_source)
        {
            return;
        }
        self.array_share_context().set(true);
        self.array_share_source()
            .set(Some(Self::const_str(code, name_idx).to_string()));
    }
}
