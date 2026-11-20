//! The peeking short-circuit chains: `[||]` and `[//]` reductions.
//!
//! `JumpIfTrue` and `JumpIfNotNil` test the top of the stack without popping
//! it, so a chain needs no `Dup` and no cleanup block: an element that decides
//! the chain stays on the stack as its value, and one that does not is popped
//! before the next element is evaluated.

use super::*;

impl Compiler {
    /// Compile `items` as a chain that stops at the first element `jump`
    /// (a peeking `JumpIfTrue(0)` or `JumpIfNotNil(0)`) takes; the last
    /// element is the value when none does. `items` is not empty.
    pub(super) fn compile_peeking_short_circuit(&mut self, items: &[Expr], jump: OpCode) {
        let mut jumps = Vec::new();
        let last_idx = items.len() - 1;
        for (i, item) in items.iter().enumerate() {
            self.compile_expr(item);
            if i < last_idx {
                jumps.push(self.code.emit(jump.clone()));
                self.code.emit(OpCode::Pop);
            }
        }
        for j in jumps {
            self.code.patch_jump(j);
        }
    }
}
