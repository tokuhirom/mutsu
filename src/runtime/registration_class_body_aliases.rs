//! Class-body `our` declarations create lexical aliases to package storage.

use super::registration_class_body::ClassBodyCx;
use super::*;

impl Interpreter {
    /// The package variable survives in its canonical qualified store; its
    /// bare alias ends with the class body, like a class-body `my` binding.
    // Cost: O(n), n = top-level operations in the compiled class-body plan.
    pub(super) fn restore_class_body_our_aliases(
        &mut self,
        cx: &ClassBodyCx<'_>,
        plan: &[crate::opcode::ClassBodyOp],
    ) {
        for op in plan {
            let crate::opcode::ClassBodyOp::Other {
                raw: Stmt::VarDecl { name, is_our: true, .. },
                ..
            } = op else {
                continue;
            };
            match cx.saved_env.get(name).cloned() {
                Some(previous) => { self.env.insert(name.clone(), previous); }
                None => { self.env.remove(name); }
            }
        }
    }
}
