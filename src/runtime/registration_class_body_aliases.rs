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
                raw:
                    Stmt::VarDecl {
                        name,
                        is_our: true,
                        constant,
                        ..
                    },
                ..
            } = op
            else {
                continue;
            };
            // Constants and legacy scalar forms still resolve through their
            // bare binding. Only ordinary package cells and aggregate stores
            // have the authoritative qualified home this cleanup preserves.
            if constant.is_some()
                || name.starts_with('&')
            {
                continue;
            }
            if !name.starts_with(['@', '%']) {
                let key = crate::qualified::qualified_text(cx.name, name);
                if !self
                    .get_our_var(key.as_str())
                    .is_some_and(Value::is_container_ref)
                {
                    continue;
                }
            }
            match cx.saved_env.get(name).cloned() {
                Some(previous) => {
                    self.env.insert(name.clone(), previous);
                }
                None => {
                    self.env.remove(name);
                }
            }
        }
    }
}
