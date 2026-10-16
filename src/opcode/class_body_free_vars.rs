//! The free variables of a class body's statement chunks, as seen by the code
//! that declares the class.
use super::*;

impl CompiledCode {
    /// The precompiled chunks of every statement the class bodies declared in
    /// this code run when the class is declared (`$log ~= 'body;'` in `sub f
    /// { my class K { $log ~= 'body;' } }`). Each chunk is compiled as its
    /// own code, so the declaring code's op scan never sees the names it
    /// uses.
    // Cost: O(c), c = statements across this code's class bodies.
    pub(super) fn class_body_chunk_codes(&self) -> impl Iterator<Item = &CompiledCode> {
        self.class_decl_plans
            .iter()
            .flat_map(|plan| plan.body_plan.iter())
            .filter_map(|op| match op {
                ClassBodyOp::ClassSub { chunk, .. }
                | ClassBodyOp::CodeAlias { chunk, .. }
                | ClassBodyOp::ProtoMethod { chunk, .. }
                | ClassBodyOp::LeavePhaser { chunk, .. }
                | ClassBodyOp::Other { chunk, .. } => chunk.as_ref(),
                _ => None,
            })
            .map(|chunk| chunk.code.as_ref())
    }
}
