//! Tier B inline emission for the METAOP_ASSIGN identity seed
//! (`OpCode::MetaAssignIdentity` and the seed half of
//! `OpCode::GetLocalMetaAssign`) — split out of `vm_jit_tier_b.rs` purely by
//! file size. See that module's header for the fast-path correctness contract.

use super::vm_jit_tier_b::TierB;
use cranelift_codegen::ir::{InstBuilder, types};
use cranelift_frontend::FunctionBuilder;

type CVal = cranelift_codegen::ir::Value;

impl TierB {
    /// `$x OP= $y` seeds an *undefined* `$x` with the operator's zero-argument
    /// value; for a concrete `$x` it must do nothing at all. Since `$i += 1` is
    /// the single most common compound assignment, the "does nothing" case is
    /// emitted inline as a tag test with no call: a small-Int, encoded-Num or
    /// Bool word is definitively concrete and falls straight through.
    ///
    /// Every other word shape — `Package` and `Nil` (which need the seed), a
    /// `ContainerRef` cell (whose *inner* value decides), and any heap value —
    /// goes to the shim, which re-runs the interpreter arm on the untouched
    /// stack. `slow_fn` must be the infallible (void) shim, so this is only
    /// used for the `Zero` / `One` identities; `/=` and `%=` always throw on an
    /// undefined container and keep the plain fallible call.
    pub(super) fn emit_meta_assign_identity(
        &self,
        b: &mut FunctionBuilder,
        codep: CVal,
        identity_code: u32,
        slow_fn: usize,
    ) {
        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let top_addr = self.slot_addr(b, ptr, len, 1);
        let wt = b.ins().load(types::I64, Self::mf(), top_addr, 0);
        let page = self.page(b, wt);
        let is_int = self.is_int_page(b, page);
        let is_num = self.is_num_page(b, page);
        let concrete = b.ins().bor(is_int, is_num);
        let slow = b.create_block();
        let done = b.create_block();
        b.ins().brif(concrete, done, &[], slow, &[]);

        b.switch_to_block(slow);
        let idv = b.ins().iconst(types::I32, identity_code as i64);
        let callee = b.ins().iconst(self.ptr_ty, slow_fn as i64);
        b.ins()
            .call_indirect(self.v_code_u32, callee, &[self.interp, codep, idv]);
        b.ins().jump(done, &[]);

        b.switch_to_block(done);
    }
}
