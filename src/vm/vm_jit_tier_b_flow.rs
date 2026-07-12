//! Tier B conditional-branch emission (`JumpIfFalse` / `JumpIfTrue` Bool
//! fast paths) — the control-flow half of `vm_jit_tier_b.rs`, split out
//! purely by file size. See that module's header for the fast-path
//! correctness contract.

use super::vm_jit_tier_b::TierB;
use crate::value::jit_words as w;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, types};
use cranelift_frontend::FunctionBuilder;

impl TierB {
    /// `JumpIfFalse` with a Bool-word fast path: True pops and falls through,
    /// False pops, mirrors the interpreter's post-pop Failure marking, polls
    /// the safepoint on a backedge and jumps; every other word goes through
    /// the Tier A cond shim. Leaves the builder in the fall-through block.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn emit_jump_if_false(
        &self,
        b: &mut FunctionBuilder,
        target: cranelift_codegen::ir::Block,
        backedge: bool,
        safepoint_fn: usize,
        mark_fn: usize,
        cond_fn: usize,
    ) {
        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let top_addr = self.slot_addr(b, ptr, len, 1);
        let wt = b.ins().load(types::I64, Self::mf(), top_addr, 0);

        let pop_fall = b.create_block();
        let chk_false = b.create_block();
        let pop_take = b.create_block();
        let slow = b.create_block();
        let fall = b.create_block();

        let is_true = b.ins().icmp_imm(IntCC::Equal, wt, w::TRUE_BITS as i64);
        b.ins().brif(is_true, pop_fall, &[], chk_false, &[]);

        b.switch_to_block(pop_fall);
        self.adjust_len(b, len, -1);
        b.ins().jump(fall, &[]);

        b.switch_to_block(chk_false);
        let is_false = b.ins().icmp_imm(IntCC::Equal, wt, w::FALSE_BITS as i64);
        b.ins().brif(is_false, pop_take, &[], slow, &[]);

        b.switch_to_block(pop_take);
        self.adjust_len(b, len, -1);
        // The interpreter arm marks a Failure at the new stack top when the
        // branch is taken; the shim is a cheap top-of-stack check.
        self.call_v1(b, mark_fn);
        if backedge {
            self.call_v1(b, safepoint_fn);
        }
        b.ins().jump(target, &[]);

        b.switch_to_block(slow);
        let callee = b.ins().iconst(self.ptr_ty, cond_fn as i64);
        let call = b.ins().call_indirect(self.s1, callee, &[self.interp]);
        let cond = b.inst_results(call)[0];
        if backedge {
            let poll = b.create_block();
            b.ins().brif(cond, poll, &[], fall, &[]);
            b.switch_to_block(poll);
            self.call_v1(b, safepoint_fn);
            b.ins().jump(target, &[]);
        } else {
            b.ins().brif(cond, target, &[], fall, &[]);
        }

        b.switch_to_block(fall);
    }

    /// `JumpIfTrue` (peek semantics — the tested word stays on the stack on
    /// both paths) with a Bool-word fast path. Leaves the builder in the
    /// fall-through block.
    pub(super) fn emit_jump_if_true(
        &self,
        b: &mut FunctionBuilder,
        target: cranelift_codegen::ir::Block,
        backedge: bool,
        safepoint_fn: usize,
        cond_fn: usize,
    ) {
        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let top_addr = self.slot_addr(b, ptr, len, 1);
        let wt = b.ins().load(types::I64, Self::mf(), top_addr, 0);

        let take = b.create_block();
        let chk_false = b.create_block();
        let slow = b.create_block();
        let fall = b.create_block();

        let is_true = b.ins().icmp_imm(IntCC::Equal, wt, w::TRUE_BITS as i64);
        b.ins().brif(is_true, take, &[], chk_false, &[]);

        b.switch_to_block(take);
        if backedge {
            self.call_v1(b, safepoint_fn);
        }
        b.ins().jump(target, &[]);

        b.switch_to_block(chk_false);
        let is_false = b.ins().icmp_imm(IntCC::Equal, wt, w::FALSE_BITS as i64);
        b.ins().brif(is_false, fall, &[], slow, &[]);

        b.switch_to_block(slow);
        let callee = b.ins().iconst(self.ptr_ty, cond_fn as i64);
        let call = b.ins().call_indirect(self.s1, callee, &[self.interp]);
        let cond = b.inst_results(call)[0];
        if backedge {
            let poll = b.create_block();
            b.ins().brif(cond, poll, &[], fall, &[]);
            b.switch_to_block(poll);
            self.call_v1(b, safepoint_fn);
            b.ins().jump(target, &[]);
        } else {
            b.ins().brif(cond, target, &[], fall, &[]);
        }

        b.switch_to_block(fall);
    }
}
