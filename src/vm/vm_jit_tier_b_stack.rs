//! Tier B inline emission for the stack-shape opcodes `Dup` and `Pop` —
//! split out of `vm_jit_tier_b.rs` purely by file size. See that module's
//! header for the fast-path correctness contract.
//!
//! Every `&&` / `||` / `?? !!` / `//` chain compiles to `Dup`, a conditional
//! jump and `Pop`, so a JIT-compiled routine body made of such a chain
//! (`(0x41 <= $o <= 0x5A) || (0x61 <= $o <= 0x7A) || ...`) used to leave the
//! native code for a full interpreter dispatch (`vm_jit_helpers::step`) on
//! every one of them: ~170 Ir apiece for what is a word copy or a length
//! decrement.
//!
//! Both fast paths move raw words only when the word owns nothing — a small
//! Int, a Num, a `Bool` or a `Package` (the same probe `emit_get_local` uses
//! to duplicate a slot word) — so they need no refcount or GC traffic. Every
//! other word, an empty stack and (for `Dup`) a full one go to the generic
//! step, which re-runs the interpreter arm on the untouched stack.

use super::vm_jit_tier_b::TierB;
use crate::value::jit_words as w;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{Block, InstBuilder, SigRef, types};
use cranelift_frontend::FunctionBuilder;

type CVal = cranelift_codegen::ir::Value;

/// What the step slow path of an inline stack op needs.
pub(super) struct StepFallback {
    /// `(interp, code, u32, fns) -> i32` — `vm_jit_helpers::step`'s signature.
    pub(super) sig: SigRef,
    /// `vm_jit_helpers::step`.
    pub(super) step_fn: usize,
    /// The `CompiledFns` pointer of the JIT entry.
    pub(super) fnsp: CVal,
    /// Index of the opcode being emitted.
    pub(super) opidx: u32,
}

impl TierB {
    /// `Dup`: push a copy of the top word. Interpreter arm:
    /// `stack.push(stack.last().unwrap().clone())`, which for a refcount-free
    /// word is a raw 8-byte copy.
    // Cost: O(1).
    pub(super) fn emit_dup(&self, b: &mut FunctionBuilder, fb: &StepFallback) {
        let slow = b.create_block();
        let done = b.create_block();
        let chk_cap = b.create_block();
        let push = b.create_block();

        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let empty = b.ins().icmp_imm_s(IntCC::Equal, len, 0);
        let load = b.create_block();
        b.ins().brif(empty, slow, &[], load, &[]);

        b.switch_to_block(load);
        let top_addr = self.slot_addr(b, ptr, len, 1);
        let word = b.ins().load(types::I64, Self::mf(), top_addr, 0);
        self.branch_on_refcount_free(b, word, chk_cap, slow);

        b.switch_to_block(chk_cap);
        let cap = self.stack_cap(b);
        let full = b.ins().icmp(IntCC::Equal, len, cap);
        b.ins().brif(full, slow, &[], push, &[]);

        b.switch_to_block(push);
        let off = b.ins().ishl_imm_u(len, 3);
        let addr = b.ins().iadd(ptr, off);
        b.ins().store(Self::mf(), word, addr, 0);
        self.adjust_len(b, len, 1);
        b.ins().jump(done, &[]);

        self.emit_step_fallback(b, slow, done, fb);
        b.switch_to_block(done);
    }

    /// `Pop`: discard the top word. The interpreter arm only does more than
    /// drop it for a `LazyList` (sunk here), which is not refcount-free, so a
    /// refcount-free word is a length decrement.
    // Cost: O(1).
    pub(super) fn emit_pop(&self, b: &mut FunctionBuilder, fb: &StepFallback) {
        let slow = b.create_block();
        let done = b.create_block();
        let drop_blk = b.create_block();

        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let empty = b.ins().icmp_imm_s(IntCC::Equal, len, 0);
        let load = b.create_block();
        b.ins().brif(empty, slow, &[], load, &[]);

        b.switch_to_block(load);
        let top_addr = self.slot_addr(b, ptr, len, 1);
        let word = b.ins().load(types::I64, Self::mf(), top_addr, 0);
        self.branch_on_refcount_free(b, word, drop_blk, slow);

        b.switch_to_block(drop_blk);
        self.adjust_len(b, len, -1);
        b.ins().jump(done, &[]);

        self.emit_step_fallback(b, slow, done, fb);
        b.switch_to_block(done);
    }

    /// Branch to `yes` when `word` is a small Int, a Num, a `Bool` or a
    /// `Package`, to `no` otherwise. Two ordered range tests, as in
    /// `emit_get_local`: pages `INT_PAGE ..= NUM_PAGE_MAX` are exactly the
    /// Int and Num words, and `Bool`/`Package` are adjacent kind ids.
    fn branch_on_refcount_free(&self, b: &mut FunctionBuilder, word: CVal, yes: Block, no: Block) {
        let page = self.page(b, word);
        let page_rel = b.ins().iadd_imm_s(page, -(w::INT_PAGE as i64));
        let int_or_num = b.ins().icmp_imm_s(
            IntCC::UnsignedLessThanOrEqual,
            page_rel,
            (w::NUM_PAGE_MAX - w::INT_PAGE) as i64,
        );
        let kind_chk = b.create_block();
        b.ins().brif(int_or_num, yes, &[], kind_chk, &[]);

        b.switch_to_block(kind_chk);
        let masked = b.ins().band_imm_u(word, w::KIND_MASK as i64);
        let kind_rel = b
            .ins()
            .iadd_imm_s(masked, (w::BOOL_PATTERN as i64).wrapping_neg());
        let inline_kind = b.ins().icmp_imm_s(
            IntCC::UnsignedLessThanOrEqual,
            kind_rel,
            w::BOOL_PACKAGE_SPAN as i64,
        );
        b.ins().brif(inline_kind, yes, &[], no, &[]);
    }

    /// Fill `slow` with the generic step of the op, returning its non-zero
    /// status from the JIT function and joining `done` otherwise.
    fn emit_step_fallback(
        &self,
        b: &mut FunctionBuilder,
        slow: Block,
        done: Block,
        fb: &StepFallback,
    ) {
        b.switch_to_block(slow);
        let callee = b.ins().iconst(self.ptr_ty, fb.step_fn as i64);
        let idx = b.ins().iconst(types::I32, fb.opidx as i64);
        let call = b
            .ins()
            .call_indirect(fb.sig, callee, &[self.interp, self.codep, idx, fb.fnsp]);
        let status = b.inst_results(call)[0];
        let err = b.create_block();
        b.ins().brif(status, err, &[], done, &[]);
        b.switch_to_block(err);
        b.ins().return_(&[status]);
    }
}
