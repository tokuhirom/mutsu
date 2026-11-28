//! Tier B inline emission for `+&`, `+|` and `+^` (`OpCode::BitAnd`,
//! `BitOr`, `BitXor`) on two small Ints — split out of `vm_jit_tier_b.rs` by
//! file size. See that module's header for the fast-path correctness contract.

use super::vm_jit_tier_b::TierB;
use cranelift_codegen::ir::{InstBuilder, types};
use cranelift_frontend::FunctionBuilder;

/// The Int bitwise infix ops Tier B emits inline.
#[derive(Clone, Copy)]
pub(super) enum IntBitOp {
    And,
    Or,
    Xor,
}

impl TierB {
    /// `a +& b` / `a +| b` / `a +^ b` with both operands small inline Ints and
    /// no user infix operator declared anywhere: one native instruction.
    ///
    /// This is exactly the case `exec_bit_*_op`'s own fast path
    /// (`try_small_int_bitop`) takes, which computes `int_bitop` on the two
    /// i64 values; on two small Ints that is the plain i64 `&`/`|`/`^`. Every
    /// other word shape (a boxed Int, `BigInt`, `Num`, `Str`, `Bool`, a
    /// `ContainerRef`), and any program that declared a user infix
    /// (`no_user_infix`, the guard `Add` carries), goes to the unchanged
    /// Tier A shim on the untouched stack.
    ///
    /// No range check: each operand is a sign-extended 48-bit value, so its
    /// bits 47..63 are one repeated bit; a bitwise op keeps that property, so
    /// the result is again a sign-extended 48-bit value and packs losslessly.
    // Cost: O(1) per execution (two loads, a tag test, one ALU op, a store).
    pub(super) fn emit_int_bitop(&self, b: &mut FunctionBuilder, op: IntBitOp, slow_fn: usize) {
        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let a_addr = self.slot_addr(b, ptr, len, 2);
        let b_addr = self.slot_addr(b, ptr, len, 1);
        let wa = b.ins().load(types::I64, Self::mf(), a_addr, 0);
        let wb = b.ins().load(types::I64, Self::mf(), b_addr, 0);
        let pa = self.page(b, wa);
        let pb = self.page(b, wb);
        let a_int = self.is_int_page(b, pa);
        let b_int = self.is_int_page(b, pb);
        let no_override = self.no_user_infix(b);
        let both_int = b.ins().band(a_int, b_int);
        let fast = b.ins().band(both_int, no_override);

        let int_blk = b.create_block();
        let slow_blk = b.create_block();
        let done = b.create_block();
        b.ins().brif(fast, int_blk, &[], slow_blk, &[]);

        b.switch_to_block(int_blk);
        let av = self.sx48(b, wa);
        let bv = self.sx48(b, wb);
        let res = match op {
            IntBitOp::And => b.ins().band(av, bv),
            IntBitOp::Or => b.ins().bor(av, bv),
            IntBitOp::Xor => b.ins().bxor(av, bv),
        };
        let word = self.pack_int(b, res);
        b.ins().store(Self::mf(), word, a_addr, 0);
        self.adjust_len(b, len, -1);
        b.ins().jump(done, &[]);

        // -- Slow path: the Tier A shim reruns the full interpreter arm --
        b.switch_to_block(slow_blk);
        self.call_status(b, slow_fn);
        b.ins().jump(done, &[]);

        b.switch_to_block(done);
    }
}
