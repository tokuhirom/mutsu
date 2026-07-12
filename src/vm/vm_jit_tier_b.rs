//! Tier B inline emission (ADR-0004 §2.3, J4): NaN-box tag-dispatched fast
//! paths compiled directly into the CLIF body, with the Tier A helper shim as
//! the slow path.
//!
//! Values on the VM stack are 8-byte NaN-box words (`docs/nanbox-3b1-step-b`
//! layout, exported to this module through `crate::value::jit_words`). The
//! emitted fast paths cover exactly the type combinations the interpreter's
//! own `exec_*_op` fast paths cover — small Int (+ small Int) and Num (+ Num)
//! — and route every other word shape (boxed Int, BigInt, Rat, Junction,
//! overloaded operators, ...) to the unchanged Tier A shim, which re-runs the
//! full interpreter arm on the still-untouched stack. Small Int and Num words
//! carry no payload ownership, so the inline paths move raw bits with **zero
//! refcount or GC traffic** (the ADR-0004 J4 gate).
//!
//! Stack access: the data pointer / length words of `Interpreter::stack` are
//! loaded through offsets discovered by `vm_jit_layout` — reloaded at each
//! opcode (never cached across a helper call, which may reallocate the Vec).

use super::vm_jit_layout::JitLayout;
use crate::value::jit_words as w;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{InstBuilder, MemFlags, SigRef, Type, types};
use cranelift_frontend::FunctionBuilder;

type CVal = cranelift_codegen::ir::Value;

#[derive(Clone, Copy)]
pub(super) enum IntArith {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Copy)]
pub(super) enum NumCmp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

pub(super) struct TierB {
    pub(super) interp: CVal,
    pub(super) ptr_ty: Type,
    pub(super) lay: &'static JitLayout,
    /// `(interp) -> i32` — fallible slow-path shims.
    pub(super) s1: SigRef,
    /// `(interp) -> ()` — infallible helpers.
    pub(super) v1: SigRef,
    /// `(interp, code, u32) -> ()` — the `load_const` slow path.
    pub(super) v_code_u32: SigRef,
}

impl TierB {
    #[inline]
    pub(super) fn mf() -> MemFlags {
        MemFlags::trusted()
    }

    pub(super) fn stack_ptr(&self, b: &mut FunctionBuilder) -> CVal {
        b.ins().load(
            self.ptr_ty,
            Self::mf(),
            self.interp,
            self.lay.stack + self.lay.vec_ptr,
        )
    }

    pub(super) fn stack_len(&self, b: &mut FunctionBuilder) -> CVal {
        b.ins().load(
            types::I64,
            Self::mf(),
            self.interp,
            self.lay.stack + self.lay.vec_len,
        )
    }

    fn stack_cap(&self, b: &mut FunctionBuilder) -> CVal {
        b.ins().load(
            types::I64,
            Self::mf(),
            self.interp,
            self.lay.stack + self.lay.vec_cap,
        )
    }

    /// Address of the stack slot `back` entries below the top (`back` = 1 is
    /// the top of the stack): `ptr + (len - back) * 8`.
    pub(super) fn slot_addr(
        &self,
        b: &mut FunctionBuilder,
        ptr: CVal,
        len: CVal,
        back: i64,
    ) -> CVal {
        let idx = b.ins().iadd_imm(len, -back);
        let off = b.ins().ishl_imm(idx, 3);
        b.ins().iadd(ptr, off)
    }

    /// Store `len + delta` back into the stack's length word.
    pub(super) fn adjust_len(&self, b: &mut FunctionBuilder, len: CVal, delta: i64) {
        let new_len = b.ins().iadd_imm(len, delta);
        b.ins().store(
            Self::mf(),
            new_len,
            self.interp,
            self.lay.stack + self.lay.vec_len,
        );
    }

    /// `word >> 48` — the 16-bit page.
    fn page(&self, b: &mut FunctionBuilder, word: CVal) -> CVal {
        b.ins().ushr_imm(word, w::PAGE_SHIFT as i64)
    }

    /// `page == INT_PAGE` (small inline Int; boxed Ints go slow-path).
    fn is_int_page(&self, b: &mut FunctionBuilder, page: CVal) -> CVal {
        b.ins().icmp_imm(IntCC::Equal, page, w::INT_PAGE as i64)
    }

    /// `NUM_PAGE_MIN <= page <= NUM_PAGE_MAX` (encoded f64).
    fn is_num_page(&self, b: &mut FunctionBuilder, page: CVal) -> CVal {
        let shifted = b.ins().iadd_imm(page, -(w::NUM_PAGE_MIN as i64));
        b.ins().icmp_imm(
            IntCC::UnsignedLessThanOrEqual,
            shifted,
            (w::NUM_PAGE_MAX - w::NUM_PAGE_MIN) as i64,
        )
    }

    /// Sign-extend the 48-bit Int payload to i64.
    fn sx48(&self, b: &mut FunctionBuilder, word: CVal) -> CVal {
        let hi = b.ins().ishl_imm(word, 16);
        b.ins().sshr_imm(hi, 16)
    }

    /// Pack an in-range i64 back into a small-Int word.
    fn pack_int(&self, b: &mut FunctionBuilder, v: CVal) -> CVal {
        let payload = b.ins().band_imm(v, w::PAYLOAD48_MASK as i64);
        b.ins()
            .bor_imm(payload, (w::INT_PAGE << w::PAGE_SHIFT) as i64)
    }

    /// Decode an encoded Num word to an f64 SSA value.
    fn decode_num(&self, b: &mut FunctionBuilder, word: CVal) -> CVal {
        let bits = b.ins().iadd_imm(word, -(w::DOUBLE_OFFSET as i64));
        b.ins().bitcast(types::F64, MemFlags::new(), bits)
    }

    /// Encode an f64 SSA value to a Num word (NaN collapsed to the canonical
    /// quiet-NaN word, exactly like `Value::num`).
    fn encode_num(&self, b: &mut FunctionBuilder, f: CVal) -> CVal {
        let bits = b.ins().bitcast(types::I64, MemFlags::new(), f);
        let word = b.ins().iadd_imm(bits, w::DOUBLE_OFFSET as i64);
        let not_nan = b.ins().fcmp(FloatCC::Equal, f, f);
        let nan_word = b.ins().iconst(types::I64, w::NUM_CANONICAL_NAN_WORD as i64);
        b.ins().select(not_nan, word, nan_word)
    }

    /// Load the process-wide user-infix-declaration counter (zero ⟺ no user
    /// `infix:<op>` override can exist anywhere — see `USER_INFIX_DECLS`).
    fn no_user_infix(&self, b: &mut FunctionBuilder) -> CVal {
        let addr = std::ptr::addr_of!(super::vm_jit::USER_INFIX_DECLS) as usize;
        let addr = b.ins().iconst(self.ptr_ty, addr as i64);
        let ctr = b.ins().load(types::I32, Self::mf(), addr, 0);
        b.ins().icmp_imm(IntCC::Equal, ctr, 0)
    }

    /// Call a fallible `(interp) -> status` shim and return-from-function on a
    /// nonzero status. Leaves the builder in a fresh continuation block.
    fn call_status(&self, b: &mut FunctionBuilder, f: usize) {
        let callee = b.ins().iconst(self.ptr_ty, f as i64);
        let call = b.ins().call_indirect(self.s1, callee, &[self.interp]);
        let status = b.inst_results(call)[0];
        let err = b.create_block();
        let cont = b.create_block();
        b.ins().brif(status, err, &[], cont, &[]);
        b.switch_to_block(err);
        b.ins().return_(&[status]);
        b.switch_to_block(cont);
    }

    /// Call an infallible `(interp) -> ()` helper.
    pub(super) fn call_v1(&self, b: &mut FunctionBuilder, f: usize) {
        let callee = b.ins().iconst(self.ptr_ty, f as i64);
        b.ins().call_indirect(self.v1, callee, &[self.interp]);
    }

    /// `Add`/`Sub`/`Mul`: pop two Int (or two Num) words, push the result.
    /// Fast-path conditions mirror the interpreter arm exactly: small Int
    /// operands whose result stays in the small-Int range (else the helper
    /// boxes it), or two Num operands; `Add` additionally requires no user
    /// `infix:<+>` declaration (the only arith arm with an override check).
    pub(super) fn emit_int_num_arith(&self, b: &mut FunctionBuilder, op: IntArith, slow_fn: usize) {
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
        let mut both_int = b.ins().band(a_int, b_int);
        let no_override = if matches!(op, IntArith::Add) {
            let no = self.no_user_infix(b);
            both_int = b.ins().band(both_int, no);
            Some(no)
        } else {
            None
        };

        let int_blk = b.create_block();
        let num_chk = b.create_block();
        let num_blk = b.create_block();
        let slow_blk = b.create_block();
        let done = b.create_block();
        b.ins().brif(both_int, int_blk, &[], num_chk, &[]);

        // -- Int fast path --
        b.switch_to_block(int_blk);
        let av = self.sx48(b, wa);
        let bv = self.sx48(b, wb);
        let (res, mul_of) = match op {
            // 48-bit operands cannot overflow an i64 add/sub; only the
            // result's small-Int range needs checking.
            IntArith::Add => (b.ins().iadd(av, bv), None),
            IntArith::Sub => (b.ins().isub(av, bv), None),
            IntArith::Mul => {
                let (r, of) = b.ins().smul_overflow(av, bv);
                (r, Some(of))
            }
        };
        let hi = b.ins().ishl_imm(res, 16);
        let back = b.ins().sshr_imm(hi, 16);
        let fits = b.ins().icmp(IntCC::Equal, back, res);
        let ok = match mul_of {
            Some(of) => {
                let no_of = b.ins().icmp_imm(IntCC::Equal, of, 0);
                b.ins().band(fits, no_of)
            }
            None => fits,
        };
        let int_store = b.create_block();
        b.ins().brif(ok, int_store, &[], slow_blk, &[]);
        b.switch_to_block(int_store);
        let word = self.pack_int(b, res);
        b.ins().store(Self::mf(), word, a_addr, 0);
        self.adjust_len(b, len, -1);
        b.ins().jump(done, &[]);

        // -- Num fast path --
        b.switch_to_block(num_chk);
        let a_num = self.is_num_page(b, pa);
        let b_num = self.is_num_page(b, pb);
        let mut both_num = b.ins().band(a_num, b_num);
        if let Some(no) = no_override {
            both_num = b.ins().band(both_num, no);
        }
        b.ins().brif(both_num, num_blk, &[], slow_blk, &[]);
        b.switch_to_block(num_blk);
        let fa = self.decode_num(b, wa);
        let fb = self.decode_num(b, wb);
        let fr = match op {
            IntArith::Add => b.ins().fadd(fa, fb),
            IntArith::Sub => b.ins().fsub(fa, fb),
            IntArith::Mul => b.ins().fmul(fa, fb),
        };
        let word = self.encode_num(b, fr);
        b.ins().store(Self::mf(), word, a_addr, 0);
        self.adjust_len(b, len, -1);
        b.ins().jump(done, &[]);

        // -- Slow path: the Tier A shim reruns the full interpreter arm --
        b.switch_to_block(slow_blk);
        self.call_status(b, slow_fn);
        b.ins().jump(done, &[]);

        b.switch_to_block(done);
    }

    /// `NumLt`/`NumLe`/`NumGt`/`NumGe`/`NumEq`/`NumNe`: pop two Int (or two
    /// Num) words, push True/False. Small-Int words are canonical, so Eq/Ne
    /// compare raw words; the ordered comparisons compare the sign-extended
    /// payloads (via a 16-bit left shift, which preserves order). Float
    /// comparisons use the ordered/unordered CC that matches the interpreter
    /// fast path's Rust `f64` operator semantics (NaN → False, except `!=`).
    pub(super) fn emit_compare(&self, b: &mut FunctionBuilder, op: NumCmp, slow_fn: usize) {
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
        let both_int = b.ins().band(a_int, b_int);

        let int_blk = b.create_block();
        let num_chk = b.create_block();
        let num_blk = b.create_block();
        let slow_blk = b.create_block();
        let done = b.create_block();
        b.ins().brif(both_int, int_blk, &[], num_chk, &[]);

        b.switch_to_block(int_blk);
        let cond = match op {
            NumCmp::Eq => b.ins().icmp(IntCC::Equal, wa, wb),
            NumCmp::Ne => b.ins().icmp(IntCC::NotEqual, wa, wb),
            _ => {
                let sa = b.ins().ishl_imm(wa, 16);
                let sb = b.ins().ishl_imm(wb, 16);
                let cc = match op {
                    NumCmp::Lt => IntCC::SignedLessThan,
                    NumCmp::Le => IntCC::SignedLessThanOrEqual,
                    NumCmp::Gt => IntCC::SignedGreaterThan,
                    NumCmp::Ge => IntCC::SignedGreaterThanOrEqual,
                    NumCmp::Eq | NumCmp::Ne => unreachable!(),
                };
                b.ins().icmp(cc, sa, sb)
            }
        };
        self.store_truth(b, cond, a_addr, len);
        b.ins().jump(done, &[]);

        b.switch_to_block(num_chk);
        let a_num = self.is_num_page(b, pa);
        let b_num = self.is_num_page(b, pb);
        let both_num = b.ins().band(a_num, b_num);
        b.ins().brif(both_num, num_blk, &[], slow_blk, &[]);
        b.switch_to_block(num_blk);
        let fa = self.decode_num(b, wa);
        let fb = self.decode_num(b, wb);
        let cc = match op {
            NumCmp::Lt => FloatCC::LessThan,
            NumCmp::Le => FloatCC::LessThanOrEqual,
            NumCmp::Gt => FloatCC::GreaterThan,
            NumCmp::Ge => FloatCC::GreaterThanOrEqual,
            // Interpreter: NaN operands force False for ==, True for !=;
            // FloatCC::Equal is ordered and FloatCC::NotEqual is unordered,
            // matching exactly.
            NumCmp::Eq => FloatCC::Equal,
            NumCmp::Ne => FloatCC::NotEqual,
        };
        let cond = b.ins().fcmp(cc, fa, fb);
        self.store_truth(b, cond, a_addr, len);
        b.ins().jump(done, &[]);

        b.switch_to_block(slow_blk);
        self.call_status(b, slow_fn);
        b.ins().jump(done, &[]);

        b.switch_to_block(done);
    }

    /// Store True/False (selected by `cond`) into the slot at `a_addr` and
    /// shrink the stack by one (the two operands collapse into the result).
    fn store_truth(&self, b: &mut FunctionBuilder, cond: CVal, a_addr: CVal, len: CVal) {
        let t = b.ins().iconst(types::I64, w::TRUE_BITS as i64);
        let f = b.ins().iconst(types::I64, w::FALSE_BITS as i64);
        let word = b.ins().select(cond, t, f);
        b.ins().store(Self::mf(), word, a_addr, 0);
        self.adjust_len(b, len, -1);
    }

    /// `LoadConst` of a refcount-free constant (small Int / Num / Bool / Nil
    /// / ...): push the 8-byte word as a raw immediate. Only the full-stack
    /// (grow) case calls the Tier A shim.
    pub(super) fn emit_load_const(
        &self,
        b: &mut FunctionBuilder,
        word: u64,
        codep: CVal,
        idx: u32,
        slow_fn: usize,
    ) {
        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let cap = self.stack_cap(b);
        let full = b.ins().icmp(IntCC::Equal, len, cap);
        let fast = b.create_block();
        let slow = b.create_block();
        let done = b.create_block();
        b.ins().brif(full, slow, &[], fast, &[]);

        b.switch_to_block(fast);
        let off = b.ins().ishl_imm(len, 3);
        let addr = b.ins().iadd(ptr, off);
        let wv = b.ins().iconst(types::I64, word as i64);
        b.ins().store(Self::mf(), wv, addr, 0);
        self.adjust_len(b, len, 1);
        b.ins().jump(done, &[]);

        b.switch_to_block(slow);
        let callee = b.ins().iconst(self.ptr_ty, slow_fn as i64);
        let idxv = b.ins().iconst(types::I32, idx as i64);
        b.ins()
            .call_indirect(self.v_code_u32, callee, &[self.interp, codep, idxv]);
        b.ins().jump(done, &[]);

        b.switch_to_block(done);
    }

    /// `SetSourceLine`: a single immediate store to the interpreter field.
    pub(super) fn emit_set_source_line(&self, b: &mut FunctionBuilder, line: i64) {
        let v = b.ins().iconst(types::I64, line);
        b.ins()
            .store(Self::mf(), v, self.interp, self.lay.cur_source_line);
    }

    /// `ContainerizePair`: a no-op unless the top word is a `Pair` (the only
    /// shape the interpreter arm rewrites), which goes to the Tier A shim.
    pub(super) fn emit_containerize_pair(&self, b: &mut FunctionBuilder, slow_fn: usize) {
        let ptr = self.stack_ptr(b);
        let len = self.stack_len(b);
        let top_addr = self.slot_addr(b, ptr, len, 1);
        let wt = b.ins().load(types::I64, Self::mf(), top_addr, 0);
        let masked = b.ins().band_imm(wt, w::KIND_MASK as i64);
        let is_pair = b
            .ins()
            .icmp_imm(IntCC::Equal, masked, w::PAIR_PATTERN as i64);
        let slow = b.create_block();
        let done = b.create_block();
        b.ins().brif(is_pair, slow, &[], done, &[]);
        b.switch_to_block(slow);
        self.call_v1(b, slow_fn);
        b.ins().jump(done, &[]);
        b.switch_to_block(done);
    }
}
