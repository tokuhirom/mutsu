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
use cranelift_codegen::ir::{InstBuilder, MemFlagsData, SigRef, Type, types};
use cranelift_frontend::FunctionBuilder;

type CVal = cranelift_codegen::ir::Value;

#[derive(Clone, Copy)]
pub(super) enum IntArith {
    Add,
    Sub,
    Mul,
}

/// `div` / `mod` — Raku's *floor* integer division and modulo (`-7 div 3 == -3`,
/// `-7 mod 3 == 2`), as opposed to `/` and `%`.
#[derive(Clone, Copy)]
pub(super) enum IntDivMod {
    Div,
    Mod,
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
    /// `(interp, code, u32) -> i32` — the `get_local` slow path.
    pub(super) s_code_u32: SigRef,
}

impl TierB {
    #[inline]
    pub(super) fn mf() -> MemFlagsData {
        MemFlagsData::trusted()
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
        b.ins().bitcast(types::F64, MemFlagsData::new(), bits)
    }

    /// Encode an f64 SSA value to a Num word (NaN collapsed to the canonical
    /// quiet-NaN word, exactly like `Value::num`).
    fn encode_num(&self, b: &mut FunctionBuilder, f: CVal) -> CVal {
        let bits = b.ins().bitcast(types::I64, MemFlagsData::new(), f);
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

    /// `IntDiv`/`IntMod` (`div` / `mod`): pop two Int words, push the *floor*
    /// quotient / remainder. The interpreter arm uses `Integer::div_floor` /
    /// `mod_floor`, so the native path must match: Cranelift's `sdiv`/`srem`
    /// truncate toward zero, so a floor correction is applied when the remainder
    /// is non-zero and the operand signs differ.
    ///
    /// Fast-path conditions mirror the interpreter's Int/Int arm exactly: two
    /// small-Int operands with a non-zero divisor. Everything else — a zero
    /// divisor (which must raise `X::Numeric::DivideByZero`), `BigInt`, `Num`
    /// (which the arm coerces via `to_int`), junctions — falls to the shim.
    pub(super) fn emit_int_divmod(&self, b: &mut FunctionBuilder, op: IntDivMod, slow_fn: usize) {
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
        let slow_blk = b.create_block();
        let done = b.create_block();
        b.ins().brif(both_int, int_blk, &[], slow_blk, &[]);

        // -- Int fast path (divisor must be non-zero; `sdiv` would trap) --
        b.switch_to_block(int_blk);
        let av = self.sx48(b, wa);
        let bv = self.sx48(b, wb);
        let nonzero = b.ins().icmp_imm(IntCC::NotEqual, bv, 0);
        let calc_blk = b.create_block();
        b.ins().brif(nonzero, calc_blk, &[], slow_blk, &[]);

        b.switch_to_block(calc_blk);
        // Operands are sign-extended 48-bit, so `a == i64::MIN` is impossible and
        // `sdiv` cannot trap on the INT_MIN / -1 overflow.
        let rem = b.ins().srem(av, bv);
        // Floor correction applies exactly when the truncated remainder is
        // non-zero and the operand signs differ.
        let rem_nz = b.ins().icmp_imm(IntCC::NotEqual, rem, 0);
        let sign_xor = b.ins().bxor(av, bv);
        let signs_differ = b.ins().icmp_imm(IntCC::SignedLessThan, sign_xor, 0);
        let correct = b.ins().band(rem_nz, signs_differ);
        let zero = b.ins().iconst(types::I64, 0);
        let res = match op {
            IntDivMod::Div => {
                let q = b.ins().sdiv(av, bv);
                let one = b.ins().iconst(types::I64, 1);
                let adj = b.ins().select(correct, one, zero);
                b.ins().isub(q, adj)
            }
            IntDivMod::Mod => {
                let adj = b.ins().select(correct, bv, zero);
                b.ins().iadd(rem, adj)
            }
        };
        // `mod`'s result is bounded by the divisor and `div`'s by the dividend, so
        // both fit the small-Int range for every input except `MIN_48 div -1`.
        // Check anyway, and let the shim box the one case that does not.
        let hi = b.ins().ishl_imm(res, 16);
        let back = b.ins().sshr_imm(hi, 16);
        let fits = b.ins().icmp(IntCC::Equal, back, res);
        let int_store = b.create_block();
        b.ins().brif(fits, int_store, &[], slow_blk, &[]);
        b.switch_to_block(int_store);
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

    /// `GetLocal` of a statically-eligible plain local (ADR-0004 J4d): when
    /// no dynamic spoiler exists — no `ContainerRef` cell anywhere in the
    /// process, no `$CALLER::x := ...` alias, no atomic variable on this
    /// interpreter, no sigilless attribute alias — and the slot word is a
    /// refcount-free scalar (small Int / Num / Bool / Package), the
    /// interpreter arm reduces to `stack.push(locals[idx].clone())`, which is
    /// emitted here as two loads and a store. Every other combination calls
    /// the Tier A shim, which re-runs the full `exec_get_local_op` guard
    /// chain (cell adoption, lazy thunks, HashEntryRef, Nil declaration
    /// checks, ...) on the untouched stack.
    ///
    /// The static half of the eligibility (name shape, attribute slots) is
    /// checked by the caller; see `get_local_tier_b_eligible`.
    pub(super) fn emit_get_local(
        &self,
        b: &mut FunctionBuilder,
        codep: CVal,
        idx: u32,
        slow_fn: usize,
    ) {
        let slow_blk = b.create_block();
        let done = b.create_block();

        // -- process-global spoiler latches (see vm_jit::CONTAINER_CELLS /
        // CALLER_VAR_BINDS): both zero ⟺ the resolve_binding and env
        // cell-adoption probes are provably no-ops everywhere.
        let cells_addr = std::ptr::addr_of!(super::vm_jit::CONTAINER_CELLS) as usize;
        let cells_addr = b.ins().iconst(self.ptr_ty, cells_addr as i64);
        let cells = b.ins().load(types::I32, Self::mf(), cells_addr, 0);
        let binds_addr = std::ptr::addr_of!(super::vm_jit::CALLER_VAR_BINDS) as usize;
        let binds_addr = b.ins().iconst(self.ptr_ty, binds_addr as i64);
        let binds = b.ins().load(types::I32, Self::mf(), binds_addr, 0);
        let global_spoil = b.ins().bor(cells, binds);
        // -- per-interpreter spoiler flags (plain bool fields, 0 or 1).
        let atomic = b
            .ins()
            .load(types::I8, Self::mf(), self.interp, self.lay.atomic_var_seen);
        let sigil = b.ins().load(
            types::I8,
            Self::mf(),
            self.interp,
            self.lay.sigilless_attrs_active,
        );
        let interp_spoil = b.ins().bor(atomic, sigil);
        let interp_spoil = b.ins().uextend(types::I32, interp_spoil);
        let spoiled = b.ins().bor(global_spoil, interp_spoil);
        let word_chk = b.create_block();
        b.ins().brif(spoiled, slow_blk, &[], word_chk, &[]);

        // -- slot word load (bounds-checked: a non-standard runner may have
        // installed a shorter locals vec; the shim handles that shape).
        b.switch_to_block(word_chk);
        let lptr = b.ins().load(
            self.ptr_ty,
            Self::mf(),
            self.interp,
            self.lay.locals + self.lay.vec_ptr,
        );
        let llen = b.ins().load(
            types::I64,
            Self::mf(),
            self.interp,
            self.lay.locals + self.lay.vec_len,
        );
        let inbounds = b
            .ins()
            .icmp_imm(IntCC::UnsignedGreaterThan, llen, idx as i64);
        let tag_chk = b.create_block();
        b.ins().brif(inbounds, tag_chk, &[], slow_blk, &[]);

        b.switch_to_block(tag_chk);
        let word = b.ins().load(types::I64, Self::mf(), lptr, (idx as i32) * 8);
        // Refcount-free scalar probe: small Int page, encoded-Num page range,
        // Bool kind, or Package kind. Everything else (Nil included — the arm
        // has a whole undeclared-check branch for it) goes to the shim.
        let page = self.page(b, word);
        let is_int = self.is_int_page(b, page);
        let is_num = self.is_num_page(b, page);
        let masked = b.ins().band_imm(word, w::KIND_MASK as i64);
        let is_bool = b
            .ins()
            .icmp_imm(IntCC::Equal, masked, w::BOOL_PATTERN as i64);
        let is_pkg = b
            .ins()
            .icmp_imm(IntCC::Equal, masked, w::PACKAGE_PATTERN as i64);
        let num_or_int = b.ins().bor(is_int, is_num);
        let inline_kind = b.ins().bor(is_bool, is_pkg);
        let ok = b.ins().bor(num_or_int, inline_kind);
        let push_chk = b.create_block();
        b.ins().brif(ok, push_chk, &[], slow_blk, &[]);

        // -- push (only a full stack falls back, mirroring emit_load_const).
        b.switch_to_block(push_chk);
        let sptr = self.stack_ptr(b);
        let slen = self.stack_len(b);
        let scap = self.stack_cap(b);
        let full = b.ins().icmp(IntCC::Equal, slen, scap);
        let push_blk = b.create_block();
        b.ins().brif(full, slow_blk, &[], push_blk, &[]);
        b.switch_to_block(push_blk);
        let off = b.ins().ishl_imm(slen, 3);
        let addr = b.ins().iadd(sptr, off);
        b.ins().store(Self::mf(), word, addr, 0);
        self.adjust_len(b, slen, 1);
        b.ins().jump(done, &[]);

        // -- slow path: the Tier A shim re-runs the full interpreter arm.
        b.switch_to_block(slow_blk);
        let callee = b.ins().iconst(self.ptr_ty, slow_fn as i64);
        let idxv = b.ins().iconst(types::I32, idx as i64);
        let call = b
            .ins()
            .call_indirect(self.s_code_u32, callee, &[self.interp, codep, idxv]);
        let status = b.inst_results(call)[0];
        let err = b.create_block();
        let cont = b.create_block();
        b.ins().brif(status, err, &[], cont, &[]);
        b.switch_to_block(err);
        b.ins().return_(&[status]);
        b.switch_to_block(cont);
        b.ins().jump(done, &[]);

        b.switch_to_block(done);
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
