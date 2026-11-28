//! Loop kernels: lowering of array element accesses, `++`/`--` and the bit
//! operators (ADR-19288 D5).
//!
//! An element read or store is a call of campaign B's helper
//! (`vm_kernel_elem`) with the interpreter and code pointers the entry put in
//! the loop context. A helper that declines bails the iteration, which the
//! driver then reruns generically. A store either writes or declines with no
//! mutation, and the region never reads an array it stores into
//! (`carried_slots`), so an iteration replayed after a later bail recomputes
//! and rewrites the same elements.

use super::vm_kernel_elem::{
    kernel_elem_read_int, kernel_elem_read_num, kernel_elem_store_int, kernel_elem_store_num,
};
use super::vm_kernel_hash::{
    kernel_elem2_read_int, kernel_elem2_read_num, kernel_hash_read_int, kernel_hash_read_num,
    kernel_hash_store_int, kernel_hash_store_num, kernel_hash_store_prefixed_int,
    kernel_hash_store_prefixed_num,
};
use super::vm_loop_kernel_emit::Em;
use super::vm_loop_kernel_types::K;
use crate::opcode::OpCode;
use crate::opcode::kernel_slot::SlotKind;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    InstBuilder, MemFlagsData, SigRef, StackSlotData, StackSlotKind, Value as ClValue, types,
};

/// The helper signatures a kernel imports, and where the loop context holds
/// the pointers they take.
pub(super) struct ElemSigs {
    /// `(interp, code, slot, index, out) -> status`, for both read helpers.
    pub(super) read: SigRef,
    /// `(interp, code, slot, index, value: i64) -> status`.
    pub(super) store_int: SigRef,
    /// `(interp, code, slot, index, value: f64) -> status`.
    pub(super) store_num: SigRef,
    /// `(interp, code, slot, i, j, out) -> status`, for both grid reads.
    pub(super) read2: SigRef,
    /// `(interp, code, slot, prefix, key, value: i64) -> status`.
    pub(super) store_prefixed_int: SigRef,
    /// `(interp, code, slot, prefix, key, value: f64) -> status`.
    pub(super) store_prefixed_num: SigRef,
    /// Offsets of the interpreter and code pointers in the loop context.
    pub(super) interp_offset: i32,
    pub(super) code_offset: i32,
}

impl Em<'_, '_> {
    /// The interpreter and code pointers the entry stored in the context.
    fn elem_ptrs(&mut self) -> (ClValue, ClValue) {
        let mf = MemFlagsData::trusted();
        let interp = self
            .b
            .ins()
            .load(self.ptr, mf, self.ctxp, self.elem.interp_offset);
        let code = self
            .b
            .ins()
            .load(self.ptr, mf, self.ctxp, self.elem.code_offset);
        (interp, code)
    }

    /// Bail unless the helper's `status` is OK.
    fn guard_status(&mut self, status: ClValue) {
        let ok = self.b.ins().icmp_imm_s(
            IntCC::Equal,
            status,
            super::vm_kernel_elem::KERNEL_ELEM_OK as i64,
        );
        self.guard(ok);
    }

    /// A read helper `f` called with `keys` after the pointers and slot, the
    /// element written to an 8-byte stack slot of kind `kind`; a decline bails.
    fn read_via(
        &mut self,
        sig: SigRef,
        f: usize,
        slot: u32,
        keys: &[ClValue],
        kind: SlotKind,
    ) -> ClValue {
        let out =
            self.b
                .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 3));
        let out_addr = self.b.ins().stack_addr(self.ptr, out, 0);
        let (interp, code) = self.elem_ptrs();
        let slot_v = self.b.ins().iconst(types::I32, slot as i64);
        let f = self.b.ins().iconst(self.ptr, f as i64);
        let mut args = vec![interp, code, slot_v];
        args.extend_from_slice(keys);
        args.push(out_addr);
        let call = self.b.ins().call_indirect(sig, f, &args);
        let status = self.b.inst_results(call)[0];
        self.guard_status(status);
        let ty = match kind {
            SlotKind::Int => types::I64,
            SlotKind::Num => types::F64,
        };
        self.b.ins().stack_load(self.ptr, ty, out, 0)
    }

    /// `@a[index]` for the read array in `slot`, whose elements are `kind`.
    pub(super) fn elem_read(&mut self, slot: u32, index: ClValue, kind: SlotKind) -> ClValue {
        let f = match kind {
            SlotKind::Int => kernel_elem_read_int as *const () as usize,
            SlotKind::Num => kernel_elem_read_num as *const () as usize,
        };
        self.read_via(self.elem.read, f, slot, &[index], kind)
    }

    /// `%h{key}` for the read hash in `slot`, whose values are `kind`.
    pub(super) fn hash_read(&mut self, slot: u32, key: ClValue, kind: SlotKind) -> ClValue {
        let f = match kind {
            SlotKind::Int => kernel_hash_read_int as *const () as usize,
            SlotKind::Num => kernel_hash_read_num as *const () as usize,
        };
        self.read_via(self.elem.read, f, slot, &[key], kind)
    }

    /// `@g[i][j]` for the read grid in `slot`, whose elements are `kind`.
    pub(super) fn elem2_read(
        &mut self,
        slot: u32,
        i: ClValue,
        j: ClValue,
        kind: SlotKind,
    ) -> ClValue {
        let f = match kind {
            SlotKind::Int => kernel_elem2_read_int as *const () as usize,
            SlotKind::Num => kernel_elem2_read_num as *const () as usize,
        };
        self.read_via(self.elem.read2, f, slot, &[i, j], kind)
    }

    /// `@a[index] = value` for the stored array in `slot`.
    pub(super) fn elem_store(&mut self, slot: u32, index: ClValue, value: ClValue, k: K) {
        let f = if k == K::Int {
            kernel_elem_store_int as *const () as usize
        } else {
            kernel_elem_store_num as *const () as usize
        };
        self.store_via(f, slot, index, value, k);
    }

    /// `%h{key} = value` for the stored hash in `slot`.
    pub(super) fn hash_store(&mut self, slot: u32, key: ClValue, value: ClValue, k: K) {
        let f = if k == K::Int {
            kernel_hash_store_int as *const () as usize
        } else {
            kernel_hash_store_num as *const () as usize
        };
        self.store_via(f, slot, key, value, k);
    }

    /// `%h{"prefix$key"} = value` for the stored hash in `slot`, the prefix
    /// being the `Str` constant at index `prefix`.
    pub(super) fn hash_store_prefixed(
        &mut self,
        slot: u32,
        prefix: u32,
        key: ClValue,
        value: ClValue,
        k: K,
    ) {
        let (f, sig) = if k == K::Int {
            (
                kernel_hash_store_prefixed_int as *const () as usize,
                self.elem.store_prefixed_int,
            )
        } else {
            (
                kernel_hash_store_prefixed_num as *const () as usize,
                self.elem.store_prefixed_num,
            )
        };
        let (interp, code) = self.elem_ptrs();
        let slot_v = self.b.ins().iconst(types::I32, slot as i64);
        let prefix_v = self.b.ins().iconst(types::I32, prefix as i64);
        let f = self.b.ins().iconst(self.ptr, f as i64);
        let call = self
            .b
            .ins()
            .call_indirect(sig, f, &[interp, code, slot_v, prefix_v, key, value]);
        let status = self.b.inst_results(call)[0];
        self.guard_status(status);
    }

    /// A store helper `f` on `slot`; a decline bails.
    fn store_via(&mut self, f: usize, slot: u32, index: ClValue, value: ClValue, k: K) {
        let (interp, code) = self.elem_ptrs();
        let slot_v = self.b.ins().iconst(types::I32, slot as i64);
        let sig = if k == K::Int {
            self.elem.store_int
        } else {
            self.elem.store_num
        };
        let f = self.b.ins().iconst(self.ptr, f as i64);
        let call = self
            .b
            .ins()
            .call_indirect(sig, f, &[interp, code, slot_v, index, value]);
        let status = self.b.inst_results(call)[0];
        self.guard_status(status);
    }

    /// `++$x` / `--$x` / `$x++` / `$x--` on the Int local in `slot`: the new
    /// value is stored, and the expression's value returned. Stepping past
    /// the `i64` range bails.
    pub(super) fn incdec(&mut self, op: &OpCode, slot: u32) -> Option<ClValue> {
        let (var, _) = *self.vars.get(&slot)?;
        let old = self.b.use_var(var);
        let one = self.b.ins().iconst(types::I64, 1);
        let up = matches!(op, OpCode::PreIncrement(..) | OpCode::PostIncrement(..));
        let (new, of) = if up {
            self.b.ins().sadd_overflow(old, one)
        } else {
            self.b.ins().ssub_overflow(old, one)
        };
        let ok = self.b.ins().icmp_imm_s(IntCC::Equal, of, 0);
        self.guard(ok);
        self.b.def_var(var, new);
        let pre = matches!(op, OpCode::PreIncrement(..) | OpCode::PreDecrement(..));
        Some(if pre { new } else { old })
    }

    /// `+&`, `+|` and `+^` on two Ints: two's complement, as Raku's
    /// arbitrary-precision forms are for values in the `i64` range. `+>` is
    /// an arithmetic shift (Raku's floor division by a power of two) for a
    /// count in `0..64`; any other count bails.
    pub(super) fn bitop(&mut self, op: &OpCode, x: ClValue, y: ClValue) -> ClValue {
        match op {
            OpCode::BitAnd => self.b.ins().band(x, y),
            OpCode::BitOr => self.b.ins().bor(x, y),
            OpCode::BitShiftRight => {
                let ok = self.b.ins().icmp_imm_u(IntCC::UnsignedLessThan, y, 64);
                self.guard(ok);
                self.b.ins().sshr(x, y)
            }
            _ => self.b.ins().bxor(x, y),
        }
    }
}
