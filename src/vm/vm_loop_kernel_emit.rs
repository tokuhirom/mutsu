//! Loop kernels: emitting one segment's ops (ADR-19288 D5).
//!
//! `Em::segment` lowers the ops of `[start, end)` against the kinds the typing
//! pass settled (`vm_loop_kernel_typing`): `Int` values are `i64`, `Num` values
//! `f64`, comparison results `i8`. A counted `for` or a `while` nested in the
//! segment becomes a native loop in place. Any condition the kernel cannot
//! express branches to the bail block, which hands back the state of the
//! outermost iteration's start.

use super::vm_loop_kernel_types::{
    K, LoopVar, nested_range, nested_while, num_const, num_method_call,
};
use super::*;
use crate::opcode::kernel_slot::SlotKind;

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{Block, InstBuilder, MemFlagsData, SigRef, Value as ClValue, types};
use cranelift_frontend::{FunctionBuilder, Variable};
use std::collections::BTreeMap;

/// The Cranelift type of a stack entry of kind `k`.
pub(super) fn clty(k: K) -> types::Type {
    match k {
        K::Num => types::F64,
        K::Int => types::I64,
        K::Bool => types::I8,
        // A placeholder word: only an `Index` consumes it, by its slot.
        K::Arr(_) => types::I64,
        // The row index a grid's first subscript applied.
        K::Row(_) => types::I64,
        // A placeholder word, and a `"prefix$i"` key's Int.
        K::StrConst(_) | K::IntKey(_) => types::I64,
    }
}

pub(super) struct Em<'f, 'c> {
    pub(super) b: FunctionBuilder<'f>,
    pub(super) code: &'c CompiledCode,
    pub(super) vars: BTreeMap<u32, (Variable, SlotKind)>,
    pub(super) bail: Block,
    pub(super) var: Option<LoopVar>,
    /// The outermost loop's counter (the topic of an implicit-topic `for`).
    pub(super) counter: Variable,
    pub(super) ctxp: ClValue,
    pub(super) poll_sig: SigRef,
    pub(super) poll_fn: usize,
    pub(super) poll_offset: i32,
    pub(super) ptr: types::Type,
    /// The callee table and sub-kernel context offsets in the loop context.
    pub(super) callees_offset: i32,
    pub(super) kctx_offset: i32,
    /// Sub-kernel signatures by arity (index = arity).
    pub(super) call_sigs: Vec<SigRef>,
    /// Calls emitted so far: the next call's slot in the callee table.
    pub(super) next_call: usize,
    /// The element kind of each array the region reads.
    pub(super) elem_reads: Vec<super::vm_loop_kernel_types::ElemRead>,
    /// Element helper signatures and context offsets.
    pub(super) elem: super::vm_loop_kernel_emit_elem::ElemSigs,
}

impl Em<'_, '_> {
    pub(super) fn guard(&mut self, ok: ClValue) {
        let cont = self.b.create_block();
        self.b.ins().brif(ok, cont, &[], self.bail, &[]);
        self.b.switch_to_block(cont);
    }

    /// `v` of kind `k` as an `f64`.
    fn as_f64(&mut self, v: ClValue, k: K) -> ClValue {
        if k == K::Int {
            self.b.ins().fcvt_from_sint(types::F64, v)
        } else {
            v
        }
    }

    /// The truthiness of `v` of kind `k` as a branch condition.
    pub(super) fn truthy(&mut self, v: ClValue, k: K) -> ClValue {
        match k {
            K::Num => {
                let zero = self.b.ins().f64const(0.0);
                // `NotEqual` is unordered-or-not-equal: NaN is true, as in Raku.
                self.b.ins().fcmp(FloatCC::NotEqual, v, zero)
            }
            _ => v,
        }
    }

    /// The back-edge safepoint countdown.
    pub(super) fn poll(&mut self) {
        let mf = MemFlagsData::trusted();
        let left = self
            .b
            .ins()
            .load(types::I32, mf, self.ctxp, self.poll_offset);
        let left = self.b.ins().iadd_imm_s(left, -1);
        self.b.ins().store(mf, left, self.ctxp, self.poll_offset);
        let poll_blk = self.b.create_block();
        let go = self.b.create_block();
        self.b.ins().brif(left, go, &[], poll_blk, &[]);
        self.b.switch_to_block(poll_blk);
        let f = self.b.ins().iconst(self.ptr, self.poll_fn as i64);
        self.b.ins().call_indirect(self.poll_sig, f, &[self.ctxp]);
        self.b.ins().jump(go, &[]);
        self.b.switch_to_block(go);
    }

    /// A counted loop nested in a segment, over `[lo, hi)` or `[lo, hi]`.
    fn nested_range(
        &mut self,
        lo: ClValue,
        hi: ClValue,
        inclusive: bool,
        var: u32,
        body: (usize, usize),
        joins: &BTreeMap<(usize, usize), Vec<K>>,
    ) -> Option<()> {
        let cv = self.b.declare_var(types::I64);
        self.b.def_var(cv, lo);
        let header = self.b.create_block();
        let body_blk = self.b.create_block();
        let exit = self.b.create_block();
        self.b.ins().jump(header, &[]);
        self.b.switch_to_block(header);
        self.poll();
        let c = self.b.use_var(cv);
        let cc = if inclusive {
            IntCC::SignedLessThanOrEqual
        } else {
            IntCC::SignedLessThan
        };
        let more = self.b.ins().icmp(cc, c, hi);
        self.b.ins().brif(more, body_blk, &[], exit, &[]);
        self.b.switch_to_block(body_blk);
        let (v, _) = *self.vars.get(&var)?;
        self.b.def_var(v, c);
        self.segment(body.0, body.1, joins)?;
        let c = self.b.use_var(cv);
        let one = self.b.ins().iconst(types::I64, 1);
        let (next, of) = self.b.ins().sadd_overflow(c, one);
        let ok = self.b.ins().icmp_imm_s(IntCC::Equal, of, 0);
        self.guard(ok);
        self.b.def_var(cv, next);
        self.b.ins().jump(header, &[]);
        self.b.switch_to_block(exit);
        Some(())
    }

    /// A `while` nested in a segment.
    fn nested_while(
        &mut self,
        cond: (usize, usize),
        body_end: usize,
        joins: &BTreeMap<(usize, usize), Vec<K>>,
    ) -> Option<()> {
        let header = self.b.create_block();
        let body_blk = self.b.create_block();
        let exit = self.b.create_block();
        self.b.ins().jump(header, &[]);
        self.b.switch_to_block(header);
        self.poll();
        let mut left = self.segment(cond.0, cond.1, joins)?;
        let (v, k) = left.pop()?;
        let t = self.truthy(v, k);
        self.b.ins().brif(t, body_blk, &[], exit, &[]);
        self.b.switch_to_block(body_blk);
        self.segment(cond.1, body_end, joins)?;
        self.b.ins().jump(header, &[]);
        self.b.switch_to_block(exit);
        Some(())
    }

    /// Emit the ops of `[start, end)`. Returns the stack left at its end.
    pub(super) fn segment(
        &mut self,
        start: usize,
        end: usize,
        joins: &BTreeMap<(usize, usize), Vec<K>>,
    ) -> Option<Vec<(ClValue, K)>> {
        // Join blocks, with one block parameter per stack entry.
        let mut blocks: BTreeMap<usize, (Block, Vec<K>)> = BTreeMap::new();
        // An empty segment (`while $i++ < $m { }`) has no join inside it.
        let inner = (start < end)
            .then(|| joins.range((start, start + 1)..(start, end)))
            .into_iter()
            .flatten();
        for (&(_, t), kinds) in inner {
            let blk = self.b.create_block();
            for k in kinds {
                self.b.append_block_param(blk, clty(*k));
            }
            blocks.insert(t, (blk, kinds.clone()));
        }
        // The segment's own end, reached by fall-through or by a jump.
        let end_kinds: Vec<K> = joins.get(&(start, end)).cloned()?;
        let end_blk = self.b.create_block();
        for k in &end_kinds {
            self.b.append_block_param(end_blk, clty(*k));
        }
        let target = |t: usize, blocks: &BTreeMap<usize, (Block, Vec<K>)>| {
            if t == end {
                Some(end_blk)
            } else {
                blocks.get(&t).map(|b| b.0)
            }
        };
        let args = |stack: &[(ClValue, K)]| -> Vec<cranelift_codegen::ir::BlockArg> {
            stack.iter().map(|(v, _)| (*v).into()).collect()
        };
        let mut stack: Vec<(ClValue, K)> = Vec::new();
        let mut open = true;
        let mut i = start;
        while i < end {
            if let Some((blk, kinds)) = blocks.get(&i).cloned() {
                if open {
                    let a = args(&stack);
                    self.b.ins().jump(blk, &a);
                }
                self.b.switch_to_block(blk);
                let params = self.b.block_params(blk).to_vec();
                stack = params.into_iter().zip(kinds).collect();
                open = true;
            }
            if !open {
                i += 1;
                continue;
            }
            let op = &self.code.ops[i];
            if num_method_call(self.code, op).is_some() {
                let (v, k) = stack.pop()?;
                let v = self.as_f64(v, k);
                stack.push((v, K::Num));
                i += 1;
                continue;
            }
            if let Some((inclusive, var, body_start, body_end, resume)) = nested_range(self.code, i)
            {
                let (hi, _) = stack.pop()?;
                let (lo, _) = stack.pop()?;
                self.nested_range(lo, hi, inclusive, var, (body_start, body_end), joins)?;
                i = resume;
                continue;
            }
            if let Some((cond_start, cond_end, body_end)) = nested_while(self.code, i) {
                self.nested_while((cond_start, cond_end), body_end, joins)?;
                i = body_end;
                continue;
            }
            match op {
                OpCode::GetLocal(s) if self.elem_reads.iter().any(|e| e.0 == *s) => {
                    let v = self.b.ins().iconst(types::I64, i64::from(*s));
                    stack.push((v, K::Arr(*s)));
                }
                OpCode::GetLocal(s) | OpCode::GetLocalMetaAssign { slot: s, .. } => {
                    let (v, k) = *self.vars.get(s)?;
                    stack.push((self.b.use_var(v), K::of(k)));
                }
                OpCode::Index { .. } => {
                    let (index, _) = stack.pop()?;
                    let (row, k) = stack.pop()?;
                    let (slot, second) = match k {
                        K::Arr(s) => (s, false),
                        K::Row(s) => (s, true),
                        _ => return None,
                    };
                    let &(_, shape, kind) = self.elem_reads.iter().find(|e| e.0 == slot)?;
                    use super::vm_loop_kernel_types::ElemShape;
                    let v = match (shape, second) {
                        (ElemShape::Array, _) => self.elem_read(slot, index, kind),
                        (ElemShape::Hash, _) => self.hash_read(slot, index, kind),
                        (ElemShape::Grid, false) => {
                            stack.push((index, K::Row(slot)));
                            i += 1;
                            continue;
                        }
                        (ElemShape::Grid, true) => self.elem2_read(slot, row, index, kind),
                    };
                    stack.push((v, K::of(kind)));
                }
                OpCode::IndexAssignLocal {
                    slot,
                    is_positional,
                    ..
                } => {
                    let (v, k) = stack.pop()?;
                    let (index, key_kind) = stack.pop()?;
                    if *is_positional {
                        self.elem_store(*slot, index, v, k);
                    } else if let K::IntKey(prefix) = key_kind {
                        self.hash_store_prefixed(*slot, prefix, index, v, k);
                    } else {
                        self.hash_store(*slot, index, v, k);
                    }
                    stack.push((v, k));
                }
                OpCode::SinkPopAssign => {
                    stack.pop()?;
                }
                OpCode::PreIncrement(_, Some(s))
                | OpCode::PreDecrement(_, Some(s))
                | OpCode::PostIncrement(_, Some(s))
                | OpCode::PostDecrement(_, Some(s)) => {
                    let v = self.incdec(op, *s)?;
                    stack.push((v, K::Int));
                }
                OpCode::BitAnd | OpCode::BitOr | OpCode::BitXor | OpCode::BitShiftRight => {
                    let (y, _) = stack.pop()?;
                    let (x, _) = stack.pop()?;
                    let r = self.bitop(op, x, y);
                    stack.push((r, K::Int));
                }
                OpCode::GetGlobal(_) => {
                    debug_assert_eq!(self.var, Some(LoopVar::Topic));
                    stack.push((self.b.use_var(self.counter), K::Int));
                }
                // A `"prefix$i"` key's pieces: the constant has no machine
                // value (its index travels in the kind), the key carries the Int.
                OpCode::LoadConst(c) if super::vm_loop_kernel_types::str_const(self.code, *c) => {
                    let none = self.b.ins().iconst(types::I64, 0);
                    stack.push((none, K::StrConst(*c)));
                }
                OpCode::StringConcat(2) => {
                    let (y, _) = stack.pop()?;
                    let (_, x) = stack.pop()?;
                    let K::StrConst(c) = x else {
                        return None;
                    };
                    stack.push((y, K::IntKey(c)));
                }
                OpCode::LoadConst(c) => {
                    let (k, iv, fv) = num_const(self.code, *c)?;
                    let v = if k == K::Int {
                        self.b.ins().iconst(types::I64, iv)
                    } else {
                        self.b.ins().f64const(fv)
                    };
                    stack.push((v, k));
                }
                OpCode::Add | OpCode::Sub | OpCode::Mul => {
                    let (y, ky) = stack.pop()?;
                    let (x, kx) = stack.pop()?;
                    if kx == K::Int && ky == K::Int {
                        let (r, of) = match op {
                            OpCode::Add => self.b.ins().sadd_overflow(x, y),
                            OpCode::Sub => self.b.ins().ssub_overflow(x, y),
                            _ => self.b.ins().smul_overflow(x, y),
                        };
                        let ok = self.b.ins().icmp_imm_s(IntCC::Equal, of, 0);
                        self.guard(ok);
                        stack.push((r, K::Int));
                    } else {
                        let x = self.as_f64(x, kx);
                        let y = self.as_f64(y, ky);
                        let r = match op {
                            OpCode::Add => self.b.ins().fadd(x, y),
                            OpCode::Sub => self.b.ins().fsub(x, y),
                            _ => self.b.ins().fmul(x, y),
                        };
                        stack.push((r, K::Num));
                    }
                }
                OpCode::IntDiv | OpCode::IntMod | OpCode::Mod => {
                    // Floored, as `Integer::div_floor`/`mod_floor` and Tier B's
                    // `emit_int_divmod`; a zero divisor and `MIN div -1` bail.
                    let (y, _) = stack.pop()?;
                    let (x, _) = stack.pop()?;
                    let nonzero = self.b.ins().icmp_imm_s(IntCC::NotEqual, y, 0);
                    self.guard(nonzero);
                    let is_min = self.b.ins().icmp_imm_s(IntCC::Equal, x, i64::MIN);
                    let is_m1 = self.b.ins().icmp_imm_s(IntCC::Equal, y, -1);
                    let both = self.b.ins().band(is_min, is_m1);
                    let fine = self.b.ins().icmp_imm_s(IntCC::Equal, both, 0);
                    self.guard(fine);
                    let rem = self.b.ins().srem(x, y);
                    let rem_nz = self.b.ins().icmp_imm_s(IntCC::NotEqual, rem, 0);
                    let sx = self.b.ins().bxor(x, y);
                    let differ = self.b.ins().icmp_imm_s(IntCC::SignedLessThan, sx, 0);
                    let correct = self.b.ins().band(rem_nz, differ);
                    let zero = self.b.ins().iconst(types::I64, 0);
                    let r = if matches!(op, OpCode::IntDiv) {
                        let q = self.b.ins().sdiv(x, y);
                        let one = self.b.ins().iconst(types::I64, 1);
                        let adj = self.b.ins().select(correct, one, zero);
                        self.b.ins().isub(q, adj)
                    } else {
                        let adj = self.b.ins().select(correct, y, zero);
                        self.b.ins().iadd(rem, adj)
                    };
                    stack.push((r, K::Int));
                }
                OpCode::NumLt
                | OpCode::NumLe
                | OpCode::NumGt
                | OpCode::NumGe
                | OpCode::NumEq
                | OpCode::NumNe => {
                    let (y, k) = stack.pop()?;
                    let (x, _) = stack.pop()?;
                    let r = if k == K::Int {
                        let cc = match op {
                            OpCode::NumLt => IntCC::SignedLessThan,
                            OpCode::NumLe => IntCC::SignedLessThanOrEqual,
                            OpCode::NumGt => IntCC::SignedGreaterThan,
                            OpCode::NumGe => IntCC::SignedGreaterThanOrEqual,
                            OpCode::NumEq => IntCC::Equal,
                            _ => IntCC::NotEqual,
                        };
                        self.b.ins().icmp(cc, x, y)
                    } else {
                        let cc = match op {
                            OpCode::NumLt => FloatCC::LessThan,
                            OpCode::NumLe => FloatCC::LessThanOrEqual,
                            OpCode::NumGt => FloatCC::GreaterThan,
                            OpCode::NumGe => FloatCC::GreaterThanOrEqual,
                            OpCode::NumEq => FloatCC::Equal,
                            _ => FloatCC::NotEqual,
                        };
                        self.b.ins().fcmp(cc, x, y)
                    };
                    stack.push((r, K::Bool));
                }
                OpCode::Dup => stack.push(*stack.last()?),
                OpCode::Pop => {
                    stack.pop()?;
                }
                OpCode::ContainerizePair | OpCode::WrapVarRef { .. } => {}
                OpCode::CallFunc { arity, .. } => {
                    // A direct call of the sub kernel the entry resolved into
                    // this call's slot; a bail in it bails this iteration.
                    let mf = MemFlagsData::trusted();
                    let k = self.next_call;
                    self.next_call += 1;
                    let at = stack.len().checked_sub(*arity as usize)?;
                    let kctx = self.b.ins().iadd_imm_s(self.ctxp, self.kctx_offset as i64);
                    let mut cargs = vec![kctx];
                    cargs.extend(stack.drain(at..).map(|(v, _)| v));
                    let off = self.callees_offset + 8 * k as i32;
                    let f = self.b.ins().load(self.ptr, mf, self.ctxp, off);
                    let sig = *self.call_sigs.get(*arity as usize)?;
                    let call = self.b.ins().call_indirect(sig, f, &cargs);
                    let r = self.b.inst_results(call)[0];
                    let flag = self.b.ins().load(types::I32, mf, kctx, 0);
                    let ok = self.b.ins().icmp_imm_s(IntCC::Equal, flag, 0);
                    self.guard(ok);
                    stack.push((r, K::Int));
                }
                OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) => {
                    let (c, k) = stack.pop()?;
                    let c = self.truthy(c, k);
                    let to = target(*t as usize, &blocks)?;
                    let next = self.b.create_block();
                    let a = args(&stack);
                    if matches!(op, OpCode::JumpIfFalse(_)) {
                        self.b.ins().brif(c, next, &[], to, &a);
                    } else {
                        self.b.ins().brif(c, to, &a, next, &[]);
                    }
                    self.b.switch_to_block(next);
                }
                OpCode::Jump(t) => {
                    let to = target(*t as usize, &blocks)?;
                    let a = args(&stack);
                    self.b.ins().jump(to, &a);
                    open = false;
                }
                OpCode::CheckReadOnly(_)
                | OpCode::CheckReadOnlyModify(_)
                | OpCode::MarkArrayShareSource(_)
                | OpCode::SetVarDynamic { .. } => {}
                OpCode::SetLocal(s) | OpCode::SetLocalDecl { slot: s, .. } => {
                    let (v, _) = stack.pop()?;
                    let (var, _) = *self.vars.get(s)?;
                    self.b.def_var(var, v);
                }
                _ => return None,
            }
            i += 1;
        }
        if open {
            let a = args(&stack);
            self.b.ins().jump(end_blk, &a);
        }
        self.b.switch_to_block(end_blk);
        let params = self.b.block_params(end_blk).to_vec();
        Some(params.into_iter().zip(end_kinds).collect())
    }
}
