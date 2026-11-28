//! Loop kernels: Cranelift lowering (ADR-19288 D5).
//!
//! A typed region (`vm_loop_kernel_types`) becomes one native function
//! `extern "C" fn(ctx: *mut LoopCtx, buf: *mut u64) -> u32` that runs the
//! whole loop over machine words. `buf` carries the state in and out:
//!
//! | word | in | out (status 0, done) | out (status 1, bailed) |
//! |---|---|---|---|
//! | 0 | the first counter value (`for`) | the counter past the end | the counter of the iteration that bailed |
//! | 1 | 0 | iterations run | iterations completed |
//! | 2.. | each carried slot | its final value | its value at the start of the bailed iteration |
//! | then | - | each declaration's final value | - |
//!
//! A bail (an overflowing `+`/`-`/`*`, a zero divisor, `MIN div -1`) returns
//! the state as it was when the iteration began, so the driver reruns that
//! iteration generically from its start. Nothing is resumed mid-iteration.
//! Every back-edge counts down to the ordinary safepoint poll; a kernel frame
//! holds no `Value`.

use super::vm_jit_compile::with_engine;
use super::vm_loop_kernel_types::{K, LoopShape, LoopVar, Typed, num_const};
use super::*;
use crate::opcode::kernel_slot::SlotKind;

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{AbiParam, Block, InstBuilder, MemFlagsData, Value as ClValue, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module};

/// Back-edges between two runs of the ordinary safepoint poll.
pub(super) const LOOP_POLL_EVERY: u32 = 4096;

/// Per-run state of a loop kernel.
#[repr(C)]
pub(super) struct LoopCtx {
    /// Back-edges left before the next poll.
    pub(super) poll: u32,
    /// 1 when the range's end is inclusive.
    pub(super) inclusive: u32,
    /// The range's end.
    pub(super) end: i64,
}

const CTX_POLL: i32 = 0;
const CTX_INCLUSIVE: i32 = 4;
const CTX_END: i32 = 8;

extern "C" fn loop_poll(ctx: *mut LoopCtx) {
    // SAFETY: `ctx` is the `LoopCtx` the entry point passed in, live on its
    // Rust frame for the whole native run.
    unsafe { (*ctx).poll = LOOP_POLL_EVERY };
    crate::vm::vm_poll::poll(crate::gc::SafepointKind::Backedge, 0);
}

fn ty(k: SlotKind) -> types::Type {
    match k {
        SlotKind::Int => types::I64,
        SlotKind::Num => types::F64,
    }
}

/// Compile the typed region. `None` declines.
pub(super) fn compile(
    code: &CompiledCode,
    shape: &LoopShape,
    carried: &[(u32, SlotKind)],
    typed: &Typed,
) -> Option<usize> {
    with_engine(|module, n| build(module, n, code, shape, carried, typed))
}

struct Em<'f, 'c> {
    b: FunctionBuilder<'f>,
    code: &'c CompiledCode,
    vars: std::collections::BTreeMap<u32, (Variable, SlotKind)>,
    bail: Block,
    var: Option<LoopVar>,
    counter: Variable,
}

impl Em<'_, '_> {
    fn guard(&mut self, ok: ClValue) {
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
    fn truthy(&mut self, v: ClValue, k: K) -> ClValue {
        match k {
            K::Num => {
                let zero = self.b.ins().f64const(0.0);
                // `NotEqual` is unordered-or-not-equal: NaN is true, as in Raku.
                self.b.ins().fcmp(FloatCC::NotEqual, v, zero)
            }
            _ => v,
        }
    }

    /// Emit the ops of one segment. Returns the stack left at its end.
    fn segment(
        &mut self,
        start: usize,
        end: usize,
        joins: &std::collections::BTreeMap<usize, Vec<K>>,
    ) -> Option<Vec<(ClValue, K)>> {
        // Join blocks, with one block parameter per stack entry.
        let mut blocks: std::collections::BTreeMap<usize, (Block, Vec<K>)> = Default::default();
        for (&t, kinds) in joins.range(start + 1..end) {
            let blk = self.b.create_block();
            for k in kinds {
                let t = match k {
                    K::Num => types::F64,
                    K::Int => types::I64,
                    K::Bool => types::I8,
                };
                self.b.append_block_param(blk, t);
            }
            blocks.insert(t, (blk, kinds.clone()));
        }
        // The segment's own end, reached by fall-through or by a jump.
        let end_kinds: Vec<K> = joins.get(&end).cloned()?;
        let end_blk = self.b.create_block();
        for k in &end_kinds {
            let t = match k {
                K::Num => types::F64,
                K::Int => types::I64,
                K::Bool => types::I8,
            };
            self.b.append_block_param(end_blk, t);
        }
        let mut stack: Vec<(ClValue, K)> = Vec::new();
        let mut open = true;
        let jump_args = |stack: &[(ClValue, K)]| -> Vec<cranelift_codegen::ir::BlockArg> {
            stack.iter().map(|(v, _)| (*v).into()).collect()
        };
        let target = |t: usize, blocks: &std::collections::BTreeMap<usize, (Block, Vec<K>)>| {
            if t == end { Some(end_blk) } else { blocks.get(&t).map(|b| b.0) }
        };
        for i in start..end {
            if let Some((blk, kinds)) = blocks.get(&i).cloned() {
                if open {
                    let args = jump_args(&stack);
                    self.b.ins().jump(blk, &args);
                }
                self.b.switch_to_block(blk);
                let params = self.b.block_params(blk).to_vec();
                stack = params.into_iter().zip(kinds).collect();
                open = true;
            }
            if !open {
                continue;
            }
            let op = &self.code.ops[i];
            match op {
                OpCode::GetLocal(s) | OpCode::GetLocalMetaAssign { slot: s, .. } => {
                    let (v, k) = *self.vars.get(s)?;
                    stack.push((self.b.use_var(v), K::of(k)));
                }
                OpCode::GetGlobal(_) => {
                    debug_assert_eq!(self.var, Some(LoopVar::Topic));
                    stack.push((self.b.use_var(self.counter), K::Int));
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
                OpCode::IntDiv | OpCode::IntMod => {
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
                OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) => {
                    let (c, k) = stack.pop()?;
                    let c = self.truthy(c, k);
                    let to = target(*t as usize, &blocks)?;
                    let next = self.b.create_block();
                    let args = jump_args(&stack);
                    if matches!(op, OpCode::JumpIfFalse(_)) {
                        self.b.ins().brif(c, next, &[], to, &args);
                    } else {
                        self.b.ins().brif(c, to, &args, next, &[]);
                    }
                    self.b.switch_to_block(next);
                }
                OpCode::Jump(t) => {
                    let to = target(*t as usize, &blocks)?;
                    let args = jump_args(&stack);
                    self.b.ins().jump(to, &args);
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
        }
        if open {
            let args = jump_args(&stack);
            self.b.ins().jump(end_blk, &args);
        }
        self.b.switch_to_block(end_blk);
        let params = self.b.block_params(end_blk).to_vec();
        Some(params.into_iter().zip(end_kinds).collect())
    }
}

fn build(
    module: &mut cranelift_jit::JITModule,
    n: u64,
    code: &CompiledCode,
    shape: &LoopShape,
    carried: &[(u32, SlotKind)],
    typed: &Typed,
) -> Option<usize> {
    let frontend_config = module.target_config();
    let ptr = frontend_config.pointer_type();
    let mut ctx = module.make_context();
    ctx.func.signature.params.push(AbiParam::new(ptr));
    ctx.func.signature.params.push(AbiParam::new(ptr));
    ctx.func.signature.returns.push(AbiParam::new(types::I32));
    let mut fb_ctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
    let mut poll_sig = cranelift_codegen::ir::Signature::new(module.isa().default_call_conv());
    poll_sig.params.push(AbiParam::new(ptr));
    let poll_sig = b.import_signature(poll_sig);
    let mf = MemFlagsData::trusted();

    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);
    let ctxp = b.block_params(entry)[0];
    let buf = b.block_params(entry)[1];

    let counter = b.declare_var(types::I64);
    let iters = b.declare_var(types::I64);
    let w0 = b.ins().load(types::I64, mf, buf, 0);
    b.def_var(counter, w0);
    let zero = b.ins().iconst(types::I64, 0);
    b.def_var(iters, zero);

    let mut vars: std::collections::BTreeMap<u32, (Variable, SlotKind)> = Default::default();
    for (&slot, &k) in &typed.slot_kind {
        let v = b.declare_var(ty(k));
        let init = if k == SlotKind::Int {
            b.ins().iconst(types::I64, 0)
        } else {
            b.ins().f64const(0.0)
        };
        b.def_var(v, init);
        vars.insert(slot, (v, k));
    }
    // Carried slots in, after the two counter words.
    for (i, (slot, k)) in carried.iter().enumerate() {
        let off = 8 * (2 + i as i32);
        let w = b.ins().load(ty(*k), mf, buf, off);
        b.def_var(vars.get(slot)?.0, w);
    }
    // Snapshots of the counter and the carried slots at the iteration start.
    let snap_counter = b.declare_var(types::I64);
    let snaps: Vec<Variable> = carried.iter().map(|(_, k)| b.declare_var(ty(*k))).collect();
    let header = b.create_block();
    let bail = b.create_block();
    let exit = b.create_block();
    b.ins().jump(header, &[]);

    // -- header: snapshot, poll, the loop test --
    b.switch_to_block(header);
    let c = b.use_var(counter);
    b.def_var(snap_counter, c);
    for ((slot, _), snap) in carried.iter().zip(&snaps) {
        let v = b.use_var(vars.get(slot)?.0);
        b.def_var(*snap, v);
    }
    let left = b.ins().load(types::I32, mf, ctxp, CTX_POLL);
    let left = b.ins().iadd_imm_s(left, -1);
    b.ins().store(mf, left, ctxp, CTX_POLL);
    let poll_blk = b.create_block();
    let test = b.create_block();
    b.ins().brif(left, test, &[], poll_blk, &[]);
    b.switch_to_block(poll_blk);
    let poll_fn = b.ins().iconst(ptr, loop_poll as *const () as usize as i64);
    b.ins().call_indirect(poll_sig, poll_fn, &[ctxp]);
    b.ins().jump(test, &[]);
    b.switch_to_block(test);

    let mut em = Em {
        b,
        code,
        vars,
        bail,
        var: shape.loop_var(),
        counter,
    };
    let body_blk = em.b.create_block();
    match *shape {
        LoopShape::Range { var, .. } => {
            let i = em.b.use_var(counter);
            let end = em.b.ins().load(types::I64, mf, ctxp, CTX_END);
            let incl = em.b.ins().load(types::I32, mf, ctxp, CTX_INCLUSIVE);
            let le = em.b.ins().icmp(IntCC::SignedLessThanOrEqual, i, end);
            let lt = em.b.ins().icmp(IntCC::SignedLessThan, i, end);
            let more = em.b.ins().select(incl, le, lt);
            em.b.ins().brif(more, body_blk, &[], exit, &[]);
            em.b.switch_to_block(body_blk);
            if let LoopVar::Slot(s) = var {
                let (v, _) = *em.vars.get(&s)?;
                em.b.def_var(v, i);
            }
            let segs = shape.segments();
            em.segment(segs[0].0, segs[0].1, &typed.joins)?;
            // The back-edge: the next integer (overflow past `i64::MAX` bails).
            let i = em.b.use_var(counter);
            let one = em.b.ins().iconst(types::I64, 1);
            let (next, of) = em.b.ins().sadd_overflow(i, one);
            let ok = em.b.ins().icmp_imm_s(IntCC::Equal, of, 0);
            em.guard(ok);
            em.b.def_var(counter, next);
        }
        LoopShape::While { cond, body_end } => {
            let mut left = em.segment(cond.0, cond.1, &typed.joins)?;
            let (v, k) = left.pop()?;
            let t = em.truthy(v, k);
            em.b.ins().brif(t, body_blk, &[], exit, &[]);
            em.b.switch_to_block(body_blk);
            em.segment(cond.1, body_end, &typed.joins)?;
        }
    }
    let it = em.b.use_var(iters);
    let it = em.b.ins().iadd_imm_s(it, 1);
    em.b.def_var(iters, it);
    em.b.ins().jump(header, &[]);

    // -- exit: the final state out --
    em.b.switch_to_block(exit);
    let c = em.b.use_var(counter);
    em.b.ins().store(mf, c, buf, 0);
    let it = em.b.use_var(iters);
    em.b.ins().store(mf, it, buf, 8);
    for (i, (slot, _)) in carried.iter().enumerate() {
        let v = em.b.use_var(em.vars.get(slot)?.0);
        em.b.ins().store(mf, v, buf, 8 * (2 + i as i32));
    }
    for (j, (_, slot)) in typed.decls.iter().enumerate() {
        let v = em.b.use_var(em.vars.get(slot)?.0);
        em.b.ins().store(mf, v, buf, 8 * (2 + carried.len() as i32 + j as i32));
    }
    let ok = em.b.ins().iconst(types::I32, 0);
    em.b.ins().return_(&[ok]);

    // -- bail: the state at the start of the bailed iteration --
    em.b.switch_to_block(bail);
    let c = em.b.use_var(snap_counter);
    em.b.ins().store(mf, c, buf, 0);
    let it = em.b.use_var(iters);
    em.b.ins().store(mf, it, buf, 8);
    for (i, snap) in snaps.iter().enumerate() {
        let v = em.b.use_var(*snap);
        em.b.ins().store(mf, v, buf, 8 * (2 + i as i32));
    }
    let one = em.b.ins().iconst(types::I32, 1);
    em.b.ins().return_(&[one]);

    let mut b = em.b;
    b.seal_all_blocks();
    b.finalize(frontend_config);
    let name = format!("mutsu_loop_kernel_{n}");
    let id = module
        .declare_function(&name, Linkage::Local, &ctx.func.signature)
        .ok()?;
    module.define_function(id, &mut ctx).ok()?;
    module.clear_context(&mut ctx);
    module.finalize_definitions().ok()?;
    Some(module.get_finalized_function(id) as usize)
}
