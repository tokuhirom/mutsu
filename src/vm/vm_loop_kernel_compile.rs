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
use super::vm_loop_kernel_emit::Em;
use super::vm_loop_kernel_types::{LoopShape, LoopVar};
use super::vm_loop_kernel_typing::Typed;
use super::*;
use crate::opcode::kernel_slot::SlotKind;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlagsData, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module};

/// Back-edges between two runs of the ordinary safepoint poll.
pub(super) const LOOP_POLL_EVERY: u32 = 4096;

/// The most sub calls one loop kernel makes (its callee table's size).
pub(super) const MAX_CALLS: usize = 8;

/// Per-run state of a loop kernel.
#[repr(C)]
pub(super) struct LoopCtx {
    /// Back-edges left before the next poll.
    pub(super) poll: u32,
    /// 1 when the range's end is inclusive.
    pub(super) inclusive: u32,
    /// The range's end.
    pub(super) end: i64,
    /// The context every sub kernel the loop calls runs with.
    pub(super) kctx: super::vm_kernel_compile::KernelCtx,
    /// Each call's sub-kernel entry, resolved at entry.
    pub(super) callees: [usize; MAX_CALLS],
    /// The interpreter and the code the element helpers act on
    /// (`vm_kernel_elem`); set by the entry for the length of the run.
    pub(super) interp: *mut Interpreter,
    pub(super) code: *const CompiledCode,
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
    elem_reads: &[(u32, SlotKind)],
    typed: &Typed,
) -> Option<usize> {
    with_engine(|module, n| build(module, n, code, shape, carried, elem_reads, typed))
}

fn build(
    module: &mut cranelift_jit::JITModule,
    n: u64,
    code: &CompiledCode,
    shape: &LoopShape,
    carried: &[(u32, SlotKind)],
    elem_reads: &[(u32, SlotKind)],
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
    let call_sigs: Vec<_> = (0..=super::vm_kernel_compile::MAX_KERNEL_PARAMS)
        .map(|n| {
            let mut sig = cranelift_codegen::ir::Signature::new(module.isa().default_call_conv());
            sig.params.push(AbiParam::new(ptr));
            for _ in 0..n {
                sig.params.push(AbiParam::new(types::I64));
            }
            sig.returns.push(AbiParam::new(types::I64));
            b.import_signature(sig)
        })
        .collect();
    let elem_sig = |b: &mut FunctionBuilder, value: types::Type| {
        let mut sig = cranelift_codegen::ir::Signature::new(module.isa().default_call_conv());
        for t in [ptr, ptr, types::I32, types::I64, value] {
            sig.params.push(AbiParam::new(t));
        }
        sig.returns.push(AbiParam::new(types::I32));
        b.import_signature(sig)
    };
    let elem = super::vm_loop_kernel_emit_elem::ElemSigs {
        read: elem_sig(&mut b, ptr),
        store_int: elem_sig(&mut b, types::I64),
        store_num: elem_sig(&mut b, types::F64),
        interp_offset: std::mem::offset_of!(LoopCtx, interp) as i32,
        code_offset: std::mem::offset_of!(LoopCtx, code) as i32,
    };
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
    let mut em = Em {
        b,
        code,
        vars,
        bail,
        var: shape.loop_var(),
        counter,
        ctxp,
        poll_sig,
        poll_fn: loop_poll as *const () as usize,
        poll_offset: CTX_POLL,
        ptr,
        callees_offset: std::mem::offset_of!(LoopCtx, callees) as i32,
        kctx_offset: std::mem::offset_of!(LoopCtx, kctx) as i32,
        call_sigs,
        next_call: 0,
        elem_reads: elem_reads.to_vec(),
        elem,
    };
    em.poll();
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
        em.b.ins()
            .store(mf, v, buf, 8 * (2 + carried.len() as i32 + j as i32));
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
    super::vm_jit_verify::define_function_checked(module, id, &mut ctx, "loop kernel")?;
    module.clear_context(&mut ctx);
    module.finalize_definitions().ok()?;
    Some(module.get_finalized_function(id) as usize)
}
