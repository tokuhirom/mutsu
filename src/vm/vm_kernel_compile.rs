//! Native int kernels: admission and Cranelift lowering (ADR-19288 stage 1).
//!
//! A sub whose signature and bytecode pass `vm_kernel_admit` is lowered to one native
//! function over `i64` values: `extern "C" fn(ctx, a0, .., an) -> i64`. Every
//! value of the body is a machine integer or a comparison result, so nothing is
//! boxed, nothing is dispatched and a self-call is a direct machine call. Any
//! condition the kernel cannot express (an overflowing `+`/`-`/`*`, a zero
//! divisor, the depth bound) sets `KernelCtx::bail` and unwinds; the entry point
//! (`vm_kernel.rs`) then reruns the whole call on the generic path, which raises
//! the exception, promotes to a big integer or reports the recursion. Nothing is
//! ever resumed from the middle of a kernel (ADR-0004 §2.3 holds).
//!
//! Admission is a closed op list (ADR-19288 D1): an op not matched below
//! declines the routine, so a new opcode can only make kernels rarer.

use super::vm_jit_compile::with_engine;
use super::vm_kernel_admit::{Shape, admit_signature, int_const, plain_decl, scan};
use super::*;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, Block, InstBuilder, MemFlagsData, Value as ClValue, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module};

/// The most parameters a kernel takes (the native ABI passes each in a register).
pub(super) const MAX_KERNEL_PARAMS: usize = 6;

/// Kernel calls between two runs of the ordinary call-boundary safepoint.
pub(super) const POLL_EVERY: u32 = 4096;

/// Per-entry state shared by every native frame of one kernel run.
#[repr(C)]
pub(super) struct KernelCtx {
    /// Non-zero once a frame has bailed: every frame returns at once.
    pub(super) bail: u32,
    /// Native frames currently open.
    pub(super) depth: u32,
    /// Calls left before the next safepoint poll.
    pub(super) poll: u32,
    /// The depth past which a frame bails (from the native stack headroom).
    pub(super) depth_limit: u32,
}

const CTX_BAIL: i32 = 0;
const CTX_DEPTH: i32 = 4;
const CTX_POLL: i32 = 8;
const CTX_DEPTH_LIMIT: i32 = 12;

pub(crate) use crate::opcode::kernel_slot::KernelInfo;

/// The cold safepoint a kernel frame calls every [`POLL_EVERY`] calls. A
/// kernel frame holds no `Value`, so the thread may park and a collection may
/// run under it.
extern "C" fn kernel_poll(ctx: *mut KernelCtx) {
    // SAFETY: `ctx` is the `KernelCtx` the entry point passed to the kernel,
    // live on its Rust frame for the whole native run.
    unsafe { (*ctx).poll = POLL_EVERY };
    crate::vm::vm_poll::poll(crate::gc::SafepointKind::Call, 0);
}

/// Admit and compile `cf` as a kernel. `None` declines (permanently).
// Cost: O(n) admission plus one Cranelift compile, n = ops.
pub(super) fn compile(cf: &CompiledFunction) -> Option<KernelInfo> {
    let params = admit_signature(cf)?;
    let code = &*cf.code;
    let shape = scan(code, &params)?;
    let arity = params.len();
    let entry = with_engine(|module, n| build(module, n, code, &params, &shape))?;
    Some(KernelInfo {
        entry,
        arity: arity as u8,
        self_name: shape.self_name,
    })
}

fn build(
    module: &mut cranelift_jit::JITModule,
    n: u64,
    code: &CompiledCode,
    params: &[usize],
    shape: &Shape,
) -> Option<usize> {
    let frontend_config = module.target_config();
    let ptr = frontend_config.pointer_type();
    let mut ctx = module.make_context();
    ctx.func.signature.params.push(AbiParam::new(ptr));
    for _ in params {
        ctx.func.signature.params.push(AbiParam::new(types::I64));
    }
    ctx.func.signature.returns.push(AbiParam::new(types::I64));
    let name = format!("mutsu_kernel_{n}");
    let id = module
        .declare_function(&name, Linkage::Local, &ctx.func.signature)
        .ok()?;

    let mut fb_ctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
    let self_ref = module.declare_func_in_func(id, b.func);
    let mut poll_sig = cranelift_codegen::ir::Signature::new(module.isa().default_call_conv());
    poll_sig.params.push(AbiParam::new(ptr));
    let poll_sig = b.import_signature(poll_sig);
    let mf = MemFlagsData::trusted();

    let vars: Vec<Variable> = (0..code.locals.len())
        .map(|_| b.declare_var(types::I64))
        .collect();

    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);
    let ctxp = b.block_params(entry)[0];
    let args: Vec<ClValue> = b.block_params(entry)[1..].to_vec();
    for v in &vars {
        let zero = b.ins().iconst(types::I64, 0);
        b.def_var(*v, zero);
    }
    for (slot, a) in params.iter().zip(args) {
        b.def_var(vars[*slot], a);
    }

    let bail = b.create_block();
    let blocks: std::collections::BTreeMap<usize, Block> = shape
        .targets
        .iter()
        .map(|t| (*t, b.create_block()))
        .collect();

    // Prologue: the depth bound and the safepoint countdown.
    let depth = b.ins().load(types::I32, mf, ctxp, CTX_DEPTH);
    let depth = b.ins().iadd_imm_s(depth, 1);
    b.ins().store(mf, depth, ctxp, CTX_DEPTH);
    let limit = b.ins().load(types::I32, mf, ctxp, CTX_DEPTH_LIMIT);
    let too_deep = b.ins().icmp(IntCC::UnsignedGreaterThan, depth, limit);
    let body = b.create_block();
    b.ins().brif(too_deep, bail, &[], body, &[]);
    b.switch_to_block(body);
    let left = b.ins().load(types::I32, mf, ctxp, CTX_POLL);
    let left = b.ins().iadd_imm_s(left, -1);
    b.ins().store(mf, left, ctxp, CTX_POLL);
    let poll_blk = b.create_block();
    let start = b.create_block();
    b.ins().brif(left, start, &[], poll_blk, &[]);
    b.switch_to_block(poll_blk);
    let poll_fn = b
        .ins()
        .iconst(ptr, kernel_poll as *const () as usize as i64);
    b.ins().call_indirect(poll_sig, poll_fn, &[ctxp]);
    b.ins().jump(start, &[]);
    b.switch_to_block(start);

    // Return `v`: close this frame's depth first.
    let ret = |b: &mut FunctionBuilder, v: ClValue| {
        let d = b.ins().load(types::I32, mf, ctxp, CTX_DEPTH);
        let d = b.ins().iadd_imm_s(d, -1);
        b.ins().store(mf, d, ctxp, CTX_DEPTH);
        b.ins().return_(&[v]);
    };
    // Continue only when `ok` holds; otherwise bail.
    let guard = |b: &mut FunctionBuilder, ok: ClValue| {
        let cont = b.create_block();
        b.ins().brif(ok, cont, &[], bail, &[]);
        b.switch_to_block(cont);
    };

    let mut stack: Vec<ClValue> = Vec::new();
    let mut open = true;
    let mut pending_decl: Option<u32> = None;
    for (i, op) in code.ops.iter().enumerate() {
        if let Some(&blk) = blocks.get(&i) {
            if open {
                b.ins().jump(blk, &[]);
            }
            b.switch_to_block(blk);
            stack.clear();
            open = true;
        }
        if !open {
            continue;
        }
        match op {
            OpCode::GetLocal(s) => stack.push(b.use_var(vars[*s as usize])),
            OpCode::LoadConst(k) => {
                let n = int_const(code, *k)?;
                stack.push(b.ins().iconst(types::I64, n));
            }
            OpCode::Add | OpCode::Sub | OpCode::Mul => {
                let y = stack.pop()?;
                let x = stack.pop()?;
                let (r, of) = match op {
                    OpCode::Add => b.ins().sadd_overflow(x, y),
                    OpCode::Sub => b.ins().ssub_overflow(x, y),
                    _ => b.ins().smul_overflow(x, y),
                };
                let ok = b.ins().icmp_imm_s(IntCC::Equal, of, 0);
                guard(&mut b, ok);
                stack.push(r);
            }
            OpCode::IntDiv | OpCode::IntMod => {
                // Raku's floored `div`/`mod`, as Tier B's `emit_int_divmod`
                // and `Integer::div_floor`/`mod_floor`: a zero divisor and
                // `i64::MIN div -1` bail to the generic path.
                let y = stack.pop()?;
                let x = stack.pop()?;
                let nonzero = b.ins().icmp_imm_s(IntCC::NotEqual, y, 0);
                guard(&mut b, nonzero);
                let is_min = b.ins().icmp_imm_s(IntCC::Equal, x, i64::MIN);
                let is_m1 = b.ins().icmp_imm_s(IntCC::Equal, y, -1);
                let both = b.ins().band(is_min, is_m1);
                let fine = b.ins().icmp_imm_s(IntCC::Equal, both, 0);
                guard(&mut b, fine);
                let rem = b.ins().srem(x, y);
                let rem_nz = b.ins().icmp_imm_s(IntCC::NotEqual, rem, 0);
                let sx = b.ins().bxor(x, y);
                let differ = b.ins().icmp_imm_s(IntCC::SignedLessThan, sx, 0);
                let correct = b.ins().band(rem_nz, differ);
                let zero = b.ins().iconst(types::I64, 0);
                let r = if matches!(op, OpCode::IntDiv) {
                    let q = b.ins().sdiv(x, y);
                    let one = b.ins().iconst(types::I64, 1);
                    let adj = b.ins().select(correct, one, zero);
                    b.ins().isub(q, adj)
                } else {
                    let adj = b.ins().select(correct, y, zero);
                    b.ins().iadd(rem, adj)
                };
                stack.push(r);
            }
            OpCode::NumLt
            | OpCode::NumLe
            | OpCode::NumGt
            | OpCode::NumGe
            | OpCode::NumEq
            | OpCode::NumNe => {
                let y = stack.pop()?;
                let x = stack.pop()?;
                let cc = match op {
                    OpCode::NumLt => IntCC::SignedLessThan,
                    OpCode::NumLe => IntCC::SignedLessThanOrEqual,
                    OpCode::NumGt => IntCC::SignedGreaterThan,
                    OpCode::NumGe => IntCC::SignedGreaterThanOrEqual,
                    OpCode::NumEq => IntCC::Equal,
                    _ => IntCC::NotEqual,
                };
                stack.push(b.ins().icmp(cc, x, y));
            }
            OpCode::ContainerizePair => {}
            OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) => {
                let c = stack.pop()?;
                let target = *blocks.get(&(*t as usize))?;
                let next = b.create_block();
                if matches!(op, OpCode::JumpIfFalse(_)) {
                    b.ins().brif(c, next, &[], target, &[]);
                } else {
                    b.ins().brif(c, target, &[], next, &[]);
                }
                b.switch_to_block(next);
            }
            OpCode::Jump(t) => {
                let target = *blocks.get(&(*t as usize))?;
                b.ins().jump(target, &[]);
                open = false;
            }
            OpCode::Return => {
                let v = stack.pop()?;
                ret(&mut b, v);
                open = false;
            }
            OpCode::SetVarDynamic { .. } => pending_decl = plain_decl(op),
            OpCode::SetLocalDecl { slot, .. } => {
                debug_assert_eq!(pending_decl, Some(*slot));
                pending_decl = None;
                let v = stack.pop()?;
                b.def_var(vars[*slot as usize], v);
            }
            OpCode::CallFunc { arity, .. } => {
                let at = stack.len().checked_sub(*arity as usize)?;
                let mut cargs = vec![ctxp];
                cargs.extend(stack.drain(at..));
                let call = b.ins().call(self_ref, &cargs);
                let r = b.inst_results(call)[0];
                let flag = b.ins().load(types::I32, mf, ctxp, CTX_BAIL);
                let ok = b.ins().icmp_imm_s(IntCC::Equal, flag, 0);
                guard(&mut b, ok);
                stack.push(r);
            }
            _ => return None,
        }
    }
    if open {
        let v = stack.pop()?;
        ret(&mut b, v);
    }
    b.switch_to_block(bail);
    let one = b.ins().iconst(types::I32, 1);
    b.ins().store(mf, one, ctxp, CTX_BAIL);
    let zero = b.ins().iconst(types::I64, 0);
    b.ins().return_(&[zero]);
    b.seal_all_blocks();
    b.finalize(frontend_config);

    module.define_function(id, &mut ctx).ok()?;
    module.clear_context(&mut ctx);
    module.finalize_definitions().ok()?;
    Some(module.get_finalized_function(id) as usize)
}
