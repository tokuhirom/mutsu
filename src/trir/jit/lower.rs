//! Lowering one [`TrChunk`] to a Cranelift function (ADR-0116 §2.2).
//!
//! - **The int bank** is one `i64` variable per depth ([`super::shape`]
//!   proves the depth at every op), so `LoadI; ConstI; AddI; StoreI` is a
//!   register add.
//! - **The native slots** are variables too. A slot a call may read or write
//!   through a reference (a `TrArg::Native` argument) is written to the
//!   frame's memory before such a call and read back after it; nothing else
//!   can reach a slot.
//! - **Every jump** is a native branch; a backward one polls the safepoint.
//! - **Every other op** calls [`super::shims::trir_jit_step`], which runs the
//!   interpreter's own definition of it.

use super::TrJitFn;
use super::shape;
use super::shims;
use crate::trir::frame::TrFrame;
use crate::trir::{TrArg, TrChunk, TrOp};

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    AbiParam, Block, InstBuilder, MemFlagsData, SigRef, StackSlotData, StackSlotKind, Type, Value,
    types,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module};

/// Whether `op` is emitted inline (rather than stepped through the shim).
fn is_inline(op: &TrOp) -> bool {
    use TrOp::*;
    matches!(
        op,
        ConstI(_)
            | LoadI(_)
            | StoreI(_)
            | IncI(_)
            | DecI(_)
            | IncIVoid(_)
            | DecIVoid(_)
            | GetRefI(_)
            | SetRefI(_)
            | IncRefI(_)
            | IncRefIVoid(_)
            | DecRefI(_)
            | DecRefIVoid(_)
            | AddI
            | SubI
            | MulI
            | NegI
            | BitAndI
            | BitOrI
            | BitXorI
            | ShlI
            | ShrI
            | EqI
            | NeI
            | LtI
            | LeI
            | GtI
            | GeI
            | NotI
            | AddN
            | SubN
            | MulN
            | DivN
            | EqN
            | LtN
            | LeN
            | GtN
            | GeN
            | IntToNum
            | NumToInt
            | WrapI { .. }
            | PopI
            | Jump(_)
            | JumpIfFalseI(_)
            | JumpIfTrueI(_)
            | JumpIfFalseKeepI(_)
            | JumpIfTrueKeepI(_)
    )
}

/// Whether a stepped `op` can reach a frame slot through a reference.
fn reads_slots(op: &TrOp) -> bool {
    matches!(op, TrOp::CallTr(_) | TrOp::CallGen(_))
}

/// Whether `op` ends the chunk.
fn is_return(op: &TrOp) -> bool {
    matches!(
        op,
        TrOp::ReturnI | TrOp::ReturnN | TrOp::ReturnObj | TrOp::ReturnNil
    )
}

/// Lower `chunk`. `None` declines it: a shape the analysis rejects, a host
/// without 64-bit pointers, or a Cranelift failure.
// Cost: O(n), n = the chunk's op count.
pub(super) fn compile(chunk: &TrChunk) -> Option<TrJitFn> {
    let ops = &chunk.ops;
    if ops.is_empty() {
        return None;
    }
    let depths = shape::int_depths(chunk).ok()?;

    // The deepest the int bank gets, and the widest a stepped op's operand
    // buffer has to be.
    let mut max_depth = 0u32;
    let mut max_io = 1u32;
    for (ip, d) in depths.iter().enumerate() {
        let Some(d) = *d else { continue };
        let (pops, pushes) = shape::int_effect(chunk, &ops[ip]);
        max_depth = max_depth.max(d).max(d - pops + pushes);
        if !is_inline(&ops[ip]) {
            max_io = max_io.max(pops).max(pushes);
        }
    }
    let mut is_target = vec![false; ops.len() + 1];
    for op in ops {
        if let Some(t) = shape::jump_target(op) {
            is_target[t] = true;
        }
    }
    let n_native = chunk.n_native as usize;
    let mut spilled = vec![false; n_native];
    for call in &chunk.calls {
        for a in &call.args {
            if let TrArg::Native(s) = a
                && let Some(flag) = spilled.get_mut(*s as usize)
            {
                *flag = true;
            }
        }
    }
    let uses_refs = ops.iter().any(|op| {
        matches!(
            op,
            TrOp::GetRefI(_)
                | TrOp::SetRefI(_)
                | TrOp::IncRefI(_)
                | TrOp::IncRefIVoid(_)
                | TrOp::DecRefI(_)
                | TrOp::DecRefIVoid(_)
        )
    });
    let needs_nl = uses_refs || spilled.iter().any(|s| *s);

    let mut guard = crate::vm::vm_jit_engine::lock();
    let engine = crate::vm::vm_jit_engine::get_or_init(&mut guard)?;
    let module = &mut engine.module;
    let ptr = module.target_config().pointer_type();
    if ptr != types::I64 {
        return None;
    }
    let mut ctx = module.make_context();
    for _ in 0..4 {
        ctx.func.signature.params.push(AbiParam::new(ptr));
    }
    ctx.func.signature.returns.push(AbiParam::new(types::I32));

    let mut fb_ctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
    let mut sig = |params: &[Type], ret: Option<Type>| {
        let mut s = module.make_signature();
        for p in params {
            s.params.push(AbiParam::new(*p));
        }
        if let Some(r) = ret {
            s.returns.push(AbiParam::new(r));
        }
        b.import_signature(s)
    };
    let i32t = types::I32;
    let step_sig = sig(&[ptr, ptr, ptr, ptr, i32t, ptr, i32t, i32t], Some(i32t));
    let nl_sig = sig(&[ptr], Some(ptr));
    let poll_sig = sig(&[ptr], None);

    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);
    let params = b.block_params(entry).to_vec();
    let (interp, chunkp, framep, fnsp) = (params[0], params[1], params[2], params[3]);
    let io = b.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        max_io * 8,
        3,
    ));

    let slot_vars: Vec<Variable> = (0..n_native).map(|_| b.declare_var(types::I64)).collect();
    let stack_vars: Vec<Variable> = (0..=max_depth).map(|_| b.declare_var(types::I64)).collect();
    let nlp = b.declare_var(ptr);
    let sbase = b.declare_var(ptr);

    let call_fn = |b: &mut FunctionBuilder, sig: SigRef, f: usize, args: &[Value]| {
        let callee = b.ins().iconst(ptr, f as i64);
        let call = b.ins().call_indirect(sig, callee, args);
        b.inst_results(call).first().copied()
    };

    // The frame's slot base: `nl + nbase * 8`, re-derived whenever `nl` may
    // have moved.
    let nbase_off = std::mem::offset_of!(TrFrame, nbase) as i32;
    let nbase32 = b
        .ins()
        .load(i32t, MemFlagsData::trusted(), framep, nbase_off);
    let nbase = b.ins().uextend(types::I64, nbase32);
    let nbase_bytes = b.ins().imul_imm(nbase, 8);
    let refresh_nl = |b: &mut FunctionBuilder| {
        let p = call_fn(
            b,
            nl_sig,
            shims::trir_jit_nl as *const () as usize,
            &[interp],
        )
        .expect("trir_jit_nl returns a pointer");
        b.def_var(nlp, p);
        let s = b.ins().iadd(p, nbase_bytes);
        b.def_var(sbase, s);
    };
    // Every slot starts as what the door bound into the frame.
    refresh_nl(&mut b);
    for (k, var) in slot_vars.iter().enumerate() {
        let base = b.use_var(sbase);
        let v = b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), base, (k * 8) as i32);
        b.def_var(*var, v);
    }

    let mut blocks: std::collections::HashMap<usize, Block> = std::collections::HashMap::new();
    for (ip, t) in is_target.iter().enumerate() {
        if *t && depths.get(ip).copied().flatten().is_some() {
            blocks.insert(ip, b.create_block());
        }
    }

    let flags = MemFlagsData::trusted();
    let mut terminated = false;
    for (ip, op) in ops.iter().enumerate() {
        let Some(d) = depths[ip] else { continue };
        let d = d as usize;
        if let Some(&blk) = blocks.get(&ip) {
            if !terminated {
                b.ins().jump(blk, &[]);
            }
            b.switch_to_block(blk);
            terminated = false;
        } else if terminated {
            // Reached only by falling through a terminator: the analysis and
            // the emitter disagree about this chunk.
            return None;
        }
        let top = |b: &mut FunctionBuilder, k: usize| b.use_var(stack_vars[d - k]);
        let slot_addr = |b: &mut FunctionBuilder, var: Variable| {
            let r = b.use_var(var);
            let off = b.ins().imul_imm(r, 8);
            let p = b.use_var(nlp);
            b.ins().iadd(p, off)
        };
        let f64_of = |b: &mut FunctionBuilder, v: Value| b.ins().bitcast(types::F64, flags, v);
        let i64_of = |b: &mut FunctionBuilder, v: Value| b.ins().bitcast(types::I64, flags, v);
        match op {
            TrOp::ConstI(v) => {
                let c = b.ins().iconst(types::I64, *v);
                b.def_var(stack_vars[d], c);
            }
            TrOp::LoadI(n) => {
                let v = b.use_var(slot_vars[*n as usize]);
                b.def_var(stack_vars[d], v);
            }
            TrOp::StoreI(n) => {
                let v = top(&mut b, 1);
                b.def_var(slot_vars[*n as usize], v);
            }
            TrOp::IncI(n) | TrOp::DecI(n) | TrOp::IncIVoid(n) | TrOp::DecIVoid(n) => {
                let var = slot_vars[*n as usize];
                let old = b.use_var(var);
                let delta = if matches!(op, TrOp::IncI(_) | TrOp::IncIVoid(_)) {
                    1
                } else {
                    -1
                };
                let new = b.ins().iadd_imm(old, delta);
                b.def_var(var, new);
                if matches!(op, TrOp::IncI(_) | TrOp::DecI(_)) {
                    b.def_var(stack_vars[d], new);
                }
            }
            TrOp::GetRefI(n) => {
                let a = slot_addr(&mut b, slot_vars[*n as usize]);
                let v = b.ins().load(types::I64, flags, a, 0);
                b.def_var(stack_vars[d], v);
            }
            TrOp::SetRefI(n) => {
                let v = top(&mut b, 1);
                let a = slot_addr(&mut b, slot_vars[*n as usize]);
                b.ins().store(flags, v, a, 0);
            }
            TrOp::IncRefI(n) | TrOp::IncRefIVoid(n) | TrOp::DecRefI(n) | TrOp::DecRefIVoid(n) => {
                let a = slot_addr(&mut b, slot_vars[*n as usize]);
                let old = b.ins().load(types::I64, flags, a, 0);
                let delta = if matches!(op, TrOp::IncRefI(_) | TrOp::IncRefIVoid(_)) {
                    1
                } else {
                    -1
                };
                let new = b.ins().iadd_imm(old, delta);
                b.ins().store(flags, new, a, 0);
                if matches!(op, TrOp::IncRefI(_) | TrOp::DecRefI(_)) {
                    b.def_var(stack_vars[d], new);
                }
            }
            TrOp::AddI
            | TrOp::SubI
            | TrOp::MulI
            | TrOp::BitAndI
            | TrOp::BitOrI
            | TrOp::BitXorI
            | TrOp::ShlI
            | TrOp::ShrI => {
                let l = top(&mut b, 2);
                let r = top(&mut b, 1);
                let v = match op {
                    TrOp::AddI => b.ins().iadd(l, r),
                    TrOp::SubI => b.ins().isub(l, r),
                    TrOp::MulI => b.ins().imul(l, r),
                    TrOp::BitAndI => b.ins().band(l, r),
                    TrOp::BitOrI => b.ins().bor(l, r),
                    TrOp::BitXorI => b.ins().bxor(l, r),
                    // Rust's `wrapping_shl`/`wrapping_shr` mask the amount to
                    // the width, as Cranelift's shifts do.
                    TrOp::ShlI => b.ins().ishl(l, r),
                    _ => b.ins().sshr(l, r),
                };
                b.def_var(stack_vars[d - 2], v);
            }
            TrOp::EqI | TrOp::NeI | TrOp::LtI | TrOp::LeI | TrOp::GtI | TrOp::GeI => {
                let l = top(&mut b, 2);
                let r = top(&mut b, 1);
                let cc = match op {
                    TrOp::EqI => IntCC::Equal,
                    TrOp::NeI => IntCC::NotEqual,
                    TrOp::LtI => IntCC::SignedLessThan,
                    TrOp::LeI => IntCC::SignedLessThanOrEqual,
                    TrOp::GtI => IntCC::SignedGreaterThan,
                    _ => IntCC::SignedGreaterThanOrEqual,
                };
                let c = b.ins().icmp(cc, l, r);
                let v = b.ins().uextend(types::I64, c);
                b.def_var(stack_vars[d - 2], v);
            }
            TrOp::NegI => {
                let x = top(&mut b, 1);
                let v = b.ins().ineg(x);
                b.def_var(stack_vars[d - 1], v);
            }
            TrOp::NotI => {
                let x = top(&mut b, 1);
                let c = b.ins().icmp_imm(IntCC::Equal, x, 0);
                let v = b.ins().uextend(types::I64, c);
                b.def_var(stack_vars[d - 1], v);
            }
            TrOp::AddN | TrOp::SubN | TrOp::MulN | TrOp::DivN => {
                let l = top(&mut b, 2);
                let r = top(&mut b, 1);
                let (l, r) = (f64_of(&mut b, l), f64_of(&mut b, r));
                let v = match op {
                    TrOp::AddN => b.ins().fadd(l, r),
                    TrOp::SubN => b.ins().fsub(l, r),
                    TrOp::MulN => b.ins().fmul(l, r),
                    _ => b.ins().fdiv(l, r),
                };
                let v = i64_of(&mut b, v);
                b.def_var(stack_vars[d - 2], v);
            }
            TrOp::EqN | TrOp::LtN | TrOp::LeN | TrOp::GtN | TrOp::GeN => {
                let l = top(&mut b, 2);
                let r = top(&mut b, 1);
                let (l, r) = (f64_of(&mut b, l), f64_of(&mut b, r));
                // Rust's `f64` comparisons are the ordered ones: false on NaN.
                let cc = match op {
                    TrOp::EqN => FloatCC::Equal,
                    TrOp::LtN => FloatCC::LessThan,
                    TrOp::LeN => FloatCC::LessThanOrEqual,
                    TrOp::GtN => FloatCC::GreaterThan,
                    _ => FloatCC::GreaterThanOrEqual,
                };
                let c = b.ins().fcmp(cc, l, r);
                let v = b.ins().uextend(types::I64, c);
                b.def_var(stack_vars[d - 2], v);
            }
            TrOp::IntToNum => {
                let x = top(&mut b, 1);
                let f = b.ins().fcvt_from_sint(types::F64, x);
                let v = i64_of(&mut b, f);
                b.def_var(stack_vars[d - 1], v);
            }
            TrOp::NumToInt => {
                // `as i64`: saturating, NaN to 0 — `fcvt_to_sint_sat`.
                let x = top(&mut b, 1);
                let f = f64_of(&mut b, x);
                let v = b.ins().fcvt_to_sint_sat(types::I64, f);
                b.def_var(stack_vars[d - 1], v);
            }
            TrOp::WrapI { bits, signed } => {
                let x = top(&mut b, 1);
                let shift = 64 - i64::from(*bits);
                let v = if shift <= 0 || shift >= 64 {
                    x
                } else {
                    let up = b.ins().ishl_imm(x, shift);
                    if *signed {
                        b.ins().sshr_imm(up, shift)
                    } else {
                        b.ins().ushr_imm(up, shift)
                    }
                };
                b.def_var(stack_vars[d - 1], v);
            }
            TrOp::PopI => {}
            TrOp::Jump(t) => {
                let t = *t as usize;
                if t <= ip {
                    call_fn(
                        &mut b,
                        poll_sig,
                        shims::trir_jit_poll as *const () as usize,
                        &[interp],
                    );
                }
                b.ins().jump(*blocks.get(&t)?, &[]);
                terminated = true;
            }
            TrOp::JumpIfFalseI(t)
            | TrOp::JumpIfTrueI(t)
            | TrOp::JumpIfFalseKeepI(t)
            | TrOp::JumpIfTrueKeepI(t) => {
                let t = *t as usize;
                if t <= ip {
                    call_fn(
                        &mut b,
                        poll_sig,
                        shims::trir_jit_poll as *const () as usize,
                        &[interp],
                    );
                }
                let c = top(&mut b, 1);
                let target = *blocks.get(&t)?;
                let fall = b.create_block();
                if matches!(op, TrOp::JumpIfFalseI(_) | TrOp::JumpIfFalseKeepI(_)) {
                    b.ins().brif(c, fall, &[], target, &[]);
                } else {
                    b.ins().brif(c, target, &[], fall, &[]);
                }
                b.switch_to_block(fall);
            }
            _ => {
                // Stepped through the interpreter's own definition of the op.
                let (pops, pushes) = shape::int_effect(chunk, op);
                let (pops, pushes) = (pops as usize, pushes as usize);
                for k in 0..pops {
                    let v = b.use_var(stack_vars[d - pops + k]);
                    b.ins().stack_store(v, io, (k * 8) as i32);
                }
                let touches = reads_slots(op);
                if touches {
                    for (k, var) in slot_vars.iter().enumerate() {
                        if spilled[k] {
                            let v = b.use_var(*var);
                            let base = b.use_var(sbase);
                            b.ins().store(flags, v, base, (k * 8) as i32);
                        }
                    }
                }
                let ipv = b.ins().iconst(i32t, ip as i64);
                let iop = b.ins().stack_addr(ptr, io, 0);
                let nin = b.ins().iconst(i32t, pops as i64);
                let nout = b.ins().iconst(i32t, pushes as i64);
                let status = call_fn(
                    &mut b,
                    step_sig,
                    shims::trir_jit_step as *const () as usize,
                    &[interp, chunkp, framep, fnsp, ipv, iop, nin, nout],
                )?;
                if is_return(op) {
                    b.ins().return_(&[status]);
                    terminated = true;
                    continue;
                }
                let out = b.create_block();
                let cont = b.create_block();
                b.ins().brif(status, out, &[], cont, &[]);
                b.switch_to_block(out);
                b.ins().return_(&[status]);
                b.switch_to_block(cont);
                if needs_nl {
                    refresh_nl(&mut b);
                }
                if touches {
                    for (k, var) in slot_vars.iter().enumerate() {
                        if spilled[k] {
                            let base = b.use_var(sbase);
                            let v = b.ins().load(types::I64, flags, base, (k * 8) as i32);
                            b.def_var(*var, v);
                        }
                    }
                }
                for k in 0..pushes {
                    let v = b.ins().stack_load(types::I64, io, (k * 8) as i32);
                    b.def_var(stack_vars[d - pops + k], v);
                }
            }
        }
    }
    if !terminated {
        // Falling off the end: the compiler ends every chunk in a return, so
        // this is unreachable; report it as the internal error it would be.
        let err = b
            .ins()
            .iconst(i32t, crate::vm::vm_jit::JIT_STATUS_ERR as i64);
        b.ins().return_(&[err]);
    }
    b.seal_all_blocks();
    b.finalize();

    engine.fn_counter += 1;
    let name = format!("mutsu_trir_{}", engine.fn_counter);
    let module = &mut engine.module;
    let id = module
        .declare_function(&name, Linkage::Local, &ctx.func.signature)
        .ok()?;
    module.define_function(id, &mut ctx).ok()?;
    module.clear_context(&mut ctx);
    module.finalize_definitions().ok()?;
    let addr = module.get_finalized_function(id);
    // SAFETY: `addr` is the finalized code for the signature declared above,
    // which is `TrJitFn`'s; the module lives for the process lifetime.
    Some(unsafe { std::mem::transmute::<*const u8, TrJitFn>(addr) })
}
