//! Tier A bytecode-to-native translation on Cranelift (ADR-0004 §2.3, J1).
//!
//! A supported chunk is lowered opcode-by-opcode into a sequence of calls to
//! the `vm_jit_helpers` shims (subroutine threading); `Jump`/`JumpIfFalse`
//! become native branches between basic blocks, with a GC safepoint poll on
//! every backedge (ADR-0004 §2.4). No guards, no deopt: a chunk containing
//! any opcode outside the Tier A set is rejected up front and permanently
//! left to the interpreter.

use super::vm_jit::JitEntryFn;
use super::vm_jit_helpers as helpers;
use super::vm_jit_support::{noarg_shim, step_supported};
use super::vm_jit_tier_b::{IntArith, NumCmp, TierB};
use super::*;

use cranelift_codegen::ir::{AbiParam, Block, InstBuilder, SigRef, Type, types};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use std::sync::Mutex;

/// The one process-wide JIT module. Compiled code lives for the process
/// lifetime (entries are cached in `CompiledCode::jit.entry` as raw function
/// pointers), so the module is created once and never dropped.
struct Engine {
    module: JITModule,
    fn_counter: u64,
}

// SAFETY: `JITModule` is only manipulated under the `ENGINE` mutex; the
// finalized code memory it owns is immutable after `finalize_definitions`
// and is executed (not mutated) from any thread, which is sound regardless
// of which thread performed the compilation.
unsafe impl Send for Engine {}

static ENGINE: Mutex<Option<Engine>> = Mutex::new(None);

/// Compile `code` to a native Tier A body. `None` = bailout (unsupported
/// opcode, or an internal Cranelift failure): the caller marks the chunk so
/// it is never scanned again.
pub(super) fn compile_chunk(code: &CompiledCode) -> Option<JitEntryFn> {
    compile_range(code, 0, code.ops.len())
}

/// Compile the opcode sub-range `[start, end)` of `code` to a native body
/// (ADR-0004 J4b hot-loop entry: a compound loop's body/cond executed by
/// `run_range`). Same acceptance rules as a whole chunk, plus: every jump
/// must stay inside the range (a target of exactly `end` is the normal
/// fall-through exit). `None` = bailout, cached per range by the caller.
pub(super) fn compile_range(code: &CompiledCode, start: usize, end: usize) -> Option<JitEntryFn> {
    // Static support scan + jump-target collection. Any opcode outside the
    // Tier A set rejects the whole range (no partial compilation).
    let mut targets: std::collections::HashSet<usize> = std::collections::HashSet::new();
    for op in &code.ops[start..end] {
        if noarg_shim(op).is_some() || step_supported(op) {
            continue;
        }
        match op {
            OpCode::LoadConst(_)
            | OpCode::GetLocal(_)
            | OpCode::SetLocal(_)
            | OpCode::SetLocalDecl { .. }
            | OpCode::ContainerizePair
            | OpCode::SetSourceLine(_)
            | OpCode::CallFunc { .. }
            | OpCode::CallMethod { .. }
            | OpCode::CallMethodMut { .. } => {}
            OpCode::Jump(t)
            | OpCode::JumpIfFalse(t)
            | OpCode::JumpIfTrue(t)
            | OpCode::JumpIfNotNil(t) => {
                let t = *t as usize;
                if t < start || t > end {
                    // A jump escaping the range (can only come from a
                    // mis-shaped range request, not a loop body).
                    crate::vm::vm_stats::record_jit_bailout(op);
                    return None;
                }
                targets.insert(t);
            }
            other => {
                crate::vm::vm_stats::record_jit_bailout(other);
                return None;
            }
        }
    }
    build(code, start, end, &targets)
}

/// Helper-call signatures used by the emitted code, imported once per
/// function. All shims take the interpreter pointer first; fallible ones
/// return an `i32` status (see `vm_jit::JIT_STATUS_*`).
struct Sigs {
    /// `(interp) -> ()`
    v1: SigRef,
    /// `(interp) -> i32`
    s1: SigRef,
    /// `(interp, i64) -> ()`
    v_i64: SigRef,
    /// `(interp, code, u32) -> ()`
    v_code_u32: SigRef,
    /// `(interp, code, u32) -> i32`
    s_code_u32: SigRef,
    /// `(interp, code, u32, u32) -> i32`
    s_code_u32_u32: SigRef,
    /// `(interp, code, u32, fns) -> i32`
    s_call: SigRef,
}

fn make_sigs(module: &JITModule, b: &mut FunctionBuilder, ptr: Type) -> Sigs {
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
    Sigs {
        v1: sig(&[ptr], None),
        s1: sig(&[ptr], Some(types::I32)),
        v_i64: sig(&[ptr, types::I64], None),
        v_code_u32: sig(&[ptr, ptr, types::I32], None),
        s_code_u32: sig(&[ptr, ptr, types::I32], Some(types::I32)),
        s_code_u32_u32: sig(&[ptr, ptr, types::I32, types::I32], Some(types::I32)),
        s_call: sig(&[ptr, ptr, types::I32, ptr], Some(types::I32)),
    }
}

fn build(
    code: &CompiledCode,
    start: usize,
    end: usize,
    targets: &std::collections::HashSet<usize>,
) -> Option<JitEntryFn> {
    let mut guard = ENGINE.lock().unwrap();
    let engine = match guard.as_mut() {
        Some(e) => e,
        None => {
            let mut flags = settings::builder();
            flags.set("opt_level", "speed").ok()?;
            let isa = cranelift_native::builder()
                .ok()?
                .finish(settings::Flags::new(flags))
                .ok()?;
            let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
            guard.insert(Engine {
                module: JITModule::new(builder),
                fn_counter: 0,
            })
        }
    };
    let module = &mut engine.module;

    let ptr = module.target_config().pointer_type();
    let mut ctx = module.make_context();
    ctx.func.signature.params.push(AbiParam::new(ptr)); // interp
    ctx.func.signature.params.push(AbiParam::new(ptr)); // code
    ctx.func.signature.params.push(AbiParam::new(ptr)); // compiled_fns
    ctx.func.signature.returns.push(AbiParam::new(types::I32));

    let mut fb_ctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
    let sigs = make_sigs(module, &mut b, ptr);

    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);
    let interp = b.block_params(entry)[0];
    let codep = b.block_params(entry)[1];
    let fnsp = b.block_params(entry)[2];

    // Tier B inline emitter (ADR-0004 J4): available when the stack layout
    // probe succeeded on this target. `None` degrades every opcode below to
    // its Tier A helper-call form.
    let tier_b = if ptr == types::I64 {
        super::vm_jit_layout::layout().map(|lay| TierB {
            interp,
            ptr_ty: ptr,
            lay,
            s1: sigs.s1,
            v1: sigs.v1,
            v_code_u32: sigs.v_code_u32,
        })
    } else {
        None
    };

    let block_at: std::collections::HashMap<usize, Block> =
        targets.iter().map(|t| (*t, b.create_block())).collect();

    // Emits `call helper(args)`, with the callee address as an inline
    // constant (no symbol table needed for a process-local JIT).
    let call_helper = |b: &mut FunctionBuilder,
                       sig: SigRef,
                       f: usize,
                       args: &[cranelift_codegen::ir::Value]|
     -> Option<cranelift_codegen::ir::Value> {
        let callee = b.ins().iconst(ptr, f as i64);
        let call = b.ins().call_indirect(sig, callee, args);
        b.inst_results(call).first().copied()
    };
    // status != 0 -> return status; else continue in a fresh block.
    let check_status = |b: &mut FunctionBuilder, status: cranelift_codegen::ir::Value| {
        let err = b.create_block();
        let cont = b.create_block();
        b.ins().brif(status, err, &[], cont, &[]);
        b.switch_to_block(err);
        b.ins().return_(&[status]);
        b.switch_to_block(cont);
    };

    // `open` = the current block still needs a terminator.
    let mut open = true;
    for (i, op) in code.ops[start..end]
        .iter()
        .enumerate()
        .map(|(k, op)| (start + k, op))
    {
        if let Some(&blk) = block_at.get(&i) {
            if open {
                b.ins().jump(blk, &[]);
            }
            b.switch_to_block(blk);
            open = true;
        }
        if !open {
            // Straight-line code right after a terminator with no incoming
            // jump: unreachable, skip.
            continue;
        }
        match op {
            OpCode::LoadConst(idx) => {
                let word = code.constants[*idx as usize].nanbox_bits();
                if let Some(tb) = &tier_b
                    && crate::value::jit_words::is_refcount_free(word)
                {
                    tb.emit_load_const(
                        &mut b,
                        word,
                        codep,
                        *idx,
                        helpers::load_const as *const () as usize,
                    );
                } else {
                    let idxv = b.ins().iconst(types::I32, *idx as i64);
                    call_helper(
                        &mut b,
                        sigs.v_code_u32,
                        helpers::load_const as *const () as usize,
                        &[interp, codep, idxv],
                    );
                }
            }
            OpCode::SetSourceLine(line) => {
                if let Some(tb) = &tier_b {
                    tb.emit_set_source_line(&mut b, *line);
                } else {
                    let linev = b.ins().iconst(types::I64, *line);
                    call_helper(
                        &mut b,
                        sigs.v_i64,
                        helpers::set_source_line as *const () as usize,
                        &[interp, linev],
                    );
                }
            }
            OpCode::ContainerizePair => {
                if let Some(tb) = &tier_b {
                    tb.emit_containerize_pair(
                        &mut b,
                        helpers::containerize_pair as *const () as usize,
                    );
                } else {
                    call_helper(
                        &mut b,
                        sigs.v1,
                        helpers::containerize_pair as *const () as usize,
                        &[interp],
                    );
                }
            }
            // Tier B inline arithmetic / comparison (Int and Num fast paths
            // in native code; everything else through the Tier A shim).
            OpCode::Add if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_int_num_arith(&mut b, IntArith::Add, helpers::add as *const () as usize);
            }
            OpCode::Sub if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_int_num_arith(&mut b, IntArith::Sub, helpers::sub as *const () as usize);
            }
            OpCode::Mul if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_int_num_arith(&mut b, IntArith::Mul, helpers::mul as *const () as usize);
            }
            OpCode::NumLt if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_compare(&mut b, NumCmp::Lt, helpers::num_lt as *const () as usize);
            }
            OpCode::NumLe if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_compare(&mut b, NumCmp::Le, helpers::num_le as *const () as usize);
            }
            OpCode::NumGt if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_compare(&mut b, NumCmp::Gt, helpers::num_gt as *const () as usize);
            }
            OpCode::NumGe if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_compare(&mut b, NumCmp::Ge, helpers::num_ge as *const () as usize);
            }
            OpCode::NumEq if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_compare(&mut b, NumCmp::Eq, helpers::num_eq as *const () as usize);
            }
            OpCode::NumNe if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                tb.emit_compare(&mut b, NumCmp::Ne, helpers::num_ne as *const () as usize);
            }
            OpCode::GetLocal(idx) => {
                let idxv = b.ins().iconst(types::I32, *idx as i64);
                let status = call_helper(
                    &mut b,
                    sigs.s_code_u32,
                    helpers::get_local as *const () as usize,
                    &[interp, codep, idxv],
                )?;
                check_status(&mut b, status);
            }
            OpCode::SetLocal(idx) => {
                let idxv = b.ins().iconst(types::I32, *idx as i64);
                let status = call_helper(
                    &mut b,
                    sigs.s_code_u32,
                    helpers::set_local as *const () as usize,
                    &[interp, codep, idxv],
                )?;
                check_status(&mut b, status);
            }
            OpCode::SetLocalDecl {
                slot,
                explicit_init,
            } => {
                let slotv = b.ins().iconst(types::I32, *slot as i64);
                let initv = b.ins().iconst(types::I32, i64::from(*explicit_init));
                let status = call_helper(
                    &mut b,
                    sigs.s_code_u32_u32,
                    helpers::set_local_decl as *const () as usize,
                    &[interp, codep, slotv, initv],
                )?;
                check_status(&mut b, status);
            }
            OpCode::CallFunc { .. } => {
                let opidx = b.ins().iconst(types::I32, i as i64);
                let status = call_helper(
                    &mut b,
                    sigs.s_call,
                    helpers::call_func as *const () as usize,
                    &[interp, codep, opidx, fnsp],
                )?;
                check_status(&mut b, status);
            }
            OpCode::CallMethod { .. } | OpCode::CallMethodMut { .. } => {
                let f = if matches!(op, OpCode::CallMethod { .. }) {
                    helpers::call_method as *const () as usize
                } else {
                    helpers::call_method_mut as *const () as usize
                };
                let opidx = b.ins().iconst(types::I32, i as i64);
                let status = call_helper(&mut b, sigs.s_code_u32, f, &[interp, codep, opidx])?;
                check_status(&mut b, status);
            }
            OpCode::Jump(t) => {
                let t = *t as usize;
                if t <= i {
                    // Backedge: poll the GC safepoint so a native loop keeps
                    // participating in cooperative STW (ADR-0004 §2.4).
                    call_helper(
                        &mut b,
                        sigs.v1,
                        helpers::safepoint as *const () as usize,
                        &[interp],
                    );
                }
                b.ins().jump(block_at[&t], &[]);
                open = false;
            }
            OpCode::JumpIfFalse(t) if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                let t = *t as usize;
                tb.emit_jump_if_false(
                    &mut b,
                    block_at[&t],
                    t <= i,
                    helpers::safepoint as *const () as usize,
                    helpers::mark_failure_top as *const () as usize,
                    helpers::jump_if_false_cond as *const () as usize,
                );
            }
            OpCode::JumpIfTrue(t) if tier_b.is_some() => {
                let tb = tier_b.as_ref().unwrap();
                let t = *t as usize;
                tb.emit_jump_if_true(
                    &mut b,
                    block_at[&t],
                    t <= i,
                    helpers::safepoint as *const () as usize,
                    helpers::jump_if_true_cond as *const () as usize,
                );
            }
            OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) | OpCode::JumpIfNotNil(t) => {
                // Conditional branch: the cond helper returns 1 when the jump
                // must be taken (JumpIfFalse pops the tested value; the other
                // two peek, mirroring their interpreter arms).
                let cond_fn = match op {
                    OpCode::JumpIfFalse(_) => helpers::jump_if_false_cond as *const () as usize,
                    OpCode::JumpIfTrue(_) => helpers::jump_if_true_cond as *const () as usize,
                    _ => helpers::jump_if_not_nil_cond as *const () as usize,
                };
                let t = *t as usize;
                let cond = call_helper(&mut b, sigs.s1, cond_fn, &[interp])?;
                let next = b.create_block();
                if t <= i {
                    let poll = b.create_block();
                    b.ins().brif(cond, poll, &[], next, &[]);
                    b.switch_to_block(poll);
                    call_helper(
                        &mut b,
                        sigs.v1,
                        helpers::safepoint as *const () as usize,
                        &[interp],
                    );
                    b.ins().jump(block_at[&t], &[]);
                } else {
                    b.ins().brif(cond, block_at[&t], &[], next, &[]);
                }
                b.switch_to_block(next);
            }
            op => {
                if let Some(f) = noarg_shim(op) {
                    // Dedicated payload-free shim (arith / compare / Return —
                    // for Return, OK status = a rebound `&return` ran and
                    // execution falls through; otherwise the parked return
                    // signal exits the native body via the status check).
                    let status = call_helper(&mut b, sigs.s1, f, &[interp])?;
                    check_status(&mut b, status);
                } else if step_supported(op) {
                    // Generic straight-line step: one interpreter dispatch.
                    let opidx = b.ins().iconst(types::I32, i as i64);
                    let status = call_helper(
                        &mut b,
                        sigs.s_call,
                        helpers::step as *const () as usize,
                        &[interp, codep, opidx, fnsp],
                    )?;
                    check_status(&mut b, status);
                } else {
                    // The support scan rejected everything else.
                    unreachable!("unsupported opcode reached JIT emission")
                }
            }
        }
    }
    // Fall off the end of the chunk: OK status (result is on the stack).
    if open {
        let zero = b.ins().iconst(types::I32, 0);
        b.ins().return_(&[zero]);
    }
    // A jump target one past the range's last opcode is a normal
    // fall-through exit (`end == code.ops.len()` for a whole chunk).
    if let Some(&blk) = block_at.get(&end) {
        b.switch_to_block(blk);
        let zero = b.ins().iconst(types::I32, 0);
        b.ins().return_(&[zero]);
    }
    b.seal_all_blocks();
    b.finalize();

    engine.fn_counter += 1;
    let name = format!("mutsu_jit_{}", engine.fn_counter);
    let module = &mut engine.module;
    let id = module
        .declare_function(&name, Linkage::Local, &ctx.func.signature)
        .ok()?;
    module.define_function(id, &mut ctx).ok()?;
    module.clear_context(&mut ctx);
    module.finalize_definitions().ok()?;
    let addr = module.get_finalized_function(id);
    // SAFETY: `addr` is the finalized code for the signature declared above,
    // which matches `JitEntryFn` exactly; the module (and thus the code
    // memory) lives for the process lifetime.
    Some(unsafe { std::mem::transmute::<*const u8, JitEntryFn>(addr) })
}
