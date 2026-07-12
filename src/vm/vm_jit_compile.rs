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

/// Payload-free fallible opcodes with a dedicated `(interp) -> status` shim
/// (the hot arith / compare / string family, plus `Return`). Returns the shim
/// address for emission, `None` when the opcode is not in this family.
fn noarg_shim(op: &OpCode) -> Option<usize> {
    let f: unsafe extern "C" fn(*mut Interpreter) -> u32 = match op {
        OpCode::Add => helpers::add,
        OpCode::Sub => helpers::sub,
        OpCode::Mul => helpers::mul,
        OpCode::Div => helpers::div,
        OpCode::Mod => helpers::modulo,
        OpCode::Pow => helpers::pow,
        OpCode::Negate => helpers::negate,
        OpCode::NumLt => helpers::num_lt,
        OpCode::NumLe => helpers::num_le,
        OpCode::NumGt => helpers::num_gt,
        OpCode::NumGe => helpers::num_ge,
        OpCode::NumEq => helpers::num_eq,
        OpCode::NumNe => helpers::num_ne,
        OpCode::Concat => helpers::concat,
        OpCode::StrEq => helpers::str_eq,
        OpCode::StrNe => helpers::str_ne,
        OpCode::Return => helpers::ret,
        _ => return None,
    };
    Some(f as *const () as usize)
}

/// Straight-line opcodes without a dedicated shim, executed through the
/// generic `helpers::step` (one interpreter dispatch per opcode). Every entry
/// is verified against its `exec_one` arm to unconditionally leave
/// `ip == start + 1` on Ok — no jumps, no compound-loop bodies, no arms that
/// consult or rewrite `ip` beyond the increment. Anything not provably
/// straight-line stays OFF this list and bails the chunk out.
fn step_supported(op: &OpCode) -> bool {
    matches!(
        op,
        // Constants / stack shape
        OpCode::LoadNil
            | OpCode::LoadTrue
            | OpCode::LoadFalse
            | OpCode::Dup
            | OpCode::Pop
            // Variable reads
            | OpCode::GetGlobal(_)
            | OpCode::GetOurVar(_)
            | OpCode::GetArrayVar(_)
            | OpCode::GetHashVar(_)
            | OpCode::GetBareWord(_)
            | OpCode::GetCaptureVar(_)
            | OpCode::GetCodeVar(_)
            | OpCode::GetSelfOrNoSelf(_)
            | OpCode::GetUpvalue { .. }
            // Variable writes / declarations
            | OpCode::SetGlobal(_)
            | OpCode::SetGlobalRaw(_)
            | OpCode::SetVarDynamic { .. }
            | OpCode::SetVarType { .. }
            | OpCode::AssignExpr(_)
            | OpCode::TopicDotAssign(_)
            | OpCode::AtomicCompoundVar { .. }
            | OpCode::IndexAssignExprNamed { .. }
            | OpCode::WrapVarRef(_)
            | OpCode::LetSave { .. }
            | OpCode::CheckReadOnly(_)
            | OpCode::MarkVarReadonly(_)
            | OpCode::CheckDynamicVarDeclared(_)
            // Increment / decrement
            | OpCode::PostIncrement(..)
            | OpCode::PostDecrement(..)
            | OpCode::PreIncrement(..)
            | OpCode::PreDecrement(..)
            | OpCode::PreIncrementIndex(_)
            | OpCode::PreDecrementIndex(_)
            // Arith predicates
            | OpCode::DivisibleBy
            | OpCode::NotDivisibleBy
            // Closure construction
            | OpCode::MakeLambda(..)
            | OpCode::MakeAnonSub(..)
            | OpCode::MakeAnonSubParams(..)
            | OpCode::MakeGather(..)
            // Calls through a code variable (re-entrant, like CallMethod)
            | OpCode::CallOnCodeVar { .. }
            | OpCode::ExecCallPairs { .. }
            // In-place container mutation
            | OpCode::ArrayPush { .. }
            | OpCode::TagContainerRef(..)
            | OpCode::TagContainerRefReversed(..)
            | OpCode::MarkAccessorRefContext
            // List / hash construction, indexing and coercion
            | OpCode::MakeArray(_)
            | OpCode::MakeRealArray(_)
            | OpCode::MakeRealArrayNoFlatten(_)
            | OpCode::MakeHash(_)
            | OpCode::MakePair
            | OpCode::CoerceToList
            | OpCode::Itemize
            | OpCode::DeitemizeZen
            | OpCode::Decont
            | OpCode::Index { .. }
            | OpCode::IndexAutovivifyLazy
            // String / bool / numeric helpers
            | OpCode::StringConcat(_)
            | OpCode::StrCoerce
            | OpCode::BoolCoerce
            | OpCode::Not
            | OpCode::Gcd
            | OpCode::Lcm
            | OpCode::IntDiv
            | OpCode::IntMod
            | OpCode::NumCoerce
            // Sink context (forces lazies / throws unhandled Failures)
            | OpCode::SinkPop(_)
            // Topic / context markers
            | OpCode::MarkBindContext
            | OpCode::MarkVarDeclContext
            | OpCode::MarkExplicitInitializerContext
            | OpCode::MarkShapedDeclContext
            | OpCode::SetTopic
            | OpCode::SaveTopic
            | OpCode::RestoreTopic
            | OpCode::PushEnterResult
            | OpCode::LoadEnterResult
            // Always-throwing terminator (records its own resume point)
            | OpCode::Die
    )
}

/// Compile `code` to a native Tier A body. `None` = bailout (unsupported
/// opcode, or an internal Cranelift failure): the caller marks the chunk so
/// it is never scanned again.
pub(super) fn compile_chunk(code: &CompiledCode) -> Option<JitEntryFn> {
    // Static support scan + jump-target collection. Any opcode outside the
    // Tier A set rejects the whole chunk (no partial compilation).
    let mut targets: std::collections::HashSet<usize> = std::collections::HashSet::new();
    for op in code.ops.iter() {
        if noarg_shim(op).is_some() || step_supported(op) {
            continue;
        }
        match op {
            OpCode::LoadConst(_)
            | OpCode::GetLocal(_)
            | OpCode::SetLocal(_)
            | OpCode::ContainerizePair
            | OpCode::SetSourceLine(_)
            | OpCode::CallFunc { .. }
            | OpCode::CallMethod { .. }
            | OpCode::CallMethodMut { .. } => {}
            OpCode::Jump(t)
            | OpCode::JumpIfFalse(t)
            | OpCode::JumpIfTrue(t)
            | OpCode::JumpIfNotNil(t) => {
                targets.insert(*t as usize);
            }
            other => {
                crate::vm::vm_stats::record_jit_bailout(other);
                return None;
            }
        }
    }
    build(code, &targets)
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
        s_call: sig(&[ptr, ptr, types::I32, ptr], Some(types::I32)),
    }
}

fn build(code: &CompiledCode, targets: &std::collections::HashSet<usize>) -> Option<JitEntryFn> {
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
    for (i, op) in code.ops.iter().enumerate() {
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
                let idxv = b.ins().iconst(types::I32, *idx as i64);
                call_helper(
                    &mut b,
                    sigs.v_code_u32,
                    helpers::load_const as *const () as usize,
                    &[interp, codep, idxv],
                );
            }
            OpCode::SetSourceLine(line) => {
                let linev = b.ins().iconst(types::I64, *line);
                call_helper(
                    &mut b,
                    sigs.v_i64,
                    helpers::set_source_line as *const () as usize,
                    &[interp, linev],
                );
            }
            OpCode::ContainerizePair => {
                call_helper(
                    &mut b,
                    sigs.v1,
                    helpers::containerize_pair as *const () as usize,
                    &[interp],
                );
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
                let status =
                    call_helper(&mut b, sigs.s_code_u32, f, &[interp, codep, opidx])?;
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
    // A jump target one past the last opcode is a normal fall-through exit.
    if let Some(&blk) = block_at.get(&code.ops.len()) {
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
