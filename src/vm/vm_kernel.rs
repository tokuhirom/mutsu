//! Native int kernels: the entry point (ADR-19288 stage 1).
//!
//! A sub that `vm_kernel_compile::compile` admits runs as one native function
//! over machine integers. It is entered from the light call path's name-keyed
//! cache hit, the same door TRIR uses, under guards checked once per entry
//! (ADR-19288 D2): every argument a plain `Int`, no user infix operator in the
//! process, no wrapped routine, and every self-call in the body resolving to
//! this very routine. Nothing a kernel runs can change any of those, so the
//! calls a kernel makes to itself are not re-checked.
//!
//! When the kernel bails (overflow, zero divisor, depth) the entry discards it
//! and reports `None`: the caller then takes today's path with the arguments
//! still on the stack, and that path does what the kernel could not. A kernel
//! reads only its arguments and constants and writes only its own registers,
//! so the rerun sees exactly the state the kernel saw (ADR-19288 D4).

use super::*;

/// Consecutive bails after which a routine stops trying its kernel.
#[cfg(feature = "jit")]
const MAX_BAIL_STREAK: u32 = 8;

/// The depth bound when the native stack's headroom is unknown.
#[cfg(feature = "jit")]
const DEFAULT_DEPTH_LIMIT: u32 = 2_000;

/// The most native kernel frames one entry opens.
#[cfg(feature = "jit")]
const MAX_DEPTH_LIMIT: u32 = 100_000;

/// Native stack bytes budgeted per kernel frame when deriving the depth bound
/// (a kernel frame is a few dozen bytes; the rest is margin).
#[cfg(feature = "jit")]
const BYTES_PER_FRAME: usize = 256;

static KERNEL_COMPILES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static KERNEL_ENTRIES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static KERNEL_BAILS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// The `MUTSU_VM_STATS` line for kernels.
pub(crate) fn stats_line() -> String {
    use std::sync::atomic::Ordering::Relaxed;
    format!(
        "[mutsu vm-stats] kernel: compiles={} entries={} bails={}",
        KERNEL_COMPILES.load(Relaxed),
        KERNEL_ENTRIES.load(Relaxed),
        KERNEL_BAILS.load(Relaxed)
    )
}

/// Whether kernels may run at all: the JIT is on, `MUTSU_KERNEL` is not
/// `off`/`0`, and the profiler is not sampling (a kernel frame has no line).
#[cfg(feature = "jit")]
#[inline]
fn kernels_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    super::vm_jit::jit_enabled()
        && *ENABLED.get_or_init(|| {
            !matches!(std::env::var("MUTSU_KERNEL").ok().as_deref(), Some(v)
                if v == "0" || v.eq_ignore_ascii_case("off"))
        })
        && !crate::vm::vm_poll::profiler_armed()
}

impl Interpreter {
    /// Run `cf` as a native kernel with the `arity` arguments on top of the
    /// stack, if it has one and the entry guards hold. `Some(value)`: the
    /// arguments are popped and `value` is the call's result. `None`: nothing
    /// was changed, and the caller continues on its ordinary path.
    ///
    /// `name_sym` / `cur_pkg_sym` are the name and package this call site
    /// resolved `cf` under; a self-call in the body resolves in `cf`'s own
    /// package, so the kernel is entered only when the two agree.
    // Cost: O(a) guards plus the native run, a = arguments.
    #[cfg(feature = "jit")]
    pub(super) fn try_call_kernel(
        &mut self,
        cf: &CompiledFunction,
        name_sym: Symbol,
        cur_pkg_sym: Symbol,
        arity: usize,
    ) -> Option<Value> {
        use std::sync::atomic::Ordering::Relaxed;
        if !kernels_enabled() {
            return None;
        }
        let slot = &cf.code.jit.kernel;
        if slot.disabled.load(Relaxed) {
            return None;
        }
        let info = match slot.info.get() {
            Some(Some(info)) => *info,
            Some(None) => return None,
            None => {
                let calls = slot.calls.fetch_add(1, Relaxed) + 1;
                if calls < super::vm_jit::jit_threshold() {
                    return None;
                }
                let compiled = super::vm_kernel_compile::compile(cf);
                if compiled.is_some() {
                    KERNEL_COMPILES.fetch_add(1, Relaxed);
                }
                (*slot.info.get_or_init(|| compiled))?
            }
        };
        if info.arity as usize != arity || self.stack.len() < arity {
            return None;
        }
        if let Some(self_name) = info.self_name
            && (self_name != name_sym
                || cf.package_sym() != cur_pkg_sym
                || crate::parser::static_outer_latch::static_outer_observed()
                || self.env().frame_activation_binds_routine(&self_name.resolve()))
        {
            return None;
        }
        if super::vm_jit::USER_INFIX_DECLS.load(Relaxed) != 0 || self.any_routine_wrapped() {
            return None;
        }
        let base = self.stack.len() - arity;
        let mut a = [0i64; super::vm_kernel_compile::MAX_KERNEL_PARAMS];
        for (dst, v) in a.iter_mut().zip(&self.stack[base..]) {
            // A variable argument arrives wrapped for a possible `is rw` bind
            // (`WrapVarRef`); a kernel's parameters are readonly, so it binds
            // the value, as the light bind's `arg_binding_value` does.
            let v = v.unwrap_varref();
            *dst = match v.view() {
                ValueView::Int(n) => n,
                ValueView::ContainerRef(cell) => match cell.load().view() {
                    ValueView::Int(n) => n,
                    _ => return None,
                },
                _ => return None,
            };
        }
        let depth_limit = match crate::native_stack::headroom_bytes() {
            Some(bytes) => ((bytes / BYTES_PER_FRAME) as u32).min(MAX_DEPTH_LIMIT),
            None => DEFAULT_DEPTH_LIMIT,
        };
        let mut ctx = super::vm_kernel_compile::KernelCtx {
            bail: 0,
            depth: 0,
            poll: super::vm_kernel_compile::POLL_EVERY,
            depth_limit,
        };
        KERNEL_ENTRIES.fetch_add(1, Relaxed);
        let r = run_kernel(info, &mut ctx, &a);
        if ctx.bail != 0 {
            KERNEL_BAILS.fetch_add(1, Relaxed);
            if slot.bail_streak.fetch_add(1, Relaxed) + 1 >= MAX_BAIL_STREAK {
                slot.disabled.store(true, Relaxed);
            }
            return None;
        }
        slot.bail_streak.store(0, Relaxed);
        self.stack.truncate(base);
        Some(Value::int(r))
    }

    /// JIT-less builds have no kernels.
    #[cfg(not(feature = "jit"))]
    #[inline(always)]
    pub(super) fn try_call_kernel(
        &mut self,
        _cf: &CompiledFunction,
        _name_sym: Symbol,
        _cur_pkg_sym: Symbol,
        _arity: usize,
    ) -> Option<Value> {
        None
    }
}

/// Call the kernel's native entry with `info.arity` of the arguments in `a`.
#[cfg(feature = "jit")]
fn run_kernel(
    info: crate::opcode::kernel_slot::KernelInfo,
    ctx: &mut super::vm_kernel_compile::KernelCtx,
    a: &[i64; super::vm_kernel_compile::MAX_KERNEL_PARAMS],
) -> i64 {
    use super::vm_kernel_compile::KernelCtx as C;
    let p = ctx as *mut C;
    // SAFETY: `info.entry` is a finalized kernel compiled for exactly
    // `info.arity` i64 parameters after the context pointer, returning an i64
    // (`vm_kernel_compile::build`); the JIT module that owns the code lives for
    // the process. The kernel touches only `*ctx`, which outlives the call.
    unsafe {
        match info.arity {
            1 => std::mem::transmute::<usize, extern "C" fn(*mut C, i64) -> i64>(info.entry)(
                p, a[0],
            ),
            2 => std::mem::transmute::<usize, extern "C" fn(*mut C, i64, i64) -> i64>(
                info.entry,
            )(p, a[0], a[1]),
            3 => std::mem::transmute::<usize, extern "C" fn(*mut C, i64, i64, i64) -> i64>(
                info.entry,
            )(p, a[0], a[1], a[2]),
            4 => std::mem::transmute::<usize, extern "C" fn(*mut C, i64, i64, i64, i64) -> i64>(
                info.entry,
            )(p, a[0], a[1], a[2], a[3]),
            5 => std::mem::transmute::<
                usize,
                extern "C" fn(*mut C, i64, i64, i64, i64, i64) -> i64,
            >(info.entry)(p, a[0], a[1], a[2], a[3], a[4]),
            _ => std::mem::transmute::<
                usize,
                extern "C" fn(*mut C, i64, i64, i64, i64, i64, i64) -> i64,
            >(info.entry)(p, a[0], a[1], a[2], a[3], a[4], a[5]),
        }
    }
}
