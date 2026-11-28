//! Loop kernels: the entry point the loop drivers call (ADR-19288 D5).
//!
//! At an iteration boundary of a counted `for` over an int `Range` or of a
//! `while`, the driver asks `Interpreter::try_loop_kernel` to run the rest of
//! the loop natively. The kernel reads every slot the loop carries (each must
//! hold a plain `Int` or `Num`), runs whole iterations over machine words, and
//! writes the slots back through the ordinary store. If an iteration cannot
//! finish natively, the slots are written back as they were when that
//! iteration began and the driver continues generically from it, so nothing is
//! resumed mid-iteration and the rerun sees exactly the state the kernel saw.

use super::vm_loop_kernel_compile::{LOOP_POLL_EVERY, LoopCtx};
use super::vm_loop_kernel_types::{LoopShape, carried_slots, num_method_call};
use super::vm_loop_kernel_typing::type_region;
use super::*;
use crate::opcode::kernel_slot::{LoopKernelInfo, LoopSite, SlotKind};

/// Loop entries before a site that is not already known hot is compiled.
const LOOP_ENTRY_THRESHOLD: u32 = 16;

/// The most slots a loop kernel carries in and out.
const MAX_CARRIED: usize = 24;

/// The most declarations a loop kernel's body makes.
const MAX_DECLS: usize = 24;

/// What a loop kernel did.
pub(super) enum LoopOutcome {
    /// Nothing ran; the driver continues as if it had not asked.
    Declined,
    /// The loop finished: no iterations are left.
    Done,
    /// The kernel stopped at the start of an iteration it could not finish:
    /// the driver reruns that iteration generically. For a `for`, the
    /// counter value of that iteration.
    Bailed(i64),
}

static LOOP_KERNEL_COMPILES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static LOOP_KERNEL_ENTRIES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static LOOP_KERNEL_BAILS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// The `MUTSU_VM_STATS` line for loop kernels.
pub(crate) fn stats_line() -> String {
    use std::sync::atomic::Ordering::Relaxed;
    format!(
        "[mutsu vm-stats] loop-kernel: compiles={} entries={} bails={}",
        LOOP_KERNEL_COMPILES.load(Relaxed),
        LOOP_KERNEL_ENTRIES.load(Relaxed),
        LOOP_KERNEL_BAILS.load(Relaxed)
    )
}

impl Interpreter {
    /// Run the rest of the loop at `ip` (`shape`) natively; for a `for`,
    /// `range` is the counter value to start at, the end and its inclusivity.
    /// `hot` says the driver already knows the loop is long.
    // Cost: O(c) entry and exit work plus the native run, c = carried slots;
    // O(n) once per site, n = ops in the region.
    pub(super) fn try_loop_kernel(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        shape: LoopShape,
        range: Option<(i64, i64, bool)>,
        hot: bool,
        compiled_fns: &CompiledFns,
    ) -> Result<LoopOutcome, RuntimeError> {
        use std::sync::atomic::Ordering::Relaxed;
        if !super::vm_kernel::kernels_enabled()
            || super::vm_jit::USER_INFIX_DECLS.load(Relaxed) != 0
            || super::vm_jit::LOCAL_READ_SPOILERS.load(Relaxed) != 0
            // `state` variables are synced per iteration by the drivers.
            || !code.state_locals.is_empty()
        {
            return Ok(LoopOutcome::Declined);
        }
        // The site: its carried slots, decided once.
        let carried_slots_list = {
            let Ok(mut sites) = code.jit.loop_kernels.sites.lock() else {
                return Ok(LoopOutcome::Declined);
            };
            let k = match sites.iter().position(|s| s.ip == ip as u32) {
                Some(k) => k,
                None => {
                    sites.push(LoopSite {
                        ip: ip as u32,
                        entries: 0,
                        carried: carried_slots(code, &shape)
                            .filter(|c| c.len() <= MAX_CARRIED)
                            .map(Into::into),
                        variants: Vec::new(),
                    });
                    sites.len() - 1
                }
            };
            let site = &mut sites[k];
            let Some(carried) = site.carried.clone() else {
                return Ok(LoopOutcome::Declined);
            };
            site.entries = site.entries.saturating_add(1);
            if !hot && site.entries < LOOP_ENTRY_THRESHOLD && site.variants.is_empty() {
                return Ok(LoopOutcome::Declined);
            }
            carried
        };
        // The carried slots' current values, each a plain Int or Num. Fixed
        // arrays: a kernel entry allocates nothing.
        let n = carried_slots_list.len();
        let mut carried = [(0u32, SlotKind::Int); MAX_CARRIED];
        let mut words = [0u64; MAX_CARRIED];
        for (i, &slot) in carried_slots_list.iter().enumerate() {
            self.exec_get_local_op(code, slot)?;
            let Some(v) = self.stack.pop() else {
                return Ok(LoopOutcome::Declined);
            };
            (carried[i], words[i]) = match v.view() {
                ValueView::Int(x) => ((slot, SlotKind::Int), x as u64),
                ValueView::Num(f) => ((slot, SlotKind::Num), f.to_bits()),
                _ => return Ok(LoopOutcome::Declined),
            };
        }
        let carried = &carried[..n];
        let Some(info) = self.loop_kernel_variant(code, ip, &shape, carried) else {
            return Ok(LoopOutcome::Declined);
        };
        // A readonly check that fails raises on the generic path, at its op.
        for &(modify, name_idx) in info.readonly_checks.iter() {
            let r = if modify {
                self.exec_check_read_only_modify_op(code, name_idx)
            } else {
                self.exec_check_read_only_op(code, name_idx)
            };
            if r.is_err() {
                return Ok(LoopOutcome::Declined);
            }
        }
        // Each `.Num` site must reach the built-in method: the method lane
        // answers it (no augment, wrap or user `find_method`), with a Num.
        for &(site_ip, kind) in info.num_sites.iter() {
            if !self.num_site_is_builtin(code, site_ip as usize, kind) {
                return Ok(LoopOutcome::Declined);
            }
        }
        // Each sub call must reach a sub kernel, as the generic call would.
        let mut callees = [0usize; super::vm_loop_kernel_compile::MAX_CALLS];
        for (k, &(call_ip, arity)) in info.calls.iter().enumerate() {
            let OpCode::CallFunc {
                name_idx,
                arg_sources_idx,
                ..
            } = code.ops[call_ip as usize]
            else {
                return Ok(LoopOutcome::Declined);
            };
            let Some((cf, name_sym, pkg_sym)) =
                self.light_cache_callee(code, name_idx, arg_sources_idx, compiled_fns)
            else {
                return Ok(LoopOutcome::Declined);
            };
            let Some(kernel) = self.kernel_for_call(cf, name_sym, pkg_sym, arity as usize, true)
            else {
                return Ok(LoopOutcome::Declined);
            };
            callees[k] = kernel.entry;
        }
        let mut buf = [0u64; 2 + MAX_CARRIED + MAX_DECLS];
        let (start, end, inclusive) = range.unwrap_or((0, 0, false));
        buf[0] = start as u64;
        buf[2..2 + n].copy_from_slice(&words[..n]);
        let mut ctx = LoopCtx {
            poll: LOOP_POLL_EVERY,
            inclusive: inclusive as u32,
            end,
            kctx: super::vm_kernel::fresh_kernel_ctx(),
            callees,
        };
        LOOP_KERNEL_ENTRIES.fetch_add(1, Relaxed);
        // SAFETY: `info.entry` is a finalized loop kernel compiled for this
        // region with exactly these carried slots (`vm_loop_kernel_compile`),
        // whose signature is `extern "C" fn(*mut LoopCtx, *mut u64) -> u32`;
        // `buf` holds the `2 + carried + decls` words it reads and writes, and
        // both pointers outlive the call. The JIT module lives for the process.
        let status = unsafe {
            let f = std::mem::transmute::<usize, extern "C" fn(*mut LoopCtx, *mut u64) -> u32>(
                info.entry,
            );
            f(&mut ctx, buf.as_mut_ptr())
        };
        let iterations = buf[1];
        // The carried slots back, through the ordinary store, where they moved.
        for (i, &(slot, kind)) in info.carried.iter().enumerate() {
            let w = buf[2 + i];
            if w == words[i] {
                continue;
            }
            self.stack.push(match kind {
                SlotKind::Int => Value::int(w as i64),
                SlotKind::Num => Value::num(f64::from_bits(w)),
            });
            self.exec_set_local_op(code, slot)?;
            self.publish_state_local(code, slot);
        }
        if status != 0 {
            LOOP_KERNEL_BAILS.fetch_add(1, Relaxed);
            return Ok(LoopOutcome::Bailed(buf[0] as i64));
        }
        if iterations > 0 {
            // The body's declarations as the last iteration left them.
            let base = 2 + info.carried.len();
            for (j, &(decl_ip, slot, kind)) in info.decls.iter().enumerate() {
                let w = buf[base + j];
                self.exec_set_var_dynamic_op(code, &code.ops[decl_ip as usize]);
                self.stack.push(match kind {
                    SlotKind::Int => Value::int(w as i64),
                    SlotKind::Num => Value::num(f64::from_bits(w)),
                });
                self.explicit_initializer_context().set(true);
                self.vardecl_context().set(true);
                self.decl_typechecked_context().set(false);
                self.exec_set_local_op(code, slot)?;
                self.publish_state_local(code, slot);
            }
            loan_env!(self, run_pending_instance_destroys())?;
        }
        Ok(LoopOutcome::Done)
    }

    /// The compiled variant of the site at `ip` for these carried kinds,
    /// compiling it on first use. `None` when the region declines.
    fn loop_kernel_variant(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        shape: &LoopShape,
        carried: &[(u32, SlotKind)],
    ) -> Option<std::sync::Arc<LoopKernelInfo>> {
        let mut sites = code.jit.loop_kernels.sites.lock().ok()?;
        let site = sites.iter_mut().find(|s| s.ip == ip as u32)?;
        if let Some((_, v)) = site
            .variants
            .iter()
            .find(|(k, _)| k.iter().copied().eq(carried.iter().map(|c| c.1)))
        {
            return v.clone();
        }
        let kinds: Box<[SlotKind]> = carried.iter().map(|c| c.1).collect();
        // A few kind signatures per site at most; past that, stop compiling.
        if site.variants.len() >= 4 {
            return None;
        }
        let compiled = type_region(code, shape, carried)
            .filter(|typed| typed.decls.len() <= MAX_DECLS)
            .and_then(|typed| {
                let entry = super::vm_loop_kernel_compile::compile(code, shape, carried, &typed)?;
                Some(std::sync::Arc::new(LoopKernelInfo {
                    entry,
                    carried: carried.to_vec().into_boxed_slice(),
                    decls: typed
                        .decls
                        .iter()
                        .map(|&(dip, slot)| (dip, slot, typed.slot_kind[&slot]))
                        .collect(),
                    readonly_checks: typed.readonly_checks.clone().into_boxed_slice(),
                    num_sites: typed.num_sites.clone().into_boxed_slice(),
                    calls: typed.calls.clone().into_boxed_slice(),
                }))
            });
        if compiled.is_some() {
            LOOP_KERNEL_COMPILES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        site.variants.push((kinds, compiled.clone()));
        compiled
    }

    /// Whether the `.Num` call at `ip` on a receiver of `kind` is answered by
    /// the built-in method: the method-site lane (which refuses an augmented,
    /// wrapped or `find_method`-intercepted call) answers a sample receiver
    /// with the Num the kernel would compute.
    // Cost: O(1), one lane probe.
    fn num_site_is_builtin(&mut self, code: &CompiledCode, ip: usize, kind: SlotKind) -> bool {
        let Some((name_idx, target)) = num_method_call(code, &code.ops[ip]) else {
            return false;
        };
        let (sample, expect) = match kind {
            SlotKind::Int => (Value::int(7), 7.0),
            SlotKind::Num => (Value::num(7.5), 7.5),
        };
        self.stack.push(sample);
        let answer = self.site_lane_answer(code, ip, name_idx, 0, target);
        self.stack.pop();
        matches!(answer.map(|a| a.result), Some(Ok(v)) if matches!(v.view(), ValueView::Num(f) if f == expect))
    }
}
