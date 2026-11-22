//! Running an inlined callee's region (ADR-8673 D2; the splice is
//! `compile/inline_call.rs`).
//!
//! [`TrOp::InlineEnter`] does at run time what a `CallTr` did around its
//! callee's body, minus the frame: it checks the link, records the callee's
//! deferred routine-stack entry, switches to the callee's package and seeds
//! its free variables after the caller's. [`TrOp::InlineReturn`] undoes it
//! and leaves the result where the call left it. The active regions sit on
//! [`InlineRegions`], so an error or a bail out of the middle of one undoes
//! it too (`exec.rs`), and a `fail` raised inside becomes the callee's
//! `Failure` value, as the callee's routine boundary made it.

use super::exec::TrOutcome;
use super::frame::TrFrame;
use super::{TrChunk, TrOp};
use crate::opcode::CompiledFns;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// One inlined callee running now.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ActiveInline {
    pub(crate) region: u32,
    pub(crate) join: u32,
    /// Operand-bank heights at entry, restored on the way out.
    pub(crate) ns_mark: u32,
    pub(crate) os_mark: u32,
    /// Where the callee's free variables start.
    pub(crate) outer_base: u32,
    /// The package to switch back to, when the callee's differs.
    pub(crate) saved_pkg: Option<Symbol>,
}

/// The inlined callees running now, innermost last. A TRIR frame's own
/// regions sit above whatever its callers left, and never nest.
pub(crate) type InlineRegions = Vec<ActiveInline>;

impl Interpreter {
    /// [`Interpreter::run_trir_chunk`] for a body with inlined callees.
    // Cost: O(1) per op executed, plus O(r) per region left by an error, r =
    // active regions.
    pub(super) fn run_trir_chunk_with_regions(
        &mut self,
        chunk: &TrChunk,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<TrOutcome, RuntimeError> {
        // A body with inlined callees (ADR-8673 D2): leaving it any way but
        // through a region's own return undoes the regions still active, and
        // a `fail` raised inside one is that callee's `Failure` value, as its
        // routine boundary made it (`run_trir_inner_routine`).
        let base = self.trir.inline.len();
        let mut ip = 0usize;
        loop {
            let outcome = self.run_trir_ops(chunk, frame, compiled_fns, ip);
            match outcome {
                Err(e) if e.is_fail() && self.trir.inline.len() > base => {
                    let Some(a) = self.trir_inline_leave() else {
                        return Err(e);
                    };
                    let callee = chunk
                        .inline_regions
                        .get(a.region as usize)
                        .map(|r| r.link.chunk.name)
                        .unwrap_or(chunk.name);
                    let callable_id = self.registration_clone_id(
                        &self.current_package_sym().resolve(),
                        &callee.resolve(),
                    );
                    self.trir_inline_restore_pkg(&a);
                    if e.fail_passes_frame(callable_id) {
                        self.trir_inline_unwind(base);
                        return Err(e);
                    }
                    let v = self.fail_error_to_failure_value(&e);
                    self.trir.ns.truncate(a.ns_mark as usize);
                    self.trir.os.truncate(a.os_mark as usize);
                    self.trir.os.push(v);
                    if !self.trir_reseed_outers(chunk, frame) {
                        self.trir_inline_unwind(base);
                        return Ok(TrOutcome::Bail);
                    }
                    ip = a.join as usize;
                }
                other => {
                    self.trir_inline_unwind(base);
                    return other;
                }
            }
        }
    }

    /// One of the three region ops: `None` bails, `Some(Some(ip))` jumps to
    /// the region's join.
    // Cost: O(1) plus the outer seeding, O(o), o = the callee's free variables.
    pub(super) fn exec_trir_inline_op(
        &mut self,
        chunk: &TrChunk,
        op: &TrOp,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Option<Option<usize>> {
        let ok = match op {
            TrOp::InlineEnter { region, join } => {
                self.trir_inline_enter(chunk, *region, *join, frame, compiled_fns)
            }
            TrOp::InlineReturn { join, .. } => {
                if !self.trir_inline_return(chunk, frame) {
                    return None;
                }
                return Some(Some(*join as usize));
            }
            TrOp::InlineReseed(_) => self.trir_inline_reseed(chunk, frame),
            _ => false,
        };
        ok.then_some(None)
    }

    /// `InlineEnter`: `false` when the callee cannot be served (wrapped,
    /// replaced, or its free variables cannot be bound), which bails the
    /// chunk as the `CallTr` would have.
    // Cost: O(f), f = the callee's free variables (seeded once per entry).
    pub(super) fn trir_inline_enter(
        &mut self,
        chunk: &TrChunk,
        region: u32,
        join: u32,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> bool {
        let Some(r) = chunk.inline_regions.get(region as usize) else {
            return false;
        };
        if self.any_routine_wrapped() && self.routine_is_wrapped(&r.name.resolve()) {
            return false;
        }
        if !r.link.current_in(compiled_fns) {
            return false;
        }
        let current = self.current_package_sym();
        let saved_pkg = r.link.pkg.filter(|p| *p != current).map(|p| {
            self.set_current_package_sym(p);
            current
        });
        let outer_base = self.trir.outers.len() as u32;
        debug_assert_eq!(
            outer_base as usize,
            frame.outer_base as usize + chunk.outers.len(),
            "an inlined callee's free variables follow its caller's"
        );
        let seeding = TrFrame {
            outer_base,
            ..frame
        };
        if !self.trir_seed_outers(&r.link.chunk, seeding) {
            if let Some(p) = saved_pkg {
                self.set_current_package_sym(p);
            }
            return false;
        }
        self.trir.inline.push(ActiveInline {
            region,
            join,
            ns_mark: self.trir.ns.len() as u32,
            os_mark: self.trir.os.len() as u32,
            outer_base,
            saved_pkg,
        });
        self.trir_defer_routine(&r.link.chunk);
        if !super::lazy_frames::lazy_frames_enabled() {
            self.trir_observe();
        }
        super::stats::record_inline_entry();
        true
    }

    /// Leave the innermost region: its routine-stack entry and free
    /// variables go. The package is switched back by the caller
    /// ([`Self::trir_inline_restore_pkg`]), after anything that has to run
    /// in the callee's package.
    // Cost: O(f), f = the callee's free variables (dropped).
    pub(super) fn trir_inline_leave(&mut self) -> Option<ActiveInline> {
        let a = self.trir.inline.pop()?;
        self.trir.outers.truncate(a.outer_base as usize);
        self.trir_finish_deferred_routine();
        Some(a)
    }

    // Cost: O(1).
    pub(super) fn trir_inline_restore_pkg(&mut self, a: &ActiveInline) {
        if let Some(p) = a.saved_pkg {
            self.set_current_package_sym(p);
        }
    }

    /// `InlineReturn`: the callee's value is on the boxed bank. `false` when
    /// the caller's free variables cannot be re-read, which bails as the
    /// `CallTr` would.
    // Cost: O(f + g), f = the callee's free variables, g = the caller's.
    pub(super) fn trir_inline_return(&mut self, chunk: &TrChunk, frame: TrFrame) -> bool {
        let v = self.trir.os.pop().unwrap_or(Value::NIL);
        let Some(a) = self.trir_inline_leave() else {
            return false;
        };
        self.trir_inline_restore_pkg(&a);
        self.trir.ns.truncate(a.ns_mark as usize);
        self.trir.os.truncate(a.os_mark as usize);
        self.trir.os.push(v);
        // As after a `CallTr`: the callee may have written a free variable
        // the caller holds a copy of.
        self.trir_reseed_outers(chunk, frame)
    }

    /// `InlineReseed`: re-read the innermost callee's free variables.
    // Cost: O(f), f = the callee's free variables.
    pub(super) fn trir_inline_reseed(&mut self, chunk: &TrChunk, frame: TrFrame) -> bool {
        let Some(a) = self.trir.inline.last().copied() else {
            return false;
        };
        let Some(r) = chunk.inline_regions.get(a.region as usize) else {
            return false;
        };
        let callee = r.link.chunk.clone();
        let seeding = TrFrame {
            outer_base: a.outer_base,
            ..frame
        };
        self.trir_reseed_outers(&callee, seeding)
    }

    /// Undo every region above `base` (a bail or an error leaving the
    /// chunk).
    // Cost: O(r), r = regions undone.
    pub(super) fn trir_inline_unwind(&mut self, base: usize) {
        while self.trir.inline.len() > base {
            if let Some(a) = self.trir_inline_leave() {
                self.trir_inline_restore_pkg(&a);
            }
        }
    }
}
