//! Entering and leaving a TRIR routine from the ordinary VM.
//!
//! Two doors. [`Interpreter::exec_call_trir_site`] is ADR-0110 §3.3's static
//! call linkage: the call site resolved the callee at compile time and the
//! arguments are read straight out of the caller's frame slots, so there is
//! no name, no dispatch key, no binder and no pushed argument. The other,
//! [`Interpreter::try_call_trir`], is §4's generic prologue for a `CallFunc`
//! that reaches a TRIR routine by name with its arguments on the VM stack.
//!
//! Declining is free everywhere: every `None` below leaves the VM state
//! exactly as it was, and the caller takes its ordinary path.

use super::compile::TrirCompiler;
use super::exec::TrOutcome;
use super::frame::TrFrame;
use super::{TrChunk, TrKind};
use crate::opcode::{CompiledCode, CompiledFns, CompiledFunction};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// Where a native `is rw` parameter's result goes when the routine returns.
#[derive(Clone, Copy)]
enum RwTarget {
    /// A caller frame slot holding the value directly.
    Slot(u32),
    /// A caller frame slot holding a shared container cell; the write goes
    /// through the cell, so no env mirror is needed.
    Cell(u32),
}

/// One bound `is rw` parameter at the OUTER boundary: the spill slot holding
/// its value for the duration of the call, and where it goes back.
type RwPlan = ([(u16, RwTarget); 4], usize);

impl Interpreter {
    /// Execute a compile-time-resolved TRIR call site (ADR-0110 §3.3).
    ///
    /// `None` means the site could not be served — the callee has been
    /// replaced or wrapped since it was resolved, or an argument has a shape
    /// the chunk's proof does not cover. The caller then takes the cold
    /// by-name fallback, which reproduces the untyped call site exactly.
    pub(crate) fn exec_call_trir_site(
        &mut self,
        site: &crate::trir::TrCallSite,
        compiled_fns: &CompiledFns,
        caller_code: &CompiledCode,
    ) -> Option<Result<Value, RuntimeError>> {
        // ADR-0110 §3.3's run-time guard. A `.wrap`ped routine must reach its
        // wrapper, which a statically linked call site would step straight
        // past; the emptiness test costs nothing in a program that has never
        // wrapped anything.
        if self.any_routine_wrapped() && self.routine_is_wrapped(&site.name.resolve()) {
            return None;
        }
        let cf = compiled_fns
            .get(&site.key)
            .filter(|cf| cf.fingerprint == site.fingerprint)?;
        let chunk = cf.trir.as_ref()?.clone();
        if chunk.params.len() != site.arg_slots.len() {
            return None;
        }
        self.run_trir_from_outside(&chunk, compiled_fns, |me, frame| {
            me.trir_bind_from_slots(&chunk, frame, site, caller_code)
        })
    }

    /// Run `cf`'s chunk with the arguments at `stack[args_base..]`.
    ///
    /// On `Some`, the arguments have been consumed (the stack is truncated
    /// back to `args_base`), exactly as the light call path consumes them.
    pub(crate) fn try_call_trir(
        &mut self,
        cf: &CompiledFunction,
        args_base: usize,
        caller_code: Option<&CompiledCode>,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        if self.any_routine_wrapped() {
            return None;
        }
        let chunk = cf.trir.as_ref()?.clone();
        if self.stack.len() - args_base != chunk.params.len() {
            return None;
        }
        let out = self.run_trir_from_outside(&chunk, compiled_fns, |me, frame| {
            me.trir_bind_from_stack(&chunk, frame, args_base, caller_code)
        })?;
        self.stack.truncate(args_base);
        Some(out)
    }

    /// Open a frame, let `bind` fill it, run the chunk, write back the
    /// `is rw` parameters, and close the frame — on every exit path.
    ///
    /// Shared by both doors so the frame bookkeeping exists once.
    fn run_trir_from_outside(
        &mut self,
        chunk: &TrChunk,
        compiled_fns: &CompiledFns,
        bind: impl FnOnce(&mut Self, TrFrame) -> Option<RwPlan>,
    ) -> Option<Result<Value, RuntimeError>> {
        // The frame carries one SPILL slot per `is rw` parameter above its own
        // native slots: the caller's variable lives in a `Value`, which has no
        // index a reference could name, so its value is copied into a spill,
        // the parameter is pointed at the spill, and the spill is copied back
        // below. That is copy-in/copy-out again — but expressed as a reference,
        // so a callee that passes the parameter on writes the same place.
        let spills = chunk.params.iter().filter(|p| p.is_rw).count() as u16;
        let frame = self.trir.push_frame(chunk.n_native + spills, chunk.n_obj);
        let Some((rw, rw_len)) = bind(self, frame) else {
            self.trir.pop_frame(frame);
            return None;
        };
        if !self.trir_seed_outers(chunk, frame) {
            self.trir.pop_frame(frame);
            return None;
        }
        let outcome = self.run_trir_chunk(chunk, frame, compiled_fns);
        let result = match outcome {
            Ok(TrOutcome::Value(v)) => v,
            Ok(TrOutcome::Bail) => {
                // A checked op met a shape the compiler's proof did not cover.
                // The compiler only admits a bail-capable op where re-running
                // the routine from the beginning is equivalent to never having
                // started it, so the untyped path can take the call whole.
                self.trir.pop_frame(frame);
                return None;
            }
            Err(e) => {
                self.trir.pop_frame(frame);
                return Some(Err(e));
            }
        };
        // Read the spills back BEFORE the frame is closed.
        let mut writes: [(i64, RwTarget); 4] = [(0, RwTarget::Slot(0)); 4];
        for i in 0..rw_len {
            let (spill, target) = rw[i];
            writes[i] = (self.trir.nl[frame.nbase as usize + spill as usize], target);
        }
        self.trir.pop_frame(frame);
        for &(raw, target) in &writes[..rw_len] {
            let v = Value::int(raw);
            match target {
                RwTarget::Cell(slot) => {
                    if let ValueView::ContainerRef(cell) = self.locals[slot as usize].view() {
                        *cell.lock().unwrap_or_else(|e| e.into_inner()) = v;
                    } else {
                        // The bind pass proved this slot held a cell — an
                        // internal invariant, so report rather than panic
                        // (#8186).
                        return Some(Err(RuntimeError::new(
                            "internal error: a TRIR `is rw` slot lost its container".to_string(),
                        )));
                    }
                }
                RwTarget::Slot(slot) => self.locals[slot as usize] = v,
            }
        }
        Some(Ok(result))
    }

    /// Bind the parameters of a statically resolved call site from the
    /// caller's own frame slots.
    fn trir_bind_from_slots(
        &mut self,
        chunk: &TrChunk,
        frame: TrFrame,
        site: &crate::trir::TrCallSite,
        caller_code: &CompiledCode,
    ) -> Option<RwPlan> {
        let mut rw = [(0u16, RwTarget::Slot(0)); 4];
        let mut rw_len = 0usize;
        let mut next_spill = chunk.n_native;
        for (i, p) in chunk.params.iter().enumerate() {
            let caller_slot = site.arg_slots[i];
            if p.is_rw {
                if rw_len == rw.len() {
                    return None;
                }
                // Two `is rw` parameters bound to the SAME caller variable
                // (`f($p, $p)`) share one container on the untyped path, so a
                // write through one is visible to the other inside the body.
                // Two spills cannot reproduce that, so decline.
                if rw[..rw_len].iter().any(|(_, t)| {
                    matches!(t, RwTarget::Slot(s) | RwTarget::Cell(s) if *s == caller_slot)
                }) {
                    return None;
                }
                let (raw, target) = self.bind_rw_slot(caller_slot, caller_code)?;
                self.seed_rw_spill(frame, p.slot, next_spill, raw);
                rw[rw_len] = (next_spill, target);
                next_spill += 1;
                rw_len += 1;
                continue;
            }
            let idx = caller_slot as usize;
            if idx >= self.locals.len() {
                return None;
            }
            if let Some(cell) = self.trir_captured_cell(caller_code, idx) {
                self.locals[idx] = cell;
            }
            let val = deref_cell(&self.locals[idx]);
            self.bind_ro_param(frame, p, &val)?;
        }
        Some((rw, rw_len))
    }

    /// Bind the parameters of a by-name call from the VM stack.
    fn trir_bind_from_stack(
        &mut self,
        chunk: &TrChunk,
        frame: TrFrame,
        args_base: usize,
        caller_code: Option<&CompiledCode>,
    ) -> Option<RwPlan> {
        let mut rw = [(0u16, RwTarget::Slot(0)); 4];
        let mut rw_len = 0usize;
        let mut next_spill = chunk.n_native;
        for (i, p) in chunk.params.iter().enumerate() {
            if p.is_rw {
                if rw_len == rw.len() {
                    return None;
                }
                // The argument is a `WrapVarRef`-tagged caller lexical; the
                // slot it names is the one to write back. A `u32::MAX` slot
                // is the compiler's "known not a local of this frame"
                // sentinel, not an index.
                let arg = &self.stack[args_base + i];
                if !matches!(arg.view(), ValueView::VarRef { .. }) {
                    return None;
                }
                let caller_slot = arg.varref_slot().filter(|s| *s != u32::MAX)?;
                if rw[..rw_len].iter().any(|(_, t)| {
                    matches!(t, RwTarget::Slot(s) | RwTarget::Cell(s) if *s == caller_slot)
                }) {
                    return None;
                }
                let (raw, target) = self.bind_rw_slot(caller_slot, caller_code?)?;
                self.seed_rw_spill(frame, p.slot, next_spill, raw);
                rw[rw_len] = (next_spill, target);
                next_spill += 1;
                rw_len += 1;
                continue;
            }
            let val = self.stack[args_base + i].unwrap_varref().clone();
            self.bind_ro_param(frame, p, &val)?;
        }
        Some((rw, rw_len))
    }

    /// Put `raw` in the frame's spill slot and point the parameter at it.
    fn seed_rw_spill(&mut self, frame: TrFrame, param_slot: u16, spill: u16, raw: i64) {
        let nbase = frame.nbase as usize;
        self.trir.nl[nbase + spill as usize] = raw;
        self.trir.nl[nbase + param_slot as usize] = (nbase + spill as usize) as i64;
    }

    /// Bind one read-only parameter, mirroring the general binder's
    /// admissions: an `Int` or a `Bool` (which does `Int`) for a native
    /// `int`, an `Int`/`Num` for a native `num`, an actual string for a
    /// native `str`. A bare type object, a `BigInt` outside `int`'s range and
    /// everything else decline, so the untyped path raises the error the
    /// program should see.
    fn bind_ro_param(
        &mut self,
        frame: TrFrame,
        p: &crate::trir::TrParam,
        val: &Value,
    ) -> Option<()> {
        match p.kind {
            TrKind::Int => {
                let n = match val.view() {
                    ValueView::Int(i) => i,
                    ValueView::Bool(b) => b as i64,
                    _ => return None,
                };
                self.trir.nl[frame.nbase as usize + p.slot as usize] = n;
            }
            TrKind::Num => {
                let n = match val.view() {
                    ValueView::Num(n) => n,
                    ValueView::Int(i) => i as f64,
                    _ => return None,
                };
                self.trir.nl[frame.nbase as usize + p.slot as usize] = n.to_bits() as i64;
            }
            TrKind::Obj => {
                if p.type_name == "str" && val.as_str().is_none() {
                    return None;
                }
                self.trir.ol[frame.obase as usize + p.slot as usize] = val.clone();
            }
        }
        Some(())
    }

    /// Read a native `is rw` parameter out of the caller's slot, and settle
    /// where its result goes back.
    ///
    /// A slot the caller also mirrors by name (`needs_env_sync`), or that an
    /// inner closure captured, is resolved or promoted to a shared
    /// `ContainerRef` cell — the same promotion the untyped `is rw` path
    /// performs — after which both halves are the same container and the
    /// writeback is one store through it.
    fn bind_rw_slot(&mut self, slot: u32, caller_code: &CompiledCode) -> Option<(i64, RwTarget)> {
        let idx = slot as usize;
        if idx >= caller_code.locals.len() || idx >= self.locals.len() {
            return None;
        }
        // A variable an inner closure captured has its authoritative
        // container in the ENV, not in the frame slot — `WrapVarRef` resolves
        // exactly this before handing a caller lexical to an `is rw`
        // parameter.
        if let Some(cell) = self.trir_captured_cell(caller_code, idx) {
            self.locals[idx] = cell;
        }
        if let ValueView::ContainerRef(cell) = self.locals[idx].view() {
            let inner = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
            let raw = Self::trir_rw_int(&inner)?;
            return Some((raw, RwTarget::Cell(slot)));
        }
        let raw = Self::trir_rw_int(&self.locals[idx])?;
        // ALWAYS promote, exactly as the untyped `is rw` path does. Writing
        // the slot in place instead looks equivalent and is not: a closure
        // that captured the same variable, or an env mirror the caller keeps,
        // holds a container this write would never reach. The promotion
        // happens once per variable; every later call takes the cell branch.
        let name = caller_code.locals[idx].clone();
        let inner = self.locals[idx].clone();
        let cell = self.capture_var_cell_boxing_type_objects(caller_code, &name, inner, Some(slot));
        if !cell.is_container_ref() {
            return None;
        }
        self.locals[idx] = cell;
        Some((raw, RwTarget::Cell(slot)))
    }

    /// The shared container an inner closure captured this caller local
    /// into, when the slot itself does not already hold it.
    fn trir_captured_cell(&self, caller_code: &CompiledCode, idx: usize) -> Option<Value> {
        if self.locals[idx].is_container_ref() || caller_code.container_ref_capture_syms.is_empty()
        {
            return None;
        }
        let sym = *caller_code.locals_sym.get(idx)?;
        if !caller_code.container_ref_capture_syms.contains(&sym) {
            return None;
        }
        self.env()
            .get_sym(sym)
            .filter(|v| v.is_container_ref())
            .cloned()
    }

    /// The raw `i64` a native `is rw` parameter binds from.
    fn trir_rw_int(v: &Value) -> Option<i64> {
        match v.view() {
            ValueView::Int(i) => Some(i),
            ValueView::Bool(b) => Some(b as i64),
            _ => None,
        }
    }

    /// The cold path of a `CallTrir` site: rebuild the argument shape the
    /// untyped call site would have produced (every plain lexical wrapped in
    /// a slotted `VarRef`, which is what `WrapVarRef` does) and dispatch by
    /// name.
    pub(crate) fn exec_call_trir_fallback(
        &mut self,
        site: &crate::trir::TrCallSite,
        caller_code: &CompiledCode,
    ) -> Result<Value, RuntimeError> {
        let mut args = Vec::with_capacity(site.arg_slots.len());
        for &slot in &site.arg_slots {
            let value = self
                .locals
                .get(slot as usize)
                .cloned()
                .unwrap_or(Value::NIL);
            let sym = caller_code
                .locals_sym
                .get(slot as usize)
                .copied()
                .unwrap_or_else(|| Symbol::intern(""));
            args.push(Value::varref_slotted(sym, value, None, Some(slot)));
        }
        let name = site.name.resolve();
        // Mirror `exec_call_func_op`'s save/restore. A statically linked
        // site's arguments are all plain lexicals, never literals, so the
        // mask this call publishes for multi-candidate selection is empty —
        // but it must be published, or the callee's dispatch would read the
        // CALLER's.
        let saved = std::mem::replace(&mut self.literal_native_args, 0);
        let result = self.call_function(&name, args);
        self.literal_native_args = saved;
        result
    }
}

/// A binding's current value: the contents of a shared container cell, or
/// the value itself.
///
/// By reference rather than `Value::into_deref`, which takes the container by
/// value and so pays an atomic increment and a matching decrement just to
/// look inside it.
#[inline]
pub(super) fn deref_cell(v: &Value) -> Value {
    match v.view() {
        ValueView::ContainerRef(cell) => cell.lock().unwrap_or_else(|e| e.into_inner()).clone(),
        _ => v.clone(),
    }
}

/// Compile a routine to TRIR at declaration time, or answer `None`.
///
/// One call site (`compiler/helpers_sub_body.rs`), so the eligibility gate
/// and the chunk can never disagree about which routines have one.
pub(crate) fn compile_routine(
    name: Symbol,
    param_defs: &[crate::ast::ParamDef],
    params: &[String],
    return_type: Option<&str>,
    body: &[crate::ast::Stmt],
    routines: Option<&crate::trir::compile::TrirRoutineMap>,
    fns: Option<&CompiledFns>,
) -> Option<std::sync::Arc<TrChunk>> {
    let chunk = TrirCompiler::compile(name, param_defs, params, return_type, body, routines, fns);
    if dump_enabled() {
        match &chunk {
            Some(c) => eprintln!(
                "trir: {} accepted ({} ops, {} native slots, {} obj slots, {} outers, {} calls)",
                name.as_str(),
                c.ops.len(),
                c.n_native,
                c.n_obj,
                c.outers.len(),
                c.calls.len(),
            ),
            None => eprintln!("trir: {} declined", name.as_str()),
        }
    }
    chunk.map(std::sync::Arc::new)
}

/// Whether `MUTSU_TRIR_DUMP` asked for the eligibility decisions to be
/// reported. Read once: this runs per routine declaration.
fn dump_enabled() -> bool {
    use std::sync::OnceLock;
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_TRIR_DUMP").is_ok())
}
