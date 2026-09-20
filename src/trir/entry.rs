//! Entering and leaving a TRIR routine.
//!
//! Two doors. [`Interpreter::exec_call_trir_site`] is ADR-0110 §3.3's static
//! call linkage: the call site resolved the callee at compile time and the
//! arguments are read straight out of the caller's frame slots, so there is
//! no name, no dispatch key, no binder and no pushed argument. The other,
//! [`Interpreter::try_call_trir`], is §4's generic prologue for a `CallFunc`
//! that reaches a TRIR routine by name and hands it arguments on the VM
//! stack.
//!
//! Declining is free everywhere: every `None` below leaves the VM state
//! exactly as it was, and the caller takes its ordinary path.

use super::compile::TrirCompiler;
use super::exec::{TrExecState, TrOutcome, TrScratch};
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

/// The bound `is rw` parameters: which native slot holds each, and where it
/// goes back. Four is well past any Stage 1 signature, and a fixed array
/// keeps the per-call path free of a heap allocation.
type RwPlan = ([(u16, RwTarget); 4], usize);

impl Interpreter {
    /// Execute a compile-time-resolved TRIR call site (ADR-0110 §3.3).
    ///
    /// `None` means the site could not be served — the callee has been
    /// replaced since it was resolved, or an argument has a shape the chunk's
    /// proof does not cover. The caller then takes the cold by-name fallback,
    /// which reproduces the untyped call site exactly.
    pub(crate) fn exec_call_trir_site(
        &mut self,
        site: &crate::trir::TrCallSite,
        compiled_fns: &CompiledFns,
        caller_code: &CompiledCode,
    ) -> Option<Result<Value, RuntimeError>> {
        let cf = compiled_fns
            .get(&site.key)
            .filter(|cf| cf.fingerprint == site.fingerprint)?;
        let chunk = cf.trir.as_ref()?.clone();
        if chunk.params.len() != site.arg_slots.len() {
            return None;
        }
        let mut st = TrExecState::new(&chunk, self.take_trir_scratch());
        let plan = self.trir_bind_from_slots(&chunk, &mut st, site, caller_code);
        self.trir_run_and_finish(&chunk, st, plan)
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
    ) -> Option<Result<Value, RuntimeError>> {
        let chunk = cf.trir.as_ref()?.clone();
        if self.stack.len() - args_base != chunk.params.len() {
            return None;
        }
        let mut st = TrExecState::new(&chunk, self.take_trir_scratch());
        let plan = self.trir_bind_from_stack(&chunk, &mut st, args_base, caller_code);
        let out = self.trir_run_and_finish(&chunk, st, plan)?;
        self.stack.truncate(args_base);
        Some(out)
    }

    /// Resolve the free variables, run the chunk, and write the `is rw`
    /// results back. Shared by both doors, so the recycle-on-every-exit
    /// bookkeeping exists once.
    fn trir_run_and_finish(
        &mut self,
        chunk: &TrChunk,
        mut st: TrExecState<'_>,
        plan: Option<RwPlan>,
    ) -> Option<Result<Value, RuntimeError>> {
        let Some((rw, rw_len)) = plan else {
            self.recycle_trir_scratch(st.finish());
            return None;
        };
        // Stage 1 bodies contain no calls at all (the compiler admits no call
        // form), so nothing running inside the chunk can read or write a free
        // variable — which is what makes reading them once at entry exactly
        // equivalent to reading them per access, with no cell handle and no
        // invalidation to get wrong. When a later stage admits a body that
        // calls out, this has to become the pre-resolved cell of §3.1.
        if !chunk.outers.is_empty() && !self.trir_push_outers(chunk, &mut st) {
            self.recycle_trir_scratch(st.finish());
            return None;
        }
        let TrOutcome::Value(result) = st.run() else {
            // A checked boundary op met a shape the compiler's proof did not
            // cover. Nothing observable has happened yet — the `is rw`
            // writeback is below, and a Stage 1 body has no other effect — so
            // the untyped path can run the call from the beginning.
            self.recycle_trir_scratch(st.finish());
            return None;
        };
        let mut failure = None;
        for &(native_slot, target) in &rw[..rw_len] {
            let v = Value::int(st.native_slot(native_slot));
            match target {
                RwTarget::Cell(slot) => {
                    if let ValueView::ContainerRef(cell) = self.locals[slot as usize].view() {
                        *cell.lock().unwrap_or_else(|e| e.into_inner()) = v;
                    } else {
                        // The bind pass proved this slot held a cell, and
                        // nothing in a Stage 1 body can have replaced it — an
                        // internal invariant, so report rather than panic
                        // (#8186).
                        failure = Some(RuntimeError::new(
                            "internal error: a TRIR `is rw` slot lost its container".to_string(),
                        ));
                    }
                }
                RwTarget::Slot(slot) => self.locals[slot as usize] = v,
            }
        }
        self.recycle_trir_scratch(st.finish());
        match failure {
            Some(e) => Some(Err(e)),
            None => Some(Ok(result)),
        }
    }

    /// Bind the parameters of a statically resolved call site from the
    /// caller's own frame slots.
    fn trir_bind_from_slots(
        &mut self,
        chunk: &TrChunk,
        st: &mut TrExecState<'_>,
        site: &crate::trir::TrCallSite,
        caller_code: &CompiledCode,
    ) -> Option<RwPlan> {
        let mut rw = [(0u16, RwTarget::Slot(0)); 4];
        let mut rw_len = 0usize;
        for (i, p) in chunk.params.iter().enumerate() {
            let caller_slot = site.arg_slots[i];
            if p.is_rw {
                if rw_len == rw.len() {
                    return None;
                }
                let (raw, target) = self.bind_rw_slot(caller_slot, caller_code)?;
                st.set_native_slot(p.slot, raw);
                rw[rw_len] = (p.slot, target);
                rw_len += 1;
                continue;
            }
            let val = self.locals.get(caller_slot as usize)?.clone().into_deref();
            Self::bind_ro_param(st, p, &val)?;
        }
        Some((rw, rw_len))
    }

    /// Bind the parameters of a by-name call from the VM stack.
    fn trir_bind_from_stack(
        &mut self,
        chunk: &TrChunk,
        st: &mut TrExecState<'_>,
        args_base: usize,
        caller_code: Option<&CompiledCode>,
    ) -> Option<RwPlan> {
        let mut rw = [(0u16, RwTarget::Slot(0)); 4];
        let mut rw_len = 0usize;
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
                let (raw, target) = self.bind_rw_slot(caller_slot, caller_code?)?;
                st.set_native_slot(p.slot, raw);
                rw[rw_len] = (p.slot, target);
                rw_len += 1;
                continue;
            }
            let val = self.stack[args_base + i].unwrap_varref().clone();
            Self::bind_ro_param(st, p, &val)?;
        }
        Some((rw, rw_len))
    }

    /// Bind one read-only parameter, mirroring the general binder's
    /// admissions: an `Int` or a `Bool` (which does `Int`) for a native
    /// `int`, an `Int`/`Num` for a native `num`, an actual string for a
    /// native `str`. A bare type object, a `BigInt` outside `int`'s range and
    /// everything else decline, so the untyped path raises the error the
    /// program should see.
    fn bind_ro_param(
        st: &mut TrExecState<'_>,
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
                st.set_native_slot(p.slot, n);
            }
            TrKind::Num => {
                let n = match val.view() {
                    ValueView::Num(n) => n,
                    ValueView::Int(i) => i as f64,
                    _ => return None,
                };
                st.set_native_slot(p.slot, n.to_bits() as i64);
            }
            TrKind::Obj => {
                if p.type_name == "str" && val.as_str().is_none() {
                    return None;
                }
                st.set_obj_slot(p.slot, val.clone());
            }
        }
        Some(())
    }

    /// Read a native `is rw` parameter out of the caller's slot, and settle
    /// where its result goes back.
    ///
    /// A slot the caller also mirrors by name (`needs_env_sync`) is promoted
    /// ONCE to a shared `ContainerRef` cell — the same promotion the untyped
    /// `is rw` path performs (`capture_var_cell_boxing_type_objects`) — after
    /// which both halves are the same container and the writeback is one
    /// store through it. Writing the slot and the env mirror separately on
    /// every call was the alternative, and it cost a hash insert per call.
    ///
    /// Copy-in/copy-out over the call is sound *because a Stage 1 body cannot
    /// call anything*: the caller's frame is suspended and no other code runs
    /// between the read and the write, so nothing exists that could observe
    /// the two diverge. A later stage admitting a body that calls out must
    /// replace this with a real slot reference.
    fn bind_rw_slot(&mut self, slot: u32, caller_code: &CompiledCode) -> Option<(i64, RwTarget)> {
        let idx = slot as usize;
        if idx >= caller_code.locals.len() || idx >= self.locals.len() {
            return None;
        }
        if let ValueView::ContainerRef(cell) = self.locals[idx].view() {
            let inner = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
            let raw = Self::trir_rw_int(&inner)?;
            return Some((raw, RwTarget::Cell(slot)));
        }
        let raw = Self::trir_rw_int(&self.locals[idx])?;
        if !caller_code.needs_env_sync.get(idx).copied().unwrap_or(true) {
            return Some((raw, RwTarget::Slot(slot)));
        }
        // Promote, so every later call through this site writes one container
        // that both the slot and the env mirror already denote.
        let name = caller_code.locals[idx].clone();
        let inner = self.locals[idx].clone();
        let cell = self.capture_var_cell_boxing_type_objects(caller_code, &name, inner, Some(slot));
        if !cell.is_container_ref() {
            return None;
        }
        self.locals[idx] = cell;
        Some((raw, RwTarget::Cell(slot)))
    }

    /// The raw `i64` a native `is rw` parameter binds from.
    fn trir_rw_int(v: &Value) -> Option<i64> {
        match v.view() {
            ValueView::Int(i) => Some(i),
            ValueView::Bool(b) => Some(b as i64),
            _ => None,
        }
    }

    /// Seed this invocation's free variables, answering whether all of them
    /// resolved.
    ///
    /// The *bindings* are resolved once per chunk and memoized
    /// (`trir_outer_cache`, invalidated by `unit_lexical_gen`); what is read
    /// per call is the binding's current value. `unit_lexicals` holds shared
    /// cells rather than snapshots, so a cached cell stays live — a write
    /// through it from anywhere is seen here without re-resolving.
    fn trir_push_outers(&mut self, chunk: &TrChunk, st: &mut TrExecState<'_>) -> bool {
        let key = chunk as *const TrChunk as usize;
        let cache_gen = self.unit_lexical_gen;
        if !self
            .trir_outer_cache
            .get(&key)
            .is_some_and(|(g, _)| *g == cache_gen)
        {
            let mut bindings = Vec::with_capacity(chunk.outers.len());
            let mut all_celled = true;
            for o in &chunk.outers {
                match self.trir_outer_binding(chunk.name, o.name.as_str()) {
                    Some(v) => {
                        all_celled &= v.is_container_ref();
                        bindings.push(v);
                    }
                    None => return false,
                }
            }
            if !all_celled {
                // At least one free variable resolved to a plain environment
                // VALUE rather than a shared cell, so caching it would freeze
                // it: re-resolve on every call instead. (The capture pass
                // gives a mainline `my` a cell as soon as a named sub reads
                // it, so this is the uncommon shape.)
                for v in bindings {
                    st.push_outer(v.into_deref());
                }
                return true;
            }
            self.trir_outer_cache.insert(key, (cache_gen, bindings));
        }
        // Cached: every entry is a shared cell, so reading THROUGH it here is
        // what makes a write from anywhere visible without re-resolving. The
        // borrow ends inside the loop, so the vector itself is not cloned.
        for i in 0..chunk.outers.len() {
            let v = self.trir_outer_cache[&key].1[i].clone().into_deref();
            st.push_outer(v);
        }
        true
    }

    /// The BINDING (the `unit_lexicals` cell, or the environment entry) a
    /// TRIR chunk's free variable names.
    ///
    /// Resolved against the CALLEE's own captured-lexical bucket (ADR-0024's
    /// `mainline_lexical_subs`), not against the running frame:
    /// `Interpreter::unit_lexical_slot` asks the routine stack which bucket is
    /// active, and TRIR pushes no routine frame, so it would answer for the
    /// caller. Asking by the callee's name is what the frame would have said,
    /// without the frame.
    fn trir_outer_binding(&self, callee: Symbol, name: &str) -> Option<Value> {
        if let Some(bucket) = self.mainline_lexical_subs.get(callee.as_str())
            && let Some(v) = self.unit_lexicals.get(bucket).and_then(|m| m.get(name))
        {
            return Some(v.clone());
        }
        // A free variable the capture pass did not put in a bucket is an
        // ordinary environment name (a mainline `my` the sub reads while the
        // mainline frame is still live). It may well be a plain value rather
        // than a cell, which is why the caller refuses to cache a binding
        // that is not celled.
        self.env().get(name).cloned()
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
            let value = self.locals.get(slot as usize).cloned().unwrap_or(Value::NIL);
            let sym = caller_code
                .locals_sym
                .get(slot as usize)
                .copied()
                .unwrap_or_else(|| Symbol::intern(""));
            args.push(Value::varref_slotted(sym, value, None, Some(slot)));
        }
        let name = site.name.resolve();
        self.call_function(&name, args)
    }

    /// Borrow the pooled TRIR frame buffers. A nested call (which Stage 1
    /// cannot make, but a later stage will) simply gets a fresh set.
    fn take_trir_scratch(&mut self) -> Box<TrScratch> {
        self.trir_scratch.take().unwrap_or_default()
    }

    /// Hand the buffers back, keeping their allocations for the next call.
    fn recycle_trir_scratch(&mut self, mut buf: Box<TrScratch>) {
        buf.reset();
        self.trir_scratch = Some(buf);
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
) -> Option<std::sync::Arc<TrChunk>> {
    let chunk = TrirCompiler::compile(name, param_defs, params, return_type, body);
    if std::env::var("MUTSU_TRIR_DUMP").is_ok() {
        match &chunk {
            Some(c) => eprintln!(
                "trir: {} accepted ({} ops, {} native slots, {} obj slots, {} outers)",
                name.as_str(),
                c.ops.len(),
                c.n_native,
                c.n_obj,
                c.outers.len(),
            ),
            None => eprintln!("trir: {} declined", name.as_str()),
        }
    }
    chunk.map(std::sync::Arc::new)
}
