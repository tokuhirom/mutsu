//! The two call ops of a TRIR body — ADR-0110 §3.3.
//!
//! `CallTr` is the static linkage the ADR describes: the callee was resolved
//! at compile time, its signature is known at the call site, so the binder is
//! compiled away and a native `is rw` parameter is passed as a reference to
//! the caller's own slot rather than as a container.
//!
//! `CallGen` is the escape the ADR's Stage 2 list does not name but
//! `JSON::Fast` needs on every one of its scanner routines: the hot loop is
//! typed and the routine ends in a `die` helper that is arbitrary Raku.
//! Refusing the routine for the cold path would leave the hot loop untyped,
//! so instead the call goes out through the ordinary dispatch — boxed
//! arguments, boxed result. Nothing about it is a fallback *inside* the
//! instruction set: a call is a call, TRIR simply did not resolve this one.

use super::exec::TrOutcome;
use super::frame::TrFrame;
use super::{TrArg, TrCallee, TrChunk, TrKind};
use crate::opcode::CompiledFns;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// Execute a `CallTr` site. `Ok(None)` means the callee could not be
    /// served after all (it has been replaced, or wrapped), which bails the
    /// whole chunk.
    pub(super) fn exec_trir_inner_call(
        &mut self,
        chunk: &TrChunk,
        site: u32,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<Option<()>, RuntimeError> {
        let call = &chunk.calls[site as usize];
        let TrCallee::Trir(link) = &call.callee else {
            return Ok(None);
        };
        // ADR-0110 §3.3's run-time guard, as on the outermost door.
        if self.any_routine_wrapped() && self.routine_is_wrapped(&call.name.resolve()) {
            return Ok(None);
        }
        if !link.current_in(compiled_fns) {
            return Ok(None);
        }
        let callee = link.chunk.clone();
        let callee_pkg = link.pkg;
        if callee.params.len() != call.args.len() {
            return Ok(None);
        }
        // ADR-0100: refuse while there is still stack to raise with, so deep
        // TRIR recursion becomes a catchable exception rather than a
        // guard-page abort.
        self.guard_native_stack()?;

        // The VALUE arguments were pushed left to right, so they are the top
        // `n_nat` entries of the native bank and the top `n_obj` of the boxed
        // one, in argument order. They are read in place and dropped once the
        // callee frame holds them, rather than popped into two vectors
        // allocated per call.
        let mut n_nat = 0usize;
        let mut n_obj = 0usize;
        for arg in &call.args {
            match arg {
                TrArg::Value(TrKind::Int | TrKind::Num) => n_nat += 1,
                TrArg::Value(TrKind::Obj) => n_obj += 1,
                TrArg::Native(_) | TrArg::Obj(_) | TrArg::Ref(_) => {}
            }
        }
        let ns_base = self.trir.ns.len().saturating_sub(n_nat);
        let os_base = self.trir.os.len().saturating_sub(n_obj);
        let mut next_nat = ns_base;
        let mut next_obj = os_base;

        let mut callee_frame = self.trir.push_frame(callee.n_native, callee.n_obj);
        let nbase = frame.nbase as usize;
        let obase = frame.obase as usize;
        let cnb = callee_frame.nbase as usize;
        let cob = callee_frame.obase as usize;
        for (i, p) in callee.params.iter().enumerate() {
            let slot = p.slot as usize;
            match (&call.args[i], p.is_rw) {
                // A native `is rw` parameter: hand it the ABSOLUTE index of
                // the caller's slot. The callee's `GetRefI`/`SetRefI` then
                // write the caller's variable directly, and passing the
                // parameter on keeps naming the same slot.
                (TrArg::Native(s), true) => {
                    self.trir.nl[cnb + slot] = (nbase + *s as usize) as i64;
                }
                (TrArg::Ref(s), true) => {
                    self.trir.nl[cnb + slot] = self.trir.nl[nbase + *s as usize];
                }
                (TrArg::Native(s), false) => {
                    self.trir.nl[cnb + slot] = self.trir.nl[nbase + *s as usize];
                }
                (TrArg::Ref(s), false) => {
                    let r = self.trir.nl[nbase + *s as usize] as usize;
                    self.trir.nl[cnb + slot] = self.trir.nl[r];
                }
                (TrArg::Obj(s), _) => {
                    self.trir.ol[cob + slot] = self.trir.ol[obase + *s as usize].clone();
                }
                (TrArg::Value(TrKind::Int | TrKind::Num), _) => {
                    self.trir.nl[cnb + slot] = self.trir.ns.get(next_nat).copied().unwrap_or(0);
                    next_nat += 1;
                }
                (TrArg::Value(TrKind::Obj), _) => {
                    if let Some(v) = self.trir.os.get_mut(next_obj) {
                        self.trir.ol[cob + slot] = std::mem::replace(v, Value::NIL);
                    }
                    next_obj += 1;
                }
            }
        }
        // The consumed values sit below the callee frame's operand marks.
        self.trir.ns.truncate(ns_base);
        self.trir.os.truncate(os_base);
        callee_frame.ns_mark = ns_base as u32;
        callee_frame.os_mark = os_base as u32;
        // The callee's own package, for its body only — see
        // `trir_body_package`. A resolved call may cross packages, and the
        // callee's free variables and `CallGen` sites resolve by name.
        let guard = callee_pkg.map(|p| self.enter_package_guarded_sym(p));
        if !self.trir_seed_outers(&callee, callee_frame) {
            drop(guard);
            self.trir.pop_frame(callee_frame);
            return Ok(None);
        }
        let outcome = self.run_trir_routine(&callee, callee_frame, compiled_fns);
        drop(guard);
        self.trir.pop_frame(callee_frame);
        match outcome? {
            TrOutcome::Value(v) => {
                self.push_trir_result(v, call.result);
                Ok(Some(()))
            }
            TrOutcome::Bail => Ok(None),
        }
    }

    /// Execute a `CallGen` site: box the arguments, dispatch by name through
    /// the ordinary machinery, and take the result back boxed — unless the
    /// site has been linked to the TRIR routine it reaches (`gen_link.rs`).
    /// `Ok(None)` means a linked callee bailed, which bails this chunk too.
    pub(super) fn exec_trir_generic_call(
        &mut self,
        chunk: &TrChunk,
        site: u32,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<Option<()>, RuntimeError> {
        let call = &chunk.calls[site as usize];
        match self.try_trir_gen_link(chunk, site, call, frame, compiled_fns)? {
            super::gen_link::GenOutcome::Done => return Ok(Some(())),
            super::gen_link::GenOutcome::Bail => return Ok(None),
            super::gen_link::GenOutcome::NotLinked => {}
        }
        let nbase = frame.nbase as usize;
        let obase = frame.obase as usize;
        // Only a by-variable argument can be handed over as a container, so a
        // call whose arguments are all already-evaluated values needs no
        // signature lookup at all — which is most of them (`die "..."`).
        let rw_mask = if call.args.iter().any(|a| !matches!(a, TrArg::Value(_))) {
            self.trir_callee_rw_mask(&call.name.resolve())
        } else {
            0
        };
        let mut args: Vec<Value> = vec![Value::NIL; call.args.len()];
        for (i, arg) in call.args.iter().enumerate().rev() {
            let wants_container = rw_mask >> i.min(63) & 1 == 1;
            args[i] = match arg {
                TrArg::Value(TrKind::Int) => Value::int(self.ipop()),
                TrArg::Value(TrKind::Num) => Value::num(f64::from_bits(self.ipop() as u64)),
                TrArg::Value(TrKind::Obj) => self.opop(),
                // A named variable. It is handed over as a BARE container
                // only where `rw_mask` says the callee declares that
                // parameter `is rw` — the shape the `is rw` alias pre-pass
                // calls "already a shared cell relayed from an outer `is rw`
                // parameter" and passes through unchanged — and as its plain
                // value everywhere else.
                //
                // Containerizing unconditionally is what a call site does not
                // know enough to do: it is NOT transparent. A container
                // argument reaches a `proto`'s `{*}` re-dispatch as itself
                // and fails the winning candidate's type check against its
                // own type, and `nativecast`'s "type object as its first
                // argument" check rejects it too. An untyped call site emits
                // `WrapVarRef`, whose value is the container only when the
                // variable is genuinely captured; the mask is how a TRIR
                // frame — which has no caller local for `capture_var_cell` to
                // alias — reaches the same place.
                TrArg::Native(s) => {
                    let v = Value::int(self.trir.nl[nbase + *s as usize]);
                    if wants_container {
                        v.into_container_ref()
                    } else {
                        v
                    }
                }
                TrArg::Ref(s) => {
                    let r = self.trir.nl[nbase + *s as usize] as usize;
                    let v = Value::int(self.trir.nl[r]);
                    if wants_container {
                        v.into_container_ref()
                    } else {
                        v
                    }
                }
                TrArg::Obj(s) => {
                    let v = self.trir.ol[obase + *s as usize].clone();
                    if wants_container {
                        v.into_container_ref()
                    } else {
                        v
                    }
                }
            };
        }
        let name = call.name.resolve();
        // Mirror `exec_call_func_op`'s save/restore of the multi-candidate
        // literal mask: a TRIR site's arguments are never literals, but the
        // mask must be published rather than inherited from the caller.
        let saved = std::mem::replace(&mut self.literal_native_args, 0);
        let armed = self.trir_gen_arm(call.name);
        // Only an `is rw` parameter was handed a container, so only then do
        // the arguments need keeping for the read-back below.
        let kept = (rw_mask != 0).then(|| args.clone());
        let result = self.call_function(&name, args);
        self.trir_gen_settle(chunk, site, armed);
        self.literal_native_args = saved;
        let result = result?;
        // Read the containers back, so an `is rw` parameter's write lands in
        // this frame's slot.
        let args = kept.unwrap_or_default();
        for (i, arg) in call.args.iter().enumerate().take(args.len()) {
            let ValueView::ContainerRef(cell) = args[i].view() else {
                continue;
            };
            let inner = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
            match arg {
                TrArg::Native(s) => {
                    if let Some(n) = inner.as_int() {
                        self.trir.nl[nbase + *s as usize] = n;
                    }
                }
                TrArg::Ref(s) => {
                    if let Some(n) = inner.as_int() {
                        let r = self.trir.nl[nbase + *s as usize] as usize;
                        self.trir.nl[r] = n;
                    }
                }
                TrArg::Obj(s) => self.trir.ol[obase + *s as usize] = inner,
                TrArg::Value(_) => {}
            }
        }
        self.push_trir_result(result, call.result);
        Ok(Some(()))
    }

    /// Execute a `MethodGen` site: pop the arguments and the receiver, and
    /// dispatch the method through the ordinary method dispatch.
    pub(super) fn exec_trir_method_call(
        &mut self,
        chunk: &TrChunk,
        site: u32,
    ) -> Result<(), RuntimeError> {
        let m = &chunk.methods[site as usize];
        let n = m.arity as usize;
        let base = self.trir.os.len().saturating_sub(n);
        let args: Vec<Value> = self.trir.os.drain(base..).collect();
        let target = self.opop();
        let name = m.name.resolve();
        // `CallMethod`'s own rendering of an itemized receiver (`$[1, 2]`),
        // which the by-value dispatch does not know about.
        if name == "raku"
            && n == 0
            && crate::builtins::methods_0arg::raku_repr::raku_scalar_itemized(&target)
            && let Some(rendered) = self.raku_repr_with_dispatch(&target)
        {
            self.trir.os.push(Value::str(rendered));
            return Ok(());
        }
        let v = self.call_method_with_values(target, &name, args)?;
        self.trir.os.push(v);
        Ok(())
    }

    /// Which of the routine named `name`'s first 64 positional parameters are
    /// `is rw`, as a bitmask — the arguments a generic call must hand over as
    /// containers so the callee's write reaches this frame's slot.
    ///
    /// `0` for everything this cannot resolve to a single routine: a builtin,
    /// an interpreter hook, an unresolved multi. That is the right default —
    /// those take values, and handing one a container is what broke
    /// `nativecast` and every `proto` candidate's type check.
    fn trir_callee_rw_mask(&self, name: &str) -> u64 {
        let Some(def) = self.resolve_function(name) else {
            return 0;
        };
        let mut mask = 0u64;
        let mut positional = 0usize;
        for pd in &def.param_defs {
            if pd.named {
                continue;
            }
            if positional >= 64 {
                break;
            }
            if pd.traits.iter().any(|t| t == "rw") {
                mask |= 1 << positional;
            }
            positional += 1;
        }
        mask
    }

    /// Put a call's result on the bank the compiler expects it on.
    pub(super) fn push_trir_result(&mut self, v: Value, kind: TrKind) {
        match kind {
            TrKind::Int => self.trir.ns.push(v.as_int().unwrap_or(0)),
            TrKind::Num => self.trir.ns.push(v.to_f64().to_bits() as i64),
            TrKind::Obj => self.trir.os.push(v),
        }
    }
}
