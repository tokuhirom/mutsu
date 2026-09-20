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
        let TrCallee::Trir { key, fingerprint } = call.callee else {
            return Ok(None);
        };
        // ADR-0110 §3.3's run-time guard, as on the outermost door.
        if self.any_routine_wrapped() && self.routine_is_wrapped(&call.name.resolve()) {
            return Ok(None);
        }
        let Some(cf) = compiled_fns
            .get(&key)
            .filter(|cf| cf.fingerprint == fingerprint)
        else {
            return Ok(None);
        };
        let Some(callee) = cf.trir.clone() else {
            return Ok(None);
        };
        if callee.params.len() != call.args.len() {
            return Ok(None);
        }
        // ADR-0100: refuse while there is still stack to raise with, so deep
        // TRIR recursion becomes a catchable exception rather than a
        // guard-page abort.
        self.guard_native_stack()?;

        // The arguments were pushed left to right, so take the VALUE ones off
        // each bank back to front.
        let mut natives: Vec<i64> = vec![0; call.args.len()];
        let mut objs: Vec<Value> = vec![Value::NIL; call.args.len()];
        for (i, arg) in call.args.iter().enumerate().rev() {
            match arg {
                TrArg::Value(TrKind::Int | TrKind::Num) => natives[i] = self.ipop(),
                TrArg::Value(TrKind::Obj) => objs[i] = self.opop(),
                TrArg::Native(_) | TrArg::Obj(_) | TrArg::Ref(_) => {}
            }
        }

        let callee_frame = self.trir.push_frame(callee.n_native, callee.n_obj);
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
                    self.trir.nl[cnb + slot] = natives[i];
                }
                (TrArg::Value(TrKind::Obj), _) => {
                    self.trir.ol[cob + slot] = std::mem::replace(&mut objs[i], Value::NIL);
                }
            }
        }
        if !self.trir_seed_outers(&callee, callee_frame) {
            self.trir.pop_frame(callee_frame);
            return Ok(None);
        }
        let outcome = self.run_trir_chunk(&callee, callee_frame, compiled_fns);
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
    /// the ordinary machinery, and take the result back boxed.
    pub(super) fn exec_trir_generic_call(
        &mut self,
        chunk: &TrChunk,
        site: u32,
        frame: TrFrame,
    ) -> Result<(), RuntimeError> {
        let call = chunk.calls[site as usize].clone();
        let nbase = frame.nbase as usize;
        let obase = frame.obase as usize;
        let mut args: Vec<Value> = vec![Value::NIL; call.args.len()];
        for (i, arg) in call.args.iter().enumerate().rev() {
            args[i] = match arg {
                TrArg::Value(TrKind::Int) => Value::int(self.ipop()),
                TrArg::Value(TrKind::Num) => Value::num(f64::from_bits(self.ipop() as u64)),
                TrArg::Value(TrKind::Obj) => self.opop(),
                // A named variable, passed as a container: the callee's
                // signature is unknown here, so it may have an `is rw`
                // parameter, and a by-value argument would silently drop the
                // write. Copied back below.
                TrArg::Native(s) => {
                    Value::int(self.trir.nl[nbase + *s as usize]).into_container_ref()
                }
                TrArg::Ref(s) => {
                    let r = self.trir.nl[nbase + *s as usize] as usize;
                    Value::int(self.trir.nl[r]).into_container_ref()
                }
                TrArg::Obj(s) => self.trir.ol[obase + *s as usize].clone().into_container_ref(),
            };
        }
        let name = call.name.resolve();
        // Mirror `exec_call_func_op`'s save/restore of the multi-candidate
        // literal mask: a TRIR site's arguments are never literals, but the
        // mask must be published rather than inherited from the caller.
        let saved = std::mem::replace(&mut self.literal_native_args, 0);
        let result = self.call_function(&name, args.clone());
        self.literal_native_args = saved;
        let result = result?;
        // Read the containers back, so an `is rw` parameter's write lands in
        // this frame's slot.
        for (i, arg) in call.args.iter().enumerate() {
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
        Ok(())
    }

    /// Put a call's result on the bank the compiler expects it on.
    fn push_trir_result(&mut self, v: Value, kind: TrKind) {
        match kind {
            TrKind::Int => self.trir.ns.push(v.as_int().unwrap_or(0)),
            TrKind::Num => self.trir.ns.push(v.to_f64().to_bits() as i64),
            TrKind::Obj => self.trir.os.push(v),
        }
    }
}
