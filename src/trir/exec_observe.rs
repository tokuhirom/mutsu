//! The TRIR ops that can observe the routine stack (ADR-8673 D1).
//!
//! The main loop (`exec.rs`) dispatches the ops proved not to look at the
//! routine stack itself and sends every other op here, after pushing the
//! routine-stack entries that inner TRIR calls deferred
//! ([`Interpreter::trir_observe`]). Each op here can reach code or state that
//! reads the stack:
//!
//! - `ConcatN`, `ConcatBin`: `.Str` of a non-`Str` operand, and the warning a
//!   type object raises (a backtrace, a `CONTROL` handler);
//! - `TruthyObj`, `TruthyDefined`: `.Bool` / `.defined` of a user object;
//! - `ClassOperand`: type-name resolution, which reads the top frame's
//!   package on its miss path;
//! - `MethodGen`, `CallGen`: calls out of TRIR (`callframe`, `CALLER::`,
//!   backtraces, `&?ROUTINE`); a `CallGen` link check reads the top frame.

use super::frame::TrFrame;
use super::{TrChunk, TrOp};
use crate::opcode::CompiledFns;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

impl Interpreter {
    /// Run one observing op. `Ok(None)` bails the chunk.
    // Cost: O(1) dispatch plus the op's own work (a call, a coercion).
    pub(super) fn exec_trir_observing_op(
        &mut self,
        chunk: &TrChunk,
        op: &TrOp,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<Option<()>, RuntimeError> {
        match op {
            TrOp::ConcatN(n) => {
                let base = self.trir.os.len().saturating_sub(*n as usize);
                let vals: Vec<Value> = self.trir.os.drain(base..).collect();
                self.stack.extend(vals);
                self.exec_string_concat_op(*n as u32, &[])?;
                let v = self.stack.pop().unwrap_or(Value::NIL);
                self.trir.os.push(v);
            }

            TrOp::TruthyDefined => {
                // The same test the untyped `JumpIfNotNil` arm makes.
                let v = self.opop();
                let defined = self.value_is_defined_dispatch(&v);
                self.trir.ns.push(defined as i64);
            }

            TrOp::TruthyObj => {
                let v = self.opop();
                let t = self.eval_truthy(&v);
                self.trir.ns.push(t as i64);
            }
            TrOp::ConcatBin => {
                let r = self.opop();
                let l = self.opop();
                self.stack.push(l);
                self.stack.push(r);
                self.exec_concat_op()?;
                let v = self.stack.pop().unwrap_or(Value::NIL);
                self.trir.os.push(v);
            }

            TrOp::ClassOperand(site) => {
                let v = self.trir_class_operand(site, compiled_fns)?;
                self.trir.os.push(v);
            }

            TrOp::MethodGen(site) => {
                self.exec_trir_method_call(chunk, *site)?;
                if !self.trir_reseed_outers(chunk, frame) {
                    return Ok(None);
                }
            }
            TrOp::CallGen(site) => {
                if self
                    .exec_trir_generic_call(chunk, *site, frame, compiled_fns)?
                    .is_none()
                {
                    return Ok(None);
                }
                if !self.trir_reseed_outers(chunk, frame) {
                    return Ok(None);
                }
            }
            // The main loop dispatches every other op; reaching here with one
            // is a dispatch bug, reported rather than run with a stale stack.
            _ => {
                return Err(RuntimeError::new(format!(
                    "internal error: TRIR op {op:?} reached the observing dispatch"
                )));
            }
        }
        Ok(Some(()))
    }
}
