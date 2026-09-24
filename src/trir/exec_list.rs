//! The typed list ops of TRIR (ADR-0112 Step 3, ADR-0116 D2.1) and the
//! operand-bank pops every arm uses.

use super::TrOp;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

impl Interpreter {
    /// Run one of the typed list ops. The `*Local` forms read their list
    /// straight out of boxed slot `n`, so they pay no `Value` clone and drop.
    // Cost: O(1) amortized, plus the generic `nqp::elems` for a non-list operand.
    #[inline]
    pub(super) fn trir_list_op(&mut self, op: &TrOp, obase: usize) -> Result<(), RuntimeError> {
        match op {
            TrOp::ElemsO => {
                let v = self.opop();
                let n = self.nqp_elems_count(&v)?;
                self.trir.ns.push(n);
            }
            TrOp::ShiftIO => {
                let v = self.opop();
                let r = Self::nqp_shift_int(&v)?;
                self.trir.ns.push(r);
            }
            TrOp::PushIO => {
                let i = self.ipop();
                let target = self.opop();
                let r = crate::runtime::nqp_ops_text::push_elem("push_i", &target, Value::int(i))?;
                self.trir.os.push(r);
            }
            TrOp::ElemsLocal(n) => {
                let slot = &self.trir.ol[obase + *n as usize];
                let len = match Self::nqp_elems_len_of(slot) {
                    Some(len) => len as i64,
                    None => {
                        let v = slot.clone();
                        self.nqp_elems_count(&v)?
                    }
                };
                self.trir.ns.push(len);
            }
            TrOp::ShiftILocal(n) => {
                let r = Self::nqp_shift_int(&self.trir.ol[obase + *n as usize])?;
                self.trir.ns.push(r);
            }
            TrOp::PushILocal(n) => {
                let i = self.ipop();
                let r = crate::runtime::nqp_ops_text::push_elem(
                    "push_i",
                    &self.trir.ol[obase + *n as usize],
                    Value::int(i),
                )?;
                self.trir.os.push(r);
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "internal: trir_list_op handed {op:?}, which is not a list op"
                )));
            }
        }
        Ok(())
    }

    /// Pop the native operand stack.
    ///
    /// The compiler balances every bank, so it is never empty here; a
    /// hand-built chunk that got it wrong reads 0 rather than panicking,
    /// which keeps an internal bug from becoming a process abort (#8186).
    #[inline]
    pub(super) fn ipop(&mut self) -> i64 {
        self.trir.ns.pop().unwrap_or(0)
    }

    /// Pop the boxed operand stack, with the same contract.
    #[inline]
    pub(super) fn opop(&mut self) -> Value {
        self.trir.os.pop().unwrap_or(Value::NIL)
    }
}
