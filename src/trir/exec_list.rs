//! The typed list ops of TRIR (ADR-0112 Step 3, ADR-0116 D2.1) and the
//! operand-bank pops every arm uses.

use super::TrOp;
use crate::runtime::Interpreter;
use crate::runtime::nqp_backing::{
    uni_code_of, with_list_data_mut, with_uni_codes, with_uni_codes_mut,
};
use crate::runtime::to_int;
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
                let len = self.trir_local_elems(obase + *n as usize)?;
                self.trir.ns.push(len);
            }
            TrOp::ShiftILocal(n) => {
                let r = self.trir_shift_int_local(obase + *n as usize)?;
                self.trir.ns.push(r);
            }
            TrOp::PushILocal(n) => {
                let i = self.ipop();
                self.trir_push_int_local(obase + *n as usize, i)?;
                self.trir.os.push(Value::int(i));
            }
            TrOp::PushILocalVoid(n) => {
                let i = self.ipop();
                self.trir_push_int_local(obase + *n as usize, i)?;
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "internal: trir_list_op handed {op:?}, which is not a list op"
                )));
            }
        }
        Ok(())
    }

    /// `nqp::shift_i` off the list in absolute boxed slot `abs`, in place: a
    /// plain list or a `Uni` directly, anything else through the general op.
    // Cost: O(1) amortized.
    // A `Uni` is answered inline; anything else out of line.
    #[inline(always)]
    pub(super) fn trir_shift_int_local(&mut self, abs: usize) -> Result<i64, RuntimeError> {
        let slot = &self.trir.ol[abs];
        if let Some(code) = with_uni_codes_mut(slot, |codes| codes.shift()) {
            return Ok(code.map_or(0, i64::from));
        }
        self.trir_shift_int_local_list(abs)
    }

    /// [`Self::trir_shift_int_local`] for anything but a `Uni`.
    // Cost: O(1) amortized for a list, plus the general op otherwise.
    #[inline(never)]
    fn trir_shift_int_local_list(&mut self, abs: usize) -> Result<i64, RuntimeError> {
        let slot = &self.trir.ol[abs];
        match with_list_data_mut(slot, |data| data.shift_front()) {
            Some(elem) => Ok(elem.map_or(0, |v| v.as_int().unwrap_or_else(|| to_int(&v)))),
            None => Self::nqp_shift_int(slot),
        }
    }

    /// `nqp::push_i` of `i` onto the list in absolute boxed slot `abs`, in
    /// place: a `Uni`'s packed store or a plain list directly, anything else
    /// (a Buf, an `IterationBuffer`) through the general op.
    // Cost: O(1) amortized.
    // A `Uni` is answered inline; anything else out of line.
    #[inline(always)]
    pub(super) fn trir_push_int_local(&mut self, abs: usize, i: i64) -> Result<(), RuntimeError> {
        let slot = &self.trir.ol[abs];
        if with_uni_codes_mut(slot, |codes| codes.push(uni_code_of(i))).is_some() {
            return Ok(());
        }
        self.trir_push_int_local_list(abs, i)
    }

    /// [`Self::trir_push_int_local`] for anything but a `Uni`.
    // Cost: O(1) amortized for a list, plus the general op otherwise.
    #[inline(never)]
    fn trir_push_int_local_list(&mut self, abs: usize, i: i64) -> Result<(), RuntimeError> {
        let slot = &self.trir.ol[abs];
        if with_list_data_mut(slot, |data| data.push(Value::int(i))).is_none() {
            crate::runtime::nqp_ops_text::push_elem("push_i", slot, Value::int(i))?;
        }
        Ok(())
    }

    /// `nqp::elems` of the list in absolute boxed slot `abs`, read in place.
    // Cost: O(1) for a list, plus the generic `nqp::elems` for anything else.
    // A `Uni` is answered inline; anything else out of line.
    #[inline(always)]
    pub(super) fn trir_local_elems(&mut self, abs: usize) -> Result<i64, RuntimeError> {
        let slot = &self.trir.ol[abs];
        if let Some(len) = with_uni_codes(slot, |codes| codes.len()) {
            return Ok(len as i64);
        }
        self.trir_local_elems_list(abs)
    }

    /// [`Self::trir_local_elems`] for anything but a `Uni`.
    // Cost: O(1) for a list, plus the generic `nqp::elems` otherwise.
    #[inline(never)]
    fn trir_local_elems_list(&mut self, abs: usize) -> Result<i64, RuntimeError> {
        let slot = &self.trir.ol[abs];
        match Self::nqp_elems_len_of(slot) {
            Some(len) => Ok(len as i64),
            None => {
                let v = slot.clone();
                self.nqp_elems_count(&v)
            }
        }
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
