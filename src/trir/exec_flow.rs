//! The results of running TRIR ops, and the operand-bank pops every op
//! shares. Split from `exec.rs` so the op switch has the file to itself.

use crate::runtime::Interpreter;
use crate::value::Value;

/// Where control goes after one op ([`Interpreter::trir_step`]).
pub(crate) enum TrFlow {
    Next,
    Jump(usize),
    Done(TrOutcome),
}

/// What a TRIR chunk did.
pub(crate) enum TrOutcome {
    Value(Value),
    /// An operation met a value whose shape the compiler's proof did not
    /// cover — an `nqp::atpos_i` on something that is not a list, an
    /// `UnboxI` of a non-integer, a callee that has been replaced. The caller
    /// re-runs the routine on the untyped path, which raises whatever the
    /// program should see.
    ///
    /// This is NOT a fallback arm inside the instruction set: it is a bail,
    /// and the compiler only ever places a bail-capable op where re-running
    /// the routine from the beginning is equivalent to never having started
    /// it (see [`super::compile`]'s `effectful` tracking).
    Bail,
}

impl Interpreter {
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
