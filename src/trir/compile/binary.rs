//! Binary operators in a TRIR body: short-circuit logic, concatenation,
//! and native arithmetic and comparison.

use super::TrirCompiler;
use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::trir::{TrKind, TrOp};

impl TrirCompiler<'_> {
    /// `a && b` / `a || b`, short-circuiting, on native int operands.
    ///
    /// Only there: the result is an OPERAND, not its truth value, and only an
    /// int is its own truth value. A boxed operand used to be narrowed to its
    /// truthiness, so `has-interp($s) && 'both'` answered `1`
    /// (`t/modules/import-export/imported-call-expression-prefix.t`); such an
    /// expression now declines, and the untyped path yields the operand.
    fn compile_short_circuit(&mut self, and: bool, l: &Expr, r: &Expr) -> Option<TrKind> {
        let lk = self.compile_expr(l)?;
        let lk = self.narrow_int_operand(lk);
        if lk != TrKind::Int {
            self.note_decline(|| "a non-int left operand of && / ||".to_string());
            return None;
        }
        // The jump PEEKS: Raku's `&&`/`||` yield an OPERAND rather than a
        // boolean, and on the int bank the operand is its own truth value, so
        // the short-circuit result is the value already there.
        let jump_at = self.ops.len();
        self.ops.push(if and {
            TrOp::JumpIfFalseKeepI(0)
        } else {
            TrOp::JumpIfTrueKeepI(0)
        });
        self.ops.push(TrOp::PopI);
        let rk = self.compile_expr(r)?;
        let rk = self.narrow_int_operand(rk);
        if rk != TrKind::Int {
            self.note_decline(|| "a non-int right operand of && / ||".to_string());
            return None;
        }
        let end = self.ops.len() as u32;
        match &mut self.ops[jump_at] {
            TrOp::JumpIfFalseKeepI(x) | TrOp::JumpIfTrueKeepI(x) => *x = end,
            _ => return None,
        }
        Some(TrKind::Int)
    }

    /// Narrow a boxed operand that an int-returning `nqp::` op produced
    /// (`nqp::isge_i(...) && ...` through the generic op), whose value IS a
    /// native int; anything else keeps its kind.
    fn narrow_int_operand(&mut self, kind: TrKind) -> TrKind {
        if kind == TrKind::Obj && self.nqp_sourced && self.nqp_int_result {
            return self.narrow_nqp_result(kind);
        }
        kind
    }

    pub(super) fn compile_binary(
        &mut self,
        left: &Expr,
        op: &TokenKind,
        right: &Expr,
    ) -> Option<TrKind> {
        if matches!(op, TokenKind::AndAnd | TokenKind::OrOr) {
            return self.compile_short_circuit(*op == TokenKind::AndAnd, left, right);
        }
        // `~` goes through the interpreter's own `Concat`, which is where a
        // user `infix:<~>` override is honoured — so TRIR neither reproduces
        // that rule nor has to prove nobody declared one.
        if matches!(op, TokenKind::Tilde) {
            let lk = self.compile_expr(left)?;
            self.coerce(lk, TrKind::Obj)?;
            let rk = self.compile_expr(right)?;
            self.coerce(rk, TrKind::Obj)?;
            self.ops.push(TrOp::ConcatBin);
            return Some(TrKind::Obj);
        }
        // Only arithmetic and comparison on operands the compiler already
        // proved native. A boxed operand declines: `+` on two boxed values is
        // full Raku multi-dispatch (a user `infix:<+>` may override it), and
        // reproducing that is not TRIR's job.
        let lk = self.compile_expr(left)?;
        let lk = self.narrow_nqp_result(lk);
        if !lk.is_native() {
            self.note_decline(|| format!("boxed left operand of {op:?}"));
            return None;
        }
        let rk = self.compile_expr(right)?;
        let rk = self.narrow_nqp_result(rk);
        if !rk.is_native() {
            self.note_decline(|| format!("boxed right operand of {op:?}"));
            return None;
        }
        // Widen to `num` when either side is one, exactly as Raku's own
        // native numeric promotion does.
        let want = if lk == TrKind::Num || rk == TrKind::Num {
            TrKind::Num
        } else {
            TrKind::Int
        };
        if rk != want {
            self.coerce(rk, want)?;
        }
        if lk != want {
            // The left operand is buried under the right one; Stage 1 does
            // not reorder the bank, so a mixed-kind pair with the NUM on the
            // right declines rather than emitting a swap.
            return None;
        }
        let (ops, result): (&[TrOp], TrKind) = match (want, op.clone()) {
            (TrKind::Int, TokenKind::Plus) => (&[TrOp::AddI], TrKind::Int),
            (TrKind::Int, TokenKind::Minus) => (&[TrOp::SubI], TrKind::Int),
            (TrKind::Int, TokenKind::Star) => (&[TrOp::MulI], TrKind::Int),
            (TrKind::Int, TokenKind::EqEq) => (&[TrOp::EqI], TrKind::Int),
            (TrKind::Int, TokenKind::BangEq) => (&[TrOp::NeI], TrKind::Int),
            (TrKind::Int, TokenKind::Lt) => (&[TrOp::LtI], TrKind::Int),
            (TrKind::Int, TokenKind::Lte) => (&[TrOp::LeI], TrKind::Int),
            (TrKind::Int, TokenKind::Gt) => (&[TrOp::GtI], TrKind::Int),
            (TrKind::Int, TokenKind::Gte) => (&[TrOp::GeI], TrKind::Int),
            (TrKind::Num, TokenKind::Plus) => (&[TrOp::AddN], TrKind::Num),
            (TrKind::Num, TokenKind::Minus) => (&[TrOp::SubN], TrKind::Num),
            (TrKind::Num, TokenKind::Star) => (&[TrOp::MulN], TrKind::Num),
            (TrKind::Num, TokenKind::Slash) => (&[TrOp::DivN], TrKind::Num),
            (TrKind::Num, TokenKind::EqEq) => (&[TrOp::EqN], TrKind::Int),
            (TrKind::Num, TokenKind::Lt) => (&[TrOp::LtN], TrKind::Int),
            (TrKind::Num, TokenKind::Lte) => (&[TrOp::LeN], TrKind::Int),
            (TrKind::Num, TokenKind::Gt) => (&[TrOp::GtN], TrKind::Int),
            (TrKind::Num, TokenKind::Gte) => (&[TrOp::GeN], TrKind::Int),
            // `int / int` is a `Rat` in Raku, not an integer division — it is
            // deliberately absent here. So is `%`: Raku's `%` takes the
            // divisor's sign (`-17 % 5 == 3`) where `ModI` is `nqp::mod_i`,
            // which takes the dividend's, and it raises its own error on a
            // zero divisor.
            _ => {
                self.note_decline(|| format!("operator {op:?} on {want:?}"));
                return None;
            }
        };
        self.ops.extend_from_slice(ops);
        Some(result)
    }
}
