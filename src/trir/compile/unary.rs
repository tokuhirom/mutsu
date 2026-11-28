//! Prefix and postfix operators for [`super::TrirCompiler`]: `++`/`--` on a
//! native int slot, `!` and unary minus.

use super::{Binding, TrirCompiler};
use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::trir::{TrKind, TrOp};

impl TrirCompiler<'_> {
    /// `$i++` / `$i--`: the value before the step, then the step itself (the
    /// sink form of the prefix operator, which runs every check the prefix
    /// form does).
    pub(super) fn compile_postfix_step(&mut self, op: &TokenKind, expr: &Expr) -> Option<TrKind> {
        let Expr::Var(name) = expr else {
            self.note_decline(|| "postfix ++/-- on something other than a variable".to_string());
            return None;
        };
        let before = self.compile_var(name)?;
        if before != TrKind::Int {
            self.note_decline(|| format!("postfix ++/-- on a {before:?} value"));
            return None;
        }
        self.compile_unary(op, expr, true)?;
        Some(TrKind::Int)
    }

    pub(super) fn compile_unary(
        &mut self,
        op: &TokenKind,
        expr: &Expr,
        sink: bool,
    ) -> Option<TrKind> {
        match op {
            TokenKind::PlusPlus | TokenKind::MinusMinus => {
                let Expr::Var(name) = expr else {
                    self.note_decline(|| "++/-- on something other than a variable".to_string());
                    return None;
                };
                let Some(Binding { slot, kind }) = self.binding_of(name) else {
                    let n = name.clone();
                    self.note_decline(|| format!("++/-- on non-local {n}"));
                    return None;
                };
                if kind != TrKind::Int {
                    self.note_decline(|| format!("++/-- on a {kind:?} variable"));
                    return None;
                }
                if self.slot_is_readonly_param(slot, kind) {
                    let n = name.clone();
                    self.note_decline(|| format!("++/-- on the read-only parameter {n}"));
                    return None;
                }
                if self.slot_is_sized(slot, kind) {
                    let n = name.clone();
                    self.note_decline(|| format!("++/-- on the sized native {n}"));
                    return None;
                }
                let up = *op == TokenKind::PlusPlus;
                let by_ref = self.slot_is_ref(slot, kind);
                self.ops.push(match (up, sink, by_ref) {
                    (true, false, false) => TrOp::IncI(slot),
                    (true, true, false) => TrOp::IncIVoid(slot),
                    (false, false, false) => TrOp::DecI(slot),
                    (false, true, false) => TrOp::DecIVoid(slot),
                    (true, false, true) => TrOp::IncRefI(slot),
                    (true, true, true) => TrOp::IncRefIVoid(slot),
                    (false, false, true) => TrOp::DecRefI(slot),
                    (false, true, true) => TrOp::DecRefIVoid(slot),
                });
                Some(TrKind::Int)
            }
            TokenKind::Bang => {
                let k = self.compile_expr(expr)?;
                self.truthy(k)?;
                self.ops.push(TrOp::NotI);
                Some(TrKind::Int)
            }
            TokenKind::Minus => {
                let k = self.compile_expr(expr)?;
                match k {
                    TrKind::Int => {
                        self.ops.push(TrOp::NegI);
                        Some(TrKind::Int)
                    }
                    other => {
                        self.note_decline(|| format!("unary minus on {other:?}"));
                        None
                    }
                }
            }
            other => {
                self.note_decline(|| format!("prefix operator {other:?}"));
                None
            }
        }
    }
}
