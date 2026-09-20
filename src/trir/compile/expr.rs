//! Expression lowering for [`super::TrirCompiler`].
//!
//! Every arm either proves its operand kinds and emits typed ops, or returns
//! `None` and declines the whole routine. Type inference is deliberately
//! trivial (ADR-0110 §3.2): slot kinds, literal kinds, and typed-op result
//! kinds. No flow-sensitive inference, no speculation.

use super::{Binding, TrirCompiler};
use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::trir::{TrKind, TrOp};
use crate::value::{Value, ValueView};

/// An `nqp::` value op TRIR lowers, with the operand kinds it needs and the
/// kind it yields. `(op, [operand kinds], result)`.
type NqpForm = (&'static [TrKind], TrKind, &'static [TrOp]);

impl TrirCompiler {
    /// Compile `e` for its value, answering the bank/kind it left it on.
    pub(super) fn compile_expr(&mut self, e: &Expr) -> Option<TrKind> {
        match e {
            // Transparent, exactly as the untyped compiler treats it: the
            // marker exists for the junction chain-flattener, not for
            // evaluation.
            Expr::Grouped(inner) => self.compile_expr(inner),
            Expr::Literal(v) => self.compile_literal(v),
            Expr::Var(name) => self.compile_var(name),
            Expr::Unary { op, expr } => self.compile_unary(op, expr, false),
            Expr::Binary { left, op, right } => self.compile_binary(left, op, right),
            Expr::Call { name, args } => self.compile_call(&name.resolve(), args),
            Expr::AssignExpr {
                name,
                expr,
                is_bind,
            } => {
                if *is_bind {
                    return None;
                }
                self.compile_assign(name, expr)
            }
            _ => None,
        }
    }

    /// Compile `e` in sink (statement) position: the value is discarded.
    ///
    /// Exists for the one shape that dominates a scanner loop body — a bare
    /// `++$pos` — where emitting the value-yielding form and popping it costs
    /// two extra instructions per ITERATION.
    pub(super) fn compile_expr_sink(&mut self, e: &Expr) -> Option<()> {
        if let Expr::Grouped(inner) = e {
            return self.compile_expr_sink(inner);
        }
        if let Expr::Unary { op, expr } = e
            && matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
        {
            self.compile_unary(op, expr, true)?;
            return Some(());
        }
        let kind = self.compile_expr(e)?;
        self.drop_top(kind);
        Some(())
    }

    fn compile_literal(&mut self, v: &Value) -> Option<TrKind> {
        match v.view() {
            ValueView::Int(i) => {
                self.ops.push(TrOp::ConstI(i));
                Some(TrKind::Int)
            }
            ValueView::Num(n) => {
                self.ops.push(TrOp::ConstI(n.to_bits() as i64));
                Some(TrKind::Num)
            }
            ValueView::Str(_) | ValueView::Bool(_) => {
                let idx = self.add_const(v.clone());
                self.ops.push(TrOp::ConstObj(idx));
                Some(TrKind::Obj)
            }
            _ => None,
        }
    }

    fn compile_var(&mut self, name: &str) -> Option<TrKind> {
        if let Some(Binding { slot, kind }) = self.binding_of(name) {
            self.load(slot, kind);
            return Some(kind);
        }
        // A free variable: resolved once per invocation rather than per
        // access (ADR-0110 §3.1). Only a plain, unqualified, sigil-less
        // scalar name reaches here — anything with a `::`, a sigil or a
        // twigil is a package/dynamic/pseudo-package access, which TRIR does
        // not serve.
        if name.is_empty()
            || name.contains("::")
            || name.starts_with(['$', '@', '%', '&', '*', '?', '!', '.', '='])
        {
            return None;
        }
        let idx = self.outer(name);
        self.ops.push(TrOp::LoadOuter(idx));
        Some(TrKind::Obj)
    }

    fn compile_unary(&mut self, op: &TokenKind, expr: &Expr, sink: bool) -> Option<TrKind> {
        match op {
            TokenKind::PlusPlus | TokenKind::MinusMinus => {
                let Expr::Var(name) = expr else { return None };
                let Binding { slot, kind } = self.binding_of(name)?;
                if kind != TrKind::Int {
                    return None;
                }
                let up = *op == TokenKind::PlusPlus;
                self.ops.push(match (up, sink) {
                    (true, false) => TrOp::IncI(slot),
                    (true, true) => TrOp::IncIVoid(slot),
                    (false, false) => TrOp::DecI(slot),
                    (false, true) => TrOp::DecIVoid(slot),
                });
                Some(TrKind::Int)
            }
            TokenKind::Minus => {
                let k = self.compile_expr(expr)?;
                match k {
                    TrKind::Int => {
                        self.ops.push(TrOp::NegI);
                        Some(TrKind::Int)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn compile_binary(&mut self, left: &Expr, op: &TokenKind, right: &Expr) -> Option<TrKind> {
        // Only arithmetic and comparison on operands the compiler already
        // proved native. A boxed operand declines: `+` on two boxed values is
        // full Raku multi-dispatch (a user `infix:<+>` may override it), and
        // reproducing that is not TRIR's job.
        let lk = self.compile_expr(left)?;
        if !lk.is_native() {
            return None;
        }
        let rk = self.compile_expr(right)?;
        if !rk.is_native() {
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
            (TrKind::Int, TokenKind::Percent) => (&[TrOp::ModI], TrKind::Int),
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
            // deliberately absent here.
            _ => return None,
        };
        self.ops.extend_from_slice(ops);
        Some(result)
    }

    fn compile_assign(&mut self, name: &str, expr: &Expr) -> Option<TrKind> {
        let Binding { slot, kind } = self.binding_of(name)?;
        let got = self.compile_expr(expr)?;
        self.coerce(got, kind)?;
        self.store(slot, kind);
        self.load(slot, kind);
        Some(kind)
    }

    fn compile_call(&mut self, name: &str, args: &[Expr]) -> Option<TrKind> {
        match name {
            "nqp::stmts" => self.compile_nqp_stmts(args),
            "nqp::while" | "nqp::until" if args.len() == 2 => {
                self.compile_nqp_loop(name == "nqp::while", &args[0], &args[1])
            }
            "nqp::if" | "nqp::unless" if args.len() == 2 || args.len() == 3 => {
                self.compile_nqp_if(name == "nqp::if", args)
            }
            _ => self.compile_nqp_value_op(name, args),
        }
    }

    fn compile_nqp_stmts(&mut self, args: &[Expr]) -> Option<TrKind> {
        if args.is_empty() {
            let idx = self.add_const(Value::NIL);
            self.ops.push(TrOp::ConstObj(idx));
            return Some(TrKind::Obj);
        }
        for a in &args[..args.len() - 1] {
            self.compile_expr_sink(a)?;
        }
        self.compile_expr(&args[args.len() - 1])
    }

    fn compile_nqp_loop(&mut self, while_form: bool, cond: &Expr, body: &Expr) -> Option<TrKind> {
        let start = self.ops.len() as u32;
        let ck = self.compile_expr(cond)?;
        if ck != TrKind::Int {
            return None;
        }
        let exit_at = self.ops.len();
        self.ops.push(if while_form {
            TrOp::JumpIfFalseI(0)
        } else {
            TrOp::JumpIfTrueI(0)
        });
        self.compile_expr_sink(body)?;
        self.ops.push(TrOp::Jump(start));
        let end = self.ops.len() as u32;
        match &mut self.ops[exit_at] {
            TrOp::JumpIfFalseI(t) | TrOp::JumpIfTrueI(t) => *t = end,
            _ => unreachable!("the exit jump was just emitted"),
        }
        let idx = self.add_const(Value::NIL);
        self.ops.push(TrOp::ConstObj(idx));
        Some(TrKind::Obj)
    }

    fn compile_nqp_if(&mut self, if_form: bool, args: &[Expr]) -> Option<TrKind> {
        let ck = self.compile_expr(&args[0])?;
        if ck != TrKind::Int {
            return None;
        }
        let branch_at = self.ops.len();
        self.ops.push(if if_form {
            TrOp::JumpIfFalseI(0)
        } else {
            TrOp::JumpIfTrueI(0)
        });
        let then_kind = self.compile_expr(&args[1])?;
        let jump_end_at = self.ops.len();
        self.ops.push(TrOp::Jump(0));
        let else_at = self.ops.len() as u32;
        let else_kind = match args.get(2) {
            Some(e) => self.compile_expr(e)?,
            None => {
                let idx = self.add_const(Value::NIL);
                self.ops.push(TrOp::ConstObj(idx));
                TrKind::Obj
            }
        };
        // Both arms must leave the same kind on the same bank. A mismatch
        // would need the `then` arm boxed BEFORE its jump, which Stage 1
        // declines rather than patching after the fact.
        if then_kind != else_kind {
            return None;
        }
        let end = self.ops.len() as u32;
        match &mut self.ops[branch_at] {
            TrOp::JumpIfFalseI(t) | TrOp::JumpIfTrueI(t) => *t = else_at,
            _ => unreachable!("the branch was just emitted"),
        }
        match &mut self.ops[jump_end_at] {
            TrOp::Jump(t) => *t = end,
            _ => unreachable!("the jump was just emitted"),
        }
        Some(then_kind)
    }

    /// The `nqp::` VALUE ops TRIR lowers to typed instructions.
    fn compile_nqp_value_op(&mut self, name: &str, args: &[Expr]) -> Option<TrKind> {
        let op = name.strip_prefix("nqp::")?;
        // The fused, operand-direct string/list reads: their first operand is
        // a boxed value read straight out of a slot or an outer cell, so the
        // generic form's `Value::clone` (an atomic refcount pair per
        // execution) is avoidable entirely. `nom-ws`'s loop is exactly this.
        if args.len() == 2
            && let Some(fused) = self.try_fused_two(op, &args[0])
        {
            let ik = self.compile_expr(&args[1])?;
            if ik != TrKind::Int {
                return None;
            }
            self.ops.push(fused);
            return Some(TrKind::Int);
        }
        if op == "chars"
            && args.len() == 1
            && let Expr::Var(n) = &args[0]
            && let Some(Binding { slot, kind }) = self.binding_of(n)
            && kind == TrKind::Obj
        {
            self.ops.push(TrOp::CharsLocal(slot));
            return Some(TrKind::Int);
        }
        let form = nqp_form(op)?;
        let (want, result, emit) = form;
        if args.len() != want.len() {
            return None;
        }
        for (a, k) in args.iter().zip(want) {
            let got = self.compile_expr(a)?;
            self.coerce(got, *k)?;
        }
        self.ops.extend_from_slice(emit);
        Some(result)
    }

    /// The operand-direct form of a two-operand string/list read whose first
    /// argument is a plain, never-reassigned boxed slot or an outer lexical.
    fn try_fused_two(&mut self, op: &str, first: &Expr) -> Option<TrOp> {
        let Expr::Var(n) = first else { return None };
        let make: fn(u16) -> TrOp = match op {
            "ordat" => TrOp::OrdAtLocal,
            "atpos_i" => TrOp::AtPosILocal,
            _ => return None,
        };
        let outer_make: fn(u16) -> TrOp = match op {
            "ordat" => TrOp::OrdAtOuter,
            "atpos_i" => TrOp::AtPosIOuter,
            _ => unreachable!("the match above admitted only these two"),
        };
        match self.binding_of(n) {
            Some(Binding {
                slot,
                kind: TrKind::Obj,
            }) => {
                // The per-frame character memo assumes the slot's value does
                // not change under it.
                if self.obj_slot_written(slot) {
                    return None;
                }
                Some(make(slot))
            }
            Some(_) => None,
            None => {
                if n.is_empty() || n.contains("::") || n.starts_with(['$', '@', '%', '&', '*']) {
                    return None;
                }
                Some(outer_make(self.outer(n)))
            }
        }
    }
}

/// The typed lowering of one `nqp::` value op: operand kinds, result kind,
/// and the instructions to emit. Ops absent from this table decline.
fn nqp_form(op: &str) -> Option<NqpForm> {
    use TrKind::{Int, Num, Obj};
    const I2: &[TrKind] = &[TrKind::Int, TrKind::Int];
    const I1: &[TrKind] = &[TrKind::Int];
    const N2: &[TrKind] = &[TrKind::Num, TrKind::Num];
    const O1: &[TrKind] = &[TrKind::Obj];
    const OI: &[TrKind] = &[TrKind::Obj, TrKind::Int];
    Some(match op {
        "add_i" => (I2, Int, &[TrOp::AddI]),
        "sub_i" => (I2, Int, &[TrOp::SubI]),
        "mul_i" => (I2, Int, &[TrOp::MulI]),
        "div_i" => (I2, Int, &[TrOp::DivI]),
        "mod_i" => (I2, Int, &[TrOp::ModI]),
        "neg_i" => (I1, Int, &[TrOp::NegI]),
        "bitand_i" => (I2, Int, &[TrOp::BitAndI]),
        "bitor_i" => (I2, Int, &[TrOp::BitOrI]),
        "bitxor_i" => (I2, Int, &[TrOp::BitXorI]),
        "bitshiftl_i" => (I2, Int, &[TrOp::ShlI]),
        "bitshiftr_i" => (I2, Int, &[TrOp::ShrI]),
        "iseq_i" => (I2, Int, &[TrOp::EqI]),
        "isne_i" => (I2, Int, &[TrOp::NeI]),
        "islt_i" => (I2, Int, &[TrOp::LtI]),
        "isle_i" => (I2, Int, &[TrOp::LeI]),
        "isgt_i" => (I2, Int, &[TrOp::GtI]),
        "isge_i" => (I2, Int, &[TrOp::GeI]),
        "not_i" => (I1, Int, &[TrOp::NotI]),
        "add_n" => (N2, Num, &[TrOp::AddN]),
        "sub_n" => (N2, Num, &[TrOp::SubN]),
        "mul_n" => (N2, Num, &[TrOp::MulN]),
        "div_n" => (N2, Num, &[TrOp::DivN]),
        "iseq_n" => (N2, Int, &[TrOp::EqN]),
        "islt_n" => (N2, Int, &[TrOp::LtN]),
        "isle_n" => (N2, Int, &[TrOp::LeN]),
        "isgt_n" => (N2, Int, &[TrOp::GtN]),
        "isge_n" => (N2, Int, &[TrOp::GeN]),
        "box_i" => (I1, Obj, &[TrOp::BoxI]),
        "unbox_i" => (O1, Int, &[TrOp::UnboxI]),
        "chars" => (O1, Int, &[TrOp::CharsS]),
        "ordat" => (OI, Int, &[TrOp::OrdAt]),
        "atpos_i" => (OI, Int, &[TrOp::AtPosI]),
        _ => return None,
    })
}
