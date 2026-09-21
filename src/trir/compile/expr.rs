//! Expression lowering for [`super::TrirCompiler`].
//!
//! Every arm either proves its operand kinds and emits typed ops, or returns
//! `None` and declines the whole routine. Type inference is deliberately
//! trivial (ADR-0110 §3.2): slot kinds, literal kinds, and typed-op result
//! kinds. No flow-sensitive inference, no speculation.

use super::{Binding, TrirCompiler};
use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::trir::{TrKind, TrOp};
use crate::value::{Value, ValueView};

/// An `nqp::` value op TRIR lowers, with the operand kinds it needs and the
/// kind it yields. `(op, [operand kinds], result)`.
type NqpForm = (&'static [TrKind], TrKind, &'static [TrOp]);

impl TrirCompiler<'_> {
    /// Compile `e` for its value, answering the bank/kind it left it on.
    pub(super) fn compile_expr(&mut self, e: &Expr) -> Option<TrKind> {
        self.nqp_sourced = false;
        match e {
            // Transparent, exactly as the untyped compiler treats it: the
            // marker exists for the junction chain-flattener, not for
            // evaluation.
            Expr::Grouped(inner) => self.compile_expr(inner),
            // `(my int $end = EXPR)` in expression position: the parser wraps
            // the declaration as a statement, and its value is the bound
            // value. JSON::Fast's scanners are written almost entirely this
            // way.
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { .. } => self.compile_var_decl(stmt, true)?,
                _ => None,
            },
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => self.compile_ternary(cond, then_expr, else_expr),
            Expr::Literal(v) => self.compile_literal(v),
            // A bareword term: a type object (`Map`, `NFD`), a constant, a
            // package. It appears as an argument to `nqp::getattr`/`istype`/
            // `create` throughout JSON::Fast.
            Expr::BareWord(name) => {
                if name.starts_with("nqp::") {
                    // A no-paren zero-argument nqp term.
                    return self.compile_nqp_value_op(name, &[]);
                }
                let idx = self.add_const(Value::str(name.clone()));
                self.ops.push(TrOp::LoadBareWord(idx));
                Some(TrKind::Obj)
            }
            Expr::Var(name) => self.compile_var(name),
            // `%result` / `@result`: the same slot the declaration made,
            // keyed by the sigiled name.
            Expr::ArrayLiteral(items) => {
                if items.len() > u16::MAX as usize {
                    return None;
                }
                for it in items {
                    let k = self.compile_expr(it)?;
                    self.coerce(k, TrKind::Obj)?;
                }
                self.ops.push(TrOp::MakeListN(items.len() as u16));
                Some(TrKind::Obj)
            }
            Expr::HashVar(n) => self.compile_sigiled_var('%', n),
            Expr::ArrayVar(n) => self.compile_sigiled_var('@', n),
            // `"at $pos: ..."` — the pieces, concatenated. Every `die` helper
            // in a hand-written scanner is one of these, and refusing them
            // would refuse the routine that raises the error.
            Expr::StringInterpolation(parts) => {
                if parts.len() > u16::MAX as usize {
                    return None;
                }
                for p in parts {
                    let k = self.compile_expr(p)?;
                    self.coerce(k, TrKind::Obj)?;
                }
                self.ops.push(TrOp::ConcatN(parts.len() as u16));
                Some(TrKind::Obj)
            }
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
            other => {
                self.note_decline(|| {
                    let rendered = format!("{other:?}")
                        .split_whitespace()
                        .collect::<Vec<_>>()
                        .join(" ");
                    // Truncate on a char boundary: an `Expr` debug rendering
                    // embeds source text, which need not be ASCII.
                    let cut = rendered
                        .char_indices()
                        .map(|(i, _)| i)
                        .chain(std::iter::once(rendered.len()))
                        .take_while(|i| *i <= 160)
                        .last()
                        .unwrap_or(0);
                    format!("expression {}", &rendered[..cut])
                });
                None
            }
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

    /// Emit a bare `++`/`--` on `name` for its side effect, discarding the
    /// value. Used by a call argument that is an increment
    /// (`nom-ws($text, ++$pos)`), where the callee binds the VARIABLE.
    pub(super) fn compile_unary_sink(&mut self, op: &TokenKind, name: &str) -> Option<()> {
        let e = Expr::Var(name.to_string());
        self.compile_unary(op, &e, true)?;
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

    /// A `%`/`@`-sigiled read of one of this frame's own containers.
    fn compile_sigiled_var(&mut self, sigil: char, bare: &str) -> Option<TrKind> {
        let key = format!("{sigil}{bare}");
        let Some(Binding { slot, kind }) = self.binding_of(&key) else {
            self.note_decline(|| format!("container {key} is not this frame's"));
            return None;
        };
        self.load(slot, kind);
        Some(kind)
    }

    fn compile_var(&mut self, name: &str) -> Option<TrKind> {
        if let Some(Binding { slot, kind }) = self.binding_of(name) {
            self.load(slot, kind);
            return Some(kind);
        }
        // A dynamic variable (`$*ALLOW-JSONC` arrives as `*ALLOW-JSONC`) is
        // a by-name read of the dynamic scope, which no slot can stand in
        // for; read it where it is.
        if let Some(bare) = name.strip_prefix('*')
            && !bare.is_empty()
            && bare.starts_with(|c: char| c.is_alphabetic() || c == '_')
        {
            let idx = self.add_const(Value::str(name.to_string()));
            self.ops.push(TrOp::LoadDynamic(idx));
            return Some(TrKind::Obj);
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
            self.note_decline(|| format!("free variable {name}"));
            return None;
        }
        let idx = self.outer(name);
        self.ops.push(TrOp::LoadOuter(idx));
        Some(TrKind::Obj)
    }

    fn compile_unary(&mut self, op: &TokenKind, expr: &Expr, sink: bool) -> Option<TrKind> {
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

    /// `a ?? b !! c`, and the shared lowering behind `&&`/`||`.
    fn compile_ternary(&mut self, c: &Expr, t: &Expr, e: &Expr) -> Option<TrKind> {
        let ck = self.compile_expr(c)?;
        self.truthy(ck)?;
        let branch_at = self.ops.len();
        self.ops.push(TrOp::JumpIfFalseI(0));
        let tk = self.compile_expr(t)?;
        let jump_end_at = self.ops.len();
        self.ops.push(TrOp::Jump(0));
        let else_at = self.ops.len() as u32;
        let ek = self.compile_expr(e)?;
        // `unify_arms` may have inserted a box before the `then` arm's jump,
        // which moves everything at or past it — including that jump.
        let (unified, shifted) = self.unify_arms(tk, ek, jump_end_at)?;
        let jump_end_at = jump_end_at + shifted;
        let else_at = else_at + shifted as u32;
        let end = self.ops.len() as u32;
        match &mut self.ops[branch_at] {
            TrOp::JumpIfFalseI(x) => *x = else_at,
            _ => return None,
        }
        match &mut self.ops[jump_end_at] {
            TrOp::Jump(x) => *x = end,
            _ => return None,
        }
        Some(unified)
    }

    /// Unbox a value an `nqp::` op just produced, where `iarg` coercion IS
    /// the op's semantics. Leaves anything else alone.
    pub(super) fn narrow_nqp_result(&mut self, kind: TrKind) -> TrKind {
        if kind == TrKind::Obj && self.nqp_sourced {
            self.ops.push(TrOp::UnboxI);
            self.nqp_sourced = false;
            return TrKind::Int;
        }
        kind
    }

    /// Reduce the top of a bank to an int-bank 0/1 truth value.
    ///
    /// Only for kinds whose truth Raku settles without dispatch: a native
    /// number is false at 0. A boxed operand declines, because `.Bool` on one
    /// is a method call.
    pub(super) fn truthy(&mut self, kind: TrKind) -> Option<()> {
        match kind {
            TrKind::Int => Some(()),
            TrKind::Num => {
                self.ops.push(TrOp::NumToInt);
                Some(())
            }
            // A boxed condition's truth is the interpreter's own rule, not
            // one TRIR reproduces: `eval_truthy` is what `JumpIfFalse` uses,
            // including a `.Bool` override and a `Failure` being marked
            // handled.
            TrKind::Obj => {
                self.ops.push(TrOp::TruthyObj);
                Some(())
            }
        }
    }

    /// `a && b` / `a || b`, short-circuiting, on native operands.
    fn compile_short_circuit(&mut self, and: bool, l: &Expr, r: &Expr) -> Option<TrKind> {
        let lk = self.compile_expr(l)?;
        self.truthy(lk)?;
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
        self.truthy(rk)?;
        let end = self.ops.len() as u32;
        match &mut self.ops[jump_at] {
            TrOp::JumpIfFalseKeepI(x) | TrOp::JumpIfTrueKeepI(x) => *x = end,
            _ => return None,
        }
        Some(TrKind::Int)
    }

    fn compile_binary(&mut self, left: &Expr, op: &TokenKind, right: &Expr) -> Option<TrKind> {
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
            _ => {
                self.note_decline(|| format!("operator {op:?} on {want:?}"));
                return None;
            }
        };
        self.ops.extend_from_slice(ops);
        Some(result)
    }

    fn compile_assign(&mut self, name: &str, expr: &Expr) -> Option<TrKind> {
        let Some(Binding { slot, kind }) = self.binding_of(name) else {
            let n = name.to_string();
            self.note_decline(|| format!("assignment to non-local {n}"));
            return None;
        };
        if self.slot_is_readonly_param(slot, kind) {
            let n = name.to_string();
            self.note_decline(|| format!("assignment to the read-only parameter {n}"));
            return None;
        }
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
            // `nqp::ifnull(a, b)` is lazy in `b`: rakudo's idiom installs a
            // fresh store only when there is none, and evaluating both arms
            // would install one over a live store.
            "nqp::ifnull" if args.len() == 2 => {
                let ak = self.compile_expr(&args[0])?;
                self.coerce(ak, TrKind::Obj)?;
                self.ops.push(TrOp::DupObj);
                self.ops.push(TrOp::TruthyDefined);
                let keep_at = self.ops.len();
                self.ops.push(TrOp::JumpIfTrueI(0));
                self.ops.push(TrOp::PopObj);
                let bk = self.compile_expr(&args[1])?;
                self.coerce(bk, TrKind::Obj)?;
                let end = self.ops.len() as u32;
                match &mut self.ops[keep_at] {
                    TrOp::JumpIfTrueI(x) => *x = end,
                    _ => return None,
                }
                Some(TrKind::Obj)
            }
            _ => {
                if name.starts_with("nqp::") {
                    return self.compile_nqp_value_op(name, args);
                }
                self.compile_routine_call(name, args)
            }
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
        self.truthy(ck)?;
        let exit_at = self.ops.len();
        self.ops.push(if while_form {
            TrOp::JumpIfFalseI(0)
        } else {
            TrOp::JumpIfTrueI(0)
        });
        self.compile_expr_sink(body)?;
        self.ops.push(TrOp::Jump(start));
        let end = self.ops.len() as u32;
        // Declining rather than asserting: this is a compile-time pass with a
        // free "no" (#8186's never-panic goal), so an emit the arm below does
        // not recognize costs a speedup, never a crash.
        match &mut self.ops[exit_at] {
            TrOp::JumpIfFalseI(t) | TrOp::JumpIfTrueI(t) => *t = end,
            _ => return None,
        }
        let idx = self.add_const(Value::NIL);
        self.ops.push(TrOp::ConstObj(idx));
        Some(TrKind::Obj)
    }

    fn compile_nqp_if(&mut self, if_form: bool, args: &[Expr]) -> Option<TrKind> {
        let ck = self.compile_expr(&args[0])?;
        self.truthy(ck)?;
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
        let (unified, shifted) = self.unify_arms(then_kind, else_kind, jump_end_at)?;
        let jump_end_at = jump_end_at + shifted;
        let else_at = else_at + shifted as u32;
        let end = self.ops.len() as u32;
        match &mut self.ops[branch_at] {
            TrOp::JumpIfFalseI(t) | TrOp::JumpIfTrueI(t) => *t = else_at,
            _ => return None,
        }
        match &mut self.ops[jump_end_at] {
            TrOp::Jump(t) => *t = end,
            _ => return None,
        }
        Some(unified)
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
        if let Some((want, result, emit)) = nqp_form(op)
            && args.len() == want.len()
        {
            for (a, k) in args.iter().zip(want) {
                let got = self.compile_expr(a)?;
                self.coerce(got, *k)?;
            }
            self.ops.extend_from_slice(emit);
            self.nqp_sourced = true;
            return Some(result);
        }
        // `nqp::const::CCLASS_WORD` and friends are compile-time integers,
        // not ops.
        if let Some(v) = crate::compiler::nqp_forms::nqp_const_value(name) {
            self.ops.push(TrOp::ConstI(v));
            return Some(TrKind::Int);
        }
        // Everything else in the namespace goes through the ordinary
        // implementation with boxed operands. That is not a fallback arm: an
        // `nqp::` op is a primitive either way, and boxing its operands is
        // not what the untyped path's ~211 ns per opcode was spent on.
        let Some(id) = crate::runtime::nqp_op_ids::nqp_op_id(op) else {
            let o = op.to_string();
            self.note_decline(|| format!("unknown nqp op {o}"));
            return None;
        };
        if args.len() > u8::MAX as usize {
            return None;
        }
        for a in args {
            let got = self.compile_expr(a)?;
            self.coerce(got, TrKind::Obj)?;
        }
        self.ops.push(TrOp::NqpOpGen {
            id,
            arity: args.len() as u8,
        });
        self.nqp_sourced = true;
        Some(TrKind::Obj)
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
            _ => return None,
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
    const OII: &[TrKind] = &[TrKind::Obj, TrKind::Int, TrKind::Int];
    const OOI: &[TrKind] = &[TrKind::Obj, TrKind::Obj, TrKind::Int];
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
        "substr" => (OII, Obj, &[TrOp::SubstrS]),
        "eqat" => (OOI, Int, &[TrOp::EqAtS]),
        _ => return None,
    })
}
