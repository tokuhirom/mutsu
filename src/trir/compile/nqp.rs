//! `nqp::` forms in a TRIR body: the control-flow forms lowered to jumps,
//! and the value ops lowered to typed instructions or `NqpOpGen`.

use super::{Binding, TrirCompiler};
use crate::ast::Expr;
use crate::trir::{TrKind, TrOp};
use crate::value::Value;

/// An `nqp::` value op TRIR lowers, with the operand kinds it needs and the
/// kind it yields. `(op, [operand kinds], result)`.
type NqpForm = (&'static [TrKind], TrKind, &'static [TrOp]);

impl TrirCompiler<'_> {
    pub(super) fn compile_nqp_stmts(&mut self, args: &[Expr]) -> Option<TrKind> {
        if args.is_empty() {
            let idx = self.add_const(Value::NIL);
            self.ops.push(TrOp::ConstObj(idx));
            return Some(TrKind::Obj);
        }
        for a in &args[..args.len() - 1] {
            self.compile_expr_sink(a)?;
        }
        self.compile_expr_tail(&args[args.len() - 1])
    }

    /// A sunk `nqp::while` / `nqp::until`: the loop leaves nothing. In value
    /// position rakudo yields a lazy Seq of the body values, which TRIR does
    /// not build, so only sink and tail positions reach here (#9415).
    pub(super) fn compile_nqp_loop(
        &mut self,
        while_form: bool,
        cond: &Expr,
        body: &Expr,
    ) -> Option<()> {
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
        Some(())
    }

    /// `nqp::repeat_while` / `nqp::repeat_until`, sunk: the body runs before
    /// the first test, so the loop is entered at the body and the condition
    /// jumps back to it.
    pub(super) fn compile_nqp_repeat_loop(
        &mut self,
        while_form: bool,
        cond: &Expr,
        body: &Expr,
    ) -> Option<()> {
        let start = self.ops.len() as u32;
        self.compile_expr_sink(body)?;
        let ck = self.compile_expr(cond)?;
        self.truthy(ck)?;
        self.ops.push(if while_form {
            TrOp::JumpIfTrueI(start)
        } else {
            TrOp::JumpIfFalseI(start)
        });
        Some(())
    }

    /// Compile a sunk `nqp::` loop form; `None` when `e` is not one.
    pub(super) fn compile_nqp_loop_sink(&mut self, e: &Expr) -> Option<Option<()>> {
        let Expr::Call { name, args } = e else {
            return None;
        };
        if args.len() != 2 {
            return None;
        }
        Some(match name.resolve().as_str() {
            "nqp::while" => self.compile_nqp_loop(true, &args[0], &args[1]),
            "nqp::until" => self.compile_nqp_loop(false, &args[0], &args[1]),
            "nqp::repeat_while" => self.compile_nqp_repeat_loop(true, &args[0], &args[1]),
            "nqp::repeat_until" => self.compile_nqp_repeat_loop(false, &args[0], &args[1]),
            _ => return None,
        })
    }

    /// Compile an expression whose loop form, if it is one, yields Nil: a
    /// body's tail, or an `nqp::stmts` operand (a void loop in rakudo). The
    /// bytecode compiler's `with_stmt_root` draws the same line.
    pub(super) fn compile_expr_tail(&mut self, e: &Expr) -> Option<TrKind> {
        if let Some(done) = self.compile_nqp_loop_sink(e) {
            done?;
            let idx = self.add_const(Value::NIL);
            self.ops.push(TrOp::ConstObj(idx));
            return Some(TrKind::Obj);
        }
        self.compile_expr(e)
    }

    pub(super) fn compile_nqp_if(&mut self, if_form: bool, args: &[Expr]) -> Option<TrKind> {
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

    /// `nqp::if`/`nqp::unless` whose value is discarded: both arms are
    /// compiled for effect, so neither leaves a value for the join to drop,
    /// and the arms need no common kind.
    pub(super) fn compile_nqp_if_sink(&mut self, if_form: bool, args: &[Expr]) -> Option<()> {
        let ck = self.compile_expr(&args[0])?;
        self.truthy(ck)?;
        let branch_at = self.ops.len();
        self.ops.push(if if_form {
            TrOp::JumpIfFalseI(0)
        } else {
            TrOp::JumpIfTrueI(0)
        });
        self.compile_expr_sink(&args[1])?;
        let skip_to = match args.get(2) {
            Some(else_expr) => {
                let jump_end_at = self.ops.len();
                self.ops.push(TrOp::Jump(0));
                let else_at = self.ops.len() as u32;
                self.compile_expr_sink(else_expr)?;
                let end = self.ops.len() as u32;
                match &mut self.ops[jump_end_at] {
                    TrOp::Jump(t) => *t = end,
                    _ => return None,
                }
                else_at
            }
            None => self.ops.len() as u32,
        };
        match &mut self.ops[branch_at] {
            TrOp::JumpIfFalseI(t) | TrOp::JumpIfTrueI(t) => *t = skip_to,
            _ => return None,
        }
        Some(())
    }

    /// The `nqp::` VALUE ops TRIR lowers to typed instructions.
    pub(super) fn compile_nqp_value_op(&mut self, name: &str, args: &[Expr]) -> Option<TrKind> {
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
                let got = self.compile_nqp_operand(a)?;
                self.coerce(got, *k)?;
            }
            self.ops.extend_from_slice(emit);
            self.nqp_sourced = true;
            return Some(result);
        }
        if let Some(kind) = self.try_typed_list_op(op, args) {
            return kind;
        }
        // `nqp::const::CCLASS_WORD` and friends are compile-time integers,
        // not ops.
        if let Some(v) = crate::compiler::nqp_forms::nqp_const_value(name) {
            self.ops.push(TrOp::ConstI(v));
            return Some(TrKind::Int);
        }
        if let Some(kind) = self.try_attr_op(op, args) {
            return kind;
        }
        // `nqp::iscont` asks about its operand's CONTAINER, which the main
        // compiler supplies by compiling the operand as `.VAR`
        // (`try_compile_nqp_form`); a TRIR operand is always the bare value,
        // so it would answer 0 for every variable (#9346). Leave the routine
        // to the bytecode path.
        if op == "iscont" {
            self.note_decline(|| "nqp::iscont needs its operand's container".to_string());
            return None;
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
            let got = self.compile_nqp_operand(a)?;
            self.coerce(got, TrKind::Obj)?;
        }
        self.ops.push(TrOp::NqpOpGen {
            id,
            arity: args.len() as u8,
        });
        self.nqp_sourced = true;
        self.nqp_int_result = nqp_op_returns_int(op);
        Some(TrKind::Obj)
    }

    /// The typed list ops of ADR-0112 Step 3: `elems`, `shift_i`, `push_i` on
    /// a boxed list operand, reached without the dispatch table. `None` means
    /// "not one of these shapes" and nothing has been emitted; `Some(r)` is
    /// the compile's answer (`r == None` declines, as any operand may).
    fn try_typed_list_op(&mut self, op: &str, args: &[Expr]) -> Option<Option<TrKind>> {
        let r = match (op, args.len()) {
            ("elems", 1) => (|| {
                let at = self.ops.len();
                let got = self.compile_nqp_operand(&args[0])?;
                self.coerce(got, TrKind::Obj)?;
                let op = match self.take_sole_load_obj(at) {
                    Some(n) => TrOp::ElemsLocal(n),
                    None => TrOp::ElemsO,
                };
                self.ops.push(op);
                Some(TrKind::Int)
            })(),
            ("shift_i", 1) => (|| {
                let at = self.ops.len();
                let got = self.compile_nqp_operand(&args[0])?;
                self.coerce(got, TrKind::Obj)?;
                let op = match self.take_sole_load_obj(at) {
                    Some(n) => TrOp::ShiftILocal(n),
                    None => TrOp::ShiftIO,
                };
                self.ops.push(op);
                Some(TrKind::Int)
            })(),
            ("push_i", 2) => (|| {
                let at = self.ops.len();
                let got = self.compile_nqp_operand(&args[0])?;
                self.coerce(got, TrKind::Obj)?;
                let target = match self.ops[at..] {
                    [TrOp::LoadObj(n)] => Some(n),
                    _ => None,
                };
                let val_at = self.ops.len();
                let val = self.compile_nqp_operand(&args[1])?;
                // The slot is read when the push runs, after the value is
                // computed rather than before it; that is the same answer
                // only when computing the value cannot write the slot.
                let target =
                    target.filter(|&n| val == TrKind::Int && self.ops_keep_slot(val_at, n));
                if let Some(n) = target {
                    // `ops_keep_slot` also proved the value's ops hold no
                    // jump, so nothing refers to their indices.
                    self.ops.remove(at);
                    self.ops.push(TrOp::PushILocal(n));
                } else if val == TrKind::Int {
                    self.ops.push(TrOp::PushIO);
                } else {
                    // Only a native int is the typed shape; anything else
                    // keeps the dispatch table's own coercion.
                    self.coerce(val, TrKind::Obj)?;
                    let id = crate::runtime::nqp_op_ids::nqp_op_id("push_i")?;
                    self.ops.push(TrOp::NqpOpGen { id, arity: 2 });
                }
                Some(TrKind::Obj)
            })(),
            _ => return None,
        };
        self.nqp_sourced = r.is_some();
        Some(r)
    }

    /// When the ops emitted since `at` are exactly one `LoadObj(n)`, remove
    /// it and answer `n`, so the caller can emit the operand-direct form of
    /// its op (ADR-0116 D2.1). One op means no jump can target inside it.
    fn take_sole_load_obj(&mut self, at: usize) -> Option<u16> {
        match self.ops[at..] {
            [TrOp::LoadObj(n)] => {
                self.ops.truncate(at);
                Some(n)
            }
            _ => None,
        }
    }

    /// Whether the ops emitted since `at` are straight-line code that can
    /// neither store into boxed slot `n` nor run code that might: no
    /// `StoreObj(n)`, no call and no jump.
    fn ops_keep_slot(&self, at: usize, n: u16) -> bool {
        self.ops[at..].iter().all(|op| match op {
            TrOp::StoreObj(m) => *m != n,
            TrOp::CallTr(_)
            | TrOp::CallGen(_)
            | TrOp::MethodGen(_)
            | TrOp::Jump(_)
            | TrOp::JumpIfFalseI(_)
            | TrOp::JumpIfTrueI(_)
            | TrOp::JumpIfFalseKeepI(_)
            | TrOp::JumpIfTrueKeepI(_) => false,
            _ => true,
        })
    }

    /// Compile one operand of an `nqp::` op. A sigilless parameter is
    /// admitted exactly here (see the `BareWord` arm of `compile_expr`).
    pub(super) fn compile_nqp_operand(&mut self, a: &Expr) -> Option<TrKind> {
        self.nqp_operand = matches!(a, Expr::BareWord(_));
        let got = self.compile_expr(a);
        self.nqp_operand = false;
        got
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
                // The operand-direct form reads the slot at the op, so the
                // slot must still hold the value the operand expression named.
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
    use TrKind::{Int, Obj};
    const I2: &[TrKind] = &[TrKind::Int, TrKind::Int];
    const I1: &[TrKind] = &[TrKind::Int];
    const O1: &[TrKind] = &[TrKind::Obj];
    const OI: &[TrKind] = &[TrKind::Obj, TrKind::Int];
    const OII: &[TrKind] = &[TrKind::Obj, TrKind::Int, TrKind::Int];
    const OOI: &[TrKind] = &[TrKind::Obj, TrKind::Obj, TrKind::Int];
    if let Some(p) = crate::runtime::nqp_pure::by_name(op) {
        return pure_form(p);
    }
    Some(match op {
        // Not pure (they can raise on a zero divisor), so not `NqpPure`s.
        "div_i" => (I2, Int, &[TrOp::DivI]),
        "mod_i" => (I2, Int, &[TrOp::ModI]),
        "box_i" => (I1, Obj, &[TrOp::BoxI]),
        "chars" => (O1, Int, &[TrOp::CharsS]),
        "ordat" => (OI, Int, &[TrOp::OrdAt]),
        "atpos_i" => (OI, Int, &[TrOp::AtPosI]),
        "substr" => (OII, Obj, &[TrOp::SubstrS]),
        "eqat" => (OOI, Int, &[TrOp::EqAtS]),
        _ => return None,
    })
}

/// The typed lowering of a pure op, keyed on the interpreter's own
/// [`crate::runtime::nqp_pure::NqpPure`] rather than a second copy of its
/// names. Every `TrOp` here runs the body in `runtime::nqp_native` (or, for
/// a comparison, the plain comparison `nqp_pure::eval` does), so the two
/// tiers answer alike. A pure op TRIR has no typed form for declines to the
/// boxed `NqpOpGen` path, which calls `nqp_pure::eval` itself.
fn pure_form(p: crate::runtime::nqp_pure::NqpPure) -> Option<NqpForm> {
    use crate::runtime::nqp_pure::NqpPure as P;
    use TrKind::{Int, Num};
    const I2: &[TrKind] = &[TrKind::Int, TrKind::Int];
    const I1: &[TrKind] = &[TrKind::Int];
    const N2: &[TrKind] = &[TrKind::Num, TrKind::Num];
    Some(match p {
        P::AddI => (I2, Int, &[TrOp::AddI]),
        P::SubI => (I2, Int, &[TrOp::SubI]),
        P::MulI => (I2, Int, &[TrOp::MulI]),
        P::NegI => (I1, Int, &[TrOp::NegI]),
        P::BitAndI => (I2, Int, &[TrOp::BitAndI]),
        P::BitOrI => (I2, Int, &[TrOp::BitOrI]),
        P::BitXorI => (I2, Int, &[TrOp::BitXorI]),
        P::ShlI => (I2, Int, &[TrOp::ShlI]),
        P::ShrI => (I2, Int, &[TrOp::ShrI]),
        P::IsEqI => (I2, Int, &[TrOp::EqI]),
        P::IsNeI => (I2, Int, &[TrOp::NeI]),
        P::IsLtI => (I2, Int, &[TrOp::LtI]),
        P::IsLeI => (I2, Int, &[TrOp::LeI]),
        P::IsGtI => (I2, Int, &[TrOp::GtI]),
        P::IsGeI => (I2, Int, &[TrOp::GeI]),
        P::NotI => (I1, Int, &[TrOp::NotI]),
        P::AddN => (N2, Num, &[TrOp::AddN]),
        P::SubN => (N2, Num, &[TrOp::SubN]),
        P::MulN => (N2, Num, &[TrOp::MulN]),
        P::DivN => (N2, Num, &[TrOp::DivN]),
        P::IsEqN => (N2, Int, &[TrOp::EqN]),
        P::IsLtN => (N2, Int, &[TrOp::LtN]),
        P::IsLeN => (N2, Int, &[TrOp::LeN]),
        P::IsGtN => (N2, Int, &[TrOp::GtN]),
        P::IsGeN => (N2, Int, &[TrOp::GeN]),
        P::AbsI | P::BitNegI | P::CmpI | P::NegN | P::AbsN | P::IsNeN | P::CmpN | P::IsNanOrInf => {
            return None;
        }
    })
}

/// Whether `nqp::op` returns a native int, by NQP's own naming: the typed
/// `_i` ops, every `is*` predicate, and the int-valued queries.
pub(super) fn nqp_op_returns_int(op: &str) -> bool {
    op.ends_with("_i")
        || op.starts_with("is")
        || matches!(
            op,
            "eqat"
                | "elems"
                | "chars"
                | "ord"
                | "ordat"
                | "index"
                | "rindex"
                | "findcclass"
                | "findnotcclass"
                | "existskey"
                | "existspos"
                | "eqaddr"
                | "cmp_s"
                | "cmp_n"
        )
}
