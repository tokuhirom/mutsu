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
        self.compile_expr(&args[args.len() - 1])
    }

    pub(super) fn compile_nqp_loop(
        &mut self,
        while_form: bool,
        cond: &Expr,
        body: &Expr,
    ) -> Option<TrKind> {
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
    fn compile_nqp_operand(&mut self, a: &Expr) -> Option<TrKind> {
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

/// Whether `nqp::op` returns a native int, by NQP's own naming: the typed
/// `_i` ops, every `is*` predicate, and the int-valued queries.
fn nqp_op_returns_int(op: &str) -> bool {
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
