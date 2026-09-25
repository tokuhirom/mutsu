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

impl TrirCompiler<'_> {
    /// Compile `e` for its value, answering the bank/kind it left it on.
    pub(super) fn compile_expr(&mut self, e: &Expr) -> Option<TrKind> {
        self.nqp_sourced = false;
        self.nqp_int_result = false;
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
                // A block run for its value — `"... {nqp::chr($o).raku} ..."`
                // in every scanner's error message. A bare block sees the
                // enclosing lexicals and its own `my`s are its own, so it is
                // its statements in a nested scope.
                Stmt::Block(stmts) => {
                    let saved = self.locals.clone();
                    let last = self.compile_body(stmts, false);
                    self.locals = saved;
                    match last? {
                        Some(kind) => Some(kind),
                        None => {
                            let idx = self.add_const(Value::NIL);
                            self.ops.push(TrOp::ConstObj(idx));
                            Some(TrKind::Obj)
                        }
                    }
                }
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
                // A sigilless parameter (`\codes`) is read as a bareword. It
                // may be bound to the caller's CONTAINER, which a slot holding
                // its value can stand in for only where the value is all that
                // is used: as an `nqp::` op's operand. Handed on to a routine
                // (`leaf(c)`), the container itself would have to travel.
                if let Some(Binding { slot, kind }) =
                    self.binding_of(&super::params::sigilless_key(name))
                {
                    if !std::mem::take(&mut self.nqp_operand) {
                        let n = name.clone();
                        self.note_decline(|| {
                            format!("sigilless {n} used other than as an nqp:: operand")
                        });
                        return None;
                    }
                    self.load(slot, kind);
                    return Some(kind);
                }
                // A no-paren call to an inlined inner sub.
                if self.inline_subs.contains_key(name.as_str()) {
                    return self.compile_inline_call(name, &[]);
                }
                self.ops.push(TrOp::ClassOperand(Box::new(
                    crate::trir::class_operand::ClassOperandSite::term(
                        crate::symbol::Symbol::intern(name),
                    ),
                )));
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
            Expr::MethodCall {
                target,
                name,
                args,
                modifier: None,
                quoted: false,
            } => self.compile_method_call(target, &name.resolve(), args),
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
        // A control form in sink position leaves nothing, rather than a
        // value on each arm that the join then drops: the arms are sunk too.
        if let Expr::Call { name, args } = e {
            let name = name.resolve();
            match name.as_str() {
                "nqp::if" | "nqp::unless" if args.len() == 2 || args.len() == 3 => {
                    return self.compile_nqp_if_sink(name == "nqp::if", args);
                }
                "nqp::stmts" => {
                    for a in args.iter() {
                        self.compile_expr_sink(a)?;
                    }
                    return Some(());
                }
                _ => {}
            }
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
            // A type object is immutable, so sharing the constant is exact:
            // ADR-0115's folded CORE type names arrive here.
            ValueView::Str(_) | ValueView::Bool(_) | ValueView::Package(_) => {
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
            if kind == TrKind::Obj && self.nqp_bound.contains(&slot) {
                self.nqp_sourced = true;
            }
            return Some(kind);
        }
        // An inlined inner sub's own topic, match and error variables are its
        // own, not the enclosing routine's; no slot stands in for them.
        if !self.inline_stack.is_empty() && matches!(name, "_" | "/" | "!" | "¢") {
            self.note_decline(|| format!("${name} inside an inlined inner sub"));
            return None;
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
        self.check_not_bound(slot, kind, name)?;
        let got = self.compile_expr(expr)?;
        let tn = self.native_type_name(slot);
        self.coerce_store(got, kind, tn)?;
        self.store(slot, kind);
        self.load(slot, kind);
        Some(kind)
    }

    fn compile_call(&mut self, name: &str, args: &[Expr]) -> Option<TrKind> {
        match name {
            // Lexotic, not a call: see `ret.rs`.
            "return" => self.compile_return(args),
            "nqp::stmts" => self.compile_nqp_stmts(args),
            "nqp::while" | "nqp::until" if args.len() == 2 => {
                self.compile_nqp_loop(name == "nqp::while", &args[0], &args[1])
            }
            "nqp::repeat_while" | "nqp::repeat_until" if args.len() == 2 => {
                self.compile_nqp_repeat_loop(name == "nqp::repeat_while", &args[0], &args[1])
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
                    // An empty-parens call (`nqp::list()`) carries the
                    // parser's Test call-site marker, which no nqp op takes.
                    if args.iter().any(super::inline::is_callsite_marker) {
                        let args: Vec<Expr> = args
                            .iter()
                            .filter(|a| !super::inline::is_callsite_marker(a))
                            .cloned()
                            .collect();
                        return self.compile_nqp_value_op(name, &args);
                    }
                    return self.compile_nqp_value_op(name, args);
                }
                if self.inline_subs.contains_key(name) {
                    return self.compile_inline_call(name, args);
                }
                self.compile_routine_call(name, args)
            }
        }
    }
}
