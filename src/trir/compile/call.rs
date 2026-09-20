//! Compiling a call inside a TRIR body — ADR-0110 §3.3.
//!
//! Two shapes, and which one a call gets is decided entirely by what the
//! compiler can prove about the callee:
//!
//! - **Resolved** (`CallTr`): the callee is a routine this same compile has
//!   already registered with a chunk of its own, so its signature is known
//!   here. The binder is compiled away and a native `is rw` parameter is
//!   passed as a reference to the caller's slot.
//! - **Generic** (`CallGen`): anything else — a forward reference (JSON::Fast's
//!   `nom-ws` calls `nom-comment`, declared eleven lines later), a routine in
//!   another compunit, a builtin, or a cold `die` helper whose body is
//!   arbitrary Raku. The arguments are boxed and the ordinary dispatch takes
//!   it.
//!
//! The generic form is what keeps eligibility from collapsing: every one of
//! `JSON::Fast`'s scanner routines ends in an error helper, and refusing the
//! routine for its cold path would leave the hot loop untyped too.

use super::{Binding, TrirCompiler};
use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::trir::{TrArg, TrCallee, TrInnerCall, TrKind, TrOp};

impl TrirCompiler<'_> {
    /// Compile `name(args)`, answering the kind it leaves on a bank.
    pub(super) fn compile_routine_call(&mut self, name: &str, args: &[Expr]) -> Option<TrKind> {
        if args.len() > u8::MAX as usize || args.iter().any(Self::is_named_arg) {
            self.note_decline(|| format!("call to {name} with a named or spread argument"));
            return None;
        }
        let callee = self.resolve_trir_callee(name, args.len());
        let params = callee.as_ref().map(|(_, _, p)| p.clone());
        let mut plan = Vec::with_capacity(args.len());
        for (i, a) in args.iter().enumerate() {
            // A parameter the callee declares `is rw` takes the variable, not
            // its value. With no resolved signature the callee MIGHT have
            // one, so a named variable is passed as a container either way
            // and read back after the call.
            let wants_ref = match &params {
                Some(ps) => ps.get(i).map(|p| p.is_rw).unwrap_or(false),
                None => true,
            };
            let want_kind = params.as_deref().and_then(|p| p.get(i).map(|p| p.kind));
            match self.compile_call_arg(a, wants_ref, want_kind) {
                Some(arg) => plan.push(arg),
                None => {
                    self.note_decline(|| format!("argument {} of the call to {name}", i + 1));
                    return None;
                }
            }
        }
        let (kind, site_callee, sym) = match callee {
            Some((key, fingerprint, _)) => (
                TrKind::Obj,
                TrCallee::Trir { key, fingerprint },
                crate::symbol::Symbol::intern(name),
            ),
            None => (
                TrKind::Obj,
                TrCallee::Generic,
                crate::symbol::Symbol::intern(name),
            ),
        };
        let idx = self.calls.len() as u32;
        let resolved = matches!(site_callee, TrCallee::Trir { .. });
        self.calls.push(TrInnerCall {
            callee: site_callee,
            name: sym,
            args: plan,
            result: kind,
        });
        self.ops.push(if resolved {
            TrOp::CallTr(idx)
        } else {
            TrOp::CallGen(idx)
        });
        Some(kind)
    }

    /// One argument's supply plan.
    ///
    /// `wants_ref` is whether the callee might WRITE this argument. When it
    /// is set and the argument names one of this frame's own variables, the
    /// variable itself is passed; otherwise the argument is evaluated to a
    /// value like any other.
    fn compile_call_arg(
        &mut self,
        a: &Expr,
        wants_ref: bool,
        want_kind: Option<TrKind>,
    ) -> Option<TrArg> {
        if wants_ref {
            // `f($pos)` — the variable itself.
            if let Expr::Var(n) = a
                && let Some(arg) = self.slot_arg(n)
            {
                return Some(arg);
            }
            // `f(++$pos)` — Raku's `++` yields the container, so this binds
            // the variable too. Emit the increment, then pass the variable.
            if let Expr::Unary { op, expr } = a
                && matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
                && let Expr::Var(n) = expr.as_ref()
                && let Some(arg) = self.slot_arg(n)
            {
                self.compile_unary_sink(op, n)?;
                return Some(arg);
            }
            // `f($pos = EXPR)` — same, after the assignment.
            if let Expr::AssignExpr {
                name,
                expr,
                is_bind: false,
            } = a
                && let Some(arg) = self.slot_arg(name)
            {
                let Binding { slot, kind } = self.binding_of(name)?;
                let got = self.compile_expr(expr)?;
                self.coerce(got, kind)?;
                self.store(slot, kind);
                return Some(arg);
            }
            // Not an lvalue this frame owns. A resolved callee's `is rw`
            // parameter cannot bind a temporary, so decline; a generic
            // callee's might not be `is rw` at all, so a value is right.
            if want_kind.is_some() {
                return None;
            }
        }
        let got = self.compile_expr(a)?;
        let kind = match want_kind {
            Some(k) => {
                self.coerce(got, k)?;
                k
            }
            // A generic callee takes everything boxed.
            None => {
                self.coerce(got, TrKind::Obj)?;
                TrKind::Obj
            }
        };
        Some(TrArg::Value(kind))
    }

    /// The by-variable argument form for a name this frame owns, or `None`.
    fn slot_arg(&self, name: &str) -> Option<TrArg> {
        let Binding { slot, kind } = self.binding_of(name)?;
        Some(match kind {
            _ if self.is_rw_param_slot(slot, kind) => TrArg::Ref(slot),
            TrKind::Int | TrKind::Num => TrArg::Native(slot),
            TrKind::Obj => TrArg::Obj(slot),
        })
    }

    /// Whether `slot` is one of this routine's own native `is rw` parameters,
    /// i.e. already holds a reference rather than a value.
    fn is_rw_param_slot(&self, slot: u16, kind: TrKind) -> bool {
        kind.is_native()
            && self
                .params
                .iter()
                .any(|p| p.is_rw && p.slot == slot && p.kind.is_native())
    }

    /// The callee's key, fingerprint and signature when this compile has
    /// already registered it with a chunk.
    fn resolve_trir_callee(
        &self,
        name: &str,
        arity: usize,
    ) -> Option<(crate::symbol::Symbol, u64, Vec<crate::trir::TrParam>)> {
        let (key, fingerprint) = *self.routines?.get(&(name.to_string(), arity))?;
        let cf = self.fns?.get(&key)?;
        if cf.fingerprint != fingerprint {
            return None;
        }
        let chunk = cf.trir.as_ref()?;
        if chunk.params.len() != arity {
            return None;
        }
        Some((key, fingerprint, chunk.params.clone()))
    }

    /// True for a named argument (`key => v`) or a `|EXPR` spread, neither
    /// of which any TRIR call shape supplies. Mirrors
    /// `Compiler::is_named_arg_expr`.
    fn is_named_arg(e: &Expr) -> bool {
        match e {
            Expr::Binary { op, .. } => *op == TokenKind::FatArrow,
            Expr::Unary { op, .. } => *op == TokenKind::Pipe,
            Expr::Literal(lit) => matches!(lit.view(), crate::value::ValueView::Pair(..)),
            _ => false,
        }
    }
}
