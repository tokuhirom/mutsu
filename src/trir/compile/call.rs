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
use crate::trir::{TrArg, TrCallee, TrInnerCall, TrKind, TrLink, TrOp};

impl TrirCompiler<'_> {
    /// Compile `name(args)`, answering the kind it leaves on a bank.
    pub(super) fn compile_routine_call(&mut self, name: &str, args: &[Expr]) -> Option<TrKind> {
        if args.len() > u8::MAX as usize || args.iter().any(Self::is_named_arg) {
            self.note_decline(|| format!("call to {name} with a named or spread argument"));
            return None;
        }
        let callee = self.resolve_trir_callee(name, args.len());
        let params = callee.as_ref().map(|link| link.chunk.params.clone());
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
            Some(link) => (
                TrKind::Obj,
                TrCallee::Trir(link),
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
            // A slot whose reads rely on it never being written, or whose
            // stores must wrap, cannot be handed to a callee that might
            // write it.
            let lvalue = match a {
                Expr::Var(n) => Some(n.as_str()),
                Expr::Unary { expr, .. } => match expr.as_ref() {
                    Expr::Var(n) => Some(n.as_str()),
                    _ => None,
                },
                Expr::AssignExpr { name, .. } => Some(name.as_str()),
                _ => None,
            };
            let fixed = lvalue.and_then(|n| {
                let Binding { slot, kind } = self.binding_of(n)?;
                Some((
                    n.to_string(),
                    self.slot_is_sized(slot, kind),
                    kind == TrKind::Obj && self.nqp_bound.contains(&slot),
                ))
            });
            match fixed {
                Some((n, true, _)) => {
                    self.note_decline(|| format!("sized {n} passed where a callee might write it"));
                    return None;
                }
                // A `:=`-bound value is not a container, so it goes over as
                // the value, exactly as the binding itself would: a callee
                // that writes an `is rw` parameter bound to it dies on the
                // untyped path too. A resolved `is rw` parameter needs a
                // slot reference, which a value is not.
                Some((n, _, true)) => {
                    if want_kind.is_some() {
                        self.note_decline(|| format!("the `:=`-bound {n} passed `is rw`"));
                        return None;
                    }
                }
                _ => {
                    // `f($pos)` — the variable itself.
                    if let Expr::Var(n) = a
                        && let Some(arg) = self.slot_arg(n)
                    {
                        return Some(arg);
                    }
                }
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
                self.check_not_bound(slot, kind, name)?;
                let got = self.compile_expr(expr)?;
                let tn = self.native_type_name(slot);
                self.coerce_store(got, kind, tn)?;
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

    /// The link to the callee when this compile has already registered it
    /// with a chunk.
    fn resolve_trir_callee(&self, name: &str, arity: usize) -> Option<TrLink> {
        let (key, fingerprint) = *self.routines?.get(&(name.to_string(), arity))?;
        let cf = self.fns?.get(&key)?;
        if cf.fingerprint != fingerprint {
            return None;
        }
        let link = TrLink::to(key, cf)?;
        // A `CallTr` binds by copying, with no type test; a callee whose
        // parameters carry a nominal check goes through `CallGen`, whose
        // run-time link binds through `bind_ro_param`.
        if link
            .chunk
            .params
            .iter()
            .any(|p| p.check.is_some() || p.sigilless)
        {
            return None;
        }
        (link.chunk.params.len() == arity).then_some(link)
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
