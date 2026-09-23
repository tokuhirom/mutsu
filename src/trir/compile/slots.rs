//! Declarations, slot loads and stores, and kind coercion for
//! [`super::TrirCompiler`].

use super::params::{native_kind_of, sized_int_width};
use super::{Binding, TrirCompiler};
use crate::ast::{Expr, Stmt};
use crate::trir::{TrKind, TrOp};
use crate::value::Value;

impl TrirCompiler<'_> {
    /// Compile a `my` declaration. Answers the kind it left on a bank when
    /// `keep` asked for its value, `None` otherwise.
    ///
    /// Shared with the expression form: `(my int $end = EXPR)` is a
    /// `DoStmt(VarDecl)` in value position, which is how JSON::Fast's
    /// scanners are written almost throughout.
    pub(super) fn compile_var_decl(&mut self, stmt: &Stmt, keep: bool) -> Option<Option<TrKind>> {
        let Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic,
            is_export,
            custom_traits,
            where_constraint,
            ..
        } = stmt
        else {
            return None;
        };
        if *is_state
            || *is_our
            || *is_dynamic
            || *is_export
            || where_constraint.is_some()
            || name.starts_with('&')
        {
            let n = name.clone();
            self.note_decline(|| format!("declaration shape {n}"));
            return None;
        }
        // `my %result;` / `my @result;` — a FRESH container per invocation,
        // held in a boxed slot under its sigiled name. Only the empty form:
        // an initializer would be a list/hash construction this does not
        // compile.
        if name.starts_with(['@', '%']) {
            let empty = match expr {
                Expr::Hash(entries) if entries.is_empty() => TrOp::NewHash,
                Expr::Literal(v) => match v.view() {
                    crate::value::ValueView::Array(items, _) if items.is_empty() => TrOp::NewArray,
                    _ => {
                        let n = name.clone();
                        self.note_decline(|| format!("initialized container declaration {n}"));
                        return None;
                    }
                },
                _ => {
                    let n = name.clone();
                    self.note_decline(|| format!("initialized container declaration {n}"));
                    return None;
                }
            };
            self.ops.push(empty);
            let slot = self.alloc(name, TrKind::Obj);
            self.store(slot, TrKind::Obj);
            if keep {
                self.load(slot, TrKind::Obj);
                return Some(Some(TrKind::Obj));
            }
            return Some(None);
        }
        // `__has_initializer` is the parser's own marker for `my T $x = ...`;
        // `__scalar_bind` marks `my $x := ...`, which binds rather than
        // assigns — for a fresh `my` in a TRIR frame the two are the same
        // thing, because nothing else can name the slot. Any other trait
        // declines.
        if custom_traits
            .iter()
            .any(|(t, _)| t != "__has_initializer" && t != "__scalar_bind")
        {
            return None;
        }
        let tc = type_constraint.as_deref();
        let width = tc.and_then(sized_int_width);
        let kind = match native_kind_of(tc) {
            Some(k) => k,
            None if width.is_some() => TrKind::Int,
            None if tc.is_none() || tc == Some("str") => TrKind::Obj,
            None => {
                let t = tc.unwrap_or("").to_string();
                self.note_decline(|| format!("declared type {t}"));
                return None;
            }
        };
        let init = self.compile_expr(expr)?;
        let from_nqp = self.nqp_sourced;
        self.coerce_store(init, kind, width.map_or("int", |w| w.2))?;
        let slot = self.alloc(name, kind);
        // A redeclaration (`my` inside a loop body runs once per iteration)
        // gets a fresh slot, so these facts are per slot, not per name.
        if let Some(w) = width {
            self.sized.insert(slot, w);
        }
        let bound = custom_traits.iter().any(|(t, _)| t == "__scalar_bind");
        if kind == TrKind::Obj && bound && tc.is_none() && from_nqp {
            self.nqp_bound.insert(slot);
        }
        self.store(slot, kind);
        if kind == TrKind::Obj && self.nqp_bound.contains(&slot) {
            // The declaration's own store is the binding, not a later write.
            self.obj_written[slot as usize] = false;
        }
        if keep {
            // A declaration in value position yields the bound value; re-read
            // it rather than duplicating a bank.
            self.load(slot, kind);
            return Some(Some(kind));
        }
        Some(None)
    }

    pub(super) fn drop_top(&mut self, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::PopI),
            TrKind::Obj => self.ops.push(TrOp::PopObj),
        }
    }

    /// Whether a native slot holds a REFERENCE rather than a value — true
    /// exactly for this routine's own `is rw` native parameters, which are
    /// bound to the caller's slot (ADR-0110 §3.3).
    /// Whether `slot` holds one of this routine's READ-ONLY parameters.
    ///
    /// A Raku parameter is readonly unless declared `is rw`, and writing one
    /// is `X::Assignment::RO` — which the general binder raises and a typed
    /// slot store cannot. TRIR therefore declines the routine and lets the
    /// untyped path raise it, rather than accepting `sub f($x) { $x = 1 }` and
    /// quietly writing the slot (roast's `S06-traits/misc.t` pins exactly
    /// that: the assignment form must die, and it stopped dying).
    ///
    /// The kind is part of the identity: the native and boxed banks number
    /// their slots independently, so native slot 0 and boxed slot 0 are
    /// different bindings.
    pub(super) fn slot_is_readonly_param(&self, slot: u16, kind: TrKind) -> bool {
        self.params
            .iter()
            .any(|p| !p.is_rw && p.slot == slot && p.kind == kind)
    }

    pub(super) fn slot_is_ref(&self, slot: u16, kind: TrKind) -> bool {
        kind.is_native()
            && self
                .params
                .iter()
                .any(|p| p.is_rw && p.kind.is_native() && p.slot == slot)
    }

    pub(super) fn load(&mut self, slot: u16, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num if self.slot_is_ref(slot, kind) => {
                self.ops.push(TrOp::GetRefI(slot))
            }
            TrKind::Int | TrKind::Num => self.ops.push(TrOp::LoadI(slot)),
            TrKind::Obj => self.ops.push(TrOp::LoadObj(slot)),
        }
    }

    pub(super) fn store(&mut self, slot: u16, kind: TrKind) {
        match kind {
            TrKind::Int | TrKind::Num if self.slot_is_ref(slot, kind) => {
                self.ops.push(TrOp::SetRefI(slot))
            }
            TrKind::Int | TrKind::Num => {
                if let Some(&(bits, signed, _)) = self.sized.get(&slot) {
                    self.ops.push(TrOp::WrapI { bits, signed });
                }
                self.ops.push(TrOp::StoreI(slot))
            }
            TrKind::Obj => {
                self.obj_written[slot as usize] = true;
                self.ops.push(TrOp::StoreObj(slot));
            }
        }
    }

    /// Emit the conversion from `have` to `want`, or decline when there is
    /// none TRIR performs without the general binder's coercion rules.
    pub(super) fn coerce(&mut self, have: TrKind, want: TrKind) -> Option<()> {
        match (have, want) {
            (a, b) if a == b => Some(()),
            (TrKind::Int, TrKind::Num) => {
                self.ops.push(TrOp::IntToNum);
                Some(())
            }
            (TrKind::Num, TrKind::Int) => {
                self.ops.push(TrOp::NumToInt);
                Some(())
            }
            (TrKind::Int, TrKind::Obj) => {
                self.ops.push(TrOp::BoxI);
                Some(())
            }
            (TrKind::Num, TrKind::Obj) => {
                self.ops.push(TrOp::BoxN);
                Some(())
            }
            // Unboxing is a checked boundary op, but "is this boxed value an
            // int" is a run-time question, so an implicit narrowing from a
            // boxed expression into a native slot declines instead: TRIR
            // never silently coerces where the general binder would raise.
            (TrKind::Obj, TrKind::Int) if self.nqp_sourced => {
                self.ops.push(TrOp::UnboxI);
                Some(())
            }
            (TrKind::Obj, want) => {
                self.note_decline(|| format!("narrowing a boxed value to {want:?}"));
                None
            }
            // Unreachable: the equality guard above covers both, but the
            // exhaustiveness check does not count guarded arms.
            (TrKind::Int, TrKind::Int) | (TrKind::Num, TrKind::Num) => Some(()),
        }
    }

    /// Decline a write to a slot whose reads were compiled on the strength
    /// of it never being written: an `nqp`-bound boxed slot, whose reads
    /// narrow, and a sized native one, which an increment or an `is rw`
    /// callee would write without the width's wrap.
    pub(super) fn check_not_bound(&mut self, slot: u16, kind: TrKind, name: &str) -> Option<()> {
        if kind == TrKind::Obj && self.nqp_bound.contains(&slot) {
            let n = name.to_string();
            self.note_decline(|| format!("a write to the `:=`-bound {n}"));
            return None;
        }
        Some(())
    }

    /// The declared native integer type of `slot`, for a store's
    /// narrowing and its error message.
    pub(super) fn native_type_name(&self, slot: u16) -> &'static str {
        self.sized.get(&slot).map_or("int", |w| w.2)
    }

    /// [`Self::coerce`] for a value about to be STORED into a slot of kind
    /// `want`. Narrowing a boxed `nqp::` result into a native integer
    /// variable is not `nqp`'s lenient `iarg` coercion but the assignment's
    /// own check, which dies on a type object (`my int $a =
    /// nqp::atpos(...)` of a hole) — so it gets the store's op.
    pub(super) fn coerce_store(
        &mut self,
        have: TrKind,
        want: TrKind,
        type_name: &'static str,
    ) -> Option<()> {
        if have == TrKind::Obj && want == TrKind::Int && self.nqp_sourced {
            let idx = self.add_const(Value::str(type_name.to_string()));
            self.ops.push(TrOp::NarrowStoreI(idx));
            return Some(());
        }
        self.coerce(have, want)
    }

    /// Whether native `slot` was declared with a sized type.
    pub(super) fn slot_is_sized(&self, slot: u16, kind: TrKind) -> bool {
        kind.is_native() && self.sized.contains_key(&slot)
    }

    pub(super) fn binding_of(&self, name: &str) -> Option<Binding> {
        self.locals.get(name).copied()
    }

    /// Whether boxed slot `slot` is ever assigned after its declaration. The
    /// operand-direct string reads memoize the slot's characters for the
    /// frame, which is only sound while nothing rewrites the slot.
    pub(super) fn obj_slot_written(&self, slot: u16) -> bool {
        self.obj_written.get(slot as usize).copied().unwrap_or(true)
    }
}
