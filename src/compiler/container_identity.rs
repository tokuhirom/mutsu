//! Container identity: `=:=` and `nqp::eqaddr`.
//!
//! Rakudo defines `infix:<=:=>` as `nqp::eqaddr` of its raw operands, so the
//! two are one primitive: are the operands, *as given* (a `$` variable is its
//! Scalar, an `@` variable its Array, an element its container), the same
//! object? `nqp::eqaddr` is that answer as an int (#17620). The compiler keeps
//! what it knows about each operand's binding and picks the opcode that can
//! decide it; the opcodes are in `vm/vm_comparison_container_ops.rs`.

use super::*;

impl Compiler {
    /// Push whether `left` and `right` are the same container or object, as
    /// a Bool.
    // Cost: O(1) at compile time per operand shape; the emitted opcode's cost
    // is stated on its dispatch arm.
    pub(super) fn compile_container_identity(&mut self, left: &Expr, right: &Expr) {
        // A literal `Nil` operand (`$x =:= Nil`, `@list[$i] =:= Nil`):
        // Nil is a singleton, never a container, so this is a pure value
        // identity check — a read that yields literal Nil (e.g. a `.List`
        // hole element) IS Nil. Emit with no fresh-container flags so the
        // VM falls through to object identity. Two *reads* that both
        // happen to yield Nil stay non-identical via the fresh flags below.
        let left_lit_nil = matches!(left, Expr::Literal(lit) if lit.is_nil());
        let right_lit_nil = matches!(right, Expr::Literal(lit) if lit.is_nil());
        if left_lit_nil || right_lit_nil {
            self.compile_expr(left);
            self.compile_expr(right);
            self.code.emit(OpCode::ContainerEq(0));
            return;
        }
        // A typed readonly parameter (`Int $x`) is bound straight to the
        // argument, so against a bare value (a literal or type object) or
        // another such parameter `=:=` is value identity (#16959).
        let direct = |e: &Expr| matches!(e.peel_parens(), Expr::Var(n) if self.direct_bound_params.contains(n));
        let bare_value = |e: &Expr| matches!(e.peel_parens(), Expr::Literal(_) | Expr::BareWord(_));
        if (direct(left) && (bare_value(right) || direct(right)))
            || (direct(right) && bare_value(left))
        {
            self.compile_expr(left);
            self.compile_expr(right);
            self.code.emit(OpCode::ContainerEq(0));
            return;
        }
        // `EXPR.self` hands back the invocant's *value*, never its
        // container, so pairing it with a named variable is decided by
        // whether that name has a container at all — a runtime fact (`:=`
        // vs `=`), so it goes to `ContainerEqDeconted` rather than being
        // folded here. Checked before the both-named case below, which a
        // method call can never satisfy anyway.
        //
        // A literal or a type-object / term bareword (`$x =:= 1`,
        // `$x =:= IterationEnd`) is a bare value in the same way: it is
        // the same "container" as `$x` only when `$x` was `:=`-bound to it
        // and so owns no Scalar (`my $x = 1; $x =:= 1` is False).
        // An `@`/`%` variable is the Array/Hash itself, never a Scalar, so
        // against a `$` variable it decides the same way: `$x =:= @a` only
        // when `$x` is `:=`-bound to it (`my $w = @a` owns a Scalar).
        let is_aggregate_var = |e: &Expr| matches!(e, Expr::ArrayVar(_) | Expr::HashVar(_));
        let self_decont_name = if self.expr_is_decontainerized_value(left)
            || (is_aggregate_var(left) && matches!(right.peel_parens(), Expr::Var(_)))
        {
            self.container_eq_decont_name(right)
        } else if self.expr_is_decontainerized_value(right)
            || (is_aggregate_var(right) && matches!(left.peel_parens(), Expr::Var(_)))
        {
            self.container_eq_decont_name(left)
        } else {
            None
        };
        if let Some(name) = self_decont_name {
            self.compile_expr(left);
            self.compile_expr(right);
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::ContainerEqDeconted { name_idx });
            return;
        }
        // Resolve variable names from both sides, including through
        // list-indexing patterns like ($foo, "x")[0] which preserves
        // the container of $foo.
        // A sigilless parameter (`\p`) binds to the caller's container, and
        // the binder records which variable it aliases: it is a named
        // operand like a `$` variable, not a bare value.
        let sigilless_name = |e: &Expr| match e.peel_parens() {
            Expr::BareWord(n) if self.sigilless_locals.contains(n) => {
                self.container_eq_decont_name(e)
            }
            _ => None,
        };
        let left_name = Self::resolve_container_var_name(left).or_else(|| sigilless_name(left));
        let right_name = Self::resolve_container_var_name(right).or_else(|| sigilless_name(right));
        if let (Some(ln), Some(rn)) = (&left_name, &right_name) {
            self.compile_expr(left);
            self.compile_expr(right);
            let left_idx = self.code.add_constant(Value::str(ln.clone()));
            let right_idx = self.code.add_constant(Value::str(rn.clone()));
            let slot_of = |name: &str| {
                self.local_map
                    .get(name)
                    .copied()
                    .unwrap_or(crate::opcode::NO_LOCAL_SLOT)
            };
            let (left_slot, right_slot) = (slot_of(ln), slot_of(rn));
            self.code.emit(OpCode::ContainerEqNamed {
                left_name_idx: left_idx,
                right_name_idx: right_idx,
                left_slot,
                right_slot,
            });
            return;
        }
        // When both sides are index expressions, use ContainerEqIndexed
        // to check binding metadata on array/hash elements.
        if let (Some(left_encoded), Some(right_encoded)) =
            (left.element_source_key(), right.element_source_key())
        {
            self.compile_expr(left);
            self.compile_expr(right);
            let left_idx = self.code.add_constant(Value::str(left_encoded));
            let right_idx = self.code.add_constant(Value::str(right_encoded));
            self.code.emit(OpCode::ContainerEqIndexed {
                left_name_idx: left_idx,
                right_name_idx: right_idx,
            });
            return;
        }
        // Mixed case: one side is a named variable (with a local slot holding
        // a HashEntryRef from `:=` binding), the other is an Index expression.
        // Use GetLocalRaw + IndexAutovivifyLazy + ContainerEqRaw.
        // An element of an `@`/`%` variable is a Scalar container, so
        // against an `@`/`%` variable only a shared bound cell makes the
        // two identical. An element of a List (`$cap[1]` for
        // `$cap = (0, @a)`) is the value itself and keeps the value
        // comparison.
        let raw_eq_op = |var: &Expr, index: &Expr| {
            let aggregate_elem = matches!(
                index,
                Expr::Index { target, .. }
                    if matches!(target.as_ref(), Expr::ArrayVar(_) | Expr::HashVar(_))
            );
            if aggregate_elem && matches!(var, Expr::ArrayVar(_) | Expr::HashVar(_)) {
                OpCode::ContainerEqRawAggregate
            } else {
                OpCode::ContainerEqRaw
            }
        };
        if let Some(slot) = self.container_eq_var_slot(left)
            && matches!(right, Expr::Index { .. })
        {
            self.code.emit(OpCode::GetLocalRaw(slot));
            self.scalar_bind_autovivify = true;
            self.compile_expr(right);
            self.scalar_bind_autovivify = false;
            self.code.emit(raw_eq_op(left, right));
            return;
        }
        if let Some(slot) = self.container_eq_var_slot(right)
            && matches!(left, Expr::Index { .. })
        {
            self.scalar_bind_autovivify = true;
            self.compile_expr(left);
            self.scalar_bind_autovivify = false;
            self.code.emit(OpCode::GetLocalRaw(slot));
            self.code.emit(raw_eq_op(right, left));
            return;
        }
        // Both sides are Index expressions that `Expr::element_source_key` could
        // not encode (nested `@a[i][j]`, or a non-literal subscript). Promote
        // each leaf to its shared `ContainerRef` cell via scalar_bind_autovivify
        // (+ bind_terminal so a scalar leaf is boxed) and compare cell identity
        // directly. Without this, nested-element `=:=` falls through to the
        // generic value compare below and reports False even for the same cell
        // (e.g. take-rw: `@neighbors[1][1][0] =:= @spot[0][0]`).
        if matches!(left, Expr::Index { .. }) && matches!(right, Expr::Index { .. }) {
            let saved_av = self.scalar_bind_autovivify;
            let saved_term = self.bind_terminal;
            self.scalar_bind_autovivify = true;
            self.bind_terminal = true;
            self.compile_expr(left);
            self.compile_expr(right);
            self.scalar_bind_autovivify = saved_av;
            self.bind_terminal = saved_term;
            self.code.emit(OpCode::ContainerEqRaw);
            return;
        }
        let left_fresh = Self::expr_is_fresh_container(left);
        let right_fresh = Self::expr_is_fresh_container(right);
        let mut flags =
            (if left_fresh { 1u8 } else { 0u8 }) | (if right_fresh { 2u8 } else { 0u8 });
        // `f($a) =:= $b` / `f($a) =:= g($b)` for a Block `f` returning its
        // tail's container: compare the cells, so a scalar variable
        // operand is captured as its cell and a call result is kept raw.
        let is_call = |e: &Expr| matches!(e.peel_parens(), Expr::Call { .. } | Expr::CallOn { .. });
        let cell_var = |c: &Self, e: &Expr| match e.peel_parens() {
            Expr::Var(n)
                if c.local_map.contains_key(n.as_str()) && Self::is_plain_lexical_name(n) =>
            {
                Some(n.clone())
            }
            _ => None,
        };
        let (lv, rv) = (cell_var(self, left), cell_var(self, right));
        if (is_call(left) && (is_call(right) || rv.is_some())) || (is_call(right) && lv.is_some()) {
            flags |= 4;
            for (e, v) in [(left, &lv), (right, &rv)] {
                self.compile_expr(e);
                if let Some(n) = v {
                    self.emit_wrap_var_ref(n);
                    self.code.emit(OpCode::CaptureVarCell);
                }
            }
            self.code.emit(OpCode::ContainerEq(flags));
            return;
        }
        self.compile_expr(left);
        self.compile_expr(right);
        self.code.emit(OpCode::ContainerEq(flags));
    }
}
