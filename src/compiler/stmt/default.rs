use super::*;

impl Compiler {
    /// Declare a fresh array and apply its default before the initializer
    /// expression runs. The later SetLocal assigns into this container and
    /// therefore preserves an explicit Any that came from an inner `[Nil]`.
    /// Shared by the statement-position declaration (`stmt.rs`) and the
    /// expression-position one (`expr_block.rs`), so `(my @a is default(1) =
    /// Nil, Any)` yields the same default-aware container as the statement.
    pub(in crate::compiler) fn emit_default_before_array_initializer(
        &mut self,
        name_idx: u32,
        slot: u32,
        default_trait_expr: Option<&Expr>,
    ) {
        self.compile_expr(&Expr::ArrayLiteral(Vec::new()));
        self.code.emit(OpCode::MarkVarDeclContext);
        // `SetLocal` consumes the value it stores, so nothing is left to pop:
        // a `Pop` here discarded whatever an enclosing expression had already
        // pushed (`10 + do { my @a is default(1) = 1; 5 }`, `@r[0] = my @a is
        // default(1) = ...`).
        self.code.emit(OpCode::SetLocal(slot));
        if let Some(arg) = default_trait_expr {
            let escaping = Self::is_closure_literal_arg(arg);
            self.with_escape(escaping, |s| s.compile_expr(arg));
        }
        let trait_name_idx = self.code.add_constant(Value::str("default".to_string()));
        self.code.emit(OpCode::ApplyVarTrait {
            name_idx,
            trait_name_idx,
            has_arg: default_trait_expr.is_some(),
            slot: Some(slot),
        });
    }

    /// `state @a is default(D) = RHS`: store the initializer into the
    /// container [`Compiler::emit_default_before_array_initializer`] already
    /// gave its default, then hand that container (not the raw RHS list) to
    /// `StateVarInit`. The store sees the default, so an explicit `Any`
    /// element stays `Any` while a `Nil` one becomes the default. The whole
    /// sequence sits behind the state guard, so it runs on the first entry
    /// only.
    pub(in crate::compiler) fn emit_state_array_store_into_defaulted(&mut self, slot: u32) {
        self.code.emit(OpCode::MarkExplicitInitializerContext);
        self.code.emit(OpCode::SetLocal(slot));
        self.code.emit(OpCode::GetLocal(slot));
    }

    /// Check if a default value expression statically mismatches a type constraint.
    /// Returns `Some(value_repr)` if a mismatch is detected, `None` otherwise.
    pub(super) fn check_default_type_mismatch(
        type_constraint: &str,
        expr: &Expr,
    ) -> Option<String> {
        // Split off an optional type smiley (`:D` / `:U` / `:_`).
        let (effective_constraint, smiley) = if let Some(b) = type_constraint.strip_suffix(":D") {
            (b, Some('D'))
        } else if let Some(b) = type_constraint.strip_suffix(":U") {
            (b, Some('U'))
        } else if let Some(b) = type_constraint.strip_suffix(":_") {
            (b, Some('_'))
        } else {
            (type_constraint, None)
        };
        // Only a recognized concrete built-in type can be rejected at compile
        // time. A subset / `where`-constrained type (`my $x is default(42) where
        // * == 42`, compiled to an anonymous `__mutsu_anon_subset_N`) or any
        // user-defined type narrows membership by a runtime predicate the compiler
        // cannot evaluate, so it must NOT be statically flagged as a mismatch —
        // the default may well satisfy it. S02-types/whatever.t "compile time
        // WhateverCode / Junction evaluation" exercises exactly this.
        const CHECKABLE_BUILTINS: &[&str] = &[
            "Int", "Num", "Rat", "Bool", "Str", "Numeric", "Real", "Cool", "Any", "Mu", "Stringy",
            "Complex", "Rational",
        ];
        if !CHECKABLE_BUILTINS.contains(&effective_constraint) {
            return None;
        }
        // A concrete (defined) literal default can never bind to a `:U`
        // (type-object-only) constraint, e.g. `my Int:U $y is default(0)`.
        let is_concrete_literal = matches!(
            expr,
            Expr::Literal(lit)
                if matches!(
                    lit.view(),
                    ValueView::Int(_) | ValueView::Num(_) | ValueView::Str(_) | ValueView::Bool(_) | ValueView::Rat(..)
                )
        );
        if smiley == Some('U') && is_concrete_literal {
            return Some(match expr {
                Expr::Literal(v) => v.to_string_value(),
                _ => "?".to_string(),
            });
        }
        let value_type = match expr {
            Expr::Literal(lit) => match lit.view() {
                ValueView::Str(s) => {
                    if effective_constraint != "Str"
                        && effective_constraint != "Cool"
                        && effective_constraint != "Any"
                    {
                        return Some(s.to_string());
                    }
                    return None;
                }
                ValueView::Int(_) => "Int",
                ValueView::Num(_) => "Num",
                ValueView::Bool(_) => "Bool",
                ValueView::Nil => {
                    // Nil is invalid for typed variables (Int, Str, etc.)
                    // but valid for untyped (Any, Mu) or explicitly Nil-accepting types
                    if effective_constraint != "Any"
                        && effective_constraint != "Mu"
                        && !effective_constraint.contains("Nil")
                    {
                        return Some("Nil".to_string());
                    }
                    return None;
                }
                _ => return None,
            },
            _ => return None, // non-literal, can't check statically
        };
        // Check type hierarchy (Int matches Numeric, Cool, Any, ...) against
        // the builtin type catalog, the one ancestry oracle (ADR-0051).
        if crate::builtins::builtin_type_ancestry::builtin_type_is_a(
            value_type,
            effective_constraint,
        ) {
            None
        } else {
            Some(match expr {
                Expr::Literal(v) => v.to_string_value(),
                _ => "?".to_string(),
            })
        }
    }
}
