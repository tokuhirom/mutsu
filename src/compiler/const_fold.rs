//! Compile-time constant folding of literal-only expressions (ADR-0006 §2.1).
//!
//! A binary/unary expression whose operands are all literal scalars is
//! evaluated at emit time and replaced by a single `LoadConst`. The evaluation
//! calls the *same* native operator implementations the VM uses
//! (`builtins::arith_*`, `Interpreter::concat_values`), so Int→BigInt
//! promotion, `Int/Int → Rat`, and Num formatting automatically agree with
//! runtime semantics — no separate "compile-time arithmetic" exists.
//!
//! Safety (ADR-0006 §2.1):
//!
//! - **Operator overrides.** A user `sub infix:<op>` overrides even native
//!   `Int + Int` (roast S06-operator-overloading/infix.t), and mutsu registers
//!   those in a *global* set (`user_declared_infix_ops`), so a declaration
//!   anywhere in the compilation unit can affect any expression in it. Rather
//!   than statically walking the AST for such declarations (a walker that
//!   misses one nested inside a block/sub/class body would fold *wrongly*),
//!   the compiler notices them where they are unavoidably compiled — the
//!   `SubDecl`/`ProtoDecl`/`VarDecl` arms call [`FoldCtx::note_operator_decl`]
//!   — and the unit-level [`Compiler::compile`] simply **recompiles the whole
//!   unit with folding off** if a declaration turned up after something was
//!   folded. Sound by construction, no walker to keep in sync, and the double
//!   compile is paid only by files that declare operators.
//! - **`USER_INFIX_DECLS`.** Any unit compiled *after* a user operator was
//!   registered at runtime (an `EVAL`, a `map` block, a module body) folds
//!   nothing: the process-wide counter the JIT already maintains is non-zero.
//! - **Exceptions are preserved.** An evaluation that fails (`1/0`, overflow to
//!   an error) is simply not folded, so the VM raises it at runtime as before.
//! - **Value types only.** Int/BigInt/Num/Rat/Str/Bool — immutable scalars whose
//!   identity is not observable. Containers/Instances are never folded.
//!
//! Known gap: an operator override *imported from a module* is registered when
//! the `use` executes, i.e. after the consuming unit has already been compiled,
//! so a literal-only expression there is folded against the core operator. This
//! matches what raku does for narrowly-typed multis (the core candidate wins
//! anyway); it diverges only if a module supersedes the operator for the very
//! literal types being folded.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use super::Compiler;
use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::{Value, ValueView};

/// Per-compilation-unit constant-folding state, shared with every child
/// (sub/closure body) compiler of the unit.
#[derive(Debug, Default)]
pub(crate) struct FoldCtx {
    /// False during the retry pass: fold nothing.
    enabled: AtomicBool,
    /// A user operator (`infix:<+>`, `&infix:<+>`, a runtime-named sub) is
    /// declared somewhere in this unit.
    saw_operator_decl: AtomicBool,
    /// At least one expression was folded in this unit.
    folded: AtomicBool,
}

/// `MUTSU_CONST_FOLD=0` turns folding off process-wide (A/B benchmarking and
/// bisecting a suspected fold-related behavior change).
fn folding_allowed() -> bool {
    static ALLOWED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ALLOWED.get_or_init(|| std::env::var("MUTSU_CONST_FOLD").as_deref() != Ok("0"))
}

impl FoldCtx {
    pub(crate) fn enabled() -> Self {
        Self {
            enabled: AtomicBool::new(folding_allowed()),
            saw_operator_decl: AtomicBool::new(false),
            folded: AtomicBool::new(false),
        }
    }

    pub(crate) fn is_enabled(&self) -> bool {
        self.enabled.load(Ordering::Relaxed)
    }

    pub(crate) fn disabled() -> Self {
        Self {
            enabled: AtomicBool::new(false),
            saw_operator_decl: AtomicBool::new(false),
            folded: AtomicBool::new(false),
        }
    }

    /// Record a declaration that may install a user operator. Called for every
    /// sub/proto/variable declaration whose name mentions an operator category,
    /// and for runtime-named subs (`sub ::($name)`), whose name is unknowable.
    pub(crate) fn note_operator_decl(&self) {
        self.saw_operator_decl.store(true, Ordering::Relaxed);
    }

    /// True when something was folded *and* an operator declaration turned up:
    /// the unit must be recompiled with folding off.
    pub(crate) fn needs_refold_pass(&self) -> bool {
        self.folded.load(Ordering::Relaxed) && self.saw_operator_decl.load(Ordering::Relaxed)
    }
}

/// Name of a declaration that could install a user-defined operator.
pub(crate) fn declares_operator(name: &str) -> bool {
    name.contains("infix:") || name.contains("prefix:") || name.contains("postfix:")
}

impl Compiler {
    /// True when this unit may fold. Cheap enough to call per binary expression.
    fn const_fold_enabled(&self) -> bool {
        self.fold_ctx.enabled.load(Ordering::Relaxed)
            && !self.fold_ctx.saw_operator_decl.load(Ordering::Relaxed)
            // Any user operator registered anywhere in the process (a module
            // loaded before this EVAL/block was compiled) disables folding.
            && crate::vm::vm_jit::USER_INFIX_DECLS.load(Ordering::Relaxed) == 0
    }

    /// Fold `left op right` into a single value, or `None` to compile normally.
    pub(super) fn try_const_fold_binary(
        &mut self,
        left: &Expr,
        op: &TokenKind,
        right: &Expr,
    ) -> Option<Value> {
        if !self.const_fold_enabled() {
            return None;
        }
        let value = fold_binary(left, op, right)?;
        self.fold_ctx.folded.store(true, Ordering::Relaxed);
        Some(value)
    }

    /// Fold a unary literal expression (`-1`, `-(2*3)`).
    pub(super) fn try_const_fold_unary(&mut self, op: &TokenKind, expr: &Expr) -> Option<Value> {
        if !self.const_fold_enabled() {
            return None;
        }
        let value = fold_unary(op, expr)?;
        self.fold_ctx.folded.store(true, Ordering::Relaxed);
        Some(value)
    }

    /// Share this unit's fold state with a child (sub/closure body) compiler.
    pub(super) fn inherit_fold_ctx(&self, child: &mut Compiler) {
        child.fold_ctx = Arc::clone(&self.fold_ctx);
        child.fold_root = false;
    }

    /// Flag a declaration whose name may install a user-defined operator
    /// (`sub infix:<+>`, `my &prefix:<!>`, ...), disabling folding for the unit.
    pub(super) fn note_operator_decl(&self, name: &str) {
        if declares_operator(name) {
            self.fold_ctx.note_operator_decl();
        }
    }
}

/// Scalar whose value is fully determined at compile time and whose identity is
/// not observable.
fn const_scalar(value: &Value) -> bool {
    matches!(
        value.view(),
        ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(..)
            | ValueView::Str(_)
            | ValueView::Bool(_)
    )
}

/// Numeric scalar: arithmetic folds only accept these, so the VM's
/// string→number / type-object coercion bridges (which can raise
/// X::Str::Numeric) are never bypassed.
fn const_numeric(value: &Value) -> bool {
    matches!(
        value.view(),
        ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Num(_) | ValueView::Rat(..)
    )
}

/// Evaluate an expression to a constant scalar, or `None` if it is not one.
fn const_operand(expr: &Expr) -> Option<Value> {
    match expr {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => const_scalar(v).then(|| v.clone()),
        Expr::Grouped(inner) => const_operand(inner),
        Expr::Unary { op, expr } => fold_unary(op, expr),
        Expr::Binary { left, op, right } => fold_binary(left, op, right),
        _ => None,
    }
}

fn fold_unary(op: &TokenKind, expr: &Expr) -> Option<Value> {
    let value = const_operand(expr)?;
    match op {
        // `-"3"` goes through the VM's string→numeric coercion (which can raise
        // X::Str::Numeric), so only numeric literals fold.
        TokenKind::Minus if const_numeric(&value) => crate::builtins::arith_negate(value).ok(),
        _ => None,
    }
}

fn fold_binary(left: &Expr, op: &TokenKind, right: &Expr) -> Option<Value> {
    let l = const_operand(left)?;
    let r = const_operand(right)?;
    fold_values(op, l, r)
}

/// Apply `op` to two constant scalars using the VM's own operator
/// implementations. `None` = do not fold (unsupported operator, operand type
/// outside the proven-safe set, or an evaluation that raises).
fn fold_values(op: &TokenKind, l: Value, r: Value) -> Option<Value> {
    match op {
        // Arithmetic: numeric operands only — matches the VM arms, which for
        // Int/Num/Rat/BigInt reduce to exactly these calls (the numeric-bridge
        // coercion in between is the identity for numeric values).
        TokenKind::Plus if const_numeric(&l) && const_numeric(&r) => {
            crate::builtins::arith_add(l, r).ok()
        }
        TokenKind::Minus if const_numeric(&l) && const_numeric(&r) => {
            Some(crate::builtins::arith_sub(l, r))
        }
        TokenKind::Star if const_numeric(&l) && const_numeric(&r) => {
            Some(crate::builtins::arith_mul(l, r))
        }
        TokenKind::Slash if const_numeric(&l) && const_numeric(&r) => {
            crate::builtins::arith_div(l, r).ok()
        }
        TokenKind::Percent if const_numeric(&l) && const_numeric(&r) => {
            crate::builtins::arith_mod(l, r).ok()
        }
        TokenKind::StarStar if const_numeric(&l) && const_numeric(&r) => {
            Some(crate::builtins::arith_pow(l, r))
        }
        // Concatenation: `~` stringifies via `.Stringy`/`.Str`, which for scalar
        // values is exactly what `concat_values` does (only Instances/Packages
        // can dispatch a user method, and those never reach here).
        TokenKind::Tilde => Some(crate::runtime::Interpreter::concat_values(l, r)),
        // Numeric comparison: restricted to the Int/Int and Num/Num pairs the VM
        // itself fast-paths, so the folded result is the same expression the VM
        // would evaluate (no Rat/BigInt cross-type comparison logic duplicated).
        TokenKind::EqEq
        | TokenKind::BangEq
        | TokenKind::Lt
        | TokenKind::Lte
        | TokenKind::Gt
        | TokenKind::Gte => fold_compare(op, &l, &r),
        _ => None,
    }
}

fn fold_compare(op: &TokenKind, l: &Value, r: &Value) -> Option<Value> {
    let ord = match (l.view(), r.view()) {
        (ValueView::Int(a), ValueView::Int(b)) => a.cmp(&b),
        (ValueView::Num(a), ValueView::Num(b)) => {
            // NaN is unordered (`partial_cmp` → None): every comparison against
            // it is False, except `!=`, which is True (`NaN != NaN`).
            let Some(ord) = a.partial_cmp(&b) else {
                return Some(Value::truth(matches!(op, TokenKind::BangEq)));
            };
            ord
        }
        _ => return None,
    };
    let result = match op {
        TokenKind::EqEq => ord.is_eq(),
        TokenKind::BangEq => ord.is_ne(),
        TokenKind::Lt => ord.is_lt(),
        TokenKind::Lte => ord.is_le(),
        TokenKind::Gt => ord.is_gt(),
        TokenKind::Gte => ord.is_ge(),
        _ => return None,
    };
    Some(Value::truth(result))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int(n: i64) -> Expr {
        Expr::Literal(Value::int(n))
    }

    fn bin(l: Expr, op: TokenKind, r: Expr) -> Expr {
        Expr::Binary {
            left: Box::new(l),
            op,
            right: Box::new(r),
        }
    }

    #[test]
    fn folds_nested_int_arithmetic() {
        // 60 * 60 * 24
        let expr = bin(
            bin(int(60), TokenKind::Star, int(60)),
            TokenKind::Star,
            int(24),
        );
        let folded = const_operand(&expr).expect("folds");
        assert_eq!(folded.as_int(), Some(86400));
    }

    #[test]
    fn folds_unary_minus() {
        let expr = Expr::Unary {
            op: TokenKind::Minus,
            expr: Box::new(int(7)),
        };
        assert_eq!(const_operand(&expr).unwrap().as_int(), Some(-7));
    }

    #[test]
    fn int_division_folds_to_rat() {
        let folded = fold_values(&TokenKind::Slash, Value::int(1), Value::int(3)).expect("folds");
        // 1/3 is a Rat in raku, not a Num.
        assert!(matches!(folded.view(), ValueView::Rat(1, 3)));
    }

    #[test]
    fn division_by_zero_folds_to_the_rat_the_vm_would_build() {
        // `1/0` is a Rat in raku — it only fails when the value is *used*. The
        // fold calls the same `arith_div` the VM calls, so it produces that same
        // Rat rather than raising at compile time. An operator that does return
        // an error (`Err`) is left unfolded, and the VM raises it at runtime.
        let folded = fold_values(&TokenKind::Slash, Value::int(1), Value::int(0)).expect("folds");
        assert!(matches!(folded.view(), ValueView::Rat(1, 0)));
    }

    #[test]
    fn string_operands_do_not_fold_arithmetic() {
        // `"3" + 4` must keep the VM's string→numeric coercion path.
        assert!(
            fold_values(&TokenKind::Plus, Value::str("3".to_string()), Value::int(4)).is_none()
        );
    }

    #[test]
    fn concat_folds() {
        let folded = fold_values(
            &TokenKind::Tilde,
            Value::str("a".to_string()),
            Value::int(1),
        )
        .expect("folds");
        assert_eq!(folded.as_str(), Some("a1"));
    }

    #[test]
    fn nan_comparisons_fold_as_unordered() {
        let nan = Value::num(f64::NAN);
        // Every comparison against NaN is False — except `!=`, which is True.
        for (op, expected) in [
            (TokenKind::EqEq, false),
            (TokenKind::Lt, false),
            (TokenKind::Gte, false),
            (TokenKind::BangEq, true),
        ] {
            let folded = fold_values(&op, nan.clone(), nan.clone()).expect("folds");
            assert_eq!(folded.truthy(), expected, "NaN {op:?} NaN");
        }
    }

    #[test]
    fn variables_do_not_fold() {
        let expr = bin(Expr::Var("x".to_string()), TokenKind::Plus, int(1));
        assert!(const_operand(&expr).is_none());
    }
}
