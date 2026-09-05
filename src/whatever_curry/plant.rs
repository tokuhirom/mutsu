//! The Whatever-priming **scope authority** (ADR-0033 section 4, Phase 4).
//!
//! Raku's priming scope does not run through a *thunky* operator. `&&`, `||`,
//! `//`, `and`, `or`, `andthen`, `orelse`, `notandthen` and the ternary's three
//! parts compile their operands as thunks, and Whatever-priming happens **per
//! thunk**: `* > 3 && * < 8` is *two* independent arity-1 `WhateverCode`s, and
//! the `&&` then runs at its own evaluation time, sees a truthy `Code` object on
//! the left and yields the right-hand `WhateverCode`. Measured against rakudo:
//!
//! ```text
//! (* > 3 && * < 8).arity          1
//! (* > 3 && * < 8)(5)             True
//! (1..10).grep(* > 3 && * < 8)    (1 2 3 4 5 6 7)
//! (* + 1 && 5).WHAT               (Int)
//! (* // 5)(Nil)                   No such method 'CALL-ME' for ... 'Whatever'
//! (* + 1 ?? * + 2 !! * + 3).WHAT  (WhateverCode)
//! ```
//!
//! mutsu used to prime straight *through* those operators, producing a single
//! arity-2 closure (`(1..10).grep(* > 3 && * < 8)` silently returned `5 6`), and
//! primed *nothing at all* inside a ternary (a bare `Expr::Whatever` survived to
//! the runtime and died coercing to `Numeric`). One rule fixes both directions.
//!
//! # The rule, in two halves
//!
//! 1. **A thunk barrier is opaque to the enclosing scope.** `contains_whatever`
//!    (`crate::parser::expr::whatever`), [`super::build::count_whatever`] and
//!    [`super::replace`] all stop at a barrier, so a `*` inside one contributes
//!    nothing to the arity — or even the existence — of any scope above it.
//!    That is what makes `((* > 3 && * < 8) + *)` a *single* arity-1
//!    `WhateverCode` (measured) rather than an arity-3 one, and what stops the
//!    parser's existing planting sites from ever proposing a scope that spans a
//!    barrier.
//! 2. **Each barrier operand is a scope of its own.** [`plant_here`] runs on
//!    every expression node of a freshly-parsed program and, at a barrier or a
//!    ternary, materialises an `Expr::WhateverCurry` marker around each operand
//!    that primes. For the ternary this is a strict *gain*: mutsu planted no
//!    scope there at all before.
//!
//! # Why a chained comparison is not a thunk barrier
//!
//! `a < m < b` parses to `Expr::ChainedCompare { operands, ops }`
//! (`todo/tickets/chained-compare-ast-node.md`, closing ADR-0033's
//! "Phase-4 prerequisite"), not an `Expr::Binary`, so it never reaches
//! [`is_thunk_barrier`]'s `Expr::Binary` arm in the first place — the whole
//! chain stays one priming scope by construction. This is what rakudo does
//! too: the whole chain is a single priming scope (`(1 < * < 10)(0)` is
//! `False`), while a genuine user-written `&&` over the same operands yields
//! only its right operand (`(1 < * && * < 10)(0)` is `True`). The compiler
//! expands `ChainedCompare` into a `&&`-conjunction only at compile time
//! (`crate::chain_compare::expand`, plain `TokenKind::AndAnd`), well after
//! this scope decision and every Whatever-curry walker have already run on
//! the un-expanded node — so no dedicated token is needed to keep a
//! synthesized chain conjunction distinguishable from a user-written `&&`
//! (the earlier design, `TokenKind::ChainAnd`, is retired).
//!
//! # Not barriers
//!
//! `xor` and `^^` are **not** short-circuit, and rakudo primes neither: both
//! `(* + 1 xor * + 2).WHAT` and `(* + 1 ^^ * + 2).WHAT` are `Nil` with a
//! "Useless use of ... in sink context" warning — neither a `WhateverCode` nor
//! a plain `Bool`, so no barrier treatment reproduces them. ADR-0033
//! deliberately excludes `xor`; the same measurement puts `^^` alongside it.
//! mutsu keeps currying through both (`(WhateverCode)`), a documented
//! divergence rather than a guess.

use crate::ast::Expr;
use crate::parser::should_wrap_whatevercode;
use crate::token_kind::TokenKind;

/// True for an expression whose operands are *thunks*, and therefore each a
/// Whatever-priming scope of their own rather than part of the enclosing one.
///
/// A chained comparison (`Expr::ChainedCompare`) never matches here — see the
/// module docs.
pub(crate) fn is_thunk_barrier(expr: &Expr) -> bool {
    match expr {
        Expr::Ternary { .. } => true,
        Expr::Binary { op, .. } => matches!(
            op,
            TokenKind::AndAnd
                | TokenKind::OrOr
                | TokenKind::SlashSlash
                | TokenKind::AndWord
                | TokenKind::OrWord
                | TokenKind::AndThen
                | TokenKind::OrElse
                | TokenKind::NotAndThen
        ),
        _ => false,
    }
}

/// Materialise `slot` as its own priming scope if it primes.
///
/// A bare `*` deliberately does not wrap (`should_wrap_whatevercode` excludes
/// it), which is what makes `(* // 5)` yield the `Whatever` *value* — rakudo
/// then dies with "No such method 'CALL-ME'" when it is invoked, exactly as
/// mutsu now does. An operand the parser already planted is likewise left
/// alone: `contains_whatever` does not see through a `WhateverCurry` marker, so
/// `((* > 3) && (* < 8))` is not double-wrapped.
fn materialise_scope(slot: &mut Expr) {
    if should_wrap_whatevercode(slot) {
        let body = std::mem::replace(slot, Expr::Whatever);
        *slot = Expr::WhateverCurry(Box::new(body));
    }
}

thread_local! {
    /// Whether the walk currently running is planting *every* priming scope
    /// (ADR-0033 Phase 3) rather than only the barrier ones.
    ///
    /// The parser plants its own scopes at ~29 grammar positions and then runs
    /// [`super::mark`] purely to classify leaves and split barriers, so for a
    /// parsed program this stays `false` and nothing about that path changes.
    /// A tree lowered from RakuAST has no parser behind it and therefore no
    /// scopes at all, so `rakuast::lower` runs the same walk with this set —
    /// see [`plant_all_scopes`].
    static PLANT_ALL_SCOPES: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Run `body` with every-expression scope planting enabled, restoring the
/// previous setting afterwards (including on an unwind).
pub(crate) fn with_all_scopes<R>(body: impl FnOnce() -> R) -> R {
    struct Restore(bool);
    impl Drop for Restore {
        fn drop(&mut self) {
            PLANT_ALL_SCOPES.with(|f| f.set(self.0));
        }
    }
    let _restore = Restore(PLANT_ALL_SCOPES.with(|f| f.replace(true)));
    body()
}

/// The Phase 3 half of the scope authority: materialise a scope around **this**
/// expression when it primes at all.
///
/// [`super::mark`]'s walk reaches every expression slot top-down and calls this
/// before recursing, so the *first* (outermost) node that primes gets the
/// marker — which is what makes the scope maximal, exactly as the parser's own
/// planting sites make it. Wrapping bottom-up instead would give `*.abs + 1`
/// two nested markers (an inner closure added to `1`) rather than one.
///
/// It is deliberately not universal: `should_wrap_whatevercode` is the parser's
/// own predicate, and `contains_whatever` under it stops at a call/method
/// argument, a barrier, and the other non-currying positions. That is why
/// `@a.first(* > 1)` plants one scope around the *argument* and none around the
/// method call.
fn plant_all_scopes(expr: &mut Expr) {
    if !PLANT_ALL_SCOPES.with(|f| f.get()) {
        return;
    }
    // An invocation is never itself a scope: `(* + 1)(4)` curries the *target*
    // and then calls it, so the scope belongs on the target and the recursion
    // below plants it there. `should_wrap_whatevercode` says `true` here only
    // because the parser never asks it about an invocation — it wraps the
    // target at its own dedicated grammar site instead — and wrapping the whole
    // `CallOn` would make the program evaluate to the closure rather than call
    // it.
    if matches!(expr, Expr::CallOn { .. }) {
        return;
    }
    materialise_scope(expr);
}

/// The scope authority, applied to one expression node.
///
/// Invoked from [`super::mark`]'s post-parse walk (which already visits every
/// `Expr` in the program exactly once, top-down) *before* that walk recurses
/// into the node's children, so the recursion sees — and classifies the leaves
/// of — whatever markers this planted.
pub(crate) fn plant_here(expr: &mut Expr) {
    plant_all_scopes(expr);
    plant_barriers_here(expr);
}

/// The barrier half alone, for a node that is already known to sit *directly*
/// inside a scope marker: its enclosing scope exists, so planting another one
/// around it would wrap the same expression forever, but each barrier operand
/// under it is still a scope of its own.
pub(crate) fn plant_barriers_here(expr: &mut Expr) {
    if !is_thunk_barrier(expr) {
        return;
    }
    match expr {
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            materialise_scope(cond);
            materialise_scope(then_expr);
            materialise_scope(else_expr);
        }
        Expr::Binary { left, right, .. } => {
            materialise_scope(left);
            materialise_scope(right);
        }
        // `is_thunk_barrier` matches only the two shapes above.
        _ => {}
    }
}
