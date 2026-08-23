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
//! # Why the synthesized chain conjunction is excluded
//!
//! `src/parser/expr/precedence/chain_cmp.rs` expands `a < m < b` into
//! `(a < m) && (m < b)` with the middle operand duplicated. That `&&` is a
//! compiler artefact, not a user-written thunk barrier, and treating it as one
//! would break chained comparison outright: rakudo makes the whole chain a
//! single priming scope (`(1 < * < 10)(0)` is `False`) while a genuine
//! user-written `&&` yields its right operand (`(1 < * && * < 10)(0)` is
//! `True`). ADR-0033's "Phase-4 prerequisite" section calls for making the
//! expansion distinguishable; mutsu does that with a dedicated
//! [`TokenKind::ChainAnd`], which is deliberately absent from
//! [`is_thunk_barrier`]. (Giving the expansion a whole `Expr::ChainedCompare`
//! node — the ADR's other suggestion, which would additionally let RakuAST
//! render `1 < * < 10` faithfully — is tracked separately; it needs an arm in
//! every `Expr` walker, whereas the token keeps the `Expr::Binary` shape every
//! existing walker already handles.)
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
/// Note `TokenKind::ChainAnd` is absent on purpose — see the module docs.
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

/// The scope authority, applied to one expression node.
///
/// Invoked from [`super::mark`]'s post-parse walk (which already visits every
/// `Expr` in the program exactly once, top-down) *before* that walk recurses
/// into the node's children, so the recursion sees — and classifies the leaves
/// of — whatever markers this planted.
pub(crate) fn plant_here(expr: &mut Expr) {
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
