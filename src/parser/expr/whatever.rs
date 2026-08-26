//! WhateverCode detection and inspection helpers.
//!
//! Pure, read-only predicates over the `Expr` tree used to decide whether an
//! expression should be wrapped into an `Expr::WhateverCurry` priming-scope
//! marker (ADR-0033) and, once decided, whether an operand already carries
//! one. This module owns the priming-*scope* decision — deciding whether the
//! actual closure gets built is `crate::whatever_curry::build_closure`,
//! invoked later by the compiler.

use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::ValueView;

pub(crate) fn should_wrap_whatevercode(expr: &Expr) -> bool {
    if !contains_whatever(expr) || is_whatever(expr) || matches!(expr, Expr::HyperWhatever) {
        return false;
    }
    if contains_xx_with_bare_whatever(expr) {
        return false;
    }
    match expr {
        // SmartMatch: Whatever on RHS is handled at runtime (autoprime).
        // LHS compound Whatever curries (e.g. `*.abs ~~ Code`), but bare
        // `* ~~ Type` currying is handled in the precedence parser where we can
        // distinguish a true bare * operand from a parenthesized ((*)).
        Expr::Binary {
            op: TokenKind::SmartMatch,
            ..
        } => false,
        // BangTilde (!~~): bare * on LHS SHOULD curry (e.g. `* !~~ Int`), because
        // `!~~` is not Whatever-aware in Raku.
        Expr::Binary {
            op: TokenKind::BangTilde,
            left,
            ..
        } => contains_whatever(left),
        Expr::Binary {
            op: TokenKind::Ident(name),
            ..
        } if name == "o" => false,
        // `*(args)` invokes a *bare* Whatever value — it does not curry. Whatever
        // has no `CALL-ME`, so this dies at runtime (X::Method::NotFound). Only a
        // CallOn on a *compound* curry target (`*[0](...)`, `*.foo(...)`) wraps the
        // target into a WhateverCode and invokes it (handled by the target-wrap arm
        // in `expression`). Keep the bare-target CallOn unwrapped here.
        Expr::CallOn { target, .. }
            if is_whatever(target) || matches!(target.as_ref(), Expr::HyperWhatever) =>
        {
            false
        }
        // List replication `xx` does not Whatever-curry a *bare* `*` operand: a
        // standalone `*` is the Whatever value, repeated literally. `* xx 2` is
        // `(*, *)`; `1 xx *`/`1 x *` is the infinite-repeat form. None wrap into a
        // WhateverCode. (A compound operand like `(*+1) xx 2` is already wrapped at
        // the parenthesis, so its `left` is a Lambda, not a bare Whatever. Note
        // string replication `* x 2` DOES curry into a WhateverCode in Raku, so
        // only the right-Whatever form is exempt for `x`.)
        Expr::Binary {
            op: TokenKind::Ident(name),
            left,
            right,
        } if ((name == "x" || name == "xx") && is_whatever(right))
            || (name == "xx" && is_whatever(left)) =>
        {
            false
        }
        _ => true,
    }
}

/// Whether a non-bareword `=>` pair with these operands should Whatever-curry
/// into a `WhateverCode`.
///
/// Raku's `=>` participates in Whatever-currying for **non-bareword** keys: when
/// either operand is (or contains) a currying `*`, the whole pair becomes a
/// `WhateverCode` that yields the `Pair` when called. So `* => *`, `"k" => *`,
/// `5 => *`, `* => 5`, `"x" ~ * => *` and `"k" => (* + 1)` are all
/// `WhateverCode`, while `* xx 3 => 1` (the `xx` operand opts out) and
/// `"k" => 5` stay plain `Pair`s.
///
/// This is decided at the `=>` construction site — not in `contains_whatever` —
/// because a colonpair (`:as(*)`) and a string-keyed `=>` pair (`"as" => *`)
/// share the same inner `Binary{FatArrow, Literal(Str), …}` AST and are only
/// distinguishable by their caller. `contains_whatever` keeps the colonpair
/// exemption (a bare `Binary{FatArrow}` with a string-literal LHS stays a
/// literal `Pair`); the currying `=>` form routes through here instead. Bareword
/// keys (`a => *`, a named-argument `Pair`) never reach this — the caller gates
/// on `is_bareword` first.
pub(crate) fn fat_arrow_curries(left: &Expr, right: &Expr) -> bool {
    fn operand_curries(e: &Expr) -> bool {
        // A bare `*` or an already-planted `(* …)` curry does not wrap when it
        // stands alone (that is why `should_wrap_whatevercode` excludes them),
        // but as a `=>` operand it does make the pair curry. Everything else
        // defers to the shared `should_wrap_whatevercode` opt-out logic (so `xx`,
        // `o`, smartmatch etc. still suppress currying).
        is_whatever(e) || is_wrapped_whatevercode(e) || should_wrap_whatevercode(e)
    }
    operand_curries(left) || operand_curries(right)
}

fn contains_xx_with_bare_whatever(expr: &Expr) -> bool {
    match expr {
        Expr::Binary {
            left,
            op: TokenKind::Ident(name),
            right,
            ..
        } => {
            // `xx` with a *bare* `*` on either side is a literal repetition of the
            // Whatever value (`* xx 2` → `(*, *)`, `1 xx *` → infinite), not a curry
            // point — so an enclosing postfix (`(* xx 3).elems`) must not wrap the
            // whole chain into a WhateverCode either. (`x`/`xx` with a right `*` was
            // already exempt; string `* x 2` DOES curry, so only `xx` exempts left.)
            ((name == "xx" || name == "x") && is_whatever(right))
                || (name == "xx" && is_whatever(left))
                || contains_xx_with_bare_whatever(left)
                || contains_xx_with_bare_whatever(right)
        }
        Expr::Unary { expr, .. } => contains_xx_with_bare_whatever(expr),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            contains_xx_with_bare_whatever(target)
                || args.iter().any(contains_xx_with_bare_whatever)
        }
        Expr::Index { target, index, .. } => {
            contains_xx_with_bare_whatever(target) || contains_xx_with_bare_whatever(index)
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            args.iter().any(contains_xx_with_bare_whatever)
        }
        Expr::CallOn { target, args } => {
            contains_xx_with_bare_whatever(target)
                || args.iter().any(contains_xx_with_bare_whatever)
        }
        Expr::ArrayLiteral(items) | Expr::BracketArray(items, _) => {
            items.iter().any(contains_xx_with_bare_whatever)
        }
        Expr::CaptureLiteral(items) => items.iter().any(contains_xx_with_bare_whatever),
        Expr::ChainedCompare { operands, .. } => {
            operands.iter().any(contains_xx_with_bare_whatever)
        }
        _ => false,
    }
}

/// True for a bare `*` leaf — either a `Whatever` *value* or a `WhateverArg`
/// *priming argument* (ADR-0033 Phase 2). Outside `src/rakuast/`, the two are
/// deliberately indistinguishable: every scope/arity predicate built on this
/// helper must keep computing exactly what it computes today regardless of
/// which leaf the classifier in `src/whatever_curry/mark.rs` produced.
pub(crate) fn is_whatever(expr: &Expr) -> bool {
    matches!(expr, Expr::Whatever | Expr::WhateverArg)
}

/// True when `expr` is an already-planted WhateverCurry marker (produced by a
/// parenthesized curry such as `(* - 1)`). Such a marker is opaque as a
/// *value* (e.g. when passed as an argument or stored in a variable), but when
/// it appears as an *operand* of a further operator/method in the same
/// expression, Raku composes it into a new, larger WhateverCode (`(* - 1) -
/// 1`, `(^*).roll`, `1 +< (* - 1)`). The currying machinery (`count_whatever` /
/// `replace_whatever_*` in `crate::whatever_curry`) already knows how to
/// inline such a marker; this helper lets the composing operand positions
/// detect it.
fn is_wrapped_whatevercode(expr: &Expr) -> bool {
    matches!(expr, Expr::WhateverCurry(_))
}

pub(crate) fn contains_whatever(expr: &Expr) -> bool {
    match expr {
        e if is_whatever(e) || matches!(e, Expr::HyperWhatever) => true,
        // Thunk barriers (`&&`, `||`, `//`, `and`, `or`, `andthen`, `orelse`,
        // `notandthen`, and the ternary) are **opaque** to the enclosing
        // priming scope: each operand is a thunk, hence a priming scope of its
        // own, planted by `crate::whatever_curry::plant`. A `*` below a barrier
        // therefore neither creates nor enlarges any scope above it — which is
        // exactly why `(* > 3 && * < 8)` is two arity-1 `WhateverCode`s (rakudo:
        // `.arity` 1, `(5)` True) rather than one arity-2 closure, and why
        // `((* > 3 && * < 8) + *)` is a single arity-1 closure. ADR-0033 Phase 4.
        e if crate::whatever_curry::is_thunk_barrier(e) => false,
        // Don't treat bare * inside range/sequence operators as WhateverCode.
        // `1..*` is a Range, but `1..*-1` is a WhateverCode.
        // If an endpoint contains a non-bare Whatever (e.g. `*-1`), the whole
        // range should become a WhateverCode.
        Expr::Binary {
            op:
                TokenKind::DotDot
                | TokenKind::DotDotCaret
                | TokenKind::CaretDotDot
                | TokenKind::CaretDotDotCaret,
            left,
            right,
        } => {
            fn endpoint_has_compound_whatever(e: &Expr) -> bool {
                contains_whatever(e) && !is_whatever(e)
            }
            endpoint_has_compound_whatever(left) || endpoint_has_compound_whatever(right)
        }
        Expr::Binary {
            op: TokenKind::DotDotDot | TokenKind::DotDotDotCaret,
            ..
        } => false,
        // SmartMatch/BangTilde: Whatever on the RHS is handled at runtime
        // (autoprime to WhateverCode). Only check LHS for Whatever.
        Expr::Binary {
            op: TokenKind::SmartMatch | TokenKind::BangTilde,
            left,
            ..
        } => contains_whatever(left),
        // Named FatArrow pairs (colonpairs): `:as(*)` produces `"as" => *` which should
        // be Pair("as", Whatever), NOT a WhateverCode.  When the left side is a string
        // literal, skip WhateverCode propagation so the Pair is not auto-curried.
        Expr::Binary {
            op: TokenKind::FatArrow,
            left,
            ..
        } if matches!(left.as_ref(), Expr::Literal(lit) if matches!(lit.view(), ValueView::Str(_))) => {
            false
        }
        // Composition operators `o` and `∘` never auto-curry: their operands are
        // always treated as callables, so `(* + 1) o (* * 2)` should compose
        // two WhateverCodes rather than becoming a WhateverCode itself.
        Expr::Binary {
            op: TokenKind::Ident(name),
            ..
        } if name == "o" || name == "\u{2218}" => false,
        // `xx` replicates its LHS as a value N times (producing a Seq), so a
        // WhateverCode LHS is NOT composed — `(* - 1) xx 3` is a Seq of three
        // WhateverCodes, not a curried WhateverCode. Keep the historical
        // bare-`*` behavior (only the literal placeholder triggers wrapping).
        Expr::Binary {
            op: TokenKind::Ident(name),
            left,
            right,
        } if name == "xx" => contains_whatever(left) || contains_whatever(right),
        Expr::Binary { left, right, .. } => {
            contains_whatever(left)
                || contains_whatever(right)
                || is_wrapped_whatevercode(left)
                || is_wrapped_whatevercode(right)
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
            contains_whatever(expr) || is_wrapped_whatevercode(expr)
        }
        // Non-currying pseudo-methods evaluate immediately on Whatever AND on
        // an already-built WhateverCode: `*.WHAT` is `(Whatever)` and
        // `(* + 1).WHAT` is `(WhateverCode)`. Rakudo's list is exactly these
        // six — `.WHICH` and `.WHY` DO curry (`*.WHICH` is a WhateverCode).
        Expr::MethodCall { target, name, .. }
            if matches!(
                name.resolve().as_str(),
                "WHAT" | "WHO" | "HOW" | "WHERE" | "DEFINITE" | "VAR"
            ) && (is_whatever(target) || is_wrapped_whatevercode(target)) =>
        {
            false
        }
        // A hyper method call curries exactly like a plain one: `*.comb».uc` is
        // a WhateverCode, not an eager `Whatever.comb».uc`.
        Expr::MethodCall { target, .. }
        | Expr::DynamicMethodCall { target, .. }
        | Expr::HyperMethodCall { target, .. }
        | Expr::HyperMethodCallDynamic { target, .. } => {
            contains_whatever(target) || is_wrapped_whatevercode(target)
        }
        Expr::CallOn { target, .. } => {
            // Only the *target* of an invocation can curry: `(*.foo).(x)` invokes
            // the curried `*.foo`, and `*[0]([1,2,3])` invokes the curried `*[0]`.
            // A bare `*` (or compound Whatever) passed as a call *argument* is NOT
            // a curry point — it is a Whatever value handed to the callee. So
            // `&infix:<+>(*, 42)` invokes `&infix:<+>` with a Whatever argument
            // (which dies in `+`), it does NOT make a closure.
            contains_whatever(target)
        }
        // Only check target, not index: @a[*-1] should NOT make the whole expr a WhateverCode.
        // The [*-1] subscript handles its own WhateverCode wrapping.
        Expr::Index { target, .. } => contains_whatever(target) || is_wrapped_whatevercode(target),
        // User-defined infix operators: `* quack 5`, `3 quack *`, etc.
        // Exclude flip-flop operators (ff, fff and variants) since `ff *` means
        // "stay true forever" and `*` should not trigger WhateverCode wrapping.
        Expr::InfixFunc {
            name, left, right, ..
        } => {
            let is_flipflop = matches!(
                name.as_str(),
                "ff" | "fff" | "^ff" | "ff^" | "^ff^" | "^fff" | "fff^" | "^fff^"
            );
            if is_flipflop {
                return false;
            }
            contains_whatever(left)
                || is_wrapped_whatevercode(left)
                || right
                    .iter()
                    .any(|e| contains_whatever(e) || is_wrapped_whatevercode(e))
        }
        // `todo/tickets/chained-compare-ast-node.md`: a chained comparison is
        // a single priming scope spanning every operand (measured against
        // rakudo: `(1 < * < 10)(0)` is `False`, one arity-1 `WhateverCode`).
        // A *bare* `*` in any operand makes the whole chain curry (recursing
        // via `contains_whatever` naturally finds it). Deliberately NOT
        // checked here: `is_wrapped_whatevercode` on an operand. Unlike the
        // generic `Expr::Binary` arm below (where an already-materialized
        // `(* - 1)` composes into `(* - 1) - 1`'s enclosing curry), a chain
        // is expanded well after parsing (`crate::chain_compare::expand`,
        // at compile time), by which point an operand that is itself a
        // `WhateverCurry` marker -- whether from an explicit `(* + 1)` or
        // from `wrap_smartmatch_rhs`'s autoprime of a compound SmartMatch
        // RHS -- must stay an independent, already-scoped closure rather
        // than being absorbed into the outer chain's arity: mirrors mutsu's
        // own pre-existing behaviour, where a chain never saw through to an
        // operand's already-built closure in the first place, because by
        // the time the old code's `&&`/`DoBlock` expansion reached the
        // enclosing `should_wrap_whatevercode` check, it was already a
        // shape `contains_whatever` does not recurse into. (This does mean
        // mutsu still does not compose a parenthesized curry like
        // `1 < (* + 1) < 10` across the whole chain the way rakudo does --
        // a narrower, pre-existing divergence this ticket does not fix,
        // since fixing it would need to re-derive composition rules this
        // gate never had to begin with.) Verified against both raku and the
        // behaviour before this ticket for the case that DOES matter here:
        // `("foo" ~~ *.chars == 3) ~~ Bool` is `True` (`*.chars` stays its
        // own WhateverCode, invoked once by `~~`, `False` compares against
        // `3` uncurried) -- roast/S03-smartmatch/disorganized.t.
        Expr::ChainedCompare { operands, .. } => operands.iter().any(contains_whatever),
        // R meta-operators with Whatever: `5 R- *` should curry.
        // X/Z meta-operators with bare * in list contexts mean "extend" rather
        // than WhateverCode, so only enable for R (reverse) meta-ops.
        Expr::MetaOp {
            meta, left, right, ..
        } if meta == "R" => {
            contains_whatever(left)
                || contains_whatever(right)
                || is_wrapped_whatevercode(left)
                || is_wrapped_whatevercode(right)
        }
        _ => false,
    }
}
