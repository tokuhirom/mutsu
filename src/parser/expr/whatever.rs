//! WhateverCode detection and inspection helpers.
//!
//! Pure, read-only predicates over the `Expr` tree used to decide whether an
//! expression should be wrapped into a `WhateverCode` closure and, if so, how
//! many `*` placeholders it contains.

use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::Value;

pub(crate) fn should_wrap_whatevercode(expr: &Expr) -> bool {
    if !contains_whatever(expr) || is_whatever(expr) {
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
        } if ((name == "x" || name == "xx") && matches!(&**right, Expr::Whatever))
            || (name == "xx" && matches!(&**left, Expr::Whatever)) =>
        {
            false
        }
        _ => true,
    }
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
            ((name == "xx" || name == "x") && matches!(&**right, Expr::Whatever))
                || (name == "xx" && matches!(&**left, Expr::Whatever))
                || contains_xx_with_bare_whatever(left)
                || contains_xx_with_bare_whatever(right)
        }
        Expr::Unary { expr, .. } => contains_xx_with_bare_whatever(expr),
        Expr::MethodCall { target, args, .. } => {
            contains_xx_with_bare_whatever(target)
                || args.iter().any(contains_xx_with_bare_whatever)
        }
        Expr::Index { target, index, .. } => {
            contains_xx_with_bare_whatever(target) || contains_xx_with_bare_whatever(index)
        }
        Expr::Call { args, .. } => args.iter().any(contains_xx_with_bare_whatever),
        Expr::CallOn { target, args } => {
            contains_xx_with_bare_whatever(target)
                || args.iter().any(contains_xx_with_bare_whatever)
        }
        Expr::ArrayLiteral(items) | Expr::BracketArray(items, _) => {
            items.iter().any(contains_xx_with_bare_whatever)
        }
        Expr::CaptureLiteral(items) => items.iter().any(contains_xx_with_bare_whatever),
        _ => false,
    }
}

pub(crate) fn is_whatever(expr: &Expr) -> bool {
    matches!(expr, Expr::Whatever)
}

/// True when `expr` is an already-wrapped WhateverCode closure (produced by a
/// parenthesized curry such as `(* - 1)`). Such a closure is opaque as a *value*
/// (e.g. when passed as an argument or stored in a variable), but when it appears
/// as an *operand* of a further operator/method in the same expression, Raku
/// composes it into a new, larger WhateverCode (`(* - 1) - 1`, `(^*).roll`,
/// `1 +< (* - 1)`). The currying machinery (`count_whatever` /
/// `replace_whatever_*`) already knows how to inline such a closure; this helper
/// lets the composing operand positions detect it.
fn is_wrapped_whatevercode(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Lambda {
            is_whatever_code: true,
            ..
        } | Expr::AnonSubParams {
            is_whatever_code: true,
            ..
        }
    )
}

pub(crate) fn contains_whatever(expr: &Expr) -> bool {
    match expr {
        e if is_whatever(e) => true,
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
        } if matches!(left.as_ref(), Expr::Literal(Value::Str(_))) => false,
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
        // Pseudo-methods (.WHAT, .WHO, .HOW, etc.) are always evaluated immediately
        // on Whatever, they don't create WhateverCode. e.g. *.WHAT returns (Whatever).
        Expr::MethodCall { target, name, .. }
            if matches!(
                name.resolve().as_str(),
                "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "DEFINITE" | "VAR"
            ) && is_whatever(target) =>
        {
            false
        }
        Expr::MethodCall { target, .. } | Expr::DynamicMethodCall { target, .. } => {
            contains_whatever(target) || is_wrapped_whatevercode(target)
        }
        Expr::CallOn { target, args } => {
            // Already-wrapped WhateverCode args (Lambda/AnonSubParams with
            // is_whatever_code: true) are opaque values, not raw Whatever
            // placeholders.  Only bare * or compound-Whatever args should
            // trigger auto-currying of the whole CallOn.
            contains_whatever(target)
                || args.iter().any(|a| {
                    !matches!(
                        a,
                        Expr::Lambda {
                            is_whatever_code: true,
                            ..
                        } | Expr::AnonSubParams {
                            is_whatever_code: true,
                            ..
                        }
                    ) && contains_whatever(a)
                })
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

/// Count the number of distinct Whatever (`*`) placeholders in an expression.
pub(crate) fn count_whatever(expr: &Expr) -> usize {
    match expr {
        e if is_whatever(e) => 1,
        // A nested, already-wrapped WhateverCode operand (e.g. `(* - 1)` inside
        // `(* - 1) - 1`) contributes its own placeholder count: a single-param
        // `_`/`__wc_0` lambda counts as 1, a multi-param one as its arity.
        Expr::Lambda {
            is_whatever_code: true,
            ..
        } => 1,
        Expr::AnonSubParams {
            is_whatever_code: true,
            params,
            ..
        } => params.len(),
        Expr::Binary {
            left,
            op: TokenKind::AndAnd,
            right,
        } => {
            if let (
                Expr::Binary {
                    left: ll,
                    right: lr,
                    ..
                },
                Expr::Binary {
                    left: rl,
                    right: rr,
                    ..
                },
            ) = (left.as_ref(), right.as_ref())
                && exprs_structurally_eq(lr, rl)
                && count_whatever(lr) > 0
            {
                // Chained comparison `a OP m OP b` is expanded to
                // `(a OP m) && (m OP b)` with the middle `m` duplicated. Count the
                // shared middle's placeholders once so the WhateverCode arity is
                // the number of distinct operands, not double the middle.
                return count_whatever(ll) + count_whatever(lr) + count_whatever(rr);
            }
            count_whatever(left) + count_whatever(right)
        }
        // For range operators: count Whatever in endpoints only when
        // the endpoint contains compound Whatever (not bare *).
        Expr::Binary {
            op:
                TokenKind::DotDot
                | TokenKind::DotDotCaret
                | TokenKind::CaretDotDot
                | TokenKind::CaretDotDotCaret,
            left,
            right,
        } => {
            let lc = if contains_whatever(left) && !is_whatever(left) {
                count_whatever(left)
            } else {
                0
            };
            let rc = if contains_whatever(right) && !is_whatever(right) {
                count_whatever(right)
            } else {
                0
            };
            lc + rc
        }
        Expr::Binary {
            op: TokenKind::DotDotDot | TokenKind::DotDotDotCaret,
            ..
        } => 0,
        // SmartMatch/BangTilde: Whatever on RHS is handled at runtime; only count LHS.
        Expr::Binary {
            op: TokenKind::SmartMatch | TokenKind::BangTilde,
            left,
            ..
        } => count_whatever(left),
        Expr::Binary { left, right, .. } => count_whatever(left) + count_whatever(right),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => count_whatever(expr),
        Expr::MethodCall { target, .. } | Expr::DynamicMethodCall { target, .. } => {
            count_whatever(target)
        }
        Expr::CallOn { target, args } => {
            count_whatever(target) + args.iter().map(count_whatever).sum::<usize>()
        }
        // Only check target, not index (subscript handles its own WhateverCode)
        Expr::Index { target, .. } => count_whatever(target),
        // User-defined infix operators
        Expr::InfixFunc { left, right, .. } => {
            count_whatever(left) + right.iter().map(count_whatever).sum::<usize>()
        }
        // R/X/Z meta-operators. R always curries on a Whatever operand; X/Z
        // currying is decided at parse time in `container.rs` (a standalone `*`
        // operand curries, a trailing `*` in a comma-list operand extends), but
        // once the decision to wrap is made we must count placeholders here so
        // the WhateverCode gets the right arity.
        Expr::MetaOp {
            meta, left, right, ..
        } if matches!(meta.as_str(), "R" | "X" | "Z") => {
            count_whatever(left) + count_whatever(right)
        }
        _ => 0,
    }
}

/// Structural equality of two expressions, used to detect the shared middle
/// term of an expanded chained comparison (`a OP m OP b` => `(a OP m) && (m OP
/// b)`). `Expr` cannot derive `PartialEq` (it embeds `Value`), and this only runs
/// while wrapping a WhateverCode, so a `Debug`-string comparison is sufficient.
pub(crate) fn exprs_structurally_eq(a: &Expr, b: &Expr) -> bool {
    format!("{a:?}") == format!("{b:?}")
}

/// Check if an expression contains a reference to $_ (the topic variable).
/// Used to determine whether a WhateverCode lambda should avoid using $_ as its param.
pub(crate) fn expr_contains_topic(expr: &Expr) -> bool {
    match expr {
        Expr::Var(name) if name == "_" => true,
        Expr::Whatever => false,
        Expr::Binary { left, right, .. } => expr_contains_topic(left) || expr_contains_topic(right),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => expr_contains_topic(expr),
        Expr::MethodCall { target, args, .. } => {
            expr_contains_topic(target) || args.iter().any(expr_contains_topic)
        }
        Expr::CallOn { target, args } => {
            expr_contains_topic(target) || args.iter().any(expr_contains_topic)
        }
        Expr::Index { target, index, .. } => {
            expr_contains_topic(target) || expr_contains_topic(index)
        }
        Expr::InfixFunc { left, right, .. } => {
            expr_contains_topic(left) || right.iter().any(expr_contains_topic)
        }
        Expr::MetaOp { left, right, .. } => expr_contains_topic(left) || expr_contains_topic(right),
        _ => false,
    }
}
