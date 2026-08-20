//! WhateverCode closure construction: expanding an `Expr::WhateverCurry`
//! marker's un-curried body into the `Lambda` / `AnonSubParams` closure the
//! parser used to build eagerly (`wrap_whatevercode`, pre-ADR-0033).

use super::replace::{replace_whatever_numbered, replace_whatever_single};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::parser::{contains_whatever, is_whatever, should_wrap_whatevercode};
use crate::token_kind::TokenKind;

pub(crate) fn make_wc_param(name: String) -> ParamDef {
    ParamDef {
        name,
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
    }
}

/// A WhateverCode parameter (`__wc_N`) is `is raw`, so `*++`, `*.=foo` and
/// `* =:= $x` write back to the caller's container.
fn make_wc_param_raw(name: String) -> ParamDef {
    ParamDef {
        traits: vec!["raw".to_string()],
        ..make_wc_param(name)
    }
}

/// Build a WhateverCode lambda from an expression containing Whatever
/// placeholders. This is the closure-construction step ADR-0033 defers out of
/// the parser: the parser now wraps the un-curried body in
/// `Expr::WhateverCurry`, and the compiler calls this from its
/// `Expr::WhateverCurry` arm at compile time. The result is exactly what the
/// parser used to build on the spot, so emitted bytecode is unchanged.
pub(crate) fn build_closure(expr: &Expr) -> Expr {
    if let Expr::CallOn { target, args } = expr
        && should_wrap_whatevercode(target)
        && !args.iter().any(contains_whatever)
    {
        return Expr::CallOn {
            target: Box::new(build_closure(target)),
            args: args.clone(),
        };
    }

    let wc_count = count_whatever(expr);

    if wc_count <= 1 && !expr_contains_topic(expr) {
        // Single-arg: use Lambda with param "_" for backward compat (this keeps the
        // `deepmap`/hyper container-passing path working, which binds each leaf to
        // the topic `_`). `compile_expr_lambda` marks `_` `is raw` when the body
        // mutates the placeholder (`*++`, `* =:= $x`), so mutation/identity work.
        let body_expr = replace_whatever_single(expr);
        Expr::Lambda {
            param: "_".to_string(),
            body: vec![Stmt::Expr(body_expr)],
            is_whatever_code: true,
            param_sigilless: false,
        }
    } else if wc_count <= 1 {
        // Single-arg, but expression already contains $_ — use a numbered param
        // to avoid shadowing the outer $_.
        let mut counter = 0;
        let body_expr = replace_whatever_numbered(expr, &mut counter);
        let param_name = "__wc_0".to_string();
        Expr::AnonSubParams {
            params: vec![param_name.clone()],
            param_defs: vec![make_wc_param_raw(param_name)],
            return_type: None,
            body: vec![Stmt::Expr(body_expr)],
            is_rw: false,
            is_whatever_code: true,
        }
    } else {
        // Multi-arg: use AnonSubParams with numbered params
        let mut counter = 0;
        let body_expr = replace_whatever_numbered(expr, &mut counter);
        let params: Vec<String> = (0..counter).map(|i| format!("__wc_{i}")).collect();
        Expr::AnonSubParams {
            params: params.clone(),
            param_defs: params.iter().cloned().map(make_wc_param_raw).collect(),
            return_type: None,
            body: vec![Stmt::Expr(body_expr)],
            is_rw: false,
            is_whatever_code: true,
        }
    }
}

/// Count the number of distinct Whatever (`*`) placeholders in an expression.
pub(crate) fn count_whatever(expr: &Expr) -> usize {
    match expr {
        e if is_whatever(e) => 1,
        // A nested, already-planted WhateverCurry operand (e.g. `(* - 1)`
        // inside `(* - 1) - 1`) contributes its own un-curried placeholder
        // count. `count_whatever` already handles the chained-comparison
        // dedup below, so recursing here yields exactly the arity
        // `build_closure` would give it if built standalone.
        Expr::WhateverCurry(inner) => count_whatever(inner),
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
        // SmartMatch/BangTilde: a *compound* Whatever on the RHS is left for
        // runtime autoprime (not curried), so only LHS counts in general. But
        // the RHS-autoprime forms `X ~~ *` / `X !~~ *` (ADR-0033 Phase 2 section
        // 2.5) plant a `WhateverCurry` directly over this Binary with a *bare*
        // placeholder on the right, and that placeholder must be counted too —
        // it is the only one when the LHS has none (`Int ~~ *`).
        Expr::Binary {
            op: TokenKind::SmartMatch | TokenKind::BangTilde,
            left,
            right,
        } => count_whatever(left) + usize::from(is_whatever(right)),
        Expr::Binary { left, right, .. } => count_whatever(left) + count_whatever(right),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => count_whatever(expr),
        Expr::MethodCall { target, .. }
        | Expr::DynamicMethodCall { target, .. }
        | Expr::HyperMethodCall { target, .. }
        | Expr::HyperMethodCallDynamic { target, .. } => count_whatever(target),
        Expr::CallOn { target, .. } => {
            // Only the *target* of an invocation curries. A Whatever passed as a
            // call *argument* (`$sub(*)`, `&infix:<+>(*, 42)`) is a Whatever value
            // handed to the callee, NOT a placeholder of the enclosing
            // WhateverCode — so it must not add to the arity. This mirrors
            // `contains_whatever`, which already only inspects the target.
            count_whatever(target)
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
        Expr::Whatever | Expr::WhateverArg => false,
        Expr::WhateverCurry(inner) => expr_contains_topic(inner),
        Expr::Binary { left, right, .. } => expr_contains_topic(left) || expr_contains_topic(right),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => expr_contains_topic(expr),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
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
