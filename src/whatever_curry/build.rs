//! WhateverCode closure construction: expanding an `Expr::WhateverCurry`
//! marker's un-curried body into the `Lambda` / `AnonSubParams` closure the
//! parser used to build eagerly (`wrap_whatevercode`, pre-ADR-0033).

use super::replace::{replace_whatever_numbered, replace_whatever_single};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::parser::{contains_whatever, is_whatever, should_wrap_whatevercode};
use crate::symbol::Symbol;
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

    // A HyperWhatever primes the expression with a slurpy positional parameter
    // and maps the primed body over every supplied argument. Reuse the ordinary
    // single-Whatever substitution for the per-item callback so all operators
    // and postfix chains retain their normal compiler path.
    if wc_count == 0 && contains_hyperwhatever(expr) {
        let item_body = replace_whatever_single(expr);
        let mapper = Expr::Lambda {
            param: "_".to_string(),
            body: vec![Stmt::Expr(item_body)],
            is_whatever_code: true,
            param_sigilless: false,
        };
        let args_name = "@__hw_args".to_string();
        let mut args_def = make_wc_param(args_name.clone());
        args_def.slurpy = true;
        return Expr::AnonSubParams {
            params: vec![args_name.clone()],
            param_defs: vec![args_def],
            return_type: None,
            body: vec![Stmt::Expr(Expr::MethodCall {
                target: Box::new(Expr::ArrayVar("__hw_args".to_string())),
                name: Symbol::intern("map"),
                args: vec![mapper],
                modifier: None,
                quoted: false,
            })],
            is_rw: false,
            is_whatever_code: true,
        };
    }

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

/// Whether this priming scope contains a HyperWhatever placeholder.
///
/// `contains_whatever` also reports composed, already-planted ordinary
/// `WhateverCurry` operands.  Those can have a zero visible placeholder count
/// at an enclosing scope, so using that broader predicate here would mistake
/// ordinary compositions such as `(^*).roll` for HyperWhatever and give them
/// slurpy/map semantics.
fn contains_hyperwhatever(expr: &Expr) -> bool {
    match expr {
        Expr::HyperWhatever => true,
        Expr::WhateverCurry(inner) => contains_hyperwhatever(inner),
        e if super::plant::is_thunk_barrier(e) => false,
        Expr::ChainedCompare { operands, .. } => operands.iter().any(contains_hyperwhatever),
        Expr::Binary { left, right, .. } => {
            contains_hyperwhatever(left) || contains_hyperwhatever(right)
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => contains_hyperwhatever(expr),
        Expr::MethodCall { target, .. }
        | Expr::DynamicMethodCall { target, .. }
        | Expr::HyperMethodCall { target, .. }
        | Expr::HyperMethodCallDynamic { target, .. }
        | Expr::CallOn { target, .. }
        | Expr::Index { target, .. } => contains_hyperwhatever(target),
        Expr::InfixFunc { left, right, .. } => {
            contains_hyperwhatever(left) || right.iter().any(contains_hyperwhatever)
        }
        Expr::MetaOp { left, right, .. } => {
            contains_hyperwhatever(left) || contains_hyperwhatever(right)
        }
        _ => false,
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
        // A thunk barrier is opaque to the enclosing priming scope: its operands
        // are scopes of their own (already materialised as `WhateverCurry`
        // markers by `super::plant`), so they contribute no placeholder to the
        // arity of whatever encloses them. ADR-0033 Phase 4.
        e if super::plant::is_thunk_barrier(e) => 0,
        // `todo/tickets/chained-compare-ast-node.md`: unlike the retired
        // `TokenKind::ChainAnd` expansion, `operands` is never duplicated —
        // each distinct operand appears exactly once — so the arity is simply
        // the sum of each operand's own count, EXCEPT the final operand when
        // the chain's last link is a SmartMatch/BangTilde (mirrors the
        // SmartMatch/BangTilde arm below: only a bare `*` RHS counts, a
        // compound one autoprimes independently). See the matching
        // `contains_whatever` arm for why only the *last* operand can ever
        // be in that RHS role.
        Expr::ChainedCompare { operands, ops } => {
            let last_is_smartmatch_rhs = ops
                .last()
                .is_some_and(|(op, _)| matches!(op, TokenKind::SmartMatch | TokenKind::BangTilde));
            let (init, last) = operands.split_at(operands.len() - 1);
            let init_sum: usize = init.iter().map(count_whatever).sum();
            let last_contrib = if last_is_smartmatch_rhs {
                usize::from(is_whatever(&last[0]))
            } else {
                count_whatever(&last[0])
            };
            init_sum + last_contrib
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
        // A zen slice is the empty-subscript postcircumfix; like `Index`, only
        // its target can hold the `*` that decides the arity.
        Expr::ZenSlice(target) => count_whatever(target),
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

/// Check if an expression contains a reference to $_ (the topic variable).
/// Used to determine whether a WhateverCode lambda should avoid using $_ as its param.
pub(crate) fn expr_contains_topic(expr: &Expr) -> bool {
    match expr {
        Expr::Var(name) if name == "_" => true,
        Expr::Whatever | Expr::WhateverArg => false,
        Expr::WhateverCurry(inner) => expr_contains_topic(inner),
        Expr::ChainedCompare { operands, .. } => operands.iter().any(expr_contains_topic),
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
        Expr::ZenSlice(target) => expr_contains_topic(target),
        Expr::InfixFunc { left, right, .. } => {
            expr_contains_topic(left) || right.iter().any(expr_contains_topic)
        }
        Expr::MetaOp { left, right, .. } => expr_contains_topic(left) || expr_contains_topic(right),
        _ => false,
    }
}
