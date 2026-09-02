//! Expression-level recursion for the `*` leaf classifier (see `super`'s
//! module doc). This is where the section 2.1 leaf-classification table
//! actually lives: each arm below picks, for its own operand positions,
//! whether a nested `*` stays a value (`mark_value_leaf`) or becomes an
//! argument (the default `mark_expr` recursion, which converts a bare
//! `Expr::Whatever` it reaches directly).

use super::{mark_opt_value_leaf, mark_stmts, mark_value_leaf};
use crate::ast::Expr;
use crate::token_kind::TokenKind;

fn is_flipflop(name: &str) -> bool {
    matches!(
        name,
        "ff" | "fff" | "^ff" | "ff^" | "^ff^" | "^fff" | "fff^" | "^fff^"
    )
}

/// The six non-currying pseudo-methods: `*.WHAT` evaluates eagerly on the
/// `Whatever` value itself rather than curried, so their target is a value
/// position (`*.WHICH` / `*.abs` are ordinary methods and DO curry).
fn is_noncurrying_pseudo_method(name: &str) -> bool {
    matches!(name, "WHAT" | "WHO" | "HOW" | "WHERE" | "DEFINITE" | "VAR")
}

pub(super) fn mark_expr(expr: &mut Expr) {
    // ADR-0033 Phase 4: this same top-down walk is also the priming-*scope*
    // authority. At a thunk barrier (or a ternary) each operand is its own
    // scope, so plant a `WhateverCurry` marker around it *before* recursing —
    // the recursion below then classifies the leaves inside the new marker
    // exactly as it would have classified them in place.
    crate::whatever_curry::plant_here(expr);
    match expr {
        Expr::Whatever => *expr = Expr::WhateverArg,
        // Already classified (re-running mark_expr should never happen in
        // practice, but stay idempotent) or out of scope for priming (`**`).
        Expr::WhateverArg | Expr::HyperWhatever => {}
        // A marker's un-curried body is exactly the "argument" role: recurse
        // straight through, no wrapper of its own.
        Expr::WhateverCurry(inner) => mark_expr(inner),
        Expr::Grouped(inner) => mark_expr(inner),
        // Comma-list positions: `1, *, 2`, `[*]`, `\(*, 1)`.
        Expr::ArrayLiteral(items) | Expr::BracketArray(items, _) | Expr::CaptureLiteral(items) => {
            for item in items {
                mark_value_leaf(item);
            }
        }
        Expr::Binary { left, op, right } => match op {
            // Range/series endpoints: `1..*` / `1..*-1`, `1,2...*`.
            TokenKind::DotDot
            | TokenKind::DotDotCaret
            | TokenKind::CaretDotDot
            | TokenKind::CaretDotDotCaret
            | TokenKind::DotDotDot
            | TokenKind::DotDotDotCaret => {
                mark_value_leaf(left);
                mark_value_leaf(right);
            }
            // `xx` replicates its operand as a value; `x` (string repeat)
            // curries both operands instead (`1 x *` is `Argument` even
            // though mutsu plants no `WhateverCurry` there — ADR-0033
            // section 2.1's scope-independence example) and so takes the
            // default arm below.
            TokenKind::Ident(name) if name == "xx" => {
                mark_value_leaf(left);
                mark_value_leaf(right);
            }
            _ => {
                mark_expr(left);
                mark_expr(right);
            }
        },
        // `todo/tickets/chained-compare-ast-node.md`: each operand is an
        // ordinary argument position, same as a `Binary` comparison's
        // operands (the chain as a whole is one priming scope, not a thunk
        // barrier — see `is_thunk_barrier`/`contains_whatever`).
        Expr::ChainedCompare { operands, .. } => {
            for o in operands {
                mark_expr(o);
            }
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => mark_expr(expr),
        Expr::MethodCall {
            target, name, args, ..
        } => {
            if is_noncurrying_pseudo_method(name.resolve().as_str()) {
                mark_value_leaf(target);
            } else {
                mark_expr(target);
            }
            for a in args {
                mark_value_leaf(a);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        } => {
            mark_expr(target);
            mark_expr(name_expr);
            for a in args {
                mark_value_leaf(a);
            }
        }
        Expr::HyperMethodCall { target, args, .. } => {
            mark_expr(target);
            for a in args {
                mark_value_leaf(a);
            }
        }
        Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            mark_expr(target);
            mark_expr(name_expr);
            for a in args {
                mark_value_leaf(a);
            }
        }
        // `*(1)` invokes the bare Whatever *value* (dies at runtime — no
        // CALL-ME); only a compound target (`(*+1)(5)`) curries.
        Expr::CallOn { target, args } => {
            mark_value_leaf(target);
            for a in args {
                mark_value_leaf(a);
            }
        }
        // Whole-slice subscript: `@a[*]` is a value, `@a[*-1]` is compound.
        Expr::Index { target, index, .. } => {
            mark_expr(target);
            mark_value_leaf(index);
        }
        Expr::MultiDimIndex {
            target, dimensions, ..
        } => {
            mark_expr(target);
            for d in dimensions {
                mark_value_leaf(d);
            }
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
            ..
        } => {
            mark_expr(target);
            for d in dimensions {
                mark_value_leaf(d);
            }
            mark_value_leaf(value);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            mark_expr(target);
            mark_value_leaf(index);
            mark_value_leaf(value);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            mark_expr(cond);
            mark_expr(then_expr);
            mark_expr(else_expr);
        }
        // `($x = *)` / `($x := *)` in expression position.
        Expr::AssignExpr { expr, .. } => mark_value_leaf(expr),
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                mark_value_leaf(a);
            }
        }
        // A non-bareword `=>` pair value is a call/method-argument-like
        // value position (`"k" => *` — but only the bareword-key form
        // reaches this arm via `PositionalPair`; a plain quoted-key pair is
        // already planted in a `WhateverCurry` by the parser and recurses
        // via the generic `Expr::WhateverCurry` arm instead).
        Expr::PositionalPair(inner) => match inner.as_mut() {
            Expr::Binary {
                op: TokenKind::FatArrow,
                right,
                ..
            } => mark_value_leaf(right),
            other => mark_expr(other),
        },
        Expr::InfixFunc {
            name, left, right, ..
        } => {
            if is_flipflop(name) {
                mark_value_leaf(left);
                for r in right {
                    mark_value_leaf(r);
                }
            } else {
                mark_expr(left);
                for r in right {
                    mark_expr(r);
                }
            }
        }
        Expr::HyperOp { left, right, .. } | Expr::HyperFuncOp { left, right, .. } => {
            mark_expr(left);
            mark_expr(right);
        }
        // Z/X/R meta-operators: a Whatever operand is always `Argument`
        // (measured), even for Z/X where mutsu plants no `WhateverCurry`.
        Expr::MetaOp { left, right, .. } => {
            mark_expr(left);
            mark_expr(right);
        }
        Expr::Reduction { expr, .. } => mark_expr(expr),
        Expr::Feed { source, sink, .. } => {
            mark_expr(source);
            mark_expr(sink);
        }
        Expr::Hash(pairs) => {
            for (_, value) in pairs {
                mark_opt_value_leaf(value);
            }
        }
        Expr::DoBlock { body, .. }
        | Expr::Block(body)
        | Expr::AnonSub { body, .. }
        | Expr::Gather(body)
        | Expr::AnonSubParams { body, .. }
        | Expr::Lambda { body, .. }
        | Expr::PhaserExpr { body, .. }
        | Expr::Once { body } => mark_stmts(body),
        Expr::Try { body, catch } => {
            mark_stmts(body);
            if let Some(catch) = catch {
                mark_stmts(catch);
            }
        }
        Expr::DoStmt(stmt) => super::stmt::mark_stmt(stmt),
        Expr::Exists { target, arg, .. } => {
            mark_expr(target);
            if let Some(arg) = arg {
                mark_expr(arg);
            }
        }
        Expr::ZenSlice(inner)
        | Expr::Eager(inner)
        | Expr::Itemize(inner)
        | Expr::DeitemizeForBind(inner)
        | Expr::IndirectTypeLookup(inner) => mark_expr(inner),
        Expr::IndirectCodeLookup { package, .. } => mark_expr(package),
        Expr::SymbolicDeref { expr, .. } => mark_expr(expr),
        Expr::SymbolicDerefAssign { expr, value, .. } => {
            mark_expr(expr);
            mark_value_leaf(value);
        }
        Expr::IndirectTypeLookupAssign { expr, value } => {
            mark_expr(expr);
            mark_value_leaf(value);
        }
        Expr::HyperSlice { target, .. } => mark_expr(target),
        Expr::StringInterpolation(parts) => {
            for p in parts {
                mark_expr(p);
            }
        }
        // Terminal / irrelevant to Whatever-priming: no `Expr` children (or
        // children that cannot syntactically hold a bare `*`, e.g. regex/
        // substitution literals whose pattern is stored as raw text).
        Expr::Literal(_)
        | Expr::LiteralSrc(_, _)
        | Expr::BareWord(_)
        | Expr::HeredocInterpolation(_, _)
        | Expr::Var(_)
        | Expr::CaptureVar(_)
        | Expr::ArrayVar(_)
        | Expr::HashVar(_)
        | Expr::CodeVar(_)
        | Expr::EnvIndex(_)
        | Expr::MatchRegex(_)
        | Expr::Subst { .. }
        | Expr::NonDestructiveSubst { .. }
        | Expr::Transliterate { .. }
        | Expr::RoutineMagic
        | Expr::BlockMagic
        | Expr::ControlFlow { .. }
        | Expr::PseudoStash(_) => {}
    }
}
