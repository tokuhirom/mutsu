//! The exhaustive default mutable recursion over [`Expr`] (ADR-10499). It
//! mirrors [`super::walk_expr()`] child for child.

use super::{VisitMut, exprs_mut, params_mut, traits_mut, walk_regex_tree_mut};
use crate::ast::Expr;

/// Visits every child of `e` (statements, expressions, parameters, regex
/// nodes) mutably.
// Cost: O(n), n = size of `e`'s subtree.
pub(crate) fn walk_expr_mut<V: VisitMut + ?Sized>(v: &mut V, e: &mut Expr) {
    match e {
        Expr::Literal(_value) => {}
        Expr::ShadowableTermKeyword { name: _, value: _ } => {}
        Expr::ExportTermOrCall { name: _, call } => v.visit_expr_mut(call),
        Expr::RegexLiteral { value: _, tree } | Expr::MatchRegexTree { value: _, tree } => {
            walk_regex_tree_mut(v, tree)
        }
        Expr::LiteralSrc(_value, _source) => {}
        Expr::Grouped(inner)
        | Expr::ZenSlice(inner)
        | Expr::WhateverCurry(inner)
        | Expr::PositionalPair(inner)
        | Expr::Eager(inner)
        | Expr::Itemize(inner)
        | Expr::DeitemizeForBind(inner)
        | Expr::IndirectTypeLookup(inner)
        | Expr::IndirectTypeLookupTail { head: inner, .. } => v.visit_expr_mut(inner),
        Expr::GivenPointyTopic
        | Expr::Whatever
        | Expr::WhateverArg
        | Expr::HyperWhatever
        | Expr::RoutineMagic
        | Expr::BlockMagic => {}
        Expr::BareWord(_name) => {}
        Expr::UserRoutineCall { name: _, args } => exprs_mut(v, args),
        Expr::StringInterpolation(parts)
        | Expr::ArrayLiteral(parts)
        | Expr::BracketArray(parts, _)
        | Expr::CaptureLiteral(parts, _) => exprs_mut(v, parts),
        Expr::HeredocInterpolation(_source, _) => {}
        Expr::Var(_name)
        | Expr::CaptureVar(_name)
        | Expr::ArrayVar(_name)
        | Expr::HashVar(_name)
        | Expr::CodeVar(_name) => {}
        Expr::EnvIndex(_key) => {}
        Expr::MatchRegex(_value) => {}
        Expr::MatchRegexDynamicAdverbs {
            value: _,
            pos_expr,
            continue_expr,
        } => {
            for e in [pos_expr, continue_expr].into_iter().flatten() {
                v.visit_expr_mut(e);
            }
        }
        Expr::Subst {
            pattern: _,
            replacement: _,
            samecase: _,
            sigspace: _,
            samemark: _,
            samespace: _,
            global: _,
            nth: _,
            x: _,
            replacement_thunk,
        }
        | Expr::NonDestructiveSubst {
            pattern: _,
            replacement: _,
            samecase: _,
            sigspace: _,
            samemark: _,
            samespace: _,
            global: _,
            nth: _,
            x: _,
            replacement_thunk,
        } => {
            if let Some(e) = replacement_thunk {
                v.visit_expr_mut(e);
            }
        }
        Expr::Transliterate {
            from: _,
            to: _,
            delete: _,
            complement: _,
            squash: _,
            non_destructive: _,
        } => {}
        Expr::Contextualizer { kind: _, inner } => v.visit_expr_mut(inner),
        Expr::MethodCall {
            target,
            name: _,
            args,
            modifier: _,
            quoted: _,
        }
        | Expr::HyperMethodCall {
            target,
            name: _,
            args,
            modifier: _,
            quoted: _,
        } => {
            v.visit_expr_mut(target);
            exprs_mut(v, args);
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier: _,
            quoted: _,
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            modifier: _,
        } => {
            v.visit_expr_mut(target);
            v.visit_expr_mut(name_expr);
            exprs_mut(v, args);
        }
        Expr::Exists {
            target,
            negated: _,
            delete: _,
            arg,
            adverb: _,
        } => {
            v.visit_expr_mut(target);
            if let Some(a) = arg {
                v.visit_expr_mut(a);
            }
        }
        Expr::PhaserExpr { kind: _, body }
        | Expr::Once { body }
        | Expr::Block(body)
        | Expr::Gather(body)
        | Expr::AnonSub {
            body,
            is_rw: _,
            is_raw: _,
            is_block: _,
            doc: _,
        } => v.visit_stmts_mut(body),
        Expr::AnonSubParams {
            params: _,
            param_defs,
            return_type: _,
            body,
            is_rw: _,
            is_raw: _,
            custom_traits,
            is_whatever_code: _,
            declarator: _,
        } => {
            params_mut(v, param_defs);
            traits_mut(v, custom_traits.as_mut_slice());
            v.visit_stmts_mut(body);
        }
        Expr::CallOn { target, args } => {
            v.visit_expr_mut(target);
            exprs_mut(v, args);
        }
        Expr::Lambda {
            param: _,
            body,
            is_whatever_code: _,
            param_sigilless: _,
        } => v.visit_stmts_mut(body),
        Expr::Index {
            target,
            index,
            is_positional: _,
        } => {
            v.visit_expr_mut(target);
            v.visit_expr_mut(index);
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional: _,
        } => {
            v.visit_expr_mut(target);
            exprs_mut(v, dimensions);
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
            is_positional: _,
        } => {
            v.visit_expr_mut(target);
            exprs_mut(v, dimensions);
            v.visit_expr_mut(value);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            is_positional: _,
        } => {
            v.visit_expr_mut(target);
            v.visit_expr_mut(index);
            v.visit_expr_mut(value);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            v.visit_expr_mut(cond);
            v.visit_expr_mut(then_expr);
            v.visit_expr_mut(else_expr);
        }
        Expr::AssignExpr {
            name: _,
            expr,
            is_bind: _,
        } => v.visit_expr_mut(expr),
        Expr::CompoundAssign {
            target,
            op: _,
            rhs,
            expanded,
        } => {
            v.visit_expr_mut(target);
            v.visit_expr_mut(rhs);
            v.visit_expr_mut(expanded);
        }
        Expr::Unary { op: _, expr } | Expr::PostfixOp { op: _, expr } => v.visit_expr_mut(expr),
        Expr::Binary { left, op: _, right } => {
            v.visit_expr_mut(left);
            v.visit_expr_mut(right);
        }
        Expr::ChainedCompare { operands, ops: _ } => exprs_mut(v, operands),
        Expr::Hash(pairs, _) => {
            for (_key, value) in pairs {
                if let Some(e) = value {
                    v.visit_expr_mut(e);
                }
            }
        }
        Expr::Call { name: _, args } => exprs_mut(v, args),
        Expr::Try { body, catch } => {
            v.visit_stmts_mut(body);
            if let Some(c) = catch {
                v.visit_stmts_mut(c);
            }
        }
        Expr::Reduction { op: _, expr } => v.visit_expr_mut(expr),
        Expr::InfixFunc {
            name: _,
            left,
            right,
            modifier: _,
        } => {
            v.visit_expr_mut(left);
            exprs_mut(v, right);
        }
        Expr::HyperOp {
            op: _,
            left,
            right,
            dwim_left: _,
            dwim_right: _,
        }
        | Expr::HyperFuncOp {
            func_name: _,
            left,
            right,
            dwim_left: _,
            dwim_right: _,
        }
        | Expr::MetaOp {
            meta: _,
            op: _,
            left,
            right,
        } => {
            v.visit_expr_mut(left);
            v.visit_expr_mut(right);
        }
        Expr::Feed {
            source,
            sink,
            append: _,
            left_is_source: _,
        } => {
            v.visit_expr_mut(source);
            v.visit_expr_mut(sink);
        }
        Expr::DoBlock {
            body,
            label: _,
            origin: _,
        } => v.visit_stmts_mut(body),
        Expr::DoStmt(stmt) => v.visit_stmt_mut(stmt),
        Expr::ControlFlow {
            kind: _,
            label: _,
            value,
            take_value: _,
        } => {
            if let Some(value) = value {
                v.visit_expr_mut(value);
            }
        }
        Expr::IndirectCodeLookup { package, name: _ } => v.visit_expr_mut(package),
        Expr::SymbolicDeref { sigil: _, expr } => v.visit_expr_mut(expr),
        Expr::SymbolicDerefAssign {
            sigil: _,
            expr,
            value,
        }
        | Expr::IndirectTypeLookupAssign { expr, value } => {
            v.visit_expr_mut(expr);
            v.visit_expr_mut(value);
        }
        Expr::PseudoStash(_name) => {}
        Expr::HyperSlice { target, adverb: _ } => v.visit_expr_mut(target),
    }
}
