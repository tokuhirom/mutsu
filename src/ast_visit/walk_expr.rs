//! The exhaustive default recursion over [`Expr`]. See the module doc of
//! [`super`] for why every field is named.

use super::{NameKind, Visit, exprs, names, params, traits, walk_literal, walk_regex_tree};
use crate::ast::Expr;

/// Visits every child of `e` (statements, expressions, parameters, regex
/// nodes) and reports every identifier `e` itself holds.
// Cost: O(n), n = size of `e`'s subtree.
pub(crate) fn walk_expr<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, e: &'ast Expr) {
    match e {
        Expr::Literal(value) => walk_literal(v, value),
        Expr::ShadowableTermKeyword { name, value } => {
            v.visit_name(name.as_str(), NameKind::Term);
            walk_literal(v, value);
        }
        Expr::ExportTermOrCall { name, call } => {
            v.visit_name(name.as_str(), NameKind::Term);
            v.visit_expr(call);
        }
        Expr::RegexLiteral { value, tree } | Expr::MatchRegexTree { value, tree } => {
            walk_literal(v, value);
            walk_regex_tree(v, tree);
        }
        Expr::LiteralSrc(value, _source) => walk_literal(v, value),
        Expr::Grouped(inner)
        | Expr::ZenSlice(inner)
        | Expr::WhateverCurry(inner)
        | Expr::PositionalPair(inner)
        | Expr::Eager(inner)
        | Expr::Itemize(inner)
        | Expr::DeitemizeForBind(inner)
        | Expr::IndirectTypeLookup(inner)
        | Expr::IndirectTypeLookupTail { head: inner, .. } => v.visit_expr(inner),
        Expr::Whatever
        | Expr::WhateverArg
        | Expr::HyperWhatever
        | Expr::RoutineMagic
        | Expr::BlockMagic => {}
        Expr::BareWord(name) => v.visit_name(name, NameKind::Term),
        Expr::UserRoutineCall { name, args } => {
            v.visit_name(name.as_str(), NameKind::UserRoutineCall);
            exprs(v, args);
        }
        Expr::StringInterpolation(parts)
        | Expr::ArrayLiteral(parts)
        | Expr::BracketArray(parts, _)
        | Expr::CaptureLiteral(parts) => exprs(v, parts),
        Expr::HeredocInterpolation(source, _) => v.visit_name(source, NameKind::Source),
        Expr::Var(name) => v.visit_name(name, NameKind::Var),
        Expr::CaptureVar(name) => v.visit_name(name, NameKind::CaptureVar),
        Expr::ArrayVar(name) => v.visit_name(name, NameKind::ArrayVar),
        Expr::HashVar(name) => v.visit_name(name, NameKind::HashVar),
        Expr::CodeVar(name) => v.visit_name(name, NameKind::CodeVar),
        // The key of `%*ENV<key>` is data.
        Expr::EnvIndex(_key) => {}
        Expr::MatchRegex(value) => walk_literal(v, value),
        Expr::MatchRegexDynamicAdverbs {
            value,
            pos_expr,
            continue_expr,
        } => {
            walk_literal(v, value);
            for e in [pos_expr, continue_expr].into_iter().flatten() {
                v.visit_expr(e);
            }
        }
        Expr::Subst {
            pattern,
            replacement,
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
            pattern,
            replacement,
            samecase: _,
            sigspace: _,
            samemark: _,
            samespace: _,
            global: _,
            nth: _,
            x: _,
            replacement_thunk,
        } => {
            v.visit_name(pattern, NameKind::Source);
            v.visit_name(replacement, NameKind::Source);
            if let Some(e) = replacement_thunk {
                v.visit_expr(e);
            }
        }
        // The two character tables are data.
        Expr::Transliterate {
            from: _,
            to: _,
            delete: _,
            complement: _,
            squash: _,
            non_destructive: _,
        } => {}
        Expr::Contextualizer { kind: _, inner } => v.visit_expr(inner),
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        }
        | Expr::HyperMethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } => {
            v.visit_expr(target);
            v.visit_name(name.as_str(), NameKind::Method);
            exprs(v, args);
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
            v.visit_expr(target);
            v.visit_expr(name_expr);
            exprs(v, args);
        }
        Expr::Exists {
            target,
            negated: _,
            delete: _,
            arg,
            adverb: _,
        } => {
            v.visit_expr(target);
            if let Some(a) = arg {
                v.visit_expr(a);
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
        } => super::walk_stmts(v, body),
        Expr::AnonSubParams {
            params: flat,
            param_defs,
            return_type,
            body,
            is_rw: _,
            is_raw: _,
            custom_traits,
            is_whatever_code: _,
            declarator: _,
        } => {
            names(v, flat, NameKind::BlockParam);
            params(v, param_defs);
            names(v, return_type.iter(), NameKind::Type);
            traits(v, custom_traits.as_slice());
            super::walk_stmts(v, body);
        }
        Expr::CallOn { target, args } => {
            v.visit_expr(target);
            exprs(v, args);
        }
        Expr::Lambda {
            param,
            body,
            is_whatever_code: _,
            param_sigilless: _,
        } => {
            v.visit_name(param, NameKind::BlockParam);
            super::walk_stmts(v, body);
        }
        Expr::Index {
            target,
            index,
            is_positional: _,
        } => {
            v.visit_expr(target);
            v.visit_expr(index);
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional: _,
        } => {
            v.visit_expr(target);
            exprs(v, dimensions);
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
            is_positional: _,
        } => {
            v.visit_expr(target);
            exprs(v, dimensions);
            v.visit_expr(value);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            is_positional: _,
        } => {
            v.visit_expr(target);
            v.visit_expr(index);
            v.visit_expr(value);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            v.visit_expr(cond);
            v.visit_expr(then_expr);
            v.visit_expr(else_expr);
        }
        Expr::AssignExpr {
            name,
            expr,
            is_bind: _,
        } => {
            v.visit_name(name, NameKind::AssignTarget);
            v.visit_expr(expr);
        }
        Expr::CompoundAssign {
            target,
            op,
            rhs,
            expanded,
        } => {
            v.visit_expr(target);
            v.visit_name(op, NameKind::Operator);
            v.visit_expr(rhs);
            v.visit_expr(expanded);
        }
        Expr::Unary { op: _, expr } | Expr::PostfixOp { op: _, expr } => v.visit_expr(expr),
        Expr::Binary { left, op: _, right } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        Expr::ChainedCompare { operands, ops: _ } => exprs(v, operands),
        // Hash-literal keys are data.
        Expr::Hash(pairs, _) => {
            for (_key, value) in pairs {
                if let Some(e) = value {
                    v.visit_expr(e);
                }
            }
        }
        Expr::Call { name, args } => {
            v.visit_name(name.as_str(), NameKind::Call);
            exprs(v, args);
        }
        Expr::Try { body, catch } => {
            super::walk_stmts(v, body);
            if let Some(c) = catch {
                super::walk_stmts(v, c);
            }
        }
        Expr::Reduction { op, expr } => {
            v.visit_name(op, NameKind::Operator);
            v.visit_expr(expr);
        }
        Expr::InfixFunc {
            name,
            left,
            right,
            modifier,
        } => {
            v.visit_name(name, NameKind::Operator);
            v.visit_expr(left);
            exprs(v, right);
            names(v, modifier.iter(), NameKind::Operator);
        }
        Expr::HyperOp {
            op,
            left,
            right,
            dwim_left: _,
            dwim_right: _,
        }
        | Expr::HyperFuncOp {
            func_name: op,
            left,
            right,
            dwim_left: _,
            dwim_right: _,
        } => {
            v.visit_name(op, NameKind::Operator);
            v.visit_expr(left);
            v.visit_expr(right);
        }
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } => {
            v.visit_name(meta, NameKind::Operator);
            v.visit_name(op, NameKind::Operator);
            v.visit_expr(left);
            v.visit_expr(right);
        }
        Expr::Feed {
            source,
            sink,
            append: _,
            left_is_source: _,
        } => {
            v.visit_expr(source);
            v.visit_expr(sink);
        }
        Expr::DoBlock {
            body,
            label,
            origin: _,
        } => {
            names(v, label.iter(), NameKind::Label);
            super::walk_stmts(v, body);
        }
        Expr::DoStmt(stmt) => v.visit_stmt(stmt),
        Expr::ControlFlow { kind: _, label } => names(v, label.iter(), NameKind::Label),
        Expr::IndirectCodeLookup { package, name } => {
            v.visit_expr(package);
            v.visit_name(name, NameKind::Symbolic);
        }
        Expr::SymbolicDeref { sigil, expr } => {
            v.visit_name(sigil, NameKind::Symbolic);
            v.visit_expr(expr);
        }
        Expr::SymbolicDerefAssign { sigil, expr, value } => {
            v.visit_name(sigil, NameKind::Symbolic);
            v.visit_expr(expr);
            v.visit_expr(value);
        }
        Expr::IndirectTypeLookupAssign { expr, value } => {
            v.visit_expr(expr);
            v.visit_expr(value);
        }
        Expr::PseudoStash(name) => v.visit_name(name, NameKind::Symbolic),
        Expr::HyperSlice { target, adverb: _ } => v.visit_expr(target),
    }
}
