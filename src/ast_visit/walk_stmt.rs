//! The exhaustive default recursion over [`Stmt`]. See the module doc of
//! [`super`] for why every field is named.

use super::{NameKind, Visit, exprs, names, params, traits, walk_call_arg, walk_regex_tree};
use crate::ast::Stmt;

/// Visits every statement of `body` through [`Visit::visit_stmt`].
// Cost: O(n), n = size of `body`'s subtree.
pub(crate) fn walk_stmts<V: Visit + ?Sized>(v: &mut V, body: &[Stmt]) {
    for stmt in body {
        v.visit_stmt(stmt);
    }
}

/// Visits every child of `s` (statements, expressions, parameters, regex
/// nodes) and reports every identifier `s` itself holds.
// Cost: O(n), n = size of `s`'s subtree.
pub(crate) fn walk_stmt<V: Visit + ?Sized>(v: &mut V, s: &Stmt) {
    match s {
        Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state: _,
            is_our: _,
            is_dynamic: _,
            is_export: _,
            export_tags,
            custom_traits,
            where_constraint,
        } => {
            v.visit_name(name, NameKind::VarDecl);
            v.visit_expr(expr);
            names(v, type_constraint.iter(), NameKind::Type);
            names(v, export_tags, NameKind::Module);
            traits(v, custom_traits);
            if let Some(e) = where_constraint {
                v.visit_expr(e);
            }
        }
        Stmt::MarkReadonly(name, _) => v.visit_name(name, NameKind::MarkReadonly),
        Stmt::MarkBoundContainer(name) => v.visit_name(name, NameKind::MarkBoundContainer),
        Stmt::MarkBind
        | Stmt::Proceed
        | Stmt::Succeed
        | Stmt::ReactDone
        | Stmt::SupplyBodyDone
        | Stmt::SetLine(_) => {}
        // A copy of declarations that stay in the tree, where they are walked.
        Stmt::NestedTypeShells(_) => {}
        Stmt::LoopExitGuard {
            label: _,
            next_ph,
            exit_ph,
        } => {
            super::walk_stmts(v, next_ph);
            super::walk_stmts(v, exit_ph);
        }
        Stmt::LoopExitGuardEnd => {}
        Stmt::NestedMethodCapture {
            index: _,
            closure,
            routines,
        } => {
            v.visit_expr(closure);
            for r in routines {
                v.visit_name(r.as_str(), NameKind::Decl);
            }
        }
        Stmt::MarkSigillessReadonly(name) | Stmt::MarkSigilless(name) => {
            v.visit_name(name, NameKind::Sigilless)
        }
        Stmt::Assign {
            name,
            expr,
            op: _,
            target_is_sigilless: _,
        } => {
            v.visit_name(name, NameKind::AssignTarget);
            v.visit_expr(expr);
        }
        Stmt::SubDecl {
            name,
            name_expr,
            params: flat,
            param_defs,
            return_type,
            associativity,
            precedence_trait,
            signature_alternates,
            body,
            multi: _,
            is_rw: _,
            is_raw: _,
            is_export: _,
            export_tags,
            is_test_assertion: _,
            supersede: _,
            custom_traits,
        } => {
            v.visit_name(name.as_str(), NameKind::SubDecl);
            if let Some(e) = name_expr {
                v.visit_expr(e);
            }
            names(v, flat, NameKind::BlockParam);
            params(v, param_defs);
            names(v, return_type.iter(), NameKind::Type);
            names(v, associativity.iter(), NameKind::Operator);
            if let Some((relation, other)) = precedence_trait {
                v.visit_name(relation, NameKind::Trait);
                v.visit_name(other, NameKind::Operator);
            }
            for (alt_flat, alt_defs) in signature_alternates {
                names(v, alt_flat, NameKind::BlockParam);
                params(v, alt_defs);
            }
            names(v, export_tags, NameKind::Module);
            traits(v, custom_traits);
            super::walk_stmts(v, body);
        }
        Stmt::TokenDecl {
            name,
            params: flat,
            param_defs,
            body,
            source_regex,
            regex_kind: _,
            multi: _,
            is_my: _,
            is_our: _,
            is_export: _,
            export_tags,
        }
        | Stmt::RuleDecl {
            name,
            params: flat,
            param_defs,
            body,
            source_regex,
            multi: _,
            is_export: _,
            export_tags,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            names(v, flat, NameKind::BlockParam);
            params(v, param_defs);
            if let Some(tree) = source_regex {
                walk_regex_tree(v, tree);
            }
            names(v, export_tags, NameKind::Module);
            super::walk_stmts(v, body);
        }
        Stmt::ProtoToken { name } => v.visit_name(name.as_str(), NameKind::Decl),
        Stmt::TrustsDecl { name } => v.visit_name(name.as_str(), NameKind::Type),
        Stmt::Package {
            name,
            body,
            kind: _,
            is_unit: _,
            is_my: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            super::walk_stmts(v, body);
        }
        // The declaration itself is visited where the prologue keeps it.
        Stmt::PackageRuntimeBody { body, .. } => super::walk_stmts(v, body),
        Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e)
        | Stmt::Expr(e) => v.visit_expr(e),
        Stmt::For {
            iterable,
            param,
            param_def,
            params: flat,
            params_def,
            body,
            label,
            mode: _,
            rw_block: _,
            explicit_zero_params: _,
            is_statement_modifier: _,
            uses_block_magic: _,
        } => {
            v.visit_expr(iterable);
            names(v, param.iter(), NameKind::BlockParam);
            if let Some(p) = param_def.as_ref() {
                v.visit_param(p);
            }
            names(v, flat, NameKind::BlockParam);
            params(v, params_def);
            names(v, label.iter(), NameKind::Label);
            super::walk_stmts(v, body);
        }
        Stmt::Say(items) | Stmt::Put(items) | Stmt::Print(items) | Stmt::Note(items) => {
            exprs(v, items)
        }
        Stmt::Call { name, args } => {
            v.visit_name(name.as_str(), NameKind::Call);
            for a in args {
                walk_call_arg(v, a);
            }
        }
        Stmt::Use {
            module,
            arg,
            tags,
            condition,
        } => {
            v.visit_name(module, NameKind::Module);
            if let Some(e) = arg {
                v.visit_expr(e);
            }
            names(v, tags, NameKind::Module);
            if let Some(e) = condition {
                v.visit_expr(e);
            }
        }
        Stmt::No { module, arg } => {
            v.visit_name(module, NameKind::Module);
            if let Some(e) = arg {
                v.visit_expr(e);
            }
        }
        Stmt::Need { module } => v.visit_name(module, NameKind::Module),
        Stmt::Import { module, tags } => {
            v.visit_name(module, NameKind::Module);
            names(v, tags, NameKind::Module);
        }
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::React { body }
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body) => super::walk_stmts(v, body),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var,
            is_statement_modifier: _,
            is_unless: _,
            with_kind: _,
        } => {
            v.visit_expr(cond);
            names(v, binding_var.iter(), NameKind::BlockParam);
            super::walk_stmts(v, then_branch);
            super::walk_stmts(v, else_branch);
        }
        Stmt::While {
            cond,
            body,
            label,
            is_statement_modifier: _,
            is_until: _,
        } => {
            v.visit_expr(cond);
            names(v, label.iter(), NameKind::Label);
            super::walk_stmts(v, body);
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat: _,
            label,
            is_until: _,
        } => {
            if let Some(i) = init {
                v.visit_stmt(i);
            }
            for e in [cond, step].into_iter().flatten() {
                v.visit_expr(e);
            }
            names(v, label.iter(), NameKind::Label);
            super::walk_stmts(v, body);
        }
        Stmt::Whenever {
            supply,
            params: flat,
            param_defs,
            body,
        } => {
            v.visit_expr(supply);
            names(v, flat, NameKind::BlockParam);
            params(v, param_defs);
            super::walk_stmts(v, body);
        }
        Stmt::Last(label) | Stmt::Next(label) | Stmt::Redo(label) => {
            names(v, label.iter(), NameKind::Label)
        }
        Stmt::Given {
            topic: cond,
            body,
            is_statement_modifier: _,
            with_kind: _,
        }
        | Stmt::When {
            cond,
            body,
            is_statement_modifier: _,
        } => {
            v.visit_expr(cond);
            super::walk_stmts(v, body);
        }
        Stmt::DocPhaser(inner) => v.visit_stmt(inner),
        Stmt::Label { name, stmt } => {
            v.visit_name(name, NameKind::Label);
            v.visit_stmt(stmt);
        }
        Stmt::EnumDecl {
            name,
            variants,
            variant_form: _,
            is_export: _,
            export_tags,
            is_my: _,
            base_type,
            roles,
            language_version: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            for (key, value) in variants {
                v.visit_name(key, NameKind::Decl);
                if let Some(e) = value {
                    v.visit_expr(e);
                }
            }
            names(v, export_tags, NameKind::Module);
            names(v, base_type.iter(), NameKind::Type);
            names(v, roles, NameKind::Type);
        }
        Stmt::ClassDecl { .. }
        | Stmt::HasDecl { .. }
        | Stmt::MethodDecl { .. }
        | Stmt::RoleDecl { .. } => super::walk_decl::walk_type_member_decl(v, s),
        Stmt::DoesDecl {
            name,
            args,
            from_is: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Type);
            if let Some(a) = args {
                exprs(v, a);
            }
        }
        Stmt::AugmentClass {
            name,
            body,
            does_roles,
            is_role: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            for r in does_roles {
                v.visit_name(r.as_str(), NameKind::Type);
            }
            super::walk_stmts(v, body);
        }
        Stmt::SubsetDecl {
            name,
            base,
            base_is_explicit: _,
            predicate,
            version: _,
            is_export: _,
            export_tags,
            is_my: _,
            decl_id: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            v.visit_name(base, NameKind::Type);
            if let Some(e) = predicate {
                v.visit_expr(e);
            }
            names(v, export_tags, NameKind::Module);
        }
        Stmt::Phaser {
            kind: _,
            body,
            // The condition's source text, kept only for the error message.
            condition: _,
            end_index: _,
        } => super::walk_stmts(v, body),
        Stmt::ProtoDecl {
            name,
            params: flat,
            param_defs,
            return_type,
            body,
            is_export: _,
            export_tags,
            custom_traits,
            trait_args,
            is_method: _,
            is_our: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            names(v, flat, NameKind::BlockParam);
            params(v, param_defs);
            names(v, return_type.iter(), NameKind::Type);
            names(v, export_tags, NameKind::Module);
            names(v, custom_traits, NameKind::Trait);
            // The names above are the same traits; only their argument
            // expressions are new to the visitor.
            for e in trait_args.iter().filter_map(|(_, a)| a.as_ref()) {
                v.visit_expr(e);
            }
            super::walk_stmts(v, body);
        }
        Stmt::Let {
            name,
            index,
            value,
            is_temp: _,
            undefine_first: _,
            nested_lvalue: _,
        } => {
            v.visit_name(name, NameKind::TempTarget);
            for e in [index, value].into_iter().flatten() {
                v.visit_expr(e);
            }
        }
        Stmt::TempMethodAssign {
            var_name,
            method_name,
            method_args,
            value,
        } => {
            v.visit_name(var_name, NameKind::TempTarget);
            v.visit_name(method_name, NameKind::Method);
            exprs(v, method_args);
            v.visit_expr(value);
        }
    }
}
