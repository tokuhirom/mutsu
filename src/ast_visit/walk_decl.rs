//! The default recursion over the class, role, attribute and method
//! declarations of [`Stmt`], split out of [`super::walk_stmt()`]. Every field is
//! named, as in the rest of the walker.

use super::{NameKind, Visit, exprs, names, params, traits, walk_handle_spec};
use crate::ast::Stmt;

/// The [`super::walk_stmt()`] arm for `ClassDecl`, `HasDecl`, `MethodDecl` and
/// `RoleDecl`; `walk_stmt`'s own match keeps the variant list exhaustive.
// Cost: O(n), n = size of `s`'s subtree.
pub(super) fn walk_type_member_decl<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, s: &'ast Stmt) {
    match s {
        Stmt::ClassDecl {
            name,
            name_expr,
            parents,
            class_is_rw: _,
            is_hidden: _,
            is_lexical: _,
            hidden_parents,
            does_parents,
            repr,
            body,
            language_version: _,
            custom_traits,
            is_unit: _,
            implicit_grammar_parent: _,
            is_grammar: _,
            decl_id: _,
            parent_args,
            body_parents,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            if let Some(e) = name_expr {
                v.visit_expr(e);
            }
            names(v, parents, NameKind::Type);
            names(v, hidden_parents, NameKind::Type);
            names(v, does_parents, NameKind::Type);
            names(v, repr.iter(), NameKind::Trait);
            traits(v, custom_traits);
            for (parent, args) in parent_args {
                v.visit_name(parent, NameKind::Type);
                exprs(v, args);
            }
            names(v, body_parents, NameKind::Type);
            super::walk_stmts(v, body);
        }
        Stmt::HasDecl {
            name,
            is_public: _,
            default,
            handles,
            is_rw: _,
            is_readonly: _,
            type_constraint,
            type_smiley: _,
            is_required: _,
            sigil: _,
            where_constraint,
            is_alias: _,
            is_embedded: _,
            is_our: _,
            is_my: _,
            is_default,
            is_type,
            deprecated_message: _,
            is_built: _,
            unknown_traits,
            default_is_bind: _,
            default_is_seed: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Attribute);
            for e in [default, is_default].into_iter().flatten() {
                v.visit_expr(e);
            }
            for h in handles {
                walk_handle_spec(v, h);
            }
            names(v, type_constraint.iter(), NameKind::Type);
            if let Some(e) = where_constraint {
                v.visit_expr(e);
            }
            names(v, is_type.iter(), NameKind::Type);
            for (trait_name, trait_arg, arg) in unknown_traits {
                v.visit_name(trait_name, NameKind::Trait);
                v.visit_name(trait_arg, NameKind::Source);
                if let Some(e) = arg {
                    v.visit_expr(e);
                }
            }
        }
        Stmt::MethodDecl {
            name,
            name_expr,
            params: flat,
            param_defs,
            body,
            multi: _,
            is_rw: _,
            is_raw: _,
            is_private: _,
            is_our: _,
            is_my: _,
            is_submethod: _,
            our_variable_form: _,
            return_type,
            is_default_candidate: _,
            deprecated_message: _,
            handles,
            custom_traits,
            is_export: _,
            export_tags,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            if let Some(e) = name_expr {
                v.visit_expr(e);
            }
            names(v, flat, NameKind::BlockParam);
            params(v, param_defs);
            names(v, return_type.iter(), NameKind::Type);
            for h in handles {
                walk_handle_spec(v, h);
            }
            traits(v, custom_traits);
            names(v, export_tags, NameKind::Module);
            super::walk_stmts(v, body);
        }
        Stmt::RoleDecl {
            name,
            type_params,
            type_param_defs,
            is_export: _,
            export_tags,
            body,
            is_rw: _,
            language_version: _,
            custom_traits,
            decl_id: _,
        } => {
            v.visit_name(name.as_str(), NameKind::Decl);
            names(v, type_params, NameKind::BlockParam);
            params(v, type_param_defs);
            names(v, export_tags, NameKind::Module);
            traits(v, custom_traits);
            super::walk_stmts(v, body);
        }
        // Every other variant is walked by `walk_stmt` itself.
        _ => {}
    }
}
