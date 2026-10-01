//! The default mutable recursion over the class, role, attribute and method
//! declarations of [`Stmt`], split out of [`super::walk_stmt_mut()`] the way
//! [`super::walk_decl`] is split out of [`super::walk_stmt()`].

use super::{VisitMut, exprs_mut, params_mut, traits_mut, walk_handle_spec_mut};
use crate::ast::Stmt;

/// The [`super::walk_stmt_mut()`] arm for `ClassDecl`, `HasDecl`,
/// `MethodDecl` and `RoleDecl`; `walk_stmt_mut`'s own match keeps the variant
/// list exhaustive.
// Cost: O(n), n = size of `s`'s subtree.
pub(super) fn walk_type_member_decl_mut<V: VisitMut + ?Sized>(v: &mut V, s: &mut Stmt) {
    match s {
        Stmt::ClassDecl {
            name: _,
            name_expr,
            parents: _,
            class_is_rw: _,
            is_hidden: _,
            is_lexical: _,
            hidden_parents: _,
            does_parents: _,
            repr: _,
            body,
            language_version: _,
            custom_traits,
            is_unit: _,
            implicit_grammar_parent: _,
            is_grammar: _,
            decl_id: _,
            parent_args,
            body_parents: _,
        } => {
            if let Some(e) = name_expr {
                v.visit_expr_mut(e);
            }
            traits_mut(v, custom_traits);
            for (_parent, args) in parent_args {
                exprs_mut(v, args);
            }
            v.visit_stmts_mut(body);
        }
        Stmt::HasDecl {
            name: _,
            is_public: _,
            default,
            handles,
            is_rw: _,
            is_readonly: _,
            type_constraint: _,
            type_smiley: _,
            is_required: _,
            sigil: _,
            where_constraint,
            is_alias: _,
            is_embedded: _,
            is_our: _,
            is_my: _,
            is_default,
            is_type: _,
            deprecated_message: _,
            is_built: _,
            unknown_traits,
            default_is_bind: _,
        } => {
            for e in [default, is_default].into_iter().flatten() {
                v.visit_expr_mut(e);
            }
            for h in handles {
                walk_handle_spec_mut(v, h);
            }
            if let Some(e) = where_constraint {
                v.visit_expr_mut(e);
            }
            for (_trait_name, _trait_arg, arg) in unknown_traits {
                if let Some(e) = arg {
                    v.visit_expr_mut(e);
                }
            }
        }
        Stmt::MethodDecl {
            name: _,
            name_expr,
            params: _,
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
            return_type: _,
            is_default_candidate: _,
            deprecated_message: _,
            handles,
            custom_traits,
            is_export: _,
            export_tags: _,
        } => {
            if let Some(e) = name_expr {
                v.visit_expr_mut(e);
            }
            params_mut(v, param_defs);
            for h in handles {
                walk_handle_spec_mut(v, h);
            }
            traits_mut(v, custom_traits);
            v.visit_stmts_mut(body);
        }
        Stmt::RoleDecl {
            name: _,
            type_params: _,
            type_param_defs,
            is_export: _,
            export_tags: _,
            body,
            is_rw: _,
            language_version: _,
            custom_traits,
            decl_id: _,
        } => {
            params_mut(v, type_param_defs);
            traits_mut(v, custom_traits);
            v.visit_stmts_mut(body);
        }
        // Every other variant is walked by `walk_stmt_mut` itself.
        _ => {}
    }
}
