//! The exhaustive default mutable recursion over [`Stmt`] (ADR-10499). It
//! mirrors [`super::walk_stmt()`] child for child; identifier strings are not
//! reported, since a rewrite that renames does so in the hook of the node
//! that owns the name.

use super::{VisitMut, exprs_mut, params_mut, traits_mut, walk_call_arg_mut, walk_regex_tree_mut};
use crate::ast::Stmt;

/// Visits every statement of `body` through [`VisitMut::visit_stmt_mut`].
// Cost: O(n), n = size of `body`'s subtree.
pub(crate) fn walk_stmts_mut<V: VisitMut + ?Sized>(v: &mut V, body: &mut [Stmt]) {
    for stmt in body {
        v.visit_stmt_mut(stmt);
    }
}

/// Visits every child of `s` (statements, expressions, parameters, regex
/// nodes) mutably.
// Cost: O(n), n = size of `s`'s subtree.
pub(crate) fn walk_stmt_mut<V: VisitMut + ?Sized>(v: &mut V, s: &mut Stmt) {
    match s {
        Stmt::VarDecl {
            name: _,
            expr,
            type_constraint: _,
            is_state: _,
            is_our: _,
            is_dynamic: _,
            is_export: _,
            export_tags: _,
            custom_traits,
            where_constraint,
        } => {
            v.visit_expr_mut(expr);
            traits_mut(v, custom_traits);
            if let Some(e) = where_constraint {
                v.visit_expr_mut(e);
            }
        }
        // Not code; see `walk_stmt`. A rewriting pass leaves the record's
        // copies as the parser wrote them, which is the source form.
        Stmt::SourceForm(_) => {}
        Stmt::MarkReadonly(_name, _) => {}
        Stmt::MarkBoundContainer(_name) => {}
        Stmt::MarkBind
        | Stmt::Proceed
        | Stmt::Succeed
        | Stmt::ReactDone
        | Stmt::SupplyBodyDone
        | Stmt::SetLine(_)
        | Stmt::BeginPrologueEnd
        | Stmt::Trace { .. } => {}
        // A copy of declarations that stay in the tree, where they are walked.
        Stmt::NestedTypeShells(_) | Stmt::UndeclaredRoutine(_) => {}
        Stmt::LoopExitGuard {
            label: _,
            next_ph,
            exit_ph,
        } => {
            v.visit_stmts_mut(next_ph);
            v.visit_stmts_mut(exit_ph);
        }
        Stmt::LoopExitGuardEnd => {}
        Stmt::NestedMethodCapture {
            index: _,
            closure,
            routines: _,
        } => v.visit_expr_mut(closure),
        Stmt::MarkSigillessReadonly(_name) | Stmt::MarkSigilless(_name) => {}
        Stmt::Assign {
            name: _,
            expr,
            op: _,
            target_is_sigilless: _,
        } => v.visit_expr_mut(expr),
        Stmt::SubDecl {
            name: _,
            name_expr,
            params: _,
            param_defs,
            return_type: _,
            associativity: _,
            precedence_trait: _,
            signature_alternates,
            body,
            multi: _,
            is_rw: _,
            is_raw: _,
            is_export: _,
            export_tags: _,
            is_test_assertion: _,
            supersede: _,
            custom_traits,
        } => {
            if let Some(e) = name_expr {
                v.visit_expr_mut(e);
            }
            params_mut(v, param_defs);
            for (_alt_flat, alt_defs) in signature_alternates {
                params_mut(v, alt_defs);
            }
            traits_mut(v, custom_traits);
            v.visit_stmts_mut(body);
        }
        Stmt::TokenDecl {
            name: _,
            params: _,
            param_defs,
            body,
            source_regex,
            regex_kind: _,
            multi: _,
            is_my: _,
            is_our: _,
            is_export: _,
            export_tags: _,
        }
        | Stmt::RuleDecl {
            name: _,
            params: _,
            param_defs,
            body,
            source_regex,
            multi: _,
            is_export: _,
            export_tags: _,
        } => {
            params_mut(v, param_defs);
            if let Some(tree) = source_regex {
                walk_regex_tree_mut(v, tree);
            }
            v.visit_stmts_mut(body);
        }
        Stmt::ProtoToken { name: _ } => {}
        Stmt::TrustsDecl { name: _ } => {}
        Stmt::Package {
            name: _,
            body,
            kind: _,
            is_unit: _,
            is_my: _,
        } => v.visit_stmts_mut(body),
        Stmt::PackageRuntimeBody {
            name: _,
            body,
            lexicals: _,
            decl: _,
        } => v.visit_stmts_mut(body),
        Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e)
        | Stmt::Expr(e) => v.visit_expr_mut(e),
        Stmt::For {
            iterable,
            param: _,
            param_def,
            params: _,
            params_def,
            body,
            label: _,
            mode: _,
            rw_block: _,
            explicit_zero_params: _,
            is_statement_modifier: _,
            uses_block_magic: _,
        } => {
            v.visit_expr_mut(iterable);
            if let Some(p) = param_def.as_mut() {
                v.visit_param_mut(p);
            }
            params_mut(v, params_def);
            v.visit_stmts_mut(body);
        }
        Stmt::Say(items) | Stmt::Put(items) | Stmt::Print(items) | Stmt::Note(items) => {
            exprs_mut(v, items)
        }
        Stmt::Call { name: _, args } => {
            for a in args {
                walk_call_arg_mut(v, a);
            }
        }
        Stmt::Use {
            module: _,
            arg,
            tags: _,
            condition,
            if_imports: _,
        } => {
            if let Some(e) = arg {
                v.visit_expr_mut(e);
            }
            if let Some(e) = condition {
                v.visit_expr_mut(e);
            }
        }
        Stmt::No { module: _, arg } => {
            if let Some(e) = arg {
                v.visit_expr_mut(e);
            }
        }
        Stmt::Need { module: _ } => {}
        Stmt::Import { module: _, tags: _ } => {}
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::React { body }
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body) => v.visit_stmts_mut(body),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var: _,
            is_statement_modifier: _,
            is_unless: _,
            with_kind: _,
        } => {
            v.visit_expr_mut(cond);
            v.visit_stmts_mut(then_branch);
            v.visit_stmts_mut(else_branch);
        }
        Stmt::While {
            cond,
            body,
            label: _,
            is_statement_modifier: _,
            is_until: _,
        } => {
            v.visit_expr_mut(cond);
            v.visit_stmts_mut(body);
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat: _,
            label: _,
            is_until: _,
        } => {
            if let Some(i) = init {
                v.visit_stmt_mut(i);
            }
            for e in [cond, step].into_iter().flatten() {
                v.visit_expr_mut(e);
            }
            v.visit_stmts_mut(body);
        }
        Stmt::Whenever {
            supply,
            params: _,
            param_defs,
            body,
        } => {
            v.visit_expr_mut(supply);
            params_mut(v, param_defs);
            v.visit_stmts_mut(body);
        }
        Stmt::Last(_label) | Stmt::Next(_label) | Stmt::Redo(_label) => {}
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
            v.visit_expr_mut(cond);
            v.visit_stmts_mut(body);
        }
        Stmt::DocPhaser(inner) => v.visit_stmt_mut(inner),
        Stmt::Label { name: _, stmt } => v.visit_stmt_mut(stmt),
        Stmt::EnumDecl {
            name: _,
            variants,
            variant_form: _,
            is_export: _,
            export_tags: _,
            is_my: _,
            base_type: _,
            roles: _,
            language_version: _,
        } => {
            for (_key, value) in variants {
                if let Some(e) = value {
                    v.visit_expr_mut(e);
                }
            }
        }
        Stmt::ClassDecl { .. }
        | Stmt::HasDecl { .. }
        | Stmt::MethodDecl { .. }
        | Stmt::RoleDecl { .. } => super::walk_mut_decl::walk_type_member_decl_mut(v, s),
        Stmt::DoesDecl {
            name: _,
            args,
            from_is: _,
        } => {
            if let Some(a) = args {
                exprs_mut(v, a);
            }
        }
        Stmt::AugmentClass {
            name: _,
            body,
            does_roles: _,
            is_role: _,
        } => v.visit_stmts_mut(body),
        Stmt::SubsetDecl {
            name: _,
            base: _,
            base_is_explicit: _,
            predicate,
            version: _,
            is_export: _,
            export_tags: _,
            is_my: _,
            decl_id: _,
        } => {
            if let Some(e) = predicate {
                v.visit_expr_mut(e);
            }
        }
        Stmt::Phaser {
            kind: _,
            body,
            condition: _,
            end_index: _,
        } => v.visit_stmts_mut(body),
        Stmt::ProtoDecl {
            name: _,
            params: _,
            param_defs,
            return_type: _,
            body,
            is_export: _,
            export_tags: _,
            custom_traits: _,
            trait_args,
            is_method: _,
            is_our: _,
        } => {
            params_mut(v, param_defs);
            for e in trait_args.iter_mut().filter_map(|(_, a)| a.as_mut()) {
                v.visit_expr_mut(e);
            }
            v.visit_stmts_mut(body);
        }
        Stmt::Let {
            name: _,
            index,
            value,
            is_temp: _,
            undefine_first: _,
            nested_lvalue: _,
        } => {
            for e in [index, value].into_iter().flatten() {
                v.visit_expr_mut(e);
            }
        }
        Stmt::TempMethodAssign {
            var_name: _,
            method_name: _,
            method_args,
            value,
        } => {
            exprs_mut(v, method_args);
            v.visit_expr_mut(value);
        }
    }
}
