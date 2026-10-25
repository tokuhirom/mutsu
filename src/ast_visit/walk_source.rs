//! Opt-in traversal of compiler-inert written-source records.

use super::{NameKind, Visit, exprs, names, params, traits, walk_stmts};
use crate::ast::routine_trait::{RoutineTrait, TraitArgument};
use crate::ast::{SignatureDecl, SignatureInit, SignatureVar, SourceForm};

// Cost: O(n), n = source-record subtree nodes.
pub(crate) fn walk_source_form<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, form: &'ast SourceForm) {
    match form {
        SourceForm::IfPointy { param_defs, body } => {
            params(v, param_defs);
            walk_stmts(v, body);
        }
        SourceForm::WithPointy {
            param_name,
            param_def,
            body,
        } => {
            v.visit_name(param_name, NameKind::BlockParam);
            if let Some(param) = param_def {
                v.visit_param(param);
            }
            walk_stmts(v, body);
        }
        SourceForm::GivenPointy { param_def, body } => {
            v.visit_param(param_def);
            walk_stmts(v, body);
        }
        SourceForm::SignatureDecl(declaration) => v.visit_signature_decl(declaration),
        SourceForm::SignatureBind { signature, rhs } => {
            v.visit_expr(signature);
            v.visit_expr(rhs);
        }
        SourceForm::RoutineTraits(items) => {
            for item in items {
                match item {
                    RoutineTrait::Is { name, argument } => {
                        v.visit_name(name, NameKind::Trait);
                        match argument {
                            Some(TraitArgument::Parentheses(expr)) => v.visit_expr(expr),
                            Some(TraitArgument::Words(_)) | None => {}
                            Some(TraitArgument::ExportTags(tags)) => {
                                names(v, tags, NameKind::Module)
                            }
                            Some(TraitArgument::Operator(name)) => {
                                v.visit_name(name, NameKind::Operator)
                            }
                        }
                    }
                    RoutineTrait::Returns(name) | RoutineTrait::Of(name) => {
                        v.visit_name(name, NameKind::Type)
                    }
                    RoutineTrait::Handles(term) => v.visit_expr(term),
                    RoutineTrait::Unsupported(source) => v.visit_name(source, NameKind::Source),
                }
            }
        }
        SourceForm::DeclarationCompoundAssign => {}
    }
}

/// The names and expressions of a written `.=` declaration.
// Cost: O(n), n = nodes of the declaration's call.
pub(crate) fn walk_method_assign_decl<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    declaration: &'ast crate::ast::method_assign_decl::MethodAssignDecl,
) {
    let crate::ast::method_assign_decl::MethodAssignDecl {
        name,
        anonymous: _,
        type_constraint,
        is_state: _,
        is_our: _,
        is_dynamic: _,
        is_export: _,
        export_tags,
        custom_traits,
        where_constraint,
        method,
        callee,
        args,
        chain,
        shape,
        is_v6c: _,
    } = declaration;
    v.visit_name(name, NameKind::VarDecl);
    names(v, type_constraint.iter(), NameKind::Type);
    names(v, export_tags, NameKind::Module);
    traits(v, custom_traits);
    if let Some(expr) = where_constraint {
        v.visit_expr(expr);
    }
    match callee {
        Some(callee) => v.visit_expr(callee),
        None => v.visit_name(&method.resolve(), NameKind::Method),
    }
    exprs(v, args);
    if let Some(chain) = chain {
        v.visit_expr(chain);
    }
    if let Some(dims) = shape {
        exprs(v, dims);
    }
}

// Cost: O(n), n = declaring signature nodes.
pub(crate) fn walk_signature_decl<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    decl: &'ast SignatureDecl,
) {
    let SignatureDecl {
        vars,
        group_shape: _,
        is_state: _,
        is_our: _,
        type_constraint,
        group_default,
        has_nested_group: _,
        init,
    } = decl;
    for variable in vars {
        v.visit_signature_var(variable);
    }
    names(v, type_constraint.iter(), NameKind::Type);
    if let Some(expr) = group_default {
        v.visit_expr(expr);
    }
    if let Some(SignatureInit { is_binding: _, rhs }) = init {
        v.visit_expr(rhs);
    }
}

// Cost: O(n), n = parameter expression nodes.
pub(crate) fn walk_signature_var<'ast, V: Visit<'ast> + ?Sized>(
    v: &mut V,
    var: &'ast SignatureVar,
) {
    let SignatureVar {
        name,
        is_slurpy: _,
        is_optional: _,
        is_named: _,
        default,
        per_var_type_constraint,
        where_constraint,
        sigilless: _,
        literal_value,
        param_trait: _,
    } = var;
    v.visit_name(name, NameKind::VarDecl);
    names(v, per_var_type_constraint.iter(), NameKind::Type);
    for expr in [default, where_constraint, literal_value]
        .into_iter()
        .flatten()
    {
        v.visit_expr(expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Stmt};

    #[derive(Default)]
    struct Names {
        names: Vec<String>,
    }

    impl<'ast> Visit<'ast> for Names {
        fn visit_name(&mut self, name: &str, _kind: NameKind) {
            self.names.push(name.into());
        }
    }

    #[derive(Default)]
    struct CopyingNames(Names);

    impl<'ast> Visit<'ast> for CopyingNames {
        fn visit_source_form(&mut self, form: &'ast SourceForm) {
            walk_source_form(self, form);
        }
        fn visit_name(&mut self, name: &str, kind: NameKind) {
            self.0.visit_name(name, kind);
        }
    }

    #[test]
    fn source_records_are_opt_in_and_walk_their_original_declarations() {
        let record = Stmt::SourceForm(Box::new(SourceForm::SignatureBind {
            signature: Expr::Var("written".into()),
            rhs: Expr::Literal(crate::value::Value::NIL),
        }));
        let mut ordinary = Names::default();
        ordinary.visit_stmt(&record);
        assert!(ordinary.names.is_empty());
        let mut copying = CopyingNames::default();
        copying.visit_stmt(&record);
        assert_eq!(copying.0.names, ["written"]);
    }
}
