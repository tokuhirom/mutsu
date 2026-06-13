use super::*;
use crate::ast::{PhaserKind, Stmt};

/// Collect the names of all types (class/role/enum/subset) declared anywhere in
/// `stmts`, descending into block-like bodies. Used so a sub parameter type that
/// names a type declared in the same compilation unit is not falsely rejected.
fn collect_declared_type_names(stmts: &[Stmt], out: &mut std::collections::HashSet<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::ClassDecl { name, body, .. } | Stmt::RoleDecl { name, body, .. } => {
                out.insert(name.resolve().to_string());
                collect_declared_type_names(body, out);
            }
            Stmt::EnumDecl { name, .. } | Stmt::SubsetDecl { name, .. } => {
                out.insert(name.resolve().to_string());
            }
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Package { body, .. } => {
                collect_declared_type_names(body, out);
            }
            _ => {}
        }
    }
}

/// Validate parameter type constraints for plain subs in `stmts` (descending
/// into bare blocks/packages, but not class/role bodies whose methods may
/// legitimately forward-reference the enclosing type).
fn walk_validate_sub_param_types(
    interp: &Interpreter,
    stmts: &[Stmt],
    declared: &std::collections::HashSet<String>,
) -> Result<(), RuntimeError> {
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl {
                param_defs,
                body,
                return_type,
                ..
            } => {
                interp.validate_param_type_constraints(param_defs, declared)?;
                interp.validate_return_type_constraint(
                    return_type.as_deref(),
                    param_defs,
                    declared,
                )?;
                walk_validate_sub_param_types(interp, body, declared)?;
            }
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Package { body, .. } => {
                walk_validate_sub_param_types(interp, body, declared)?;
            }
            _ => {}
        }
    }
    Ok(())
}

impl Interpreter {
    /// Reject sub parameter types that name a type unknown to this compilation
    /// unit (e.g. `sub yoink(Junctoin $barf)`) -> X::Parameter::InvalidType.
    pub(crate) fn check_eval_param_type_constraints(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut declared = std::collections::HashSet::new();
        collect_declared_type_names(stmts, &mut declared);
        walk_validate_sub_param_types(self, stmts, &declared)
    }

    /// Parse and run only BEGIN/CHECK phasers from EVAL'd code (`:check` mode).
    fn parse_and_check_only_with_operators(
        &mut self,
        src: &str,
        op_names: &[String],
        op_assoc: &HashMap<String, String>,
        imported_names: &[String],
    ) -> Result<Value, RuntimeError> {
        let user_sub_names = self.collect_eval_user_sub_names();
        match crate::parser::parse_program_with_operators_and_user_subs(
            src,
            op_names,
            op_assoc,
            imported_names,
            &user_sub_names,
        ) {
            Ok((stmts, _)) => {
                self.check_eval_class_redeclarations(&stmts)?;
                self.check_eval_undeclared_vars(&stmts)?;
                self.check_eval_undeclared_names(&stmts)?;
                let mut stmts = self.inject_eval_methods_into_class(stmts);
                crate::runtime::phasers::reorder_phasers_for_eval(&mut stmts);
                let phaser_stmts: Vec<Stmt> = stmts
                    .into_iter()
                    .filter(|s| {
                        matches!(
                            s,
                            Stmt::Phaser {
                                kind: PhaserKind::Begin,
                                ..
                            } | Stmt::Phaser {
                                kind: PhaserKind::Check,
                                ..
                            }
                        )
                    })
                    .collect();
                if !phaser_stmts.is_empty() {
                    self.eval_block_value(&phaser_stmts)?;
                }
                Ok(Value::Nil)
            }
            Err(parse_err) => {
                let (partial_stmts, _) = crate::parser::parse_program_partial_with_operators(
                    src,
                    op_names,
                    op_assoc,
                    imported_names,
                );
                self.execute_begin_phasers(&partial_stmts);
                Err(parse_err)
            }
        }
    }

    /// EVAL with :check -- parse, run BEGIN/CHECK phasers, skip main body.
    pub(super) fn eval_eval_string_check_only(
        &mut self,
        code: &str,
    ) -> Result<Value, RuntimeError> {
        let trimmed = code.trim();
        let saved_in_eval = self.env.get("__mutsu_in_eval").cloned();
        self.env
            .insert("__mutsu_in_eval".to_string(), Value::Bool(true));
        let op_names = self.collect_operator_sub_names();
        let op_assoc = self.collect_operator_assoc_map();
        let imported_names = self.collect_eval_imported_function_names();
        let result = self.parse_and_check_only_with_operators(
            trimmed,
            &op_names,
            &op_assoc,
            &imported_names,
        );
        if let Some(saved) = saved_in_eval {
            self.env.insert("__mutsu_in_eval".to_string(), saved);
        } else {
            self.env.remove("__mutsu_in_eval");
        }
        result
    }
}
