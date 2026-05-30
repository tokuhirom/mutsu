use super::*;
use crate::ast::{PhaserKind, Stmt};

impl Interpreter {
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
