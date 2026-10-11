//! Bound the tree copied into a control statement's written-source record.

use crate::ast::{Expr, ParamDef, SourceForm, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_param, walk_regex_node, walk_stmt, walk_stmts};
use crate::value::RuntimeError;

const MAX_NODES: usize = 16_384;
const MAX_DEPTH: usize = 128;

#[derive(Default)]
struct Budget {
    nodes: usize,
    depth: usize,
    exceeded: bool,
}

impl Budget {
    fn enter(&mut self) -> bool {
        if self.exceeded || self.nodes == MAX_NODES || self.depth == MAX_DEPTH {
            self.exceeded = true;
            return false;
        }
        self.nodes += 1;
        self.depth += 1;
        true
    }
}

impl<'ast> Visit<'ast> for Budget {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if !self.enter() {
            return;
        }
        // Ordinary analyses skip source records. A copy budget must count
        // their bodies too, since nested records duplicate the same subtree.
        match stmt {
            Stmt::SourceForm(form) => match form.as_ref() {
                SourceForm::DeclarationModifier {
                    declaration,
                    condition,
                    ..
                } => {
                    self.visit_stmt(declaration);
                    self.visit_expr(condition);
                }
                SourceForm::ControlPointy {
                    condition,
                    param_defs,
                    body,
                    ..
                } => {
                    self.visit_expr(condition);
                    for param in param_defs {
                        self.visit_param(param);
                    }
                    walk_stmts(self, body);
                }
                SourceForm::IfPointy { param_defs, body } => {
                    for param in param_defs {
                        self.visit_param(param);
                    }
                    walk_stmts(self, body);
                }
                SourceForm::WithPointy {
                    param_def, body, ..
                } => {
                    if let Some(param) = param_def {
                        self.visit_param(param);
                    }
                    walk_stmts(self, body);
                }
                SourceForm::GivenPointy { param_def, body } => {
                    self.visit_param(param_def);
                    walk_stmts(self, body);
                }
                SourceForm::SupplyBlock(body) => walk_stmts(self, body),
                SourceForm::HyperAssign { target, value, .. } => {
                    self.visit_expr(target);
                    self.visit_expr(value);
                }
                SourceForm::SignatureDecl(decl) => {
                    for var in &decl.vars {
                        for expr in [&var.default, &var.where_constraint, &var.literal_value]
                            .into_iter()
                            .flatten()
                        {
                            self.visit_expr(expr);
                        }
                    }
                    if let Some(expr) = &decl.group_default {
                        self.visit_expr(expr);
                    }
                    if let Some(init) = &decl.init {
                        self.visit_expr(&init.rhs);
                    }
                }
                SourceForm::MethodAssignDecl(decl) => {
                    for expr in &decl.args {
                        self.visit_expr(expr);
                    }
                    for (_, expr) in &decl.custom_traits {
                        if let Some(expr) = expr {
                            self.visit_expr(expr);
                        }
                    }
                    if let Some(expr) = &decl.where_constraint {
                        self.visit_expr(expr);
                    }
                }
                SourceForm::RoutineTraits(traits) => {
                    for item in traits {
                        if let crate::ast::routine_trait::RoutineTrait::Is {
                            argument:
                                Some(crate::ast::routine_trait::TraitArgument::Parentheses(expr)),
                            ..
                        } = item
                        {
                            self.visit_expr(expr);
                        }
                    }
                }
                SourceForm::BarePhaser => {}
            },
            _ => walk_stmt(self, stmt),
        }
        self.depth -= 1;
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.enter() {
            walk_expr(self, expr);
            self.depth -= 1;
        }
    }

    fn visit_param(&mut self, param: &'ast ParamDef) {
        if self.enter() {
            walk_param(self, param);
            self.depth -= 1;
        }
    }

    fn visit_regex_node(&mut self, node: &'ast crate::regex_tree::RegexNode) {
        if self.enter() {
            walk_regex_node(self, node);
            self.depth -= 1;
        }
    }
}

// Cost: O(n), n = copied nodes; recursion and accepted copies are bounded.
pub(in crate::parser::stmt) fn check(
    cond: &Expr,
    params: &[ParamDef],
    body: &[Stmt],
) -> Result<(), RuntimeError> {
    let mut budget = Budget::default();
    budget.visit_expr(cond);
    for param in params {
        budget.visit_param(param);
    }
    walk_stmts(&mut budget, body);
    if budget.exceeded {
        Err(RuntimeError::new(
            "Control signature source form exceeds its size or depth limit",
        ))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ControlPointyKind;
    use crate::value::Value;

    #[test]
    fn nested_source_copies_are_rejected_before_the_next_clone() {
        let condition = Expr::Literal(Value::FALSE);
        let mut body = vec![Stmt::Expr(Expr::Literal(Value::NIL))];
        for _ in 0..32 {
            if check(&condition, &[], &body).is_err() {
                return;
            }
            let record = Stmt::SourceForm(Box::new(SourceForm::ControlPointy {
                kind: ControlPointyKind::While,
                label: None,
                condition: condition.clone(),
                param_defs: Vec::new(),
                body: body.clone(),
            }));
            body = vec![record, Stmt::Block(body)];
        }
        panic!("the source copy budget did not reject exponential growth");
    }

    #[test]
    fn parsed_nested_control_signatures_report_a_resource_error() {
        for keyword in [
            "while 0",
            "until 1",
            "unless 0",
            "if 1",
            "if 0 {} else",
            "with 1",
            "without Any",
            "with Any {} orwith 1",
        ] {
            let source = format!("{keyword} -> $value {{ ").repeat(20) + "1;" + &" }".repeat(20);
            let error = crate::parser::parse_program(&source).expect_err("bounded source record");
            assert!(
                error.message.contains("source form exceeds"),
                "{keyword}: {error:?}"
            );
        }
    }

    #[test]
    fn declaration_modifier_source_records_share_the_copy_budget() {
        let condition = Expr::Literal(Value::TRUE);
        let mut body = vec![Stmt::Expr(Expr::Literal(Value::NIL))];
        for _ in 0..32 {
            if check(&condition, &[], &body).is_err() {
                return;
            }
            let record = Stmt::SourceForm(Box::new(SourceForm::DeclarationModifier {
                declaration: Box::new(Stmt::SyntheticBlock(body.clone())),
                condition: condition.clone(),
                kind: crate::ast::decl_modifier::DeclarationModifier::If,
            }));
            body = vec![record, Stmt::SyntheticBlock(body)];
        }
        panic!("the declaration source copy budget did not bound nested records");
    }
}
