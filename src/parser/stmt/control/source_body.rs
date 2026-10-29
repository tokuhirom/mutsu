//! Bound the tree copied into a declaration's written-source record.

use crate::ast::{Expr, ParamDef, Stmt};
use crate::ast_visit::{
    Visit, walk_expr, walk_param, walk_regex_node, walk_routine_traits, walk_signature_var,
    walk_stmt, walk_stmts,
};
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
        walk_stmt(self, stmt);
        self.depth -= 1;
    }

    fn visit_routine_traits(&mut self, traits: &'ast [crate::ast::routine_trait::RoutineTrait]) {
        walk_routine_traits(self, traits);
    }

    fn visit_signature_var(&mut self, var: &'ast crate::ast::SignatureVar) {
        if self.enter() {
            walk_signature_var(self, var);
            self.depth -= 1;
        }
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
pub(crate) fn check(cond: &Expr, params: &[ParamDef], body: &[Stmt]) -> Result<(), RuntimeError> {
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

// Cost: O(n), n = signature nodes; traversal and accepted allocation are bounded.
pub(crate) fn check_signature(decl: &crate::ast::SignatureDecl) -> Result<(), RuntimeError> {
    let mut budget = Budget::default();
    budget.visit_signature_decl(decl);
    if budget.exceeded {
        Err(RuntimeError::new(
            "Signature declaration source form exceeds its size or depth limit",
        ))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn deeply_nested_source_is_rejected() {
        let condition = Expr::Literal(Value::FALSE);
        let mut body = vec![Stmt::Expr(Expr::Literal(Value::NIL))];
        for _ in 0..(MAX_DEPTH + 1) {
            body = vec![Stmt::Block(body)];
        }
        assert!(check(&condition, &[], &body).is_err());
    }

    #[test]
    fn nested_pointy_conditionals_copy_nothing() {
        // A pointy `if`/`with` used to keep a second copy of its body next to
        // the expansion, which doubled the tree at every nesting level.
        for keyword in [
            "if 1",
            "if 0 {} else",
            "with 1",
            "without Any",
            "with Any {} orwith 1",
        ] {
            let source = format!("{keyword} -> $value {{ ").repeat(20) + "1;" + &" }".repeat(20);
            crate::parser::parse_program(&source).unwrap_or_else(|e| panic!("{keyword}: {e:?}"));
        }
    }

    #[test]
    fn signature_materialization_reports_oversized_parameters_before_expansion() {
        let declaration = crate::ast::SignatureDecl {
            vars: vec![crate::ast::SignatureVar::plain("$value"); MAX_NODES + 1],
            group_shape: None,
            is_state: false,
            is_our: false,
            type_constraint: None,
            group_default: None,
            has_nested_group: false,
            init: Some(crate::ast::SignatureInit {
                is_binding: true,
                rhs: Expr::ArrayLiteral(Vec::new()),
            }),
        };
        let error = crate::rakuast::signature_expand::signature_decl(declaration)
            .expect_err("signature allocation has a catchable budget error");
        assert!(error.message.contains("source form exceeds"));
    }
}
