//! The RakuAST end of the slang host (ADR-15762 §2.3, §2.5): core productions
//! the Rust parser parses on a slang token's behalf become RakuAST nodes, and the
//! node a slang action attaches is lowered where the production stood.

use super::{RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

/// Parse the core production `rule` at the start of `text`: its length in bytes
/// and its RakuAST node, `None` when it does not match there, or an error when
/// the production has no shim yet.
// Cost: O(n), n = size of the production (parse and conversion).
pub(crate) fn parse_production(rule: &str, text: &str) -> Result<Option<(usize, Value)>, String> {
    // The node is read from the parse (`.AST`'s shapes), so the parse keeps the
    // term origins that reading wants.
    let _origins_wanted = crate::parser::term_origins::Wanted::enter();
    if rule == "signature" {
        return signature_production(text);
    }
    if rule == "statementlist" {
        return statementlist_production(text);
    }
    let parsed = crate::parser::parse_slang_shim_production(rule, text)
        .ok_or_else(|| format!("the core rule <{rule}> has no slang shim yet (ADR-15762 §2.3)"))?;
    let Ok((rest, stmt)) = parsed else {
        return Ok(None);
    };
    let node = production_node(&stmt).map_err(|e| e.message.to_string())?;
    Ok(Some((text.len() - rest.len(), node)))
}

/// The `<statementlist>` core rule: the statements at the start of `text` as a
/// `RakuAST::StatementList` (ADR-16888).
// Cost: O(n), n = size of the list (parse and conversion).
fn statementlist_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, stmts)) = crate::parser::parse_slang_statementlist(text) else {
        return Ok(None);
    };
    let names = NameScope::default();
    let node =
        super::statements::statement_list(&names, &stmts).map_err(|e| e.message.to_string())?;
    Ok(Some((
        text.len() - rest.len(),
        Value::rakuast(Box::new(node)),
    )))
}

/// The `<signature>` core rule: the parameters at the start of `text` as a
/// `RakuAST::Signature`.
// Cost: O(n), n = size of the parameter list.
fn signature_production(text: &str) -> Result<Option<(usize, Value)>, String> {
    let Some((rest, params, returns)) = crate::parser::parse_slang_signature(text) else {
        return Ok(None);
    };
    let names = NameScope::default();
    let node = super::routine_origins::anon_signature(&names, &params, returns.as_deref())
        .map_err(|e| e.message.to_string())?;
    Ok(Some((
        text.len() - rest.len(),
        Value::rakuast(Box::new(node)),
    )))
}

/// The node `stmt` stands for: the expression of its `Statement::Expression`
/// (a `{ ... }` block is a `RakuAST::Block`), else the statement itself.
// Cost: O(n), n = size of the statement.
fn production_node(stmt: &Stmt) -> Result<Value, RuntimeError> {
    let names = NameScope::default();
    let list = super::statements::statement_list(&names, std::slice::from_ref(stmt))?;
    let Some(RakuAstFieldValue::Node(first)) = list.fields.first().map(|f| &f.value) else {
        return Err(RuntimeError::new("slang shim: the production has no node"));
    };
    if let ValueView::RakuAst(statement) = first.view()
        && let Some(expression) = statement
            .fields
            .iter()
            .find(|f| f.name == Some("expression"))
        && let RakuAstFieldValue::Node(expression) = &expression.value
    {
        return Ok(expression.clone());
    }
    Ok(first.clone())
}

/// Lower the statement node a slang action attached into the statement it
/// stands for at the hook.
// Cost: O(n), n = size of the node.
pub(crate) fn lower_statement(node: &Value) -> Result<Stmt, RuntimeError> {
    let ValueView::RakuAst(node) = node.view() else {
        return Err(RuntimeError::new(
            "slang production produced no RakuAST node",
        ));
    };
    let names = NameScope::default();
    let lowered = super::lower::lower_fragment(&names, node)?;
    Ok(Stmt::Ast(Box::new(
        crate::ast::parsed::ParsedStmt::Lowered {
            stmt: Box::new(lowered),
            node: Value::rakuast(Box::new(RakuAstNode::clone(node))),
        },
    )))
}

/// Lower the node a slang's `statementlist` wrapper answered (ADR-16888): the
/// statements of a `RakuAST::StatementList`, each as it is lowered at a hook,
/// or the one statement of any other node.
// Cost: O(n), n = size of the node.
pub(crate) fn lower_statements(node: &Value) -> Result<Vec<Stmt>, RuntimeError> {
    let ValueView::RakuAst(list) = node.view() else {
        return Err(RuntimeError::new(
            "slang production produced no RakuAST node",
        ));
    };
    if list.class != super::RakuAstClass::StatementList {
        return Ok(vec![lower_statement(node)?]);
    }
    let mut stmts = Vec::new();
    for field in &list.fields {
        if super::origin::is_origin(field) {
            continue;
        }
        let RakuAstFieldValue::Node(child) = &field.value else {
            continue;
        };
        if let ValueView::RakuAst(inner) = child.view()
            && inner.class == super::RakuAstClass::StatementEmpty
        {
            continue;
        }
        stmts.push(lower_statement(child)?);
    }
    Ok(stmts)
}

/// Lower the expression node a slang action attached into the expression it
/// stands for at the hook (a `term`, `value` or `number` production).
// Cost: O(n), n = size of the node.
pub(crate) fn lower_expression(node: &Value) -> Result<Expr, RuntimeError> {
    let ValueView::RakuAst(inner) = node.view() else {
        return Err(RuntimeError::new(
            "slang production produced no RakuAST node",
        ));
    };
    let names = NameScope::default();
    let _method_scope = super::underscore_bindings::MethodScope::enter(false);
    let lowered =
        crate::parser::with_rakuast_lexical_scope(|| super::lower::lower_expr(&names, inner))?;
    Ok(Expr::Ast(Box::new(
        crate::ast::parsed::ParsedExpr::Lowered {
            expr: Box::new(lowered),
            node: node.clone(),
        },
    )))
}

/// The pass that runs a slang's overrides of core actions (`INFIX-EXPR`,
/// `PREFIX-EXPR`; ADR-15762 slice 3) over the parsed unit, bottom-up, before the
/// unit is finalized: each infix or prefix application becomes the node the
/// slang's action attached (the core node, when it attached none).
///
/// Only what follows the `use` of the activating module is touched, up to the
/// end of the block that holds the `use`.
// Cost: O(n) for the walk, plus the slang's own action per application, n =
// size of the unit; O(1) when the unit has no override.
pub(crate) fn apply_action_overrides(stmts: &mut [Stmt]) -> Result<(), RuntimeError> {
    let Some(activator) = crate::parser::slang_action_activator() else {
        return Ok(());
    };
    struct Pass {
        activator: String,
        enabled: bool,
        failed: Option<RuntimeError>,
    }
    impl crate::ast_visit::VisitMut for Pass {
        // The refinement a `use` makes ends with the block that holds it.
        fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
            let enclosing = self.enabled;
            crate::ast_visit::walk_stmts_mut(self, body);
            self.enabled = enclosing;
        }
        fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
            crate::ast_visit::walk_stmt_mut(self, stmt);
            if matches!(stmt, Stmt::Use { module, .. } if *module == self.activator) {
                self.enabled = true;
            }
        }
        fn visit_expr_mut(&mut self, expr: &mut Expr) {
            crate::ast_visit::walk_expr_mut(self, expr);
            if !self.enabled || self.failed.is_some() {
                return;
            }
            let rule = match expr {
                Expr::Binary { .. } => "INFIX-EXPR",
                Expr::Unary { .. } => "PREFIX-EXPR",
                _ => return,
            };
            if !crate::parser::slang_overrides_action(rule) {
                return;
            }
            match apply_action(rule, expr, "") {
                Ok(lowered) => *expr = lowered,
                Err(e) => self.failed = Some(e),
            }
        }
    }
    let mut pass = Pass {
        activator,
        enabled: false,
        failed: None,
    };
    for stmt in stmts.iter_mut() {
        crate::ast_visit::VisitMut::visit_stmt_mut(&mut pass, stmt);
    }
    pass.failed.map_or(Ok(()), Err)
}

/// The slang's `numish` override over the numeric literal `expr`, read by the
/// parser from `source`: the expression for the node its action attached.
// Cost: the slang's own action, plus O(n) for the node, n = its size.
pub(crate) fn lower_numish(expr: &Expr, source: &str) -> Result<Expr, RuntimeError> {
    apply_action("numish", expr, source)
}

/// Run the slang's override of `rule` over the node of `expr`.
// Cost: O(n) in the application, plus the slang's action.
fn apply_action(rule: &str, expr: &Expr, source: &str) -> Result<Expr, RuntimeError> {
    let names = NameScope::default();
    let node = super::view::view_expr(&names, expr)?;
    let core = Value::rakuast(Box::new(node));
    let attached = crate::parser::run_slang_action(rule, core, source)
        .map_err(|e| RuntimeError::new(format!("slang action {rule}: {e}")))?;
    lower_expression(&attached)
}
