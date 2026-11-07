//! The RakuAST end of the slang host (ADR-15762 §2.3, §2.5): core productions
//! the Rust parser parses on a slang token's behalf become RakuAST nodes, and the
//! node a slang action attaches is lowered where the production stood.

use super::{RakuAstFieldValue, RakuAstNode};
use crate::ast::Stmt;
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
    let parsed = crate::parser::parse_slang_shim_production(rule, text).ok_or_else(|| {
        format!("the core rule <{rule}> has no slang shim yet (ADR-15762 §2.3)")
    })?;
    let Ok((rest, stmt)) = parsed else {
        return Ok(None);
    };
    let node = production_node(&stmt).map_err(|e| e.message.to_string())?;
    Ok(Some((text.len() - rest.len(), node)))
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
        return Err(RuntimeError::new("slang production produced no RakuAST node"));
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
