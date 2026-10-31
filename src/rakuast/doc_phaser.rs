//! DOC phasers have no public executable node in Rakudo's ordinary AST:
//! without `--doc`, their visible statement is `Nil`. Keep the original
//! phaser behind that public statement so lowering can restore `--doc`
//! execution without running a parse-time probe.

use crate::scope::NameScope;
use super::convert::{
    convert_stmt, name_from_identifier, node_field, statement_expression, unsupported,
};
use super::lower::{child_node, lower_stmt};
use super::{RakuAstClass, RakuAstField, RakuAstNode};
use crate::ast::Stmt;
use crate::value::RuntimeError;

const SOURCE_DOC_PHASER: &str = "source-doc-phaser";

// Cost: O(n), n = nodes in the phaser body.
pub(super) fn convert(names: &NameScope, inner: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    let original = convert_stmt(names, inner)?.ok_or_else(|| unsupported("DOC phaser body"))?;
    let nil = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermName,
        fields: vec![node_field(None, name_from_identifier("Nil"))],
    };
    let mut public = statement_expression(nil);
    public
        .fields
        .push(node_field(Some(SOURCE_DOC_PHASER), original));
    Ok(public)
}

// Cost: O(n), n = nodes in the phaser body.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Stmt, RuntimeError>> {
    let field = node
        .fields
        .iter()
        .find(|field| field.name == Some(SOURCE_DOC_PHASER))?;
    Some(
        child_node(&field.value)
            .and_then(lower_stmt)
            .map(|stmt| Stmt::DocPhaser(Box::new(stmt))),
    )
}

// Cost: O(1).
pub(super) fn is_source_field(field: &RakuAstField) -> bool {
    field.name == Some(SOURCE_DOC_PHASER)
}
