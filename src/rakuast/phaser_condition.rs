//! `PRE` / `POST` phasers across the RakuAST boundary.
//!
//! Rakudo models `PRE { COND }` as `StatementPrefix::Phaser::Pre` over a
//! statement that *calls* the block (`ApplyPostfix(Block, Call::Term)`), `POST
//! { COND }` as `Phaser::Post` over the block, and the bare forms (`PRE COND`)
//! over the statement itself (measured on 2026.09).
//!
//! mutsu also keeps the phaser's argument as verbatim source text
//! (`Stmt::Phaser::condition`): `X::Phaser::PrePost.condition` and its message
//! quote it, and no deparse of the tree can reproduce it. The converter keeps
//! that text in a hidden `condition-source` field, the way a statement keeps
//! its line (`origin`) and a regex code block its spelling (`regex_code`), and
//! `lower` puts it back. A hand-built node has none, so its phaser lowers with
//! no condition text, as a synthesized one has.
//!
//! The field is part of the model but not of the constructor form: Rakudo's
//! `.raku` shows no source text either.

use super::convert::{block_node, node_field, statement_expression};
use super::lower::{lower_block, named_child_or_positional};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{PhaserKind, Stmt};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The hidden field's name.
const FIELD: &str = "condition-source";

/// Whether `field` is the hidden condition text of a `PRE` / `POST` node.
// Cost: O(1).
pub(super) fn is_source(node: &RakuAstNode, field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
        && matches!(
            node.class,
            RakuAstClass::StatementPrefixPhaserPre | RakuAstClass::StatementPrefixPhaserPost
        )
}

/// The node of a `PRE` / `POST` phaser whose body is `body` and whose argument
/// was written `condition`.
// Cost: O(n), n = size of the body.
pub(super) fn convert(
    kind: &PhaserKind,
    body: &[Stmt],
    condition: &Symbol,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let (class, is_pre) = match kind {
        PhaserKind::Pre => (RakuAstClass::StatementPrefixPhaserPre, true),
        PhaserKind::Post => (RakuAstClass::StatementPrefixPhaserPost, false),
        _ => return Ok(None),
    };
    let text = condition.resolve();
    let positional = if text.trim_start().starts_with('{') {
        let block = block_node(body)?;
        if is_pre {
            statement_expression(RakuAstNode {
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), block),
                    node_field(
                        Some("postfix"),
                        RakuAstNode {
                            class: RakuAstClass::CallTerm,
                            fields: Vec::new(),
                        },
                    ),
                ],
            })
        } else {
            block
        }
    } else {
        // The bare form `PRE COND`: the written statement.
        let [stmt] = body else { return Ok(None) };
        let Some(node) = super::convert::convert_stmt(stmt)? else {
            return Ok(None);
        };
        node
    };
    Ok(Some(statement_expression(RakuAstNode {
        class,
        fields: vec![
            node_field(None, positional),
            RakuAstField {
                name: Some(FIELD),
                value: RakuAstFieldValue::Node(Value::str(text)),
            },
        ],
    })))
}

/// The statement a `PRE` / `POST` node lowers to.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode, kind: PhaserKind) -> Result<Stmt, RuntimeError> {
    let condition = node
        .fields
        .iter()
        .find(|f| f.name == Some(FIELD))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => Some(Symbol::intern(s.as_str())),
                _ => None,
            },
            _ => None,
        });
    let positional = named_child_or_positional(node)?;
    let body = match positional.class {
        RakuAstClass::Block => lower_block(positional)?,
        // `PRE { COND }`: the statement calls the block; the body is the block's.
        RakuAstClass::StatementExpression if let Some(block) = called_block(positional) => {
            lower_block(block)?
        }
        _ => vec![super::lower::lower_stmt(positional)?],
    };
    Ok(Stmt::Phaser {
        kind,
        body,
        condition,
        end_index: None,
    })
}

/// The block `Statement::Expression(ApplyPostfix(Block, Call::Term))` calls.
fn called_block(statement: &RakuAstNode) -> Option<&RakuAstNode> {
    let expression = super::lower::named_child(statement, "expression").ok()?;
    if expression.class != RakuAstClass::ApplyPostfix {
        return None;
    }
    let postfix = super::lower::named_child(expression, "postfix").ok()?;
    if postfix.class != RakuAstClass::CallTerm {
        return None;
    }
    let operand = super::lower::named_child(expression, "operand").ok()?;
    (operand.class == RakuAstClass::Block).then_some(operand)
}
