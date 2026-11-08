//! `PRE` / `POST` phasers across the RakuAST boundary.
//!
//! Rakudo models `PRE { COND }` as `StatementPrefix::Phaser::Pre` over a
//! statement that *calls* the block (`ApplyPostfix(Block, Call::Term)`), `POST
//! { COND }` as `Phaser::Post` over the block, and the bare forms (`PRE COND`)
//! over the statement itself (measured on 2026.09).
//!
//! mutsu also keeps the phaser's argument as verbatim source text
//! (`Stmt::Phaser::condition`): `X::Phaser::PrePost.condition` and its message
//! quote it, and no deparse of the tree can reproduce it. The view keeps
//! that text in a hidden `condition-source` field, the way a statement keeps
//! its line (`origin`) and a regex code block its spelling (`regex_code`), and
//! `lower` puts it back. A hand-built node has none, so its phaser lowers with
//! no condition text, as a synthesized one has.
//!
//! The field is part of the model but not of the constructor form: Rakudo's
//! `.raku` shows no source text either.

use super::lower::{lower_block, named_child_or_positional};
use super::view::{block_node, node_field, statement_expression};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{PhaserKind, Stmt};
use crate::scope::NameScope;
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
pub(super) fn view_node(
    names: &NameScope,
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
        let block = block_node(names, body)?;
        if is_pre {
            statement_expression(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::ApplyPostfix,
                fields: vec![
                    node_field(Some("operand"), block),
                    node_field(
                        Some("postfix"),
                        RakuAstNode {
                            origin: Default::default(),
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
        let Some(node) = super::view::view_stmt(names, stmt)? else {
            return Ok(None);
        };
        node
    };
    Ok(Some(statement_expression(RakuAstNode {
        origin: Default::default(),
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
pub(super) fn lower(
    names: &NameScope,
    node: &RakuAstNode,
    kind: PhaserKind,
) -> Result<Stmt, RuntimeError> {
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
        RakuAstClass::Block => lower_block(names, positional)?,
        // `PRE { COND }`: the statement calls the block; the body is the block's.
        RakuAstClass::StatementExpression if let Some(block) = called_block(positional) => {
            lower_block(names, block)?
        }
        _ => vec![super::lower::lower_stmt(names, positional)?],
    };
    Ok(Stmt::Phaser {
        spans: Default::default(),
        kind,
        body,
        condition,
        end_index: None,
        bare: false,
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

/// `Phaser::Pre.blorst`: rakudo wraps the written statement into
/// `X::Phaser::PrePost.new(condition => "SRC").throw unless <statement>`, so the
/// statement becomes the `condition-modifier` (`RakuAST::Deparse` reads it back
/// from there). `original-blorst` is the statement as written.
///
/// `None` for a node with no recorded source text (a hand-built one), which has
/// nothing to quote.
// Cost: O(n), n = size of the node.
pub(super) fn wrapped_blorst(node: &RakuAstNode) -> Option<Value> {
    let source = node.fields.iter().find(|f| f.name == Some(FIELD))?;
    let RakuAstFieldValue::Node(text) = &source.value else {
        return None;
    };
    let statement = named_child_or_positional(node).ok()?;
    let written = super::lower::named_child(statement, "expression").ok()?;
    let node_of = |class, fields| RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    };
    let call = |name: &str, args: Option<RakuAstNode>| {
        let mut fields = vec![node_field(
            Some("name"),
            super::view::name_from_identifier(name),
        )];
        fields.extend(args.map(|a| node_field(Some("args"), a)));
        node_of(RakuAstClass::CallMethod, fields)
    };
    // `condition => "SRC"`, built as rakudo does: a `ColonPair::Value` over a
    // plain `StrLiteral`.
    let condition = node_of(
        RakuAstClass::ColonPairValue,
        vec![
            super::view::leaf_field(Some("key"), Value::str("condition".to_string())),
            node_field(
                Some("value"),
                node_of(
                    RakuAstClass::StrLiteral,
                    vec![super::view::leaf_field(None, text.clone())],
                ),
            ),
        ],
    );
    let exception = node_of(
        RakuAstClass::TypeSimple,
        vec![node_field(
            None,
            super::name_parts::name_from_parts(
                ["X", "Phaser", "PrePost"]
                    .iter()
                    .map(|part| super::name_parts::simple_part(part))
                    .collect(),
            ),
        )],
    );
    let created = node_of(
        RakuAstClass::ApplyPostfix,
        vec![
            node_field(Some("operand"), exception),
            node_field(
                Some("postfix"),
                call(
                    "new",
                    Some(node_of(
                        RakuAstClass::ArgList,
                        vec![node_field(None, condition)],
                    )),
                ),
            ),
        ],
    );
    let throw = node_of(
        RakuAstClass::ApplyPostfix,
        vec![
            node_field(Some("operand"), created),
            node_field(Some("postfix"), call("throw", None)),
        ],
    );
    let unless = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementModifierUnless,
        fields: vec![node_field(None, written.clone())],
    };
    Some(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementExpression,
        fields: vec![
            node_field(Some("expression"), throw),
            node_field(Some("condition-modifier"), unless),
        ],
    })))
}
