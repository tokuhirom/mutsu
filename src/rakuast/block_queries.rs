//! The question rakudo 2026.09's `RakuAST::Block` answers about its
//! placeholder parameters (`src/Raku/ast/blocks-and-sub-signatures.rakumod`):
//! `has-placeholder-parameters` is true when the block's own body declares one
//! (`$^a`, `:$^n`, `@_`, `%_`); a placeholder in a nested block, pointy block
//! or routine belongs to that one. Measured over nine shapes.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(n), n = size of the block's own body.
fn declares_placeholder(node: &RakuAstNode) -> bool {
    node.fields.iter().any(|field| {
        let values: Vec<&Value> = match &field.value {
            RakuAstFieldValue::Node(v) => vec![v],
            RakuAstFieldValue::List(items) => items.iter().collect(),
            RakuAstFieldValue::Adverb(_) => Vec::new(),
        };
        values.into_iter().any(|v| match v.view() {
            ValueView::RakuAst(child) => match child.class {
                RakuAstClass::VarDeclarationPlaceholderPositional
                | RakuAstClass::VarDeclarationPlaceholderNamed
                | RakuAstClass::VarDeclarationPlaceholderSlurpyArray
                | RakuAstClass::VarDeclarationPlaceholderSlurpyHash => true,
                // Their placeholders are their own.
                RakuAstClass::Block
                | RakuAstClass::PointyBlock
                | RakuAstClass::Sub
                | RakuAstClass::Method
                | RakuAstClass::Submethod => false,
                _ => declares_placeholder(child),
            },
            _ => false,
        })
    })
}

/// `has-placeholder-parameters`.
// Cost: O(n), n = size of the block's own body.
pub(crate) fn has_placeholder_parameters(node: &RakuAstNode) -> Value {
    Value::truth(declares_placeholder(node))
}

/// The placeholder declarations of the block's own body, in source order.
// Cost: O(n), n = size of the block's own body.
fn collect_placeholders<'a>(node: &'a RakuAstNode, out: &mut Vec<&'a RakuAstNode>) {
    for field in &node.fields {
        let values: &[Value] = match &field.value {
            RakuAstFieldValue::Node(v) => std::slice::from_ref(v),
            RakuAstFieldValue::List(items) => items,
            RakuAstFieldValue::Adverb(_) => &[],
        };
        for value in values {
            let ValueView::RakuAst(child) = value.view() else {
                continue;
            };
            match child.class {
                RakuAstClass::VarDeclarationPlaceholderPositional
                | RakuAstClass::VarDeclarationPlaceholderNamed
                | RakuAstClass::VarDeclarationPlaceholderSlurpyArray
                | RakuAstClass::VarDeclarationPlaceholderSlurpyHash => out.push(child),
                RakuAstClass::Block
                | RakuAstClass::PointyBlock
                | RakuAstClass::Sub
                | RakuAstClass::Method
                | RakuAstClass::Submethod => {}
                _ => collect_placeholders(child, out),
            }
        }
    }
}

/// `placeholder-signature`: the `Signature` the block's placeholders make, as
/// rakudo builds it -- the positional ones sorted by name, then the named
/// ones, then `@_` and `%_` as flattening slurpies; `Any` when the block has
/// none.
// Cost: O(n + p log p), n = size of the block's own body, p = placeholders.
pub(crate) fn placeholder_signature(node: &RakuAstNode) -> Value {
    let mut found = Vec::new();
    collect_placeholders(node, &mut found);
    if found.is_empty() {
        return Value::package(crate::symbol::Symbol::intern("Any"));
    }
    let mut positional: Vec<(char, String)> = Vec::new();
    let mut named: Vec<(char, String)> = Vec::new();
    let (mut array, mut hash) = (false, false);
    for decl in found {
        match decl.class {
            RakuAstClass::VarDeclarationPlaceholderSlurpyArray => array = true,
            RakuAstClass::VarDeclarationPlaceholderSlurpyHash => hash = true,
            class => {
                let Ok(spelled) = super::placeholder::spelling(decl) else {
                    continue;
                };
                let list = if class == RakuAstClass::VarDeclarationPlaceholderNamed {
                    &mut named
                } else {
                    &mut positional
                };
                if !list.contains(&spelled) {
                    list.push(spelled);
                }
            }
        }
    }
    positional.sort_by(|a, b| a.1.cmp(&b.1));
    let field = |name: &'static str, value: RakuAstFieldValue| super::RakuAstField {
        name: Some(name),
        value,
    };
    let target = |spelled: String| {
        let var = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ParameterTargetVar,
            fields: vec![field("name", RakuAstFieldValue::Node(Value::str(spelled)))],
        };
        field(
            "target",
            RakuAstFieldValue::Node(Value::rakuast(Box::new(var))),
        )
    };
    let parameter = |fields: Vec<super::RakuAstField>| {
        Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Parameter,
            fields,
        }))
    };
    let not_optional = || field("optional", RakuAstFieldValue::Node(Value::truth(false)));
    let mut parameters = Vec::new();
    for (sigil, name) in positional {
        parameters.push(parameter(vec![
            target(format!("{sigil}{name}")),
            not_optional(),
        ]));
    }
    for (sigil, name) in named {
        let names = field(
            "names",
            RakuAstFieldValue::List(vec![Value::str(name.clone())]),
        );
        parameters.push(parameter(vec![
            names,
            target(format!("{sigil}{name}")),
            not_optional(),
        ]));
    }
    for (present, spelled) in [(array, "@_"), (hash, "%_")] {
        if present {
            let slurpy = super::slurpy_marker_value(RakuAstClass::ParameterSlurpyFlattened);
            parameters.push(parameter(vec![
                target(spelled.to_string()),
                field("slurpy", RakuAstFieldValue::Node(slurpy)),
            ]));
        }
    }
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Signature,
        fields: vec![field("parameters", RakuAstFieldValue::List(parameters))],
    }))
}

/// Whether `class` is one rakudo's `Block.replace-body` is inherited by: the
/// block, the pointy block and every routine.
fn takes_a_body(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::Block
            | RakuAstClass::PointyBlock
            | RakuAstClass::Sub
            | RakuAstClass::Method
            | RakuAstClass::Submethod
    )
}

/// `replace-body($new-body)`: the node's `body` field becomes `$new-body`,
/// which must be a `RakuAST::Blockoid`.
// Cost: O(f), f = fields of the node.
pub(crate) fn replace_body(node: &mut RakuAstNode, new_body: Value) -> Result<(), RuntimeError> {
    if !takes_a_body(node.class) {
        return Err(RuntimeError::new(
            "replace-body is only available on RakuAST::Block",
        ));
    }
    if !matches!(new_body.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::Blockoid) {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$new-body'; expected RakuAST::Blockoid but got {}",
            crate::value::type_name::value_type_name(&new_body)
        )));
    }
    let value = RakuAstFieldValue::Node(new_body);
    match node.fields.iter_mut().find(|f| f.name == Some("body")) {
        Some(slot) => slot.value = value,
        None => node.fields.push(super::RakuAstField {
            name: Some("body"),
            value,
        }),
    }
    Ok(())
}

/// `replace-signature($new-signature)`: the routine's `signature` field becomes
/// `$new-signature`, which must be a `RakuAST::Signature`.
// Cost: O(f), f = fields of the node.
pub(crate) fn replace_signature(
    node: &mut RakuAstNode,
    new_signature: &Value,
) -> Result<(), RuntimeError> {
    if !matches!(
        node.class,
        RakuAstClass::Sub
            | RakuAstClass::Method
            | RakuAstClass::Submethod
            | RakuAstClass::PointyBlock
    ) {
        return Err(RuntimeError::new(
            "replace-signature is only available on RakuAST::Routine and RakuAST::PointyBlock",
        ));
    }
    if !matches!(new_signature.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::Signature)
    {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$new-signature'; expected RakuAST::Signature but got {}",
            crate::value::type_name::value_type_name(new_signature)
        )));
    }
    let value = RakuAstFieldValue::Node(new_signature.clone());
    match node.fields.iter_mut().find(|f| f.name == Some("signature")) {
        Some(slot) => slot.value = value,
        None => node.fields.push(super::RakuAstField {
            name: Some("signature"),
            value,
        }),
    }
    Ok(())
}

/// `replace-scope($scope)` on a declaration: the `scope` field becomes `$scope`
/// (`'my'`, `'our'`, `'has'`, ...), written first as the constructor prints it.
// Cost: O(f), f = fields of the node.
pub(crate) fn replace_scope(node: &mut RakuAstNode, scope: &Value) -> Result<(), RuntimeError> {
    let Some(text) = scope.as_str() else {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$scope'; expected Str but got {}",
            crate::value::type_name::value_type_name(scope)
        )));
    };
    let value = RakuAstFieldValue::Node(Value::str(text.to_string()));
    match node.fields.iter_mut().find(|f| f.name == Some("scope")) {
        Some(slot) => slot.value = value,
        None => node.fields.insert(
            0,
            super::RakuAstField {
                name: Some("scope"),
                value,
            },
        ),
    }
    Ok(())
}

/// `replace-name($name)` on a routine: the `name` field becomes `$name`, which
/// must be a `RakuAST::Name`.
// Cost: O(f), f = fields of the node.
pub(crate) fn replace_name(node: &mut RakuAstNode, name: &Value) -> Result<(), RuntimeError> {
    if !matches!(name.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::Name) {
        return Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$name'; expected RakuAST::Name but got {}",
            crate::value::type_name::value_type_name(name)
        )));
    }
    let value = RakuAstFieldValue::Node(name.clone());
    match node.fields.iter_mut().find(|f| f.name == Some("name")) {
        Some(slot) => slot.value = value,
        None => {
            // After a leading `scope`, as the constructor prints the fields.
            let at = usize::from(node.fields.first().is_some_and(|f| f.name == Some("scope")));
            node.fields.insert(
                at,
                super::RakuAstField {
                    name: Some("name"),
                    value,
                },
            );
        }
    }
    Ok(())
}

/// `set-implicit-topic($on, :required, :exception)` on a block: marks it as
/// taking the topic of its statement. Turning it off drops the three flags.
// Cost: O(f), f = fields of the node.
pub(crate) fn set_implicit_topic(
    node: &mut RakuAstNode,
    on: bool,
    required: bool,
    exception: bool,
) -> Result<(), RuntimeError> {
    if !matches!(node.class, RakuAstClass::Block | RakuAstClass::PointyBlock) {
        return Err(RuntimeError::new(
            "set-implicit-topic is only available on RakuAST::Block",
        ));
    }
    let flags = [
        ("implicit-topic", on),
        ("required-topic", on && required),
        ("exception", on && exception),
    ];
    for (name, set) in flags {
        node.fields.retain(|f| f.name != Some(name));
        if set {
            node.fields.push(super::view::true_field(name));
        }
    }
    Ok(())
}

/// The hidden field marking a statement-level block. Rakudo's `bare-block` is
/// parse-time state (`Block.new(:bare-block)` is rejected), `True` for a block
/// that is a whole statement (`{ 2 };`, also nested ones) and `False` for every
/// other block, a hand-built one included, so the statement conversion records
/// it and the row reads it back.
const BARE_BLOCK: &str = "bare-block";

/// Whether `field` is the hidden statement-level marker of a `Block`.
// Cost: O(1).
pub(super) fn is_bare_marker(node: &RakuAstNode, field: &super::RakuAstField) -> bool {
    node.class == RakuAstClass::Block && field.name == Some(BARE_BLOCK)
}

/// `node` marked as a statement-level block (a `PointyBlock` is never one).
// Cost: O(1).
pub(super) fn bare_block(mut node: RakuAstNode) -> RakuAstNode {
    if node.class == RakuAstClass::Block {
        node.fields.push(super::RakuAstField {
            name: Some(BARE_BLOCK),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        });
    }
    node
}

/// `bare-block`: whether the block is a statement of its own.
// Cost: O(f), f = fields of the block.
pub(crate) fn bare_block_flag(node: &RakuAstNode) -> Value {
    Value::truth(node.fields.iter().any(|f| is_bare_marker(node, f)))
}

/// The classes of the expressions of the top-level statements of the block
/// `node`'s own body: the phasers a block collects (`ENTER`, `LEAVE`, the loop
/// ones, `PRE`, `POST`) are those; a nested block's statements are its own and
/// `BEGIN`, `CHECK`, `INIT`, `END` belong to the compilation unit.
// Cost: O(s), s = top-level statements of the body.
fn own_statement_classes(node: &RakuAstNode) -> Vec<RakuAstClass> {
    fn node_of(value: &Value) -> Option<&RakuAstNode> {
        match value.view() {
            ValueView::RakuAst(n) => Some(n),
            _ => None,
        }
    }
    fn field_node<'a>(node: &'a RakuAstNode, name: &str) -> Option<&'a RakuAstNode> {
        node.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name == Some(name) => node_of(v),
            _ => None,
        })
    }
    let mut out = Vec::new();
    let Some(blockoid) = field_node(node, "body") else {
        return out;
    };
    let list = blockoid.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) => node_of(v).filter(|n| n.class == RakuAstClass::StatementList),
        _ => None,
    });
    let Some(list) = list else { return out };
    for f in &list.fields {
        let RakuAstFieldValue::Node(v) = &f.value else {
            continue;
        };
        let Some(stmt) = node_of(v) else { continue };
        out.push(field_node(stmt, "expression").map_or(stmt.class, |e| e.class));
    }
    out
}

/// The phaser a class stands for, as `has-phaser` names it.
// Cost: O(1).
fn phaser_name(class: RakuAstClass) -> Option<&'static str> {
    use RakuAstClass::*;
    Some(match class {
        StatementPrefixPhaserEnter => "ENTER",
        StatementPrefixPhaserLeave => "LEAVE",
        StatementPrefixPhaserKeep => "KEEP",
        StatementPrefixPhaserUndo => "UNDO",
        StatementPrefixPhaserFirst => "FIRST",
        StatementPrefixPhaserNext => "NEXT",
        StatementPrefixPhaserLast => "LAST",
        StatementPrefixPhaserPre => "PRE",
        StatementPrefixPhaserPost => "POST",
        _ => return None,
    })
}

/// `has-any-phasers`: the block's own body holds a phaser a block collects.
// Cost: O(s), s = top-level statements of the body.
pub(crate) fn has_any_phasers(node: &RakuAstNode) -> Value {
    Value::truth(
        own_statement_classes(node)
            .into_iter()
            .any(|class| phaser_name(class).is_some()),
    )
}

/// `has-loop-phasers`: the block's own body holds `FIRST`, `NEXT` or `LAST`.
// Cost: O(s), s = top-level statements of the body.
pub(crate) fn has_loop_phasers(node: &RakuAstNode) -> Value {
    Value::truth(
        own_statement_classes(node)
            .into_iter()
            .filter_map(phaser_name)
            .any(|name| matches!(name, "FIRST" | "NEXT" | "LAST")),
    )
}

/// `has-phaser($name)`: the block's own body holds that phaser.
// Cost: O(s), s = top-level statements of the body.
pub(crate) fn has_phaser(node: &RakuAstNode, name: &str) -> Value {
    Value::truth(
        own_statement_classes(node)
            .into_iter()
            .filter_map(phaser_name)
            .any(|phaser| phaser == name),
    )
}
