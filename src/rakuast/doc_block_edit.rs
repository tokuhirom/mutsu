//! The in-place builders and the config queries of `RakuAST::Doc::Block`, as
//! rakudo 2026.09 has them (`src/Raku/ast/doc.rakumod`), each measured against
//! it:
//!
//! - `set-type($type)` takes a `Str`, `set-level($level)` an `Int` (`0` means
//!   none) and `set-margin($margin)` a `Str`; all answer `Nil`.
//! - `set-paragraphs(@paragraphs)` replaces the list (a lone value is a list of
//!   one) and answers `Nil`; `add-paragraph($paragraph)` appends and answers
//!   what it added.
//! - `set-config(%config)` replaces the configuration and answers `Nil`;
//!   `add-config($key, $value)` sets one key (the last one wins) and answers
//!   the value. The block keeps its configuration as `key => value` pairs (see
//!   [`super::doc_block`]).
//! - `config-keys` and `config-pairs` are `List`s over that configuration;
//!   `leading-whitespace` is the whitespace at the start of the first
//!   paragraph (`Any` when there is none, `""` when it is not a paragraph
//!   node).

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

fn block(node: &RakuAstNode, method: &str) -> Result<(), RuntimeError> {
    if node.class == RakuAstClass::DocBlock {
        Ok(())
    } else {
        Err(RuntimeError::new(format!(
            "{method} is only available on RakuAST::Doc::Block"
        )))
    }
}

/// Put the node's fields in the order the constructor builds them (the class's
/// `model_fields` order), so a node edited in place prints as the one the
/// constructor would have made; other fields keep their place after those.
// Cost: O(f log f), f = fields of the node.
fn in_constructor_order(node: &mut RakuAstNode) {
    let order = super::fields::model_fields(node.class);
    let rank = |field: &RakuAstField| {
        order
            .iter()
            .position(|(name, _)| field.name == Some(*name))
            .unwrap_or(order.len())
    };
    node.fields.sort_by_key(rank);
}

fn type_error(param: &str, expected: &str, got: &Value) -> RuntimeError {
    RuntimeError::new(format!(
        "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
         '${param}'; expected {expected} but got {}",
        crate::value::type_name::value_type_name(got)
    ))
}

// Cost: O(f), f = fields of the node.
fn set_leaf(node: &mut RakuAstNode, name: &'static str, value: Option<Value>) {
    let at = node.fields.iter().position(|f| f.name == Some(name));
    match (at, value) {
        (Some(at), Some(value)) => node.fields[at].value = RakuAstFieldValue::Node(value),
        (Some(at), None) => {
            node.fields.remove(at);
        }
        (None, Some(value)) => node.fields.push(RakuAstField {
            name: Some(name),
            value: RakuAstFieldValue::Node(value),
        }),
        (None, None) => {}
    }
}

/// `set-type($type)`.
// Cost: O(f), f = fields of the node.
pub(crate) fn set_type(node: &mut RakuAstNode, value: &Value) -> Result<Value, RuntimeError> {
    block(node, "set-type")?;
    if !matches!(value.view(), ValueView::Str(_)) {
        return Err(type_error("type", "Str", value));
    }
    set_leaf(node, "type", Some(value.clone()));
    in_constructor_order(node);
    Ok(Value::NIL)
}

/// `set-margin($margin)`.
// Cost: O(f), f = fields of the node.
pub(crate) fn set_margin(node: &mut RakuAstNode, value: &Value) -> Result<Value, RuntimeError> {
    block(node, "set-margin")?;
    if !matches!(value.view(), ValueView::Str(_)) {
        return Err(type_error("margin", "Str", value));
    }
    set_leaf(node, "margin", Some(value.clone()));
    in_constructor_order(node);
    Ok(Value::NIL)
}

/// `set-level($level)`: `0` is no level at all.
// Cost: O(f), f = fields of the node.
pub(crate) fn set_level(node: &mut RakuAstNode, value: &Value) -> Result<Value, RuntimeError> {
    block(node, "set-level")?;
    let Some(level) = value.as_int() else {
        return Err(type_error("level", "Int", value));
    };
    set_leaf(node, "level", (level != 0).then(|| value.clone()));
    in_constructor_order(node);
    Ok(Value::NIL)
}

// Cost: O(f), f = fields of the node.
fn list_mut<'a>(node: &'a mut RakuAstNode, name: &'static str) -> &'a mut Vec<Value> {
    let at = node
        .fields
        .iter()
        .position(|f| f.name == Some(name) && matches!(f.value, RakuAstFieldValue::List(_)))
        .unwrap_or_else(|| {
            node.fields.push(RakuAstField {
                name: Some(name),
                value: RakuAstFieldValue::List(Vec::new()),
            });
            node.fields.len() - 1
        });
    match &mut node.fields[at].value {
        RakuAstFieldValue::List(items) => items,
        _ => unreachable!("the field was just made a list"),
    }
}

/// `set-paragraphs(@paragraphs)`.
// Cost: O(f + n), f = fields of the node, n = paragraphs given.
pub(crate) fn set_paragraphs(node: &mut RakuAstNode, value: &Value) -> Result<Value, RuntimeError> {
    block(node, "set-paragraphs")?;
    let items = value
        .as_list_items()
        .map_or_else(|| vec![value.clone()], <[Value]>::to_vec);
    *list_mut(node, "paragraphs") = items;
    in_constructor_order(node);
    Ok(Value::NIL)
}

/// `add-paragraph($paragraph)`.
// Cost: O(f), f = fields of the node.
pub(crate) fn add_paragraph(
    node: &mut RakuAstNode,
    paragraph: &Value,
) -> Result<Value, RuntimeError> {
    block(node, "add-paragraph")?;
    list_mut(node, "paragraphs").push(paragraph.clone());
    in_constructor_order(node);
    Ok(paragraph.clone())
}

/// `set-config(%config)`: the pairs of a `Map`/`Hash` in key order, or a list
/// of pairs as given; `Nil` is accepted and changes nothing.
// Cost: O(f + k log k), f = fields of the node, k = keys given.
pub(crate) fn set_config(node: &mut RakuAstNode, config: &Value) -> Result<Value, RuntimeError> {
    block(node, "set-config")?;
    if config.is_nil() {
        return Ok(Value::NIL);
    }
    let pairs = super::doc_block::config_pairs(config).or_else(|| {
        // A list of pairs, kept as `key => value` with a text key.
        config
            .as_list_items()?
            .iter()
            .map(|item| match item.view() {
                ValueView::Pair(key, value) => Some(Value::pair(key.to_string(), value.clone())),
                ValueView::ValuePair(key, value) => {
                    Some(Value::pair(key.to_string_value(), value.clone()))
                }
                _ => None,
            })
            .collect()
    });
    let Some(pairs) = pairs else {
        return Err(RuntimeError::new(format!(
            "A doc block config must be a map or a list of pairs, got {}",
            crate::value::type_name::value_type_name(config)
        )));
    };
    *list_mut(node, "config") = pairs;
    in_constructor_order(node);
    Ok(Value::NIL)
}

/// `add-config($key, $value)`: the last value for a key wins.
// Cost: O(f + k), f = fields of the node, k = config pairs.
pub(crate) fn add_config(
    node: &mut RakuAstNode,
    key: &Value,
    value: &Value,
) -> Result<Value, RuntimeError> {
    block(node, "add-config")?;
    let ValueView::Str(key) = key.view() else {
        return Err(type_error("key", "Str", key));
    };
    let pairs = list_mut(node, "config");
    let key = key.to_string();
    pairs.retain(|pair| !matches!(pair.view(), ValueView::Pair(k, _) if *k == key));
    pairs.push(Value::pair(key, value.clone()));
    pairs.sort_by_key(|pair| match pair.view() {
        ValueView::Pair(k, _) => k.to_string(),
        _ => String::new(),
    });
    in_constructor_order(node);
    Ok(value.clone())
}

/// `config-keys`: the configuration's keys, in order, as a `List`.
// Cost: O(k), k = config pairs.
pub(crate) fn config_keys(node: &RakuAstNode) -> Value {
    Value::array(
        config_items(node)
            .into_iter()
            .filter_map(|pair| match pair.view() {
                ValueView::Pair(key, _) => Some(Value::str(key.to_string())),
                _ => None,
            })
            .collect(),
    )
}

/// `config-pairs`: the configuration's pairs, in order, as a `List`.
// Cost: O(k), k = config pairs.
pub(crate) fn config_pairs(node: &RakuAstNode) -> Value {
    Value::array(config_items(node))
}

fn config_items(node: &RakuAstNode) -> Vec<Value> {
    node.fields
        .iter()
        .find(|f| f.name == Some("config"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => Some(items.clone()),
            _ => None,
        })
        .unwrap_or_default()
}

/// `leading-whitespace`: the whitespace at the start of the first paragraph.
// Cost: O(w), w = the leading whitespace.
pub(crate) fn leading_whitespace(node: &RakuAstNode) -> Value {
    let first = node
        .fields
        .iter()
        .find(|f| f.name == Some("paragraphs"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => items.first().cloned(),
            _ => None,
        });
    match first.as_ref().map(Value::view) {
        Some(ValueView::RakuAst(paragraph)) => super::doc_edit::leading_whitespace(paragraph),
        Some(_) => Value::str(String::new()),
        None => Value::package(crate::symbol::Symbol::intern("Any")),
    }
}

/// The named arguments of `args` as `(name, value)`, the last of a repeated
/// name winning (as `named_arg` does).
// Cost: O(a), a = arguments.
fn named_pairs(args: &[Value]) -> Vec<(String, Value)> {
    args.iter()
        .filter_map(|arg| match arg.view() {
            ValueView::Pair(key, value) => Some((key.to_string(), value.clone())),
            ValueView::ValuePair(key, value) => Some((key.to_string_value(), value.clone())),
            _ => None,
        })
        .collect()
}

/// `Doc::Block.from-alias(:type, :$lemma, :@paragraphs, ...)`: a `Doc::Block`
/// whose paragraphs are the lemma and then the text of `@paragraphs`, each
/// element ending in a newline (`""` when there are none). A missing lemma is
/// the `Any` type object. The rest goes to `new` as it is.
// Cost: O(a + n), a = arguments, n = size of the paragraphs.
pub(crate) fn from_alias(args: &[Value]) -> Result<Value, RuntimeError> {
    let mut pairs = named_pairs(args);
    let take = |pairs: &mut Vec<(String, Value)>, name: &str| {
        let at = pairs.iter().rposition(|(key, _)| key == name)?;
        Some(pairs.remove(at).1)
    };
    let lemma = take(&mut pairs, "lemma")
        .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Any")));
    let given = take(&mut pairs, "paragraphs")
        .and_then(|list| list.as_list_items().map(<[Value]>::to_vec))
        .unwrap_or_default();
    let text: String = given
        .iter()
        .map(|item| {
            let mut text = match item.view() {
                ValueView::RakuAst(node) => super::doc_str::text(node),
                _ => item.to_string_value(),
            };
            text.push('\n');
            text
        })
        .collect();
    pairs.retain(|(key, _)| key != "paragraphs" && key != "lemma");
    let mut rest = pairs;
    rest.push((
        "paragraphs".to_string(),
        Value::array(vec![lemma, Value::str(text)]),
    ));
    construct_block(rest)
}

/// `Doc::Block.from-config(:type, :$key, ...)`: a `Doc::Block` whose only
/// paragraph is the key.
// Cost: O(a), a = arguments.
pub(crate) fn from_config(args: &[Value]) -> Result<Value, RuntimeError> {
    let mut pairs = named_pairs(args);
    let key = pairs
        .iter()
        .rposition(|(name, _)| name == "key")
        .map(|at| pairs.remove(at).1)
        .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Any")));
    pairs.retain(|(name, _)| name != "key" && name != "paragraphs");
    pairs.push(("paragraphs".to_string(), Value::array(vec![key])));
    construct_block(pairs)
}

// Cost: O(a), a = arguments.
fn construct_block(pairs: Vec<(String, Value)>) -> Result<Value, RuntimeError> {
    let args: Vec<Value> = pairs
        .into_iter()
        .map(|(name, value)| Value::pair(name, value))
        .collect();
    super::named_ctor::construct("RakuAST::Doc::Block", &args).unwrap_or_else(|| {
        Err(RuntimeError::new(
            "RakuAST::Doc::Block.new is not available",
        ))
    })
}
