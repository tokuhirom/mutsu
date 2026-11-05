//! The builders and queries of `RakuAST::Doc::Paragraph` and
//! `RakuAST::Doc::Markup` that the accessors do not cover, as rakudo 2026.09
//! has them (`src/Raku/ast/doc.rakumod`), each measured against it:
//!
//! - `Paragraph.from-string($text)` is a class method that answers the text
//!   itself; `leading-whitespace` is the run of whitespace at the start of the
//!   first atom (`""` when that atom is not text or there is none).
//! - `Paragraph.add-atom($atom)` and `Markup.add-atom($atom)` append in place
//!   and answer the atom; `Markup.add-meta($item)` does the same for the meta.
//! - `Markup.set-atoms(@atoms)` / `set-meta(@meta)` replace the list (a lone
//!   value is a list of one), `set-opener($text)` / `set-closer($text)` replace
//!   the delimiter (it must be a `Str`); all answer `Nil`.
//! - `Markup.flatten` is the text between the delimiters (the atoms, nested
//!   codes spelled out, then the meta).

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, origin};
use crate::value::{RuntimeError, Value, ValueView};

fn only(node: &RakuAstNode, class: RakuAstClass, method: &str) -> Result<(), RuntimeError> {
    if node.class == class {
        Ok(())
    } else {
        Err(RuntimeError::new(format!(
            "{method} is only available on {}",
            class.printed_name()
        )))
    }
}

/// `Paragraph.from-string($text)`.
// Cost: O(1).
pub(crate) fn from_string(args: &[Value]) -> Result<Value, RuntimeError> {
    match args {
        [text] if matches!(text.view(), ValueView::Str(_)) => Ok(text.clone()),
        [other] => Err(RuntimeError::new(format!(
            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
             '$text'; expected Str but got {}",
            crate::value::type_name::value_type_name(other)
        ))),
        _ => Err(RuntimeError::new(format!(
            "Too few positionals passed; expected 2 arguments but got {}",
            args.len() + 1
        ))),
    }
}

/// `Paragraph.leading-whitespace`.
// Cost: O(w), w = the leading whitespace.
pub(crate) fn leading_whitespace(node: &RakuAstNode) -> Value {
    let first = node
        .fields
        .iter()
        .find(|f| f.name.is_none() && !origin::is_origin(f));
    let text = match first.map(|f| &f.value) {
        Some(RakuAstFieldValue::Node(v)) => match v.view() {
            ValueView::Str(s) => s.to_string(),
            _ => String::new(),
        },
        _ => String::new(),
    };
    let end = text
        .char_indices()
        .find(|(_, c)| !c.is_whitespace())
        .map_or(text.len(), |(i, _)| i);
    Value::str(text[..end].to_string())
}

/// `Paragraph.add-atom($atom)`.
// Cost: O(1) amortized.
pub(crate) fn paragraph_add_atom(
    node: &mut RakuAstNode,
    atom: &Value,
) -> Result<Value, RuntimeError> {
    only(node, RakuAstClass::DocParagraph, "add-atom")?;
    node.fields.push(RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(atom.clone()),
    });
    Ok(atom.clone())
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

/// A list argument as its items; a lone value is a list of one.
fn items_of(value: &Value) -> Vec<Value> {
    value
        .as_list_items()
        .map_or_else(|| vec![value.clone()], <[Value]>::to_vec)
}

/// `Markup.add-atom($atom)` / `Markup.add-meta($item)`.
// Cost: O(f), f = fields of the node.
pub(crate) fn markup_add(
    node: &mut RakuAstNode,
    field: &'static str,
    item: &Value,
) -> Result<Value, RuntimeError> {
    only(node, RakuAstClass::DocMarkup, "add-atom/add-meta")?;
    list_mut(node, field).push(item.clone());
    Ok(item.clone())
}

/// `Markup.set-atoms(@atoms)` / `Markup.set-meta(@meta)`.
// Cost: O(f + n), f = fields of the node, n = items given.
pub(crate) fn markup_set_list(
    node: &mut RakuAstNode,
    field: &'static str,
    value: &Value,
) -> Result<Value, RuntimeError> {
    only(node, RakuAstClass::DocMarkup, "set-atoms/set-meta")?;
    *list_mut(node, field) = items_of(value);
    Ok(Value::NIL)
}

/// `Markup.set-opener($text)` / `Markup.set-closer($text)`.
// Cost: O(f), f = fields of the node.
pub(crate) fn markup_set_delimiter(
    node: &mut RakuAstNode,
    field: &'static str,
    text: &Value,
) -> Result<Value, RuntimeError> {
    only(node, RakuAstClass::DocMarkup, "set-opener/set-closer")?;
    if !matches!(text.view(), ValueView::Str(_)) {
        return Err(RuntimeError::new(format!(
            "This type cannot unbox to a native string: P6opaque, {}",
            crate::value::type_name::value_type_name(text)
        )));
    }
    let value = RakuAstFieldValue::Node(text.clone());
    match node.fields.iter_mut().find(|f| f.name == Some(field)) {
        Some(slot) => slot.value = value,
        None => node.fields.push(RakuAstField {
            name: Some(field),
            value,
        }),
    }
    Ok(Value::NIL)
}

/// `Markup.flatten`.
// Cost: O(n), n = size of the code.
pub(crate) fn flatten(node: &RakuAstNode) -> Value {
    Value::str(super::doc_str::markup_inner(node))
}
