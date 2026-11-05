//! The declarator documentation of a RakuAST declaration (`WHY`), as rakudo
//! 2026.09 keeps it (`RakuAST::Doc::DeclaratorTarget`, which `Sub`, `Method`,
//! `Class`, `Block`, `Parameter`, `Type::Subset`, ... do).
//!
//! `X.new(WHY => $doc)` and `X.set-WHY($doc)` store a `Doc::Declarator`;
//! `WHY` answers it, or the `Doc::Declarator` type object when there is none.
//! Rakudo keeps it in a private attribute that `.raku` leaves out, and so does
//! mutsu: the value lives in the hidden field [`FIELD`], which no walk, render
//! or lowering reads.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// The hidden field holding the documentation node.
const FIELD: &str = "WHY";

/// Whether `field` is the hidden documentation of `node`.
// Cost: O(1).
pub(super) fn is_hidden(_node: &RakuAstNode, field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
}

fn doc_type() -> Value {
    Value::package(crate::symbol::Symbol::intern("RakuAST::Doc::Declarator"))
}

/// `WHY`: the stored documentation; else, for a parsed declaration, the
/// `Doc::Declarator` built from its `#|` / `#=` comments; else the
/// `Doc::Declarator` type object.
// Cost: O(f + l), f = fields of the node, l = lines of its comments.
pub(crate) fn why(node: &RakuAstNode) -> Value {
    if let Some(value) = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some(FIELD), RakuAstFieldValue::Node(value)) => Some(value.clone()),
        _ => None,
    }) {
        return value;
    }
    let leading = super::declarator_docs::lines(node, "declarator-docs-leading");
    let trailing = super::declarator_docs::lines(node, "declarator-docs-trailing");
    if leading.is_empty() && trailing.is_empty() {
        return doc_type();
    }
    let list =
        |lines: Vec<String>| RakuAstFieldValue::List(lines.into_iter().map(Value::str).collect());
    // rakudo's `WHEREFORE` is the declaration itself; a node is a value here,
    // so the doc holds a copy of it (#16783).
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::DocDeclarator,
        fields: vec![
            RakuAstField {
                name: Some("WHEREFORE"),
                value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node.clone()))),
            },
            RakuAstField {
                name: Some("leading"),
                value: list(leading),
            },
            RakuAstField {
                name: Some("trailing"),
                value: list(trailing),
            },
        ],
    }))
}

/// Check that `value` is a `Doc::Declarator` (a node or its type object), as
/// the `RakuAST::Doc::Declarator $WHY` parameter does.
// Cost: O(1).
fn check(value: &Value) -> Result<(), RuntimeError> {
    let ok = match value.view() {
        ValueView::RakuAst(node) => node.class == RakuAstClass::DocDeclarator,
        ValueView::Package(name) => name.as_str() == "RakuAST::Doc::Declarator",
        _ => false,
    };
    if ok {
        return Ok(());
    }
    Err(RuntimeError::new(format!(
        "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '$WHY'; \
         expected RakuAST::Doc::Declarator but got {}",
        crate::value::type_name::value_type_name(value)
    )))
}

/// `set-WHY($doc)`: keep `$doc` when it is defined (a type object leaves the
/// stored documentation alone, as rakudo does) and answer `Nil`.
// Cost: O(f), f = fields of the node.
pub(crate) fn set_why(node: &mut RakuAstNode, doc: &Value) -> Result<Value, RuntimeError> {
    check(doc)?;
    if matches!(doc.view(), ValueView::RakuAst(_)) {
        let value = RakuAstFieldValue::Node(doc.clone());
        match node.fields.iter_mut().find(|f| f.name == Some(FIELD)) {
            Some(slot) => slot.value = value,
            None => node.fields.push(RakuAstField {
                name: Some(FIELD),
                value,
            }),
        }
    }
    Ok(Value::NIL)
}

/// Split the `WHY` argument off a `new` call of a class that declares it, so
/// the class's own constructor never sees it.
// Cost: O(a), a = arguments.
pub(super) fn split_arg(
    class_name: &str,
    args: &[Value],
) -> Result<(Vec<Value>, Option<Value>), RuntimeError> {
    if !super::ctor_names::declares(class_name, FIELD) {
        return Ok((args.to_vec(), None));
    }
    let mut rest = Vec::with_capacity(args.len());
    let mut doc = None;
    for arg in args {
        let named_why = match arg.view() {
            ValueView::Pair(key, value) if key == FIELD => Some(value.clone()),
            ValueView::ValuePair(key, value) if key.to_string_value() == FIELD => {
                Some(value.clone())
            }
            _ => None,
        };
        match named_why {
            Some(value) => {
                check(&value)?;
                doc = Some(value);
            }
            None => rest.push(arg.clone()),
        }
    }
    Ok((rest, doc))
}

/// Give the freshly built `node` the documentation `doc`.
// Cost: O(f), f = fields of the node.
pub(super) fn attach(built: Value, doc: Value) -> Value {
    match built.view() {
        ValueView::RakuAst(node) if matches!(doc.view(), ValueView::RakuAst(_)) => {
            let mut node = node.clone();
            let _ = set_why(&mut node, &doc);
            Value::rakuast(Box::new(node))
        }
        _ => built,
    }
}
