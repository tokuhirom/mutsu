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
        return with_wherefore(node, value);
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
        let value = RakuAstFieldValue::Node(without_cycle(node, doc));
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

/// The lines of a documentation argument: a list of strings, or one string.
// Cost: O(l), l = lines.
fn lines_arg(value: &Value) -> Vec<String> {
    let value = value.with_deref(|v| v.descalarize().clone());
    match value.as_list_items() {
        Some(items) => items.iter().map(|v| v.to_string_value()).collect(),
        None if value.is_nil() => Vec::new(),
        None => vec![value.to_string_value()],
    }
}

/// `declarator-docs(:@leading, :@trailing)`: replace the documentation of the
/// declaration by the two texts (an omitted one is empty).
// Cost: O(f + l), f = fields of the node, l = lines.
pub(crate) fn declarator_docs(
    node: &mut RakuAstNode,
    leading: Option<&Value>,
    trailing: Option<&Value>,
) {
    let get = |v: Option<&Value>| v.map(lines_arg).unwrap_or_default();
    super::declarator_docs::replace(node, get(leading), get(trailing));
}

/// `cut-WHY`: the documentation the declaration holds (the `Doc::Declarator`
/// type object when it has none), which it no longer holds afterwards.
// Cost: O(f + l), f = fields of the node, l = lines.
pub(crate) fn cut_why(node: &mut RakuAstNode) -> Value {
    let doc = why(node);
    super::declarator_docs::clear(node);
    doc
}

fn doc_lines(node: &RakuAstNode, name: &str) -> Vec<Value> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .map_or_else(Vec::new, |f| match &f.value {
            RakuAstFieldValue::List(items) => items.clone(),
            RakuAstFieldValue::Node(item) => vec![item.clone()],
            RakuAstFieldValue::Adverb(_) => Vec::new(),
        })
}

fn put_lines(node: &mut RakuAstNode, name: &'static str, lines: Vec<Value>) {
    node.fields.retain(|f| f.name != Some(name));
    if !lines.is_empty() {
        node.fields.push(RakuAstField {
            name: Some(name),
            value: RakuAstFieldValue::List(lines),
        });
    }
}

/// `Doc::Declarator.add-leading($line)` / `add-trailing`.
// Cost: O(l), l = lines held.
pub(crate) fn add_line(node: &mut RakuAstNode, name: &'static str, line: &Value) -> Value {
    let mut lines = doc_lines(node, name);
    lines.push(Value::str(line.to_string_value()));
    put_lines(node, name, lines);
    Value::NIL
}

/// `Doc::Declarator.set-leading(@lines)` / `set-trailing`.
// Cost: O(l), l = lines given.
pub(crate) fn set_lines(node: &mut RakuAstNode, name: &'static str, lines: &Value) -> Value {
    let lines = lines_arg(lines).into_iter().map(Value::str).collect();
    put_lines(node, name, lines);
    Value::NIL
}

/// `Doc::Declarator.paragraphs`: the leading and trailing texts, each joined
/// by newlines.
// Cost: O(l), l = lines held.
pub(crate) fn paragraphs(node: &RakuAstNode) -> Value {
    let join = |name| {
        doc_lines(node, name)
            .iter()
            .map(Value::to_string_value)
            .collect::<Vec<_>>()
            .join("\n")
    };
    Value::array(vec![
        Value::str(join("leading")),
        Value::str(join("trailing")),
    ])
}

/// The `WHEREFORE` field of a `Doc::Declarator` node.
fn wherefore_of(doc: &RakuAstNode) -> Option<&Value> {
    doc.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some("WHEREFORE"), RakuAstFieldValue::Node(v)) => Some(v),
        _ => None,
    })
}

/// `doc` without its `WHEREFORE` when that is `node` itself. The node is
/// edited in place and shared by every holder, so storing a doc that holds the
/// same node would make the tree contain itself; [`why`] gives the doc its
/// `WHEREFORE` back when it is read.
// Cost: O(f), f = fields of the doc.
fn without_cycle(node: &RakuAstNode, doc: &Value) -> Value {
    let ValueView::RakuAst(held) = doc.view() else {
        return doc.clone();
    };
    let points_here = wherefore_of(held).is_some_and(|w| match w.view() {
        ValueView::RakuAst(target) => std::ptr::eq(target, node),
        _ => false,
    });
    if !points_here {
        return doc.clone();
    }
    let mut stripped = held.clone();
    stripped.fields.retain(|f| f.name != Some("WHEREFORE"));
    Value::rakuast(Box::new(stripped))
}

/// `doc` with `WHEREFORE` set to (a copy of) `node` when it has none.
// Cost: O(f), f = fields of the node.
fn with_wherefore(node: &RakuAstNode, doc: Value) -> Value {
    let ValueView::RakuAst(held) = doc.view() else {
        return doc;
    };
    if held.class != RakuAstClass::DocDeclarator || wherefore_of(held).is_some() {
        return doc;
    }
    let mut full = held.clone();
    full.fields.insert(
        0,
        RakuAstField {
            name: Some("WHEREFORE"),
            value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node.clone()))),
        },
    );
    Value::rakuast(Box::new(full))
}
