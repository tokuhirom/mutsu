//! The builders and queries of `RakuAST::Doc::Paragraph` and
//! `RakuAST::Doc::Markup`, run by [`crate::rakuast::doc_edit`]; this file only
//! registers the rows.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::rakuast::{RakuAstClass, doc_block_edit, doc_edit};
use crate::value::{RuntimeError, Value, ValueView};

// `leading-whitespace` is declared on both `Doc::Paragraph` and `Doc::Block`;
// one row answers both (a block's is its first paragraph's).
// Cost: O(w), w = the leading whitespace.
fn leading_whitespace(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) if node.class == RakuAstClass::DocBlock => {
            Some(doc_block_edit::leading_whitespace(node))
        }
        ValueView::RakuAst(node) => Some(doc_edit::leading_whitespace(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(n), n = size of the code.
fn flatten(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(doc_edit::flatten(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

/// Whether `target` is a node of `class`. The node shape lists several owners
/// whose rows share a name (`add-atom`), so a row declines a node of another
/// class and the next owner's row is tried.
fn is_class(target: &Value, class: RakuAstClass) -> bool {
    native_node(target).is_some_and(|base| match base.view() {
        ValueView::RakuAst(node) => node.class == class,
        _ => false,
    })
}

// `add-atom` is declared on both owners; the node shape reaches the first, so
// that one row answers both classes.
// Cost: O(f), f = fields of the node.
fn add_atom(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let atom = &args[0];
    if is_class(target, RakuAstClass::DocParagraph) {
        target.rakuast_edit(|node| doc_edit::paragraph_add_atom(node, atom))
    } else if is_class(target, RakuAstClass::DocMarkup) {
        target.rakuast_edit(|node| doc_edit::markup_add(node, "atoms", atom))
    } else {
        None
    }
}

/// A one-argument row that edits the node in place.
macro_rules! edit_row {
    ($owner:literal, $class:expr, $name:literal, |$node:ident, $arg:ident| $body:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 1,
            handler: Handler::Narrow(|target, args| {
                if !is_class(target, $class) {
                    return None;
                }
                let $arg = &args[0];
                target.rakuast_edit(|$node| $body)
            }),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
            named: &[],
        }
    };
}

// Cost: O(k), k = config pairs.
fn config_keys(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(doc_block_edit::config_keys(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(k), k = config pairs.
fn config_pairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(doc_block_edit::config_pairs(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Doc::Paragraph",
        name: "leading-whitespace",
        arity: 0,
        handler: Handler::Pure(leading_whitespace),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Doc::Paragraph",
        name: "add-atom",
        arity: 1,
        handler: Handler::Narrow(add_atom),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Doc::Markup",
        name: "flatten",
        arity: 0,
        handler: Handler::Pure(flatten),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    edit_row!(
        "RakuAST::Doc::Markup",
        RakuAstClass::DocMarkup,
        "add-meta",
        |node, item| doc_edit::markup_add(node, "meta", item)
    ),
    edit_row!(
        "RakuAST::Doc::Markup",
        RakuAstClass::DocMarkup,
        "set-atoms",
        |node, list| doc_edit::markup_set_list(node, "atoms", list)
    ),
    edit_row!(
        "RakuAST::Doc::Markup",
        RakuAstClass::DocMarkup,
        "set-meta",
        |node, list| doc_edit::markup_set_list(node, "meta", list)
    ),
    edit_row!(
        "RakuAST::Doc::Markup",
        RakuAstClass::DocMarkup,
        "set-opener",
        |node, text| doc_edit::markup_set_delimiter(node, "opener", text)
    ),
    edit_row!(
        "RakuAST::Doc::Markup",
        RakuAstClass::DocMarkup,
        "set-closer",
        |node, text| doc_edit::markup_set_delimiter(node, "closer", text)
    ),
    edit_row!(
        "RakuAST::Doc::Block",
        RakuAstClass::DocBlock,
        "set-type",
        |node, text| doc_block_edit::set_type(node, text)
    ),
    edit_row!(
        "RakuAST::Doc::Block",
        RakuAstClass::DocBlock,
        "set-level",
        |node, level| doc_block_edit::set_level(node, level)
    ),
    edit_row!(
        "RakuAST::Doc::Block",
        RakuAstClass::DocBlock,
        "set-margin",
        |node, text| doc_block_edit::set_margin(node, text)
    ),
    edit_row!(
        "RakuAST::Doc::Block",
        RakuAstClass::DocBlock,
        "set-paragraphs",
        |node, list| doc_block_edit::set_paragraphs(node, list)
    ),
    edit_row!(
        "RakuAST::Doc::Block",
        RakuAstClass::DocBlock,
        "add-paragraph",
        |node, paragraph| doc_block_edit::add_paragraph(node, paragraph)
    ),
    edit_row!(
        "RakuAST::Doc::Block",
        RakuAstClass::DocBlock,
        "set-config",
        |node, config| doc_block_edit::set_config(node, config)
    ),
    MethodRow {
        owner: "RakuAST::Doc::Block",
        name: "add-config",
        arity: 2,
        handler: Handler::Narrow(|target, args| {
            if !is_class(target, RakuAstClass::DocBlock) {
                return None;
            }
            target.rakuast_edit(|node| doc_block_edit::add_config(node, &args[0], &args[1]))
        }),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Doc::Block",
        name: "config-keys",
        arity: 0,
        handler: Handler::Pure(config_keys),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Doc::Block",
        name: "config-pairs",
        arity: 0,
        handler: Handler::Pure(config_pairs),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
