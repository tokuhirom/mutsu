//! `WHY` and `set-WHY` of `RakuAST::Doc::DeclaratorTarget`, the role every
//! documentable declaration does; answered by [`crate::rakuast::why_doc`],
//! this file only registers the rows.

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(f), f = fields of the node.
fn why(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::why_doc::why(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

// Cost: O(f), f = fields of the node.
fn set_why(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| crate::rakuast::why_doc::set_why(node, &args[0]))
}

// Cost: O(f + l), f = fields of the node, l = lines of the texts.
fn declarator_docs(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    target
        .rakuast_edit(|node| {
            crate::rakuast::why_doc::declarator_docs(
                node,
                named.get("leading"),
                named.get("trailing"),
            );
            Ok(Value::NIL)
        })
        .map(|r| r.map(|_| target.clone()))
}

// Cost: O(f + l), f = fields of the node, l = lines of the texts.
fn cut_why(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.rakuast_edit(|node| Ok(crate::rakuast::why_doc::cut_why(node)))
}

// Cost: O(l), l = lines held.
fn paragraphs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => Some(crate::rakuast::why_doc::paragraphs(node)),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

macro_rules! line_row {
    ($f:ident, $field:literal, $edit:ident) => {
        // Cost: O(l), l = lines held.
        fn $f(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
            target.rakuast_edit(|node| Ok(crate::rakuast::why_doc::$edit(node, $field, &args[0])))
        }
    };
}
line_row!(add_leading, "leading", add_line);
line_row!(add_trailing, "trailing", add_line);
line_row!(set_leading, "leading", set_lines);
line_row!(set_trailing, "trailing", set_lines);

const fn narrow(name: &'static str, owner: &'static str, f: super::super::NarrowFn) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity: 1,
        handler: Handler::Narrow(f),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Doc::DeclaratorTarget",
        name: "WHY",
        arity: 0,
        handler: Handler::Pure(why),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Doc::DeclaratorTarget",
        name: "declarator-docs",
        arity: 0,
        handler: Handler::Named(declarator_docs),
        flags: RowFlags::OWNER_ONLY,
        named: &["leading", "trailing"],
    },
    MethodRow {
        owner: "RakuAST::Doc::DeclaratorTarget",
        name: "cut-WHY",
        arity: 0,
        handler: Handler::Narrow(cut_why),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Doc::Declarator",
        name: "paragraphs",
        arity: 0,
        handler: Handler::Pure(paragraphs),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    narrow("add-leading", "RakuAST::Doc::Declarator", add_leading),
    narrow("add-trailing", "RakuAST::Doc::Declarator", add_trailing),
    narrow("set-leading", "RakuAST::Doc::Declarator", set_leading),
    narrow("set-trailing", "RakuAST::Doc::Declarator", set_trailing),
    MethodRow {
        owner: "RakuAST::Doc::DeclaratorTarget",
        name: "set-WHY",
        arity: 1,
        handler: Handler::Narrow(set_why),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];
