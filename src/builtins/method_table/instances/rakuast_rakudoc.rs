//! `RakuAST::Node.rakudoc` (rakudo's `src/core.c/RakuAST/Fixups.rakumod`): the
//! documentation nodes of a tree, a `Seq` in source order. Measured on rakudo
//! 2026.09: the descendants are walked pre-order (the node itself is not
//! visited); a `RakuAST::Doc::Block` is taken as it is, and a declaration that
//! carries `#|` / `#=` docs is taken as a `RakuAST::Doc::Declarator`
//! (`WHEREFORE` the declaration, `leading` / `trailing` the doc texts).

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_node::children;
use super::rakuast_visit::native_node;
use crate::rakuast::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Node",
    name: "rakudoc",
    arity: 0,
    handler: Handler::Interp(rakudoc),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];

/// The deepest nesting followed.
const MAX_DEPTH: usize = 4096;

// Cost: O(n), n = nodes of the tree.
fn rakudoc(
    _interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let base = native_node(target)?;
    let mut out = Vec::new();
    collect(&base, 0, &mut out);
    Some(Ok(Value::seq(out)))
}

// Cost: O(size of the subtree).
fn collect(node: &Value, depth: usize, out: &mut Vec<Value>) {
    let ValueView::RakuAst(native) = node.view() else {
        return;
    };
    if depth > MAX_DEPTH {
        return;
    }
    for child in children(native) {
        let ValueView::RakuAst(held) = child.view() else {
            continue;
        };
        if held.class == RakuAstClass::DocBlock {
            out.push(child.clone());
            continue;
        }
        if let Some((leading, trailing)) = crate::rakuast::declarator_docs_of(held) {
            out.push(declarator(&child, leading, trailing));
        }
        collect(&child, depth + 1, out);
    }
}

/// The `Doc::Declarator` for the documented declaration `whose`.
// Cost: O(1).
fn declarator(whose: &Value, leading: Option<String>, trailing: Option<String>) -> Value {
    let mut fields = vec![RakuAstField {
        name: Some("WHEREFORE"),
        value: RakuAstFieldValue::Node(whose.clone()),
    }];
    for (name, text) in [("leading", leading), ("trailing", trailing)] {
        if let Some(text) = text {
            fields.push(RakuAstField {
                name: Some(name),
                value: RakuAstFieldValue::List(vec![Value::str(text)]),
            });
        }
    }
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::DocDeclarator,
        fields,
    }))
}
