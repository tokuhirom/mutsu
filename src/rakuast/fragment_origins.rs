//! Origins for a node tree converted from a fragment of the unit's text that
//! was parsed on its own (ADR-10723 Stage 2).
//!
//! An interpolated `"@r[0]"` keeps its subscript as an expression the string
//! parser read from the text between the brackets, without positions. The
//! text is parsed again here, spelled, its ranges are moved to where the text
//! sits in the unit, and the result is converted with them in reach; the
//! ranges of the resulting nodes are then copied onto the nodes the unit's
//! own conversion made, which have the same shape. A node whose shape differs
//! stops the copy: exact or absent.

use super::{RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// Give the statements held by `list` (the `SemiList` of a subscript), which
/// were read from `text` starting at byte `at` of the unit, their ranges.
// Cost: O(n), n = length of `text`, plus the parse of it.
pub(super) fn statements(names: &NameScope, list: &mut RakuAstNode, text: &str, at: usize) {
    let Ok((mut stmts, _)) = crate::parser::parse_fragment_spelled(text) else {
        return;
    };
    crate::ast_visit::walk_stmts_mut(&mut super::regex_call_origins::Shift(at), &mut stmts);
    let _origins = super::expr_origins::strip(&mut stmts);
    let _statement =
        super::expr_origins::InStatement::enter(Some(SrcSpan::new(at, at + text.len())));
    let Ok(source) = super::view::statement_list(names, &stmts) else {
        return;
    };
    copy_fields(list, &source);
}

/// Copy the origins of `source` onto `target`, a node of the same shape.
// Cost: O(n), n = nodes of the smaller tree.
fn copy(target: &mut RakuAstNode, source: &RakuAstNode) {
    if target.class != source.class {
        return;
    }
    // A statement's range is relative to the fragment (the statement list
    // derives it from the terms inside instead).
    if target.origin.0.is_none() && target.class != super::RakuAstClass::StatementExpression {
        target.origin.0 = source.origin.0.clone();
    }
    copy_fields(target, source);
}

/// [`copy`] over the fields of two nodes, which must hold as many.
// Cost: O(n), n = nodes of the smaller tree.
fn copy_fields(target: &mut RakuAstNode, source: &RakuAstNode) {
    if target.fields.len() != source.fields.len() {
        return;
    }
    for (t, s) in target.fields.iter_mut().zip(&source.fields) {
        match (&mut t.value, &s.value) {
            (RakuAstFieldValue::Node(t), RakuAstFieldValue::Node(s)) => copy_value(t, s),
            (RakuAstFieldValue::List(t), RakuAstFieldValue::List(s)) if t.len() == s.len() => {
                for (t, s) in t.iter_mut().zip(s) {
                    copy_value(t, s);
                }
            }
            _ => {}
        }
    }
}

// Cost: O(n), n = nodes of the smaller tree.
fn copy_value(target: &mut Value, source: &Value) {
    let (ValueView::RakuAst(t), ValueView::RakuAst(s)) = (target.view(), source.view()) else {
        return;
    };
    let mut t = RakuAstNode::clone(t);
    copy(&mut t, s);
    *target = Value::rakuast(Box::new(t));
}
