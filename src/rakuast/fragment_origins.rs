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
    let Ok((mut stmts, _)) = crate::parser::parse_fragment_unfinalized(text) else {
        return;
    };
    crate::ast_visit::walk_stmts_mut(&mut super::regex_call_origins::Shift(at), &mut stmts);
    let _origins = super::expr_origins::strip(&mut stmts);
    let _statement =
        super::expr_origins::InStatement::enter(Some(SrcSpan::new(at, at + text.len())));
    let Ok(source) = super::view::statement_list(names, &stmts) else {
        return;
    };
    copy_fields(list, &source, true);
}

/// Give `target`, the expression node converted from `text` (the single
/// expression statement of a fragment starting at byte `at` of the unit),
/// its ranges.
// Cost: O(n), n = length of `text`, plus the parse of it.
pub(super) fn expression(names: &NameScope, target: &mut RakuAstNode, text: &str, at: usize) {
    let Ok((mut stmts, _)) = crate::parser::parse_fragment_unfinalized(text) else {
        return;
    };
    crate::ast_visit::walk_stmts_mut(&mut super::regex_call_origins::Shift(at), &mut stmts);
    let _origins = super::expr_origins::strip(&mut stmts);
    let _statement =
        super::expr_origins::InStatement::enter(Some(SrcSpan::new(at, at + text.len())));
    let Ok(source) = super::view::statement_list(names, &stmts) else {
        return;
    };
    let Some(statement) = super::infix_origins::child_at(&source, 0) else {
        return;
    };
    let Some(expression) = super::infix_origins::child_at(statement, 0) else {
        return;
    };
    copy(target, expression, false);
}

/// Give each argument of `call`'s `args`, written at `inside` (between a
/// call's parentheses, or after its name) but left without a range by the
/// parser (`say(* + *)`, `say 1, * + *`), its ranges: see [`comma_items`].
// Cost: O(n), n = length of `inside`, plus the parse of each unplaced
// argument's text.
pub(super) fn arguments(names: &NameScope, call: &mut RakuAstNode, inside: SrcSpan) {
    super::infix_origins::with_child(call, "args", |list| {
        let items: Vec<&mut Value> = list
            .fields
            .iter_mut()
            .filter_map(|f| match &mut f.value {
                RakuAstFieldValue::Node(v) => Some(v),
                _ => None,
            })
            .collect();
        comma_items(names, items, inside.from as usize, inside.to as usize);
    });
}

/// Give each operand of `node`, a comma list with a range (`* + *, 2`), that
/// the parser left without a range its ranges: see [`comma_items`].
// Cost: O(n), n = length of the list's source, plus the parse of each
// unplaced operand's text.
pub(super) fn list_operands(names: &NameScope, node: &mut RakuAstNode) {
    use super::infix_origins::{byte_range, child, leaf_text};
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    if child(node, "infix").and_then(leaf_text).as_deref() != Some(",") {
        return;
    }
    let Some(items) = node.fields.iter_mut().find_map(|f| match &mut f.value {
        RakuAstFieldValue::List(items) if f.name == Some("operands") => Some(items),
        _ => None,
    }) else {
        return;
    };
    comma_items(names, items.iter_mut().collect(), from, to);
}

/// Place each of `items`, comma-separated nodes written from byte `from` to
/// `to`, that has no range: the text between its neighbours (or the ends),
/// less the commas, is parsed on its own and its ranges copied over.
// Cost: O(n), n = `to - from`, plus the parse of each unplaced item's text.
fn comma_items(names: &NameScope, mut items: Vec<&mut Value>, from: usize, to: usize) {
    use super::infix_origins::byte_range;
    let range = |v: &Value| match v.view() {
        ValueView::RakuAst(item) => byte_range(item),
        _ => None,
    };
    let last = items.len().saturating_sub(1);
    for i in 0..items.len() {
        if range(items[i]).is_some() {
            continue;
        }
        let lo = if i == 0 {
            Some(from)
        } else {
            range(items[i - 1]).map(|(_, end)| end)
        };
        let hi = if i == last {
            Some(to)
        } else {
            range(items[i + 1]).map(|(start, _)| start)
        };
        let (Some(lo), Some(hi)) = (lo, hi) else {
            continue;
        };
        if lo >= hi {
            continue;
        }
        let Some(text) =
            super::unit_source::with_text(SrcSpan::new(lo, hi), |t| Some(t.to_string()))
        else {
            continue;
        };
        let mut written = text.as_str();
        if i > 0 {
            let Some(rest) = written.trim_start().strip_prefix(',') else {
                continue;
            };
            written = rest;
        }
        let trimmed = written.trim_end();
        written = if i < last {
            let Some(rest) = trimmed.strip_suffix(',') else {
                continue;
            };
            rest
        } else {
            trimmed.strip_suffix(',').unwrap_or(trimmed)
        };
        let written = written.trim();
        if written.is_empty() {
            continue;
        }
        // `written` is a slice of `text`: its offset there.
        let at = lo + (written.as_ptr() as usize - text.as_ptr() as usize);
        let ValueView::RakuAst(item) = items[i].view() else {
            continue;
        };
        let mut item = RakuAstNode::clone(item);
        expression(names, &mut item, written, at);
        *items[i] = Value::rakuast(Box::new(item));
    }
}

/// Copy the origins of `source` onto `target`, a node of the same shape.
// Cost: O(n), n = nodes of the smaller tree.
fn copy(target: &mut RakuAstNode, source: &RakuAstNode, top: bool) {
    if target.class != source.class {
        return;
    }
    // A top-level statement's range is relative to the fragment (the
    // statement list derives it from the terms inside instead).
    if target.origin.0.is_none()
        && !(top && target.class == super::RakuAstClass::StatementExpression)
    {
        target.origin.0 = source.origin.0.clone();
    }
    // `$hash.kv`: the empty `ArgList` a call visits keeps its range there.
    if target.origin.1.is_empty() {
        target.origin.1 = source.origin.1.clone();
    }
    copy_fields(target, source, false);
}

/// [`copy`] over the fields of two nodes, which must hold as many.
// Cost: O(n), n = nodes of the smaller tree.
fn copy_fields(target: &mut RakuAstNode, source: &RakuAstNode, top: bool) {
    if target.fields.len() != source.fields.len() {
        return;
    }
    for (t, s) in target.fields.iter_mut().zip(&source.fields) {
        match (&mut t.value, &s.value) {
            (RakuAstFieldValue::Node(t), RakuAstFieldValue::Node(s)) => copy_value(t, s, top),
            (RakuAstFieldValue::List(t), RakuAstFieldValue::List(s)) if t.len() == s.len() => {
                for (t, s) in t.iter_mut().zip(s) {
                    copy_value(t, s, top);
                }
            }
            _ => {}
        }
    }
}

// Cost: O(n), n = nodes of the smaller tree.
fn copy_value(target: &mut Value, source: &Value, top: bool) {
    let (ValueView::RakuAst(t), ValueView::RakuAst(s)) = (target.view(), source.view()) else {
        return;
    };
    let mut t = RakuAstNode::clone(t);
    copy(&mut t, s, top);
    *target = Value::rakuast(Box::new(t));
}
