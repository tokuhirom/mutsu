//! Origins of prefix, postfix and ternary operators, contextualizers and
//! pairs (ADR-10723 Stage 2, expression families).
//!
//! Like `infix_origins`, each range is derived from origins the node or its
//! children already carry and checked against the source text it claims:
//! the operator, the `??` / `!!`, the sigil and parentheses.

use super::infix_origins::{byte_range, child, child_at, leaf_text, with_child, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::ValueView;

/// Place the operator / parts of `node` (an `ApplyPrefix`, `ApplyPostfix`,
/// `Ternary`, `Contextualizer::*` or `ColonPair::Value`).
// Cost: O(f + n), f = fields of `node`, n = length of the source text read.
pub(super) fn derive(node: &mut RakuAstNode) {
    match node.class {
        RakuAstClass::ApplyPrefix => prefix(node),
        RakuAstClass::ApplyPostfix => postfix(node),
        RakuAstClass::Ternary => ternary(node),
        RakuAstClass::ContextualizerItem
        | RakuAstClass::ContextualizerList
        | RakuAstClass::ContextualizerHash => contextualizer(node),
        RakuAstClass::ColonPairValue => colonpair_value(node),
        RakuAstClass::FatArrow => fat_arrow(node),
        _ => {}
    }
}

/// `-$x`: the `Prefix` is the operator the application starts with.
// Cost: O(o), o = length of the operator.
fn prefix(node: &mut RakuAstNode) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(op) = child(node, "prefix")
        .filter(|p| p.class == RakuAstClass::Prefix)
        .and_then(leaf_text)
    else {
        return;
    };
    let written = super::unit_source::with_text(SrcSpan::new(from, from + op.len()), |text| {
        (text == op).then_some(())
    });
    if written.is_some() {
        with_child(node, "prefix", |p| {
            super::origin::set_span(p, SrcSpan::new(from, from + op.len()))
        });
    }
}

/// `$x++`: the `Postfix` is the operator written after the operand.
// Cost: O(n), n = length of the application's source.
fn postfix(node: &mut RakuAstNode) {
    let Some((_, to)) = byte_range(node) else {
        return;
    };
    let Some((_, operand_end)) = child(node, "operand").and_then(byte_range) else {
        return;
    };
    let Some(op) = child(node, "postfix")
        .filter(|p| p.class == RakuAstClass::Postfix)
        .and_then(leaf_text)
    else {
        return;
    };
    if operand_end > to {
        return;
    }
    let at = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
        let after = text.trim_start();
        (after == op).then_some(operand_end + (text.len() - after.len()))
    });
    if let Some(at) = at {
        with_child(node, "postfix", |p| {
            super::origin::set_span(p, SrcSpan::new(at, at + op.len()))
        });
    }
}

/// `c ?? a !! b`: the ternary spans its three operands when `??` and `!!`
/// are what separates them.
// Cost: O(n), n = length of the ternary's source.
fn ternary(node: &mut RakuAstNode) {
    if node.origin.0.is_some() {
        return;
    }
    let parts = ["condition", "then", "else"].map(|name| child(node, name).and_then(byte_range));
    let [Some(condition), Some(then), Some(otherwise)] = parts else {
        return;
    };
    let separated = |(from, to): (usize, usize), op: &str| {
        from <= to
            && super::unit_source::with_text(SrcSpan::new(from, to), |gap| {
                (gap.trim() == op).then_some(())
            })
            .is_some()
    };
    if separated((condition.1, then.0), "??") && separated((then.1, otherwise.0), "!!") {
        super::origin::set_span(node, SrcSpan::new(condition.0, otherwise.1));
    }
}

/// `$( ... )` / `@( ... )` / `%( ... )`: the `StatementSequence` is the text
/// inside the parentheses after the sigil, and a nested contextualizer
/// (`$@( ... )`) starts after the outer sigil.
// Cost: O(n + d), n = length of the source, d = nesting of contextualizers.
fn contextualizer(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let sigil = match node.class {
        RakuAstClass::ContextualizerItem => '$',
        RakuAstClass::ContextualizerList => '@',
        _ => '%',
    };
    let starts = super::unit_source::with_text(SrcSpan::new(from, from + 1), |text| {
        text.starts_with(sigil).then_some(())
    });
    if starts.is_none() {
        return;
    }
    let Some(inner) = child_at(node, 0) else {
        return;
    };
    if matches!(
        inner.class,
        RakuAstClass::ContextualizerItem
            | RakuAstClass::ContextualizerList
            | RakuAstClass::ContextualizerHash
    ) {
        with_child_at(node, 0, |inner| {
            super::origin::set_span(inner, SrcSpan::new(from + 1, to));
            contextualizer(inner);
        });
        return;
    }
    if let Some(inside) = super::list_origins::bracketed(from + 1, to, ('(', ')')) {
        with_child_at(node, 0, |list| {
            super::list_origins::statement_list(list, inside)
        });
    }
}

/// `:key(...)`: the parenthesized value runs from the `(` after the key to
/// the end of the pair.
// Cost: O(n + s), n = length of the pair's source, s = statements in it.
fn colonpair_value(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(key) = node.fields.iter().find_map(|f| match &f.value {
        super::RakuAstFieldValue::Node(v) if f.name == Some("key") => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    }) else {
        return;
    };
    let Some(value) = child(node, "value") else {
        return;
    };
    if value.class != RakuAstClass::CircumfixParentheses || value.origin.0.is_some() {
        return;
    }
    let open = from + 1 + key.len();
    let written = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text.strip_prefix(':')?.strip_prefix(key.as_str())?;
        (rest.starts_with('(') && rest.ends_with(')')).then_some(())
    });
    if written.is_some() && open < to {
        with_child(node, "value", |value| {
            super::origin::set_span(value, SrcSpan::new(open, to));
            super::list_origins::circumfix(value);
        });
    }
}

/// `key => value`: the pair runs from its key to the end of its value, when
/// the key and `=>` are written right before the value.
// Cost: O(n), n = length of the source before the value.
fn fat_arrow(node: &mut RakuAstNode) {
    if node.origin.0.is_some() {
        return;
    }
    let Some(key) = node.fields.iter().find_map(|f| match &f.value {
        super::RakuAstFieldValue::Node(v) if f.name == Some("key") => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    }) else {
        return;
    };
    let Some((value_from, value_to)) = child(node, "value").and_then(byte_range) else {
        return;
    };
    // The key, `=>` and the whitespace around it, at most a line's worth.
    let window = value_from.saturating_sub(key.len() + 256);
    let from = super::unit_source::with_text(SrcSpan::new(window, value_from), |before| {
        let head = before.trim_end().strip_suffix("=>")?.trim_end();
        let rest = head.strip_suffix(key.as_str())?;
        let boundary = !rest.ends_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-'));
        boundary.then_some(window + rest.len())
    });
    if let Some(from) = from {
        super::origin::set_span(node, SrcSpan::new(from, value_to));
    }
}
