//! Origins of lists and the statements inside brackets (ADR-10723 Stage 2,
//! expression families): an `ApplyListInfix` (`1, 2`, `$a | $b`), the
//! `SemiList` inside `( )` / `[ ]` and inside a `[ ]` / `{ }` subscript, and
//! the `Statement::Expression`s it holds.
//!
//! Like `infix_origins`, every range is derived from origins the children
//! already carry and checked against the source text it claims: the
//! operators between a list's operands, the brackets around a `SemiList`.

use super::infix_origins::{byte_range, child, child_at, leaf_text, with_child, with_child_at};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// `a OP b OP c`: the application spans its operands (and a trailing `,`),
/// and its `Infix` covers the last operator, as Rakudo's does.
// Cost: O(n + k), n = length of the list's source, k = operands.
pub(super) fn apply_list_infix(node: &mut RakuAstNode) {
    let Some(op) = child(node, "infix")
        .filter(|infix| infix.class == RakuAstClass::Infix)
        .and_then(leaf_text)
    else {
        return;
    };
    let Some(operands) = node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::List(items) if f.name == Some("operands") => Some(items),
        _ => None,
    }) else {
        return;
    };
    let ranges: Option<Vec<(usize, usize)>> = operands
        .iter()
        .map(|v| match v.view() {
            ValueView::RakuAst(operand) => byte_range(operand),
            _ => None,
        })
        .collect();
    let Some(ranges) = ranges.filter(|r| r.len() >= 2) else {
        return;
    };
    // Each gap holds exactly the operator, between whitespace.
    let mut last_op = None;
    for pair in ranges.windows(2) {
        let ((_, end), (start, _)) = (pair[0], pair[1]);
        if end > start {
            return;
        }
        let at = super::unit_source::with_text(SrcSpan::new(end, start), |gap| {
            let after = gap.trim_start();
            (after.trim_end() == op).then_some(end + (gap.len() - after.len()))
        });
        let Some(at) = at else {
            return;
        };
        last_op = Some(at);
    }
    let (from, mut to) = (ranges[0].0, ranges[ranges.len() - 1].1);
    // A trailing `,` belongs to the list: no tighter operator ends at a `,`.
    if op == ","
        && let Some(comma) = trailing_comma(to)
    {
        last_op = Some(comma);
        to = comma + 1;
    }
    let Some(op_at) = last_op else {
        return;
    };
    super::origin::set_span(node, SrcSpan::new(from, to));
    with_child(node, "infix", |infix| {
        super::origin::set_span(infix, SrcSpan::new(op_at, op_at + op.len()))
    });
}

/// The byte offset of a `,` that follows `at` after whitespace.
// Cost: O(w), w = whitespace skipped.
fn trailing_comma(at: usize) -> Option<usize> {
    let mut end = at;
    loop {
        let next =
            super::unit_source::with_text(SrcSpan::new(end, end + 1), |c| Some(c.to_string()))?;
        match next.as_str() {
            "," => return Some(end),
            " " | "\t" | "\n" | "\r" => end += 1,
            _ => return None,
        }
    }
}

/// `( ... )` / `[ ... ]`: the `SemiList` is the text inside the brackets.
// Cost: O(n + s), n = length of the circumfix's source, s = its statements.
pub(super) fn circumfix(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let brackets = if node.class == RakuAstClass::CircumfixParentheses {
        ('(', ')')
    } else {
        ('[', ']')
    };
    if let Some(inside) = inside(from, to, brackets) {
        with_child_at(node, 0, |list| semilist(list, inside));
    }
}

/// `operand[...]` / `operand{...}`: the subscript runs from the operand's
/// end to the application's, and its `SemiList` is the text inside it.
// Cost: O(n + s), n = length of the subscript's source, s = its statements.
pub(super) fn postcircumfix(node: &mut RakuAstNode) {
    let Some((_, to)) = byte_range(node) else {
        return;
    };
    let Some((_, from)) = child(node, "operand").and_then(byte_range) else {
        return;
    };
    let Some(subscript) = child(node, "postfix") else {
        return;
    };
    let brackets = match subscript.class {
        RakuAstClass::PostcircumfixArrayIndex => ('[', ']'),
        RakuAstClass::PostcircumfixHashIndex => ('{', '}'),
        RakuAstClass::PostcircumfixLiteralHashIndex => return literal_hash_index(node, from, to),
        _ => return,
    };
    if subscript.origin.0.is_some() {
        return;
    }
    let Some(inside) = inside(from, to, brackets) else {
        return;
    };
    with_child(node, "postfix", |subscript| {
        super::origin::set_span(subscript, SrcSpan::new(from, to));
        with_child(subscript, "index", |list| semilist(list, inside));
    });
}

/// `%h<a b>` / `%h<<a>>` / `%h«a»`: the subscript runs from the operand's end
/// to the application's (its adverbs included), and its word quote is the text
/// between the brackets, kept raw.
// Cost: O(n), n = length of the subscript's source.
fn literal_hash_index(node: &mut RakuAstNode, from: usize, to: usize) {
    let Some(subscript) = child(node, "postfix") else {
        return;
    };
    if subscript.origin.0.is_some() {
        return;
    }
    let inner = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let (open, close) = super::origin::word_brackets_open(text)?;
        let end = open + text.get(open..)?.find(close)?;
        // What follows the closing bracket is the subscript's adverbs.
        let rest = text.get(end + close.len()..)?;
        (rest.is_empty() || rest.starts_with(':')).then_some((from + open, from + end, rest.len()))
    });
    let Some((inner_from, inner_to, adverbs)) = inner else {
        return;
    };
    with_child(node, "postfix", |subscript| {
        super::origin::set_span(subscript, SrcSpan::new(from, to));
        // A single `:name` adverb is the text after the closing bracket.
        if adverbs > 0 {
            super::origin::adverb_true(subscript, SrcSpan::new(to - adverbs, to));
        }
        with_child(subscript, "index", |quote| {
            super::origin::word_quote_inner(quote, SrcSpan::new(inner_from, inner_to))
        });
    });
}

/// The range inside `open ... close` written at `from..to`, without the
/// whitespace at either end (empty right after `open` when blank).
// Cost: O(n), n = to - from.
pub(super) fn inside(
    from: usize,
    to: usize,
    (open, close): (char, char),
) -> Option<(usize, usize)> {
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let body = text.strip_prefix(open)?.strip_suffix(close)?;
        let trimmed = body.trim_start();
        let start = from + 1 + (body.len() - trimmed.len());
        Some((start, start + trimmed.trim_end().len()))
    })
}

/// Give a `SemiList` the range `at`, and each `Statement::Expression` in it
/// the range of its expression.
// Cost: O(s), s = statements.
pub(super) fn semilist(list: &mut RakuAstNode, at: (usize, usize)) {
    if !matches!(
        list.class,
        RakuAstClass::SemiList | RakuAstClass::StatementSequence | RakuAstClass::StatementList
    ) {
        return;
    }
    super::origin::set_span(list, SrcSpan::new(at.0, at.1));
    for field in list.fields.iter_mut() {
        let RakuAstFieldValue::Node(held) = &mut field.value else {
            continue;
        };
        let ValueView::RakuAst(statement) = held.view() else {
            continue;
        };
        if statement.class != RakuAstClass::StatementExpression || statement.fields.len() != 1 {
            continue;
        }
        let Some(range) = child_at(statement, 0).and_then(byte_range) else {
            continue;
        };
        let mut statement = RakuAstNode::clone(statement);
        super::origin::set_span(&mut statement, SrcSpan::new(range.0, range.1));
        *held = Value::rakuast(Box::new(statement));
    }
}
