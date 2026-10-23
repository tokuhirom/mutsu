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
    // Each gap holds exactly the operator, between whitespace and comments.
    let mut last_op = None;
    for pair in ranges.windows(2) {
        let ((_, end), (start, _)) = (pair[0], pair[1]);
        if end > start {
            return;
        }
        let at = super::unit_source::with_text(SrcSpan::new(end, start), |gap| {
            use crate::source_comments::trivia_len;
            let lead = trivia_len(gap)?;
            let after = gap[lead..].strip_prefix(op.as_str())?;
            (trivia_len(after)? == after.len()).then_some(end + lead)
        });
        let Some(at) = at else {
            return;
        };
        last_op = Some(at);
    }
    let (from, mut to) = (ranges[0].0, ranges[ranges.len() - 1].1);
    // A trailing `,` belongs to the list: no tighter operator ends at a `,`.
    if op == "," {
        let Some(comma) = trailing_comma(to) else {
            return;
        };
        if let Some(comma) = comma {
            last_op = Some(comma);
            to = comma + 1;
        }
    }
    let Some(op_at) = last_op else {
        return;
    };
    super::origin::set_span(node, SrcSpan::new(from, to));
    with_child(node, "infix", |infix| {
        super::origin::set_span(infix, SrcSpan::new(op_at, op_at + op.len()))
    });
}

/// The byte offset of a `,` that follows `at` after whitespace and
/// comments: `Some(None)` when something else follows, `None` when the text
/// after `at` cannot be read ([`crate::source_comments::trivia_len`]).
// Cost: O(w), w = whitespace and comments skipped.
fn trailing_comma(at: usize) -> Option<Option<usize>> {
    super::unit_source::with_text_from(at, |text| {
        let gap = crate::source_comments::trivia_len(text)?;
        Some(text[gap..].starts_with(',').then_some(at + gap))
    })
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
    if let Some(inside) = bracketed(from, to, brackets) {
        with_child_at(node, 0, |list| statement_list(list, inside));
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
    // The subscript's adverbs (`@a[0] :v`) are part of its range.
    let close = brackets.1.to_string();
    let Some((close_end, adverbs)) = super::adverb_origins::split(from, to, &close) else {
        return;
    };
    let Some(inside) = bracketed(from, close_end, brackets) else {
        return;
    };
    with_child(node, "postfix", |subscript| {
        super::origin::set_span(subscript, SrcSpan::new(from, to));
        super::adverb_origins::place(subscript, &adverbs);
        with_child(subscript, "index", |list| statement_list(list, inside));
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
        Some((from + open, from + end, close.to_string()))
    });
    let Some((inner_from, inner_to, close)) = inner else {
        return;
    };
    // What follows the closing bracket is the subscript's adverbs.
    let Some((close_end, adverbs)) = super::adverb_origins::split(from, to, &close) else {
        return;
    };
    if close_end != inner_to + close.len() {
        return;
    }
    with_child(node, "postfix", |subscript| {
        super::origin::set_span(subscript, SrcSpan::new(from, to));
        super::adverb_origins::place(subscript, &adverbs);
        with_child(subscript, "index", |quote| {
            super::origin::word_quote_inner(quote, SrcSpan::new(inner_from, inner_to))
        });
    });
}

/// The text between `open` and `close` of the brackets written at
/// `from..to`, whitespace included.
// Cost: O(1) plus the brackets' width.
pub(super) fn bracketed(from: usize, to: usize, (open, close): (char, char)) -> Option<SrcSpan> {
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let body = text.strip_prefix(open)?.strip_suffix(close)?;
        let start = from + open.len_utf8();
        Some(SrcSpan::new(start, start + body.len()))
    })
}

/// Give a `StatementList` / `SemiList`, whose statements are written in
/// `inside` (the text between its brackets), the range from its first
/// statement to the `;` after its last, as Rakudo does; a list of no
/// statements sits just after the opening bracket when the brackets hold
/// only whitespace and comments. A statement without a range of its own takes its
/// expression's. Anything [`crate::source_comments::statement_list_end`]
/// cannot account for leaves the list without a range.
// Cost: O(n + s), n = length of `inside`, s = statements.
pub(super) fn statement_list(list: &mut RakuAstNode, inside: SrcSpan) {
    if !matches!(
        list.class,
        RakuAstClass::SemiList | RakuAstClass::StatementSequence | RakuAstClass::StatementList
    ) {
        return;
    }
    statement_ranges(list);
    let statements: Vec<Option<(usize, usize)>> = list
        .fields
        .iter()
        .filter_map(|f| match &f.value {
            RakuAstFieldValue::Node(held) => match held.view() {
                ValueView::RakuAst(statement) => Some(byte_range(statement)),
                _ => None,
            },
            _ => None,
        })
        .collect();
    let base = inside.from as usize;
    let range = super::unit_source::with_text(inside, |text| match statements.as_slice() {
        [] => (crate::source_comments::trivia_len(text)? == text.len()).then_some((base, base)),
        [first, .., last] | [first @ last] => {
            let (first, last) = ((*first)?.0, (*last)?.1);
            let end = crate::source_comments::statement_list_end(
                text,
                first.checked_sub(base)?,
                last.checked_sub(base)?,
            )?;
            Some((first, base + end))
        }
    });
    if let Some((from, to)) = range {
        super::origin::set_span(list, SrcSpan::new(from, to));
    }
}

/// Give each `Statement::Expression` of `list` that has no range the range
/// of its expression.
// Cost: O(s), s = statements.
fn statement_ranges(list: &mut RakuAstNode) {
    for field in list.fields.iter_mut() {
        let RakuAstFieldValue::Node(held) = &mut field.value else {
            continue;
        };
        let ValueView::RakuAst(statement) = held.view() else {
            continue;
        };
        if statement.class != RakuAstClass::StatementExpression
            || statement.fields.len() != 1
            || statement.origin.0.is_some()
        {
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
