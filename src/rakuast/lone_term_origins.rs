//! Origins of a lone term the view rebuilt rather than read from the source,
//! or the conversion could not tell apart from another (ADR-10723 Stage 2;
//! #15780): the colonpair argument of an attribute's `is built(:bind)`,
//! whose parentheses have a range, and the variable a statement list ends
//! with. The text around them, trimmed, is the term when it spells it.

use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place `term`, a `ColonPair::True` / `False` (`:bind`, `:!bind`), over
/// `span` when that is exactly how it is written.
// Cost: O(n), n = length of the term's text.
pub(super) fn place(term: &mut RakuAstNode, span: SrcSpan) {
    let prefix = match term.class {
        RakuAstClass::ColonPairTrue => ":",
        RakuAstClass::ColonPairFalse => ":!",
        _ => return,
    };
    let Some(key) = super::infix_origins::leaf_text(term) else {
        return;
    };
    let written = super::unit_source::with_text(span, |text| {
        (text == format!("{prefix}{key}")).then_some(())
    });
    if written.is_some() {
        super::origin::set_span(term, span);
    }
}

/// `{ ...; $x }`: the variable that ends a statement list, when the parser
/// left it unplaced (a parameter's name, read again in the body, is a term
/// of the same shape the conversion cannot tell apart). Its range is its
/// spelling at the end of the text `inside` the braces, after `;`, `{` or a
/// blank.
// Cost: O(v + w), v = length of the variable's name, w = trailing blanks.
pub(super) fn trailing_variable(list: &mut RakuAstNode, inside: SrcSpan) {
    use super::infix_origins::{child_at, leaf_text, with_child_at};
    let Some(last) = list.fields.len().checked_sub(1) else {
        return;
    };
    let Some(name) = child_at(list, last)
        .filter(|s| s.class == RakuAstClass::StatementExpression && s.origin.0.is_none())
        .and_then(|s| child_at(s, 0))
        .filter(|v| {
            v.origin.0.is_none()
                && matches!(v.class, RakuAstClass::VarLexical | RakuAstClass::VarDynamic)
        })
        .and_then(leaf_text)
    else {
        return;
    };
    let Some(at) = super::unit_source::with_text(inside, |text| {
        let body = text
            .trim_end()
            .strip_suffix(';')
            .unwrap_or(text.trim_end())
            .trim_end();
        let head = body.strip_suffix(name.as_str())?;
        (head.is_empty() || head.ends_with([';', '{', ' ', '\t', '\n']))
            .then_some(inside.from as usize + head.len())
    }) else {
        return;
    };
    with_child_at(list, last, |statement| {
        with_child_at(statement, 0, |var| {
            super::origin::set_span(var, SrcSpan::new(at, at + name.len()))
        })
    });
}

/// `(1 if True)` / `($c(1) for @a)`: a lone statement with a modifier inside
/// parentheses written at `inside`, whose modifier is placed but whose
/// expression the parser left unplaced. The expression is the text from the
/// start of the parentheses' contents to the modifier, trimmed (parsed again
/// for its ranges), and the statement runs from it to the modifier's end.
// Cost: O(n), n = length of the statement's text.
pub(super) fn modified_statement(list: &mut RakuAstNode, inside: SrcSpan) {
    use super::infix_origins::{byte_range, child, child_at, with_child, with_child_at};
    let [_] = list.fields.as_slice() else {
        return;
    };
    let Some(statement) = child_at(list, 0)
        .filter(|s| s.class == RakuAstClass::StatementExpression && s.origin.0.is_none())
    else {
        return;
    };
    let Some(modifier) = ["condition-modifier", "loop-modifier"]
        .into_iter()
        .find_map(|name| child(statement, name).and_then(byte_range))
    else {
        return;
    };
    if child(statement, "expression").is_none_or(|e| e.origin.0.is_some()) {
        return;
    }
    let from = inside.from as usize;
    let Some((at, written)) =
        super::unit_source::with_text(SrcSpan::new(from, modifier.0), |text| {
            let lead = text.len() - text.trim_start().len();
            let written = text.trim();
            (!written.is_empty()).then(|| (from + lead, written.to_string()))
        })
    else {
        return;
    };
    // The expression's text is parsed again as a fragment, whose ranges
    // are copied over: exact or absent.
    with_child_at(list, 0, |statement| {
        with_child(statement, "expression", |expression| {
            super::fragment_origins::expression(
                &crate::scope::NameScope::default(),
                expression,
                &written,
                at,
            );
        });
        if child(statement, "expression").is_some_and(|e| e.origin.0.is_some()) {
            super::origin::set_span(statement, SrcSpan::new(at, modifier.1));
        }
    });
}

/// `$x = $y = 1`: an infix or postfix application on the right of another
/// infix, which the parser left unplaced, runs from after the operator
/// (ending at byte `op_end`) and its blanks to the end of the outer
/// application, and its parts are derived from that range.
// Cost: O(n), n = length of the operand's text.
pub(super) fn right_after_operator(node: &mut RakuAstNode, op_end: usize) {
    use super::infix_origins::{byte_range, child, with_child};
    let Some((_, to)) = byte_range(node) else {
        return;
    };
    let unplaced = child(node, "right").is_some_and(|right| {
        right.origin.0.is_none()
            && matches!(
                right.class,
                RakuAstClass::ApplyInfix | RakuAstClass::ApplyPostfix
            )
    });
    if !unplaced || op_end >= to {
        return;
    }
    let Some(from) = super::unit_source::with_text(SrcSpan::new(op_end, to), |text| {
        let operand = text.trim_start();
        (operand.len() < text.len() && !operand.is_empty()).then(|| to - operand.len())
    }) else {
        return;
    };
    with_child(node, "right", |right| {
        super::origin::set_span(right, SrcSpan::new(from, to));
        super::infix_origins::derive(&crate::scope::NameScope::default(), right);
    });
}

/// `@a[0] := $v`: a postfix left operand the parser left unplaced, in an application
/// with a range, runs from the application's start to the blanks before the
/// operator (at byte `op_at`), and its parts are derived from that range.
// Cost: O(n), n = length of the operand's text.
pub(super) fn left_before_operator(node: &mut RakuAstNode, op_at: usize) {
    use super::infix_origins::{byte_range, child, with_child};
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    // Only a postfix application (`@a[0]`, `$o.m`), whose text is its own:
    // a term the parser unwrapped from parentheses would not be.
    if child(node, "left")
        .is_none_or(|left| left.origin.0.is_some() || left.class != RakuAstClass::ApplyPostfix)
        || op_at <= from
    {
        return;
    }
    let Some(len) = super::unit_source::with_text(SrcSpan::new(from, op_at), |text| {
        let written = text.trim_end();
        (!written.is_empty()).then_some(written.len())
    }) else {
        return;
    };
    with_child(node, "left", |left| {
        super::origin::set_span(left, SrcSpan::new(from, from + len));
        super::infix_origins::derive(&crate::scope::NameScope::default(), left);
    });
}

/// `@a[3] = @a[2] = 8`: the assignee of an assigning subscript, when it is
/// an application the parser left unplaced. It runs from after the `=` that
/// follows the subscript's closing bracket to the end of `node`, the
/// `ApplyPostfix` written at `node`'s range whose operand ends at
/// `operand_end`, and its parts are derived from that range.
// Cost: O(n), n = length of the application's text.
pub(super) fn subscript_assignee(node: &mut RakuAstNode, operand_end: usize) {
    use super::infix_origins::{byte_range, child, with_child};
    let Some((_, to)) = byte_range(node) else {
        return;
    };
    let unplaced = child(node, "postfix")
        .and_then(|subscript| child(subscript, "assignee"))
        .is_some_and(|value| {
            value.origin.0.is_none()
                && matches!(
                    value.class,
                    RakuAstClass::ApplyInfix | RakuAstClass::ApplyPostfix
                )
        });
    if !unplaced || operand_end >= to {
        return;
    }
    let Some(from) = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
        let rest = text.strip_prefix('.').unwrap_or(text);
        let (open, close) = match rest.chars().next()? {
            '[' => ('[', ']'),
            '{' => ('{', '}'),
            _ => return None,
        };
        let inner = &rest[1..];
        let after = &inner[super::subst_origins::closing(inner, open, close)? + 1..];
        let value = after.trim_start().strip_prefix('=')?;
        if value.starts_with(['=', '>']) {
            return None;
        }
        let operand = value.trim_start();
        (!operand.is_empty() && operand.len() < value.len()).then(|| to - operand.len())
    }) else {
        return;
    };
    with_child(node, "postfix", |subscript| {
        with_child(subscript, "assignee", |value| {
            super::origin::set_span(value, SrcSpan::new(from, to));
            super::infix_origins::derive(&crate::scope::NameScope::default(), value);
        })
    });
}
