//! Origins of method calls and of the argument lists of calls (ADR-10723
//! Stage 2, expression families): the `Call::Method` of `$x.name(...)` and
//! its `.?` / `.+` / `.*` / `.^` forms, `!name` (`Call::PrivateMethod`) and
//! `.&name` (`Call::NameAsMethod`), and the `ArgList` of a call, which Rakudo
//! places from its first argument to its last (a trailing `,` included), or,
//! when there is none, just inside the parentheses or right after the name.
//! Every range is read off the source text and checked against it: exact or
//! absent.

use super::origin::set_span;
use super::{RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The character offset an origin ends at.
// Cost: O(1).
fn origin_end(node: &RakuAstNode) -> Option<i64> {
    let origin = node.origin.0.as_ref()?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    match attributes.as_map().get("to")?.view() {
        ValueView::Int(to) => Some(to),
        _ => None,
    }
}

/// The end of an operand written inside parentheses: `span` is the
/// application, `operand_from` where the operand starts and `end` where it
/// ends. When only `(` and blanks lie between the application's start and
/// the operand, and as many `)` follow it, the end is after the last of them.
// Cost: O(n), n = length of the application's source.
fn past_parens(span: crate::ast::SrcSpan, operand_from: usize, end: u32) -> u32 {
    let (from, end_at) = (span.from as usize, end as usize);
    if operand_from <= from || end_at >= span.to as usize {
        return end;
    }
    let opened = super::unit_source::with_text(crate::ast::SrcSpan::new(from, operand_from), |t| {
        t.chars()
            .all(|c| c == '(' || c.is_whitespace())
            .then(|| t.matches('(').count())
    });
    let Some(opened) = opened.filter(|k| *k > 0) else {
        return end;
    };
    let closed =
        super::unit_source::with_text(crate::ast::SrcSpan::new(end_at, span.to as usize), |t| {
            let mut rest = t;
            for _ in 0..opened {
                rest = rest.trim_start().strip_prefix(')')?;
            }
            Some(t.len() - rest.len())
        });
    closed.map_or(end, |len| end + len as u32)
}

/// Give the `Call::Method` of a method-call application written at `span`
/// (`ApplyPostfix(operand, Call::Method(name, args))`) its origin, its
/// `Name`'s and its `ArgList`'s, as Rakudo does: from the method name to the
/// end of the call, the name, and the inside of the parentheses (empty right
/// after the name when there are none). The text between the operand's end
/// and the call's end is read off the source; anything but `.name`,
/// `.name(...)` and their `.?` / `.+` / `.*` / `.^` / `!` forms is left alone.
// Cost: O(n), n = length of the call's source.
pub(super) fn postfix_call(node: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    use super::RakuAstClass;
    if node.class == RakuAstClass::TermTopicCall && span.is_some() {
        topic_call(node, span);
        return;
    }
    if node.class == RakuAstClass::ApplyDottyInfix && span.is_some() {
        dotty_assign(node, span);
        return;
    }
    if node.class != RakuAstClass::ApplyPostfix || !span.is_some() {
        return;
    }
    super::decl_postfix_origins::operand(node, span);
    let operand = node
        .fields
        .iter()
        .find(|f| f.name == Some("operand"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::RakuAst(operand) => Some(RakuAstNode::clone(operand)),
                _ => None,
            },
            _ => None,
        });
    let operand_end = match &operand {
        // The curried `*` of `*.abs` has no origin of its own (Rakudo gives
        // none); the call starts right after it.
        Some(arg) if arg.class == RakuAstClass::WhateverCodeArgument => {
            super::unit_source::with_text(span, |text| text.starts_with('*').then_some(()))
                .map(|()| span.from + 1)
        }
        Some(operand) => origin_end(operand).and_then(super::unit_source::byte_of),
        None => None,
    };
    let Some(mut operand_end) = operand_end else {
        return;
    };
    // `(1, 2).map(...)`: the operand's range is inside its parentheses, which
    // the application's range includes.
    if let Some((operand_from, _)) = operand.as_ref().and_then(super::infix_origins::byte_range) {
        operand_end = past_parens(span, operand_from, operand_end);
    }
    super::call_term_origins::place(node, operand_end as usize, span);
    // `@a>>.name`: the `MetaPostfix::Hyper` runs from the `>>` to the end and
    // holds the call, which starts at its name.
    if super::infix_origins::child(node, "postfix")
        .is_some_and(|p| p.class == RakuAstClass::MetaPostfixHyper)
    {
        let operand_end = operand_end as usize;
        let skip = super::unit_source::with_text(
            crate::ast::SrcSpan::new(operand_end, span.to as usize),
            |t| {
                t.strip_prefix(">>")
                    .map(|_| 2)
                    .or_else(|| t.strip_prefix('\u{bb}').map(|_| '\u{bb}'.len_utf8()))
            },
        );
        if let Some(skip) = skip {
            super::infix_origins::with_child(node, "postfix", |hyper| {
                set_span(
                    hyper,
                    crate::ast::SrcSpan::new(operand_end, span.to as usize),
                );
                place_call(hyper, None, operand_end + skip, span, false);
            });
        }
        return;
    }
    place_call(node, Some("postfix"), operand_end as usize, span, false);
}

/// Give the `Call::Method` of a topic call written at `span` (`.name`,
/// `.name(...)`: `Term::TopicCall(Call::Method)`) its origin, its `Name`'s and
/// its `ArgList`'s, as for a postfix call; the text starts at the dot.
// Cost: O(n), n = length of the call's source.
fn topic_call(node: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    place_call(node, None, span.from as usize, span, false);
}

/// Give the pieces of a `.=` application written at `span`
/// (`ApplyDottyInfix(left, CallAssign, Method)`) their origins, as Rakudo
/// does: the `.=` operator, the method call (from its name to the end) with
/// its `Name` and `ArgList`, and a simple variable on the left. Only the
/// plain `VAR .= name`, `VAR .= name(...)` and `VAR .= name: ...` spellings
/// are placed; anything else is left alone.
// Cost: O(n), n = length of the application's source.
pub(super) fn dotty_assign(node: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    use super::RakuAstClass;
    if node.class != RakuAstClass::ApplyDottyInfix || !span.is_some() {
        return;
    }
    if super::infix_origins::child(node, "infix")
        .is_some_and(|i| i.class == RakuAstClass::DottyInfixCall)
    {
        return dotty_call(node, span);
    }
    let is_call_assign = super::infix_origins::child(node, "infix")
        .is_some_and(|i| i.class == RakuAstClass::DottyInfixCallAssign);
    if !is_call_assign {
        return;
    }
    // `VAR .= rest`: the variable, blanks, then the operator.
    let Some((var_len, op_at)) = super::unit_source::with_text(span, |text| {
        let var_len = text
            .find(|c: char| c.is_whitespace() || c == '.')
            .filter(|n| *n > 1)?;
        let var = &text[..var_len];
        let simple = var.starts_with(['$', '@', '%'])
            && var[1..]
                .chars()
                .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | ':' | '\''));
        let after_var = text[var_len..].trim_start();
        let op_at = text.len() - after_var.len();
        (simple && after_var.starts_with(".=")).then_some((var_len, op_at))
    }) else {
        return;
    };
    let from = span.from as usize;
    for f in node.fields.iter_mut() {
        let at = match f.name {
            Some("left") => (from, from + var_len),
            Some("infix") => (from + op_at, from + op_at + 2),
            _ => continue,
        };
        if let RakuAstFieldValue::Node(v) = &mut f.value
            && let ValueView::RakuAst(part) = v.view()
            && part.origin.0.is_none()
        {
            let mut part = RakuAstNode::clone(part);
            set_span(&mut part, crate::ast::SrcSpan::new(at.0, at.1));
            *v = Value::rakuast(Box::new(part));
        }
    }
    place_call(node, Some("right"), from + op_at + 2, span, true);
}

/// A method call written at the start of a continuation line
/// (`ApplyDottyInfix(left, DottyInfix::Call, Call::Method)`): the `.` is the
/// operator, the method call starts at its name. The left operand keeps the
/// range it already has; the dot is the first non-blank after it.
// Cost: O(n), n = length of the application's source.
fn dotty_call(node: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    let Some((_, left_end)) =
        super::infix_origins::child(node, "left").and_then(super::infix_origins::byte_range)
    else {
        return;
    };
    let to = span.to as usize;
    let Some(dot) = super::unit_source::with_text(crate::ast::SrcSpan::new(left_end, to), |text| {
        // Blanks, newlines and comments may separate the lines.
        let gap = crate::source_comments::trivia_len(text)?;
        // Only a plain `.name`: `.?name` and the other forms are left alone.
        text[gap..]
            .strip_prefix('.')
            .filter(|rest| rest.starts_with(|c: char| c.is_alphabetic() || c == '_'))
            .map(|_| left_end + gap)
    }) else {
        return;
    };
    super::infix_origins::with_child(node, "infix", |infix| {
        if infix.origin.0.is_none() {
            set_span(infix, crate::ast::SrcSpan::new(dot, dot + 1));
        }
    });
    place_call(node, Some("right"), dot + 1, span, true);
}

/// Place the method call held in field `field` of `node`, which starts after
/// `from` and ends with `span`: its own origin, its `Name`'s and its
/// `ArgList`'s. `dotty` is the `.=` form, where the name follows the operator
/// directly.
// Cost: O(n), n = length of the call's source.
fn place_call(
    node: &mut RakuAstNode,
    field: Option<&str>,
    from: usize,
    span: crate::ast::SrcSpan,
    dotty: bool,
) {
    use super::RakuAstClass;
    let Some(field) = node.fields.iter_mut().find(|f| f.name == field) else {
        return;
    };
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return;
    };
    let ValueView::RakuAst(call) = held.view() else {
        return;
    };
    let class = call.class;
    if !matches!(
        class,
        RakuAstClass::CallMethod
            | RakuAstClass::CallPrivateMethod
            | RakuAstClass::CallNameAsMethod
            | RakuAstClass::CallMetaMethod
    ) || call.origin.0.is_some()
    {
        return;
    }
    let Some((name, args)) =
        super::unit_source::with_text(crate::ast::SrcSpan::new(from, span.to as usize), |text| {
            let after_ws = text.trim_start();
            let after_op = match class {
                RakuAstClass::CallPrivateMethod => after_ws.strip_prefix('!'),
                RakuAstClass::CallNameAsMethod => after_ws.strip_prefix(".&"),
                _ if dotty => Some(after_ws),
                _ => after_ws
                    .strip_prefix('.')
                    .map(|r| r.strip_prefix(['?', '+', '*', '^']).unwrap_or(r))
                    .or_else(|| after_ws.strip_prefix('!')),
            }?;
            let name_at = from + (text.len() - after_op.len());
            let mut name_len = after_op
                .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':')))
                .unwrap_or(after_op.len());
            // `.name: args`: a single trailing colon is the argument list's
            // introducer, not part of the name.
            let colon_form = after_op[..name_len]
                .strip_suffix(':')
                .is_some_and(|stem| !stem.ends_with(':'))
                && after_op[name_len..].starts_with(char::is_whitespace);
            if colon_form {
                name_len -= 1;
            }
            if name_len == 0 {
                return None;
            }
            let name_end = name_at + name_len;
            let rest = &after_op[name_len..];
            let args = if colon_form {
                let args = rest[1..].trim_start();
                if args.is_empty() {
                    return None;
                }
                ArgsAt::Range(from + text.len() - args.len(), from + text.len())
            } else if rest.is_empty() {
                ArgsAt::Range(name_end, name_end)
            } else if rest.starts_with('(') && rest.ends_with(')') && rest.len() >= 2 {
                ArgsAt::Parens(crate::ast::SrcSpan::new(
                    name_end + 1,
                    from + text.len() - 1,
                ))
            } else {
                return None;
            };
            Some(((name_at, name_end), args))
        })
    else {
        return;
    };
    let mut call = RakuAstNode::clone(call);
    set_span(
        &mut call,
        crate::ast::SrcSpan::new(name.0, span.to as usize),
    );
    let args = match args {
        ArgsAt::Range(from, to) => Some((from, to)),
        ArgsAt::Parens(inside) => parenthesized(super::infix_origins::child(&call, "args"), inside),
    };
    if let Some((from, to)) = args {
        super::empty_fields::set_span(&mut call, "args", crate::ast::SrcSpan::new(from, to));
    }
    for f in call.fields.iter_mut() {
        let at = match (f.name, args) {
            (Some("name"), _) => name,
            (Some("args"), Some(args)) => args,
            _ => continue,
        };
        if let RakuAstFieldValue::Node(v) = &mut f.value
            && let ValueView::RakuAst(part) = v.view()
        {
            let mut part = RakuAstNode::clone(part);
            set_span(&mut part, crate::ast::SrcSpan::new(at.0, at.1));
            *v = Value::rakuast(Box::new(part));
        }
    }
    *held = Value::rakuast(Box::new(call));
}

/// Where a call's arguments were written: a range already known, or the
/// inside of its parentheses, placed by [`parenthesized`].
#[derive(Clone, Copy)]
enum ArgsAt {
    Range(usize, usize),
    Parens(crate::ast::SrcSpan),
}

/// The range of the `ArgList` `args` written between parentheses, at
/// `inside`: from its first argument to its last (a trailing `,` included),
/// or, with no arguments, just after the `(`. Anything around the arguments
/// but whitespace and comments, or an argument without a range, leaves it
/// without one.
// Cost: O(n + a), n = length of `inside`, a = arguments.
pub(super) fn parenthesized(
    args: Option<&RakuAstNode>,
    inside: crate::ast::SrcSpan,
) -> Option<(usize, usize)> {
    use crate::source_comments::trivia_len;
    let ranges: Vec<Option<(usize, usize)>> = args
        .map(|list| {
            list.fields
                .iter()
                .filter_map(|f| match &f.value {
                    RakuAstFieldValue::Node(v) => match v.view() {
                        ValueView::RakuAst(arg) => Some(super::infix_origins::byte_range(arg)),
                        _ => None,
                    },
                    _ => None,
                })
                .collect()
        })
        .unwrap_or_default();
    let base = inside.from as usize;
    super::unit_source::with_text(inside, |text| match ranges.as_slice() {
        [] => (trivia_len(text)? == text.len()).then_some((base, base)),
        [first, .., last] | [first @ last] => {
            let (first, last) = ((*first)?.0, (*last)?.1);
            let leading = text.get(..first.checked_sub(base)?)?;
            if trivia_len(leading)? != leading.len() {
                return None;
            }
            let tail = text.get(last.checked_sub(base)?..)?;
            let mut end = trivia_len(tail)?;
            let to = if tail[end..].starts_with(',') {
                end += 1;
                last + end
            } else {
                last
            };
            (trivia_len(&tail[end..])? == tail.len() - end).then_some((first, to))
        }
    })
}
