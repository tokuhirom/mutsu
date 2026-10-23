//! Origins of the segments of an interpolating string (ADR-10723 Stage 2,
//! expression families): `"a $x b"`, `"a{ $x }b"`, `qq{...}`.
//!
//! The segments are placed left to right, each where the source text says
//! it starts: a variable where its name is written, a block where its `{`
//! is, and the literal text between them. A segment whose written form
//! cannot be told for certain (an interpolation followed by a subscript or
//! method call, a block holding quotes) stops the walk, so the segments
//! after it stay without an origin: exact or absent.

use super::infix_origins::{byte_range, leaf_text};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Place the segments of `node`, an interpolating `QuotedString` with an
/// origin.
// Cost: O(n + s), n = length of the string's source, s = segments.
pub(super) fn interpolated(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(Some((start, end))) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        Some(body(text).map(|(open, close)| (from + open, from + close)))
    }) else {
        return;
    };
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("segments")) else {
        return;
    };
    let RakuAstFieldValue::List(segments) = &mut field.value else {
        return;
    };
    // A single literal segment is placed by `quoted_text`; a single
    // interpolation is placed here.
    let single_literal = matches!(
        segments.as_slice(),
        [only] if matches!(only.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::StrLiteral)
    );
    if segments.is_empty() || single_literal {
        return;
    }
    let mut cursor = start;
    for i in 0..segments.len() {
        let ValueView::RakuAst(segment) = segments[i].view() else {
            return;
        };
        let mut segment = RakuAstNode::clone(segment);
        if segment.origin.0.is_some() {
            return;
        }
        let placed = match segment.class {
            RakuAstClass::StrLiteral => {
                let next = match segments.get(i + 1).map(Value::view) {
                    Some(ValueView::RakuAst(next)) => written_start(next, cursor, end),
                    Some(_) => None,
                    None => Some(end),
                };
                next.filter(|&next| next > cursor && literal_matches(&segment, cursor, next))
                    .map(|next| (cursor, next))
            }
            RakuAstClass::VarLexical | RakuAstClass::VarDynamic | RakuAstClass::VarAttribute => {
                variable_at(&segment, cursor, end)
            }
            RakuAstClass::Block => block_at(&mut segment, cursor, end),
            RakuAstClass::ApplyPostfix => postfix_at(&mut segment, cursor, end),
            _ => None,
        };
        let Some((at, next)) = placed else {
            return;
        };
        super::origin::set_span(&mut segment, SrcSpan::new(at, next));
        segments[i] = Value::rakuast(Box::new(segment));
        cursor = next;
    }
}

/// The byte range of a `"..."` or `qq` string's body within its source
/// `text`: after the opening delimiter, before the closing one. Heredocs and
/// quotes with other adverbs are not handled.
// Cost: O(n), n = length of `text`.
fn body(text: &str) -> Option<(usize, usize)> {
    let (prefix, rest) = text.split_at(text.find(|c: char| !c.is_alphanumeric())?);
    if !matches!(prefix, "" | "qq") {
        return None;
    }
    let open = rest.chars().next()?;
    let close = match open {
        '"' if prefix.is_empty() => '"',
        '{' if !prefix.is_empty() => '}',
        '[' if !prefix.is_empty() => ']',
        '(' if !prefix.is_empty() => ')',
        '<' if !prefix.is_empty() => '>',
        '/' | '|' | '!' if !prefix.is_empty() => open,
        _ => return None,
    };
    text.ends_with(close).then(|| {
        (
            prefix.len() + open.len_utf8(),
            text.len() - close.len_utf8(),
        )
    })
}

/// Whether the byte at `at` is escaped by an odd run of backslashes.
// Cost: O(b), b = backslashes before `at`.
fn escaped(text: &str, at: usize) -> bool {
    text[..at].bytes().rev().take_while(|&b| b == b'\\').count() % 2 == 1
}

/// Where the source text of `next`, the segment after a literal, starts at
/// or after `cursor`: the first unescaped spelling of a variable, or `{` of
/// a block.
// Cost: O(n), n = end - cursor.
fn written_start(next: &RakuAstNode, cursor: usize, end: usize) -> Option<usize> {
    let needle = match next.class {
        RakuAstClass::VarLexical | RakuAstClass::VarDynamic | RakuAstClass::VarAttribute => {
            leaf_text(next)?
        }
        RakuAstClass::Block => "{".to_string(),
        RakuAstClass::ApplyPostfix => leaf_text(interpolated_variable(next)?)?,
        _ => return None,
    };
    super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let mut search = 0;
        while let Some(found) = text[search..].find(needle.as_str()) {
            let at = search + found;
            if !escaped(text, at) {
                return Some(cursor + at);
            }
            search = at + needle.len();
        }
        None
    })
}

/// Whether a literal written at `from..to` can be the `StrLiteral`
/// `segment`: equal to its value when written without escapes.
// Cost: O(n), n = to - from.
fn literal_matches(segment: &RakuAstNode, from: usize, to: usize) -> bool {
    let Some(value) = leaf_text(segment) else {
        return false;
    };
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        (text.contains('\\') || text == value).then_some(())
    })
    .is_some()
}

/// The range of a variable written at `cursor`, when its spelling is there
/// and nothing that would extend the interpolation (`[`, `{`, `<`, `(`, `.`)
/// follows it.
// Cost: O(v), v = length of the variable's name.
fn variable_at(segment: &RakuAstNode, cursor: usize, end: usize) -> Option<(usize, usize)> {
    let name = leaf_text(segment)?;
    super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        let extends = rest.starts_with(['[', '{', '<', '(', '.', '-', '\''])
            || rest.starts_with(|c: char| c.is_alphanumeric() || c == '_');
        (!extends).then_some((cursor, cursor + name.len()))
    })
}

/// The range of a `{ ... }` block written at `cursor`, its `Blockoid`'s and
/// `StatementList`'s, and its statements' (from their expressions). A block
/// whose text holds a quote or a backslash, which could hide a brace, is
/// left alone.
// Cost: O(n + s), n = length of the block, s = its statements.
fn block_at(block: &mut RakuAstNode, cursor: usize, end: usize) -> Option<(usize, usize)> {
    let close = super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        if !text.starts_with('{') {
            return None;
        }
        let mut depth = 0usize;
        for (at, c) in text.char_indices() {
            match c {
                '"' | '\'' | '\\' | '「' | '」' => return None,
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(cursor + at);
                    }
                }
                _ => {}
            }
        }
        None
    })?;
    let inside = super::list_origins::bracketed(cursor, close + 1, ('{', '}'))?;
    let whole = SrcSpan::new(cursor, close + 1);
    super::routine_origins::block_origins(
        block,
        crate::ast::parsed::BlockSpans {
            span: whole,
            blockoid: whole,
            list: SrcSpan::NONE,
        },
    );
    super::infix_origins::with_child(block, "body", |blockoid| {
        super::infix_origins::with_child_at(blockoid, 0, |list| {
            super::list_origins::statement_list(list, inside)
        })
    });
    Some((cursor, close + 1))
}

/// The variable an interpolated postfix application (`@x[0]`, `%h<a>`,
/// `@x.elems()`) is applied to.
// Cost: O(f), f = fields of `node`.
fn interpolated_variable(node: &RakuAstNode) -> Option<&RakuAstNode> {
    let operand = super::infix_origins::child(node, "operand")?;
    matches!(
        operand.class,
        RakuAstClass::VarLexical | RakuAstClass::VarDynamic | RakuAstClass::VarAttribute
    )
    .then_some(operand)
}

/// The range of an interpolated `$x[...]` / `%h{...}` / `%h<...>` /
/// `$x.name(...)` written at `cursor`, its variable's and, from those, its
/// subscript's or method call's. A subscript or argument list holding a quote,
/// a backslash or a comment, which could hide its closing bracket, is left
/// alone.
// Cost: O(n), n = length of the interpolation.
fn postfix_at(segment: &mut RakuAstNode, cursor: usize, end: usize) -> Option<(usize, usize)> {
    let name = leaf_text(interpolated_variable(segment)?)?;
    let postfix = super::infix_origins::child(segment, "postfix")?.class;
    let to = super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        let at = name.len();
        let close = match postfix {
            RakuAstClass::PostcircumfixArrayIndex => balanced(rest, '[', ']')?,
            RakuAstClass::PostcircumfixHashIndex => balanced(rest, '{', '}')?,
            RakuAstClass::PostcircumfixLiteralHashIndex => {
                let inner = rest.strip_prefix('<')?;
                if inner.starts_with('<') || inner.contains(['\\', '"', '\'']) {
                    return None;
                }
                inner.find('>')? + 2
            }
            RakuAstClass::CallMethod => {
                let method = rest.strip_prefix('.')?;
                let name_len = method
                    .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-')))
                    .filter(|&n| n > 0)?;
                1 + name_len + balanced(&method[name_len..], '(', ')')?
            }
            _ => return None,
        };
        Some(cursor + at + close)
    })?;
    super::origin::set_span(segment, SrcSpan::new(cursor, to));
    super::infix_origins::with_child(segment, "operand", |var| {
        super::origin::set_span(var, SrcSpan::new(cursor, cursor + name.len()))
    });
    if postfix == RakuAstClass::CallMethod {
        let mut node = RakuAstNode::clone(segment);
        super::call_origins::postfix_call(&mut node, SrcSpan::new(cursor, to));
        *segment = node;
    } else {
        super::list_origins::postcircumfix(segment);
    }
    Some((cursor, to))
}

/// The length of the bracketed text `text` starts with, `open` to its
/// matching `close`, when nothing inside could hide a bracket.
// Cost: O(n), n = length of the bracketed text.
fn balanced(text: &str, open: char, close: char) -> Option<usize> {
    if !text.starts_with(open) {
        return None;
    }
    let mut depth = 0usize;
    for (at, c) in text.char_indices() {
        match c {
            '"' | '\'' | '\\' | '#' | '「' | '」' => return None,
            c if c == open => depth += 1,
            c if c == close => {
                depth -= 1;
                if depth == 0 {
                    return Some(at + c.len_utf8());
                }
            }
            _ => {}
        }
    }
    None
}
