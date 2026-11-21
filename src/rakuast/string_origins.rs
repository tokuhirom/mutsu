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
pub(super) fn interpolated(names: &crate::scope::NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(Some((start, end))) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        Some(body(text).map(|(open, close)| (from + open, from + close)))
    }) else {
        return;
    };
    // A single literal segment is placed by `quoted_text`; a single
    // interpolation is placed here.
    let single_literal = node.fields.iter().any(|f| {
        f.name == Some("segments")
            && matches!(&f.value, RakuAstFieldValue::List(segments) if matches!(
                segments.as_slice(),
                [only] if matches!(only.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::StrLiteral)
            ))
    });
    if !single_literal {
        segments(names, node, start, end, 0);
    }
}

/// Place the segments of `node`, a `QuotedString` whose body (the text
/// between its delimiters) is written at `start..end`, left to right. A
/// heredoc's body is written `indent` columns deeper than its value: that
/// much leading whitespace on each line is part of a literal's range, not
/// of its value.
// Cost: O(n + s), n = end - start, s = segments.
pub(super) fn segments(
    names: &crate::scope::NameScope,
    node: &mut RakuAstNode,
    start: usize,
    end: usize,
    indent: usize,
) {
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("segments")) else {
        return;
    };
    let RakuAstFieldValue::List(segments) = &mut field.value else {
        return;
    };
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
                // `s/a//`: an empty literal sits where it was (not) written.
                let empty = leaf_text(&segment).is_some_and(|t| t.is_empty());
                next.filter(|&next| {
                    (next > cursor && literal_matches(&segment, cursor, next, indent))
                        || (empty && next == cursor)
                })
                .map(|next| (cursor, next))
            }
            RakuAstClass::VarLexical | RakuAstClass::VarDynamic | RakuAstClass::VarAttribute => {
                let literal = match segments.get(i + 1).map(Value::view) {
                    Some(ValueView::RakuAst(next)) if next.class == RakuAstClass::StrLiteral => {
                        leaf_text(next)
                    }
                    _ => None,
                };
                variable_at(&segment, cursor, end, literal.as_deref())
            }
            RakuAstClass::Block => super::string_block_origins::block_at(&mut segment, cursor, end),
            RakuAstClass::ApplyPostfix => postfix_at(names, &mut segment, cursor, end),
            RakuAstClass::VarNamedCapture | RakuAstClass::VarPositionalCapture => {
                match_variable_at(&mut segment, cursor, end)
            }
            RakuAstClass::QuoteWordsAtom => quote_words_atom(names, &mut segment, cursor, end),
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
    // `«a $x b»` / `<<a $x b>>`: an interpolating word quote, kept raw.
    for (open, close) in [("«", "»"), ("<<", ">>")] {
        if let Some(inner) = text.strip_prefix(open) {
            return inner
                .ends_with(close)
                .then(|| (open.len(), text.len() - close.len()));
        }
    }
    let (prefix, mut rest) = text.split_at(text.find(|c: char| !c.is_alphanumeric())?);
    if !matches!(prefix, "" | "qq" | "qqw" | "qqx" | "q" | "Q") {
        return None;
    }
    // `Q:s|$n|` / `q:c{...}`: the adverbs that make the quote interpolate.
    let mut adverbs = 0;
    while let Some(after) = rest.strip_prefix(':') {
        let len = after
            .find(|c: char| !c.is_alphanumeric())
            .unwrap_or(after.len());
        // `:c` closures are kept in a shape of their own; other adverbs
        // (`:!s`, `:to`) are not read here.
        if !matches!(
            &after[..len],
            "s" | "a" | "h" | "f" | "b" | "scalar" | "array" | "hash" | "function" | "backslash"
        ) {
            return None;
        }
        rest = &after[len..];
        adverbs += 1 + len;
    }
    if matches!(prefix, "q" | "Q") && adverbs == 0 {
        return None;
    }
    let prefix_len = prefix.len() + adverbs;
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
    text.ends_with(close)
        .then(|| (prefix_len + open.len_utf8(), text.len() - close.len_utf8()))
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
        RakuAstClass::VarNamedCapture | RakuAstClass::VarPositionalCapture => {
            super::match_vars::interpolated_text(next)?
        }
        // `« :a<b> »`: a colonpair among the words.
        RakuAstClass::QuoteWordsAtom => ":".to_string(),
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
fn literal_matches(segment: &RakuAstNode, from: usize, to: usize, indent: usize) -> bool {
    let Some(value) = leaf_text(segment) else {
        return false;
    };
    let line_start = from == 0
        || super::unit_source::with_text(SrcSpan::new(from - 1, from), |t| {
            (t == "\n").then_some(())
        })
        .is_some();
    super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        (text.contains('\\') || dedent(text, indent, line_start) == value).then_some(())
    })
    .is_some()
}

/// `text` without up to `indent` blanks at the start of each of its lines
/// (its first line too when `line_start`).
// Cost: O(n), n = length of `text`.
fn dedent(text: &str, indent: usize, line_start: bool) -> String {
    if indent == 0 {
        return text.to_string();
    }
    let mut out = String::with_capacity(text.len());
    for (i, line) in text.split_inclusive('\n').enumerate() {
        if i == 0 && !line_start {
            out.push_str(line);
            continue;
        }
        let blank = line
            .chars()
            .take(indent)
            .take_while(|c| matches!(c, ' ' | '\t'))
            .count();
        out.push_str(&line[blank..]);
    }
    out
}

/// The range of a variable written at `cursor`, when its spelling is there
/// and nothing that would extend the interpolation (`[`, `{`, `<`, `(`, `.`)
/// follows it, unless that is the text of `literal`, the literal segment
/// after it (`"$n.new answers"`: no call without parentheses).
// Cost: O(v + l), v = length of the variable's name, l = of `literal`.
fn variable_at(
    segment: &RakuAstNode,
    cursor: usize,
    end: usize,
    literal: Option<&str>,
) -> Option<(usize, usize)> {
    let name = leaf_text(segment)?;
    super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        if literal.is_some_and(|l| l.starts_with('.') && rest.starts_with(l)) {
            return Some((cursor, cursor + name.len()));
        }
        // A `-` or `'` continues the name only before a letter (`$x-y`,
        // `$don't`); `'$src' is` ends it.
        let joins = rest
            .strip_prefix(['-', '\''])
            .is_some_and(|after| after.starts_with(|c: char| c.is_alphabetic() || c == '_'));
        let extends = joins
            || rest.starts_with(['[', '{', '<', '(', '.'])
            || rest.starts_with(|c: char| c.is_alphanumeric() || c == '_');
        (!extends).then_some((cursor, cursor + name.len()))
    })
}

/// The range of a match variable (`$<word>`, `$0`) written at `cursor`;
/// a `$<word>`'s word quote is placed between its brackets.
// Cost: O(n), n = length of the variable.
fn match_variable_at(
    segment: &mut RakuAstNode,
    cursor: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let written = super::match_vars::interpolated_text(segment)?;
    super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let rest = text.strip_prefix(written.as_str())?;
        let extends = segment.class == RakuAstClass::VarPositionalCapture
            && rest.starts_with(|c: char| c.is_ascii_digit());
        (!extends && !rest.starts_with(['[', '{', '<', '.'])).then_some(())
    })?;
    let range = (cursor, cursor + written.len());
    super::origin::set_span(segment, SrcSpan::new(range.0, range.1));
    super::match_vars::named_capture_origins(segment);
    Some(range)
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
fn postfix_at(
    names: &crate::scope::NameScope,
    segment: &mut RakuAstNode,
    cursor: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let name = leaf_text(interpolated_variable(segment)?)?;
    let postfix = super::infix_origins::child(segment, "postfix")?.class;
    // `"@x.[]"`: the dot a subscript may be written after.
    let mut dot = 0;
    let to = super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        let mut rest = text.strip_prefix(name.as_str())?;
        if postfix != RakuAstClass::CallMethod
            && let Some(after) = rest.strip_prefix('.')
            && after.starts_with(['[', '{', '<'])
        {
            rest = after;
            dot = 1;
        }
        let at = name.len() + dot;
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
        // The subscript's expressions were read without positions.
        if matches!(
            postfix,
            RakuAstClass::PostcircumfixArrayIndex | RakuAstClass::PostcircumfixHashIndex
        ) {
            let open = cursor + name.len() + dot + 1;
            let inner =
                super::unit_source::with_text(SrcSpan::new(open, to - 1), |t| Some(t.to_string()));
            if let Some(inner) = inner {
                super::infix_origins::with_child(segment, "postfix", |sub| {
                    super::infix_origins::with_child(sub, "index", |list| {
                        super::fragment_origins::statements(names, list, &inner, open)
                    })
                });
            }
        }
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

/// The range of a `QuoteWordsAtom` written at `cursor` of an interpolating
/// word quote (`« :X<x> b »`): the colonpair it holds, placed over its text
/// with its value.
// Cost: O(n), n = end - cursor.
fn quote_words_atom(
    names: &crate::scope::NameScope,
    atom: &mut RakuAstNode,
    cursor: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let pair = super::infix_origins::child_at(atom, 0)?;
    let len = super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        super::hash_colonpair_origins::colonpair_len(pair, text)
    })?;
    let span = SrcSpan::new(cursor, cursor + len);
    super::infix_origins::with_child_at(atom, 0, |pair| {
        super::origin::set_span(pair, span);
        super::infix_origins::derive(names, pair);
    });
    Some((cursor, cursor + len))
}

/// Place the segments of a heredoc's `node`, whose body was written at
/// `written` (the lines before the terminator) and is dedented by the blanks
/// the terminator line `stop` starts with. A body that is one piece of text
/// has the range of all its lines, as in Rakudo.
// Cost: O(n + s), n = length of the body, s = segments.
pub(super) fn heredoc(
    names: &crate::scope::NameScope,
    node: &mut RakuAstNode,
    written: SrcSpan,
    stop: &str,
) {
    let indent = stop.chars().take_while(|c| matches!(c, ' ' | '\t')).count();
    leading_indent_literal(node, indent);
    if !written.is_some() {
        return;
    }
    let single_literal = node
        .fields
        .iter_mut()
        .find(|f| f.name == Some("segments"))
        .and_then(|f| match &mut f.value {
            RakuAstFieldValue::List(items) => match items.as_mut_slice() {
                [only] => Some(only),
                _ => None,
            },
            _ => None,
        });
    if let Some(only) = single_literal
        && let ValueView::RakuAst(text) = only.view()
        && text.class == RakuAstClass::StrLiteral
    {
        let mut text = RakuAstNode::clone(text);
        super::origin::set_span(&mut text, written);
        *only = Value::rakuast(Box::new(text));
        return;
    }
    segments(
        names,
        node,
        written.from as usize,
        written.to as usize,
        indent,
    );
}

/// An indented heredoc whose body starts with an interpolation (`    $x`)
/// starts, in Rakudo, with the empty `StrLiteral` its stripped indentation
/// leaves; `node` gets that segment when it has none.
// Cost: O(s), s = segments.
fn leading_indent_literal(node: &mut RakuAstNode, indent: usize) {
    if indent == 0 {
        return;
    }
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("segments")) else {
        return;
    };
    let RakuAstFieldValue::List(segments) = &mut field.value else {
        return;
    };
    let starts_with_literal = match segments.first().map(Value::view) {
        Some(ValueView::RakuAst(first)) => first.class == RakuAstClass::StrLiteral,
        _ => true,
    };
    if !starts_with_literal {
        segments.insert(
            0,
            Value::rakuast(Box::new(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StrLiteral,
                fields: vec![super::view::leaf_field(None, Value::str_from(""))],
            })),
        );
    }
}
