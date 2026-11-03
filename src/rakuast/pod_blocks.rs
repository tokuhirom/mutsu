//! Pod blocks written between the statements of a unit, as the
//! `RakuAST::Doc::Block` statements of its `.AST` (ADR-10723 Stage 4,
//! RakuDoc as `RakuAST::Doc`; #14880).
//!
//! The statement parser skips Pod, so the blocks are read from the source
//! text: a line that begins a directive and lies outside every top-level
//! statement starts a block, and the block runs as rakudo's reader runs it
//! (a delimited block to its `=end`, an extended or abbreviated one to the
//! next blank line or directive, a paragraph keeping the blank lines after
//! it). The blocks are then placed between the statements by their offsets.
//!
//! Only the plain shape is read: a block with configuration, markup
//! (`B<...>`), indented (implicit code) lines, one of the verbatim or tabular
//! types (`code`, `table`, ...), or one a statement starts inside, is left
//! out as before. Exact or absent.

use super::infix_origins::byte_range;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{SrcSpan, Stmt};
use crate::scope::NameScope;
use crate::value::Value;

/// The block types whose paragraphs are not plain text.
const SPECIAL: &[&str] = &[
    "code", "input", "output", "comment", "table", "defn", "config", "formula", "row", "column",
    "cell", "nested", "toc", "semantic", "alias",
];

/// How a block was written.
#[derive(Clone, Copy, PartialEq)]
enum Form {
    Delimited,
    For,
    Abbreviated,
}

/// A paragraph: its text, or a block nested in a delimited one.
enum Part {
    Text(String),
    Block(Block),
}

struct Block {
    kind: String,
    level: Option<i64>,
    form: Form,
    /// The `:name(value)` pairs after the type, in written order.
    config: Vec<(String, RakuAstNode)>,
    parts: Vec<Part>,
    /// The unit offsets the block's text spans, without the blank lines after.
    from: usize,
    to: usize,
}

struct Lines<'a> {
    names: &'a NameScope,
    /// The address of the source's first byte.
    base: usize,
    text: Vec<&'a str>,
    /// The offset of each line's first byte.
    start: Vec<usize>,
}

impl Lines<'_> {
    // Cost: O(1).
    fn blank(&self, i: usize) -> bool {
        self.text[i].trim().is_empty()
    }

    /// The directive a line begins: its name and the rest of the line.
    // Cost: O(l), l = length of the line.
    fn directive(&self, i: usize) -> Option<(&str, &str)> {
        let line = self.text.get(i)?.trim_start().strip_prefix('=')?;
        let end = line
            .find(|c: char| !(c.is_alphanumeric() || matches!(c, '-' | '_')))
            .unwrap_or(line.len());
        (end > 0 && line.starts_with(|c: char| c.is_alphabetic()))
            .then(|| (&line[..end], &line[end..]))
    }

    // Cost: O(l), l = length of the line.
    fn indent(&self, i: usize) -> usize {
        self.text[i].len() - self.text[i].trim_start().len()
    }

    /// The offset just past the text of line `i`, trailing blanks excluded.
    // Cost: O(l).
    fn end(&self, i: usize) -> usize {
        self.start[i] + self.text[i].trim_end().len()
    }
}

/// `head1` is a `head` of level 1.
// Cost: O(n), n = length of the name.
fn split_level(name: &str) -> (String, Option<i64>) {
    for base in ["head", "item", "numhead", "numitem"] {
        if let Some(digits) = name.strip_prefix(base)
            && !digits.is_empty()
            && let Ok(level) = digits.parse::<i64>()
        {
            return (base.to_string(), Some(level));
        }
    }
    (name.to_string(), None)
}

/// Whether `text` has a formatting code (`B<`, `X«`).
// Cost: O(n).
fn has_markup(text: &str) -> bool {
    let chars: Vec<char> = text.chars().collect();
    chars
        .windows(2)
        .any(|w| w[0].is_ascii_uppercase() && matches!(w[1], '<' | '«' | '「'))
}

/// A paragraph of plain lines from `i`: the text lines, then the blank ones.
/// `None` for a line the plain reader does not take.
// Cost: O(p), p = lines of the paragraph.
fn paragraph(lines: &Lines, mut i: usize, first: Option<&str>) -> Option<(String, usize)> {
    let mut text = String::new();
    if let Some(first) = first {
        text.push_str(first);
        text.push('\n');
    }
    while i < lines.text.len() && !lines.blank(i) && lines.directive(i).is_none() {
        if lines.indent(i) > 0 {
            return None;
        }
        text.push_str(lines.text[i]);
        text.push('\n');
        i += 1;
    }
    while i < lines.text.len() && lines.blank(i) {
        text.push('\n');
        i += 1;
    }
    (!has_markup(&text)).then_some((text, i))
}

/// The block that begins at line `i`, and the line after it.
// Cost: O(b), b = lines of the block.
fn block(lines: &Lines, i: usize) -> Option<(Block, usize)> {
    let (name, rest) = lines.directive(i)?;
    let from = lines.start[i] + lines.indent(i);
    match name {
        "end" => None,
        "begin" | "for" => {
            let rest = rest.trim_start();
            let kind = rest.split_whitespace().next()?;
            if SPECIAL.contains(&kind) {
                return None;
            }
            let config = config(lines, &rest[kind.len()..])?;
            let (base, level) = split_level(kind);
            if name == "for" {
                let (text, next) = paragraph(lines, i + 1, None)?;
                let parts = if text.is_empty() {
                    Vec::new()
                } else {
                    vec![Part::Text(text)]
                };
                let to = lines.end((i..next).rev().find(|&j| !lines.blank(j))?);
                return Some((
                    Block {
                        kind: base,
                        level,
                        form: Form::For,
                        config,
                        parts,
                        from,
                        to,
                    },
                    next,
                ));
            }
            let (parts, end) = content(lines, i + 1, kind)?;
            let to = lines.end(end);
            Some((
                Block {
                    kind: base,
                    level,
                    form: Form::Delimited,
                    config,
                    parts,
                    from,
                    to,
                },
                end + 1,
            ))
        }
        _ => {
            let (base, level) = split_level(name);
            if SPECIAL.contains(&base.as_str())
                || !rest.is_empty() && !rest.starts_with([' ', '\t'])
            {
                return None;
            }
            let first = rest.trim();
            let (text, next) = paragraph(lines, i + 1, (!first.is_empty()).then_some(first))?;
            let parts = if text.is_empty() {
                Vec::new()
            } else {
                vec![Part::Text(text)]
            };
            let to = lines.end((i..next).rev().find(|&j| !lines.blank(j))?);
            Some((
                Block {
                    kind: base,
                    level,
                    form: Form::Abbreviated,
                    config: Vec::new(),
                    parts,
                    from,
                    to,
                },
                next,
            ))
        }
    }
}

/// The `:name`, `:!name`, `:name(...)` and `:name<...>` pairs written after a
/// block's type, with the node each value is; `None` for text that is not
/// such a list.
// Cost: O(n), n = length of the text, plus the parse of each value.
fn config(lines: &Lines, text: &str) -> Option<Vec<(String, RakuAstNode)>> {
    let mut pairs = Vec::new();
    let mut rest = text.trim_start();
    while !rest.is_empty() {
        let after = rest.strip_prefix(':')?;
        let (negated, after) = match after.strip_prefix('!') {
            Some(after) => (true, after),
            None => (false, after),
        };
        let name_len = after
            .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-')))
            .unwrap_or(after.len());
        if name_len == 0 || !after.starts_with(|c: char| c.is_alphabetic()) {
            return None;
        }
        let name = &after[..name_len];
        let tail = &after[name_len..];
        let (value, used) = if let Some(inner) = tail.strip_prefix('(') {
            let len = super::regex_call_origins::args_len(inner)?;
            (None, 1 + len + 1)
        } else if let Some(inner) = tail.strip_prefix('<') {
            (None, 1 + inner.find('>')? + 1)
        } else {
            (Some(if negated { "False" } else { "True" }), 0)
        };
        let end = rest.len() - tail.len() + used;
        let pair = &rest[..end];
        let node = match value {
            // `:flag` is `True`, `:!flag` is `False`.
            Some(word) => expression(lines, word, None)?,
            None => {
                let at = pair.as_ptr() as usize - lines.base;
                let pair = expression(lines, pair, Some(at))?;
                super::infix_origins::child(&pair, "value")?.clone()
            }
        };
        pairs.push((name.to_string(), node));
        // Pairs are told apart by blanks.
        let next = &rest[end..];
        if !next.is_empty() && !next.starts_with(char::is_whitespace) {
            return None;
        }
        rest = next.trim_start();
    }
    Some(pairs)
}

/// The expression `text` is, read as a fragment of the unit at byte `at` of
/// it (or with no ranges when `at` is `None`).
// Cost: O(n), n = length of the text, plus the parse.
fn expression(lines: &Lines, text: &str, at: Option<usize>) -> Option<RakuAstNode> {
    let (mut stmts, _) = crate::parser::parse_fragment_spelled(text).ok()?;
    let _origins = match at {
        Some(at) => {
            crate::ast_visit::walk_stmts_mut(&mut super::regex_call_origins::Shift(at), &mut stmts);
            Some(super::expr_origins::strip(&mut stmts))
        }
        None => None,
    };
    let _statement = at
        .map(|at| super::expr_origins::InStatement::enter(Some(SrcSpan::new(at, at + text.len()))));
    let list = super::view::statement_list(lines.names, &stmts).ok()?;
    let [field] = list.fields.as_slice() else {
        return None;
    };
    let RakuAstFieldValue::Node(statement) = &field.value else {
        return None;
    };
    let crate::value::ValueView::RakuAst(statement) = statement.view() else {
        return None;
    };
    super::infix_origins::child(statement, "expression").cloned()
}

/// The paragraphs and nested blocks from line `i` up to the `=end` of `kind`,
/// and the line of that `=end`.
// Cost: O(b), b = lines of the block.
fn content(lines: &Lines, mut i: usize, kind: &str) -> Option<(Vec<Part>, usize)> {
    let mut parts = Vec::new();
    loop {
        while i < lines.text.len() && lines.blank(i) {
            i += 1;
        }
        if i >= lines.text.len() {
            return None;
        }
        match lines.directive(i) {
            Some(("end", rest)) => {
                return (rest.trim() == kind).then_some((parts, i));
            }
            Some(_) => {
                let (nested, next) = block(lines, i)?;
                parts.push(Part::Block(nested));
                i = next;
            }
            None => {
                let (text, next) = paragraph(lines, i, None)?;
                parts.push(Part::Text(text));
                i = next;
            }
        }
    }
}

impl Block {
    /// The `Doc::Block` node, with the origins of the block and its parts.
    // Cost: O(size of the block).
    fn node(&self) -> RakuAstNode {
        let field = |name: &'static str, value: Value| RakuAstField {
            name: Some(name),
            value: RakuAstFieldValue::Node(value),
        };
        let mut fields = vec![field("type", Value::str(self.kind.clone()))];
        if let Some(level) = self.level {
            fields.push(field("level", Value::int(level)));
        }
        match self.form {
            Form::Abbreviated => fields.push(field("abbreviated", Value::TRUE)),
            Form::For => fields.push(field("for", Value::TRUE)),
            Form::Delimited => {}
        }
        if !self.config.is_empty() {
            let pairs = self
                .config
                .iter()
                .map(|(key, node)| Value::pair(key.clone(), Value::rakuast(Box::new(node.clone()))))
                .collect();
            fields.push(RakuAstField {
                name: Some("config"),
                value: RakuAstFieldValue::List(pairs),
            });
        }
        if !self.parts.is_empty() {
            let parts = self
                .parts
                .iter()
                .map(|part| match part {
                    Part::Text(text) => Value::str(text.clone()),
                    Part::Block(block) => Value::rakuast(Box::new(block.node())),
                })
                .collect();
            fields.push(RakuAstField {
                name: Some("paragraphs"),
                value: RakuAstFieldValue::List(parts),
            });
        }
        let mut node = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::DocBlock,
            fields,
        };
        super::origin::set_span(&mut node, SrcSpan::new(self.from, self.to));
        node
    }
}

/// Put the Pod blocks of `source` between the statements of `list`, the unit's
/// `StatementList`, which was converted from `stmts`.
// Cost: O(n + s), n = length of the source, s = statements.
pub(super) fn insert(names: &NameScope, list: &mut RakuAstNode, source: &str, stmts: &[Stmt]) {
    if !source.lines().any(|l| l.trim_start().starts_with('=')) {
        return;
    }
    let covered: Vec<SrcSpan> = stmts
        .iter()
        .filter_map(|stmt| match stmt {
            Stmt::SetLine(_, span) if span.is_some() => Some(*span),
            _ => None,
        })
        .collect();
    let mut lines = Lines {
        names,
        base: source.as_ptr() as usize,
        text: Vec::new(),
        start: Vec::new(),
    };
    let mut offset = 0;
    for piece in source.split_inclusive('\n') {
        lines.start.push(offset);
        lines.text.push(piece.trim_end_matches(['\n', '\r']));
        offset += piece.len();
    }
    let inside = |at: usize| {
        covered
            .iter()
            .any(|s| (s.from as usize) <= at && at < s.to as usize)
    };
    let mut found: Vec<(usize, RakuAstNode)> = Vec::new();
    let mut last_to = 0usize;
    let mut i = 0;
    while i < lines.text.len() {
        let at = lines.start[i] + lines.indent(i);
        if lines.directive(i).is_none() || inside(at) {
            i += 1;
            continue;
        }
        match block(&lines, i) {
            Some((block, next)) => {
                // A statement the parser started inside the block is text to
                // rakudo's reader: leave the block out.
                let end = lines.start.get(next).copied().unwrap_or(source.len());
                if !covered
                    .iter()
                    .any(|s| at < s.from as usize && (s.from as usize) < end)
                {
                    last_to = last_to.max(block.to);
                    found.push((at, block.node()));
                }
                i = next.max(i + 1);
            }
            None => i += 1,
        }
    }
    if found.is_empty() {
        return;
    }
    // Each statement's offset; a statement without one leaves the list alone.
    let mut starts = Vec::with_capacity(list.fields.len());
    let mut unit_end = 0usize;
    for f in &list.fields {
        if super::origin::is_origin(f) {
            starts.push(None);
            continue;
        }
        let RakuAstFieldValue::Node(v) = &f.value else {
            return;
        };
        let crate::value::ValueView::RakuAst(child) = v.view() else {
            return;
        };
        let Some((from, to)) = byte_range(child) else {
            return;
        };
        // The statement's `;` is part of the list.
        unit_end = unit_end.max(to + usize::from(source[to..].starts_with(';')));
        starts.push(Some(from));
    }
    // The unit's list runs over the blocks too (it has no range of its own
    // when Pod surrounds the statements).
    if list.origin.0.is_none() {
        let first = found.first().map_or(usize::MAX, |(at, _)| *at);
        let first = starts.iter().flatten().copied().fold(first, usize::min);
        super::origin::set_span(list, SrcSpan::new(first, unit_end.max(last_to)));
    }
    let mut fields = Vec::with_capacity(list.fields.len() + found.len());
    let mut pending = found.into_iter().peekable();
    for (f, start) in std::mem::take(&mut list.fields).into_iter().zip(starts) {
        if let Some(start) = start {
            while let Some((_, node)) = pending.next_if(|(at, _)| *at < start) {
                fields.push(RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
                });
            }
        }
        fields.push(f);
    }
    for (_, node) in pending {
        fields.push(RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
        });
    }
    list.fields = fields;
}
