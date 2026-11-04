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
//! The verbatim types (`code`, `input`, `output`, `comment`) are read; the
//! tabular ones (`table`, `defn`, ...) and a block a statement starts inside
//! are left out as before. Exact or absent.

use super::infix_origins::byte_range;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{SrcSpan, Stmt};
use crate::scope::NameScope;
use crate::value::Value;

/// The block types whose paragraphs are not plain text.
const SPECIAL: &[&str] = &[
    "table", "defn", "config", "formula", "row", "column", "cell", "nested", "toc", "semantic",
    "alias",
];

/// The block types whose delimited content is one verbatim string (directives
/// and blank lines inside it included).
const VERBATIM: &[&str] = &["code", "input", "output", "comment"];

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
    Para(Vec<Atom>),
    Block(Block),
}

struct Block {
    kind: String,
    level: Option<i64>,
    form: Form,
    /// The indentation an implicit code block was written with.
    margin: Option<String>,
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

/// A formatting code of a paragraph: `B<bold>`.
struct Markup {
    letter: char,
    opener: &'static str,
    closer: &'static str,
    atoms: Vec<Atom>,
    /// The text after the first `|` of a code that takes metadata (`L`, `X`, ...).
    meta: Option<String>,
    /// The pairs of an entity code (`E<gt>`), which has no atoms of its own.
    entities: Option<Vec<(String, String)>>,
}

/// The formatting codes that take metadata after a `|`.
const META_LETTERS: &str = "ADFLMPX";

enum Atom {
    Text(String),
    Markup(Markup),
}

/// The atoms of `text` up to `closer` (or its end), the metadata after a `|`
/// when `meta_ok`, and the length consumed with the closer. `None` for text
/// the plain reader does not take: an `E<...>`, an unterminated or ambiguous
/// code.
// Cost: O(n), n = length of the text.
fn parse_atoms(
    text: &str,
    closer: Option<&str>,
    meta_ok: bool,
) -> Option<(Vec<Atom>, usize, Option<String>)> {
    let mut meta = None;
    let mut atoms = Vec::new();
    let mut buf = String::new();
    let mut at = 0;
    let mut previous: Option<char> = None;
    loop {
        let rest = &text[at..];
        let Some(ch) = rest.chars().next() else {
            if closer.is_some() {
                return None;
            }
            break;
        };
        if let Some(closer) = closer
            && rest.starts_with(closer)
        {
            at += closer.len();
            break;
        }
        if let Some(closer) = closer
            && meta_ok
            && ch == '|'
        {
            let tail = &rest[1..];
            let end = tail.find(closer)?;
            let text = &tail[..end];
            if text.chars().any(|c| matches!(c, '<' | '>' | '«' | '»')) {
                return None;
            }
            meta = Some(text.to_string());
            at += 1 + end + closer.len();
            break;
        }
        if ch.is_ascii_uppercase() {
            let after = &rest[1..];
            let opener = ["<<", "<", "«"].into_iter().find(|o| after.starts_with(o));
            if let Some(opener) = opener {
                // `E<...>` is an entity, and a letter after a word character
                // is not told apart from its text here.
                if previous.is_some_and(char::is_alphanumeric) {
                    return None;
                }
                let closer = match opener {
                    "<<" => ">>",
                    "<" => ">",
                    _ => "»",
                };
                if !buf.is_empty() {
                    atoms.push(Atom::Text(std::mem::take(&mut buf)));
                }
                if ch == 'E' {
                    let body = &after[opener.len()..];
                    let end = body.find(closer)?;
                    let content = &body[..end];
                    if content.chars().any(|c| matches!(c, '<' | '>' | '«' | '»')) {
                        return None;
                    }
                    let (shown, list) = match content.split_once('|') {
                        Some((shown, list)) => (shown, super::pod_entities::pairs_after_bar(list)),
                        None => ("", super::pod_entities::pairs(content)),
                    };
                    atoms.push(Atom::Markup(Markup {
                        letter: 'E',
                        opener: if opener == "<<" {
                            "<<"
                        } else if opener == "<" {
                            "<"
                        } else {
                            "«"
                        },
                        closer: if closer == ">>" {
                            ">>"
                        } else if closer == ">" {
                            ">"
                        } else {
                            "»"
                        },
                        atoms: if shown.is_empty() {
                            Vec::new()
                        } else {
                            vec![Atom::Text(shown.to_string())]
                        },
                        meta: None,
                        entities: Some(list),
                    }));
                    at += 1 + opener.len() + end + closer.len();
                    previous = closer.chars().last();
                    continue;
                }
                let (inner, used, inner_meta) = parse_atoms(
                    &after[opener.len()..],
                    Some(closer),
                    META_LETTERS.contains(ch),
                )?;
                if inner.is_empty() && inner_meta.is_none() {
                    return None;
                }
                atoms.push(Atom::Markup(Markup {
                    letter: ch,
                    opener: if opener == "<<" {
                        "<<"
                    } else if opener == "<" {
                        "<"
                    } else {
                        "«"
                    },
                    closer: if closer == ">>" {
                        ">>"
                    } else if closer == ">" {
                        ">"
                    } else {
                        "»"
                    },
                    atoms: inner,
                    meta: inner_meta,
                    entities: None,
                }));
                at += 1 + opener.len() + used;
                previous = Some(closer.chars().last()?);
                continue;
            }
        }
        if closer.is_some() && matches!(ch, '<' | '>' | '«' | '»') {
            return None;
        }
        buf.push(ch);
        previous = Some(ch);
        at += ch.len_utf8();
    }
    if !buf.is_empty() {
        atoms.push(Atom::Text(buf));
    }
    Some((atoms, at, meta))
}

/// The part a paragraph's `text` is: the string itself, or a
/// `Doc::Paragraph` of its atoms when it holds a formatting code.
// Cost: O(n), n = length of the text.
fn part_of(text: String) -> Option<Part> {
    part_in("", text)
}

/// [`part_of`] for a block of type `kind`: a `comment` is never read for
/// formatting codes.
// Cost: O(n), n = length of the text.
fn part_in(kind: &str, text: String) -> Option<Part> {
    if kind == "comment" {
        return Some(Part::Text(text));
    }
    if !text.chars().any(|c| matches!(c, '<' | '«')) {
        return Some(Part::Text(text));
    }
    let (atoms, _, _) = parse_atoms(&text, None, false)?;
    Some(if atoms.iter().any(|a| matches!(a, Atom::Markup(_))) {
        Part::Para(atoms)
    } else {
        Part::Text(text)
    })
}

/// A paragraph of plain lines from `i`: the text lines, then the blank ones.
/// `None` for a line the plain reader does not take.
// Cost: O(p), p = lines of the paragraph.
fn paragraph(
    lines: &Lines,
    mut i: usize,
    first: Option<&str>,
    inside_delimited: bool,
) -> Option<(String, usize)> {
    let mut text = String::new();
    if let Some(first) = first {
        text.push_str(first);
        text.push('\n');
    }
    while i < lines.text.len() && !lines.blank(i) && lines.directive(i).is_none() {
        // In a delimited block a later line of a paragraph may be indented (its
        // blanks are dropped); elsewhere that is not read.
        if lines.indent(i) > 0 && !inside_delimited {
            return None;
        }
        text.push_str(if inside_delimited {
            lines.text[i].trim_start()
        } else {
            lines.text[i]
        });
        text.push('\n');
        i += 1;
    }
    while i < lines.text.len() && lines.blank(i) {
        text.push('\n');
        i += 1;
    }
    Some((text, i))
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
                let (text, next) = paragraph(lines, i + 1, None, false)?;
                let parts = if text.is_empty() {
                    Vec::new()
                } else {
                    vec![part_in(&base, text)?]
                };
                let to = lines.end((i..next).rev().find(|&j| !lines.blank(j))?);
                return Some((
                    Block {
                        kind: base,
                        level,
                        form: Form::For,
                        margin: None,
                        config,
                        parts,
                        from,
                        to,
                    },
                    next,
                ));
            }
            let (parts, end) = if VERBATIM.contains(&kind) {
                verbatim(lines, i + 1, kind)?
            } else {
                content(lines, i + 1, kind)?
            };
            let to = lines.end(end);
            Some((
                Block {
                    kind: base,
                    level,
                    form: Form::Delimited,
                    margin: None,
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
            let (text, next) =
                paragraph(lines, i + 1, (!first.is_empty()).then_some(first), false)?;
            let parts = if text.is_empty() {
                Vec::new()
            } else {
                vec![part_in(&base, text)?]
            };
            let to = lines.end((i..next).rev().find(|&j| !lines.blank(j))?);
            Some((
                Block {
                    kind: base,
                    level,
                    form: Form::Abbreviated,
                    margin: None,
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

/// The implicit code block of the indented lines from `i`: the margin is the
/// first line's indentation, a deeper line keeps what is beyond it, and a
/// blank run belongs to the block when more of it follows. `None` for
/// indentation made of other characters than the margin's.
// Cost: O(b), b = lines of the block.
fn code(lines: &Lines, i: usize) -> Option<(Block, usize)> {
    let first = lines.text[i];
    let margin = &first[..first.len() - first.trim_start().len()];
    let mut text = String::new();
    let (mut j, mut last) = (i, i);
    while j < lines.text.len() {
        if lines.blank(j) {
            let mut k = j;
            while k < lines.text.len() && lines.blank(k) {
                k += 1;
            }
            text.push_str(&"\n".repeat(k - j));
            let more = k < lines.text.len()
                && lines.directive(k).is_none()
                && lines.text[k].starts_with(margin);
            j = k;
            if more {
                continue;
            }
            break;
        }
        if lines.directive(j).is_some() {
            break;
        }
        let line = lines.text[j];
        match line.strip_prefix(margin) {
            Some(rest) => text.push_str(rest),
            None if lines.indent(j) >= margin.len() => return None,
            None => break,
        }
        text.push('\n');
        last = j;
        j += 1;
    }
    let block = Block {
        kind: "implicit-code".to_string(),
        level: None,
        form: Form::Delimited,
        margin: Some(margin.to_string()),
        config: Vec::new(),
        parts: vec![Part::Text(text)],
        from: lines.start[i],
        to: lines.end(last),
    };
    Some((block, j))
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
            None if lines.indent(i) > 0 => {
                let (code, next) = code(lines, i)?;
                parts.push(Part::Block(code));
                i = next;
            }
            None => {
                let (text, next) = paragraph(lines, i, None, true)?;
                parts.push(part_of(text)?);
                i = next;
            }
        }
    }
}

/// The content of a verbatim block from line `i`: every line up to its
/// `=end` as one string, without the blank lines before the first.
/// `None` when the block is not closed.
// Cost: O(b), b = lines of the block.
fn verbatim(lines: &Lines, mut i: usize, kind: &str) -> Option<(Vec<Part>, usize)> {
    while i < lines.text.len() && lines.blank(i) {
        i += 1;
    }
    let mut text = String::new();
    loop {
        if i >= lines.text.len() {
            return None;
        }
        if let Some(("end", rest)) = lines.directive(i) {
            if rest.trim() != kind {
                return None;
            }
            break;
        }
        text.push_str(lines.text[i]);
        text.push('\n');
        i += 1;
    }
    let parts = if text.is_empty() {
        Vec::new()
    } else {
        vec![part_in(kind, text)?]
    };
    Some((parts, i))
}

/// The `Doc::Paragraph` of `atoms`.
// Cost: O(size of the atoms).
fn paragraph_node(atoms: &[Atom]) -> RakuAstNode {
    let fields = atoms
        .iter()
        .map(|atom| RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(match atom {
                Atom::Text(text) => Value::str(text.clone()),
                Atom::Markup(markup) => Value::rakuast(Box::new(markup_node(markup))),
            }),
        })
        .collect();
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::DocParagraph,
        fields,
    }
}

/// The `Doc::Markup` of a formatting code.
// Cost: O(size of the code).
fn markup_node(markup: &Markup) -> RakuAstNode {
    let field = |name: &'static str, value: Value| RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    };
    let atoms = markup
        .atoms
        .iter()
        .map(|atom| match atom {
            Atom::Text(text) => Value::str(text.clone()),
            Atom::Markup(inner) => Value::rakuast(Box::new(markup_node(inner))),
        })
        .collect::<Vec<_>>();
    let mut fields = vec![
        field("letter", Value::str(markup.letter.to_string())),
        field("opener", Value::str(markup.opener.to_string())),
        field("closer", Value::str(markup.closer.to_string())),
    ];
    if !atoms.is_empty() {
        fields.push(RakuAstField {
            name: Some("atoms"),
            value: RakuAstFieldValue::List(atoms),
        });
    }
    if let Some(meta) = &markup.meta {
        fields.push(RakuAstField {
            name: Some("meta"),
            value: RakuAstFieldValue::List(vec![Value::str(meta.clone())]),
        });
    }
    if let Some(entities) = markup.entities.as_ref().filter(|e| !e.is_empty()) {
        fields.push(RakuAstField {
            name: Some("meta"),
            value: RakuAstFieldValue::List(
                entities
                    .iter()
                    .map(|(key, value)| Value::pair(key.clone(), Value::str(value.clone())))
                    .collect(),
            ),
        });
    }
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::DocMarkup,
        fields,
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
        let mut fields = Vec::new();
        if let Some(margin) = &self.margin {
            fields.push(field("margin", Value::str(margin.clone())));
        }
        fields.push(field("type", Value::str(self.kind.clone())));
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
                    Part::Para(atoms) => Value::rakuast(Box::new(paragraph_node(atoms))),
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
        // Rakudo gives an implicit code block no range.
        if self.margin.is_none() {
            super::origin::set_span(&mut node, SrcSpan::new(self.from, self.to));
        }
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
