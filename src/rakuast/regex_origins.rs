//! Origins of regex nodes (ADR-10723 Stage 2): the body of a quoted regex
//! (`/ a b+ /`, `rx/.../`, `m:i/.../`) and of a `token` / `rule` / `regex`
//! declaration.
//!
//! The regex source tree (ADR-0088) keeps no positions, so the converted
//! nodes are laid over the regex's text in source order: each atom must be
//! spelled at the cursor (a literal its text, `\d` its class, `^` its
//! anchor), a group sits between its brackets, a quantifier follows its atom,
//! and a `WithWhitespace` runs over the whitespace after its atom, as Rakudo's
//! does. The first node whose spelling is not at the cursor, or whose kind is
//! not read here, ends the walk: it and every node after it stay without a
//! range. Exact or absent.

use super::infix_origins::{byte_range, leaf_text};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// Place the body of `node`, a `QuotedRegex` with a range, and its adverbs.
// Cost: O(n), n = length of the regex's source.
pub(super) fn quoted(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(text) = super::unit_source::with_text(SrcSpan::new(from, to), |t| Some(t.to_string()))
    else {
        return;
    };
    let Some(Opener {
        adverbs,
        body_at: open,
        close,
    }) = opener(&text)
    else {
        return;
    };
    if !text.ends_with(close) || open + close.len_utf8() > text.len() {
        return;
    }
    let body = SrcSpan::new(from + open, to - close.len_utf8());
    place_adverbs(node, &adverbs);
    super::infix_origins::with_child(node, "body", |regex| walk_body(names, regex, body));
}

/// Place the body of a `token` / `rule` / `regex` declaration written at its
/// range: the text between the braces after its name.
// Cost: O(n), n = length of the declaration's source.
pub(super) fn declaration(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(name) = super::infix_origins::child(node, "name").and_then(leaf_text) else {
        return;
    };
    let body = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let word = ["token", "rule", "regex"]
            .into_iter()
            .find_map(|k| text.find(k).map(|at| at + k.len()))?;
        let rest = &text[word..];
        let name_at = from + word + (rest.len() - rest.trim_start().len());
        let after_name = rest.trim_start().strip_prefix(name.as_str())?;
        if after_name.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-') {
            return None;
        }
        let brace = after_name.trim_start();
        if !brace.starts_with('{') || !text.ends_with('}') {
            return None;
        }
        Some((
            SrcSpan::new(name_at, name_at + name.len()),
            SrcSpan::new(to - brace.len() + 1, to - 1),
        ))
    });
    if let Some((name_at, body)) = body {
        super::infix_origins::with_child(node, "name", |n| super::origin::set_span(n, name_at));
        super::infix_origins::with_child(node, "body", |regex| walk_body(names, regex, body));
    }
}

/// Lay `regex` over the body text at `body`.
// Cost: O(n), n = length of the body.
fn walk_body(names: &NameScope, regex: &mut RakuAstNode, body: SrcSpan) {
    let Some(text) = super::unit_source::with_text(body, |t| Some(t.to_string())) else {
        return;
    };
    let mut walk = Walk {
        text: &text,
        base: body.from as usize,
        at: 0,
    };
    walk.blank();
    walk.place(names, regex);
}

/// What a quoted regex's text says before its body.
struct Opener {
    /// The `:name` adverbs, as offsets into the text.
    adverbs: Vec<(usize, usize)>,
    /// Where the body starts.
    body_at: usize,
    /// The delimiter that closes the body.
    close: char,
}

/// A quoted regex's adverbs (`:i`) and where its body starts and what closes
/// it, read from its `text`.
// Cost: O(n), n = length of the prefix.
fn opener(text: &str) -> Option<Opener> {
    let mut at = ["rx", "ms", "m"]
        .into_iter()
        .find(|p| text.starts_with(p) && !text[p.len()..].starts_with(char::is_alphanumeric))
        .map_or(0, str::len);
    let mut adverbs = Vec::new();
    while text[at..].starts_with(':') {
        let len = text[at + 1..]
            .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-'))
            .unwrap_or(text.len() - at - 1);
        if len == 0 {
            return None;
        }
        adverbs.push((at, at + 1 + len));
        at += 1 + len;
    }
    at += text[at..].len() - text[at..].trim_start().len();
    let open = text[at..].chars().next()?;
    let close = match open {
        '/' => '/',
        '{' => '}',
        '[' => ']',
        '(' => ')',
        '<' => '>',
        '!' | '|' => open,
        _ => return None,
    };
    Some(Opener {
        adverbs,
        body_at: at + open.len_utf8(),
        close,
    })
}

/// Give a quoted regex's `:name` adverbs the ranges read off its prefix.
// Cost: O(a), a = adverbs.
fn place_adverbs(node: &mut RakuAstNode, written: &[(usize, usize)]) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("adverbs")) else {
        return;
    };
    let RakuAstFieldValue::List(items) = &mut field.value else {
        return;
    };
    if items.len() != written.len() {
        return;
    }
    for (item, &(a, b)) in items.iter_mut().zip(written) {
        let ValueView::RakuAst(pair) = item.view() else {
            return;
        };
        let spelled = pair.class == RakuAstClass::ColonPairTrue
            && leaf_text(pair).is_some_and(|name| {
                super::unit_source::with_text(SrcSpan::new(from + a + 1, from + b), |t| {
                    (t == name).then_some(())
                })
                .is_some()
            });
        if !spelled {
            return;
        }
        let mut pair = RakuAstNode::clone(pair);
        super::origin::set_span(&mut pair, SrcSpan::new(from + a, from + b));
        *item = Value::rakuast(Box::new(pair));
    }
}

/// The cursor over a regex body.
struct Walk<'a> {
    text: &'a str,
    /// The unit offset of the body's first byte.
    base: usize,
    /// The cursor, relative to the body.
    at: usize,
}

impl Walk<'_> {
    /// Skip whitespace.
    // Cost: O(w), w = whitespace skipped.
    fn blank(&mut self) {
        let rest = &self.text[self.at..];
        self.at += rest.len() - rest.trim_start().len();
    }

    /// Consume `spelling` when it is at the cursor.
    // Cost: O(s), s = length of `spelling`.
    fn eat(&mut self, spelling: &str) -> Option<()> {
        self.text[self.at..]
            .starts_with(spelling)
            .then(|| self.at += spelling.len())
    }

    /// Lay `node`, which starts at the cursor, over the text, giving it and
    /// the nodes in it their ranges; `None` ends the walk.
    // Cost: O(n), n = length of the node's text.
    fn place(&mut self, names: &NameScope, node: &mut RakuAstNode) -> Option<()> {
        let start = self.at;
        match node.class {
            RakuAstClass::RegexLiteral => self.literal(&leaf_text(node)?)?,
            RakuAstClass::RegexQuote => self.quote(node)?,
            RakuAstClass::RegexSequence => {
                let count = node.fields.len();
                for i in 0..count {
                    if i > 0 {
                        self.blank();
                    }
                    self.child_at(names, node, i)?;
                }
            }
            RakuAstClass::RegexAlternation | RakuAstClass::RegexSequentialAlternation => {
                let separator = if node.class == RakuAstClass::RegexAlternation {
                    "|"
                } else {
                    "||"
                };
                let count = node.fields.len();
                for i in 0..count {
                    if i > 0 {
                        self.blank();
                        self.eat(separator)?;
                        if self.text[self.at..].starts_with('|') {
                            return None;
                        }
                        self.blank();
                    }
                    self.child_at(names, node, i)?;
                }
            }
            RakuAstClass::RegexWithWhitespace => {
                self.child_at(names, node, 0)?;
                self.blank();
                // A comment would be part of the whitespace.
                if self.text[self.at..].starts_with('#') {
                    return None;
                }
            }
            RakuAstClass::RegexGroup => self.bracketed(names, node, "[", "]")?,
            RakuAstClass::RegexCapturingGroup => self.bracketed(names, node, "(", ")")?,
            RakuAstClass::RegexQuantifiedAtom => self.quantified(names, node)?,
            RakuAstClass::RegexCharClass(_) => {
                self.eat(&super::regex_char_class::written(node)?)?
            }
            RakuAstClass::RegexAnchorBeginningOfString => self.eat("^")?,
            RakuAstClass::RegexAnchorEndOfString => self.eat("$")?,
            RakuAstClass::RegexAnchorBeginningOfLine => self.eat("^^")?,
            RakuAstClass::RegexAnchorEndOfLine => self.eat("$$")?,
            RakuAstClass::RegexNamedCapture => self.named_capture(names, node)?,
            RakuAstClass::RegexAssertionCharClass => self.char_class(node)?,
            RakuAstClass::RegexAssertionNamed => self.named_assertion(node)?,
            _ => return None,
        }
        // `^` must not have been the first half of `^^`, a literal the start
        // of a longer word.
        if matches!(
            node.class,
            RakuAstClass::RegexAnchorBeginningOfString | RakuAstClass::RegexAnchorEndOfString
        ) && self.text[self.at..].starts_with(['^', '$'])
        {
            return None;
        }
        super::origin::set_span(node, SrcSpan::new(self.base + start, self.base + self.at));
        Some(())
    }

    /// Place the `index`-th field of `node`, a node.
    // Cost: O(n), n = length of the child's text.
    fn child_at(&mut self, names: &NameScope, node: &mut RakuAstNode, index: usize) -> Option<()> {
        let field = node.fields.get_mut(index)?;
        self.held(names, &mut field.value)
    }

    /// Place the node held by the field `name` of `node`.
    // Cost: O(n), n = length of the child's text.
    fn child(&mut self, names: &NameScope, node: &mut RakuAstNode, name: &str) -> Option<()> {
        let field = node.fields.iter_mut().find(|f| f.name == Some(name))?;
        self.held(names, &mut field.value)
    }

    // Cost: O(n), n = length of the node's text.
    fn held(&mut self, names: &NameScope, value: &mut RakuAstFieldValue) -> Option<()> {
        let RakuAstFieldValue::Node(held) = value else {
            return None;
        };
        let ValueView::RakuAst(inner) = held.view() else {
            return None;
        };
        let mut inner = RakuAstNode::clone(inner);
        let placed = self.place(names, &mut inner);
        *held = Value::rakuast(Box::new(inner));
        placed
    }

    /// `[ ... ]` / `( ... )`.
    // Cost: O(n), n = length of the group's text.
    fn bracketed(
        &mut self,
        names: &NameScope,
        node: &mut RakuAstNode,
        open: &str,
        close: &str,
    ) -> Option<()> {
        self.eat(open)?;
        self.blank();
        self.child_at(names, node, 0)?;
        self.blank();
        self.eat(close)
    }

    /// `atom` then its quantifier (`*`, `+`, `?`, with `?` / `!` / `:` after
    /// it), then a `% separator`.
    // Cost: O(n), n = length of the quantified text.
    fn quantified(&mut self, names: &NameScope, node: &mut RakuAstNode) -> Option<()> {
        // `b ** 1..3`: the atom of a spaced quantifier is a `WithWhitespace`
        // that Rakudo places over the atom alone.
        if super::infix_origins::child(node, "atom")
            .is_some_and(|atom| atom.class == RakuAstClass::RegexWithWhitespace)
        {
            let field = node.fields.iter_mut().find(|f| f.name == Some("atom"))?;
            let RakuAstFieldValue::Node(held) = &mut field.value else {
                return None;
            };
            let ValueView::RakuAst(spaced) = held.view() else {
                return None;
            };
            let mut spaced = RakuAstNode::clone(spaced);
            let start = self.at;
            let placed = self.child_at(names, &mut spaced, 0);
            if placed.is_some() {
                super::origin::set_span(
                    &mut spaced,
                    SrcSpan::new(self.base + start, self.base + self.at),
                );
            }
            *held = Value::rakuast(Box::new(spaced));
            placed?;
            self.blank();
        } else {
            self.child(names, node, "atom")?;
        }
        let quantifier = super::infix_origins::child(node, "quantifier")?;
        let lowered = super::regex_quantifier::lower_quantifier(names, quantifier)?;
        let range;
        let spelling = match lowered.kind {
            crate::regex_tree::QuantifierKind::ZeroOrMore => "*",
            crate::regex_tree::QuantifierKind::OneOrMore => "+",
            crate::regex_tree::QuantifierKind::ZeroOrOne => "?",
            crate::regex_tree::QuantifierKind::Range {
                min: Some(min),
                max,
                excludes_min: false,
                excludes_max: false,
            } => {
                range = self.range_spelling(min, max)?;
                range.as_str()
            }
            _ => return None,
        };
        let backtrack = match lowered.backtrack {
            None => "",
            Some(crate::regex_tree::RegexBacktrack::Frugal) => "?",
            Some(crate::regex_tree::RegexBacktrack::Greedy) => "!",
            Some(crate::regex_tree::RegexBacktrack::Ratchet) => ":",
        };
        let quantifier_at = self.at;
        self.eat(spelling)?;
        self.eat(backtrack)?;
        let quantifier_span = SrcSpan::new(self.base + quantifier_at, self.base + self.at);
        super::infix_origins::with_child(node, "quantifier", |q| {
            super::origin::set_span(q, quantifier_span)
        });
        if super::infix_origins::child(node, "separator").is_some() {
            self.blank();
            let trailing = super::regex_quantifier::trailing_separator(node);
            self.eat(if trailing { "%%" } else { "%" })?;
            if self.text[self.at..].starts_with('%') {
                return None;
            }
            self.blank();
            self.child(names, node, "separator")?;
        }
        Some(())
    }

    /// How a `** min..max` quantifier at the cursor is written, when it is
    /// written with these bounds: `**2`, `** 1..3`, `**2..*`.
    // Cost: O(n), n = length of the quantifier.
    fn range_spelling(&self, min: u64, max: Option<u64>) -> Option<String> {
        let rest = self.text[self.at..].strip_prefix("**")?;
        let gap = rest.len() - rest.trim_start().len();
        let bounds = match max {
            Some(max) if max == min && !rest[gap..].starts_with(&format!("{min}..")) => {
                min.to_string()
            }
            Some(max) => format!("{min}..{max}"),
            None => format!("{min}..*"),
        };
        rest[gap..]
            .starts_with(&bounds)
            .then(|| format!("**{}{bounds}", &rest[..gap]))
    }

    /// A literal: its text, or a single escaped punctuation character
    /// (`\.` for `.`).
    // Cost: O(s), s = length of the literal.
    fn literal(&mut self, text: &str) -> Option<()> {
        if self.eat(text).is_some() {
            return Some(());
        }
        let mut chars = text.chars();
        let (Some(c), None) = (chars.next(), chars.next()) else {
            return None;
        };
        if c.is_alphanumeric() || c.is_whitespace() {
            return None;
        }
        self.eat(&format!("\\{c}"))
    }

    /// `<name>` / `<.name>`: the `Name` is the identifier inside.
    // Cost: O(n), n = length of the assertion.
    fn named_assertion(&mut self, node: &mut RakuAstNode) -> Option<()> {
        if node
            .fields
            .iter()
            .any(|f| !matches!(f.name, Some("name" | "capturing")))
        {
            return None;
        }
        let capturing = node.fields.iter().any(|f| f.name == Some("capturing"));
        let name_node = super::infix_origins::child(node, "name")?;
        let name = match super::name_parts::name_shape(name_node)? {
            super::name_parts::NameShape::Identifier(name) => name,
            _ => return None,
        };
        self.eat(if capturing { "<" } else { "<." })?;
        let name_at = self.at;
        self.eat(&name)?;
        self.eat(">")?;
        let at = SrcSpan::new(self.base + name_at, self.base + name_at + name.len());
        super::infix_origins::with_child(node, "name", |n| super::origin::set_span(n, at));
        Some(())
    }

    /// `$<name>=atom` / `@<name>=atom`.
    // Cost: O(n), n = length of the capture's text.
    fn named_capture(&mut self, names: &NameScope, node: &mut RakuAstNode) -> Option<()> {
        let name = node.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name == Some("name") => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        })?;
        let flagged = |flag: &str| node.fields.iter().any(|f| f.name == Some(flag));
        let sigil = if flagged("array") {
            "@"
        } else if flagged("hash") {
            "%"
        } else {
            "$"
        };
        self.eat(&format!("{sigil}<{name}>="))?;
        self.blank();
        self.child(names, node, "regex")
    }

    /// `<[a..z]>`, `<-[x y]>`, `<[a]+[b]-[c]>`: each enumeration runs from
    /// its sign (none on an unsigned first one) to its `]`.
    // Cost: O(n), n = length of the class's text.
    fn char_class(&mut self, node: &mut RakuAstNode) -> Option<()> {
        self.eat("<")?;
        for i in 0..node.fields.len() {
            let start = self.at;
            let RakuAstFieldValue::Node(held) = &mut node.fields[i].value else {
                return None;
            };
            let ValueView::RakuAst(element) = held.view() else {
                return None;
            };
            if element.class != RakuAstClass::RegexCharClassElementEnumeration {
                return None;
            }
            let negated = element.fields.iter().any(|f| {
                f.name == Some("negated")
                    && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
            });
            if negated {
                self.eat("-")?;
            } else if self.eat("+").is_none() && i > 0 {
                return None;
            }
            let mut element = RakuAstNode::clone(element);
            let placed = self.enumeration(&mut element);
            if placed.is_some() {
                super::origin::set_span(
                    &mut element,
                    SrcSpan::new(self.base + start, self.base + self.at),
                );
            }
            *held = Value::rakuast(Box::new(element));
            placed?;
        }
        self.eat(">")
    }

    /// `[ a b..c ]`: each `Character` and `Range` element at its spelling.
    // Cost: O(n), n = length of the enumeration's text.
    fn enumeration(&mut self, node: &mut RakuAstNode) -> Option<()> {
        self.eat("[")?;
        let field = node
            .fields
            .iter_mut()
            .find(|f| f.name == Some("elements"))?;
        let RakuAstFieldValue::List(items) = &mut field.value else {
            return None;
        };
        for item in items.iter_mut() {
            self.blank();
            let ValueView::RakuAst(element) = item.view() else {
                return None;
            };
            let start = self.at;
            let mut element = RakuAstNode::clone(element);
            let plain = |c: char| !matches!(c, ']' | '\\' | '[' | '.' | '-' | '+' | '<' | '>');
            match element.class {
                RakuAstClass::RegexCharClassEnumerationElementCharacter => {
                    let c = leaf_text(&element)?;
                    if c.chars().count() != 1 || !c.chars().all(plain) {
                        return None;
                    }
                    self.eat(&c)?;
                }
                RakuAstClass::RegexCharClassEnumerationElementRange => {
                    let bound = |name: &str| {
                        element.fields.iter().find_map(|f| match &f.value {
                            RakuAstFieldValue::Node(v) if f.name == Some(name) => match v.view() {
                                ValueView::Int(cp) => {
                                    u32::try_from(cp).ok().and_then(char::from_u32)
                                }
                                _ => None,
                            },
                            _ => None,
                        })
                    };
                    let (from, to) = (bound("from")?, bound("to")?);
                    if !plain(from) || !plain(to) {
                        return None;
                    }
                    self.eat(&from.to_string())?;
                    self.blank();
                    self.eat("..")?;
                    self.blank();
                    self.eat(&to.to_string())?;
                }
                _ => return None,
            }
            super::origin::set_span(
                &mut element,
                SrcSpan::new(self.base + start, self.base + self.at),
            );
            *item = Value::rakuast(Box::new(element));
        }
        self.blank();
        self.eat("]")
    }

    /// `'...'` / `"..."` without escapes: the `Quote`, its `QuotedString` and
    /// the `StrLiteral` inside.
    // Cost: O(n), n = length of the quote.
    fn quote(&mut self, node: &mut RakuAstNode) -> Option<()> {
        let delimiter = self.text[self.at..]
            .chars()
            .next()
            .filter(|c| matches!(c, '\'' | '"'))?;
        let start = self.at;
        let inner = &self.text[start + 1..];
        let len = inner.find(delimiter)?;
        let written = &inner[..len];
        if written.contains('\\')
            || (delimiter == '"' && written.contains(['$', '@', '%', '&', '{']))
        {
            return None;
        }
        let quoted = super::infix_origins::child_at(node, 0)?;
        let segments = quoted.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::List(items) if f.name == Some("segments") => Some(items),
            _ => None,
        })?;
        let [only] = segments.as_slice() else {
            return None;
        };
        let ValueView::RakuAst(literal) = only.view() else {
            return None;
        };
        if literal.class != RakuAstClass::StrLiteral || leaf_text(literal)? != written {
            return None;
        }
        self.at = start + 1 + len + 1;
        let whole = SrcSpan::new(self.base + start, self.base + self.at);
        super::infix_origins::with_child_at(node, 0, |quoted| {
            super::origin::set_span(quoted, whole);
            super::origin::quoted_text(quoted, whole);
        });
        Some(())
    }
}
