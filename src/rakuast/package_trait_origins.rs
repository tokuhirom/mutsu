//! Origins of the traits written in a package declaration's header (ADR-10723
//! Stage 2): `class A is B does R is rw hides C { ... }`.
//!
//! The parser folds the header's clauses into parents, roles and flags, so
//! the converted `Trait::Is` / `Trait::Does` / `Trait::Hides` nodes cannot
//! say where they were written. The header text, between the package's name
//! and its `{`, still can: it is read clause by clause, and a trait node takes
//! the range of the clause of its kind that names what it names, the k-th
//! node of a kind and name taking the k-th such clause, and only when the
//! numbers agree. A clause with an argument (`is Foo[Int]`, `is export(:a)`)
//! ends the reading, so it and the clauses after it stay without a range:
//! exact or absent.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// One `is NAME` / `does NAME` / `hides NAME` clause of a header.
struct Clause {
    class: RakuAstClass,
    name: String,
    whole: SrcSpan,
    name_at: SrcSpan,
    /// The parenthesized argument (`(1, 2)` of `is listarg(1, 2)`).
    argument: Option<SrcSpan>,
}

/// Give the traits of `decl` (a `Class`, `Grammar`, `Role`, ...) the ranges
/// of the clauses written in `header`, the text between its name and its
/// block.
// Cost: O(n + t^2), n = length of the header, t = traits.
pub(super) fn apply(names: &NameScope, decl: &mut RakuAstNode, header: SrcSpan) {
    let Some(clauses) =
        super::unit_source::with_text(header, |text| Some(clauses(text, header.from as usize)))
    else {
        return;
    };
    if clauses.is_empty() {
        return;
    }
    let Some(field) = decl.fields.iter_mut().find(|f| f.name == Some("traits")) else {
        return;
    };
    let RakuAstFieldValue::List(traits) = &mut field.value else {
        return;
    };
    let keys: Vec<Option<(RakuAstClass, String)>> = traits
        .iter()
        .map(|value| match value.view() {
            ValueView::RakuAst(node) => trait_key(node),
            _ => None,
        })
        .collect();
    for (i, value) in traits.iter_mut().enumerate() {
        let Some((class, name)) = &keys[i] else {
            continue;
        };
        let same = |k: &Option<(RakuAstClass, String)>| {
            k.as_ref().is_some_and(|(c, n)| c == class && n == name)
        };
        let nodes = keys.iter().filter(|k| same(k)).count();
        let written: Vec<&Clause> = clauses
            .iter()
            .filter(|c| c.class == *class && c.name == *name)
            .collect();
        if written.len() != nodes {
            continue;
        }
        let k = keys[..i].iter().filter(|k| same(k)).count();
        let clause = written[k];
        let ValueView::RakuAst(node) = value.view() else {
            continue;
        };
        let mut node = RakuAstNode::clone(node);
        super::origin::set_span(&mut node, clause.whole);
        if let Some(first) = node.fields.first_mut()
            && let RakuAstFieldValue::Node(held) = &mut first.value
            && let ValueView::RakuAst(named) = held.view()
        {
            let mut named = RakuAstNode::clone(named);
            super::routine_origins::type_origin(&mut named, clause.name_at);
            *held = Value::rakuast(Box::new(named));
        }
        if let Some(parens) = clause.argument {
            super::infix_origins::with_child(&mut node, "argument", |argument| {
                parenthesized_argument(names, argument, parens)
            });
        }
        *value = Value::rakuast(Box::new(node));
    }
}

/// The kind of a trait node and the name it names (`Trait::Is` of `rw`, of the
/// type `Array`).
// Cost: O(d), d = length of the name.
fn trait_key(node: &RakuAstNode) -> Option<(RakuAstClass, String)> {
    if !matches!(
        node.class,
        RakuAstClass::TraitIs | RakuAstClass::TraitDoes | RakuAstClass::TraitHides
    ) || !matches!(node.fields.len(), 1 | 2)
        || node
            .fields
            .get(1)
            .is_some_and(|f| f.name != Some("argument"))
    {
        return None;
    }
    let RakuAstFieldValue::Node(held) = &node.fields[0].value else {
        return None;
    };
    let ValueView::RakuAst(named) = held.view() else {
        return None;
    };
    let name = match named.class {
        RakuAstClass::Name => named,
        RakuAstClass::TypeSimple => super::infix_origins::child_at(named, 0)?,
        // `does R[Int:D]`: the role the parameterization applies to.
        RakuAstClass::TypeParameterized => {
            super::infix_origins::child_at(super::infix_origins::child_at(named, 0)?, 0)?
        }
        _ => return None,
    };
    match super::name_parts::name_shape(name)? {
        super::name_parts::NameShape::Identifier(name) => Some((node.class, name)),
        _ => None,
    }
}

/// The clauses written in a header `text` that starts at byte `base`, up to
/// the first one this reading does not cover.
// Cost: O(n), n = length of `text`.
fn clauses(text: &str, base: usize) -> Vec<Clause> {
    use crate::source_comments::trivia_len;
    let mut out = Vec::new();
    let mut at = 0;
    while let Some(gap) = trivia_len(&text[at..]) {
        at += gap;
        let rest = &text[at..];
        let Some((keyword, class)) = [
            ("is", RakuAstClass::TraitIs),
            ("does", RakuAstClass::TraitDoes),
            ("hides", RakuAstClass::TraitHides),
        ]
        .into_iter()
        .find(|(k, _)| rest.starts_with(k) && rest[k.len()..].starts_with(char::is_whitespace)) else {
            break;
        };
        let after = &rest[keyword.len()..];
        let Some(gap) = trivia_len(after) else {
            break;
        };
        let name_from = at + keyword.len() + gap;
        let name_len = text[name_from..]
            .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':')))
            .unwrap_or(text.len() - name_from);
        let name_to = name_from + name_len;
        let name = &text[name_from..name_to];
        // `does R[Int:D]`: a parameterized role, its brackets part of the
        // type the clause names.
        let brackets = match text[name_to..].strip_prefix('[') {
            Some(inner) => match super::subst_origins::closing(inner, '[', ']') {
                Some(close) => close + 2,
                None => break,
            },
            None => 0,
        };
        if name.is_empty()
            || name.ends_with(':')
            || text[name_to + brackets..].starts_with(['[', '<'])
        {
            break;
        }
        let name_to = name_to + brackets;
        let mut end = name_to;
        let mut argument = None;
        if text[name_to..].starts_with('(') {
            // `is repr('CStruct')`: the representation, a quoted name, which
            // the package absorbs; the clauses after it are read on.
            let close = if name == "repr" {
                super::subst_origins::closing(&text[name_to + 1..], '(', ')').map(|c| c + 1)
            } else {
                closing_paren(&text[name_to..])
            };
            let Some(close) = close else {
                break;
            };
            end = name_to + close + 1;
            argument = Some(SrcSpan::new(base + name_to, base + end));
        }
        out.push(Clause {
            class,
            name: name.to_string(),
            whole: SrcSpan::new(base + at, base + end),
            name_at: SrcSpan::new(base + name_from, base + name_to),
            argument,
        });
        at = end;
    }
    out
}

/// Start the body `Block` of `decl` (a package with traits) at its first
/// trait, as rakudo does; `block` is the range the parser gave the block.
// Cost: O(t), t = traits.
pub(super) fn block_from_first_trait(decl: &mut RakuAstNode, block: SrcSpan, header: SrcSpan) {
    let listed = decl
        .fields
        .iter()
        .find(|f| f.name == Some("traits"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(traits) => traits.first().and_then(|v| match v.view() {
                ValueView::RakuAst(node) => super::infix_origins::byte_range(node),
                _ => None,
            }),
            _ => None,
        });
    // `class T is repr('CStruct') { }`: a trait the package absorbs (its
    // representation) is not listed, yet the block starts at it too.
    let absorbed = super::unit_source::with_text(header, |text| {
        let written = text.trim_start();
        let at = header.from as usize + (text.len() - written.len());
        written
            .strip_prefix("is")
            .is_some_and(|rest| rest.trim_start().starts_with("repr("))
            .then_some((at, at))
    });
    let first = match (listed, absorbed) {
        (Some(a), Some(b)) => Some(a.min(b)),
        (a, b) => a.or(b),
    };
    let Some((from, _)) = first else {
        return;
    };
    if from >= block.from as usize {
        return;
    }
    super::infix_origins::with_child(decl, "body", |body| {
        body.origin.0 = None;
        super::origin::set_span(body, SrcSpan::new(from, block.to as usize));
    });
}

/// Place the adverbs of a package's `name` (`E:ver<1.2>:auth<zef:x>`),
/// written right after its first part, which ends at byte `at`: each
/// `ColonPair::Value` over `:key<words>`, its word quote over the brackets
/// and the quote's text inside them. The `Name` then runs to the last
/// adverb, as rakudo's does. The end of the last adverb placed, or `at`
/// when there is none (only `<...>` values are read: exact or absent).
// Cost: O(n), n = length of the adverbs' text.
pub(super) fn name_adverbs(name: &mut RakuAstNode, at: usize) -> usize {
    let Some(field) = name
        .fields
        .iter_mut()
        .find(|f| f.name == Some("colonpairs"))
    else {
        return at;
    };
    let RakuAstFieldValue::List(pairs) = &mut field.value else {
        return at;
    };
    let mut cursor = at;
    for value in pairs.iter_mut() {
        let ValueView::RakuAst(pair) = value.view() else {
            return at;
        };
        if pair.class != RakuAstClass::ColonPairValue {
            return at;
        }
        let Some(key) = pair.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name == Some("key") => v.as_str().map(str::to_string),
            _ => None,
        }) else {
            return at;
        };
        let placed = super::unit_source::with_text_from(cursor, |text| {
            let rest = text.strip_prefix(':')?.strip_prefix(key.as_str())?;
            let inner = rest.strip_prefix('<')?;
            let close = inner.find(['>', '<', '\n'])?;
            inner[close..]
                .starts_with('>')
                .then_some((key.len() + 2, close))
        });
        let Some((open, len)) = placed else {
            return at;
        };
        let mut pair = RakuAstNode::clone(pair);
        let end = cursor + open + len + 1;
        super::origin::set_span(&mut pair, SrcSpan::new(cursor, end));
        super::infix_origins::with_child(&mut pair, "value", |quote| {
            // The quote is the brackets, its text what is inside them.
            super::origin::set_span(quote, SrcSpan::new(cursor + open - 1, end));
            super::origin::word_quote_inner(quote, SrcSpan::new(cursor + open, end - 1));
        });
        *value = Value::rakuast(Box::new(pair));
        cursor = end;
    }
    cursor
}

/// The offset of the `)` that closes the `(` `text` starts with; `None` when
/// a quote, comment or escape comes first.
// Cost: O(n), n = length of the argument.
fn closing_paren(text: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (at, c) in text.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return (c == ')').then_some(at);
                }
            }
            '"' | '\'' | '#' | '\\' | '<' => return None,
            _ => {}
        }
    }
    None
}

/// Place a trait's `Circumfix::Parentheses` argument written at `parens`:
/// its `SemiList` and statements, from a parse of the text inside.
// Cost: O(n), n = length of the argument, plus the parse of it.
fn parenthesized_argument(names: &NameScope, argument: &mut RakuAstNode, parens: SrcSpan) {
    if argument.class != RakuAstClass::CircumfixParentheses {
        return;
    }
    let (from, to) = (parens.from as usize, parens.to as usize);
    let Some(inner) =
        super::unit_source::with_text(SrcSpan::new(from + 1, to - 1), |t| Some(t.to_string()))
    else {
        return;
    };
    super::origin::set_span(argument, parens);
    super::infix_origins::with_child_at(argument, 0, |list| {
        super::fragment_origins::statements(names, list, &inner, from + 1);
        super::list_origins::statement_list(list, SrcSpan::new(from + 1, to - 1));
    });
}

/// Place the term of a `handles` trait written at `whole` (`handles <a b>`,
/// `handles *`): the text after the keyword. A word quote's text sits
/// between its brackets.
// Cost: O(n), n = length of the trait.
pub(super) fn handles_term(trait_node: &mut RakuAstNode, whole: SrcSpan) {
    let at = super::unit_source::with_text(whole, |text| {
        let rest = text.strip_prefix("handles")?;
        let term = rest.trim_start();
        (term.len() < rest.len() && !term.is_empty())
            .then(|| whole.from as usize + text.len() - term.len())
    });
    let Some(at) = at else {
        return;
    };
    let term = SrcSpan::new(at, whole.to as usize);
    super::infix_origins::with_child_at(trait_node, 0, |held| match held.class {
        RakuAstClass::TermWhatever => super::origin::set_span(held, term),
        RakuAstClass::QuotedString => {
            let Some((open, close)) =
                super::unit_source::with_text(term, super::origin::word_brackets)
            else {
                return;
            };
            super::origin::set_span(held, term);
            super::origin::word_quote_inner(
                held,
                SrcSpan::new(at + open, whole.to as usize - close),
            );
        }
        _ => {}
    });
}
