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
use crate::value::{Value, ValueView};

/// One `is NAME` / `does NAME` / `hides NAME` clause of a header.
struct Clause {
    class: RakuAstClass,
    name: String,
    whole: SrcSpan,
    name_at: SrcSpan,
}

/// Give the traits of `decl` (a `Class`, `Grammar`, `Role`, ...) the ranges
/// of the clauses written in `header`, the text between its name and its
/// block.
// Cost: O(n + t^2), n = length of the header, t = traits.
pub(super) fn apply(decl: &mut RakuAstNode, header: SrcSpan) {
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
    ) || node.fields.len() != 1
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
    loop {
        let Some(gap) = trivia_len(&text[at..]) else {
            break;
        };
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
        if name.is_empty() || name.ends_with(':') || text[name_to..].starts_with(['[', '(', '<']) {
            break;
        }
        out.push(Clause {
            class,
            name: name.to_string(),
            whole: SrcSpan::new(base + at, base + name_to),
            name_at: SrcSpan::new(base + name_from, base + name_to),
        });
        at = name_to;
    }
    out
}
