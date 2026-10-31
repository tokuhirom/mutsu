//! Origins of the type and the `is` traits of a variable declaration
//! (ADR-10723 Stage 2): `my Int $x is default(3) = 4`, `has Str:D $.s`,
//! `has $.a is rw is required`.
//!
//! The parser keeps the type and the traits as text and names, so they are
//! placed from the declaration's own range: the type is the word after the
//! scope keyword, the traits the `is NAME` clauses after the variable, each
//! read back from the source and checked against the node it places. A
//! declaration whose text does not read that way (a type with parameters, a
//! trait argument holding a quote) keeps those parts without a range: exact
//! or absent.

use crate::scope::NameScope;
use super::infix_origins::{byte_range, child, with_child};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::source_comments::trivia_len;
use crate::value::{Value, ValueView};

/// The scope keywords a declaration may start with.
const SCOPES: [&str; 6] = ["my", "our", "has", "HAS", "state", "anon"];

/// Place the type and the traits of `node`, a `VarDeclaration::Simple` with
/// a range.
// Cost: O(n + t), n = length of the declaration's source, t = its traits.
pub(super) fn derive(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(sigil) = leaf(node, "sigil") else {
        return;
    };
    let twigil = leaf(node, "twigil").unwrap_or_default();
    let Some(name) = child(node, "desigilname").and_then(super::infix_origins::leaf_text) else {
        return;
    };
    let variable = format!("{sigil}{twigil}{name}");
    let Some(parts) =
        super::unit_source::with_text(SrcSpan::new(from, to), |text| read(text, from, &variable))
    else {
        return;
    };
    if let Some(at) = parts.type_at {
        with_child(node, "type", |ty| type_origin(ty, at));
    }
    traits(names, node, &parts.traits);
}

/// What the text of a declaration says about where its parts are.
struct Parts {
    /// The type word with its smiley, and where its name ends.
    type_at: Option<(usize, usize, usize)>,
    /// Each `is` clause: all of it, its name, and its parenthesized argument.
    traits: Vec<TraitAt>,
}

#[derive(Clone, Copy)]
struct TraitAt {
    whole: (usize, usize),
    name: (usize, usize),
    argument: Option<(usize, usize)>,
}

/// Read a declaration's `text` (starting at byte `base`) whose variable is
/// spelled `variable`.
// Cost: O(n), n = length of `text`.
fn read(text: &str, base: usize, variable: &str) -> Option<Parts> {
    let scope = SCOPES
        .iter()
        .find(|s| text.starts_with(**s) && text[s.len()..].starts_with(char::is_whitespace))?;
    let mut at = scope.len();
    at += trivia_len(&text[at..])?;
    let mut type_at = None;
    if !text[at..].starts_with(variable) {
        let word = text[at..]
            .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':')))
            .unwrap_or(text.len() - at);
        let written = &text[at..at + word];
        // A smiley (`Str:D`) is part of the type, not of its name.
        let name_len = [":D", ":U", ":_"]
            .iter()
            .find_map(|smiley| written.strip_suffix(smiley))
            .filter(|stem| !stem.ends_with(':'))
            .map_or(written.len(), str::len);
        // A word ending in `:` (a stash name, a colon form) is not a type.
        if word == 0 || written.ends_with(':') {
            return None;
        }
        type_at = Some((base + at, base + at + word, base + at + name_len));
        at += word;
        at += trivia_len(&text[at..])?;
        if !text[at..].starts_with(variable) {
            return None;
        }
    }
    at += variable.len();
    let mut traits = Vec::new();
    loop {
        let gap = trivia_len(&text[at..])?;
        let rest = &text[at + gap..];
        let Some(after) = rest
            .strip_prefix("is")
            .filter(|r| r.starts_with(char::is_whitespace))
        else {
            break;
        };
        let start = at + gap;
        let name_from = start + 2 + trivia_len(after)?;
        let name_len = text[name_from..]
            .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | '\'')))
            .unwrap_or(text.len() - name_from);
        if name_len == 0 {
            return Some(Parts { type_at, traits });
        }
        let name_to = name_from + name_len;
        let mut end = name_to;
        let mut argument = None;
        if text[name_to..].starts_with('(') {
            let Some(close) = closing_paren(&text[name_to..]) else {
                return Some(Parts { type_at, traits });
            };
            end = name_to + close + 1;
            argument = Some((base + name_to, base + end));
        } else if text[name_to..].starts_with(['<', '[', '{']) {
            return Some(Parts { type_at, traits });
        }
        traits.push(TraitAt {
            whole: (base + start, base + end),
            name: (base + name_from, base + name_to),
            argument,
        });
        at = end;
    }
    Some(Parts { type_at, traits })
}

/// The offset of the `)` closing the `(` `text` starts with, when nothing
/// between them could hide a parenthesis (a quote, a backslash, a comment).
// Cost: O(n), n = length of the argument.
fn closing_paren(text: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (at, c) in text.char_indices() {
        match c {
            '"' | '\'' | '\\' | '#' | '「' | '」' | '<' | '>' => return None,
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(at);
                }
            }
            _ => {}
        }
    }
    None
}

/// Place a declaration's type node: a `Type::Simple` over its `Name`, or a
/// `Type::Definedness` (`Str:D`), whose inner `Type::Simple` Rakudo places
/// over the whole written type and its `Name` over the name alone.
// Cost: O(1).
fn type_origin(ty: &mut RakuAstNode, (from, to, name_to): (usize, usize, usize)) {
    let named = |ty: &RakuAstNode| {
        child_name(ty).is_some_and(|name| {
            super::unit_source::with_text(SrcSpan::new(from, name_to), |w| {
                (w == name).then_some(())
            })
            .is_some()
        })
    };
    match ty.class {
        RakuAstClass::TypeSimple if name_to == to && named(ty) => {
            super::origin::set_span(ty, SrcSpan::new(from, to));
            with_first(ty, |name| {
                super::origin::set_span(name, SrcSpan::new(from, to))
            });
        }
        RakuAstClass::TypeDefinedness if name_to < to => {
            let Some(inner) = super::infix_origins::child_at(ty, 0) else {
                return;
            };
            if inner.class != RakuAstClass::TypeSimple || !named(inner) {
                return;
            }
            super::origin::set_span(ty, SrcSpan::new(from, to));
            with_first(ty, |inner| {
                super::origin::set_span(inner, SrcSpan::new(from, to));
                with_first(inner, |name| {
                    super::origin::set_span(name, SrcSpan::new(from, name_to))
                });
            });
        }
        _ => {}
    }
}

/// The identifier a `Type::Simple`'s `Name` spells.
// Cost: O(d), d = length of the name.
fn child_name(ty: &RakuAstNode) -> Option<String> {
    match super::name_parts::name_shape(super::infix_origins::child_at(ty, 0)?)? {
        super::name_parts::NameShape::Identifier(name) => Some(name),
        _ => None,
    }
}

/// Give `node`'s `Trait::Is` nodes the clauses in `at`, when there is one
/// clause per trait and each names its trait.
// Cost: O(t), t = traits.
fn traits(names: &NameScope, node: &mut RakuAstNode, at: &[TraitAt]) {
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("traits")) else {
        return;
    };
    let RakuAstFieldValue::List(nodes) = &mut field.value else {
        return;
    };
    if nodes.len() != at.len() {
        return;
    }
    for (value, clause) in nodes.iter_mut().zip(at) {
        let ValueView::RakuAst(trait_node) = value.view() else {
            return;
        };
        if trait_node.class != RakuAstClass::TraitIs {
            return;
        }
        let name_ok = child(trait_node, "name")
            .and_then(super::infix_origins::leaf_text)
            .is_some_and(|name| {
                super::unit_source::with_text(SrcSpan::new(clause.name.0, clause.name.1), |w| {
                    (w == name).then_some(())
                })
                .is_some()
            });
        let has_argument = child(trait_node, "argument").is_some();
        if !name_ok || has_argument != clause.argument.is_some() {
            return;
        }
        let mut trait_node = RakuAstNode::clone(trait_node);
        super::origin::set_span(
            &mut trait_node,
            SrcSpan::new(clause.whole.0, clause.whole.1),
        );
        with_child(&mut trait_node, "name", |name| {
            super::origin::set_span(name, SrcSpan::new(clause.name.0, clause.name.1))
        });
        if let Some((from, to)) = clause.argument {
            with_child(&mut trait_node, "argument", |argument| {
                if argument.class == RakuAstClass::CircumfixParentheses {
                    super::origin::set_span(argument, SrcSpan::new(from, to));
                    super::infix_origins::derive(names, argument);
                }
            });
        }
        *value = Value::rakuast(Box::new(trait_node));
    }
}

/// The string a leaf field of `node` holds.
// Cost: O(f), f = fields of `node`.
fn leaf(node: &RakuAstNode, name: &str) -> Option<String> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) if f.name == Some(name) => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    })
}

/// Apply `f` to the first field of `node`, when it holds a node.
// Cost: O(1) plus `f`.
fn with_first(node: &mut RakuAstNode, f: impl FnOnce(&mut RakuAstNode)) {
    super::infix_origins::with_child_at(node, 0, f);
}
