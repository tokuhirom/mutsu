//! `RakuAST::Name` parts beyond plain identifiers: the empty edge of a name
//! (`::Foo`, `::($x)`, `Foo::`) and the shapes the lowerer recognises.
//!
//! Rakudo 2026.09 spells the empty edge `RakuAST::Name::Part::Empty`.
//! rakudo/rakudo#6771 renames it to `RakuAST::Name::Part::EmptyEdge`, because
//! it can only be the first or the last part of a name; both spellings are
//! accepted on the way in, and `.AST` keeps emitting `Empty` like Rakudo
//! 2026.09 does. The two positions are stored differently, as measured: the
//! leading `::` is an instance (`Empty.new`), the trailing `::` of a stash
//! lookup is the bare type object (`Empty`).

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// Whether `value` is a `RakuAST::Name::Part`: a part node, or one of the
/// empty-edge type objects.
pub(crate) fn is_name_part(value: &Value) -> bool {
    match value.view() {
        ValueView::RakuAst(node) => is_name_part_class(node.class),
        ValueView::Package(name) => is_empty_part_type_object(&name.resolve()),
        _ => false,
    }
}

pub(crate) fn is_name_part_class(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::NamePartSimple
            | RakuAstClass::NamePartExpression
            | RakuAstClass::NamePartEmpty
            | RakuAstClass::NamePartEmptyEdge
    )
}

fn is_empty_part_type_object(name: &str) -> bool {
    matches!(
        name,
        "RakuAST::Name::Part::Empty" | "RakuAST::Name::Part::EmptyEdge"
    )
}

/// Whether a name part is an empty edge, in either spelling, as a type object
/// or an instance.
fn is_empty_part(value: &Value) -> bool {
    match value.view() {
        ValueView::RakuAst(node) => matches!(
            node.class,
            RakuAstClass::NamePartEmpty | RakuAstClass::NamePartEmptyEdge
        ),
        ValueView::Package(name) => is_empty_part_type_object(&name.resolve()),
        _ => false,
    }
}

/// The identifier of a `Name::Part::Simple`, or `None` for any other part.
fn simple_part_name(value: &Value) -> Option<String> {
    let ValueView::RakuAst(node) = value.view() else {
        return None;
    };
    if node.class != RakuAstClass::NamePartSimple {
        return None;
    }
    match node.fields.first() {
        Some(RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(v),
        }) => match v.view() {
            ValueView::Str(s) if !s.is_empty() => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    }
}

/// The leading `::` of a name: an `Empty` instance.
pub(super) fn leading_empty() -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::NamePartEmpty,
        fields: Vec::new(),
    }))
}

/// The trailing `::` of a stash lookup: the `Empty` type object itself.
pub(super) fn trailing_empty() -> Value {
    Value::package(crate::symbol::Symbol::intern(
        RakuAstClass::NamePartEmpty.printed_name(),
    ))
}

pub(super) fn simple_part(name: &str) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::NamePartSimple,
        fields: vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(Value::str(name.to_string())),
        }],
    }))
}

/// A `RakuAST::Name` over an explicit part list.
pub(super) fn name_from_parts(parts: Vec<Value>) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![RakuAstField {
            name: Some("parts"),
            value: RakuAstFieldValue::List(parts),
        }],
    }
}

/// Put the leading empty part in front of the parts of the `name` field in
/// `fields`: `class ::D` is `Name.new(Empty.new, Simple("D"))`. A field that is
/// not a plain name (or no field at all) is left alone.
// Cost: O(k), k = number of parts of the name.
pub(super) fn with_leading_empty(fields: &mut [RakuAstField]) {
    let Some(field) = fields.iter_mut().find(|f| f.name == Some("name")) else {
        return;
    };
    let RakuAstFieldValue::Node(value) = &field.value else {
        return;
    };
    let ValueView::RakuAst(name) = value.view() else {
        return;
    };
    let mut parts = match name.fields.first() {
        Some(RakuAstField {
            name: Some("parts"),
            value: RakuAstFieldValue::List(parts),
        }) => parts.clone(),
        Some(RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(v),
        }) => match v.view() {
            ValueView::Str(s) => identifier_segments(&s).map(simple_part).collect(),
            _ => return,
        },
        _ => return,
    };
    parts.insert(0, leading_empty());
    field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(name_from_parts(parts))));
}

/// Whether a `RakuAST::Name` starts with the empty edge (`::Foo`).
pub(super) fn has_leading_empty(node: &RakuAstNode) -> bool {
    matches!(
        node.fields.first(),
        Some(RakuAstField {
            name: Some("parts"),
            value: RakuAstFieldValue::List(parts),
        }) if parts.first().is_some_and(is_empty_part)
    )
}

/// The `::`-separated segments of a qualified identifier (`A::B` -> `A`, `B`).
///
/// The one place the RakuAST layer splits a name's text. The layer converts a
/// parsed program once, like the parser that produced the text; it never runs
/// per execution. The split itself is memoized per symbol
/// ([`crate::qualified::segments`]).
pub(super) fn identifier_segments(
    name: &str,
) -> impl Iterator<Item = &'static str> + Clone + use<> {
    crate::qualified::segments(crate::symbol::Symbol::intern(name))
        .iter()
        .map(|seg| seg.as_str())
}

/// Whether `name` is a `::`-qualified identifier (`A::B`, `Foo::v`): two or
/// more segments, every one of them an identifier. A name that merely
/// contains `::`, such as the operator name `infix:<::=>`, is not.
pub(super) fn is_qualified_identifier(name: &str) -> bool {
    let mut segments = identifier_segments(name);
    segments.clone().nth(1).is_some()
        && segments.all(|seg| {
            !seg.is_empty()
                && seg
                    .chars()
                    .all(|ch| ch.is_alphanumeric() || matches!(ch, '_' | '-' | '\''))
        })
}

/// The `Name` of a qualified identifier, one simple part per segment.
pub(super) fn qualified_name(name: &str) -> RakuAstNode {
    name_from_parts(identifier_segments(name).map(simple_part).collect())
}

/// The simple parts of an indirect name's static tail (`"::A::B"` -> `A`,
/// `B`); an empty tail has none.
pub(super) fn tail_parts(tail: &str) -> impl Iterator<Item = Value> + '_ {
    identifier_segments(tail)
        .filter(|seg| !seg.is_empty())
        .map(simple_part)
}

/// The package a stash lookup names, in the parser's spelling: `Foo::Bar::`
/// -> `Foo::Bar`, `::` -> the empty string. `None` when `stash` is not a stash
/// lookup at all.
pub(super) fn stash_stem(stash: &str) -> Option<&'static str> {
    crate::qualified::stash_stem(crate::symbol::Symbol::intern(stash)).map(|stem| stem.as_str())
}

/// The `Name` of a stash lookup on `stem` (see [`stash_stem`]): its identifier
/// parts followed by the trailing empty edge. The root stash `::` has no
/// identifiers, so it is both edges at once: `Name.new(Empty.new, Empty)`,
/// measured.
pub(super) fn stash_name(stem: &str) -> Option<RakuAstNode> {
    let mut parts = Vec::new();
    if stem.is_empty() {
        parts.push(leading_empty());
    } else {
        for segment in identifier_segments(stem) {
            if segment.is_empty() {
                return None;
            }
            parts.push(simple_part(segment));
        }
    }
    parts.push(trailing_empty());
    Some(name_from_parts(parts))
}

/// Whether `segment` names a pseudo-package, which Rakudo resolves at parse
/// time (so `MY::` renders as a `Term::Name`, measured on 2026.09).
pub(super) fn is_pseudo_package(segment: &str) -> bool {
    matches!(
        segment,
        "MY" | "OUR"
            | "GLOBAL"
            | "PROCESS"
            | "CORE"
            | "SETTING"
            | "UNIT"
            | "OUTER"
            | "OUTERS"
            | "CALLER"
            | "CALLERS"
            | "DYNAMIC"
            | "LEXICAL"
            | "CLIENT"
            | "EXPORT"
    )
}

/// What a `RakuAST::Name` means to the lowerer.
pub(super) enum NameShape<'a> {
    /// A static identifier, `::`-joined (`Foo`, `A::B`, and `::Foo` alike).
    Identifier(String),
    /// A stash lookup in the parser's spelling (`Foo::`, `::`).
    Stash(String),
    /// A dynamic `::(EXPR)` lookup, optionally followed by static segments
    /// (`::(EXPR)::A::B`) and a trailing `::`.
    Indirect {
        expr: &'a RakuAstNode,
        tail: Vec<String>,
        trailing: bool,
    },
}

/// The operator categories whose declared name carries its symbol as an
/// adverb: `infix:<foo>` is `Name.from-identifier("infix", colonpairs =>
/// (QuotedString<words val>("foo"),))`, measured on 2026.09.
const OPERATOR_CATEGORIES: [&str; 7] = [
    "prefix",
    "infix",
    "postfix",
    "circumfix",
    "postcircumfix",
    "term",
    "trait_mod",
];

/// A declared operator name in its `category:<symbol>` spelling (optionally
/// package-qualified, `Ops::infix:<pk>`) as a `Name` whose adverb is the
/// symbol's word-list `QuotedString`. `None` for any other spelling
/// (`infix:["x"]`, `infix:sym<x>`, a symbol containing `<`/`>`), which stays
/// one identifier string.
// Cost: O(n), n = length of the name.
pub(super) fn operator_name(name: &str) -> Option<RakuAstNode> {
    let (head, rest) = name.split_once(":<")?;
    let symbol = rest.strip_suffix('>')?;
    if symbol.is_empty() || symbol.chars().any(|c| matches!(c, '<' | '>' | '\\')) {
        return None;
    }
    let segments: Vec<&str> = identifier_segments(head).collect();
    let (category, qualifiers) = segments.split_last()?;
    if !OPERATOR_CATEGORIES.contains(category) || qualifiers.iter().any(|q| q.is_empty()) {
        return None;
    }
    let mut fields = if qualifiers.is_empty() {
        vec![super::convert::leaf_field(
            None,
            Value::str((*category).to_string()),
        )]
    } else {
        let parts = segments.iter().map(|seg| simple_part(seg)).collect();
        return Some(RakuAstNode {
            class: RakuAstClass::Name,
            fields: vec![
                RakuAstField {
                    name: Some("parts"),
                    value: RakuAstFieldValue::List(parts),
                },
                operator_colonpairs(symbol),
            ],
        });
    };
    fields.push(operator_colonpairs(symbol));
    Some(RakuAstNode {
        class: RakuAstClass::Name,
        fields,
    })
}

fn operator_colonpairs(symbol: &str) -> RakuAstField {
    RakuAstField {
        name: Some("colonpairs"),
        value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(
            super::convert::word_quote(symbol),
        ))]),
    }
}

/// The symbol of a name's single operator adverb (the inverse of
/// [`operator_name`]), or `None` when the name has no such adverb.
// Cost: O(1).
fn operator_symbol(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|f| f.name == Some("colonpairs"))?;
    let RakuAstFieldValue::List(pairs) = &field.value else {
        return None;
    };
    let [pair] = pairs.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(quoted) = pair.view() else {
        return None;
    };
    if quoted.class != RakuAstClass::QuotedString {
        return None;
    }
    let RakuAstFieldValue::List(segments) = &quoted
        .fields
        .iter()
        .find(|f| f.name == Some("segments"))?
        .value
    else {
        return None;
    };
    let [segment] = segments.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(segment) = segment.view() else {
        return None;
    };
    let RakuAstFieldValue::Node(text) = &segment.fields.first()?.value else {
        return None;
    };
    match text.view() {
        ValueView::Str(s) => Some(s.to_string()),
        _ => None,
    }
}

/// Classify a `RakuAST::Name` node, or `None` for a shape mutsu cannot lower.
pub(super) fn name_shape(node: &RakuAstNode) -> Option<NameShape<'_>> {
    let shape = base_name_shape(node)?;
    match (shape, operator_symbol(node)) {
        (NameShape::Identifier(name), Some(symbol)) => {
            Some(NameShape::Identifier(format!("{name}:<{symbol}>")))
        }
        (shape, _) => Some(shape),
    }
}

fn base_name_shape(node: &RakuAstNode) -> Option<NameShape<'_>> {
    if node.class != RakuAstClass::Name {
        return None;
    }
    let field = node.fields.first()?;
    let parts = match (&field.name, &field.value) {
        (None, RakuAstFieldValue::Node(v)) => {
            return match v.view() {
                ValueView::Str(s) => Some(NameShape::Identifier(s.to_string())),
                _ => None,
            };
        }
        (Some("parts"), RakuAstFieldValue::List(parts)) => parts,
        _ => return None,
    };
    let (leading, rest) = match parts.split_first() {
        Some((first, rest)) if is_empty_part(first) => (true, rest),
        _ => (false, parts.as_slice()),
    };
    let (trailing, middle) = match rest.split_last() {
        Some((last, middle)) if is_empty_part(last) => (true, middle),
        _ => (false, rest),
    };
    if leading
        && let Some((first, static_tail)) = middle.split_first()
        && let ValueView::RakuAst(part) = first.view()
        && part.class == RakuAstClass::NamePartExpression
    {
        let RakuAstFieldValue::Node(expr) = &part.fields.first()?.value else {
            return None;
        };
        let ValueView::RakuAst(expr) = expr.view() else {
            return None;
        };
        let tail = static_tail
            .iter()
            .map(simple_part_name)
            .collect::<Option<Vec<_>>>()?;
        return Some(NameShape::Indirect {
            expr,
            tail,
            trailing,
        });
    }
    let names = middle
        .iter()
        .map(simple_part_name)
        .collect::<Option<Vec<_>>>()?;
    match (leading, trailing, names.is_empty()) {
        // `::` alone: both edges, no identifier.
        (true, true, true) => Some(NameShape::Stash("::".to_string())),
        (false, true, false) => Some(NameShape::Stash(format!("{}::", names.join("::")))),
        (_, false, false) => Some(NameShape::Identifier(names.join("::"))),
        _ => None,
    }
}
