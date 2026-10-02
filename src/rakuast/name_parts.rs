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

/// The `::`-separated segments of a qualified identifier (`A::B` -> `A`, `B`).
///
/// The one place the RakuAST layer splits a name's text. The layer converts a
/// parsed program once, like the parser that produced the text; it never runs
/// per execution.
pub(super) fn identifier_segments(name: &str) -> std::str::Split<'_, &'static str> {
    name.split("::")
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
pub(super) fn stash_stem(stash: &str) -> Option<&str> {
    stash.strip_suffix("::")
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

/// Classify a `RakuAST::Name` node, or `None` for a shape mutsu cannot lower.
pub(super) fn name_shape(node: &RakuAstNode) -> Option<NameShape<'_>> {
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
