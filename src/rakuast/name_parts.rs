//! `RakuAST::Name` parts beyond plain identifiers: the empty edge of a name
//! (`::Foo`, `::($x)`, `Foo::`) and the shapes the lowerer recognises.
//!
//! The empty edge is rakudo 2026.11's `RakuAST::Name::Part::EmptyEdge`
//! (rakudo/rakudo#6771 renamed it from 2026.09's `Name::Part::Empty`, which
//! mutsu no longer accepts, #18977), because it can only be the first or the
//! last part of a name. Both edges are `EmptyEdge` instances; a hand-built
//! name may still hold the bare type object, which is accepted too.

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

fn is_name_part_class(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::NamePartSimple
            | RakuAstClass::NamePartExpression
            | RakuAstClass::NamePartEmptyEdge
    )
}

fn is_empty_part_type_object(name: &str) -> bool {
    name == "RakuAST::Name::Part::EmptyEdge"
}

/// Whether a name part is an empty edge, as a type object or an instance.
pub(super) fn is_empty_part(value: &Value) -> bool {
    match value.view() {
        ValueView::RakuAst(node) => matches!(
            node.class,
            RakuAstClass::NamePartEmptyEdge
        ),
        ValueView::Package(name) => is_empty_part_type_object(&name.resolve()),
        _ => false,
    }
}

/// The identifier of a `Name::Part::Simple`, or `None` for any other part.
pub(super) fn part_identifier(value: &Value) -> Option<String> {
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

/// The leading `::` of a name: an `EmptyEdge` instance.
pub(super) fn leading_empty() -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::NamePartEmptyEdge,
        fields: Vec::new(),
    }))
}

/// The anonymous name `::` of a declaration (`subset :: where ...`):
/// `Name(EmptyEdge, EmptyEdge)`.
// Cost: O(1).
pub(super) fn anonymous_name() -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields: vec![RakuAstField {
            name: Some("parts"),
            value: RakuAstFieldValue::List(vec![leading_empty(), trailing_empty()]),
        }],
    }
}

/// The trailing `::` of a stash lookup: an `EmptyEdge` instance too (rakudo
/// 2026.11; 2026.09 stored the bare type object here).
pub(super) fn trailing_empty() -> Value {
    leading_empty()
}

pub(super) fn simple_part(name: &str) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::NamePartSimple,
        fields: vec![RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(Value::str(name.to_string())),
        }],
    }))
}

/// A `RakuAST::Name` over an explicit part list.
pub(crate) fn name_from_parts(parts: Vec<Value>) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
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
    // The `name` field, or a lone positional one (`Type::Simple`'s name).
    let at = fields
        .iter()
        .position(|f| f.name == Some("name"))
        .or_else(|| (fields.len() == 1 && fields[0].name.is_none()).then_some(0));
    let Some(field) = at.map(|at| &mut fields[at]) else {
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

/// `::Array`, `::Int:D`: the view of a type term written after a leading
/// `::`, its name (or its smiley's base type's) starting with the empty edge.
// Cost: O(k), k = number of parts of the name.
pub(super) fn leading_colons(mut node: RakuAstNode) -> RakuAstNode {
    match node.class {
        RakuAstClass::TypeSimple | RakuAstClass::TermName => with_leading_empty(&mut node.fields),
        RakuAstClass::TypeDefinedness => {
            super::infix_origins::with_child(&mut node, "base-type", |base| {
                with_leading_empty(&mut base.fields)
            })
        }
        _ => {}
    }
    node
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
    segments.clone().nth(1).is_some() && segments.all(crate::parser::is_raku_identifier)
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
        vec![super::view::leaf_field(
            None,
            Value::str((*category).to_string()),
        )]
    } else {
        let parts = segments.iter().map(|seg| simple_part(seg)).collect();
        return Some(RakuAstNode {
            origin: Default::default(),
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
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields,
    })
}

/// The `Name` of a routine or term name: an operator's (`term:<x>`) with its
/// word quote, any other one from its identifier.
// Cost: O(n), n = length of the name.
pub(super) fn operator_or_identifier(name: &str) -> RakuAstNode {
    operator_name(name).unwrap_or_else(|| super::view::name_from_identifier(name))
}

/// A qualified name used as a term: `A::B`, or a package-qualified operator
/// name (`M::term:<ab>`).
// Cost: O(n), n = length of the name.
pub(super) fn is_qualified_term(name: &str) -> bool {
    is_qualified_identifier(name)
        || (crate::qualified::is_qualified_str(name) && operator_name(name).is_some())
}

/// The `Name` of an operator routine written with a quote form the canonical
/// name does not keep (`postcircumfix:«<< >>»`, `infix:<< a >>`): the written
/// colonpair is viewed as the quote it was, with its delimiters and spaces.
/// `None` when the written text is not an operator name of a known category.
// Cost: O(n), n = length of the written name.
pub(super) fn written_operator_name(
    names: &crate::scope::NameScope,
    written: &str,
) -> Option<RakuAstNode> {
    let (head, pairs) = written.split_once(':')?;
    let mut node = operator_name(&format!("{head}:<x>"))?;
    let exprs = crate::parser::name_colonpair_exprs(&format!(":{pairs}"))?;
    let views = exprs
        .iter()
        .map(|e| super::view::view_expr(names, e).map(|n| Value::rakuast(Box::new(n))))
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    let field = node
        .fields
        .iter_mut()
        .find(|f| f.name == Some("colonpairs"))?;
    field.value = RakuAstFieldValue::List(views);
    Some(node)
}

/// Put the written operator name of a routine declaration (see
/// [`written_operator_name`]) on its `name` field, when the parse kept one.
// Cost: O(n), n = length of the written name.
pub(super) fn apply_written_name(
    names: &crate::scope::NameScope,
    node: &mut RakuAstNode,
    spans: Option<&crate::ast::parsed::RoutineSpans>,
) {
    if let Some(written) = spans.and_then(|s| s.name_written.as_deref())
        && let Some(name) = written_operator_name(names, written)
        && let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("name"))
    {
        *field = super::view::node_field(Some("name"), name);
    }
}

/// A method's written operator name (see [`apply_written_name`]), then its
/// origins.
// Cost: O(n), n = length of the written name plus the signature's size.
pub(super) fn apply_method_name(
    names: &crate::scope::NameScope,
    node: &mut RakuAstNode,
    spans: Option<&crate::ast::parsed::RoutineSpans>,
    param_defs: &[crate::ast::ParamDef],
) {
    apply_written_name(names, node, spans);
    super::routine_origins::apply(node, spans, param_defs);
}

/// `RakuAST::Name.new(@parts, :colonpairs)`: the shape [`operator_name`]
/// builds, a lone simple part as the category leaf (#18441).
// Cost: O(p + c), p = parts, c = colonpairs.
pub(super) fn name_with_colonpairs(parts: Vec<Value>, colonpairs: Vec<Value>) -> RakuAstNode {
    let lone_simple = match parts.as_slice() {
        [part] => match part.view() {
            ValueView::RakuAst(node) if node.class == RakuAstClass::NamePartSimple => {
                super::infix_origins::leaf_text(node)
            }
            _ => None,
        },
        _ => None,
    };
    let head = match lone_simple {
        Some(category) => super::view::leaf_field(None, Value::str(category)),
        None => RakuAstField {
            name: Some("parts"),
            value: RakuAstFieldValue::List(parts),
        },
    };
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields: vec![
            head,
            RakuAstField {
                name: Some("colonpairs"),
                value: RakuAstFieldValue::List(colonpairs),
            },
        ],
    }
}

fn operator_colonpairs(symbol: &str) -> RakuAstField {
    RakuAstField {
        name: Some("colonpairs"),
        value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(super::view::word_quote(
            symbol,
        )))]),
    }
}

/// `alt:sym<foo>` / `alt:sym«a b»` (a grammar alternative's or a method's
/// `:sym` adverb, written as the flat name mutsu keeps) as the `Name` rakudo
/// builds: the stem with one `ColonPair::Value(key => "sym", value => word
/// quote)`. `None` for any other spelling, which stays one identifier string.
// Cost: O(n), n = length of the name.
pub(super) fn sym_name(name: &str) -> Option<RakuAstNode> {
    let (stem, rest) = name.split_once(":sym")?;
    if stem.is_empty() || !crate::parser::is_raku_identifier(stem) {
        return None;
    }
    let angle = rest.strip_prefix('<').and_then(|r| r.strip_suffix('>'));
    let french = rest
        .strip_prefix('\u{00AB}')
        .and_then(|r| r.strip_suffix('\u{00BB}'));
    let (quotewords, text) = match (angle, french) {
        (Some(inner), _) => (false, inner),
        (None, Some(inner)) => (true, inner),
        (None, None) => return None,
    };
    // `:sym«<foo>»`: angle brackets are plain text inside French quotes.
    let delimiter = |c: char| match quotewords {
        true => matches!(c, '\u{00AB}' | '\u{00BB}'),
        false => matches!(c, '<' | '>' | '\u{00AB}' | '\u{00BB}'),
    };
    if text.is_empty() || text.chars().any(|c| delimiter(c) || c == '\\') {
        return None;
    }
    let pair = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairValue,
        fields: vec![
            super::view::leaf_field(Some("key"), Value::str("sym".to_string())),
            super::view::node_field(
                Some("value"),
                super::view::word_quote_with(quotewords, true, text),
            ),
        ],
    };
    Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Name,
        fields: vec![
            super::view::leaf_field(None, Value::str(stem.to_string())),
            RakuAstField {
                name: Some("colonpairs"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(pair))]),
            },
        ],
    })
}

/// The flat `stem:sym<text>` / `stem:sym«text»` spelling of a name whose one
/// colonpair is a `:sym` value (the inverse of [`sym_name`]).
// Cost: O(1).
fn sym_suffix(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|f| f.name == Some("colonpairs"))?;
    let RakuAstFieldValue::List(pairs) = &field.value else {
        return None;
    };
    let [pair] = pairs.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(pair) = pair.view() else {
        return None;
    };
    if pair.class != RakuAstClass::ColonPairValue {
        return None;
    }
    let key = pair.fields.iter().find(|f| f.name == Some("key"))?;
    let RakuAstFieldValue::Node(key) = &key.value else {
        return None;
    };
    if key.as_str() != Some("sym") {
        return None;
    }
    let value = pair.fields.iter().find(|f| f.name == Some("value"))?;
    let RakuAstFieldValue::Node(value) = &value.value else {
        return None;
    };
    let ValueView::RakuAst(value) = value.view() else {
        return None;
    };
    let text = quoted_text(value)?;
    let processors = value.fields.iter().find(|f| f.name == Some("processors"))?;
    let RakuAstFieldValue::List(processors) = &processors.value else {
        return None;
    };
    match processors.first()?.as_str()? {
        "words" => Some(format!(":sym<{text}>")),
        "quotewords" => Some(format!(":sym\u{00AB}{text}\u{00BB}")),
        _ => None,
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
    // The parser trims the written symbol (`infix:<< foo >>` is `infix:<foo>`).
    quoted_text(quoted).map(|symbol| symbol.trim().to_string())
}

/// The text of a `QuotedString` of one plain segment.
// Cost: O(1).
pub(super) fn quoted_text(quoted: &RakuAstNode) -> Option<String> {
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

/// A `Name` of static parts followed by one `Part::Expression`
/// (`MY::(EXPR)`): the `::`-joined package and the key node.
// Cost: O(p), p = parts of the name.
pub(super) fn package_and_expression(node: &RakuAstNode) -> Option<(String, &RakuAstNode)> {
    if node.class != RakuAstClass::Name {
        return None;
    }
    let field = node.fields.first()?;
    let (Some("parts"), RakuAstFieldValue::List(parts)) = (&field.name, &field.value) else {
        return None;
    };
    let (last, package) = parts.split_last()?;
    let ValueView::RakuAst(last) = last.view() else {
        return None;
    };
    if last.class != RakuAstClass::NamePartExpression || package.is_empty() {
        return None;
    }
    let RakuAstFieldValue::Node(key) = &last.fields.first()?.value else {
        return None;
    };
    let ValueView::RakuAst(key) = key.view() else {
        return None;
    };
    let package = package
        .iter()
        .map(part_identifier)
        .collect::<Option<Vec<_>>>()?;
    Some((package.join("::"), key))
}

/// Classify a `RakuAST::Name` node, or `None` for a shape mutsu cannot lower.
pub(super) fn name_shape(node: &RakuAstNode) -> Option<NameShape<'_>> {
    let shape = base_name_shape(node)?;
    match (shape, operator_symbol(node)) {
        (NameShape::Identifier(name), Some(symbol)) => {
            Some(NameShape::Identifier(format!("{name}:<{symbol}>")))
        }
        (NameShape::Identifier(name), None) => match sym_suffix(node) {
            Some(suffix) => Some(NameShape::Identifier(format!("{name}{suffix}"))),
            None => Some(NameShape::Identifier(name)),
        },
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
            .map(part_identifier)
            .collect::<Option<Vec<_>>>()?;
        return Some(NameShape::Indirect {
            expr,
            tail,
            trailing,
        });
    }
    let names = middle
        .iter()
        .map(part_identifier)
        .collect::<Option<Vec<_>>>()?;
    match (leading, trailing, names.is_empty()) {
        // `::` alone: both edges, no identifier.
        (true, true, true) => Some(NameShape::Stash("::".to_string())),
        (false, true, false) => Some(NameShape::Stash(format!("{}::", names.join("::")))),
        (_, false, false) => Some(NameShape::Identifier(names.join("::"))),
        _ => None,
    }
}
