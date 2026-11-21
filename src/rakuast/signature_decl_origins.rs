//! Origins of the signature of a `my ($a, Int $b) = ...` declaration
//! (ADR-10723 Stage 2, declaration families).
//!
//! The parser desugars the declaration into one declaration per variable, so
//! the signature and its parameters carry no ranges. They are placed from the
//! variables the converted signature names, read back in order from the
//! parentheses of the written declaration: each parameter must be exactly
//! `[Type ]$name`, separated by commas. Anything else (defaults, traits,
//! destructuring) makes the whole signature stay without ranges: exact or
//! absent.

use super::infix_origins::{byte_range, child, child_at, leaf_text};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// One parameter as the converted signature spells it.
pub(super) struct Written {
    ty: Option<String>,
    var: String,
    /// A sigilless `\x` parameter: written with the backslash, its target is
    /// the name alone.
    term: bool,
    /// A named parameter `:$x`: written with the colon, which the parameter's
    /// range includes and its target's does not.
    named: bool,
    /// The `?` / `!` an `optional` field may have been written as.
    marker: Option<char>,
    /// The byte range of the default (`$x = 42`), which the parameter's range
    /// runs to.
    default: Option<(usize, usize)>,
    /// A sub-signature `($a, $b)` written in place of a variable.
    sub: Option<Vec<Written>>,
    /// The names of the `is NAME` traits after the variable (`$x is copy`).
    traits: Vec<String>,
}

/// Place the `Signature` of `node`, a `VarDeclaration::Signature`, and its
/// parameters.
// Cost: O(n + p), n = length of the signature's source, p = parameters.
pub(super) fn declaration(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let limit = child(node, "initializer")
        .and_then(byte_range)
        .map_or(to, |(init, _)| init);
    let Some(signature) = child(node, "signature") else {
        return;
    };
    let Some(parameters) = parameters(signature) else {
        return;
    };
    let spans = super::unit_source::with_text(SrcSpan::new(from, limit), |text| {
        let open = text.find('(')?;
        // Only scope declarators come before the parentheses.
        let head = text[..open].trim_end();
        if head.is_empty()
            || !head
                .split_whitespace()
                .all(|w| w.chars().all(char::is_alphabetic))
        {
            return None;
        }
        let close = text.trim_end().strip_suffix(')')?.len();
        let inner = text.get(open + 1..close)?;
        let (spans, at) = scan(inner, &parameters, from + open + 1)?;
        inner[at..].trim().is_empty().then_some(spans)
    });
    let Some(spans) = spans else {
        return;
    };
    let whole = SrcSpan::new(
        spans.first().map_or(0, |s| s.param.from as usize),
        spans.last().map_or(0, |s| s.param.to as usize),
    );
    super::infix_origins::with_child(node, "signature", |signature| {
        super::origin::set_span(signature, whole);
        place(signature, &spans);
    });
}

/// The spans of one parameter.
pub(super) struct Spans {
    pub(super) param: SrcSpan,
    var: SrcSpan,
    ty: SrcSpan,
    /// A sub-signature's range (inside its parentheses) and parameters.
    sub: Option<(SrcSpan, Vec<Spans>)>,
    /// Each `is NAME` trait's range and its name's.
    traits: Vec<(SrcSpan, SrcSpan)>,
}

/// The parameters of `signature`, as written, when every one is a plain
/// variable with an optional simple type.
// Cost: O(p), p = parameters.
pub(super) fn parameters(signature: &RakuAstNode) -> Option<Vec<Written>> {
    let list = signature
        .fields
        .iter()
        .find(|f| f.name == Some("parameters"))?;
    let RakuAstFieldValue::List(nodes) = &list.value else {
        return None;
    };
    nodes
        .iter()
        .map(|value| {
            let ValueView::RakuAst(param) = value.view() else {
                return None;
            };
            if param.class != RakuAstClass::Parameter {
                return None;
            }
            // A default, a constraint, a slurpy or a sub-signature is not a
            // plain variable.
            if param.fields.iter().any(|f| {
                matches!(
                    f.name,
                    Some("where" | "type-captures" | "invocant" | "array-shape" | "slurpy")
                )
            }) {
                return None;
            }
            let traits = is_traits(param)?;
            // `($a, $b)`: a sub-signature in place of the variable.
            if let Some(sub) = child(param, "sub-signature") {
                if child(param, "target").is_some()
                    || child(param, "default").is_some()
                    || !traits.is_empty()
                    || param.fields.iter().any(|f| f.name == Some("names"))
                {
                    return None;
                }
                return Some(Written {
                    ty: parameter_type(param)?,
                    var: String::new(),
                    term: false,
                    named: false,
                    marker: None,
                    default: None,
                    sub: Some(parameters(sub)?),
                    traits,
                });
            }
            let target = child(param, "target")?;
            // `:$x`: one name, the variable's own.
            let named = match param.fields.iter().find(|f| f.name == Some("names")) {
                None => false,
                Some(field) => {
                    let RakuAstFieldValue::List(names) = &field.value else {
                        return None;
                    };
                    let [name] = names.as_slice() else {
                        return None;
                    };
                    let var = leaf_text(target)?;
                    if target.class != RakuAstClass::ParameterTargetVar
                        || name.as_str().is_none_or(|n| var.get(1..) != Some(n))
                    {
                        return None;
                    }
                    true
                }
            };
            // `$x?` / `:$x!`: an `optional` the default would not give.
            let marker = match param.fields.iter().find(|f| f.name == Some("optional")) {
                None => None,
                Some(field) => {
                    let RakuAstFieldValue::Node(value) = &field.value else {
                        return None;
                    };
                    Some(if value.truthy() { '?' } else { '!' })
                }
            };
            // `$x = 42`: the default must carry its range.
            let default = match child(param, "default") {
                None => None,
                Some(value) => Some(super::infix_origins::byte_range(value)?),
            };
            if target.class == RakuAstClass::ParameterTargetTerm {
                let name = leaf_text(child_at(target, 0)?)?;
                return Some(Written {
                    ty: parameter_type(param)?,
                    var: name,
                    term: true,
                    named,
                    marker,
                    default,
                    sub: None,
                    traits,
                });
            }
            if target.class != RakuAstClass::ParameterTargetVar {
                return None;
            }
            Some(Written {
                ty: parameter_type(param)?,
                var: leaf_text(target)?,
                term: false,
                named,
                marker,
                default,
                sub: None,
                traits,
            })
        })
        .collect()
}

/// The names of `param`'s traits, when each is a plain `is NAME` (no
/// argument, no type); `None` for any other trait.
// Cost: O(t), t = traits.
fn is_traits(param: &RakuAstNode) -> Option<Vec<String>> {
    let Some(field) = param.fields.iter().find(|f| f.name == Some("traits")) else {
        return Some(Vec::new());
    };
    let RakuAstFieldValue::List(nodes) = &field.value else {
        return None;
    };
    nodes
        .iter()
        .map(|value| match value.view() {
            ValueView::RakuAst(node)
                if node.class == RakuAstClass::TraitIs
                    && child(node, "argument").is_none()
                    && child(node, "type").is_none() =>
            {
                leaf_text(child(node, "name")?)
            }
            _ => None,
        })
        .collect()
}

/// Read the `is NAME` traits `names` back from `inner` at `at`, each after
/// blanks: the trait's range and its name's, and the offset after them.
// Cost: O(n), n = length of the traits' text.
fn scan_traits(
    inner: &str,
    mut at: usize,
    names: &[String],
    base: usize,
) -> Option<(Vec<(SrcSpan, SrcSpan)>, usize)> {
    let mut out = Vec::with_capacity(names.len());
    for name in names {
        let gap = inner[at..].len() - inner[at..].trim_start().len();
        let start = at + gap;
        let after_is = inner[start..].strip_prefix("is")?;
        let blank = after_is.len() - after_is.trim_start().len();
        if gap == 0 || blank == 0 {
            return None;
        }
        let name_at = start + 2 + blank;
        let rest = inner[name_at..].strip_prefix(name.as_str())?;
        if rest.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '(' | '[')) {
            return None;
        }
        at = name_at + name.len();
        out.push((
            SrcSpan::new(base + start, base + at),
            SrcSpan::new(base + name_at, base + at),
        ));
    }
    Some((out, at))
}

/// The simple type name of `param` (`Some(None)` when it has none), or `None`
/// for a type this does not read.
// Cost: O(1).
fn parameter_type(param: &RakuAstNode) -> Option<Option<String>> {
    match child(param, "type") {
        // The implicit `Any` of an untyped parameter is not written.
        None => Some(None),
        Some(ty) if ty.class == RakuAstClass::TypeSetting => Some(None),
        Some(ty) if ty.class == RakuAstClass::TypeSimple => {
            Some(Some(leaf_text(child_at(ty, 0)?)?))
        }
        // `Int:D`, `Array[Int]`, `Str(Int)`: read back from the text (an empty
        // name, see [`type_word_len`]).
        Some(ty)
            if matches!(
                ty.class,
                RakuAstClass::TypeDefinedness
                    | RakuAstClass::TypeAnyDefinedness
                    | RakuAstClass::TypeParameterized
                    | RakuAstClass::TypeCoercion
            ) =>
        {
            Some(Some(String::new()))
        }
        Some(_) => None,
    }
}

/// The length of the type written at the start of `text`: a name, then a
/// bracketed application (`[Int]`) or coercion (`(Int)`), then a smiley.
/// `None` when brackets do not balance plainly before a blank.
// Cost: O(n), n = length of the type.
fn type_word_len(text: &str) -> Option<usize> {
    let mut at = text
        .find(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':')))
        .unwrap_or(text.len());
    // `Int:D` stops before the smiley's colon only if a bracket follows.
    if let Some(open @ ('[' | '(')) = text[at..].chars().next() {
        let close = if open == '[' { ']' } else { ')' };
        at += 1 + super::regex_call_origins::bracketed_len(&text[at + 1..], close)? + 1;
        for smiley in [":D", ":U", ":_"] {
            if text[at..].starts_with(smiley) {
                at += smiley.len();
                break;
            }
        }
    }
    (at > 0).then_some(at)
}

/// Read `parameters` back, in order, from `inner`, which starts at byte
/// `base` of the unit.
// Cost: O(n), n = length of `inner`.
pub(super) fn scan(
    inner: &str,
    parameters: &[Written],
    base: usize,
) -> Option<(Vec<Spans>, usize)> {
    let mut at = 0;
    let mut out = Vec::with_capacity(parameters.len());
    for (i, written) in parameters.iter().enumerate() {
        at += inner[at..].len() - inner[at..].trim_start().len();
        let start = at;
        let mut ty = SrcSpan::NONE;
        if let Some(name) = &written.ty {
            let len = if name.is_empty() {
                type_word_len(&inner[at..])?
            } else {
                inner[at..].strip_prefix(name.as_str())?;
                name.len()
            };
            ty = SrcSpan::new(base + at, base + at + len);
            at += len;
            let gap = inner[at..].len() - inner[at..].trim_start().len();
            if gap == 0 {
                return None;
            }
            at += gap;
        }
        if written.term {
            inner[at..].strip_prefix('\\')?;
            at += 1;
        }
        if written.named {
            inner[at..].strip_prefix(':')?;
            at += 1;
        }
        if let Some(sub) = &written.sub {
            // `( ... )`: the parameters inside, then the closing paren.
            let open = inner[at..].strip_prefix('(')?;
            let lead = open.len() - open.trim_start().len();
            let inner_at = at + 1 + lead;
            let (spans, used) = scan(&inner[inner_at..], sub, base + inner_at)?;
            let close_at = inner_at + used;
            inner[close_at..].strip_prefix(')')?;
            let range = SrcSpan::new(
                spans
                    .first()
                    .map_or(base + inner_at, |s| s.param.from as usize),
                spans
                    .last()
                    .map_or(base + inner_at, |s| s.param.to as usize),
            );
            at = close_at + 1;
            out.push(Spans {
                param: SrcSpan::new(base + start, base + at),
                var: SrcSpan::NONE,
                ty,
                sub: Some((range, spans)),
                traits: Vec::new(),
            });
            at += inner[at..].len() - inner[at..].trim_start().len();
            if i + 1 < parameters.len() {
                inner[at..].strip_prefix(',')?;
                at += 1;
            }
            continue;
        }
        inner[at..].strip_prefix(written.var.as_str())?;
        let var = SrcSpan::new(base + at, base + at + written.var.len());
        at += written.var.len();
        // A pointy block's positional parameter is `optional => False` with
        // nothing written: the marker is read when it is there.
        if let Some(marker) = written.marker
            && inner[at..].starts_with(marker)
        {
            at += 1;
        }
        let (traits, after_traits) = scan_traits(inner, at, &written.traits, base)?;
        at = after_traits;
        if let Some((from, to)) = written.default {
            let rest = inner[at..].trim_start().strip_prefix('=')?;
            let value_at = inner.len() - rest.trim_start().len();
            if base + value_at != from || to < base + value_at {
                return None;
            }
            at = to - base;
        }
        out.push(Spans {
            param: SrcSpan::new(base + start, base + at),
            var,
            ty,
            sub: None,
            traits,
        });
        at += inner[at..].len() - inner[at..].trim_start().len();
        if i + 1 < parameters.len() {
            inner[at..].strip_prefix(',')?;
            at += 1;
        }
    }
    Some((out, at))
}

/// Give each `Parameter` of `signature` the ranges in `spans`.
// Cost: O(p), p = parameters.
pub(super) fn place(signature: &mut RakuAstNode, spans: &[Spans]) {
    let Some(field) = signature
        .fields
        .iter_mut()
        .find(|f| f.name == Some("parameters"))
    else {
        return;
    };
    let RakuAstFieldValue::List(nodes) = &mut field.value else {
        return;
    };
    for (value, span) in nodes.iter_mut().zip(spans) {
        let ValueView::RakuAst(node) = value.view() else {
            continue;
        };
        let mut node = RakuAstNode::clone(node);
        super::routine_origins::parameter(&mut node, span.param, span.var, span.ty);
        place_traits(&mut node, &span.traits);
        if let Some((range, spans)) = &span.sub {
            super::infix_origins::with_child(&mut node, "sub-signature", |signature| {
                super::origin::set_span(signature, *range);
                place(signature, spans);
            });
        }
        *value = Value::rakuast(Box::new(node));
    }
}

/// Give a parameter's `Trait::Is` nodes their ranges and their names'.
// Cost: O(t), t = traits.
fn place_traits(param: &mut RakuAstNode, traits: &[(SrcSpan, SrcSpan)]) {
    if traits.is_empty() {
        return;
    }
    let Some(field) = param.fields.iter_mut().find(|f| f.name == Some("traits")) else {
        return;
    };
    let RakuAstFieldValue::List(nodes) = &mut field.value else {
        return;
    };
    for (value, &(whole, name)) in nodes.iter_mut().zip(traits) {
        let ValueView::RakuAst(node) = value.view() else {
            continue;
        };
        let mut node = RakuAstNode::clone(node);
        super::origin::set_span(&mut node, whole);
        super::infix_origins::with_child(&mut node, "name", |n| super::origin::set_span(n, name));
        *value = Value::rakuast(Box::new(node));
    }
}
