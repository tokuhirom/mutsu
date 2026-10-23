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
struct Written {
    ty: Option<String>,
    var: String,
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
        if head.is_empty() || !head.split_whitespace().all(|w| w.chars().all(char::is_alphabetic))
        {
            return None;
        }
        let close = text.trim_end().strip_suffix(')')?.len();
        let inner = text.get(open + 1..close)?;
        scan(inner, &parameters, from + open + 1)
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
struct Spans {
    param: SrcSpan,
    var: SrcSpan,
    ty: SrcSpan,
}

/// The parameters of `signature`, as written, when every one is a plain
/// variable with an optional simple type.
// Cost: O(p), p = parameters.
fn parameters(signature: &RakuAstNode) -> Option<Vec<Written>> {
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
                    Some(
                        "default"
                            | "where"
                            | "names"
                            | "type-captures"
                            | "invocant"
                            | "array-shape"
                            | "slurpy"
                            | "sub-signature"
                            | "traits"
                    )
                )
            }) {
                return None;
            }
            let target = child(param, "target")?;
            if target.class != RakuAstClass::ParameterTargetVar {
                return None;
            }
            let ty = match child(param, "type") {
                None => None,
                Some(ty) if ty.class == RakuAstClass::TypeSimple => {
                    Some(leaf_text(child_at(ty, 0)?)?)
                }
                Some(_) => return None,
            };
            Some(Written {
                ty,
                var: leaf_text(target)?,
            })
        })
        .collect()
}

/// Read `parameters` back, in order, from `inner`, which starts at byte
/// `base` of the unit.
// Cost: O(n), n = length of `inner`.
fn scan(inner: &str, parameters: &[Written], base: usize) -> Option<Vec<Spans>> {
    let mut at = 0;
    let mut out = Vec::with_capacity(parameters.len());
    for (i, written) in parameters.iter().enumerate() {
        at += inner[at..].len() - inner[at..].trim_start().len();
        let start = at;
        let mut ty = SrcSpan::NONE;
        if let Some(name) = &written.ty {
            inner[at..].strip_prefix(name.as_str())?;
            ty = SrcSpan::new(base + at, base + at + name.len());
            at += name.len();
            let gap = inner[at..].len() - inner[at..].trim_start().len();
            if gap == 0 {
                return None;
            }
            at += gap;
        }
        inner[at..].strip_prefix(written.var.as_str())?;
        let var = SrcSpan::new(base + at, base + at + written.var.len());
        at += written.var.len();
        out.push(Spans {
            param: SrcSpan::new(base + start, base + at),
            var,
            ty,
        });
        at += inner[at..].len() - inner[at..].trim_start().len();
        if i + 1 < parameters.len() {
            inner[at..].strip_prefix(',')?;
            at += 1;
        }
    }
    inner[at..].trim().is_empty().then_some(out)
}

/// Give each `Parameter` of `signature` the ranges in `spans`.
// Cost: O(p), p = parameters.
fn place(signature: &mut RakuAstNode, spans: &[Spans]) {
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
        *value = Value::rakuast(Box::new(node));
    }
}
