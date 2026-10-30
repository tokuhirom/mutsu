//! Fields Rakudo always fills, with an empty node when the source wrote
//! nothing: the `ArgList` of `foo` / `foo()` / `.abs`, the `Signature` of
//! `sub f { }` and `-> { }`.
//!
//! mutsu leaves such a field out of the node, as Rakudo's renderer leaves it
//! out of `.raku`, and the accessor answers an empty node for it
//! (`fields::Absent::EmptyNode`). Rakudo's `visit-children` still visits the
//! empty node, and a parsed one has an origin (`foo()` places its `ArgList`
//! just inside the parentheses), so the node records that origin beside its
//! own ([`super::NodeOrigin`]) and both the accessor and `visit-children`
//! build the empty node with it. Keeping the field absent keeps equality and
//! the constructor form unchanged.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::Value;

/// The origins of the empty nodes a node's absent fields stand for.
#[derive(Debug, Clone, Default)]
pub struct EmptyOrigins(Option<Box<[(&'static str, Value)]>>);

impl EmptyOrigins {
    // Cost: O(e), e = empty fields with an origin (at most two).
    fn get(&self, field: &str) -> Option<&Value> {
        self.0
            .as_deref()?
            .iter()
            .find(|(name, _)| *name == field)
            .map(|(_, origin)| origin)
    }
}

/// The fields of `class` whose empty node Rakudo's `visit-children` visits,
/// measured on rakudo 2026.09. A `Method`'s or a regex declaration's
/// signature is not listed: Rakudo fills it with the implicit `self` / `%_`
/// parameters, which mutsu does not model.
// Cost: O(1).
fn visited(class: RakuAstClass) -> &'static [&'static str] {
    use RakuAstClass as C;
    match class {
        C::CallName
        | C::CallNameWithoutParentheses
        | C::CallMethod
        | C::CallPrivateMethod
        | C::CallNameAsMethod => &["args"],
        C::Sub | C::PointyBlock => &["signature"],
        _ => &[],
    }
}

/// The empty `class` node `node`'s absent field `field` stands for, with the
/// origin the parse gave it.
// Cost: O(e), e = empty fields with an origin.
pub(super) fn empty_node(node: &RakuAstNode, field: &str, class: RakuAstClass) -> Value {
    // The signature rakudo fills in has an initialized, empty parameter list
    // (`parameters-initialized` is True, `.raku` shows `parameters => ()`).
    let fields = if class == RakuAstClass::Signature {
        vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(Vec::new()),
        }]
    } else {
        Vec::new()
    };
    Value::rakuast(Box::new(RakuAstNode {
        class,
        fields,
        origin: super::NodeOrigin(node.origin.1.get(field).cloned(), EmptyOrigins::default(), None),
    }))
}

/// Record that `node`'s absent field `field` was written at `span` (empty:
/// where the field would begin).
// Cost: O(e + log n), e = empty fields with an origin, n = lines of the unit.
pub(super) fn set_span(node: &mut RakuAstNode, field: &'static str, span: SrcSpan) {
    if node.fields.iter().any(|f| f.name == Some(field)) || node.origin.1.get(field).is_some() {
        return;
    }
    let Some(origin) = super::unit_source::span_origin_value(span) else {
        return;
    };
    let empty = &mut node.origin.1.0;
    let mut kept = empty.take().map(Vec::from).unwrap_or_default();
    kept.push((field, origin));
    *empty = Some(kept.into_boxed_slice());
}

/// `node` with the empty nodes its absent fields stand for filled in, in
/// declaration order, when its class's `visit-children` visits them.
// Cost: O(f), f = declared fields of the class.
pub(crate) fn materialized(node: &RakuAstNode) -> Option<RakuAstNode> {
    let names = visited(node.class);
    if names
        .iter()
        .all(|name| node.fields.iter().any(|f| f.name == Some(*name)))
    {
        return None;
    }
    let declared = super::fields::model_fields(node.class);
    let mut out = node.clone();
    for name in names {
        if out.fields.iter().any(|f| f.name == Some(*name)) {
            continue;
        }
        let Some((name, super::fields::Absent::EmptyNode(class))) =
            declared.iter().find(|(n, _)| n == name)
        else {
            continue;
        };
        // After the fields declared before it.
        let before: Vec<&str> = declared
            .iter()
            .take_while(|(n, _)| n != name)
            .map(|(n, _)| *n)
            .collect();
        let at = out
            .fields
            .iter()
            .rposition(|f| f.name.is_some_and(|n| before.contains(&n)))
            .map_or(0, |i| i + 1);
        out.fields.insert(
            at,
            RakuAstField {
                name: Some(name),
                value: RakuAstFieldValue::Node(empty_node(node, name, *class)),
            },
        );
    }
    Some(out)
}
