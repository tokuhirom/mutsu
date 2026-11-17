//! The dispatcher of a `multi` routine's name while its first candidate is
//! being declared (#17793).
//!
//! Rakudo installs the proto it derives for a scope's `multi` candidates only
//! once the first candidate is complete, after its body is parsed. An operator
//! inside that body (`100 + $r` in a block-local `multi infix:<+>`) therefore
//! resolves to the outer scope's operator, not to the routine being declared.
//! A `proto` installs the dispatcher before its own body, and every later
//! candidate of the scope sees it.

use super::super::{RakuAstClass as C, RakuAstField, RakuAstFieldValue, RakuAstNode};
use super::Scopes;
use crate::value::{Value, ValueView};

impl Scopes<'_> {
    /// [`Self::find`] that skips the declaration of a `multi` whose body is
    /// being walked while its scope has no dispatcher of the name yet: rakudo
    /// installs the derived proto after the candidate's body is parsed, so the
    /// body's own `+` is the outer scope's operator.
    // Cost: O(d * h), d = scope depth, h = enclosing hidden multis (small).
    pub(super) fn find_visible(&self, name: &str) -> Option<&Value> {
        self.scopes
            .iter()
            .enumerate()
            .rev()
            .filter(|(depth, _)| {
                !self
                    .hidden
                    .iter()
                    .any(|(hidden, at)| at == depth && hidden == name)
            })
            .find_map(|(_, scope)| scope.get(name))
    }
}

/// For a `proto` or `multi` sub, record what it does to its name's dispatcher
/// in the enclosing scope: a `proto` installs it before its body; the first
/// `multi` of a scope with none is hidden while its body is walked and
/// installs the derived proto after it (the returned key, #17793).
// Cost: O(f + h), f = fields of the node, h = enclosing hidden multis.
pub(super) fn enter_routine(node: &RakuAstNode, scopes: &mut Scopes) -> Option<(String, usize)> {
    if node.class != C::Sub {
        return None;
    }
    let multiness = super::super::lower::leaf_str(node, "multiness").ok()?;
    let name = super::super::declared_routines::declared_by(node)?;
    let key = (format!("&{name}"), scopes.scopes.len().checked_sub(1)?);
    match multiness.as_str() {
        "proto" => {
            scopes.dispatchers.insert(key);
            None
        }
        "multi" if !scopes.dispatchers.contains(&key) => {
            scopes.hidden.push(key.clone());
            Some(key)
        }
        _ => None,
    }
}

/// What routine name `name` (`&name` = `key`) resolves to outside the
/// innermost scope: an outer scope's declaration, or the setting's routine or
/// operator. `None` when neither has one.
// Cost: O(d), d = scope depth.
pub(super) fn outer_resolution(name: &str, key: &str, scopes: &Scopes) -> Option<Value> {
    let depth = scopes.scopes.len();
    if let Some(outer) = scopes.scopes[..depth.saturating_sub(1)]
        .iter()
        .rev()
        .find_map(|scope| scope.get(key))
    {
        return Some(outer.clone());
    }
    super::external_setting(name)
}

/// The declaration a scope's `multi` candidates of a name the outer scope or
/// the setting already has a dispatcher for resolve to: rakudo derives a proto
/// holding the outer family's candidates and the scope's own, as a
/// `RakuAST::VarDeclaration::Implicit::Constant` named `key` whose value is
/// that proto (#17826). The value needs the interpreter, so the node carries
/// what builds it at CHECK time in its metadata: the outer resolution
/// (`outer`, or Nil), then the scope's candidates ([`attach_candidates`]).
// Cost: O(k), k = length of the name.
pub(super) fn derived_family(key: &str, outer: Option<Value>) -> Value {
    let mut node = RakuAstNode {
        origin: Default::default(),
        class: C::VarDeclarationImplicitConstant,
        fields: vec![RakuAstField {
            name: Some("name"),
            value: RakuAstFieldValue::Node(Value::str(key.to_string())),
        }],
    };
    node.origin.2 = Some(Value::array(vec![outer.unwrap_or(Value::NIL)]));
    Value::rakuast(Box::new(node))
}

/// `derived` (a [`derived_family`] node) with the scope's candidates `found`
/// after its outer resolution; `None` for any other node.
// Cost: O(c), c = candidates.
pub(super) fn attach_candidates(derived: &RakuAstNode, found: &[Value]) -> Option<Value> {
    let (outer, _) = family_parts(derived)?;
    let mut with_candidates = RakuAstNode::clone(derived);
    let mut parts = Vec::with_capacity(found.len() + 1);
    parts.push(outer);
    parts.extend_from_slice(found);
    with_candidates.origin.2 = Some(Value::array(parts));
    Some(Value::rakuast(Box::new(with_candidates)))
}

/// The outer resolution and the scope's candidates a [`derived_family`] node
/// carries; `None` for any other node.
// Cost: O(c), c = candidates copied.
pub(crate) fn family_parts(node: &RakuAstNode) -> Option<(Value, Vec<Value>)> {
    if node.class != C::VarDeclarationImplicitConstant
        || node.fields.iter().any(|f| f.name == Some("value"))
    {
        return None;
    }
    let ValueView::Array(items, _) = node.origin.2.as_ref()?.view() else {
        return None;
    };
    let (outer, candidates) = items.as_slice().split_first()?;
    Some((outer.clone(), candidates.to_vec()))
}
