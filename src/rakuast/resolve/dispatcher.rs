//! The dispatcher of a `multi` routine's name while its first candidate is
//! being declared (#17793).
//!
//! Rakudo installs the proto it derives for a scope's `multi` candidates only
//! once the first candidate is complete, after its body is parsed. An operator
//! inside that body (`100 + $r` in a block-local `multi infix:<+>`) therefore
//! resolves to the outer scope's operator, not to the routine being declared.
//! A `proto` installs the dispatcher before its own body, and every later
//! candidate of the scope sees it.

use super::super::{RakuAstClass as C, RakuAstNode};
use super::Scopes;
use crate::value::Value;

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
