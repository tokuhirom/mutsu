//! A parametric role and a plain role may share one name (`role Some[::T]
//! {...}` beside `role Some {...}`, as Definitely declares). The registry's
//! by-name tables (`roles`, `role_attribute_types`) hold one entry per name, so
//! a lookup made for a composition has to pick the candidate of the shape the
//! composition used: bracketed (`Some[Int]`) or bare (`Some`).

use super::*;

impl Interpreter {
    /// The definition of the `base` candidate that is parametric (or plain,
    /// when `parametric` is false), when the name has exactly one such
    /// candidate. `None` when the name has no candidate list or the shape is
    /// ambiguous; the caller then keeps its by-name answer.
    // Cost: O(c), c = candidates registered under `base`.
    pub(crate) fn role_candidate_of_shape(&self, base: &str, parametric: bool) -> Option<RoleDef> {
        let registry = self.registry();
        let candidates = registry.role_candidates.get(base)?;
        let mut matching = candidates
            .iter()
            .filter(|c| c.type_params.is_empty() != parametric);
        let found = matching.next()?;
        if matching.next().is_some() {
            return None;
        }
        Some(found.role_def.clone())
    }

    /// Whether the role composed as `owner` (`Some` or `Some[Int]`) declares
    /// `attr` with a type, so that a `role_attribute_types` entry under its
    /// base name is its own and not a same-named candidate's. True when the
    /// name has a single definition.
    // Cost: O(c + a), c = candidates under the base name, a = attributes.
    pub(crate) fn role_owner_declares_typed_attribute(&self, owner: &str, attr: &str) -> bool {
        let (base, parametric) = match owner.split_once('[') {
            Some((base, _)) => (base, true),
            None => (owner, false),
        };
        let registry = self.registry();
        let Some(candidates) = registry.role_candidates.get(base) else {
            return true;
        };
        let mut matching = candidates
            .iter()
            .filter(|c| c.type_params.is_empty() != parametric);
        let Some(found) = matching.next() else {
            return true;
        };
        if matching.next().is_some() {
            return true;
        }
        found
            .role_def
            .attributes
            .iter()
            .any(|a| a.name == attr && a.type_constraint.is_some())
    }
}
