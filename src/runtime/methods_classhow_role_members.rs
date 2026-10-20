//! `Metamodel::ParametricRoleHOW` member introspection: what `.^methods` and
//! `.^attributes` report for a role type object, and the attributes a
//! run-time mixin (`$o but R`, `$o does R`) contributes to `.^attributes`.

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// The role a `.^` receiver names, when it is a role type object (not a
    /// class, which also answers for its punned role's name once punned).
    // Cost: O(1), two registry probes.
    pub(super) fn mop_receiver_role(&self, receiver: &Value, owner: &str) -> Option<String> {
        if !matches!(receiver.view(), ValueView::Package(_)) {
            return None;
        }
        let registry = self.registry();
        (registry.roles.contains_key(owner) && !registry.classes.contains_key(owner))
            .then(|| owner.to_string())
    }

    /// Whether `receiver` (owned by `owner`) is a role type object or one
    /// candidate of a role group, as `.^candidates` hands it out: `Some` with
    /// the candidate's index when it is one, `Some(None)` for the role
    /// itself, `None` for anything else (a class, an instance).
    // Cost: O(1), a few registry probes.
    pub(super) fn role_candidate_receiver(
        &self,
        receiver: &Value,
        owner: &str,
    ) -> Option<Option<usize>> {
        if let ValueView::Instance { attributes, .. } = receiver.view()
            && let Some(idx) = attributes.as_map().get("__mutsu_role_candidate_idx")
            && self.registry().role_candidates.contains_key(owner)
        {
            return Some(match idx.view() {
                ValueView::Int(i) => usize::try_from(i).ok(),
                _ => None,
            });
        }
        self.mop_receiver_role(receiver, owner).map(|_| None)
    }

    /// `R.^methods`: the methods the role itself declares. Rakudo generates a
    /// public attribute's accessor only when a class composes the role, and a
    /// role's `does`/`also does` parents keep their own methods (each is
    /// listed by `R.^roles[N].^methods`), so neither appears here. A
    /// `submethod` does (`(m BUILD)`).
    // Cost: O(m), m = methods the role (with its composed parents) holds.
    pub(super) fn role_own_method_objects(
        &self,
        role_name: &str,
        candidate: Option<usize>,
        include_private: bool,
    ) -> Vec<Value> {
        let mut result = Vec::new();
        let registry = self.registry();
        let role_def = candidate
            .and_then(|idx| registry.role_candidates.get(role_name)?.get(idx))
            .map(|cand| &cand.role_def)
            .or_else(|| registry.roles.get(role_name));
        let Some(role_def) = role_def else {
            return result;
        };
        let mut names: Vec<&String> = role_def.methods.keys().collect();
        names.sort();
        for method_name in names {
            let overloads = &role_def.methods[method_name];
            let own: Vec<MethodDef> = overloads
                .iter()
                .filter(|m| m.role_origin.as_deref().is_none_or(|origin| origin == role_name))
                .cloned()
                .collect();
            let Some(first) = own.first() else {
                continue;
            };
            if first.is_private && !include_private {
                continue;
            }
            result.push(self.make_method_object_with_owner(
                method_name,
                first,
                own.len() > 1,
                first.return_type.clone(),
                Some(&own),
                Some(role_name),
            ));
        }
        result
    }

    /// `R.^attributes`: the attributes declared in the role's own body. Those
    /// of a `does`/`also does` parent are copied into the role's definition
    /// for composition, but Rakudo reports them only on the parent. For the
    /// candidate `candidate` of the role group `role_name` (an index into
    /// `.^candidates`, which carries it as `__mutsu_role_candidate_idx`), or
    /// for the role registered under the name when `None` or out of range.
    // Cost: O(a), a = attributes the candidate (with its composed parents) holds.
    pub(super) fn role_candidate_own_attribute_objects(
        &self,
        role_name: &str,
        candidate: Option<usize>,
    ) -> Vec<Value> {
        let registry = self.registry();
        let role_def = candidate
            .and_then(|idx| registry.role_candidates.get(role_name)?.get(idx))
            .map(|cand| &cand.role_def)
            .or_else(|| registry.roles.get(role_name));
        let Some(role_def) = role_def else {
            return Vec::new();
        };
        let own: Vec<_> = role_def
            .attributes
            .iter()
            .filter(|attr| role_def.own_attribute_names.contains(&attr.name))
            .cloned()
            .collect();
        own.iter()
            .map(|attr| self.make_attribute_object(attr, role_name))
            .collect()
    }

    /// The attributes the roles mixed into `receiver` at run time contribute,
    /// most recently applied role first, each role's own before its parents'
    /// (`(5 but R).^attributes` is `($!x $!value)` in Rakudo: the mixin's
    /// anonymous class sits ahead of the base type). Empty for a non-mixin.
    // Cost: O(r * a), r = mixed-in roles, a = attributes per role.
    pub(super) fn mixin_role_attribute_objects(&self, receiver: &Value, base: &str) -> Vec<Value> {
        let ValueView::Mixin(_, mixins) = receiver.view() else {
            return Vec::new();
        };
        crate::value::types::mixin_roles_applied_last_first(mixins, base)
            .iter()
            .flat_map(|role| {
                let registry = self.registry();
                let Some(role_def) = registry.roles.get(role.as_str()) else {
                    return Vec::new();
                };
                let (mut own, inherited): (Vec<_>, Vec<_>) = role_def
                    .attributes
                    .iter()
                    .partition(|attr| role_def.own_attribute_names.contains(&attr.name));
                own.extend(inherited);
                own.into_iter()
                    .map(|attr| self.make_attribute_object(attr, role))
                    .collect::<Vec<_>>()
            })
            .collect()
    }
}
