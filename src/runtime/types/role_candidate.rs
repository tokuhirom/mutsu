//! Individual parametric role vs. role group identity.
//!
//! Rakudo keeps two distinct meta-objects for what source code writes as one
//! `role R { ... }`:
//!
//! * the **role group** (`Perl6::Metamodel::ParametricRoleGroupHOW`), which the
//!   installed *name* `R` resolves to and which dispatches across every
//!   same-named candidate, and
//! * the **individual parametric role** (`Perl6::Metamodel::ParametricRoleHOW`),
//!   one per `role` declaration, which is what the declaration *expression*
//!   evaluates to and what `R.^candidates` hands out.
//!
//! mutsu represents a type object as a bare name (`Value::Package(Symbol)`), so
//! the two need distinguishable names. An individual candidate gets a
//! **declaration-site key** — the group name, a `\u{0}` separator, and the
//! declaration's `role_id` — reusing the exact convention ADR-0047 P1 already
//! established for lexical class site keys. `\u{0}` cannot appear in a source
//! identifier, and `value::display::user_facing_type_name` already strips
//! everything after it, so `.^name`, `.gist`, `.raku` and every error message
//! keep showing the bare `R` with no extra work.
//!
//! The key is *only* ever produced for the declaration expression's value and
//! for `.^candidates`. Everywhere a role type object is consumed — `but`,
//! `does`, role composition, type matching — it is normalised straight back to
//! the group name, so composition markers, `.^roles` and `~~` never see a site
//! key. The distinction exists to answer `.HOW` (and anything that keys off it)
//! correctly; it is deliberately not a second role registration.

use super::*;

/// Separator between a role group name and the declaration-site `role_id` of
/// one of its candidates. Shared with ADR-0047's lexical class site keys, which
/// is why `user_facing_type_name` already demangles it.
const CANDIDATE_SEP: char = '\u{0}';

/// The declaration-site key naming one individual candidate of role `group`.
pub(crate) fn role_candidate_type_name(group: &str, role_id: u64) -> String {
    format!("{group}{CANDIDATE_SEP}{role_id}")
}

/// Split a candidate site key back into `(group, role_id)`. Returns `None` for
/// any name that is not shaped like one (including a lexical *class* site key,
/// whose group half is not a role — the caller verifies that).
pub(crate) fn split_role_candidate_type_name(name: &str) -> Option<(&str, u64)> {
    let (group, id) = name.rsplit_once(CANDIDATE_SEP)?;
    Some((group, id.parse().ok()?))
}

impl Interpreter {
    /// `(group, role_id)` when `name` is a role-candidate site key naming a
    /// *live* declaration — i.e. the group half is a registered role and the id
    /// half matches either one of its recorded candidates or the group's own
    /// current definition. Anything else (a plain name, a lexical class site
    /// key, a stale id) is `None`.
    pub(crate) fn role_candidate_group(&self, name: &str) -> Option<(String, u64)> {
        let (group, role_id) = split_role_candidate_type_name(name)?;
        let reg = self.registry();
        let group_def = reg.roles.get(group)?;
        let live = group_def.role_id == role_id
            || reg
                .role_candidates
                .get(group)
                .is_some_and(|cands| cands.iter().any(|c| c.role_def.role_id == role_id));
        live.then(|| (group.to_string(), role_id))
    }

    /// The role group a role type object names, whether it is the group itself
    /// (`Package("R")`) or one individual candidate (`Package("R\0<id>")`).
    /// This is the normalisation every *consumer* of a role type object applies
    /// — composition, `but`/`does`, type matching — so a site key never leaks
    /// into a composition marker or a `.^roles` answer.
    pub(crate) fn role_group_name(&self, name: &str) -> String {
        match self.role_candidate_group(name) {
            Some((group, _)) => group,
            None => name.to_string(),
        }
    }

    /// Normalise a role type object that may be an individual candidate back to
    /// its group before it is *composed* into something. Composition markers,
    /// `.^roles`, `~~` and every other downstream consumer are group-keyed, so
    /// a site key must never reach them: `1 but (role R { })` composes `R`, not
    /// `R\0<id>`. Every other value passes through untouched.
    pub(crate) fn normalize_role_type_object(&self, value: &Value) -> Value {
        if let ValueView::Package(name) = value.view()
            && let Some((group, _)) = self.role_candidate_group(&name.resolve())
        {
            return Value::package(Symbol::intern(&group));
        }
        value.clone()
    }

    /// Whether `value` is an INDIVIDUAL parametric role (one declaration), as
    /// opposed to the same-named role group. Two representations carry that
    /// identity: the candidate-keyed `Package` a role declaration expression
    /// evaluates to, and the candidate objects `.^candidates` returns.
    pub(crate) fn is_individual_role_type_object(&self, value: &Value) -> bool {
        match value.view() {
            ValueView::Package(name) => self.role_candidate_group(&name.resolve()).is_some(),
            ValueView::Instance { attributes, .. } => {
                let attrs = attributes.as_map();
                attrs.contains_key("__mutsu_role_candidate_idx")
                    && attrs.contains_key("__mutsu_role_base_name")
            }
            _ => false,
        }
    }

    /// The user-facing (group) name of a role type object, for display and for
    /// the `name` a `ParametricRoleHOW` reports.
    pub(crate) fn role_type_object_display_name(&self, value: &Value) -> String {
        match value.view() {
            ValueView::Package(name) => {
                crate::value::user_facing_type_name(&name.resolve()).into_owned()
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => attributes
                .as_map()
                .get("__mutsu_role_base_name")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| class_name.resolve()),
            _ => value.to_string_value(),
        }
    }
}
