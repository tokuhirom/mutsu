//! A variable's type record follows the variable (ADR-13757 step 4).
//!
//! A typed declaration records its constraint as an env entry keyed by the
//! variable's name (`__mutsu_type::x` and its hash-key twin), next to the
//! variable, in the frame that declares it. A by-name lookup of that record
//! used to walk the whole caller chain, so when the frame that binds the
//! variable has no record (an untyped or `Mu` parameter, a binder that writes
//! none), the walk went on and found a same-named record of a frame that
//! called it: a caller's `Str $input` constrained a callee's `$input`.
//!
//! [`Env::get_record_sym`] answers instead with the record of the frame that
//! binds the variable: a walk that reaches a caller frame binding the variable
//! without a record there ends with no record. The frame the lookup starts in
//! keeps its own rules (the static link and the capture, `Env::get_sym`); a
//! slot the running code declares reads its record in its frame only
//! (`Env::get_record_in_frame`). A caller frame's binding that is the same
//! container as the binding below it is an alias mirror (a sigilless
//! parameter installs its source's name in the callee frame), whose record is
//! the source's: the walk goes on.

use super::Env;
use super::static_link::same_variable;
use crate::symbol::Symbol;
use crate::value::Value;

impl Env {
    /// The record `key` (`__mutsu_type::x` and kin) of the variable it names,
    /// as the frame that binds the variable holds it. Any other key is an
    /// ordinary [`Self::get_sym`].
    // Cost: O(d), d = chain tiers walked, plus one lookup of the variable
    // below each caller frame that binds it.
    pub(crate) fn get_record_sym(&self, key: Symbol) -> Option<&Value> {
        let Some(subject) = key
            .shadow_meta_subject()
            .filter(|subject| subject.is_plain_user_lexical())
        else {
            return self.get_sym(key);
        };
        if let Some(v) = self.inner.get(&key) {
            return Some(v);
        }
        match crate::symbol::env_key_probe(key) {
            crate::symbol::EnvKeyProbe::Absent => None,
            crate::symbol::EnvKeyProbe::RoutineOwned if self.chain_has_routine_owner => {
                self.get_sym(key)
            }
            _ => self.walk_with_fallback(key, Some(subject)),
        }
    }
}

/// Whether a record walk that is leaving the caller frame rooted at `root`,
/// which binds `subject` to `mine` and holds no record of it, ends there: the
/// binding is the frame's own variable, not a mirror of the one below.
// Cost: O(d), d = tiers below `root`.
pub(super) fn leaves_variable(root: &Env, subject: Symbol, mine: &Value) -> bool {
    !root
        .parent
        .as_deref()
        .and_then(|below| below.get_sym(subject))
        .is_some_and(|theirs| same_variable(mine, theirs))
}
