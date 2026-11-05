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
//! [`Env::get_record_sym`] answers instead with the record of the variable
//! the running code sees: the walk ends at the frame root of a routine that
//! has a lexical outer of its own (`Env::walk_with_fallback`), and a block or
//! closure frame, whose outer is the frame that ran it, keeps walking. The
//! frame the lookup starts in keeps its own rules (the static link and the
//! capture, `Env::get_sym`); a slot the running code declares reads its record
//! in its frame only (`Env::get_record_in_frame`). A sigilless alias parameter
//! carries its source's record under its own name (the binder writes it).

use super::Env;
use crate::symbol::Symbol;
use crate::value::Value;

impl Env {
    /// The record `key` (`__mutsu_type::x` and kin) of the variable it names,
    /// as the frame that binds the variable holds it. Any other key is an
    /// ordinary [`Self::get_sym`].
    // Cost: O(d), d = chain tiers walked.
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
