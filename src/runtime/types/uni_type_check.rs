//! A bare `Uni` checked against its own core type, answered from the global
//! type tables alone (#8673).
//!
//! JSON::Fast's `unjsonify-string(Uni:D \codes, int $pos)` is bound once per
//! escaped JSON string, each time with the `NFD` that `nqp::strtocodes` just
//! filled. The general check reaches its answer -- the value's tag names the
//! constraint -- only after the whole resolution gauntlet: the name's type
//! capture, package alias, lexical type and `constant` alias lookups, ~2.6k
//! instructions per bind. And because some of those lookups read the current
//! scope, a TRIR call with such a parameter had to materialize its deferred
//! routine frames first (`trir_observe`).
//!
//! For a core `Uni` class name every one of those lookups is decided by a
//! table that does not depend on the scope:
//!
//! - a type capture rebinds a name only once some `::T` was bound in the
//!   process (`any_type_capture_seen`);
//! - a package alias only when the union of every package's alias keys
//!   holds the name (`package_type_aliases.contains_name`);
//! - a lexical type or a `constant` alias never shadows a name of the core
//!   catalogue (`bound_lexical_type_storage` and `type_alias_target` return
//!   `None` for one);
//! - a `subset` of the name is in the registry.
//!
//! When none applies, the general check's answer for a `Uni` is its tag
//! match: the constraint is `Uni` (every normalization form derives from it)
//! or the value's own form. [`Interpreter::uni_accepts_by_own_type`] is that
//! answer, and it says `false` -- "not decided here" -- whenever any table
//! could make the general check differ.

use crate::runtime::Interpreter;
use crate::value::Value;

impl Interpreter {
    /// Whether the bare `Uni` `value` satisfies `constraint` by its own type,
    /// decided without reading the current scope (see the module docs).
    /// `false` means only that the general check has to decide.
    // Cost: O(n) in the name's length, plus three hash probes and a binary
    // search of the core catalogue.
    pub(crate) fn uni_accepts_by_own_type(&self, constraint: &str, value: &Value) -> bool {
        if value.as_uni().is_none() {
            return false;
        }
        if constraint != "Uni" && constraint != crate::runtime::value_type_name(value) {
            return false;
        }
        !Self::any_type_capture_seen()
            && crate::runtime::utils::is_known_type_constraint(constraint)
            && crate::builtin_types::catalog::builtin_type_info(constraint).is_some()
            && !self.is_subset_type_name(constraint)
            && !self.types.package_type_aliases.contains_name(constraint)
    }
}
