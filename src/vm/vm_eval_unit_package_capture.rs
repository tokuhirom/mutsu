//! The captured file-scope lexicals of a package routine an EVAL unit
//! declared (#17944).
//!
//! An EVAL run inside a package (`module QQ { EVAL q[my $x; our sub f { $x }] }`)
//! or one that opens its own (`EVAL q[unit module QQ; ...]`) gives its routines
//! the unit's capture bucket, as an EVAL at `GLOBAL` does. Such a routine runs
//! in its package, whose own block lexicals live in the package store
//! (`package_scope_lexical`); a same-named `my` of the package must not answer
//! for the unit's, so the bucket is read ahead of that store.

use super::*;

impl Interpreter {
    /// The cell the running package routine captured for `name` from the
    /// EVAL unit that declared it, or `None` when the running routine is not
    /// such a routine or captured no `name`.
    // Cost: O(1) expected: the `active_unit_lexical_entry` probes plus one
    // bucket lookup; a program with no captured unit lexicals pays one
    // `is_empty` test.
    pub(super) fn eval_unit_package_capture(&self, name: &str) -> Option<Value> {
        if self.lexicals.mainline_lexical_subs.is_empty() {
            return None;
        }
        let frame = self.routine_stack().last()?;
        if frame.package == crate::symbol::wk::global_package() {
            return None;
        }
        let (key, cells) = self.active_unit_lexical_entry()?;
        if !key.starts_with(crate::runtime::EVAL_UNIT_CAPTURE_PREFIX) {
            return None;
        }
        cells.get(name).cloned()
    }
}
