//! The declared types of a unit's own file-scope `@`/`%` lexicals (#13086).
//!
//! A `unit module`'s body runs in its loader's env. Its file-scope lexicals
//! move out of that env into `unit_lexicals` when the load ends, but their
//! declared element and key types live in env metadata lanes
//! (`__mutsu_type::%seen`, `__mutsu_hash_key_type::%seen`) under qualified
//! names, which nothing took out. They stayed in the importer's env, so the
//! next module declaring a same-named typed hash overwrote them, and the first
//! module's routines checked their stores against the second module's types:
//!
//! ```text
//!   unit module MA; my %seen{List}; sub put($k) is export { %seen{$k} = 1 }
//!   unit module MB; my %seen{Hash}; ...
//!   use MA; use MB; put((1, 2));   # "expected Hash but got List"
//! ```
//!
//! So leaving a unit body records each `@`/`%` lexical's lanes per unit, as
//! `vm_class_body_static_types` does per class, and puts the importer's lanes
//! back. A constraint read that finds no lane in the env asks the running
//! unit's record, through the same package walk that finds the unit's
//! lexical itself (`unit_lexical_slot`).

use super::*;

impl Interpreter {
    /// The env metadata lanes of a unit's `@`/`%` lexical `name`: its element
    /// type and, for a hash, its key type. Empty for a scalar or code name.
    // Cost: O(|name|), at most two meta-key derivations.
    fn unit_lexical_meta_lane_keys(name: &str) -> Vec<Symbol> {
        if !name.starts_with(['@', '%']) {
            return Vec::new();
        }
        let sym = Symbol::intern(name);
        let mut lanes = vec![Self::type_meta_key_for_sym(sym)];
        if name.starts_with('%') {
            lanes.push(Self::hash_key_meta_key_for_sym(sym));
        }
        lanes
    }

    /// Before a unit body runs: the importer's lanes for the unit's `@`/`%`
    /// lexicals `names`, for [`Self::leave_unit_lexical_meta_lanes`] to put
    /// back.
    // Cost: O(n), n = `names`, one env probe per lane.
    pub(crate) fn snapshot_unit_lexical_meta_lanes(
        &self,
        names: &[String],
    ) -> Vec<(Symbol, Option<Value>)> {
        names
            .iter()
            .flat_map(|name| Self::unit_lexical_meta_lane_keys(name))
            .map(|lane| (lane, self.env().get_sym(lane).cloned()))
            .collect()
    }

    /// Leaving the body of `unit`: record the lanes the body declared for its
    /// `@`/`%` lexicals under `unit`, then restore the importer's lanes from
    /// `saved` (taken by [`Self::snapshot_unit_lexical_meta_lanes`]).
    // Cost: O(n), n = lanes in `saved`, one env probe and one insert or remove
    // each.
    pub(crate) fn leave_unit_lexical_meta_lanes(
        &mut self,
        unit: &str,
        saved: Vec<(Symbol, Option<Value>)>,
    ) {
        let declared: Vec<(String, Value)> = saved
            .iter()
            .filter_map(|(lane, _)| {
                let value = self.env().get_sym(*lane)?;
                matches!(value.view(), ValueView::Str(_)).then(|| (lane.resolve(), value.clone()))
            })
            .collect();
        if !declared.is_empty() {
            crate::runtime::cow_table_mut(&mut self.lexicals.unit_lexical_meta_lanes)
                .entry(unit.to_string())
                .or_default()
                .extend(declared);
        }
        for (lane, previous) in saved {
            match previous {
                Some(value) => {
                    self.env_mut().insert_sym(lane, value);
                }
                None => {
                    self.env_mut().remove_sym(lane);
                }
            }
        }
    }

    /// The declared-type lane `lane` a constraint read checks against.
    ///
    /// A unit's routine runs with its loader's frames below it, and a same-named
    /// `@`/`%` lexical of the loader has a lane there, so the running unit's
    /// record must answer ahead of the caller chain. A lane the running frame
    /// declares itself (a routine-local `my %seen{Str}`) answers ahead of both.
    // Cost: O(1) env probe when no unit recorded a lane of that name;
    // otherwise O(d) for the frame's tiers plus `unit_lexical_meta_lane`.
    #[inline]
    pub(crate) fn declared_meta_lane(&self, lane: Symbol) -> Option<&Value> {
        let table = &self.lexicals.unit_lexical_meta_lanes;
        if table.is_empty() || !table.contains_name(lane.as_str()) {
            return self.env().get_record_sym(lane);
        }
        self.env()
            .get_sym_in_frame(lane)
            .or_else(|| self.unit_lexical_meta_lane(lane))
            .or_else(|| self.env().get_record_sym(lane))
    }

    /// The running unit's recorded lane `lane` (a `Str` type name).
    // Cost: O(1) when no unit recorded a lane of that name; otherwise O(c·d),
    // c = candidate packages (at most four), d = their `::` depth.
    pub(crate) fn unit_lexical_meta_lane(&self, lane: Symbol) -> Option<&Value> {
        let table = &self.lexicals.unit_lexical_meta_lanes;
        let lane_name = lane.as_str();
        if table.is_empty() || !table.contains_name(lane_name) {
            return None;
        }
        if let Some(unit) = self.running_compunit_lexical_unit()
            && let Some(found) = table.get(unit.as_str()).and_then(|m| m.get(lane_name))
        {
            return Some(found);
        }
        let owner = self.routine_stack().last().and_then(|frame| {
            self.lexical_package_for_frame(frame.def_file)
                .or(frame.lexical_package)
        });
        self.unit_lexical_candidates(self.current_package_sym())
            .into_iter()
            .flatten()
            .find_map(|candidate| {
                Self::lookup_unit_lexical_chain(table, candidate.as_str(), lane_name, owner)
            })
    }
}
