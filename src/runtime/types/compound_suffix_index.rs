//! A negative filter for relative compound type names (#18274).
//!
//! `resolve_relative_compound_type(name)` asks, for every package enclosing
//! the running code, whether `Pkg::name` is a registered type. That walk is
//! asked on every type-check memo hit whose type name is package-qualified
//! (`Metamodel::GenericHOW`, `Metamodel::ClassHOW`): about four running
//! package candidates times their nesting depth, a registry read and four
//! hash probes each, all missing for a core name.
//!
//! `Pkg::name` can be an exact key of a type table only if that key ends in
//! `::name`. So the set of every proper `::`-suffix of every key of the four
//! tables `has_type_direct` asks (classes, roles, enums, subsets) answers
//! "the walk cannot succeed" with one probe. The set is rebuilt when any of
//! the tables' key generations moves; a value-only update keeps it.

use super::*;

/// The suffix set and the `(classes, roles, enum_types, subsets)` key
/// generations it was built from.
pub(crate) struct CompoundSuffixes {
    generations: (u64, u64, u64, u64),
    suffixes: rustc_hash::FxHashSet<Box<str>>,
}

impl Interpreter {
    /// Whether some registered type name could be `<package>::name`: `false`
    /// means no `Pkg::name` is a key of the class, role, enum or subset
    /// table, for any `Pkg`. A parameterized spelling (`R[Int]`) is answered
    /// `true`, because `has_type_direct` also matches its base.
    // Cost: O(1) expected while the tables' key sets are unchanged; a key
    // change rebuilds the set once, O(t * s), t = registered types, s = their
    // `::` segments.
    pub(crate) fn registered_type_suffix_possible(&self, name: &str) -> bool {
        if name.contains('[') {
            return true;
        }
        let reg = self.registry();
        let generations = (
            reg.classes.key_generation(),
            reg.roles.key_generation(),
            reg.enum_types.key_generation(),
            reg.subsets.key_generation(),
        );
        let mut cache = self.caches.compound_type_suffixes.borrow_mut();
        if cache.as_ref().is_none_or(|c| c.generations != generations) {
            let mut suffixes = rustc_hash::FxHashSet::default();
            let keys = reg
                .classes
                .keys()
                .chain(reg.roles.keys())
                .chain(reg.enum_types.keys())
                .chain(reg.subsets.keys());
            for key in keys {
                let mut rest = key.as_str();
                while let Some(at) = rest.find("::") {
                    rest = &rest[at + 2..];
                    suffixes.insert(Box::<str>::from(rest));
                }
            }
            *cache = Some(CompoundSuffixes {
                generations,
                suffixes,
            });
        }
        cache
            .as_ref()
            .is_some_and(|c| c.suffixes.contains(name))
    }
}
