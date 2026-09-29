//! Ancestry queries derived from [`crate::builtins::builtin_type_catalog`]
//! (ADR-0051 P2).
//!
//! The catalog keeps a type's class chain (`mro`) and the roles it composes
//! (`roles`) apart, because `.^mro` never lists a role. Two consumers need them
//! combined, and each used to answer from a private hand-written table:
//!
//! - **type membership** ("is an `Int` a `Cool`? a `Real`?"): smartmatch's
//!   static matcher, `isa_check`, signature `~~` and the compile-time default
//!   check — [`builtin_type_is_a`];
//! - **multi-dispatch narrowness** ("how far is `Numeric` above `Int`?"): the
//!   candidate-distance walk — [`builtin_type_narrowness_chain`], which places
//!   every role right after the last class in the MRO that composes it.

use super::builtin_type_catalog::{builtin_type_has_role, builtin_type_info};
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// `name` without a `[...]` parameterization (`Rational[Int,Int]` -> `Rational`).
fn base_name(name: &str) -> &str {
    name.split_once('[').map_or(name, |(base, _)| base)
}

/// Whether the builtin type `type_name` is `ancestor`, either nominally (a
/// class in its catalog MRO) or by composing it as a role. A name the catalog
/// has no row for answers `false`; callers keep their own rules for the
/// spellings the catalog does not model (native types, subsets, sized-buffer
/// aliases).
// Cost: O(m + r), m = MRO length of `type_name`, r = total roles along it.
pub(crate) fn builtin_type_is_a(type_name: &str, ancestor: &str) -> bool {
    let Some(info) =
        builtin_type_info(type_name).or_else(|| builtin_type_info(base_name(type_name)))
    else {
        return false;
    };
    info.mro.contains(&ancestor) || builtin_type_has_role(info.name, ancestor)
}

/// A builtin type's classes and roles in narrowness order, most specific
/// first, with role parameterizations stripped (`Rat` ->
/// `Rat, Rational, Real, Numeric, Cool, Any, Mu`). Multi-dispatch uses the
/// index as the candidate's distance, so a role sits right after the last
/// (least derived) class in the MRO whose catalog row still composes it: the
/// class that introduced it. `Array`'s `Positional` therefore follows `List`,
/// not `Array`, and an `IntStr`'s `Real` follows `Int`, not `IntStr`.
// Cost: O(1) amortized, hash lookup in a table built once per process.
pub(crate) fn builtin_type_narrowness_chain(type_name: &str) -> Option<&'static [&'static str]> {
    static CHAINS: OnceLock<FxHashMap<&'static str, Box<[&'static str]>>> = OnceLock::new();
    let chains = CHAINS.get_or_init(|| {
        super::builtin_type_catalog::all_builtin_type_names()
            .map(|name| (name, narrowness_chain_of(name)))
            .collect()
    });
    chains.get(type_name).map(|chain| &**chain)
}

fn narrowness_chain_of(name: &'static str) -> Box<[&'static str]> {
    let info = builtin_type_info(name).expect("catalog name has a row");
    // The roles each MRO level composes: the leaf's own row, and the row of
    // every ancestor the catalog knows (an ancestor without a row composes
    // nothing we can see).
    let level_roles: Vec<&'static [&'static str]> = info
        .mro
        .iter()
        .map(|class| builtin_type_info(class).map_or(&[][..], |row| row.roles))
        .collect();
    let mut chain: Vec<&'static str> = Vec::with_capacity(info.mro.len() + info.roles.len());
    for (level, class) in info.mro.iter().enumerate() {
        chain.push(class);
        for role in level_roles[level] {
            let role = base_name(role);
            let composed_further_up = level_roles[level + 1..]
                .iter()
                .any(|roles| roles.iter().any(|r| base_name(r) == role));
            if !composed_further_up && !chain.contains(&role) {
                chain.push(role);
            }
        }
    }
    chain.into_boxed_slice()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chain(name: &str) -> Vec<&'static str> {
        builtin_type_narrowness_chain(name)
            .unwrap_or_else(|| panic!("{name} has no chain"))
            .to_vec()
    }

    /// Rakudo's narrowness: `Real` is narrower than `Numeric` (it does
    /// `Numeric`), and a class's roles rank before its parent class.
    #[test]
    fn numeric_chains_put_real_before_numeric() {
        assert_eq!(
            chain("Int"),
            ["Int", "Real", "Numeric", "Cool", "Any", "Mu"]
        );
        assert_eq!(
            chain("Bool"),
            ["Bool", "Int", "Real", "Numeric", "Cool", "Any", "Mu"]
        );
        assert_eq!(
            chain("Rat"),
            ["Rat", "Rational", "Real", "Numeric", "Cool", "Any", "Mu"]
        );
        assert_eq!(
            chain("Complex"),
            ["Complex", "Numeric", "Cool", "Any", "Mu"]
        );
        assert_eq!(chain("Str"), ["Str", "Stringy", "Cool", "Any", "Mu"]);
    }

    /// A role inherited from a parent class ranks with that parent.
    #[test]
    fn inherited_roles_follow_the_class_that_composes_them() {
        assert_eq!(
            chain("Array"),
            [
                "Array",
                "List",
                "Positional",
                "Iterable",
                "Cool",
                "Any",
                "Mu"
            ]
        );
        assert_eq!(
            chain("Hash"),
            [
                "Hash",
                "Map",
                "Associative",
                "Iterable",
                "Cool",
                "Any",
                "Mu"
            ]
        );
        assert_eq!(
            chain("Sub"),
            ["Sub", "Routine", "Block", "Code", "Callable", "Any", "Mu"]
        );
        assert_eq!(
            chain("IntStr"),
            [
                "IntStr",
                "Allomorph",
                "Str",
                "Stringy",
                "Int",
                "Real",
                "Numeric",
                "Cool",
                "Any",
                "Mu"
            ]
        );
    }

    /// `Pair` is not `Cool` and `Seq` is not `Positional` in Rakudo; the
    /// hand-written chains this replaced claimed both.
    #[test]
    fn chains_carry_no_ancestor_rakudo_denies() {
        assert!(!chain("Pair").contains(&"Cool"));
        assert!(!chain("Seq").contains(&"Positional"));
    }

    #[test]
    fn is_a_reads_mro_and_roles() {
        assert!(builtin_type_is_a("Int", "Cool"));
        assert!(builtin_type_is_a("Bool", "Int"));
        assert!(builtin_type_is_a("Rat", "Rational"));
        assert!(builtin_type_is_a("Match", "Cool"));
        assert!(builtin_type_is_a("Seq", "Cool"));
        assert!(builtin_type_is_a("Array[Int]", "Positional"));
        assert!(!builtin_type_is_a("Capture", "Cool"));
        assert!(!builtin_type_is_a("Pair", "Cool"));
        assert!(!builtin_type_is_a("Date", "Cool"));
        assert!(!builtin_type_is_a("NoSuchType", "Any"));
    }
}
