//! What introspection says about a built-in type's methods (ADR-14032).
//!
//! `.^can`, `.^find_method`/`.^lookup`, `.^methods` and the compile-time
//! `augment` conflict answer two questions about a built-in owner: does Rakudo
//! declare or list the method there, and does mutsu implement it. The first is
//! read from Rakudo itself: `rakudo_method_tables.txt`, the committed output of
//! `scripts/gen-rakudo-method-tables.raku`, which records per owner the keys
//! of `::(Owner).^method_table` (`declared`), the names `::(Owner).^methods`
//! lists (`methods`), the `declared` names that are `only` methods (`only`), and
//! the names language revision 6.e adds to each (`declared.e`, ...). The
//! second is the method table: a name is answered only where a row reaches
//! it. An owner the snapshot does not list (a role such as `Blob`, a class
//! mutsu models on its own) answers from its rows alone.
//!
//! The `augment` conflict reads the snapshot without the rows: Rakudo refuses
//! to redeclare a method it declares whether or not mutsu implements it.
//!
//! The snapshot is parsed on the first introspection call, never on the
//! startup path. It replaced the hand-kept recognition table
//! (`native_method_row_table.rs`), whose flags were baked from Rakudo once and
//! drifted from both Rakudo and the rows (#11271).

use crate::builtins::method_table::{owner_has_row, owner_row_names};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::OnceLock;

const SNAPSHOT: &str = include_str!("rakudo_method_tables.txt");

/// One list of the snapshot: its names in order, and the same as a set.
#[derive(Default)]
struct NameList {
    ordered: Vec<&'static str>,
    set: FxHashSet<&'static str>,
}

impl NameList {
    fn extend(&mut self, names: impl Iterator<Item = &'static str>) {
        for name in names {
            if self.set.insert(name) {
                self.ordered.push(name);
            }
        }
    }
}

/// An owner's lists: index 0 is the base revision's, index 1 the names 6.e
/// adds.
#[derive(Default)]
struct OwnerFacts {
    declared: [NameList; 2],
    methods: [NameList; 2],
    only: [NameList; 2],
}

/// Which list of an owner a question reads.
#[derive(Clone, Copy)]
enum Kind {
    Declared,
    Methods,
    Only,
}

impl OwnerFacts {
    fn lists(&self, kind: Kind) -> &[NameList; 2] {
        match kind {
            Kind::Declared => &self.declared,
            Kind::Methods => &self.methods,
            Kind::Only => &self.only,
        }
    }

    /// Whether `name` is in the `kind` list of the program's revision.
    // Cost: O(1), at most two set probes and a thread-local read.
    fn has(&self, kind: Kind, name: &str) -> bool {
        let [base, e] = self.lists(kind);
        base.set.contains(name) || (e.set.contains(name) && revision_e())
    }

    /// The `kind` list of the program's revision, in snapshot order.
    // Cost: O(n), n = names of the list.
    fn names(&self, kind: Kind) -> Vec<&'static str> {
        let [base, e] = self.lists(kind);
        let mut names = base.ordered.clone();
        if revision_e() {
            names.extend(e.ordered.iter().filter(|name| !base.set.contains(*name)));
        }
        names
    }
}

/// Whether the program's language revision is 6.e.
// Cost: O(1), a thread-local read.
fn revision_e() -> bool {
    crate::parser::current_language_version_starts_with("6.e")
}

/// The snapshot, by owner. Parsed on the first introspection call.
// Cost: O(1) after the first call; that call is O(s), s = bytes of the snapshot.
fn facts() -> &'static FxHashMap<&'static str, OwnerFacts> {
    static FACTS: OnceLock<FxHashMap<&'static str, OwnerFacts>> = OnceLock::new();
    FACTS.get_or_init(|| {
        let mut facts: FxHashMap<&'static str, OwnerFacts> = FxHashMap::default();
        for line in SNAPSHOT.lines().filter(|line| !line.starts_with('#')) {
            let mut fields = line.split('\t');
            let (Some(kind), Some(owner)) = (fields.next(), fields.next()) else {
                continue;
            };
            let (kind, revision) = match kind.strip_suffix(".e") {
                Some(base) => (base, 1),
                None => (kind, 0),
            };
            let entry = facts.entry(owner).or_default();
            let list = match kind {
                "declared" => &mut entry.declared[revision],
                "methods" => &mut entry.methods[revision],
                "only" => &mut entry.only[revision],
                _ => continue,
            };
            list.extend(fields.filter(|name| !name.is_empty()));
        }
        facts
    })
}

/// Whether a row reaches `name` from `owner`: a row of `owner` or of a class
/// in its MRO, and with `roles`, of a role one of them composes.
// Cost: O(m * r * k), m = MRO length, r = roles per class, k = rows named `name`.
fn row_reaches(owner: &str, name: &str, roles: bool) -> bool {
    if owner_has_row(owner, name) {
        return true;
    }
    let Some(mro) = crate::builtin_types::catalog::builtin_type_mro_strs(owner) else {
        return false;
    };
    mro.iter().any(|class| {
        owner_has_row(class, name)
            || (roles
                && crate::builtin_types::catalog::builtin_type_roles_strs(class)
                    .iter()
                    .any(|role| {
                        owner_has_row(role.split_once('[').map_or(role, |(base, _)| base), name)
                    }))
    })
}

/// Whether Rakudo declares `name` directly on the built-in `owner` (it is a
/// key of `::(owner).^method_table`) and a row of `owner` implements it; for
/// an owner the snapshot does not list, whether a row of `owner` does.
/// `owner` is matched exactly, never folded: `.^can` asks once per MRO level.
// Cost: O(k), k = rows named `name`, plus two set probes.
pub(crate) fn native_method_declared(owner: &str, name: &str) -> bool {
    owner_has_row(owner, name)
        && facts()
            .get(owner)
            .is_none_or(|facts| facts.has(Kind::Declared, name))
}

/// Whether `owner` has the method `name` at all: declared there
/// ([`native_method_declared`]) or listed by its `.^methods` and reached by a
/// row (a method of a role it composes, `List.of`).
// Cost: as `row_reaches`.
pub(crate) fn native_method_row_exists(owner: &str, name: &str) -> bool {
    native_method_declared(owner, name)
        || match facts().get(owner) {
            Some(facts) => facts.has(Kind::Methods, name) && row_reaches(owner, name, true),
            None => false,
        }
}

/// #10234: whether Rakudo declares `name` on `owner` as an `only` method,
/// which an `augment` of `owner` may not redeclare. Rakudo's fact alone.
// Cost: O(1), three set probes.
pub(crate) fn native_method_declared_only(owner: &str, name: &str) -> bool {
    facts()
        .get(owner)
        .is_some_and(|facts| facts.has(Kind::Declared, name) && facts.has(Kind::Only, name))
}

/// #10234: the compile-time error Rakudo raises when `augment class owner`
/// declares a method `name` that `owner` itself already declares. A plain,
/// `only` or `proto` method cannot be added next to any method of that name
/// in `owner`'s own method table, multi dispatcher or not (`augment class Str
/// { method uc {...} }` dies although `Str.uc` is a multi). A `multi`
/// candidate can join a multi, but not an `only` method. `None` when the
/// declaration is legal, including for a name `owner` only inherits
/// (`Array.sort`, declared on `List`). Rakudo's facts alone, whether or not
/// mutsu implements the method; an owner the snapshot does not list answers
/// from its rows.
// Cost: O(1) expected (set probes).
pub(crate) fn augment_core_method_conflict(
    owner: &str,
    name: &str,
    is_multi: bool,
) -> Option<String> {
    if is_multi {
        native_method_declared_only(owner, name).then(|| {
            format!(
                "Cannot have a multi candidate for '{name}' when an only method is also in the package '{owner}'"
            )
        })
    } else {
        let declared = match facts().get(owner) {
            Some(facts) => facts.has(Kind::Declared, name),
            None => owner_has_row(owner, name),
        };
        declared.then(|| {
            format!(
                "Package '{owner}' already has a method '{name}' (did you mean to declare a multi method?)"
            )
        })
    }
}

/// The names Rakudo declares directly on `owner` that a row of `owner` or of a
/// class in its MRO implements (`Map` declares `Bag`, mutsu's row is `Any`'s),
/// in snapshot order; for an owner the snapshot does not list, its rows'
/// names. `.^methods` lists these as the class's own method table.
// Cost: O(n * m * k), n = names declared, m = MRO length, k = rows per name.
pub(crate) fn declared_names_for_owner(owner: &str) -> Vec<&'static str> {
    match facts().get(owner) {
        Some(facts) => facts
            .names(Kind::Declared)
            .into_iter()
            .filter(|name| row_reaches(owner, name, false))
            .collect(),
        None => owner_row_names(owner),
    }
}

/// The `.^methods` names of a built-in `owner` (already run through
/// `canonical_builtin_owner`): the names Rakudo's `.^methods` lists that a row
/// of the owner or of a class in its MRO answers, in snapshot order; for an
/// owner the snapshot does not list, its rows' names. A composed role's own
/// methods are listed by the caller, with the role as their package.
// Cost: O(n * m * r * k) (see `row_reaches`), n = names listed.
pub(crate) fn introspectable_names_for_owner(owner: &str) -> Vec<&'static str> {
    match facts().get(owner) {
        Some(facts) => facts
            .names(Kind::Methods)
            .into_iter()
            .filter(|name| row_reaches(owner, name, false))
            .collect(),
        None => owner_row_names(owner),
    }
}

/// Whether `name` is in [`introspectable_names_for_owner`]`(owner)`, without
/// building the list.
// Cost: as `row_reaches`.
pub(crate) fn introspectable_name_listed(owner: &str, name: &str) -> bool {
    match facts().get(owner) {
        Some(facts) => facts.has(Kind::Methods, name) && row_reaches(owner, name, false),
        None => owner_has_row(owner, name),
    }
}

/// Whether language revision 6.e adds `name` to `owner`'s method table: the
/// `declared.e` names a `REVISION_E` method row is checked against.
#[cfg(test)]
pub(crate) fn declared_in_revision_e(owner: &str, name: &str) -> bool {
    facts()
        .get(owner)
        .is_some_and(|facts| facts.declared[1].set.contains(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_snapshot_parses() {
        assert!(facts()["Rat"].has(Kind::Declared, "numerator"));
        assert!(facts()["Int"].has(Kind::Methods, "FatRat"));
        assert!(facts()["Str"].has(Kind::Only, "Int"));
        assert!(!facts()["Str"].has(Kind::Only, "uc"));
    }

    #[test]
    fn a_declared_method_needs_a_row() {
        assert!(native_method_declared("Str", "chars"));
        assert!(native_method_declared("Rat", "numerator"));
        assert!(!native_method_declared("NoSuchOwner", "no-such-method"));
        // Rakudo declares it, but mutsu has no row for it.
        assert!(!native_method_declared("Any", "iterator-and-first"));
    }

    #[test]
    fn the_augment_conflict_is_rakudo_s() {
        assert!(augment_core_method_conflict("Str", "uc", false).is_some());
        assert!(augment_core_method_conflict("Str", "Int", true).is_some());
        assert!(augment_core_method_conflict("Str", "uc", true).is_none());
        // Inherited, not declared.
        assert!(augment_core_method_conflict("Array", "sort", false).is_none());
        // Declared by Rakudo, whether or not mutsu implements it.
        assert!(augment_core_method_conflict("Any", "iterator-and-first", false).is_some());
    }

    #[test]
    fn introspection_lists_only_what_a_row_reaches() {
        let int = introspectable_names_for_owner("Int");
        assert!(int.contains(&"FatRat"));
        assert!(int.iter().all(|name| row_reaches("Int", name, false)));
    }
}
