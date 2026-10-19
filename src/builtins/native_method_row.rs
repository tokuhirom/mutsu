//! ADR-0019 Phase E, box E2a: recognition metadata for native method dispatch.
//!
//! `native_method_0arg`/`native_method_1arg` (and, until ADR-11276 slice 5, `native_method_2arg`)
//! (`builtins/methods_0arg/`, `builtins/methods_narg/`) decide by matching on the
//! method-name string whether a call is served by the pure native layer. Phase
//! E's resolver needs to ask the same question ("does (owner, name) admit a
//! native call, and at which arity?") WITHOUT invoking the cascade -- that is
//! recognition metadata, not invocation, and it is what `NativeMethodRow`
//! records. See adr0019-e2-e4-resolver-core (#7540) decision 1.
//!
//! Invocation stays in the arity cascades until Phase F retires them.
//! `.^can` (via [`native_method_row_exists`] and [`native_method_declared`]) and,
//! since ADR-0019 Phase F box F3 step 3,
//! `builtin_type_methods::builtin_type_method_names` (`.^methods`
//! enumeration, via [`introspectable_names_for_owner`]) are the production
//! readers (the arity reader and the resolver's `Native` candidate are gone). E2a's rows are a deliberately conservative first
//! pass, generated once (2026-08-10) by probing every (owner, name) pair
//! from the (now-retired) 14 built-in-method name slices against the real
//! native dispatch cascades with a representative sample value per owner,
//! plus a type-object probe for the `TYPE_OBJECT_OK` flag. A pair recognized at NO arity is conservatively classified
//! `N`/`SPECIAL` (or `MUTATES_RECEIVER` when the name also appears in the
//! Tier-A mutable-method dispatch, `vm/vm_call_method_mut_ops.rs`) --
//! exactly mirroring what the retired `native_call_unmodeled` counter flagged
//! as unrecognized when a real call reached that (owner, name) pair. Per
//! the doc comment on `builtin_type_methods`, row *generation* must stay
//! static (no native-method invocation during real `Interpreter`
//! construction); the probing that produced
//! [`RAW_ROWS`](super::native_method_row_table::RAW_ROWS) below ran once in a
//! throwaway `#[test]`, and its output was pasted here as plain data --
//! production code never calls a native method to build this table. The
//! `INTROSPECTABLE` flag (F3 step 2/3) was added the same way: raku-verified
//! by hand, then baked into the table once via a throwaway generator test.
//! The `DECLARED` flag (#9869) was baked from Rakudo the same way: set exactly
//! when `::(owner).^method_table` has the name (the table `.^can` walks).
//!
//! E2b drove the gap between this conservative table and the cascades'
//! actual behavior to zero (see the design doc's classification table and the
//! `native_call_unmodeled` counter that box wired up; ADR-11276 slice 5
//! retired it with the cascades it measured).
//!
//! The one-off baking left nothing keeping the `DECLARED` / `INTROSPECTABLE`
//! bits in step with methods added to the cascades later, and `.^can` denied
//! them (#11271). `native_method_row_rakudo_oracle` now checks the bits
//! against a committed snapshot of Rakudo's method tables on every test run.

#[cfg(test)]
use super::builtin_type_methods::builtin_method_entries;
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// Method-identity facts a resolver needs about a native row, independent of
/// any particular receiver's runtime state (see the design doc's admission-gate
/// classification table, decision 3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct NativeRowFlags(pub(crate) u8);

impl NativeRowFlags {
    // Bits 0-2 of a table row (TYPE_OBJECT_OK, MUTATES_RECEIVER, SPECIAL) described the
    // retired arity cascades; nothing reads them, and the scripts that regenerate the
    // table still write them.
    /// ADR-0019 Phase F box F3 step 3: this `(owner, name)` pair is a genuine
    /// `.^methods` entry on the built-in type, not merely a name the arity
    /// cascades happen to recognize. Set exactly on the rows that were
    /// raku-verified during F3 step 2's owner-by-owner triage (recorded in
    /// this ADR's progress notes); the many extra dispatch-recognized names
    /// `RAW_ROWS` also carries per owner (confirmed dispatch-only, e.g.
    /// `Str.perl`, `Hash.AT-POS`) do NOT get this bit.
    /// `builtin_type_methods::builtin_type_method_names` is the sole
    /// production reader, replacing the fourteen hand-written per-type name
    /// slices that function used to concatenate.
    pub(crate) const INTROSPECTABLE: NativeRowFlags = NativeRowFlags(1 << 3);
    /// #9869: Rakudo declares `name` directly on `owner` -- the name is a
    /// key of `::(owner).^method_table`, the per-class table `.^can` walks
    /// along the MRO. Distinct from [`Self::INTROSPECTABLE`], which records
    /// that `::(owner).^methods` lists the name locally; inherited names are
    /// found through the MRO for `.^methods(:all)`. Baked once from Rakudo
    /// by a throwaway generator, like the other flags. Read by
    /// [`native_method_declared`].
    pub(crate) const DECLARED: NativeRowFlags = NativeRowFlags(1 << 4);
    /// #10234: the method Rakudo declares as `name` on `owner` (a
    /// [`Self::DECLARED`] row) is an `only` method, not a multi dispatcher,
    /// so an `augment` of `owner` may not redeclare it. Checked against the
    /// `only` lines of `rakudo_method_tables.txt`. Read by
    /// [`native_method_declared_only`].
    pub(crate) const ONLY_METHOD: NativeRowFlags = NativeRowFlags(1 << 5);
    /// Rakudo declares the method only in language revision 6.e (its
    /// `core.e` setting; ADR-11276 §9.146), so under an older revision every
    /// reader of the catalog treats the row as absent: `Any.^can('snip')` is
    /// false under 6.d. Checked against the `declared.e` / `methods.e` lines
    /// of `rakudo_method_tables.txt`.
    pub(crate) const REVISION_E: NativeRowFlags = NativeRowFlags(1 << 6);

    pub(crate) const fn contains(self, bit: NativeRowFlags) -> bool {
        self.0 & bit.0 != 0
    }

    /// Whether the row exists in the program's language revision.
    // Cost: O(1) (a thread-local read for a `REVISION_E` row).
    pub(crate) fn in_force(self) -> bool {
        !self.contains(Self::REVISION_E)
            || crate::parser::current_language_version_starts_with("6.e")
    }
}

/// `(owner, name, arity_bits, flag_bits)`, generated 2026-08-10 -- see the
/// module doc comment for how. Not hand-maintained; regenerate by re-running
/// the probe (see the ADR PR for the generating snippet) rather than
/// hand-editing rows in place, the same discipline `builtin_type_methods.rs`
/// uses for its name lists.
use super::native_method_row_table::rows;

type RowKey = (&'static str, &'static str);
type RowValue = (u8, u8);

// Cost: O(1) after the first call; that call is O(r), r = rows in the table.
// Built on the startup path, so it hashes with Fx (the keys are static
// strings of our own, not attacker-chosen) and is sized once.
fn classification_table() -> &'static FxHashMap<RowKey, RowValue> {
    static TABLE: OnceLock<FxHashMap<RowKey, RowValue>> = OnceLock::new();
    TABLE.get_or_init(|| {
        let rows = rows();
        let mut table = FxHashMap::with_capacity_and_hasher(rows.len(), Default::default());
        table.extend(
            rows.iter()
                .map(|&(owner, name, arity, flags)| ((owner, name), (arity, flags))),
        );
        table
    })
}

/// The catalog entry of `(owner, name)`, unless its method does not exist in
/// the program's language revision ([`NativeRowFlags::in_force`]).
// Cost: O(1), one hash probe.
fn entry(owner: &str, name: &str) -> Option<RowValue> {
    classification_table()
        .get(&(owner, name))
        .copied()
        .filter(|&(_, flags)| NativeRowFlags(flags).in_force())
}

/// Whether the catalog has an EXPLICIT recognition row for `(owner, name)` --
/// i.e. E2a/E2b's probe actually classified this pair as a real method on
/// `owner`, at some arity, via some path. ADR-0019 Phase E box E7 step 4
/// (`.^can`, `todo/deep/adr0019-e5-e7-entry-routing.md` "E7 step 4") is the
/// first production reader, and needs exactly this "does this method exist
/// at all" question -- which the arity/flags a row used to answer cannot
/// answer by itself: a missing key falls back to the same `(N, SPECIAL)` bit
/// pattern a genuinely-probed "exists, but only via a special/mutating path
/// outside the pure arity cascades" row also carries (e.g. `Str.match`,
/// `List.push` are both `(N, ...)` despite being real methods), so checking
/// the returned mask can never distinguish "not a method" from "a method,
/// just not reachable by `native_method_{0,1,2}arg`". Presence in the table
/// is the only signal E2's data actually carries for that question.
pub(crate) fn native_method_row_exists(owner: &'static str, name: &'static str) -> bool {
    entry(owner, name).is_some()
}

/// #9869: whether Rakudo declares `name` directly on the built-in `owner`
/// (its row carries [`NativeRowFlags::DECLARED`]). `owner` is matched
/// exactly, never folded through `canonical_builtin_owner`: `.^can` asks
/// this once per MRO level, and folding would make `Sub`, `Routine`,
/// `Block` and `Code` all claim `Code`'s declarations.
// Cost: O(1), a hash lookup keyed by `(owner, name)`.
pub(crate) fn native_method_declared(owner: &str, name: &str) -> bool {
    entry(owner, name)
        .is_some_and(|(_, flags)| NativeRowFlags(flags).contains(NativeRowFlags::DECLARED))
}

/// #10234: Rakudo declares `name` directly on `owner` as an `only` method
/// ([`NativeRowFlags::ONLY_METHOD`]). `owner` is the exact type, never
/// folded: a name an owner only inherits (`Array.sort`, declared on `List`)
/// is free to augment.
// Cost: O(1) expected (one hash probe).
pub(crate) fn native_method_declared_only(owner: &str, name: &str) -> bool {
    entry(owner, name).is_some_and(|(_, flags)| {
        let flags = NativeRowFlags(flags);
        flags.contains(NativeRowFlags::DECLARED) && flags.contains(NativeRowFlags::ONLY_METHOD)
    })
}

/// #10234: the compile-time error Rakudo raises when `augment class owner`
/// declares a method `name` that `owner` itself already declares. A plain,
/// `only` or `proto` method cannot be added next to any method of that name
/// in `owner`'s own method table, multi dispatcher or not (`augment class Str
/// { method uc {...} }` dies although `Str.uc` is a multi). A `multi`
/// candidate can join a multi, but not an `only` method. `None` when the
/// declaration is legal, including for a name `owner` only inherits
/// (`Array.sort`, declared on `List`; `Str.FatRat`, declared on `Cool`).
// Cost: O(1) expected (one hash probe).
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
        native_method_declared(owner, name).then(|| {
            format!(
                "Package '{owner}' already has a method '{name}' (did you mean to declare a multi method?)"
            )
        })
    }
}

/// The names Rakudo declares directly on `owner` (the rows carrying
/// [`NativeRowFlags::DECLARED`]), in table order. `owner` is the exact catalog
/// type, never folded. `.^methods` lists these for the classes whose
/// introspectable triage ([`introspectable_names_for_owner`]) is incomplete
/// (`Map`, `Pair`, `Set`, ...): the class's own method table.
// Cost: O(r), r = rows in the catalog (a scan; introspection only).
pub(crate) fn declared_names_for_owner(owner: &str) -> Vec<&'static str> {
    rows()
        .iter()
        .filter(|&&(o, _, _, flags)| {
            let flags = NativeRowFlags(flags);
            o == owner && flags.contains(NativeRowFlags::DECLARED) && flags.in_force()
        })
        .map(|&(_, name, _, _)| name)
        .collect()
}

/// ADR-0019 Phase F box F3 step 3: the `.^methods` name list for a *folded*
/// built-in owner (already run through `canonical_builtin_owner`), read
/// straight off [`rows`] in table order. This is what
/// `builtin_type_methods::builtin_type_method_names` now delegates to instead
/// of concatenating hand-written per-type name slices -- the introspectable
/// bit on each row (set once, F3 step 2's raku-verified triage) is the only
/// thing that used to live in those slices.
pub(crate) fn introspectable_names_for_owner(owner: &str) -> Vec<&'static str> {
    owner_row_index()
        .get(owner)
        .map(|rows| {
            rows.iter()
                .filter(|&&(_, flags)| {
                    let flags = NativeRowFlags(flags);
                    flags.contains(NativeRowFlags::INTROSPECTABLE) && flags.in_force()
                })
                .map(|&(name, _)| name)
                .collect()
        })
        .unwrap_or_default()
}

type OwnerRows = FxHashMap<&'static str, Vec<(&'static str, u8)>>;

// Cost: O(1) after the first call; that call is O(r), r = rows in the table.
// Groups `(name, flags)` by owner in table order so per-owner enumeration is
// O(k), k = rows of that owner, instead of a scan of the whole table.
fn owner_row_index() -> &'static OwnerRows {
    static INDEX: OnceLock<OwnerRows> = OnceLock::new();
    INDEX.get_or_init(|| {
        let mut index = OwnerRows::default();
        for &(owner, name, _, flags) in rows() {
            index.entry(owner).or_default().push((name, flags));
        }
        index
    })
}

/// Whether `name` is in [`introspectable_names_for_owner`]`(owner)`: the
/// membership test without building the list. `owner` is already folded.
// Cost: O(1), one hash probe.
pub(crate) fn introspectable_name_listed(owner: &str, name: &str) -> bool {
    entry(owner, name)
        .is_some_and(|(_, flags)| NativeRowFlags(flags).contains(NativeRowFlags::INTROSPECTABLE))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::native_method_row_table::RAW_ROWS;
    use std::collections::HashMap;

    #[test]
    fn raw_rows_repeated_keys_agree() {
        let mut seen = HashMap::new();
        for &(owner, name, arity, flags) in RAW_ROWS {
            if let Some(&first) = seen.get(&(owner, name)) {
                assert_eq!(
                    first,
                    (arity, flags),
                    "{owner}.{name} is repeated with a different arity/flags; keep one row"
                );
            } else {
                seen.insert((owner, name), (arity, flags));
            }
        }
    }

    #[test]
    fn fold_repeated_keys_keeps_the_first_row_in_order() {
        use crate::builtins::native_method_row_table::fold_repeated_keys;
        let raw = [
            ("Duration", "rand", 1, 16),
            ("Instant", "rand", 1, 16),
            ("Duration", "raku", 1, 16),
            ("Duration", "rand", 1, 16),
        ];
        assert_eq!(
            fold_repeated_keys(&raw),
            vec![
                ("Duration", "rand", 1, 16),
                ("Instant", "rand", 1, 16),
                ("Duration", "raku", 1, 16),
            ]
        );
    }

    #[test]
    fn rows_fold_repeated_keys_into_the_first() {
        let mut seen = std::collections::HashSet::new();
        for &(owner, name, _, _) in rows() {
            assert!(seen.insert((owner, name)), "{owner}.{name} survives twice");
        }
        assert_eq!(seen.len(), classification_table().len());
    }

    /// ADR-0019 E7 step 4: `native_method_row_exists` answers "is this a
    /// real method at all", distinct from `native_method_row`'s
    /// arity/flags -- which, as the two assertions below show, cannot be
    /// used for that question on its own.
    #[test]
    fn row_exists_is_true_for_a_known_pair_and_false_for_an_unprobed_one() {
        assert!(native_method_row_exists("Str", "chars"));
        assert!(!native_method_row_exists("NoSuchOwner", "no-such-method"));
    }

    /// ADR-0019 F3 step 3 (post-cutover regression pin): `builtin_type_method_names`
    /// now enumerates straight from `RAW_ROWS`'s `INTROSPECTABLE` rows, so
    /// this is by construction rather than an independent check -- but it
    /// still guards `canonical_builtin_owner`'s fold and `introspectable_
    /// names_for_owner`'s ordering against a future regression (e.g. a fold
    /// bug that reads the wrong owner's rows, or an accidental resort).
    #[test]
    fn raw_rows_cover_every_introspection_name_in_order() {
        use super::super::builtin_type_methods::BUILTIN_METHOD_OWNERS;
        for &owner in BUILTIN_METHOD_OWNERS {
            let folded = super::super::builtin_type_methods::canonical_builtin_owner(owner);
            let introspection: Vec<&str> = builtin_method_entries(owner)
                .into_iter()
                .map(|e| e.name)
                .collect();
            let raw: Vec<&str> = rows()
                .iter()
                .filter(|&&(o, _, _, _)| o == folded)
                .map(|&(_, n, _, _)| n)
                .collect();
            for name in &introspection {
                assert!(
                    raw.contains(name),
                    "{owner} (folded {folded}): introspection name {name:?} has no RAW_ROWS entry"
                );
            }
            let raw_common: Vec<&str> = raw
                .iter()
                .filter(|n| introspection.contains(n))
                .copied()
                .collect();
            assert_eq!(
                introspection, raw_common,
                "{owner} (folded {folded}): RAW_ROWS order of shared names diverges from the introspection array"
            );
        }
    }
}
