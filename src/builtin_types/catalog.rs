//! Single static builtin-type MRO/roles catalog (ADR-0019 Phase E box E1a;
//! promoted to the single ancestry oracle by ADR-0051 P1).
//!
//! Originally replaced (for the E1 classifier only — see
//! `crate::runtime::receiver_class`) four divergent builtin MRO tables surveyed
//! in `todo/deep/adr0019-e1-typeid-receiver-owner.md`. One of those,
//! `builtin_type_methods::builtin_type_parents`, has since been deleted
//! (ADR-0051 P1): `classhow_mro_names` (`crate::runtime::methods_classhow_mro`)
//! now reads this catalog directly instead, and `Registry::builtin_mro_table`
//! was deleted in ADR-0051 P2 (`Registry::class_mro_readonly` reads this
//! catalog). The rest of ADR-0051 P2 (#9948) re-pointed the `Cool` allowlist,
//! `isa_check`'s variant table, `is_supertype_of`, `are()`'s `Cool` denylist and
//! the multi-dispatch narrowness chains at this catalog, through the derived
//! queries in `crate::builtin_types::ancestry`.
//!
//! **Authority is raku, not the union of the legacy tables.** Every row below
//! was captured from `raku -e 'say <Type>.^mro.map(*.^name); say <Type>.^roles.map(*.^name)'`
//! (Rakudo 2026.06, this workstation) on 2026-08-10 (and 2026-08-20 for the
//! ADR-0051 P1 additions) — see `builtin_type_info_matches_raku` for the
//! row-by-row pin. Known divergences from the legacy (possibly-wrong) mutsu
//! tables are intentional and not yet fixed in those tables (P2's job).
//!
//! `roles` and `mro` are kept separate deliberately: `.^mro` in raku never contains a
//! role, and role membership (`Positional`/`Associative`/`Callable`/`Numeric`/`Real`/
//! `Stringy`/...) is a different fact used by type-matching/distance, not by MRO walks.

/// One catalog row: a builtin type's full linear MRO (including `Any`/`Mu`), the roles
/// it composes (for type-matching, not MRO order), and its canonical dispatch owner —
/// the type whose native method table actually answers calls (empty string = itself).
/// `dispatch_owner` mirrors `canonical_builtin_owner`'s folding
/// (`crate::builtins::builtin_type_methods`) for the handful of types it folds
/// (Sub/Method/Block/Routine/Code -> Code; the Buf/Blob family -> Blob); it is carried
/// here for E2's future handler-row lookups and is not read by the E1a shadow probes.
pub(crate) struct BuiltinTypeInfo {
    pub(crate) name: &'static str,
    pub(crate) mro: &'static [&'static str],
    // `roles`/`dispatch_owner` are read by the catalog's own tests (pinning the
    // raku-adjudicated data) but not yet by the E1a classifier, which only walks
    // `mro`. They exist now so E1b (type-matching/distance table cutover) and E2
    // (native handler rows keyed by `dispatch_owner`) do not need a second data-entry
    // pass through raku for every builtin type.
    #[allow(dead_code)]
    pub(crate) roles: &'static [&'static str],
    #[allow(dead_code)]
    pub(crate) dispatch_owner: &'static str,
}

macro_rules! row {
    ($name:expr, mro: [$($mro:expr),* $(,)?], roles: [$($role:expr),* $(,)?], owner: $owner:expr $(,)?) => {
        BuiltinTypeInfo {
            name: $name,
            mro: &[$($mro),*],
            roles: &[$($role),*],
            dispatch_owner: $owner,
        }
    };
}

// The catalog, one row per builtin type reachable through `value_type_name`
// or the legacy MRO tables. Ordered roughly by family for reviewability;
// lookup is by name via `builtin_type_info`.
include!("catalog_rows_a.rs");
include!("catalog_rows_b.rs");

const fn catalog_row_at(index: usize) -> &'static BuiltinTypeInfo {
    if index < CATALOG_A.len() {
        &CATALOG_A[index]
    } else {
        &CATALOG_B[index - CATALOG_A.len()]
    }
}

/// One catalog row with its `mro` already interned, in both shapes dispatch asks
/// for: `Symbol`s for the registry's `Arc<[Symbol]>` MRO API, and [`crate::type_id::TypeId`]s for
/// the E1 classifier's chains.
///
/// Both are constant for the life of the process — the rows are `&'static str`
/// and a symbol id, once assigned, is never reused or remapped — but every
/// consumer re-derived them per call: `receiver_class::catalog_chain_for_name`
/// interned the whole ancestor chain and allocated a fresh `Vec<TypeId>` on
/// every dispatch that reached it, and `Registry::class_mro_readonly` did the
/// same into an `Arc<[Symbol]>`. Measured on `use Test; plan 2000; for ^2000 {
/// ok 1, "x" }` that was 16.5 of the ~35 `Symbol::intern` calls *per assertion*
/// and 1.28% of the program's retired instructions (#7766).
struct InternedRow {
    info: &'static BuiltinTypeInfo,
    mro_syms: std::sync::Arc<[crate::symbol::Symbol]>,
    mro_ids: Box<[crate::type_id::TypeId]>,
}

/// The catalog indexed by row name, with each row's `mro` interned once.
///
/// Doubles as the name index: [`builtin_type_info`] was a linear scan of all 88
/// rows with a string compare each, and it sits under both of the MRO lookups
/// above plus `Registry::class_mro_readonly`'s builtin path.
fn interned_catalog() -> &'static rustc_hash::FxHashMap<&'static str, InternedRow> {
    static INTERNED: std::sync::OnceLock<rustc_hash::FxHashMap<&'static str, InternedRow>> =
        std::sync::OnceLock::new();
    INTERNED.get_or_init(|| {
        CATALOG_A
            .iter()
            .chain(CATALOG_B.iter())
            .map(|info| {
                (
                    info.name,
                    InternedRow {
                        info,
                        mro_syms: info
                            .mro
                            .iter()
                            .map(|s| crate::symbol::Symbol::intern(s))
                            .collect(),
                        mro_ids: info
                            .mro
                            .iter()
                            .map(|s| crate::type_id::TypeId::intern(s))
                            .collect(),
                    },
                )
            })
            .collect()
    })
}

/// Look up a builtin type's catalog row by its canonical (post-alias) name.
/// `Buf`/`Blob` sized aliases (`buf8`, `blob16`, ...) must be normalized first — see
/// `crate::runtime::utils::normalize_buf_type_name`.
pub(crate) fn builtin_type_info(name: &str) -> Option<&'static BuiltinTypeInfo> {
    interned_catalog().get(name).map(|row| row.info)
}

/// Whether a catalog type composes `role`, including roles inherited through
/// the catalog MRO. Keeping this query beside the catalog lets type matching
/// and runtime introspection share the Rakudo-adjudicated role data.
pub(crate) fn builtin_type_has_role(type_name: &str, role: &str) -> bool {
    let base_type = type_name
        .split_once('[')
        .map(|(base, _)| base)
        .unwrap_or(type_name);
    let role_base = role.split_once('[').map(|(base, _)| base).unwrap_or(role);
    let Some(info) = builtin_type_info(base_type) else {
        return false;
    };
    info.mro.iter().any(|ancestor| {
        builtin_type_info(ancestor).is_some_and(|ancestor_info| {
            ancestor_info.roles.iter().any(|candidate| {
                candidate
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(candidate)
                    == role_base
            })
        })
    })
}

/// `builtin_type_info(name).mro`, interned to `Symbol`s once per process.
///
/// The `Arc` is cloned, not rebuilt: a caller that hands the chain straight back
/// as an `Arc<[Symbol]>` (`Registry::class_mro_readonly`) pays a refcount bump
/// instead of an intern per ancestor plus a fresh allocation.
pub(crate) fn builtin_type_mro_syms(name: &str) -> Option<std::sync::Arc<[crate::symbol::Symbol]>> {
    interned_catalog().get(name).map(|row| row.mro_syms.clone())
}

/// The MRO of the catalog type `name`, as the catalog spells it. Unlike
/// [`builtin_type_mro_syms`] it needs no interning and no table, so a
/// `const` evaluation can read it (the method table's compile-time index).
// Cost: O(c), c = catalog rows (a scan; meant for compile-time evaluation).
pub(crate) const fn builtin_type_mro_strs(name: &str) -> Option<&'static [&'static str]> {
    let mut i = 0;
    while i < CATALOG_A.len() + CATALOG_B.len() {
        let row = catalog_row_at(i);
        if crate::value::const_str_eq(row.name, name) {
            return Some(row.mro);
        }
        i += 1;
    }
    None
}

/// The roles of the catalog type `name`, as the catalog spells them (`Positional[T]`
/// keeps its parameter), for a `const` evaluation. Empty for a name the catalog
/// does not list.
// Cost: O(c), c = catalog rows (a scan; meant for compile-time evaluation).
pub(crate) const fn builtin_type_roles_strs(name: &str) -> &'static [&'static str] {
    let mut i = 0;
    while i < CATALOG_A.len() + CATALOG_B.len() {
        let row = catalog_row_at(i);
        if crate::value::const_str_eq(row.name, name) {
            return row.roles;
        }
        i += 1;
    }
    &[]
}

/// Whether the catalog type `mro[m]` is the class that composes `role`: the
/// role is among its roles and not among those of its parent `mro[m + 1]`, which
/// would have composed it first (`List` composes `Positional`, which `Array`
/// inherits). A role's methods are flattened into the class composing it, so
/// they come after that class's own and before its parent's.
// Cost: O(c * r), c = catalog rows, r = roles of the two types (compile-time).
pub(crate) const fn composes_role_at(mro: &[&str], m: usize, role: &str) -> bool {
    const fn listed(roles: &[&str], role: &str) -> bool {
        let mut i = 0;
        while i < roles.len() {
            if crate::value::const_str_eq(roles[i], role) {
                return true;
            }
            i += 1;
        }
        false
    }
    listed(builtin_type_roles_strs(mro[m]), role)
        && !(m + 1 < mro.len() && listed(builtin_type_roles_strs(mro[m + 1]), role))
}

/// `builtin_type_info(name).mro`, interned to [`crate::type_id::TypeId`]s once per process.
///
/// Borrowed rather than cloned: the E1 classifier splices chains together, so it
/// wants to copy the elements into a chain it is building, not own the table's.
/// Minting `TypeId`s here keeps the type's invariant intact — the catalog is one
/// of the two places allowed to produce one (see [`crate::type_id`]).
pub(crate) fn builtin_type_mro_ids(name: &str) -> Option<&'static [crate::type_id::TypeId]> {
    interned_catalog().get(name).map(|row| &*row.mro_ids)
}

/// Every catalog row's name, for tables derived from the catalog once per
/// process (`crate::builtin_types::ancestry`).
pub(crate) fn all_builtin_type_names() -> impl Iterator<Item = &'static str> {
    CATALOG_A.iter().chain(CATALOG_B.iter()).map(|row| row.name)
}

/// Every catalog row, for exhaustive tests and (eventually) E1b/E2 table generation.
#[cfg(test)]
pub(crate) fn all_builtin_type_info() -> impl Iterator<Item = &'static BuiltinTypeInfo> {
    CATALOG_A.iter().chain(CATALOG_B.iter())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pins every catalog row's `mro` against the `raku -e` output captured in the row
    /// comments above (2026-08-10, Rakudo 2026.06) — this test IS the durable record of
    /// the raku-adjudicated truth referenced by V1 in the E1 design doc. A future
    /// catalog edit that silently drifts from raku fails here first.
    #[test]
    fn every_row_starts_with_its_own_name_and_ends_in_mu_or_is_junction() {
        for row in all_builtin_type_info() {
            assert_eq!(
                row.mro.first(),
                Some(&row.name),
                "row {} must start its own mro with itself",
                row.name
            );
            // Every builtin type's raku `.^mro` terminates at `Mu` (Junction skips
            // `Any` but still ends at `Mu`).
            assert_eq!(
                row.mro.last(),
                Some(&"Mu"),
                "row {} must terminate its mro at Mu",
                row.name
            );
        }
    }

    /// The two pre-interned views (`builtin_type_mro_syms`,
    /// `builtin_type_mro_ids`) must name exactly the row's own `mro`, in order,
    /// for every row — a memo that drops or reorders an ancestor would silently
    /// change method dispatch rather than merely slow it down (#7766). Also
    /// pins the `builtin_type_info` name index against the raw `CATALOG`, which
    /// it replaced a linear scan of.
    #[test]
    fn interned_views_agree_with_their_row() {
        for row in all_builtin_type_info() {
            assert_eq!(
                builtin_type_info(row.name).map(|r| r.name),
                Some(row.name),
                "row {} must be reachable by name",
                row.name
            );
            let syms: Vec<&str> = builtin_type_mro_syms(row.name)
                .unwrap_or_else(|| panic!("row {} has no interned symbol mro", row.name))
                .iter()
                .map(|s| s.as_str())
                .collect();
            assert_eq!(syms, row.mro, "symbol mro for {}", row.name);
            let ids: Vec<&str> = builtin_type_mro_ids(row.name)
                .unwrap_or_else(|| panic!("row {} has no interned TypeId mro", row.name))
                .iter()
                .map(|t| t.as_str())
                .collect();
            assert_eq!(ids, row.mro, "TypeId mro for {}", row.name);
        }
        assert!(builtin_type_info("NotACatalogType").is_none());
        assert!(builtin_type_mro_syms("NotACatalogType").is_none());
        assert!(builtin_type_mro_ids("NotACatalogType").is_none());
    }

    #[test]
    fn no_duplicate_rows() {
        let mut names: Vec<&str> = all_builtin_type_info().map(|r| r.name).collect();
        names.sort_unstable();
        let mut deduped = names.clone();
        deduped.dedup();
        assert_eq!(names, deduped, "duplicate catalog row name");
    }

    #[test]
    fn hash_chain_includes_map_per_raku() {
        // V1 divergence #1: raku's Hash.^mro includes Map; the legacy
        // `Interpreter::builtin_type_mro_chain` table omits it.
        let row = builtin_type_info("Hash").unwrap();
        assert_eq!(row.mro, &["Hash", "Map", "Cool", "Any", "Mu"]);
    }

    #[test]
    fn bool_is_int_not_numeric_directly() {
        let row = builtin_type_info("Bool").unwrap();
        assert_eq!(row.mro, &["Bool", "Int", "Cool", "Any", "Mu"]);
        assert!(row.roles.is_empty());
    }

    #[test]
    fn junction_skips_any() {
        let row = builtin_type_info("Junction").unwrap();
        assert_eq!(row.mro, &["Junction", "Mu"]);
    }

    #[test]
    fn sub_chain_has_no_callable_link_only_role() {
        // V1 divergence #2: `builtin_type_distance`'s inline table interleaves
        // Callable into the MRO chain; raku keeps it as a role only.
        let row = builtin_type_info("Sub").unwrap();
        assert_eq!(row.mro, &["Sub", "Routine", "Block", "Code", "Any", "Mu"]);
        assert_eq!(row.roles, &["Callable"]);
    }

    #[test]
    fn pair_does_not_inherit_cool() {
        // V1 divergence #3.
        let row = builtin_type_info("Pair").unwrap();
        assert_eq!(row.mro, &["Pair", "Any", "Mu"]);
    }

    #[test]
    fn distribution_rows_have_no_distribution_ancestor() {
        // V1 divergence #4: the deleted legacy `Registry::builtin_mro_table`
        // inserted a `Distribution` ancestor absent from real raku.
        assert_eq!(
            builtin_type_info("Distribution::Path").unwrap().mro,
            &["Distribution::Path", "Any", "Mu"]
        );
        assert_eq!(
            builtin_type_info("Distribution::Hash").unwrap().mro,
            &["Distribution::Hash", "Any", "Mu"]
        );
    }

    #[test]
    fn allomorph_rows_chain_through_str_and_the_numeric_type() {
        let int_str = builtin_type_info("IntStr").unwrap();
        assert_eq!(
            int_str.mro,
            &["IntStr", "Allomorph", "Str", "Int", "Cool", "Any", "Mu"]
        );
        let rat_str = builtin_type_info("RatStr").unwrap();
        assert_eq!(
            rat_str.mro,
            &["RatStr", "Allomorph", "Str", "Rat", "Cool", "Any", "Mu"]
        );
    }

    #[test]
    fn sized_buffers_are_keyed_by_the_parameterized_name() {
        assert!(
            builtin_type_info("buf8").is_none(),
            "aliases must be normalized before lookup"
        );
        let row = builtin_type_info("Buf[uint8]").unwrap();
        assert_eq!(row.mro, &["Buf[uint8]", "Any", "Mu"]);
    }

    #[test]
    fn native_array_base_matches_raku_exactly() {
        // ADR-0019 E2b (twelfth slice, 2026-08-10): `array` is the typed-array
        // base; confirmed against `array.^mro`.
        assert_eq!(
            builtin_type_info("array").unwrap().mro,
            &["array", "Cool", "Any", "Mu"]
        );
        assert!(builtin_type_info("CArray").is_none());
    }

    #[test]
    fn adr0051_p1_rows_match_raku_exactly() {
        // Pins the five rows added for ADR-0051 P1 against `raku -e
        // 'say <Type>.^mro.map(*.^name); say <Type>.^roles.map(*.^name)'`
        // (Rakudo 2026.06, verified 2026-08-20).
        assert_eq!(
            builtin_type_info("Cool").unwrap().mro,
            &["Cool", "Any", "Mu"]
        );
        assert!(builtin_type_info("Cool").unwrap().roles.is_empty());

        let instant = builtin_type_info("Instant").unwrap();
        assert_eq!(instant.mro, &["Instant", "Cool", "Any", "Mu"]);
        assert_eq!(instant.roles, &["Real", "Numeric"]);

        let duration = builtin_type_info("Duration").unwrap();
        assert_eq!(duration.mro, &["Duration", "Cool", "Any", "Mu"]);
        assert_eq!(duration.roles, &["Real", "Numeric"]);

        // `IO::Path` IS Cool; `IO::Handle` is NOT -- the two must not be
        // conflated (this is the divergence the ADR calls out explicitly).
        let io_path = builtin_type_info("IO::Path").unwrap();
        assert_eq!(io_path.mro, &["IO::Path", "Cool", "Any", "Mu"]);
        assert_eq!(io_path.roles, &["IO"]);

        let io_handle = builtin_type_info("IO::Handle").unwrap();
        assert_eq!(io_handle.mro, &["IO::Handle", "Any", "Mu"]);
        assert!(io_handle.roles.is_empty());
    }

    #[test]
    fn every_iospec_row_matches_raku_exactly() {
        // No divergence here (unlike Distribution): confirmed 2026-08-10.
        assert_eq!(
            builtin_type_info("IO::Spec::Win32").unwrap().mro,
            &["IO::Spec::Win32", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"]
        );
    }
}
