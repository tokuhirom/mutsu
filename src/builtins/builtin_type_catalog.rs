//! Single static builtin-type MRO/roles catalog (ADR-0019 Phase E box E1a).
//!
//! Replaces (for the E1 classifier only — see `crate::runtime::receiver_class`) the
//! four divergent builtin MRO tables surveyed in
//! `todo/deep/adr0019-e1-typeid-receiver-owner.md`:
//! [`super::builtin_type_methods::builtin_type_parents`],
//! `Registry::builtin_mro_table` (`crate::runtime::registry`),
//! `Interpreter::builtin_type_mro_chain` (`crate::runtime::methods_call_helpers`), and
//! `builtin_type_distance`'s inline table (`crate::runtime::resolution_method`). Those
//! four tables are left untouched by E1a (zero behavior change); this catalog is
//! consulted only by the new shadow-mode classifier.
//!
//! **Authority is raku, not the union of the four existing tables.** Every row below
//! was captured from `raku -e 'say <Type>.^mro.map(*.^name); say <Type>.^roles.map(*.^name)'`
//! (Rakudo 2026.06, this workstation) on 2026-08-10 — see
//! `builtin_type_info_matches_raku` for the row-by-row pin. Known divergences from the
//! four existing (possibly-wrong) mutsu tables are intentional and are *not* fixed in
//! those tables here (E1a is shadow-only); they are called out in the E1a PR's
//! accepted-mismatch ledger and are E1b's job to flip live.
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

/// The catalog, one row per builtin type reachable through
/// [`crate::runtime::utils::value_type_name`] or the four legacy MRO tables. Ordered
/// roughly by family for reviewability; lookup is by name via [`builtin_type_info`].
static CATALOG: &[BuiltinTypeInfo] = &[
    // ---- Core Cool-derived scalars ----
    row!("Int", mro: ["Int", "Cool", "Any", "Mu"], roles: ["Real", "Numeric"], owner: "Int"),
    row!("Num", mro: ["Num", "Cool", "Any", "Mu"], roles: ["Real", "Numeric"], owner: "Num"),
    row!("Str", mro: ["Str", "Cool", "Any", "Mu"], roles: ["Stringy"], owner: "Str"),
    // raku: `Bool is Int` — Numeric/Real are NOT composed directly on Bool.
    row!("Bool", mro: ["Bool", "Int", "Cool", "Any", "Mu"], roles: [], owner: "Bool"),
    row!(
        "Rat",
        mro: ["Rat", "Cool", "Any", "Mu"],
        roles: ["Rational[Int,Int]", "Real", "Numeric"],
        owner: "Rat",
    ),
    row!(
        "FatRat",
        mro: ["FatRat", "Cool", "Any", "Mu"],
        roles: ["Rational[Int,Int]", "Real", "Numeric"],
        owner: "Rat",
    ),
    row!(
        "Complex",
        mro: ["Complex", "Cool", "Any", "Mu"],
        roles: ["Numeric"],
        owner: "Complex",
    ),
    // ---- Collections ----
    row!(
        "Array",
        mro: ["Array", "List", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "Array",
    ),
    row!(
        "List",
        mro: ["List", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "List",
    ),
    // `Hash is Map` — the legacy `builtin_type_mro_chain` table omits `Map` from
    // Hash's chain (V1 divergence #1); the catalog follows raku, which includes it.
    row!(
        "Hash",
        mro: ["Hash", "Map", "Cool", "Any", "Mu"],
        roles: ["Associative", "Iterable"],
        owner: "Hash",
    ),
    row!(
        "Map",
        mro: ["Map", "Cool", "Any", "Mu"],
        roles: ["Associative", "Iterable"],
        owner: "Hash",
    ),
    row!(
        "Range",
        mro: ["Range", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "Range",
    ),
    row!(
        "Seq",
        mro: ["Seq", "Cool", "Any", "Mu"],
        roles: ["Sequence", "PositionalBindFailover", "Iterable"],
        owner: "",
    ),
    // raku: `Pair` does NOT inherit Cool (unlike the legacy `builtin_type_mro_chain`
    // and `builtin_type_distance` tables, both of which insert Cool — V1 divergence).
    row!("Pair", mro: ["Pair", "Any", "Mu"], roles: ["Associative"], owner: ""),
    row!(
        "Set",
        mro: ["Set", "Any", "Mu"],
        roles: ["Setty", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "SetHash",
        mro: ["SetHash", "Any", "Mu"],
        roles: ["Setty", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "Bag",
        mro: ["Bag", "Any", "Mu"],
        roles: ["Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "BagHash",
        mro: ["BagHash", "Any", "Mu"],
        roles: ["Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "Mix",
        mro: ["Mix", "Any", "Mu"],
        roles: ["Mixy", "Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "MixHash",
        mro: ["MixHash", "Any", "Mu"],
        roles: ["Mixy", "Baggy", "QuantHash", "Associative"],
        owner: "",
    ),
    row!(
        "Slip",
        mro: ["Slip", "List", "Cool", "Any", "Mu"],
        roles: ["Positional", "Iterable"],
        owner: "List",
    ),
    row!(
        "HyperSeq",
        mro: ["HyperSeq", "Any", "Mu"],
        roles: [
            "ParallelSequence[HyperToIterator]",
            "Iterable",
            "Sequence",
            "PositionalBindFailover",
        ],
        owner: "",
    ),
    row!(
        "RaceSeq",
        mro: ["RaceSeq", "Any", "Mu"],
        roles: [
            "ParallelSequence[RaceToIterator]",
            "Iterable",
            "Sequence",
            "PositionalBindFailover",
        ],
        owner: "",
    ),
    // ---- Code/Callable family ----
    // raku: `Sub`'s chain is Sub -> Routine -> Block -> Code -> Any -> Mu, with
    // `Callable` a composed ROLE (not an MRO link) — the legacy
    // `builtin_type_distance` table interleaves `Callable` into the chain itself
    // (V1 divergence).
    row!(
        "Sub",
        mro: ["Sub", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Method",
        mro: ["Method", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Submethod",
        mro: ["Submethod", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Routine",
        mro: ["Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "Block",
        mro: ["Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!(
        "WhateverCode",
        mro: ["WhateverCode", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    row!("Code", mro: ["Code", "Any", "Mu"], roles: ["Callable"], owner: "Code"),
    // raku: `Regex` chain is Regex -> Method -> Routine -> Block -> Code -> Any -> Mu.
    // Both legacy tables (`builtin_type_mro_chain`, `builtin_type_distance`) already
    // spell this correctly (no divergence here).
    row!(
        "Regex",
        mro: ["Regex", "Method", "Routine", "Block", "Code", "Any", "Mu"],
        roles: ["Callable"],
        owner: "Code",
    ),
    // ---- Junction: raku skips `Any` entirely (Junction -> Mu directly) ----
    row!("Junction", mro: ["Junction", "Mu"], roles: [], owner: ""),
    // ---- Nil: distinct from the `Any` that `value_type_name` folds it to today ----
    row!("Nil", mro: ["Nil", "Cool", "Any", "Mu"], roles: [], owner: ""),
    // `Failure` is never declared as a real class anywhere in mutsu (prelude or
    // Rust) -- it is built purely via `Value::make_instance(Symbol::intern("Failure"), ...)`
    // wherever a native method needs one, so the class registry has no model of
    // its ancestry and `class_mro("Failure")` answers just `["Failure"]` with no
    // continuation to `Any`/`Mu` at all (found via ADR-0019 E2b: the
    // `native_call_unmodeled` counter never reached zero for `Failure`'s
    // `Any`-declared universal methods -- `so`/`defined`/`sink`/... -- no matter
    // how many rows were added, because the chain walk never got past
    // `Failure` itself). raku: `Failure ISA Nil` (`Failure.new.^mro` is
    // `Failure, Nil, Cool, Any, Mu`), which this catalog row supplies via
    // `class_chain`'s direct `builtin_type_info` lookup, bypassing the
    // registry entirely for this type the same way `Nil` above already does.
    row!("Failure", mro: ["Failure", "Nil", "Cool", "Any", "Mu"], roles: [], owner: ""),
    // `Exception` is a name every built-in `X::*` exception type registers as
    // its parent (`BUILTIN_PARENT_TYPES` in `registration_class_decl.rs`),
    // but "Exception" itself is never registered as an actual class in the
    // registry -- so `compute_class_mro`'s implicit-`Any` rule (which only
    // fires for a class actually present in `self.classes`) never applies to
    // it, and every `X::*` type's registry MRO dead-ends at `Exception` with
    // no `Any`/`Mu` continuation (e.g. `X::AdHoc`'s registry MRO was
    // `["X::AdHoc", "Exception"]`). This catalog row lets
    // `class_chain_with_catalog_tail`'s splice logic supply the missing tail
    // for every such type in one place, the same mechanism the `Failure` row
    // above uses. raku: `Exception.^mro` is `Exception, Any, Mu`.
    row!("Exception", mro: ["Exception", "Any", "Mu"], roles: [], owner: ""),
    // `CX::Warn` (and the sibling `CX::*` control-exception types) is built
    // purely via `Value::make_instance` with no registered parent at all
    // (unlike `X::AdHoc`, which at least registers `is Exception` even
    // though `Exception` itself was unregistered) -- its registry MRO was
    // the bare `["CX::Warn"]`, so the `Exception` splice above never
    // triggers for it (its registry chain never mentions `Exception`).
    // raku: `CX::Warn.^mro` is `CX::Warn, Exception, Any, Mu`.
    row!(
        "CX::Warn",
        mro: ["CX::Warn", "Exception", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    // ---- Allomorphs (V4) ----
    row!(
        "Allomorph",
        mro: ["Allomorph", "Str", "Cool", "Any", "Mu"],
        roles: ["Stringy"],
        owner: "",
    ),
    row!(
        "IntStr",
        mro: ["IntStr", "Allomorph", "Str", "Int", "Cool", "Any", "Mu"],
        roles: ["Stringy", "Real", "Numeric"],
        owner: "",
    ),
    row!(
        "NumStr",
        mro: ["NumStr", "Allomorph", "Str", "Num", "Cool", "Any", "Mu"],
        roles: ["Stringy", "Real", "Numeric"],
        owner: "",
    ),
    row!(
        "RatStr",
        mro: ["RatStr", "Allomorph", "Str", "Rat", "Cool", "Any", "Mu"],
        roles: ["Stringy", "Rational[Int,Int]", "Real", "Numeric"],
        owner: "",
    ),
    row!(
        "ComplexStr",
        mro: ["ComplexStr", "Allomorph", "Str", "Complex", "Cool", "Any", "Mu"],
        roles: ["Stringy", "Numeric"],
        owner: "",
    ),
    // ---- Buf/Blob family (V5) ----
    // The unsized `Buf`/`Blob` type objects (`BufStorage` reads answer "Buf";
    // `Buf.new(...)` with no size annotation stays unsized).
    row!("Buf", mro: ["Buf", "Any", "Mu"], roles: ["Blob[T]", "Positional[T]", "Stringy"], owner: "Blob"),
    row!("Blob", mro: ["Blob", "Any", "Mu"], roles: ["Positional[T]", "Stringy"], owner: "Blob"),
    // Sized buffers: `.^name` (and mutsu's `Instance.class_name`) renders the
    // parameterized spelling (`Buf[uint8]`); `buf8`/`blob8` are source-level aliases
    // resolved by `normalize_buf_type_name` before catalog lookup (V5).
    row!(
        "Buf[uint8]",
        mro: ["Buf[uint8]", "Any", "Mu"],
        roles: ["Buf::UnsignedBuf[uint8]", "Blob[uint8]", "UnsignedBlob[uint8]", "Positional[uint8]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "Buf[uint16]",
        mro: ["Buf[uint16]", "Any", "Mu"],
        roles: ["Buf::UnsignedBuf[uint16]", "Blob[uint16]", "UnsignedBlob[uint16]", "Positional[uint16]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "Buf[uint32]",
        mro: ["Buf[uint32]", "Any", "Mu"],
        roles: ["Buf::UnsignedBuf[uint32]", "Blob[uint32]", "UnsignedBlob[uint32]", "Positional[uint32]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "Buf[uint64]",
        mro: ["Buf[uint64]", "Any", "Mu"],
        roles: ["Buf::UnsignedBuf[uint64]", "Blob[uint64]", "UnsignedBlob[uint64]", "Positional[uint64]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "Blob[uint8]",
        mro: ["Blob[uint8]", "Any", "Mu"],
        roles: ["UnsignedBlob[uint8]", "Positional[uint8]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "Blob[uint16]",
        mro: ["Blob[uint16]", "Any", "Mu"],
        roles: ["UnsignedBlob[uint16]", "Positional[uint16]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "Blob[uint32]",
        mro: ["Blob[uint32]", "Any", "Mu"],
        roles: ["UnsignedBlob[uint32]", "Positional[uint32]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "Blob[uint64]",
        mro: ["Blob[uint64]", "Any", "Mu"],
        roles: ["UnsignedBlob[uint64]", "Positional[uint64]", "Stringy"],
        owner: "Blob",
    ),
    // Encoding buffers (`utf8`/`utf16`/`utf32`) are their OWN raku type, distinct
    // from `Blob[uintN]` — mutsu's `normalize_buf_type_name` folds them into
    // `Blob[uintN]` for element-storage purposes (pre-existing, unaffected by E1a);
    // the catalog records both facts: the type's real raku identity here, and the
    // fold as `dispatch_owner`.
    row!(
        "utf8",
        mro: ["utf8", "Any", "Mu"],
        roles: ["Blob[uint8]", "UnsignedBlob[uint8]", "Positional[uint8]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "utf16",
        mro: ["utf16", "Any", "Mu"],
        roles: ["Blob[uint16]", "UnsignedBlob[uint16]", "Positional[uint16]", "Stringy"],
        owner: "Blob",
    ),
    row!(
        "utf32",
        mro: ["utf32", "Any", "Mu"],
        roles: ["Blob[uint32]", "UnsignedBlob[uint32]", "Positional[uint32]", "Stringy"],
        owner: "Blob",
    ),
    // ---- Uni / normalization forms ----
    row!("Uni", mro: ["Uni", "Any", "Mu"], roles: ["Stringy", "Positional[uint32]"], owner: ""),
    row!("NFC", mro: ["NFC", "Uni", "Any", "Mu"], roles: ["Stringy", "Positional[uint32]"], owner: ""),
    row!("NFD", mro: ["NFD", "Uni", "Any", "Mu"], roles: ["Stringy", "Positional[uint32]"], owner: ""),
    row!("NFKC", mro: ["NFKC", "Uni", "Any", "Mu"], roles: ["Stringy", "Positional[uint32]"], owner: ""),
    row!("NFKD", mro: ["NFKD", "Uni", "Any", "Mu"], roles: ["Stringy", "Positional[uint32]"], owner: ""),
    // ---- Misc value types reachable via value_type_name ----
    row!("Version", mro: ["Version", "Any", "Mu"], roles: [], owner: ""),
    row!("Capture", mro: ["Capture", "Any", "Mu"], roles: [], owner: ""),
    row!("Promise", mro: ["Promise", "Any", "Mu"], roles: ["Awaitable"], owner: ""),
    row!("Channel", mro: ["Channel", "Any", "Mu"], roles: ["Awaitable"], owner: ""),
    row!("Whatever", mro: ["Whatever", "Any", "Mu"], roles: [], owner: ""),
    row!("HyperWhatever", mro: ["HyperWhatever", "Any", "Mu"], roles: [], owner: ""),
    row!("Proxy", mro: ["Proxy", "Any", "Mu"], roles: [], owner: ""),
    // ---- Match/Capture (Registry::builtin_mro_table family) ----
    row!(
        "Match",
        mro: ["Match", "Capture", "Cool", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    // ---- IO::Spec family (Registry::builtin_mro_table; matches raku exactly) ----
    row!("IO::Spec", mro: ["IO::Spec", "Any", "Mu"], roles: [], owner: ""),
    row!(
        "IO::Spec::Unix",
        mro: ["IO::Spec::Unix", "IO::Spec", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    row!(
        "IO::Spec::Win32",
        mro: ["IO::Spec::Win32", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    row!(
        "IO::Spec::Cygwin",
        mro: ["IO::Spec::Cygwin", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    row!(
        "IO::Spec::QNX",
        mro: ["IO::Spec::QNX", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    // ---- Distribution family ----
    // raku: NEITHER `Distribution::Path` NOR `Distribution::Hash` has `Distribution`
    // in their `.^mro` (verified 2026-08-10: both are `(Type, Any, Mu)`) — the legacy
    // `Registry::builtin_mro_table` inserts a `Distribution` ancestor that does not
    // exist in raku (V1 divergence; accepted-mismatch, not fixed here).
    row!("Distribution::Path", mro: ["Distribution::Path", "Any", "Mu"], roles: [], owner: ""),
    row!("Distribution::Hash", mro: ["Distribution::Hash", "Any", "Mu"], roles: [], owner: ""),
    // ---- CompUnit family ----
    row!(
        "CompUnit::DependencySpecification",
        mro: ["CompUnit::DependencySpecification", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    row!(
        "CompUnit::Repository::FileSystem",
        mro: ["CompUnit::Repository::FileSystem", "Any", "Mu"],
        roles: ["CompUnit::Repository::Installable", "CompUnit::Repository", "CompUnit::Repository::Locally"],
        owner: "",
    ),
    row!(
        "CompUnit::Repository::Installation",
        mro: ["CompUnit::Repository::Installation", "Any", "Mu"],
        roles: ["CompUnit::Repository", "CompUnit::Repository::Locally"],
        owner: "",
    ),
];

/// Look up a builtin type's catalog row by its canonical (post-alias) name.
/// `Buf`/`Blob` sized aliases (`buf8`, `blob16`, ...) must be normalized first — see
/// `crate::runtime::utils::normalize_buf_type_name`.
pub(crate) fn builtin_type_info(name: &str) -> Option<&'static BuiltinTypeInfo> {
    CATALOG.iter().find(|row| row.name == name)
}

/// Every catalog row, for exhaustive tests and (eventually) E1b/E2 table generation.
#[cfg(test)]
pub(crate) fn all_builtin_type_info() -> &'static [BuiltinTypeInfo] {
    CATALOG
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

    #[test]
    fn no_duplicate_rows() {
        let mut names: Vec<&str> = all_builtin_type_info().iter().map(|r| r.name).collect();
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
        // V1 divergence #4: the legacy Registry::builtin_mro_table inserts a
        // `Distribution` ancestor absent from real raku.
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
    fn every_iospec_row_matches_raku_exactly() {
        // No divergence here (unlike Distribution): confirmed 2026-08-10.
        assert_eq!(
            builtin_type_info("IO::Spec::Win32").unwrap().mro,
            &["IO::Spec::Win32", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"]
        );
    }
}
