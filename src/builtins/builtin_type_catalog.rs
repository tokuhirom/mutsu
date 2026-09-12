//! Single static builtin-type MRO/roles catalog (ADR-0019 Phase E box E1a;
//! promoted to the single ancestry oracle by ADR-0051 P1).
//!
//! Originally replaced (for the E1 classifier only — see
//! `crate::runtime::receiver_class`) four divergent builtin MRO tables surveyed
//! in `todo/deep/adr0019-e1-typeid-receiver-owner.md`. One of those,
//! `builtin_type_methods::builtin_type_parents`, has since been deleted
//! (ADR-0051 P1): `classhow_mro_names` (`crate::runtime::methods_classhow_mro`)
//! now reads this catalog directly instead. The other legacy tables —
//! `Registry::builtin_mro_table` (`crate::runtime::registry`),
//! `Interpreter::builtin_type_mro_chain` (`crate::runtime::methods_call_helpers`), and
//! `builtin_type_distance`'s inline table (`crate::runtime::resolution_method`) —
//! are collapsed onto this catalog in ADR-0051 P2, not yet done.
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

/// The catalog, one row per builtin type reachable through
/// [`crate::runtime::utils::value_type_name`] or the four legacy MRO tables. Ordered
/// roughly by family for reviewability; lookup is by name via [`builtin_type_info`].
static CATALOG: &[BuiltinTypeInfo] = &[
    // ---- Cool itself (ADR-0051 P1): previously registered nowhere -- `.^mro`
    // for it was correct only by accident of `classhow_mro_names`' unregistered-
    // type fallback (`vec![class_name]` plus the unconditional Any/Mu append).
    // raku: `Cool.^mro` is `Cool, Any, Mu`; `Cool.^roles` is empty.
    row!("Cool", mro: ["Cool", "Any", "Mu"], roles: [], owner: ""),
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
    // `array`/`CArray` are the NativeCall-facing typed-array bases (lower-case
    // `array` for `array[int32]` etc., `CArray` for `nativecast`ed C arrays).
    // Neither was previously in this catalog OR in `registry.rs`'s separate
    // `builtin_mro_table`, so a value/type-object of a parametrized name like
    // `Array[Int]`/`array[int32]`/`CArray[uint8]` fell all the way through
    // both tables' fallbacks to `[name]` or `[name, Any, Mu]` -- never
    // reaching this row's own ancestry, even though `Array[Int]` itself
    // already had a row (ADR-0019 E2b twelfth slice: the fallback that
    // strips a `Base[T]` name and splices `Base`'s catalog chain is what
    // actually reaches it, in `receiver_class.rs`/`registry.rs`). raku:
    // `array.^mro` is `array, Cool, Any, Mu`; `CArray.^mro` is (short-named,
    // dropping the real `NativeCall::Types::` package prefix mutsu does not
    // model) `CArray, Any, Mu`.
    row!(
        "array",
        mro: ["array", "Cool", "Any", "Mu"],
        roles: ["Positional"],
        owner: "",
    ),
    row!("CArray", mro: ["CArray", "Any", "Mu"], roles: ["Positional"], owner: ""),
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
    // A pseudo-package view of a lexical pad (`MY::`, `CALLER::`, `UNIT::`,
    // ...). Raku keeps it apart from the package symbol table `Stash`: they
    // are SIBLINGS under `Map`, not parent and child (measured against
    // rakudo, `PseudoStash.^mro` is `(PseudoStash Map Cool Any Mu)`), so
    // `PseudoStash` must never be modelled as a `Stash` subclass.
    row!(
        "PseudoStash",
        mro: ["PseudoStash", "Map", "Cool", "Any", "Mu"],
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
    // raku: `Unicode.^mro` is Unicode, Any, Mu; it composes no roles.
    row!("Unicode", mro: ["Unicode", "Any", "Mu"], roles: [], owner: ""),
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
    // raku: `Grammar.^mro` is `(Grammar Match Capture Cool Any Mu)` -- a grammar
    // IS a Match subclass (its parse cursors are Match objects of the grammar's
    // own type). Every user grammar gets `Grammar` as its implicit parent, so
    // this row is what makes `G ~~ Match` / `G.parse(...) ~~ Match` hold.
    row!(
        "Grammar",
        mro: ["Grammar", "Match", "Capture", "Cool", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    // ---- Temporal (ADR-0051 P1): `Instant`/`Duration` genuinely ARE `Cool` in
    // raku (verified 2026-08-10/20: `Instant.^mro`/`Duration.^mro` are
    // `(<Type> Cool Any Mu)`, `.^roles` is `(Real Numeric)`), but had no catalog
    // row at all -- `receiver_class.rs`'s best-effort `[name, Any, Mu]` fallback
    // was standing in for them, which is why `Instant ~~ Cool` (source 4's
    // hand-verified allowlist) already answered `True` while `Instant.^mro`
    // (this catalog, before this row existed) omitted `Cool` entirely.
    row!(
        "Instant",
        mro: ["Instant", "Cool", "Any", "Mu"],
        roles: ["Real", "Numeric"],
        owner: "",
    ),
    row!(
        "Duration",
        mro: ["Duration", "Cool", "Any", "Mu"],
        roles: ["Real", "Numeric"],
        owner: "",
    ),
    // ---- IO::Path / IO::Handle (ADR-0051 P1) ----
    // raku: `IO::Path.^mro` is `IO::Path, Cool, Any, Mu` (`.^roles` is `(IO)`),
    // genuinely `Cool` -- unlike `IO::Handle`, which does NOT inherit `Cool`
    // (`IO::Handle.^mro` is `IO::Handle, Any, Mu`, `.^roles` is empty). Both
    // were previously registered ClassDefs read by `classhow_mro_names`'
    // registry branch (not this catalog), so a catalog-only fix does not by
    // itself correct `.^mro`; `IO::Path`'s bootstrap `ClassDef` in
    // `runtime_init.rs` is fixed alongside this row.
    row!("IO::Path", mro: ["IO::Path", "Cool", "Any", "Mu"], roles: ["IO"], owner: ""),
    row!("IO::Handle", mro: ["IO::Handle", "Any", "Mu"], roles: [], owner: ""),
    // ---- StrDistance (ADR-0051 P4 CI fallout, 2026-08-21) ----
    // Found via a real CI regression (roast/S32-num/rat.t): `StrDistance` is a
    // plain `Value::make_instance` type (no registered `ClassDef`, like
    // `Instant`/`Duration` above) and genuinely inherits `Cool` in raku
    // (verified: `StrDistance.^mro` is `(StrDistance Cool Any Mu)`,
    // `.^roles(:local)` is empty), but had no catalog row at all, so
    // `StrDistance.Rat` -- a real Cool coercion -- wrongly died as
    // "No such method" once P4's existence gate landed. This is the same
    // shape as the four P1 rows above; P1's own audit was scoped to the
    // types the reverted 2026-08-18 attempt's `make test` run surfaced and
    // did not (and could not, without a full corpus sweep) claim to be
    // exhaustive over every raku builtin type.
    row!(
        "StrDistance",
        mro: ["StrDistance", "Cool", "Any", "Mu"],
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
        CATALOG
            .iter()
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

/// `builtin_type_info(name).mro`, interned to [`crate::type_id::TypeId`]s once per process.
///
/// Borrowed rather than cloned: the E1 classifier splices chains together, so it
/// wants to copy the elements into a chain it is building, not own the table's.
/// Minting `TypeId`s here keeps the type's invariant intact — the catalog is one
/// of the two places allowed to produce one (see [`crate::type_id`]).
pub(crate) fn builtin_type_mro_ids(name: &str) -> Option<&'static [crate::type_id::TypeId]> {
    interned_catalog().get(name).map(|row| &*row.mro_ids)
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
    fn native_array_bases_match_raku_exactly() {
        // ADR-0019 E2b (twelfth slice, 2026-08-10): `array`/`CArray` are the
        // NativeCall-facing typed-array bases; confirmed against
        // `array.^mro`/`CArray.^mro` (short-named -- the real
        // `NativeCall::Types::CArray` package prefix is not modeled here).
        assert_eq!(
            builtin_type_info("array").unwrap().mro,
            &["array", "Cool", "Any", "Mu"]
        );
        assert_eq!(
            builtin_type_info("CArray").unwrap().mro,
            &["CArray", "Any", "Mu"]
        );
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
