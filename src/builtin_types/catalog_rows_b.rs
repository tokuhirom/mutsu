static CATALOG_B: &[BuiltinTypeInfo] = &[
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
    row!("Date", mro: ["Date", "Any", "Mu"], roles: ["Dateish"], owner: ""),
    row!("DateTime", mro: ["DateTime", "Any", "Mu"], roles: ["Dateish"], owner: ""),
    row!("Whatever", mro: ["Whatever", "Any", "Mu"], roles: [], owner: ""),
    row!("HyperWhatever", mro: ["HyperWhatever", "Any", "Mu"], roles: [], owner: ""),
    row!("Proxy", mro: ["Proxy", "Any", "Mu"], roles: [], owner: ""),
    // raku: `Attribute.^mro` is `(Attribute Any Mu)` and `Lock.^mro` is
    // `(Lock Any Mu)`; neither does a role. Without a row, every method call
    // on one recomputed the MRO from a registry with no class to cache it in
    // (#12875: Method::Protected's `$attribute.get_value(self)`).
    row!("Attribute", mro: ["Attribute", "Any", "Mu"], roles: [], owner: ""),
    row!("Lock", mro: ["Lock", "Any", "Mu"], roles: [], owner: ""),
    // raku: `Variable.^mro` is `(Variable Any Mu)`; the trait handler's
    // reflective variable (ADR-11276 §9.195).
    row!("Variable", mro: ["Variable", "Any", "Mu"], roles: [], owner: ""),
    // ---- Backtrace (ADR-11276 §9.35): the shapes of the method table's rows. ----
    // raku: `Backtrace.^mro` is `(Backtrace Any Mu)`, `Backtrace::Frame.^mro`
    // is `(Frame Any Mu)`; neither does a role.
    row!("Backtrace", mro: ["Backtrace", "Any", "Mu"], roles: [], owner: ""),
    row!(
        "Backtrace::Frame",
        mro: ["Backtrace::Frame", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    // raku: `Label.^mro` is `(Label Any Mu)`; it does no role.
    row!("Label", mro: ["Label", "Any", "Mu"], roles: [], owner: ""),
    row!("Collation", mro: ["Collation", "Any", "Mu"], roles: [], owner: ""),
    // ---- Match/Capture ----
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
    // `IO::Path::Parts` (ADR-11276 §9.114): a plain `Value::make_instance`
    // type whose rows (`AT-POS`, `AT-KEY`, `flat`) the method table resolves
    // along this MRO; Rakudo's `.^roles` is these three.
    row!(
        "IO::Path::Parts",
        mro: ["IO::Path::Parts", "Any", "Mu"],
        roles: ["Iterable", "Associative", "Positional"],
        owner: "",
    ),
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
    // ---- IO::Spec family (matches raku exactly) ----
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
    // in their `.^mro` (verified 2026-08-10: both are `(Type, Any, Mu)`); both
    // compose `Distribution::Locally` and `Distribution` as roles (verified
    // 2026-09-27: `.^roles` is `(Distribution::Locally Distribution)`).
    // `CompUnit::Repository::Distribution` is a module's `$?DISTRIBUTION`, and
    // `CompUnit::Repository::Installation::LazyDistribution` (a `my class` in
    // Rakudo, so not a resolvable name there) an installed distribution
    // (verified 2026-10-16: `.^mro` `(Type, Any, Mu)`, roles as listed).
    row!(
        "Distribution::Path",
        mro: ["Distribution::Path", "Any", "Mu"],
        roles: ["Distribution::Locally", "Distribution"],
        owner: "",
    ),
    row!(
        "Distribution::Hash",
        mro: ["Distribution::Hash", "Any", "Mu"],
        roles: ["Distribution::Locally", "Distribution"],
        owner: "",
    ),
    row!(
        "CompUnit::Repository::Distribution",
        mro: ["CompUnit::Repository::Distribution", "Any", "Mu"],
        roles: ["Distribution"],
        owner: "",
    ),
    row!(
        "CompUnit::Repository::Installation::LazyDistribution",
        mro: ["CompUnit::Repository::Installation::LazyDistribution", "Any", "Mu"],
        roles: ["Distribution::Locally", "Distribution"],
        owner: "",
    ),
    // ---- CompUnit family ----
    row!("CompUnit", mro: ["CompUnit", "Any", "Mu"], roles: [], owner: ""),
    row!("CompUnit::Handle", mro: ["CompUnit::Handle", "Any", "Mu"], roles: [], owner: ""),
    row!(
        "CompUnit::DependencySpecification",
        mro: ["CompUnit::DependencySpecification", "Any", "Mu"],
        roles: [],
        owner: "",
    ),
    row!(
        "CompUnit::Repository::FileSystem",
        mro: ["CompUnit::Repository::FileSystem", "Any", "Mu"],
        roles: ["CompUnit::Repository", "CompUnit::Repository::Locally"],
        owner: "",
    ),
    row!(
        "CompUnit::Repository::Installation",
        mro: ["CompUnit::Repository::Installation", "Any", "Mu"],
        roles: ["CompUnit::Repository::Installable", "CompUnit::Repository", "CompUnit::Repository::Locally"],
        owner: "",
    ),
];
