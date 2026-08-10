//! Canonical introspection registry for built-in (non-user-defined) types.
//!
//! `.^methods`, `.^can`, and `.^mro` need to report which methods a built-in
//! type such as `Str`/`Int`/`List` responds to, and what its parent chain is.
//! Built-in types have no user-level class definition, so this information
//! cannot be read off the `registry().classes` map; it lives here instead.
//!
//! The catalog is static: constructing an [`Interpreter`](crate::runtime::Interpreter)
//! must never execute native methods merely to discover their names. The runtime
//! registry copies these entries once and serves introspection from that table.
//!
//! ## Keep in sync with dispatch
//!
//! Add a native or slow-path method to the owning static slice in this module.
//! `t/can-methods-drift.t` guards the callable/introspectable contract.

#[cfg(test)]
use crate::symbol::Symbol;
#[cfg(test)]
use crate::value::Value;

/// Numeric/stringy coercion methods shared verbatim (same order) by the leaf
/// types `Str`, `Int`/`Num`/`Rat`/`Complex`, `Bool`, and `Cool`. Declared once
/// here so a new coercion is added to every one of them at the same time.
const NUMERIC_COERCIONS: &[&str] = &[
    "Numeric", "Int", "Num", "Rat", "Bool", "Str", "gist", "raku",
];

/// `Str`-specific methods, in `.^methods` order, up to (not including) the
/// shared coercion tail.
const STR_OWN: &[&str] = &[
    "chars",
    "codes",
    "comb",
    "chomp",
    "chop",
    "contains",
    "ends-with",
    "fc",
    "flip",
    "index",
    "indices",
    "lc",
    "lines",
    "match",
    "ords",
    "pred",
    "rindex",
    "samecase",
    "samemark",
    "split",
    "starts-with",
    "substr",
    "succ",
    "tc",
    "trim",
    "trim-leading",
    "trim-trailing",
    "uc",
    "words",
    "wordcase",
    "NFC",
    "NFD",
    "NFKC",
    "NFKD",
    "encode",
    "uniparse",
    "unimatch",
    "uniprops",
    "parse-names",
    "parse-base",
    "subst",
    "subst-mutate",
    "substr-rw",
    "substr-eq",
    "trans",
    "IO",
];

/// Numeric leaf (`Int`/`Num`/`Rat`/`Complex`) methods, up to the coercion tail.
const NUMERIC_OWN: &[&str] = &[
    "abs", "ceiling", "floor", "round", "sign", "sqrt", "log", "log10", "exp", "roots", "is-prime",
    "chr", "base", "polymod", "expmod", "pred", "succ",
];

/// `Bool` methods, up to the coercion tail.
const BOOL_OWN: &[&str] = &["pred", "succ", "pick", "roll"];

/// `Cool` methods (string + math coercion helpers), up to the coercion tail.
const COOL_OWN: &[&str] = &[
    "substr",
    "chars",
    "codes",
    "chomp",
    "chop",
    "contains",
    "comb",
    "ends-with",
    "fc",
    "flip",
    "index",
    "indices",
    "lc",
    "lines",
    "match",
    "ords",
    "pred",
    "rindex",
    "samecase",
    "split",
    "starts-with",
    "succ",
    "tc",
    "trim",
    "trim-leading",
    "trim-trailing",
    "uc",
    "words",
    "wordcase",
    "abs",
    "ceiling",
    "floor",
    "round",
    "sign",
    "sqrt",
    "log",
    "log10",
    "exp",
    "is-prime",
    "chr",
    "base",
    "polymod",
];

const LIST_METHODS: &[&str] = &[
    "elems",
    "end",
    "keys",
    "values",
    "kv",
    "pairs",
    "antipairs",
    "join",
    "map",
    "grep",
    "first",
    "sort",
    "reverse",
    "rotate",
    "unique",
    "repeated",
    "squish",
    "flat",
    "eager",
    "lazy",
    "head",
    "tail",
    "skip",
    "push",
    "pop",
    "shift",
    "unshift",
    "splice",
    "append",
    "prepend",
    "classify",
    "categorize",
    "min",
    "max",
    "minmax",
    "minpairs",
    "maxpairs",
    "sum",
    "pick",
    "roll",
    "permutations",
    "combinations",
    "rotor",
    "batch",
    "produce",
    "reduce",
    "Bool",
    "Str",
    "gist",
    "raku",
    "Numeric",
    "Int",
    "Array",
    "List",
];

const HASH_METHODS: &[&str] = &[
    "elems",
    "keys",
    "values",
    "kv",
    "pairs",
    "antipairs",
    "push",
    "append",
    "classify-list",
    "categorize-list",
    "Bool",
    "Str",
    "gist",
    "raku",
    "Numeric",
    "Int",
];

const RANGE_METHODS: &[&str] = &[
    "min",
    "max",
    "bounds",
    "elems",
    "list",
    "flat",
    "reverse",
    "pick",
    "roll",
    "sum",
    "rand",
    "minmax",
    "infinite",
    "is-int",
    "Bool",
    "Str",
    "gist",
    "raku",
    "Numeric",
    "Int",
    "excludes-min",
    "excludes-max",
];

const CODE_METHODS: &[&str] = &[
    "name",
    "signature",
    "arity",
    "count",
    "of",
    "returns",
    "Bool",
    "Str",
    "gist",
    "raku",
];

const SIGNATURE_METHODS: &[&str] = &[
    "params", "arity", "count", "returns", "Bool", "Str", "gist", "raku",
];

const IO_PATH_METHODS: &[&str] = &[
    "absolute",
    "basename",
    "cleanup",
    "copy",
    "dirname",
    "e",
    "d",
    "f",
    "l",
    "r",
    "w",
    "x",
    "rw",
    "rwx",
    "s",
    "z",
    "extension",
    "IO",
    "lines",
    "mkdir",
    "modified",
    "accessed",
    "changed",
    "mode",
    "move",
    "open",
    "parent",
    "parts",
    "pred",
    "rename",
    "resolve",
    "rmdir",
    "sibling",
    "slurp",
    "spurt",
    "succ",
    "symlink",
    "link",
    "add",
    "child",
    "unlink",
    "volume",
    "watch",
    "words",
    "CWD",
    "SPEC",
    "Bool",
    "Str",
    "gist",
    "raku",
    "Numeric",
    "Int",
];

const IO_HANDLE_METHODS: &[&str] = &[
    "open",
    "close",
    "path",
    "IO",
    "slurp",
    "slurp-rest",
    "spurt",
    "lines",
    "words",
    "comb",
    "split",
    "print",
    "print-nl",
    "printf",
    "say",
    "put",
    "get",
    "getc",
    "read",
    "readchars",
    "write",
    "seek",
    "tell",
    "eof",
    "flush",
    "lock",
    "unlock",
    "opened",
    "nl-in",
    "nl-out",
    "chomp",
    "encoding",
    "decode",
    "Supply",
    "native-descriptor",
    "WRITE",
    "READ",
    "t",
    "Bool",
    "Str",
    "gist",
    "raku",
];

const ANY_METHODS: &[&str] = &[
    "say",
    "put",
    "print",
    "note",
    "so",
    "not",
    "defined",
    "WHAT",
    "WHERE",
    "HOW",
    "WHY",
    "iterator",
    "flat",
    "eager",
    "lazy",
    "map",
    "grep",
    "first",
    "sort",
    "reverse",
    "unique",
    "repeated",
    "squish",
    "head",
    "tail",
    "skip",
    "min",
    "max",
    "minmax",
    "elems",
    "end",
    "keys",
    "values",
    "kv",
    "pairs",
    "antipairs",
    "classify",
    "categorize",
    "join",
    "pick",
    "roll",
    "sum",
    "reduce",
    "produce",
    "rotor",
    "batch",
    "Bool",
    "Str",
    "gist",
    "raku",
    "Numeric",
    "Int",
];

const MU_METHODS: &[&str] = &[
    "defined", "WHAT", "WHERE", "HOW", "WHY", "WHICH", "Bool", "Str", "gist", "raku", "clone",
    "new",
];

/// The method names a built-in `type_name` responds to, in `.^methods` order.
/// Returns an empty `Vec` for types not modelled here (e.g. user classes, which
/// are handled separately via `registry().classes`).
/// `Buf`/`Blob` methods the native probe can't reach (slow-path or type-object
/// constructors). `allocate` matters beyond introspection:
/// `NativeHelpers::Blob`'s `blob-from-pointer` branches on
/// `$type.can('allocate')` and takes a REPR-poking fallback when it answers
/// false.
const BUF_METHODS: &[&str] = &[
    "allocate",
    "new",
    "push",
    "pop",
    "shift",
    "unshift",
    "append",
    "prepend",
    "splice",
    "reallocate",
    "subbuf",
    "subbuf-rw",
    "decode",
    "elems",
    "bytes",
    "of",
    "reverse",
    "list",
    "Blob",
    "Buf",
    "Bool",
    "Str",
    "gist",
    "raku",
];

pub(crate) fn builtin_type_method_names(type_name: &str) -> Vec<&'static str> {
    if crate::runtime::utils::is_buf_or_blob_class(type_name) {
        return BUF_METHODS.to_vec();
    }
    match type_name {
        "Str" => [STR_OWN, NUMERIC_COERCIONS, &["elems", "fmt"]].concat(),
        "Int" | "Num" | "Rat" | "Complex" => [NUMERIC_OWN, NUMERIC_COERCIONS].concat(),
        "List" | "Array" => LIST_METHODS.to_vec(),
        "Hash" => HASH_METHODS.to_vec(),
        "Bool" => [BOOL_OWN, NUMERIC_COERCIONS].concat(),
        "Range" => RANGE_METHODS.to_vec(),
        "Sub" | "Method" | "Block" | "Routine" | "Code" => CODE_METHODS.to_vec(),
        "Signature" => SIGNATURE_METHODS.to_vec(),
        "IO::Path" => IO_PATH_METHODS.to_vec(),
        "IO::Handle" => IO_HANDLE_METHODS.to_vec(),
        "Cool" => [COOL_OWN, NUMERIC_COERCIONS].concat(),
        "Any" => ANY_METHODS.to_vec(),
        "Mu" => MU_METHODS.to_vec(),
        _ => Vec::new(),
    }
}

/// A representative sample VALUE for a *concrete* built-in type, used to probe
/// the real native dispatch when answering `.^methods` / `.^can`. Abstract types
/// (`Any`/`Mu`/`Cool`) and types without an easily-constructed instance return
/// `None`, so the caller falls back to the declared list above.
///
/// Probing a sample value is what makes the method set *derived from dispatch*
/// rather than a hand-maintained list: e.g. `"abc"` responds to `chars`/`uc`/
/// `samemark` but not `abs`, while `2` responds to `abs` but not `chars`, so the
/// same `METHOD_UNIVERSE` yields each type's correct subset automatically.
#[cfg(test)]
pub(crate) fn builtin_sample_value(type_name: &str) -> Option<Value> {
    if crate::runtime::utils::is_buf_or_blob_class(type_name) {
        return Some(crate::value::value_buf::make_buf(
            Symbol::intern(type_name),
            vec![Value::int(1)],
        ));
    }
    Some(match type_name {
        "Str" => Value::str_from("abc"),
        "Int" => Value::int(2),
        "Num" => Value::num(1.5),
        "Rat" | "FatRat" => crate::value::make_rat(1, 2),
        "Complex" => Value::complex(1.0, 2.0),
        "Bool" => Value::TRUE,
        "List" => Value::array(vec![Value::int(1), Value::int(2), Value::int(3)]),
        "Array" => Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                Value::int(1),
                Value::int(2),
            ])),
            crate::value::ArrayKind::Array,
        ),
        "Hash" => Value::hash(std::collections::HashMap::from([(
            "a".to_string(),
            Value::int(1),
        )])),
        "Range" => Value::range(1, 3),
        _ => return None,
    })
}

/// Whether `value` responds to `method_name` via mutsu's *native* method
/// dispatch (the pure 0/1/2-arg `native_method_*` paths). `is_some()` means the
/// method NAME was recognized at that arity — independent of whether the call
/// would succeed — because the dispatch matches the method name before the
/// argument values. This is the same recognition `.^can` relies on, so
/// `.^methods` (which filters `METHOD_UNIVERSE` through this) stays consistent
/// with `.^can`. It does NOT cover slow-path methods (those needing `&mut self`,
/// e.g. block-taking `map`/`grep`/`sort`); those remain in the declared lists.
#[cfg(test)]
pub(crate) fn native_responds_to(value: &Value, method_name: &str) -> bool {
    native_method_arities(value, method_name) != 0
}

#[cfg(test)]
pub(crate) fn native_method_arities(value: &Value, method_name: &str) -> u8 {
    let sym = Symbol::intern(method_name);
    let mut arities = u8::from(crate::builtins::native_method_0arg(value, sym).is_some());
    // A few 1/2-arg native methods inspect the argument type before recognizing
    // the call (e.g. `index`/`indices` want a Str), so a single dummy can miss
    // them. Try a small spread of representative arguments — recognition just
    // needs ONE arity/arg shape to return `Some`.
    let dummies = [Value::NIL, Value::int(0), Value::str_from("")];
    if dummies
        .iter()
        .any(|a| crate::builtins::native_method_1arg(value, sym, a).is_some())
    {
        arities |= 1 << 1;
    }
    if dummies
        .iter()
        .any(|a| crate::builtins::native_method_2arg(value, sym, a, a).is_some())
    {
        arities |= 1 << 2;
    }
    arities
}

/// Generous master set of built-in method NAMES (excluding the universal
/// `Mu`/`Any` methods such as `say`/`WHAT`/`defined`, which are reported via the
/// `Any`/`Mu` lists on `:all`). `.^methods` for a concrete type filters this
/// through `native_responds_to(sample, name)`, so a name listed here that the
/// type does not actually dispatch is silently dropped — making the universe
/// safe to keep broad. Add a name here when introducing a native method whose
/// name is not already present.
#[cfg(test)]
pub(crate) const METHOD_UNIVERSE: &[&str] = &[
    // String / Cool
    "chars",
    "codes",
    "comb",
    "chomp",
    "chop",
    "contains",
    "ends-with",
    "fc",
    "flip",
    "index",
    "indices",
    "lc",
    "lines",
    "match",
    "ord",
    "ords",
    "pred",
    "rindex",
    "samecase",
    "samemark",
    "samespace",
    "split",
    "starts-with",
    "substr",
    "substr-eq",
    "substr-rw",
    "subst",
    "subst-mutate",
    "succ",
    "tc",
    "tclc",
    "trim",
    "trim-leading",
    "trim-trailing",
    "uc",
    "words",
    "wordcase",
    "indent",
    "trans",
    "encode",
    "NFC",
    "NFD",
    "NFKC",
    "NFKD",
    "uniparse",
    "parse-names",
    "parse-base",
    "fmt",
    "elems",
    "IO",
    // Unicode property accessors
    "unimatch",
    "uniname",
    "uninames",
    "uniprop",
    "uniprops",
    "unival",
    "univals",
    "uniprop-int",
    "uniprop-bool",
    "uniprop-str",
    // Numeric / Cool
    "abs",
    "ceiling",
    "floor",
    "round",
    "sign",
    "sqrt",
    "log",
    "log10",
    "exp",
    "roots",
    "is-prime",
    "chr",
    "base",
    "polymod",
    "expmod",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "atan2",
    "sinh",
    "cosh",
    "tanh",
    "sec",
    "cosec",
    "cotan",
    "lsb",
    "msb",
    // List / Array (native, non-block)
    "end",
    "keys",
    "values",
    "kv",
    "pairs",
    "antipairs",
    "join",
    "reverse",
    "rotate",
    "unique",
    "repeated",
    "squish",
    "flat",
    "eager",
    "head",
    "tail",
    "skip",
    "push",
    "pop",
    "shift",
    "unshift",
    "splice",
    "append",
    "prepend",
    "min",
    "max",
    "minmax",
    "minpairs",
    "maxpairs",
    "sum",
    "pick",
    "roll",
    "permutations",
    "combinations",
    "rotor",
    "batch",
    "list",
    "Array",
    "List",
    "Seq",
    "cache",
    // Hash
    "classify-list",
    "categorize-list",
    // Range
    "bounds",
    "rand",
    "infinite",
    "is-int",
    "excludes-min",
    "excludes-max",
    // Coercions / identity (own, not the universal Mu set)
    "Numeric",
    "Int",
    "Num",
    "Rat",
    "FatRat",
    "Complex",
    "Bool",
    "Str",
    "Stringy",
    "Capture",
    "gist",
    "raku",
    "WHICH",
];

/// One canonical built-in type×method entry. User candidates will use the same shape once
/// ADR-0019's registry migration lands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct BuiltinMethodEntry {
    pub(crate) owner: &'static str,
    pub(crate) name: &'static str,
    /// Stable catalog order used by `.^methods`.
    pub(crate) order: u16,
}

/// Built-in owners whose method entries are installed into the runtime registry.
/// Aliases such as `FatRat` and `Method` resolve through [`canonical_builtin_owner`]
/// and therefore do not need duplicate table rows.
pub(crate) const BUILTIN_METHOD_OWNERS: &[&str] = &[
    "Str",
    "Int",
    "Num",
    "Rat",
    "Complex",
    "List",
    "Array",
    "Hash",
    "Bool",
    "Range",
    "Sub",
    "Signature",
    "IO::Path",
    "IO::Handle",
    "Cool",
    "Any",
    "Mu",
    "Blob",
];

/// The canonical built-in method entries for one owner type. Both dispatch admission and
/// introspection consume this static type×method catalog.
pub(crate) fn builtin_method_entries(type_name: &str) -> Vec<BuiltinMethodEntry> {
    builtin_type_method_names(type_name)
        .into_iter()
        .enumerate()
        .map(|(order, name)| BuiltinMethodEntry {
            owner: canonical_builtin_owner(type_name),
            name,
            order: order as u16,
        })
        .collect()
}

pub(crate) fn canonical_builtin_owner(type_name: &str) -> &'static str {
    match type_name {
        "Str" => "Str",
        "Int" => "Int",
        "Num" => "Num",
        "Rat" | "FatRat" => "Rat",
        "Complex" => "Complex",
        "List" => "List",
        "Array" => "Array",
        "Hash" => "Hash",
        "Bool" => "Bool",
        "Range" => "Range",
        "Sub" | "Method" | "Block" | "Routine" | "Code" => "Code",
        "Signature" => "Signature",
        "IO::Path" => "IO::Path",
        "IO::Handle" => "IO::Handle",
        "Cool" => "Cool",
        "Any" => "Any",
        "Mu" => "Mu",
        name if crate::runtime::utils::is_buf_or_blob_class(name) => "Blob",
        _ => "",
    }
}

#[cfg(test)]
pub(crate) fn introspected_type_method_names(type_name: &str) -> Vec<&'static str> {
    builtin_method_entries(type_name)
        .into_iter()
        .map(|entry| entry.name)
        .collect()
}

/// Introspectable instance attributes for built-in types, as
/// `(name-without-sigil, type-name)` pairs in declaration order. These mirror
/// the attributes Rakudo reports from `.^attributes` (e.g. `Rat.^attributes`
/// yields `$!numerator` and `$!denominator`). Types without modelled
/// attributes return an empty slice.
pub(crate) fn builtin_type_attributes(type_name: &str) -> &'static [(&'static str, &'static str)] {
    match type_name {
        "Rat" | "FatRat" => &[("numerator", "Int"), ("denominator", "Int")],
        "Complex" => &[("re", "Num"), ("im", "Num")],
        "Int" => &[("value", "int")],
        "Num" => &[("value", "num")],
        "Str" => &[("value", "str")],
        "Pair" => &[("key", "Mu"), ("value", "Mu")],
        "Range" => &[
            ("min", "Mu"),
            ("max", "Mu"),
            ("excludes-min", "Bool"),
            ("excludes-max", "Bool"),
            ("infinite", "Bool"),
            ("is-int", "Bool"),
        ],
        // DateTime's public-accessor attributes (rakudo also has `&!formatter`,
        // omitted here so a JSON round-trip's null formatter is simply unused).
        // `second` is Mu so a fractional value passes through unchanged.
        "DateTime" => &[
            ("hour", "Int"),
            ("minute", "Int"),
            ("second", "Mu"),
            ("timezone", "Int"),
            ("year", "Int"),
            ("month", "Int"),
            ("day", "Int"),
            ("daycount", "Int"),
        ],
        _ => &[],
    }
}

/// Whether a modelled built-in attribute has a public accessor (`.year` on
/// DateTime, ...). The numeric internals (`Rat.$!numerator`, ...) stay private.
pub(crate) fn builtin_type_attr_has_accessor(type_name: &str, _attr_name: &str) -> bool {
    matches!(type_name, "DateTime")
}

/// The built-in MRO (parent chain) for `type_name`, up to but not including
/// `Any`/`Mu` (those are appended by the caller). Returns an empty slice for
/// types with no modelled built-in hierarchy.
pub(crate) fn builtin_type_parents(type_name: &str) -> &'static [&'static str] {
    match type_name {
        "Int" => &["Int", "Cool"],
        "Num" => &["Num", "Cool"],
        "Rat" | "FatRat" => &["Rat", "Cool"],
        "Complex" => &["Complex", "Cool"],
        "Str" => &["Str", "Cool"],
        "Bool" => &["Bool", "Int", "Cool"],
        "Array" => &["Array", "List", "Cool"],
        "List" => &["List", "Cool"],
        "Hash" => &["Hash", "Map", "Cool"],
        "Map" => &["Map", "Cool"],
        "Range" => &["Range", "Cool"],
        "Seq" => &["Seq", "Cool"],
        "Pair" => &["Pair"],
        _ => &[],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Types whose method lists must round-trip through the registry.
    const MODELLED_TYPES: &[&str] = &[
        "Str",
        "Int",
        "Num",
        "Rat",
        "Complex",
        "List",
        "Array",
        "Hash",
        "Bool",
        "Range",
        "Sub",
        "Signature",
        "IO::Path",
        "IO::Handle",
        "Cool",
        "Any",
        "Mu",
    ];

    #[test]
    fn no_duplicate_method_names_per_type() {
        for ty in MODELLED_TYPES {
            let names = builtin_type_method_names(ty);
            let mut seen = std::collections::HashSet::new();
            for name in &names {
                assert!(
                    seen.insert(*name),
                    "duplicate method `{name}` in built-in type `{ty}` list"
                );
            }
        }
    }

    #[test]
    fn native_probe_recognizes_per_type_methods() {
        // The probe must recognize a type's own native methods on its sample and
        // reject a method that belongs to a different type — this per-value
        // discrimination is what makes one shared `METHOD_UNIVERSE` correct.
        let s = builtin_sample_value("Str").unwrap();
        assert!(
            native_responds_to(&s, "chars"),
            "Str sample should do chars"
        );
        assert!(native_responds_to(&s, "uc"), "Str sample should do uc");
        // A Str has no native `abs` (it would need numeric coercion via the slow
        // path), so the probe must reject it — this is the discrimination that
        // lets one shared universe yield different sets per type.
        assert!(
            !native_responds_to(&s, "abs"),
            "Str sample must not claim native abs"
        );
        assert!(
            !native_responds_to(&s, "no-such-method-xyz"),
            "Str sample must not claim an unknown method"
        );

        let i = builtin_sample_value("Int").unwrap();
        assert!(native_responds_to(&i, "abs"), "Int sample should do abs");
        assert!(
            !native_responds_to(&i, "no-such-method-xyz"),
            "Int sample must not claim an unknown method"
        );
    }

    #[test]
    fn introspected_names_derive_native_surface() {
        // `.^methods` for Str must include native methods that were historically
        // missing from the hand-written list (the drift the probe fixes), and
        // must not list an unknown method.
        let str_methods = introspected_type_method_names("Str");
        for expected in ["chars", "uc", "samemark", "unimatch", "uniprops"] {
            assert!(
                str_methods.contains(&expected),
                "Str.^methods should include native method `{expected}`"
            );
        }
        assert!(
            !str_methods.contains(&"no-such-method-xyz"),
            "Str.^methods must not list a phantom method"
        );
    }

    #[test]
    fn method_catalog_is_keyed_by_owner_and_name() {
        let entries = builtin_method_entries("Str");
        let mut keys = std::collections::HashSet::new();
        for entry in entries {
            assert_eq!(entry.owner, "Str");
            assert!(
                keys.insert((entry.owner, entry.name)),
                "duplicate catalog entry for Str×{}",
                entry.name
            );
        }
        let chars = builtin_method_entries("Str")
            .into_iter()
            .find(|entry| entry.name == "chars")
            .expect("Str×chars entry");
        assert_eq!(chars.order, 0);
    }

    #[test]
    fn universe_excludes_universal_mu_any_methods() {
        // The universal Mu/Any methods (say/WHAT/defined/...) are reported via
        // the Any/Mu lists on `:all`, NOT the per-type probe — keeping them out
        // of the universe is what stops `Str.^methods` listing `say`.
        for forbidden in [
            "say", "put", "print", "note", "WHAT", "WHERE", "defined", "so", "not",
        ] {
            assert!(
                !METHOD_UNIVERSE.contains(&forbidden),
                "METHOD_UNIVERSE must not contain the universal Mu/Any method `{forbidden}`"
            );
        }
    }

    #[test]
    fn coercion_methods_present_on_every_numeric_leaf() {
        for ty in ["Str", "Int", "Num", "Rat", "Complex", "Bool", "Cool"] {
            let names = builtin_type_method_names(ty);
            for coercion in NUMERIC_COERCIONS {
                assert!(
                    names.contains(coercion),
                    "`{ty}` should report coercion method `{coercion}`"
                );
            }
        }
    }

    #[test]
    fn unmodelled_type_has_no_methods() {
        assert!(builtin_type_method_names("NoSuchType").is_empty());
        assert!(builtin_type_parents("NoSuchType").is_empty());
    }

    #[test]
    fn every_builtin_mro_parent_resolves_to_a_known_chain() {
        // Each parent named in a built-in MRO must itself be a type the registry
        // recognises (either modelled here or a higher base like Cool/Map), so a
        // walk never dead-ends on an unknown name.
        let known = |name: &str| {
            matches!(name, "Cool" | "Map" | "Any" | "Mu")
                || !builtin_type_parents(name).is_empty()
                || !builtin_type_method_names(name).is_empty()
        };
        for ty in [
            "Int", "Num", "Rat", "FatRat", "Complex", "Str", "Bool", "Array", "List", "Hash",
            "Map", "Range", "Seq", "Pair",
        ] {
            for parent in builtin_type_parents(ty) {
                assert!(known(parent), "MRO parent `{parent}` of `{ty}` is unknown");
            }
        }
    }
}
