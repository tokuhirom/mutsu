//! Canonical introspection registry for built-in (non-user-defined) types.
//!
//! `.^methods`, `.^can`, and `.^mro` need to report which methods a built-in
//! type such as `Str`/`Int`/`List` responds to, and what its parent chain is.
//! Built-in types have no user-level class definition, so this information
//! cannot be read off the `registry().classes` map; it lives here instead.
//!
//! ## Derived by probing, not a hand-maintained per-type table
//!
//! mutsu's native method dispatch (`builtins/methods_0arg/`,
//! `builtins/methods_narg/`) is `match method { "chars" => ..., }` arms, so the
//! set of methods a type responds to cannot be *enumerated* out of the dispatch
//! code. But it CAN be *probed*: `native_method_*arg(value, name).is_some()`
//! answers "does this value's dispatch recognize `name`?" — the same recognition
//! `.^can` relies on.
//!
//! So for a concrete type with a representative [`builtin_sample_value`] (Str,
//! Int, List, Hash, ...), [`introspected_type_method_names`] derives `.^methods`
//! by filtering a broad [`METHOD_UNIVERSE`] of candidate names through
//! [`native_responds_to`] against the sample. A name the type does not actually
//! dispatch is dropped, so `.^methods` stays consistent with `.^can` and cannot
//! list a phantom method. This replaces the former hand-maintained per-type
//! lists for the native surface.
//!
//! Two things still need declared data, kept in the `*_METHODS` consts below:
//! - **slow-path methods** (block-taking `map`/`grep`/`sort`/... live behind
//!   `&mut self` and are invisible to the pure native probe), and
//! - **abstract / sample-less types** (`Any`/`Mu`/`Cool`/`Sub`/`IO::*`), which
//!   have no instance to probe.
//!
//! These are unioned in by `introspected_type_method_names`.
//!
//! Because the probe is per-*value*, `.^methods` reports every method the value
//! can respond to (including ones inherited from Cool/Any), so it is a superset
//! of Rakudo's own-level `.^methods` — but it is internally consistent with
//! `.^can`, and the roast introspection tests (which assert only non-emptiness
//! and the `List(:all) > Any(:all) > Mu` ordering for built-ins) pass.
//!
//! ## Keep in sync with dispatch
//!
//! When you add a *native* method whose NAME is not already present, add it to
//! `METHOD_UNIVERSE`; the probe then surfaces it for every type that dispatches
//! it. Slow-path / abstract-type methods go in the relevant `*_METHODS` const.
//! `t/can-methods-drift.t` and the unit tests below guard consistency.

use crate::symbol::Symbol;
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
pub(crate) fn builtin_type_method_names(type_name: &str) -> Vec<&'static str> {
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
pub(crate) fn builtin_sample_value(type_name: &str) -> Option<Value> {
    Some(match type_name {
        "Str" => Value::str_from("abc"),
        "Int" => Value::Int(2),
        "Num" => Value::Num(1.5),
        "Rat" | "FatRat" => crate::value::make_rat(1, 2),
        "Complex" => Value::Complex(1.0, 2.0),
        "Bool" => Value::Bool(true),
        "List" => Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
        "Array" => Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(vec![
                Value::Int(1),
                Value::Int(2),
            ])),
            crate::value::ArrayKind::Array,
        ),
        "Hash" => Value::hash(std::collections::HashMap::from([(
            "a".to_string(),
            Value::Int(1),
        )])),
        "Range" => Value::Range(1, 3),
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
pub(crate) fn native_responds_to(value: &Value, method_name: &str) -> bool {
    let sym = Symbol::intern(method_name);
    if crate::builtins::native_method_0arg(value, sym).is_some() {
        return true;
    }
    // A few 1/2-arg native methods inspect the argument type before recognizing
    // the call (e.g. `index`/`indices` want a Str), so a single dummy can miss
    // them. Try a small spread of representative arguments — recognition just
    // needs ONE arity/arg shape to return `Some`.
    let dummies = [Value::Nil, Value::Int(0), Value::str_from("")];
    dummies
        .iter()
        .any(|a| crate::builtins::native_method_1arg(value, sym, a).is_some())
        || dummies
            .iter()
            .any(|a| crate::builtins::native_method_2arg(value, sym, a, a).is_some())
}

/// Generous master set of built-in method NAMES (excluding the universal
/// `Mu`/`Any` methods such as `say`/`WHAT`/`defined`, which are reported via the
/// `Any`/`Mu` lists on `:all`). `.^methods` for a concrete type filters this
/// through `native_responds_to(sample, name)`, so a name listed here that the
/// type does not actually dispatch is silently dropped — making the universe
/// safe to keep broad. Add a name here when introducing a native method whose
/// name is not already present.
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

/// The method names a built-in `type_name` exposes for `.^methods` / `.^can`.
/// For concrete types with a sample value the set is *derived* by probing the
/// real native dispatch (`METHOD_UNIVERSE` ∩ what the sample responds to); for
/// abstract/sample-less types it falls back to the declared list.
pub(crate) fn introspected_type_method_names(type_name: &str) -> Vec<&'static str> {
    let Some(sample) = builtin_sample_value(type_name) else {
        return builtin_type_method_names(type_name);
    };
    let mut names: Vec<&'static str> = METHOD_UNIVERSE
        .iter()
        .copied()
        .filter(|m| native_responds_to(&sample, m))
        .collect();
    // Union in any declared name the native probe can't reach (slow-path,
    // block-taking methods like map/grep/sort live behind `&mut self` and are
    // not visible to the pure native probe), so nothing regresses.
    for declared in builtin_type_method_names(type_name) {
        if !names.contains(&declared) {
            names.push(declared);
        }
    }
    names
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
