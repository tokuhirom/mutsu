//! Canonical introspection registry for built-in (non-user-defined) types.
//!
//! `.^methods`, `.^can`, and `.^mro` need to report which methods a built-in
//! type such as `Str`/`Int`/`List` responds to, and what its parent chain is.
//! Built-in types have no user-level class definition, so this information
//! cannot be read off the `registry().classes` map; it lives here instead.
//!
//! ## Why this is a table and not derived automatically
//!
//! mutsu's native method dispatch (`builtins/methods_0arg/`,
//! `builtins/methods_narg/`, the `runtime/methods.rs` slow path) is implemented
//! as `match method { "chars" => ..., }` arms interleaved with `Value`-variant
//! matching. There is no enumerable per-type registry to introspect, so the set
//! of methods a type responds to cannot be reflected out of the dispatch code at
//! runtime. Fully deriving these lists requires the unified type x method
//! dispatch table tracked in `ANALYSIS.md` section 3 — until that lands, this
//! module is the single source of truth.
//!
//! ## Keep in sync with dispatch
//!
//! When you add, remove, or rename a native method for a built-in type in
//! `builtins/methods_*`, update the matching list below. This is the ONLY place
//! the per-type method names and the built-in MRO are declared, so a single edit
//! here keeps `.^methods`, `.^can`, and `.^mro` consistent. `t/can-methods-drift.t`
//! and the unit tests at the bottom of this file guard against the lists drifting
//! out of sync (each listed method must genuinely dispatch and introspect).

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
