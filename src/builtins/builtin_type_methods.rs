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
//! ## Single source of truth (ADR-0019 Phase F box F3)
//!
//! [`builtin_type_method_names`] reads the name list straight off
//! `native_method_row::RAW_ROWS`'s `INTROSPECTABLE`-flagged rows for the
//! folded owner, in table order -- there is no longer a hand-maintained
//! per-type name slice in this file. `RAW_ROWS` carries many more
//! dispatch-recognized names per owner than `.^methods` actually lists (real
//! Rakudo does not enumerate every internal/dispatch-only name); the
//! `INTROSPECTABLE` bit marks exactly the raku-verified genuine
//! `.^methods` entries (F3 step 2's owner-by-owner triage). Add a new
//! genuine introspection name by setting that bit on its row in
//! `native_method_row_table.rs`, not by adding a slice here.
//! `t/can-methods-drift.t` guards the callable/introspectable contract.

#[cfg(test)]
use crate::symbol::Symbol;
#[cfg(test)]
use crate::value::Value;

/// The method names a built-in `type_name` responds to, in `.^methods` order.
/// Returns an empty `Vec` for types not modelled here (e.g. user classes, which
/// are handled separately via `registry().classes`).
pub(crate) fn builtin_type_method_names(type_name: &str) -> Vec<&'static str> {
    let owner = canonical_builtin_owner(type_name);
    if owner.is_empty() {
        return Vec::new();
    }
    crate::builtins::native_method_row::introspectable_names_for_owner(owner)
}

/// A representative sample VALUE for a *concrete* built-in type, used by
/// `native_method_row.rs`'s inverse-probe tests to confirm a `RAW_ROWS` row's
/// claimed arity is actually backed by the real native dispatch cascade.
/// Abstract types (`Any`/`Mu`/`Cool`) and types without an easily-constructed
/// instance return `None`.
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
        // A type's own native methods must be recognized on its sample, and a
        // method belonging to a different type must be rejected -- this
        // per-value discrimination is what `native_method_arities` gives
        // `RAW_ROWS`'s inverse-probe tests in `native_method_row.rs`.
        let s = builtin_sample_value("Str").unwrap();
        assert_ne!(
            native_method_arities(&s, "chars"),
            0,
            "Str sample should do chars"
        );
        assert_ne!(
            native_method_arities(&s, "uc"),
            0,
            "Str sample should do uc"
        );
        // A Str has no native `abs` (it would need numeric coercion via the slow
        // path), so the probe must reject it.
        assert_eq!(
            native_method_arities(&s, "abs"),
            0,
            "Str sample must not claim native abs"
        );
        assert_eq!(
            native_method_arities(&s, "no-such-method-xyz"),
            0,
            "Str sample must not claim an unknown method"
        );

        let i = builtin_sample_value("Int").unwrap();
        assert_ne!(
            native_method_arities(&i, "abs"),
            0,
            "Int sample should do abs"
        );
        assert_eq!(
            native_method_arities(&i, "no-such-method-xyz"),
            0,
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
    fn str_methods_exclude_universal_mu_any_methods() {
        // The universal Mu/Any methods (say/WHAT/defined/...) are reported via
        // the Any/Mu lists on `:all`, NOT `Str`'s own -- `RAW_ROWS`'s
        // `INTROSPECTABLE` rows for `Str` must not include any of them.
        let str_methods = builtin_type_method_names("Str");
        for forbidden in [
            "say", "put", "print", "note", "WHAT", "WHERE", "defined", "so", "not",
        ] {
            assert!(
                !str_methods.contains(&forbidden),
                "Str.^methods must not contain the universal Mu/Any method `{forbidden}`"
            );
        }
    }

    #[test]
    fn coercion_methods_present_on_every_numeric_leaf() {
        const NUMERIC_COERCIONS: &[&str] = &[
            "Numeric", "Int", "Num", "Rat", "Bool", "Str", "gist", "raku",
        ];
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
    }

    #[test]
    fn every_builtin_mro_parent_resolves_to_a_known_chain() {
        // Each parent named in a built-in MRO (ADR-0051 P1: read from the
        // catalog, the single source of truth for built-in type ancestry) must
        // itself be a type the registry recognises (either modelled here or a
        // higher base like Cool/Map), so a walk never dead-ends on an unknown
        // name.
        let known = |name: &str| {
            matches!(name, "Cool" | "Map" | "Any" | "Mu")
                || crate::builtins::builtin_type_catalog::builtin_type_info(name).is_some()
                || !builtin_type_method_names(name).is_empty()
        };
        for ty in [
            "Int", "Num", "Rat", "FatRat", "Complex", "Str", "Bool", "Array", "List", "Hash",
            "Map", "Range", "Seq", "Pair",
        ] {
            let Some(info) = crate::builtins::builtin_type_catalog::builtin_type_info(ty) else {
                continue;
            };
            for parent in info.mro {
                assert!(known(parent), "MRO parent `{parent}` of `{ty}` is unknown");
            }
        }
    }
}
