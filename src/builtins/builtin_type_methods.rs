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
//! ## Single source of truth (ADR-14032)
//!
//! [`builtin_type_method_names`] reads Rakudo's `.^methods` list for the
//! folded owner from the committed snapshot, kept to the names a method row
//! reaches (`method_introspection::introspectable_names_for_owner`). There is
//! no hand-maintained name list in this file or anywhere else.
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
    crate::builtins::method_introspection::introspectable_names_for_owner(owner)
}

/// Whether `method` is one of [`builtin_type_method_names`]`(type_name)`,
/// without materialising the list.
// Cost: O(1), one hash probe.
pub(crate) fn builtin_type_method_listed(type_name: &str, method: &str) -> bool {
    let owner = canonical_builtin_owner(type_name);
    !owner.is_empty()
        && crate::builtins::method_introspection::introspectable_name_listed(owner, method)
}

/// A representative sample VALUE for a *concrete* built-in type, for the tests
/// below that probe the native dispatch on it.
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
        "Hash" => Value::hash(crate::value::ValueMap::from_iter([(
            "a".to_string(),
            Value::int(1),
        )])),
        "Range" => Value::range(1, 3),
        _ => return None,
    })
}

#[cfg(test)]
pub(crate) fn native_method_arities(value: &Value, method_name: &str) -> u8 {
    // `Mu.perl` is `self.raku` (ADR-11276 §9.152): the call entries resolve
    // the one name to the other, so a receiver takes `perl` where it takes `raku`.
    let method_name = if method_name == "perl" {
        "raku"
    } else {
        method_name
    };
    let sym = Symbol::intern(method_name);
    let mut arities = u8::from(crate::builtins::native_method_0arg(value, sym).is_some());
    // A registered row recognizes its arity for a receiver of its shape
    // (ADR-11276), table first: a migrated method has no arm left to probe,
    // and `native_method_0arg` asks the table only for a call it would admit.
    // An itemized value (`my $range = 1..10`) is asked about its value.
    let receivers = [
        crate::builtins::method_table::Receiver::of(value),
        crate::builtins::method_table::Receiver::of_settled(value.descalarize()),
    ];
    for receiver in receivers.into_iter().flatten() {
        for arity in 0..=2u8 {
            if crate::builtins::method_table::resolve(receiver, sym, usize::from(arity)).is_some() {
                arities |= 1 << arity;
            }
        }
    }
    // A receiver with no shape reaches the rows `answer_shapeless` finds for
    // it (ADR-11276 §9.79-§9.81); a row that needs the interpreter to finish
    // (an element's own `Str`) still recognizes the call.
    for arity in 0..=2u8 {
        if crate::builtins::method_table::shapeless_row(None, value, sym, usize::from(arity))
            .is_some()
        {
            arities |= 1 << arity;
        }
    }
    // A value with no shape of its own (an immutable `Map`) reaches the rows
    // of its type's MRO through the owner chain (`method_table::invoke_owner`,
    // ADR-11276 §9.113).
    if crate::builtins::method_table::Receiver::of(value).is_none()
        && let Some(mro) = crate::builtin_types::catalog::builtin_type_mro_strs(
            &crate::value::what_type_name(value),
        )
    {
        for owner in mro {
            for arity in 0..=2u8 {
                if crate::builtins::method_table::owner_row(
                    Symbol::intern(owner),
                    sym,
                    usize::from(arity),
                )
                .is_some()
                {
                    arities |= 1 << arity;
                }
            }
        }
    }
    // A type object a `:D` row refuses has the method, as Rakudo's `.^can`
    // says (`Int.^can('abs')`); the call fails binding the invocant.
    if let Some(shape) = value.type_object_shape() {
        let instance = crate::builtins::method_table::Receiver::instance(shape);
        for arity in 0..=2u8 {
            if let Some(id) =
                crate::builtins::method_table::resolve(instance, sym, usize::from(arity))
                && crate::builtins::method_table::row(id)
                    .type_object_refusal("")
                    .is_some()
            {
                arities |= 1 << arity;
            }
        }
    }
    // A value of a built-in class with no shape reaches the rows its own class
    // declares (`invoke_owner` from the instance dispatch): a RakuAST node's
    // builders, an `Attribute`'s accessors.
    if crate::builtins::method_table::Receiver::of(value).is_none() {
        let own = match value.view() {
            crate::value::ValueView::RakuAst(node) => Symbol::intern(node.class.printed_name()),
            _ => Symbol::intern(&crate::value::what_type_name(value)),
        };
        // A built-in exception class (every `X::...` is one) also reaches the
        // rows of `Exception`, which its MRO ends in (its control methods,
        // ADR-11276 §9.163).
        let exception = value
            .instance_is_exception_by_name()
            .then(|| Symbol::intern("Exception"));
        for owner in std::iter::once(own).chain(exception) {
            for arity in 0..=2u8 {
                if crate::builtins::method_table::owner_row(owner, sym, usize::from(arity))
                    .is_some()
                {
                    arities |= 1 << arity;
                }
            }
        }
    }
    // Every receiver reaches `Mu`'s audited rows (ADR-11276 §9.81, §9.91); an
    // instance through its MRO, which `shapeless_row` reads only from the
    // interpreter it is not given here.
    if crate::value::MU_FOR_CLOSED_SHAPES.contains(&method_name) {
        let mu = Symbol::intern("Mu");
        for arity in 0..=2u8 {
            if crate::builtins::method_table::owner_row(mu, sym, usize::from(arity)).is_some() {
                arities |= 1 << arity;
            }
        }
    }
    // A few 1-arg native methods inspect the argument type before recognizing
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
    arities
}

/// The built-in owners whose `.^methods` the introspection catalog answers.
/// Aliases such as `FatRat` and `Method` resolve through [`canonical_builtin_owner`]
/// and therefore do not need duplicate table rows.
#[cfg(test)]
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
    builtin_type_method_names(type_name)
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
        "Backtrace::Frame" => &[
            ("file", "Str"),
            ("line", "Int"),
            ("code", "Code"),
            ("subname", "Str"),
        ],
        _ => &[],
    }
}

/// Whether a modelled built-in attribute has a public accessor (`.year` on
/// DateTime, ...). The numeric internals (`Rat.$!numerator`, ...) stay private.
pub(crate) fn builtin_type_attr_has_accessor(type_name: &str, _attr_name: &str) -> bool {
    matches!(type_name, "DateTime" | "Backtrace::Frame")
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
        // per-value discrimination is what `native_method_arities` gives.
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
        // A Str has no native `lsb` (Rakudo declares it on `Int`, not on `Cool`),
        // so the probe must reject it. (`abs` is `Cool`'s, which a Str reaches.)
        assert_eq!(
            native_method_arities(&s, "lsb"),
            0,
            "Str sample must not claim native lsb"
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
    fn str_methods_exclude_universal_mu_any_methods() {
        // The universal Mu/Any methods (say/WHAT/defined/...) are reported via
        // the Any/Mu lists on `:all`, NOT `Str`'s own -- Rakudo's `Str.^methods`
        // does not list any of them.
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
    fn coercion_method_ownership_matches_rakudo() {
        const NUMERIC_COERCIONS: &[&str] = &[
            "Numeric", "Int", "Num", "Rat", "Bool", "Str", "gist", "raku",
        ];
        let snapshot = include_str!("rakudo_method_tables.txt");
        for ty in ["Str", "Int", "Num", "Rat", "Complex", "Bool", "Cool"] {
            let names = builtin_type_method_names(ty);
            let rakudo_names: Vec<&str> = snapshot
                .lines()
                .map(|line| line.split('\t').collect::<Vec<_>>())
                .find(|fields| fields.first() == Some(&"methods") && fields.get(1) == Some(&ty))
                .expect("Rakudo methods for built-in owner")
                .into_iter()
                .skip(2)
                .collect();
            for coercion in NUMERIC_COERCIONS {
                assert_eq!(
                    names.contains(coercion),
                    rakudo_names.contains(coercion),
                    "`{ty}` should agree with Rakudo on local coercion `{coercion}`"
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
                || crate::builtin_types::catalog::builtin_type_info(name).is_some()
                || !builtin_type_method_names(name).is_empty()
        };
        for ty in [
            "Int", "Num", "Rat", "FatRat", "Complex", "Str", "Bool", "Array", "List", "Hash",
            "Map", "Range", "Seq", "Pair",
        ] {
            let Some(info) = crate::builtin_types::catalog::builtin_type_info(ty) else {
                continue;
            };
            for parent in info.mro {
                assert!(known(parent), "MRO parent `{parent}` of `{ty}` is unknown");
            }
        }
    }
}

#[cfg(test)]
mod listed_tests {
    use super::*;

    /// The O(1) membership probe agrees with the enumerated name list.
    #[test]
    fn listed_matches_names_for_every_owner() {
        for owner in BUILTIN_METHOD_OWNERS {
            let names = builtin_type_method_names(owner);
            for name in &names {
                assert!(builtin_type_method_listed(owner, name), "{owner}.{name}");
            }
            assert!(!builtin_type_method_listed(owner, "no-such-method"));
        }
        assert!(builtin_type_method_listed("IO::Path", "lines"));
        assert!(!builtin_type_method_listed("IO::Path", "chars"));
        assert!(!builtin_type_method_listed("NoSuchType", "lines"));
    }
}
