use super::*;

/// Marks a `Mixin` overrides map produced by `but`/`does` with a *concrete
/// value* on the right (`1 but "hi"`, `$obj does 42`, `Method but True`)
/// rather than by role composition or by allomorph construction. See
/// `Interpreter::apply_single_mixin` for why raku needs the two apart.
pub(crate) const VALUE_MIXIN_MARKER: &str = "__mutsu_value_mixin__";

/// Returns the Raku type name for a value (used in error messages).
/// The name one argument of a CURRIED parametric role contributes to the
/// role's own name. Rakudo names `R["x"]` after the argument's TYPE
/// (`R[Str]`), never its value; an argument that already IS a type object
/// (`R[Int]`) keeps its own name, which is exactly what "the argument's type
/// name" reduces to there.
pub(crate) fn parametric_role_arg_name(val: &Value) -> String {
    match val.view() {
        ValueView::Package(name) => {
            crate::value::user_facing_type_name(&name.resolve()).into_owned()
        }
        // A role argument is itself a curried role (`Foo[R['x']]` is
        // `Foo[R[Str]]`), so its own arguments get the same treatment.
        ValueView::ParametricRole {
            base_name,
            type_args,
        } => parametric_role_name(&base_name.resolve(), type_args),
        // A NAMED argument keeps its current value-based spelling. Rakudo drops
        // named arguments from the curried name entirely (`A[:a(1)].^name` is
        // just `A`), but two distinct concretizations of the same role must
        // stay distinguishable by name here — the composition machinery keys on
        // it — so collapsing `A[:a(1)]` and `A[:a(2)]` onto one string is not
        // safe yet. Left as a separate, narrower divergence.
        ValueView::Pair(..) | ValueView::ValuePair(..) => val.to_string_value(),
        _ => what_type_name(val),
    }
}

/// The full `Base[Arg,Arg]` name of a curried parametric role.
pub(crate) fn parametric_role_name(base_name: &str, type_args: &[Value]) -> String {
    let args: Vec<String> = type_args.iter().map(parametric_role_arg_name).collect();
    format!("{}[{}]", base_name, args.join(","))
}

pub(crate) fn what_type_name(val: &Value) -> String {
    match val.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => "Int".to_string(),
        ValueView::Num(_) => "Num".to_string(),
        ValueView::Str(_) => "Str".to_string(),
        ValueView::Bool(_) => "Bool".to_string(),
        ValueView::BigRat(_, _) if val.is_bigfatrat() => "FatRat".to_string(),
        ValueView::Rat(_, _) | ValueView::BigRat(_, _) => "Rat".to_string(),
        ValueView::FatRat(_, _) => "FatRat".to_string(),
        ValueView::Complex(_, _) => "Complex".to_string(),
        ValueView::Array(..) | ValueView::LazyList(_) => "Array".to_string(),
        ValueView::Seq(_) => "Seq".to_string(),
        ValueView::HyperSeq(_) => "HyperSeq".to_string(),
        ValueView::RaceSeq(_) => "RaceSeq".to_string(),
        ValueView::Hash(..) => "Hash".to_string(),
        ValueView::Set(_, is_mutable) => {
            if is_mutable {
                "SetHash".to_string()
            } else {
                "Set".to_string()
            }
        }
        ValueView::Bag(_, is_mutable) => {
            if is_mutable {
                "BagHash".to_string()
            } else {
                "Bag".to_string()
            }
        }
        ValueView::Mix(_, is_mutable) => {
            if is_mutable {
                "MixHash".to_string()
            } else {
                "Mix".to_string()
            }
        }
        ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => "Pair".to_string(),
        ValueView::Range(_, _)
        | ValueView::RangeExcl(_, _)
        | ValueView::RangeExclStart(_, _)
        | ValueView::RangeExclBoth(_, _)
        | ValueView::GenericRange { .. } => "Range".to_string(),
        ValueView::Nil => "Nil".to_string(),
        // Without this arm a Capture fell through to the `_ => "Any"` default,
        // so `(\(1, 2) but R).^name` answered `Any+{R}` where rakudo says
        // `Capture+{R}` (the shape `X::AdHoc.from-slurpy` produces).
        ValueView::Capture { .. } => "Capture".to_string(),
        // ADR-0047: a lexically-scoped `my class`/`my grammar` (including a
        // nested one, e.g. `my monitor Store { my class Session {...} }`)
        // registers its instances/type object under a mangled storage name
        // (`Foo\u{0}<decl-id>`, possibly with more than one `\u{0}` segment
        // for a nested declaration). `what_type_name` is used to build
        // user-facing text (error messages, `.^name`-style displays), so it
        // must report the demangled, user-facing name here rather than the
        // raw storage key leaking a literal NUL byte and decl-id number into
        // messages like "Type check failed in assignment ... but got
        // Store\u{0}12::Session\u{0}13". The same helper also qualifies
        // NativeCall's builtin type names (`Pointer` ->
        // `NativeCall::Types::Pointer`, see ADR-0056), so both concerns are
        // handled by routing through it here rather than reading the raw
        // Symbol.
        ValueView::Instance { class_name, .. } => {
            crate::value::user_facing_type_name(&class_name.resolve()).into_owned()
        }
        ValueView::Package(name) => {
            crate::value::user_facing_type_name(&name.resolve()).into_owned()
        }
        ValueView::Enum { enum_type, .. } => enum_type.resolve(),
        ValueView::Sub(_) | ValueView::WeakSub(_) => "Sub".to_string(),
        ValueView::Routine { .. } => "Sub".to_string(),
        ValueView::Regex(_) => "Regex".to_string(),
        ValueView::Junction { .. } => "Junction".to_string(),
        ValueView::Slip(_) => "Slip".to_string(),
        ValueView::Uni(u) if !u.form.is_empty() => u.form.clone(),
        ValueView::Uni(_) => "Uni".to_string(),
        ValueView::Mixin(inner, mixins) => {
            if let Some(name) = allomorph_type_name(inner, mixins) {
                // An allomorph with a role composed onto it keeps both halves:
                // `<42> but R` is `IntStr+{R}` in raku.
                match role_mixin_suffix_excluding(mixins, &name) {
                    Some(suffix) => format!("{name}+{{{suffix}}}"),
                    None => name,
                }
            } else {
                let base = what_type_name(inner);
                // A punned role (`R.new`) is `Mixin(Instance{R}, {__mutsu_role__R})`
                // — the role composed onto its OWN same-named (empty) instance, not
                // a mixin onto a different base. Raku names that plain `R`, so drop
                // a suffix entry that merely repeats the base type. A role mixed
                // onto a different base still gets the suffix (`W but R` -> `W+{R}`).
                match role_mixin_suffix_excluding(mixins, &base) {
                    Some(suffix) => format!("{base}+{{{suffix}}}"),
                    None => base,
                }
            }
        }
        ValueView::ContainerRef(_) => val.with_deref(what_type_name),
        _ => "Any".to_string(),
    }
}

/// Build the `+{Role,...}` suffix for a role-mixed value, if any roles were
/// composed in. Role mixins are recorded under `__mutsu_role__{name}` keys (a
/// double underscore distinguishes them from the bookkeeping keys
/// `__mutsu_role_id__` / `__mutsu_role_typeargs__` / `__mutsu_role_param__`).
/// Returns e.g. `Foo::Bar` for `5 but Foo::Bar` so `.^name` reads `Int+{Foo::Bar}`.
pub(crate) fn role_mixin_suffix(
    mixins: &std::collections::HashMap<String, Value>,
) -> Option<String> {
    role_mixin_suffix_excluding(mixins, "")
}

/// [`role_mixin_suffix`], but skipping the role whose name equals `base` — the
/// role-punning case, where `R.new` builds `Mixin(Instance{R}, __mutsu_role__R)`
/// and raku reports plain `R` rather than `R+{R}`. Pass `""` to exclude nothing.
pub(crate) fn role_mixin_suffix_excluding(
    mixins: &std::collections::HashMap<String, Value>,
    base: &str,
) -> Option<String> {
    let mut names: Vec<String> = mixins
        .keys()
        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
        .filter(|n| *n != base)
        .map(|n| role_mixin_suffix_entry(mixins, n))
        .collect();
    // `but`-mixing a plain value composes an anonymous role too, recorded under
    // its own marker rather than as a `__mutsu_role__` entry (see
    // `Interpreter::apply_single_mixin`); it still shows in the name suffix.
    if let Some(anon) = mixins.get(VALUE_MIXIN_MARKER) {
        names.push(crate::value::user_facing_type_name(&anon.to_string_value()).into_owned());
    }
    if names.is_empty() {
        return None;
    }
    // HashMap iteration order is non-deterministic; sort for a stable name.
    names.sort_unstable();
    Some(names.join(","))
}

/// Render one `+{...}` suffix entry for the composed role `role_name`.
///
/// Two things beyond the bare name matter here, both because raku shows them:
///
/// * An anonymous role (`but role { }`) is stored under a compiler-internal
///   `__ANON_ROLE_{id}__` key. Rakudo names it `<anon|N>`, and
///   [`crate::value::user_facing_type_name`] already knows that mapping (it is
///   the same one an anonymous `class`/`grammar` gets in a `.gist`). mutsu's
///   `N` is its own counter and will not equal Rakudo's, but the *shape* is
///   what identifies a mixin as anonymous -- this used to filter anon roles
///   out entirely, so `(@a but role { ... }).^name` reported a bare `Array`
///   and lost every trace of the composition.
/// * A parameterised role keeps its type arguments in the name
///   (`Int+{G[Int]}`, `Hash+{Associative[Int,Int]}`), read back from the
///   `__mutsu_role_typeargs__{name}` marker recorded alongside the role marker.
fn role_mixin_suffix_entry(
    mixins: &std::collections::HashMap<String, Value>,
    role_name: &str,
) -> String {
    let display = crate::value::user_facing_type_name(role_name).into_owned();
    // An already-parameterised spelling (a role registered under a bracketed
    // name) must not get a second `[...]` appended.
    if display.contains('[') {
        return display;
    }
    let Some(args) = mixins.get(&format!("__mutsu_role_typeargs__{role_name}")) else {
        return display;
    };
    let ValueView::Array(items, _) = args.view() else {
        return display;
    };
    if items.is_empty() {
        return display;
    }
    let rendered: Vec<String> = items.items().iter().map(what_type_name).collect();
    format!("{display}[{}]", rendered.join(","))
}

/// Build the stable composition key for a role-mixed value's `.WHAT` identity
/// (ADR-0060): the base type name plus the sorted set of
/// `(role_name, role_id, typeargs)` triples this `Mixin`'s `overrides`
/// records for genuine composition markers (`__mutsu_role__*` and the
/// `__mutsu_role_id__*`/`__mutsu_role_typeargs__*` data recorded alongside
/// each). Two `Mixin` values compose to the identical key iff they mix the
/// exact same role declarations (by declaration-site identity, not by name
/// — `role_id` disambiguates two distinct `my role A {}` sharing a name,
/// mirroring ADR-0047's "declaration site, not registry name" principle)
/// with the same type arguments onto the same base type.
///
/// Deliberately EXCLUDES: `__mutsu_attr__*` (per-instance role-attribute
/// values — two differently-initialized instances of the same role must
/// still share one `.WHAT`), `__mutsu_type_name__` (the mutable
/// `.^set_name` target — state ON the cache entry this key looks up, not
/// part of the key itself), `__mutsu_role_seq__*` (a per-application-order
/// bookkeeping stamp that differs even between two instances of the exact
/// same composition — including it in the key was tried and broke
/// `roast/S14-roles/instantiation.t`'s punned-role identity invariant, see
/// ADR-0060), `__mutsu_role_param__*` (derived from data already captured
/// by typeargs), and every other non-composition key this flat map can
/// carry (`__mutsu_var_target`, `__mutsu_how_target`, `__mutsu_topic_ro__`,
/// the allomorph `"Str"` key, `__mutsu_language_revision`, ...).
pub(crate) fn mixin_composition_key(
    base_type_name: &str,
    mixins: &std::collections::HashMap<String, Value>,
) -> String {
    let mut parts: Vec<String> = mixins
        .keys()
        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
        .map(|role_name| {
            let role_id = mixins
                .get(&format!("__mutsu_role_id__{role_name}"))
                .map(Value::to_string_value)
                .unwrap_or_default();
            let typeargs = mixins
                .get(&format!("__mutsu_role_typeargs__{role_name}"))
                .map(|v| match v.view() {
                    ValueView::Array(items, _) => items
                        .items()
                        .iter()
                        .map(what_type_name)
                        .collect::<Vec<_>>()
                        .join(","),
                    _ => v.to_string_value(),
                })
                .unwrap_or_default();
            // NUL/SOH are not valid in a role name, base type name, or
            // typearg display string, so they are safe field/entry
            // separators for a key that must not collide across differently
            // -split components.
            format!("{role_name}\u{0}{role_id}\u{0}{typeargs}")
        })
        .collect();
    // HashMap iteration order is non-deterministic; sort for a stable key.
    parts.sort_unstable();
    format!("{base_type_name}\u{1}{}", parts.join("\u{1}"))
}

/// Filter a `Mixin` value's `overrides` down to just the composition-
/// defining markers (`__mutsu_role__*`, `__mutsu_role_id__*`,
/// `__mutsu_role_typeargs__*`, `__mutsu_role_param__*`) — the subset that
/// belongs on the shared, composition-keyed `.WHAT` type object
/// ([`mixin_composition_key`]'s cache entry, ADR-0060) when it is first
/// created. Populating the fresh entry with these markers (rather than
/// leaving it empty) does double duty: `.^name`/`what_type_name` on the
/// `.WHAT` value itself can synthesize the right `Base+{Role,...}` display
/// without any extra lookup, and two DIFFERENT compositions get
/// content-different overrides maps (rather than two structurally-equal
/// empty maps), which matters because `values_identical`'s `Mixin` arm
/// (`src/runtime/utils/shaped.rs`) compares overrides by content, not by
/// `Gc` pointer.
///
/// Drops per-instance data: `__mutsu_attr__*` (role-attribute values),
/// `__mutsu_role_seq__*` (per-application-order bookkeeping),
/// `__mutsu_type_name__` (the mutable `.^set_name` target — written later,
/// in place, onto the cache entry itself), and any other bookkeeping key.
pub(crate) fn filter_composition_markers(
    mixins: &std::collections::HashMap<String, Value>,
) -> std::collections::HashMap<String, Value> {
    mixins
        .iter()
        .filter(|(k, _)| {
            k.starts_with("__mutsu_role__")
                || k.starts_with("__mutsu_role_id__")
                || k.starts_with("__mutsu_role_typeargs__")
                || k.starts_with("__mutsu_role_param__")
        })
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect()
}

/// Return the allomorphic type name for a Mixin value, if it is allomorphic.
/// An allomorphic Mixin has a "Str" key and a numeric inner value.
pub(crate) fn allomorph_type_name(
    inner: &Value,
    mixins: &std::collections::HashMap<String, Value>,
) -> Option<String> {
    if !mixins.contains_key("Str") {
        return None;
    }
    // `1 but "hi"` has the same `{Str => ...}` shape as an allomorph but is a
    // role composition in raku (`Int+{<anon|1>}`, and NOT `~~ Str`), so a map
    // carrying the value-mixin marker is never an allomorph. A genuine
    // allomorph that later gets a role mixed in keeps its allomorph identity
    // (`<42> but R` is `IntStr+{R}`), which is why the test is for this marker
    // rather than for "any role marker present".
    if mixins.contains_key(VALUE_MIXIN_MARKER) {
        return None;
    }
    match inner.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Some("IntStr".to_string()),
        ValueView::Num(_) => Some("NumStr".to_string()),
        ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _) => {
            Some("RatStr".to_string())
        }
        ValueView::Complex(_, _) => Some("ComplexStr".to_string()),
        _ => None,
    }
}

/// Build the result of `.wordcase` on an allomorph: rakudo's `Cool.wordcase`
/// on an `IntStr`/`NumStr`/`RatStr`/`ComplexStr` returns ANOTHER allomorph of
/// the same type, with the numeric part unconditionally reset to the type's
/// zero value (0 / 0e0 / 0+0i) rather than the original number — an artifact
/// of how the allomorph gets reconstructed internally (verified across all
/// four types; only the wordcased STRING carries real information). rakudo's
/// own `RatStr` reset is additionally broken: the reconstructed Rat's
/// numerator/denominator are genuinely uninitialized, so `.raku`/any numeric
/// op on the result crashes. mutsu uses the sane 0/1 zero Rat there instead of
/// replicating that crash. See
/// news/2026-08/allomorph-wordcase-reads-the-numeric-part.md.
///
/// `inner` is the allomorph's numeric component (used only to pick which
/// "zero" shape to build); `wordcased` is the already-wordcased string.
pub(crate) fn allomorph_wordcase_result(inner: &Value, wordcased: String) -> Value {
    let zero_numeric = match inner.view() {
        ValueView::Num(_) => Value::num(0.0),
        ValueView::Rat(_, _) => make_rat(0, 1),
        ValueView::FatRat(_, _) => Value::fat_rat_raw(0, 1),
        ValueView::BigRat(_, _) => {
            make_big_rat(num_bigint::BigInt::from(0), num_bigint::BigInt::from(1))
        }
        ValueView::Complex(_, _) => Value::complex(0.0, 0.0),
        // Int / BigInt allomorph (IntStr).
        _ => Value::int(0),
    };
    let mut new_mixins = std::collections::HashMap::new();
    new_mixins.insert("Str".to_string(), Value::str(wordcased));
    Value::mixin(zero_numeric, new_mixins)
}
