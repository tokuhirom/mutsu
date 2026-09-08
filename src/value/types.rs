use super::*;

/// Marks a `Mixin` overrides map produced by `but`/`does` with a *concrete
/// value* on the right (`1 but "hi"`, `$obj does 42`, `Method but True`)
/// rather than by role composition or by allomorph construction. See
/// `Interpreter::apply_single_mixin` for why raku needs the two apart.
pub(crate) const VALUE_MIXIN_MARKER: &str = "__mutsu_value_mixin__";

/// Prefix of the PER-APPLICATION record of an anonymous role minted by
/// `but <non-role>` (`__mutsu_anon_role__<anon|3>`).
///
/// [`VALUE_MIXIN_MARKER`] alone could not carry these: it is one key, so a
/// second `but "y"` overwrote the first's minted name (`(1 but "x") but "y"`
/// reported a single `Int+{<anon|2>}`), and it holds no application stamp, so
/// the anonymous role could only ever be rendered last (`(1 but "x") but A`
/// came out `Int+{A}+{<anon|1>}` instead of raku's `Int+{<anon|1>}+{A}`).
///
/// One key per application fixes both: the name is IN the key, and the entry
/// carries the same `__mutsu_role_seq__{name}` / `__mutsu_role_group__{name}`
/// stamps a named role does, so the two orders interleave correctly. The
/// marker stays as the flag whose mere PRESENCE says "this is a value mixin,
/// not an allomorph" -- the one job it kept.
pub(crate) const ANON_ROLE_MARKER_PREFIX: &str = "__mutsu_anon_role__";

/// The anonymous roles recorded in `mixins`, as `(group, seq, display name)`.
fn anon_role_entries(
    mixins: &std::collections::HashMap<String, Value>,
) -> impl Iterator<Item = (i64, i64, String)> + '_ {
    mixins
        .keys()
        .filter_map(|k| k.strip_prefix(ANON_ROLE_MARKER_PREFIX))
        .map(|name| {
            let seq = role_application_seq(mixins, name);
            (
                role_application_group(mixins, name, seq),
                seq,
                crate::value::user_facing_type_name(name).into_owned(),
            )
        })
}

/// Whether `class_name` names one of the two symbol-table classes.
///
/// Raku splits a package symbol table (`Stash`) from a pseudo-package view of
/// a lexical pad (`PseudoStash`); they are siblings under `Map`, not parent
/// and child. mutsu represents both as an instance carrying a `symbols`
/// attribute and every stash code path treats them alike, so this is the
/// predicate those paths ask instead of naming one class. Which spelling
/// produces which class is decided by
/// `Interpreter::stash_class_for_package`.
pub(crate) fn is_stash_class_name(class_name: &str) -> bool {
    matches!(class_name, "Stash" | "PseudoStash")
}

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
                    Some(suffix) => format!("{name}{suffix}"),
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
                    Some(suffix) => format!("{base}{suffix}"),
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

/// The roles a `Mixin` value's `overrides` record, MOST-RECENTLY-APPLIED
/// FIRST — the order `.^roles` reports them in
/// (`((1 but A) but B).^roles` is `(B, A, Real, Numeric)` in raku, with the
/// base type's own roles after). Each entry is rendered by
/// [`role_mixin_suffix_entry`], so a parameterised role keeps its arguments
/// (`P[Int]`) and an anonymous one its `<anon|N>` spelling.
///
/// The application order is the `__mutsu_role_seq__{name}` stamp every
/// composition site sets (`roles.rs`), the same one
/// `receiver_class::mixin_chain` sorts the dispatch order by; the name is the
/// tie-break for a marker that carries no stamp. `base` skips the role whose
/// name equals it, for the punning case
/// ([`role_mixin_suffix_excluding`]'s argument of the same name); pass `""`
/// to exclude nothing.
pub(crate) fn mixin_roles_applied_last_first(
    mixins: &std::collections::HashMap<String, Value>,
    base: &str,
) -> Vec<String> {
    let mut entries: Vec<(i64, String)> = mixins
        .keys()
        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
        .filter(|n| *n != base)
        .map(|n| {
            let seq = mixins
                .get(&format!("__mutsu_role_seq__{n}"))
                .and_then(|v| match v.view() {
                    ValueView::Int(i) => Some(i),
                    _ => None,
                })
                .unwrap_or(i64::MIN);
            (seq, role_mixin_suffix_entry(mixins, n))
        })
        // `but`-mixing a plain value composes an anonymous role, recorded under
        // its own per-application marker rather than a `__mutsu_role__` entry
        // (see `Interpreter::apply_single_mixin`); raku lists those too, in the
        // same last-first order — `((1 but "x") but "y").^roles` starts
        // `<anon|8>, <anon|7>`.
        .chain(anon_role_entries(mixins).map(|(_, seq, name)| (seq, name)))
        .collect();
    entries.sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
    entries.into_iter().map(|(_, name)| name).collect()
}

/// [`role_mixin_suffix`], but skipping the role whose name equals `base` — the
/// role-punning case, where `R.new` builds `Mixin(Instance{R}, __mutsu_role__R)`
/// and raku reports plain `R` rather than `R+{R}`. Pass `""` to exclude nothing.
pub(crate) fn role_mixin_suffix_excluding(
    mixins: &std::collections::HashMap<String, Value>,
    base: &str,
) -> Option<String> {
    // APPLICATION order, not alphabetical: raku gives each composition its own
    // bracket and shows them in the order they were applied, which is the
    // property that distinguishes `(1 but A) but B` (`Int+{A}+{B}`) from
    // `(1 but B) but A` (`Int+{B}+{A}`). Sorting the names instead -- which is
    // what this did, for a stable name over a non-deterministic `HashMap`
    // iteration order -- rendered both as `Int+{A,B}` and threw the
    // distinction away. `__mutsu_role_seq__` is the same monotonic stamp
    // `mixin_roles_applied_last_first` and `mixin_identity_key` order by, so
    // the name is still deterministic.
    let mut entries: Vec<(i64, i64, String)> = mixins
        .keys()
        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
        .filter(|n| *n != base)
        .map(|n| {
            let seq = role_application_seq(mixins, n);
            (
                role_application_group(mixins, n, seq),
                seq,
                role_mixin_suffix_entry(mixins, n),
            )
        })
        // An anonymous role from `but <non-role>` takes its place in the same
        // order: `(1 but "x") but A` is `Int+{<anon|1>}+{A}`, which it could
        // not be while the anon was appended after every named role.
        .chain(anon_role_entries(mixins))
        .collect();
    entries.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then_with(|| a.1.cmp(&b.1))
            .then_with(|| a.2.cmp(&b.2))
    });
    // One bracket per APPLICATION, roles within it comma-joined in written
    // order: raku writes `1 but (R1, R2)` as `Int+{R1,R2}` and
    // `(1 but R1) but R2` as `Int+{R1}+{R2}`, and the two are different types.
    let mut names: Vec<String> = Vec::new();
    let mut last_group: Option<i64> = None;
    for (group, _, entry) in entries {
        match names.last_mut() {
            Some(last) if Some(group) == last_group => last.push_str(&format!(",{entry}")),
            _ => {
                names.push(entry);
                last_group = Some(group);
            }
        }
    }
    if names.is_empty() {
        return None;
    }
    Some(
        names
            .into_iter()
            .map(|n| format!("+{{{n}}}"))
            .collect::<Vec<_>>()
            .join(""),
    )
}

/// The id of the `but`/`does` application that composed `role_name`
/// (`__mutsu_role_group__{name}`), used to bracket the name by application.
/// Falls back to the role's own sequence stamp when absent, which puts it in a
/// group of its own -- the pre-stamp behaviour and the right answer for every
/// single-role application.
fn role_application_group(
    mixins: &std::collections::HashMap<String, Value>,
    role_name: &str,
    fallback: i64,
) -> i64 {
    mixins
        .get(&format!("__mutsu_role_group__{role_name}"))
        .and_then(|v| match v.view() {
            ValueView::Int(i) => Some(i),
            _ => None,
        })
        .unwrap_or(fallback)
}

/// The monotonic application-order stamp recorded for `role_name` at
/// composition time (`__mutsu_role_seq__{name}`), or `i64::MIN` when the marker
/// is absent (a value built before the stamp existed, or by a path that does
/// not record one) so such entries sort first and stay deterministic.
fn role_application_seq(mixins: &std::collections::HashMap<String, Value>, role_name: &str) -> i64 {
    mixins
        .get(&format!("__mutsu_role_seq__{role_name}"))
        .and_then(|v| match v.view() {
            ValueView::Int(i) => Some(i),
            _ => None,
        })
        .unwrap_or(i64::MIN)
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
    let mut parts: Vec<(i64, i64, String)> = mixins
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
            // The GROUP is part of the key, not just the order: `1 but
            // (R1, R2)` and `(1 but R1) but R2` compose the same two roles in
            // the same order but are different types in raku (`Int+{R1,R2}` vs
            // `Int+{R1}+{R2}`, `=:=` False). Its absolute value is not in the
            // key -- only the partition it induces, rendered as the group's
            // rank among this composition's groups.
            let seq = role_application_seq(mixins, role_name);
            (
                role_application_group(mixins, role_name, seq),
                seq,
                format!("{role_name}\u{0}{role_id}\u{0}{typeargs}"),
            )
        })
        .collect();
    // Ordered by the APPLICATION stamp, not sorted: composition order is part
    // of the composed type in raku -- `(1 but A) but B` and `(1 but B) but A`
    // have different `.WHAT`s there -- and sorting normalized that away, so the
    // two shared one cache node and `=:=` answered True. The stamp's VALUE
    // stays out of the key (two separately-built instances of the same
    // composition have different stamps and must still share a `.WHAT`; that
    // is the punned-role identity invariant `roast/S14-roles/instantiation.t`
    // pins, and is why an earlier attempt to include the stamp itself was
    // reverted -- see ADR-0060). Only the order it encodes is kept, exactly as
    // `mixin_identity_key` does for `===`.
    parts.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then_with(|| a.1.cmp(&b.1))
            .then_with(|| a.2.cmp(&b.2))
    });
    // Rank the groups 0, 1, 2, ... so the key records the PARTITION without
    // the ids' absolute values, which differ between two separately-built
    // instances of the same composition (the punned-role identity invariant).
    let mut rendered: Vec<String> = Vec::new();
    let mut last_group: Option<i64> = None;
    let mut rank = 0usize;
    for (group, _, part) in parts {
        if last_group.is_some_and(|g| g != group) {
            rank += 1;
        }
        last_group = Some(group);
        rendered.push(format!("{rank}\u{0}{part}"));
    }
    format!("{base_type_name}\u{1}{}", rendered.join("\u{1}"))
}

/// Build the identity key for a role-mixed value's `===` (`.WHICH`) —
/// [`crate::runtime::utils::values_identical`]'s `Mixin` arm.
///
/// `===` on two separately-built but identically-composed values is `True` in
/// raku (`(1 but A) === (1 but A)`), so the key must exclude everything that is
/// per-APPLICATION or per-INSTANCE, while still separating two genuinely
/// different compositions. Comparing the raw `overrides` maps — which is what
/// this replaced — could never answer `True`, because every application stamps
/// its own `__mutsu_role_seq__{name}` (see `roles.rs`).
///
/// Excluded:
/// * `__mutsu_role_seq__*` — the per-application-order stamp. Its ORDER
///   information is kept (the role list below is sorted by it), only its
///   absolute value is dropped. Order matters: raku's
///   `((1 but A) but C) === ((1 but C) but A)` is `False`.
/// * `__mutsu_attr__*` — per-instance role-attribute values. raku's
///   `(1 but R(2)) === (1 but R(3))` is `True`: `===` is `.WHICH` on the base
///   value plus the composed type, and both are `Int+{R}` holding 1.
///
/// Everything else is compared as-is, which is what keeps two compositions
/// apart that only the non-role part distinguishes: the allomorph `"Str"` key
/// (`<42> === IntStr.new(42, "forty-two")` is `False`) and
/// [`VALUE_MIXIN_MARKER`]'s fresh anonymous name per `but <non-role>`
/// application (`(1 but "x") === (1 but "x")` is `False`).
pub(crate) fn mixin_identity_key(mixins: &std::collections::HashMap<String, Value>) -> String {
    // Roles in application order (`__mutsu_role_seq__` ascending, name as the
    // tie-break for a marker that carries no stamp), each with the same
    // (name, role_id, typeargs) triple `mixin_composition_key` uses.
    let mut roles: Vec<(i64, i64, String)> = mixins
        .keys()
        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
        .map(|role_name| {
            let seq = mixins
                .get(&format!("__mutsu_role_seq__{role_name}"))
                .and_then(|v| match v.view() {
                    ValueView::Int(n) => Some(n),
                    _ => None,
                })
                .unwrap_or(i64::MIN);
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
            (
                role_application_group(mixins, role_name, seq),
                seq,
                format!("{role_name}\u{0}{role_id}\u{0}{typeargs}"),
            )
        })
        .collect();
    roles.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then_with(|| a.1.cmp(&b.1))
            .then_with(|| a.2.cmp(&b.2))
    });
    // The grouping is part of the composed type (`1 but (R1, R2)` is a
    // different type from `(1 but R1) but R2`), so it is in the key -- but as
    // the PARTITION it induces, ranked 0, 1, 2, ..., not as the raw ids, which
    // differ between two separately-built instances of the same composition.
    // That is the same treatment `mixin_composition_key` gives them, and the
    // reason `__mutsu_role_group__*` is excluded from `rest` below.
    let roles: Vec<(i64, String)> = {
        let mut out = Vec::with_capacity(roles.len());
        let mut last_group: Option<i64> = None;
        let mut rank = 0i64;
        for (group, seq, part) in roles {
            if last_group.is_some_and(|g| g != group) {
                rank += 1;
            }
            last_group = Some(group);
            out.push((seq, format!("{rank}\u{0}{part}")));
        }
        out
    };

    // Every remaining key/value pair, sorted (HashMap order is not stable).
    let mut rest: Vec<String> = mixins
        .iter()
        .filter(|(k, _)| {
            !k.starts_with("__mutsu_role__")
                && !k.starts_with("__mutsu_role_seq__")
                && !k.starts_with("__mutsu_role_id__")
                && !k.starts_with("__mutsu_role_typeargs__")
                && !k.starts_with("__mutsu_role_group__")
                && !k.starts_with("__mutsu_attr__")
        })
        .map(|(k, v)| format!("{k}\u{0}{}", v.to_string_value()))
        .collect();
    rest.sort_unstable();

    let roles: Vec<String> = roles.into_iter().map(|(_, part)| part).collect();
    format!("{}\u{2}{}", roles.join("\u{1}"), rest.join("\u{1}"))
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
                // The application-order stamps are part of the composition:
                // without them the shared node cannot render its own name in
                // order, so `((1 but B) but A).WHAT.^name` came out
                // `Int+{A}+{B}`. Their VALUES are not in the composition key
                // (see `mixin_composition_key`), so the node a key resolves to
                // simply keeps whichever instance created it -- and every
                // instance sharing that key applied its roles in the same
                // order, which is the only thing read back from them here.
                || k.starts_with("__mutsu_role_seq__")
                || k.starts_with("__mutsu_role_group__")
                // An anonymous `but <non-role>` role is part of the composed
                // type too, so the shared `.WHAT` node needs its markers to
                // render `Int+{<anon|1>}` for itself.
                || k.starts_with(ANON_ROLE_MARKER_PREFIX)
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
