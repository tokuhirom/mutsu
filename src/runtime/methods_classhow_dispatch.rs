use super::*;
use crate::symbol::Symbol;

/// The "no such method" answer of `Metamodel::MethodContainer`'s `.^lookup`
/// and `.^find_method`: Rakudo hands back the **`Mu` type object**, not `Nil`.
/// `Int.^lookup("does-not-exist")` gists as `(Mu)` and `.defined` is `False`,
/// so a caller's `//` / boolean test behaves the same either way -- but
/// `.^name`, `.raku` and an `=== Mu` identity check do not, which is what the
/// `Metamodel/MethodContainer.rakudoc` example asserts.
fn mop_absent_method() -> Value {
    Value::package(Symbol::intern("Mu"))
}

/// `.^add_method`/`.^add_multi_method`'s callable argument used to always be
/// a plain `Sub` (`X.^lookup('other')`'s old return shape). ADR-0019 Phase F
/// box F1 made `.^lookup`/`.^find_method` return a Method/Submethod
/// `Instance` instead (`todo/tickets/classhow-lookup-returns-sub-not-method-
/// instance.md`), so unwrap the `__mutsu_method_callable` attribute back to
/// that same `Sub` before the rest of this file's `ValueView::Sub` match --
/// any other value (a real closure literal, etc.) passes through unchanged.
fn unwrap_method_instance_callable(value: &Value) -> Value {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if matches!(class_name.as_str(), "Method" | "Submethod" | "Regex") => {
            let am = attributes.as_map();
            // A non-dispatcher candidate carries its callable directly. A
            // multi dispatcher has none of its own -- fall back to its first
            // candidate's callable as the carrier body, mirroring the old
            // Sub-shaped dispatcher (itself built from the first candidate
            // found); the lookup-tag rewrite below still marks the RESULT as
            // the dispatcher (no candidate_idx), not that specific candidate.
            let base_callable = am.get("__mutsu_method_callable").cloned().or_else(|| {
                am.get("candidates").and_then(|c| match c.view() {
                    ValueView::Array(items, _) => items.first().cloned(),
                    _ => None,
                })
            });
            let Some(base_callable) = base_callable else {
                return value.clone();
            };
            let callable = unwrap_method_instance_callable(&base_callable);
            // Port the Instance's `__mutsu_lookup_*` attributes back onto the
            // unwrapped Sub's `env` -- this function's callers (below) still
            // read them from `SubData::env` (predating ADR-0019 Phase F box
            // F1's move of that carrier data to Instance attributes) to
            // detect a multi-family alias (`^add_method(name,
            // X.^lookup('other'))` cloning the whole candidate family, not
            // just one carrier candidate). Cleared first, not merely
            // overwritten, since a dispatcher's own attrs carry no
            // `__mutsu_lookup_candidate_idx` but its fallback candidate's
            // unwrapped env still has one from its own unwrap above.
            if let ValueView::Sub(data) = callable.view() {
                let mut new_data: crate::value::SubData = (**data).clone();
                for key in [
                    "__mutsu_lookup_class",
                    "__mutsu_lookup_method",
                    "__mutsu_lookup_candidate_idx",
                ] {
                    new_data.env.remove(key);
                    if let Some(v) = am.get(key) {
                        new_data.env.insert(key.to_string(), v.clone());
                    }
                }
                return Value::sub_value(crate::gc::Gc::new(new_data));
            }
            callable
        }
        _ => value.clone(),
    }
}

/// `Metamodel::Naming.shortname`: the type name with every `Foo::` package
/// qualifier dropped, including inside `[...]` type args -- `Foo::Bar` ->
/// `Bar`, `R[M2::N]` -> `R[N]`. Non-identifier suffixes (`<anon|1>`,
/// `Int:D`, `+{Role}`) pass through unchanged.
fn shorten_type_name(name: &str) -> String {
    let is_ident = |c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '\'';
    let chars: Vec<char> = name.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == ':' && i + 2 < chars.len() && chars[i + 1] == ':' && is_ident(chars[i + 2]) {
            // Drop the qualifier segment just emitted along with the `::`.
            while out.chars().next_back().is_some_and(is_ident) {
                out.pop();
            }
            i += 2;
            continue;
        }
        out.push(chars[i]);
        i += 1;
    }
    out
}

/// The element type `.^array_type` reports for the type named `type_name`.
///
/// A parameterised container names its element type outright
/// (`Buf[uint64]` -> `uint64`, `array[num32]` -> `num32`,
/// `CArray[int32]` -> `int32`). An unparameterised byte-buffer type carries its
/// element width in its own name (`utf16` -> `uint16`), and a bare `Buf`/`Blob`
/// is `uint8`. Anything else is `Mu`, which is what Rakudo's `ClassHOW` answers
/// for a type that is not an array (`Str.^array_type` is `Mu`).
fn array_element_type_name(type_name: &str) -> &str {
    if let Some(inner) = type_name
        .split_once('[')
        .and_then(|(_, rest)| rest.strip_suffix(']'))
    {
        return inner;
    }
    match type_name {
        "Buf" | "Blob" | "buf8" | "blob8" | "utf8" | "utf8-c8" => "uint8",
        "buf16" | "blob16" | "utf16" => "uint16",
        "buf32" | "blob32" | "utf32" => "uint32",
        "buf64" | "blob64" => "uint64",
        _ => "Mu",
    }
}

impl Interpreter {
    /// Whether `value` is a genuine role reference: a role's own type object
    /// (`ValueView::Package` whose name is a role, not a class — including a
    /// role that happens to ALSO have been punned to a class of the same
    /// name, since a bareword role mention always stays `Package`, never the
    /// `Mixin` `Interpreter::punned_role_type_object` builds), a
    /// parameterised role (`ValueView::ParametricRole`), or one of
    /// `.^candidates`' own per-candidate `Instance` objects.
    ///
    /// `.^candidates` (and any other role-group-only MOP method) is only
    /// defined on `ParametricRoleGroupHOW`/`ParametricRoleHOW`/`CurriedRoleHOW`,
    /// never on `ClassHOW` — so a punned role's class (a `Mixin`) or an
    /// ordinary class (a `Package` whose name is not a role) must NOT match,
    /// and instead fall through to the `X::Method::NotFound` default at the
    /// bottom of `dispatch_classhow_method`, matching Rakudo
    /// (`R.^pun.^candidates` throws; `R.^candidates` answers `((R))`).
    fn is_role_reference_value(&self, value: &Value) -> bool {
        match value.view() {
            ValueView::Package(name) => self.is_role_type_name(&name.resolve()),
            ValueView::ParametricRole { .. } => true,
            ValueView::Instance { attributes, .. } => {
                attributes.as_map().contains_key("__mutsu_role_base_name")
            }
            _ => false,
        }
    }

    /// Resolve a nominalizable type name to its nominal base type
    /// (`^nominalize`): strip `:D`/`:U`/`:_` definiteness, unwrap a coercion
    /// type (`Int(Rat)` -> `Int`), and walk a subset chain to the first
    /// non-subset base. Plain nominal types return themselves.
    pub(crate) fn nominalize_type_name(&self, name: &str) -> String {
        let mut current = name.to_string();
        loop {
            let stripped = current
                .strip_suffix(":D")
                .or_else(|| current.strip_suffix(":U"))
                .or_else(|| current.strip_suffix(":_"));
            if let Some(s) = stripped {
                current = s.to_string();
                continue;
            }
            if crate::runtime::types::is_coercion_constraint(&current)
                && let Some((target, _)) = crate::runtime::types::parse_coercion_type(&current)
            {
                current = target.to_string();
                continue;
            }
            if let Some(subset) = self.registry().subsets.get(&current) {
                let base = subset.base.clone();
                if !base.is_empty() && base != current {
                    current = base;
                    continue;
                }
            }
            return current;
        }
    }

    pub(super) fn dispatch_classhow_method(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "mixin" if args.len() >= 2 => {
                // `.^mixin(Role)` on a slang-activation handle (ADR-0026):
                // record the composition without composing anything.
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = args[0].view()
                    && class_name.resolve().starts_with("Mutsu::Slang::")
                {
                    return Ok(Self::slang_handle_mixin(
                        &class_name.resolve(),
                        &attributes.as_map(),
                        &args[1..],
                    ));
                }
                // Generic `.^mixin(R)`: same composition as infix `but`
                // (`Str.^mixin(R)` is the `Str+{R}` mixin type object). When
                // `R` is an actual role, route through the same role
                // composition `but`/`does` use (`eval_does_values`) rather
                // than `apply_but_mixin`'s generic by-type-name keying —
                // otherwise the mixin map is keyed by the bare role name
                // instead of the `__mutsu_role__<name>` marker every other
                // role-aware consumer (`.can`, `.^can`, `nqp::can`, `.does`)
                // expects, and (for a routine invocant) the composition is
                // never recorded for `materialize_routine_mixins` to restore
                // on a later rebuild (see
                // news/2026-08/test-assertion-trait-is-not-introspectable.md).
                let mut result = args[0].clone();
                for role in &args[1..] {
                    result = if self.is_role_application(role) {
                        self.eval_does_values(result, role.clone())?
                    } else {
                        Self::apply_but_mixin(result, role.clone())?
                    };
                }
                Ok(result)
            }
            "set_name" if args.len() == 2 => {
                // `$type.^set_name($name)` renames a metaobject (Rakudo's ClassHOW
                // method). It is most often applied to a freshly-composed
                // anonymous type, e.g. `Foo.new but role {...}`, to give it a
                // human-readable name for display. Persist the name so a later
                // `.^name` returns it.
                let new_name = args[1].to_string_value();
                match args[0].view() {
                    ValueView::Mixin(inner, mixins) => {
                        // Resolve to the composition-keyed shared node
                        // (ADR-0060) — the SAME node whether `args[0]` came
                        // from `.WHAT` (`Hash::Restricted`'s
                        // `v.var.WHAT.^set_name(...)`) or is the mixed-in
                        // instance itself (`$obj.^set_name(...)`,
                        // `t/metamodel-set-name.t`'s `Foo.new but role {...}`
                        // scenario) — so a later `.^name` on ANY value with
                        // this exact composition, including one constructed
                        // after this call, observes the rename.
                        let overrides = self.mixin_instance_composition_overrides(inner, mixins)?;
                        // SAFETY: aliased in-place mutation of a shared container
                        // (see `gc_contents_mut`); no borrow into the map is live
                        // across the insert, and the insert does not re-enter the VM.
                        let map = unsafe { crate::gc::gc_contents_mut(&overrides) };
                        map.insert(
                            "__mutsu_type_name__".to_string(),
                            Value::str(new_name.clone()),
                        );
                    }
                    ValueView::Package(name) => {
                        let resolved = name.resolve();
                        // A builtin type's `Package` value (e.g. `Hash`, `Array`) is
                        // the SAME shared value for every variable of that type —
                        // it is not a fresh per-instance metaobject. Renaming it
                        // therefore renames the type process-wide for every value
                        // of it, not just the caller's — which matches real
                        // Rakudo: `Hash.^set_name("X"); say Hash.^name` reports
                        // "X" there too (verified against `raku`). A role-mixed
                        // native value's `.WHAT` (the `ValueView::Mixin` arm
                        // above) is what gives `Hash::Restricted` a distinct
                        // per-composition anonymous type object to rename
                        // instead, when the caller wants a scoped rename rather
                        // than a global one.
                        self.type_metadata
                            .entry(resolved)
                            .or_default()
                            .insert("__set_name__".to_string(), Value::str(new_name.clone()));
                    }
                    ValueView::Instance { class_name, .. } => {
                        self.type_metadata
                            .entry(class_name.resolve())
                            .or_default()
                            .insert("__set_name__".to_string(), Value::str(new_name.clone()));
                    }
                    _ => {}
                }
                Ok(Value::str(new_name))
            }
            // `Metamodel::Versioning`'s write side. `.^set_ver`/`.^set_auth`/
            // `.^set_api` are the runtime equivalents of the declarative
            // `class C:ver<1.0>:auth<foo>:api<2>` adverbs, and Rakudo stores
            // both in the same slot -- so they land in the very
            // `type_metadata` entry the `:ver(...)` adverb writes and the
            // `"ver"`/`"auth"`/`"api"` readers below already consult. They
            // stay callable after `.^compose` (Rakudo imposes no
            // post-composition lock on metadata), which is what makes the
            // documented `BEGIN { C.^set_ver: v0.0.1 }` idiom work.
            "set_ver" | "set_auth" | "set_api" if args.len() == 2 => {
                let key = method.trim_start_matches("set_").to_string();
                let name = self.mop_receiver_owner(&args[0]);
                let stored = if key == "ver" {
                    Self::version_from_value(args[1].clone())
                } else {
                    Value::str(args[1].to_string_value())
                };
                self.type_metadata
                    .entry(name)
                    .or_default()
                    .insert(key, stored.clone());
                Ok(stored)
            }
            // `Metamodel::Documenting`: `.^set_why` attaches a pod object to
            // the METACLASS, so unlike an attribute write it is not blocked
            // once the type is composed. `.WHY` reads it back, both on the
            // HOW (`Documented.HOW.WHY`) and on the type object itself
            // (`Documented.WHY`, via `dispatch_why`).
            "set_why" if args.len() == 2 => {
                let name = self.mop_receiver_owner(&args[0]);
                self.type_metadata
                    .entry(name)
                    .or_default()
                    .insert("__set_why__".to_string(), args[1].clone());
                Ok(args[1].clone())
            }
            "WHY" if args.len() == 1 => {
                let name = self.mop_receiver_owner(&args[0]);
                if let Some(why) = self
                    .type_metadata
                    .get(&name)
                    .and_then(|m| m.get("__set_why__"))
                {
                    return Ok(why.clone());
                }
                let target = args[0].clone();
                self.dispatch_why(&target)
            }
            // `Metamodel::Trusting`: the list of types this class declared
            // `trusts` on, in declaration order. Rakudo answers with a `List`
            // of type objects (empty for a class with no `trusts`), and only
            // `ClassHOW` has the method at all -- a role's
            // `ParametricRoleGroupHOW` throws `X::Method::NotFound`, which is
            // what the `is_classhow_method` gate plus this arm's registry
            // check reproduce.
            "trusts" if args.len() == 1 => {
                // Rakudo composes `Metamodel::Trusting` into `ClassHOW` only
                // (and therefore into `GrammarHOW`, which subclasses it):
                // `module M {}; M.^trusts`, `enum E <a b>; E.^trusts` and
                // `subset S of Int; S.^trusts` all throw `X::Method::NotFound`
                // while `Int.^trusts` and `G.^trusts` answer `()`. Ask the
                // metaobject itself rather than re-deriving the taxonomy here,
                // so a new HOW kind cannot silently gain the method.
                let how = self.dispatch_how(&args[0], &[])?;
                let how_is_class_like = matches!(
                    how.view(),
                    ValueView::Instance { class_name, .. }
                        if matches!(
                            class_name.as_str(),
                            "Perl6::Metamodel::ClassHOW" | "Perl6::Metamodel::GrammarHOW"
                        )
                );
                if !how_is_class_like {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): trusts",
                    ));
                }
                let name = self.mop_receiver_owner(&args[0]);
                let trusted = self
                    .registry()
                    .class_trusts
                    .get(&name)
                    .cloned()
                    .unwrap_or_default();
                let types = trusted
                    .iter()
                    .map(|t| {
                        let canonical = self.resolve_private_class_name(&name, t);
                        Value::package(Symbol::intern(&canonical))
                    })
                    .collect::<Vec<_>>();
                Ok(Value::array(types))
            }
            "name" if args.len() == 1 => {
                if let ValueView::Mixin(inner, mixins) = args[0].view() {
                    // Same composition-keyed shared node as `set_name`
                    // (ADR-0060): a rename made via `.WHAT.^set_name(...)`
                    // or directly on the instance is visible here either way.
                    let overrides = self.mixin_instance_composition_overrides(inner, mixins)?;
                    let name = match overrides.get("__mutsu_type_name__") {
                        Some(renamed) => renamed.to_string_value(),
                        // A role-mixed value (`5 but Foo::Bar`, `$x does R`) reports
                        // its base type with a `+{Role,...}` suffix, e.g.
                        // `Int+{Foo::Bar}`. `what_type_name` builds this from the
                        // recorded role keys; `value_type_name` (a `&'static str`)
                        // cannot.
                        None => crate::value::what_type_name(&args[0]),
                    };
                    return Ok(Value::str(name));
                }
                let name = match args[0].view() {
                    ValueView::Package(name) => self
                        .type_metadata
                        .get(&name.resolve())
                        .and_then(|m| m.get("__set_name__"))
                        .map(Value::to_string_value)
                        .unwrap_or_else(|| {
                            crate::value::user_facing_type_name(&name.resolve()).to_string()
                        }),
                    ValueView::Instance { class_name, .. } => self
                        .type_metadata
                        .get(&class_name.resolve())
                        .and_then(|m| m.get("__set_name__"))
                        .map(Value::to_string_value)
                        .unwrap_or_else(|| {
                            crate::value::user_facing_type_name(&class_name.resolve()).to_string()
                        }),
                    ValueView::ParametricRole {
                        base_name,
                        type_args,
                    } => crate::value::parametric_role_name(&base_name.resolve(), type_args),
                    // A concrete builtin value (`5`, `"x"`, `%h`, ...): honor
                    // a process-wide rename of its type via
                    // `Hash.^set_name(...)` etc. — see
                    // `Interpreter::builtin_display_name`, the same helper
                    // `dispatch_caret_name`'s equivalent fallback uses.
                    _ => {
                        let owner = self.dispatch_owner_name(&args[0]);
                        self.builtin_display_name(owner)
                    }
                };
                Ok(Value::str(name))
            }
            "array_type" if !args.is_empty() => {
                // The element type of a native array-ish container. Derived from
                // the same name `.^name` reports — `dispatch_caret_name`, which
                // is where a `CArray[int32]` / `array[uint8]` gets its
                // parameterised spelling from the container metadata — so the
                // two can never disagree. `NativeHelpers::Blob` asks every
                // container it is handed for this and feeds the answer to
                // `nativesizeof` and `nativecast(Pointer[T], …)`.
                let name = self.dispatch_caret_name(&args[0])?.to_string_value();
                Ok(Value::package(crate::symbol::Symbol::intern(
                    array_element_type_name(&name),
                )))
            }
            "shortname" if !args.is_empty() => {
                let full = self
                    .dispatch_classhow_method("name", vec![args[0].clone()])?
                    .to_string_value();
                Ok(Value::str(shorten_type_name(&full)))
            }
            "ver" if args.len() == 1 => {
                let name = self.mop_receiver_owner(&args[0]);
                if let Some(meta) = self.type_metadata.get(&name)
                    && let Some(value) = meta.get("ver").cloned()
                {
                    return Ok(Self::version_from_value(value));
                }
                // Core-setting language versions surface as plain Strs
                // (`Int.^ver.WHAT` is Str in Rakudo); only a declared
                // `:ver(...)` adverb (the type_metadata path above) is a
                // real Version object.
                if let Some(subset) = self.registry().subsets.get(&name) {
                    return Ok(Value::str(subset.version.clone()));
                }
                if name == "Grammar" {
                    return Ok(Value::str_from("6.e"));
                }
                // A bare `package` uses PackageHOW, which has no `.^ver` at all, so
                // `P.^ver` must still throw X::Method::NotFound ("absent by design").
                if matches!(
                    self.registry().package_kinds.get(&name),
                    Some(crate::ast::PackageKind::Package)
                ) {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                    ));
                }
                // Core setting types report the language version they were
                // declared in (`Int.^ver` is v6.c). Checked before the class
                // registry so an add_method stub for a builtin doesn't turn
                // this into Mu.
                if Self::is_builtin_type(&name) {
                    return Ok(Value::str_from("6.c"));
                }
                // A class/module/role/enum with no declared version: `.^ver` is
                // `Mu` (an undefined type object), not an error -- matching
                // Rakudo. Reached e.g. when the `:ver(...)` adverb is an
                // expression mutsu does not evaluate at registration
                // (`unit class C:ver($?DISTRIBUTION.meta<ver>)`), or a plain
                // unversioned declaration.
                // TODO: evaluate expression-form `:ver(...)` adverbs at class
                // registration and store the result in type_metadata.
                if self.registry().classes.contains_key(&name)
                    || self.registry().roles.contains_key(&name)
                    || self.registry().enum_types.contains_key(&name)
                    || self.registry().package_kinds.contains_key(&name)
                {
                    return Ok(Value::package(crate::symbol::Symbol::intern("Mu")));
                }
                Err(RuntimeError::new(
                    "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                ))
            }
            "auth" if args.len() == 1 => {
                let name = self.mop_receiver_owner(&args[0]);
                // A bare `package` uses PackageHOW, which has no `.^auth`.
                if matches!(
                    self.registry().package_kinds.get(&name),
                    Some(crate::ast::PackageKind::Package)
                ) {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                    ));
                }
                // A type with no declared `:auth` has an empty-string auth
                // (`class C {}; C.^auth` eq ""), so default to "" rather than
                // throwing -- same shape as `.^api` below.
                if let Some(value) = self
                    .type_metadata
                    .get(&name)
                    .and_then(|meta| meta.get("auth").cloned())
                {
                    return Ok(Value::str(value.to_string_value()));
                }
                Ok(Value::str(String::new()))
            }
            "api" if args.len() == 1 => {
                let name = self.mop_receiver_owner(&args[0]);
                // A bare `package` uses PackageHOW, which has no `.^api`.
                if matches!(
                    self.registry().package_kinds.get(&name),
                    Some(crate::ast::PackageKind::Package)
                ) {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): api",
                    ));
                }
                // A declared `:api(...)` is stored in type_metadata; a type with no
                // `:api` has an empty-string api in Rakudo (`class C {}; C.^api` eq
                // ""), so default to "" rather than throwing.
                if let Some(value) = self
                    .type_metadata
                    .get(&name)
                    .and_then(|meta| meta.get("api").cloned())
                {
                    return Ok(Value::str(value.to_string_value()));
                }
                Ok(Value::str(String::new()))
            }
            "isa" if args.len() == 2 => {
                // `.^isa` answers with an Int 1/0 (Rakudo surfaces the nqp
                // boolean directly), not a Bool.
                // Allow calling .^isa on an instance: use the instance's class.
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    ValueView::RakuAst(node) => node.class.printed_name().to_string(),
                    _ => return Ok(Value::int(0)),
                };
                let other_name = match args[1].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    ValueView::RakuAst(node) => node.class.printed_name().to_string(),
                    _ => return Ok(Value::int(0)),
                };
                let is_same = class_name == other_name;
                if is_same {
                    return Ok(Value::int(1));
                }
                let class_resolved = class_name;
                let other_resolved = other_name;
                if class_resolved.starts_with("RakuAST::")
                    && other_resolved.starts_with("RakuAST::")
                {
                    return Ok(Value::int(crate::rakuast::type_object_isa(
                        &class_resolved,
                        &other_resolved,
                    ) as i64));
                }
                // Clone the base out per step so the registry read guard never spans
                // iterations (recursive read locks may deadlock).
                if let Some(mut base) = self
                    .registry()
                    .subsets
                    .get(&class_resolved)
                    .map(|s| s.base.clone())
                {
                    loop {
                        if base == other_resolved {
                            return Ok(Value::int(1));
                        }
                        let Some(parent_base) =
                            self.registry().subsets.get(&base).map(|s| s.base.clone())
                        else {
                            break;
                        };
                        if parent_base == base {
                            break;
                        }
                        base = parent_base;
                    }
                }
                let mro = self.class_mro(&class_resolved);
                Ok(Value::int(
                    mro.iter().any(|p| p.as_str() == other_resolved) as i64
                ))
            }
            "mro" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg.view() {
                        ValueView::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        ValueView::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    Ok(Value::array(mro))
                } else {
                    let mro = self.classhow_mro_names(&args[0]);
                    let mut values = self.mro_names_to_values(mro)?;
                    // The head of an MRO is the invocant's own type object
                    // (`C.^mro[0] === C`, `$o.^mro[0] === $o.WHAT`). Naming it
                    // by the class name is only equivalent while the name has
                    // exactly one type object behind it — it does not for a
                    // role that has been punned (`R.new`: the name `R` is the
                    // role *group*, while the instance's type is the punned
                    // class) nor for a role-mixed value (`(1 but R)`, whose
                    // type is `Int+{R}`, not `Int`). Take the type object from
                    // `.WHAT`, which already answers all three correctly.
                    if !matches!(args[0].view(), ValueView::Package(_))
                        && let Some(head) = values.first_mut()
                    {
                        *head = self.dispatch_what(&args[0], Vec::new())?;
                    }
                    Ok(Value::array(values))
                }
            }
            // `Metamodel::TypePretense`: the chain a role type object pretends
            // to belong to. Rakudo mixes it into the three role metaclasses
            // only, so a `ClassHOW`/`EnumHOW`/`SubsetHOW` receiver must keep
            // throwing X::Method::NotFound. Ask the metaobject itself (the same
            // shape the `trusts` arm above uses) rather than re-deriving the
            // taxonomy, so a new HOW kind cannot silently gain the method.
            "pretending_to_be" if args.len() == 1 => {
                // The receiver may be the role group (`R`), an individual
                // candidate's declaration-site key, or a curried role
                // (`R[Int]`, which arrives as a `ParametricRole` rather than a
                // `Package`). All three carry TypePretense; a class/enum/subset
                // does not.
                let type_name = match args[0].view() {
                    ValueView::ParametricRole { base_name, .. } => base_name.resolve(),
                    _ => self.mop_receiver_owner(&args[0]),
                };
                let base = type_name
                    .split_once('[')
                    .map_or(type_name.as_str(), |(base, _)| base);
                if !self.is_role_type_name(base) {
                    let how = self.dispatch_how(&args[0], &[])?;
                    let how_name = match how.view() {
                        ValueView::Instance { class_name, .. } => class_name.resolve(),
                        _ => "Mu".to_string(),
                    };
                    return Err(RuntimeError::method_not_found(
                        "pretending_to_be",
                        &how_name,
                    ));
                }
                Ok(Value::array(
                    crate::runtime::types::ROLE_PRETENDS_TO_BE
                        .iter()
                        .map(|n| Value::package(Symbol::intern(n)))
                        .collect(),
                ))
            }
            "archetypes" if !args.is_empty() => {
                let invocant_name = self.mop_receiver_owner(&args[0]);
                let base_name = invocant_name
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(invocant_name.as_str());
                let is_role = self.registry().roles.contains_key(base_name);
                let is_subset = self.registry().subsets.contains_key(base_name);
                // A coercion type (`Str(Int)`) carries parens in its name. Use
                // the strict form check — a bare `contains('(')` also fires on
                // parens embedded in a where-clause of a `T{K}` key-typed hash
                // (`Associative[Str{subset ... where any("a", "b")}]`).
                let is_coercive = crate::runtime::types::is_coercion_constraint(&invocant_name);
                // A definite type (`Int:D` / `Int:U`) wraps its base type
                // (rakudo: nominal=False, nominalizable=True, definite=True).
                let is_definite = invocant_name.ends_with(":D") || invocant_name.ends_with(":U");
                let mut attrs = HashMap::new();
                attrs.insert("composable".to_string(), Value::truth(is_role));
                // Classes, enums, and roles are nominal; subsets, coercion
                // types, and definite types are not (JSON::Unmarshal's
                // ClassLike subset — rakudo reports roles as nominal too).
                attrs.insert(
                    "nominal".to_string(),
                    Value::truth(!is_subset && !is_coercive && !is_definite),
                );
                // Subsets, coercion types, and definite types can be
                // nominalized (^nominalize).
                attrs.insert(
                    "nominalizable".to_string(),
                    Value::truth(is_subset || is_coercive || is_definite),
                );
                attrs.insert("coercive".to_string(), Value::truth(is_coercive));
                attrs.insert("definite".to_string(), Value::truth(is_definite));
                Ok(Value::make_instance(
                    Symbol::intern("Perl6::Metamodel::Archetypes"),
                    attrs,
                ))
            }
            "nominalize" if !args.is_empty() => {
                let invocant_name = self.mop_receiver_owner(&args[0]);
                let nominal = self.nominalize_type_name(&invocant_name);
                Ok(Value::package(Symbol::intern(&nominal)))
            }
            "mro_unhidden" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg.view() {
                        ValueView::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        ValueView::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    let filtered = self.filter_mro_unhidden(&args[0], mro);
                    Ok(Value::array(filtered))
                } else {
                    let mro = self.classhow_mro_unhidden_names(&args[0]);
                    Ok(Value::array(self.mro_names_to_values(mro)?))
                }
            }
            "can" if args.len() >= 2 => {
                let invocant = &args[args.len() - 2];
                // The method name is always the last argument. When called via ^can,
                // the args may be [Package, target, method_name] due to Package insertion.
                let method_name = args.last().unwrap().to_string_value();
                let results = self.collect_can_methods(invocant, &method_name);
                Ok(Value::array(results))
            }
            "does" if args.len() >= 2 => {
                let invocant = &args[args.len() - 2];
                let role_arg = args.last().unwrap();
                // Handle ParametricRole directly to compare type args properly
                if let ValueView::ParametricRole {
                    base_name,
                    type_args,
                } = role_arg.view()
                {
                    let base = base_name.resolve();
                    if let ValueView::Mixin(_, mixins) = invocant.view() {
                        let key = format!("__mutsu_role_typeargs__{}", base);
                        let has_role = invocant.does_check(&base);
                        let args_match = if let Some(ValueView::Array(actual_args, ..)) =
                            mixins.get(&key).map(Value::view)
                        {
                            actual_args.len() == type_args.len()
                                && actual_args
                                    .iter()
                                    .zip(type_args.iter())
                                    .all(|(a, e)| self.parametric_arg_subtypes(a, e))
                        } else {
                            type_args.is_empty()
                        };
                        return Ok(Value::truth(has_role && args_match));
                    }
                    return Ok(Value::truth(self.type_matches_value(
                        &format!(
                                "{}[{}]",
                                base,
                                type_args
                                    .iter()
                                    .map(|a| a.to_string_value())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        invocant,
                    )));
                }
                let type_name = match role_arg.view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => role_arg.to_string_value(),
                };
                Ok(Value::truth(self.type_matches_value(&type_name, invocant)))
            }
            "lookup" if args.len() >= 2 => {
                let invocant = &args[0];
                // Method name is always the last argument; when ^lookup is called on
                // a concrete value the Package is prepended and the original value
                // sits in between.
                let method_name = args.last().unwrap().to_string_value();
                Ok(self
                    .classhow_lookup(invocant, &method_name)
                    .unwrap_or_else(mop_absent_method))
            }
            "find_method" if args.len() >= 2 => {
                let invocant = &args[0];
                // The method name is the last *positional* argument: calling
                // `$obj.^find_method('v')` on a concrete value prepends the Package and
                // leaves the instance in between (so `args[1]` is not the name), while
                // `.^find_method('foo', :no_fallback)` trails an adverb after it.
                let Some(name_arg) = args
                    .iter()
                    .rev()
                    .find(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                else {
                    return Ok(mop_absent_method());
                };
                let method_name = name_arg.to_string_value();
                Ok(self
                    .classhow_find_method(invocant, &method_name)
                    .unwrap_or_else(mop_absent_method))
            }
            "parameterize" if args.len() >= 2 => {
                // `$type.^parameterize($T, ...)` — the metamodel form of the
                // `Type[$T]` postcircumfix parameterization. Build the same
                // `Base[Arg,...]` package name that `vm_var_index_ops.rs` produces
                // for the `[ ]` syntax, so `Set.^parameterize(Str)` and `Set[Str]`
                // yield an identical parameterized type object.
                // Parameterizing is always relative to the *generic base*, not
                // to a previously-curried spelling.  In particular, the MOP
                // permits a caller to reuse one `$type` lexical for successive
                // parameterizations (`Set[Str].^parameterize(Int())` means
                // `Set[Int()]`, not the nonexistent `Set[Str][Int()]`).
                let owner = self.mop_receiver_owner(&args[0]);
                let base = owner
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(owner.as_str());
                let param_args = args[1..]
                    .iter()
                    .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                    .map(|v| match v.view() {
                        ValueView::Package(name) => name.resolve(),
                        _ => {
                            let s = v.to_string_value();
                            s.trim_start_matches('(').trim_end_matches(')').to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Ok(Value::package(Symbol::intern(&format!(
                    "{}[{}]",
                    base, param_args
                ))))
            }
            "coerce" if args.len() >= 2 => {
                let target_constraint = self.mop_receiver_owner(&args[0]);
                let original = args[1].clone();
                let parse_coercion = |constraint: &str| -> Option<(String, Option<String>)> {
                    if !constraint.ends_with(')') || constraint.contains('[') {
                        return None;
                    }
                    let open = constraint.find('(')?;
                    if open == 0 {
                        return None;
                    }
                    let target = constraint[..open].to_string();
                    let source = &constraint[open + 1..constraint.len() - 1];
                    let source = if source.is_empty() {
                        None
                    } else {
                        Some(source.to_string())
                    };
                    Some((target, source))
                };
                if let Some((_target, source)) = parse_coercion(&target_constraint)
                    && let Some(src) = source.as_ref()
                    && !self.type_matches_value(src, &original)
                {
                    return Err(super::types::coerce_impossible_error(
                        &target_constraint,
                        &original,
                    ));
                }
                let coerced =
                    self.try_coerce_value_for_constraint(&target_constraint, original.clone())?;
                if let Some((target, _)) = parse_coercion(&target_constraint)
                    && !self.type_matches_value(&target, &coerced)
                {
                    return Err(super::types::coerce_impossible_error(
                        &target_constraint,
                        &original,
                    ));
                }
                Ok(coerced)
            }
            "add_method" if args.len() >= 3 => {
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => {
                        return Err(RuntimeError::new("add_method target must be a type object"));
                    }
                };
                // A **qualified spelling of an already-registered class** adds to
                // that class, not to a fresh stub under the long name.
                // `NativeHelpers::Pointer` adds pointer arithmetic with
                // `NativeCall::Types::Pointer.^add_method('add', …)`, while the
                // prelude registers `Pointer` under its short name and tags every
                // handle with it — so the stub was created, populated, and never
                // consulted, leaving `.add` "no such method" and `.succ`/`.pred`
                // falling through to the numeric successor.
                let class_name = match class_name.rsplit("::").next() {
                    Some(short)
                        if short != class_name
                            && !self.registry().classes.contains_key(&class_name)
                            && self.registry().classes.contains_key(short) =>
                    {
                        short.to_string()
                    }
                    _ => class_name,
                };
                let method_name = args[1].to_string_value();
                let method_value = unwrap_method_instance_callable(&args[2]);
                let ValueView::Sub(sub_data) = method_value.view() else {
                    return Ok(Value::NIL);
                };
                // `^find_method` on a *multi* method family returns its first
                // candidate as a carrier Sub tagged with `__mutsu_lookup_class`
                // / `__mutsu_lookup_method` and no candidate index. Registering
                // just that carrier would freeze the alias to one signature
                // (Text::CSV's BEGIN-time `alias` helper maps `column-names`
                // onto the four-candidate `column_names` multi). Clone the
                // whole candidate family for the new name instead.
                let multi_family: Option<Vec<MethodDef>> = (|| {
                    if sub_data.env.get("__mutsu_lookup_candidate_idx").is_some() {
                        return None;
                    }
                    let ValueView::Str(src_class) =
                        sub_data.env.get("__mutsu_lookup_class").map(Value::view)?
                    else {
                        return None;
                    };
                    let ValueView::Str(src_method) =
                        sub_data.env.get("__mutsu_lookup_method").map(Value::view)?
                    else {
                        return None;
                    };
                    // ADR-0019 F4a: `src_class` can name a role directly
                    // (`R.^find_method('m')` with `R` never `.new`-punned or
                    // `does`-composed anywhere), which has no row in the
                    // canonical method table -- the role fallback is required
                    // here, not optional, confirmed against real Rakudo (a
                    // role-owned multi aliased this way keeps every
                    // candidate, not just the carrier's own signature).
                    self.registry()
                        .get_method_overloads_with_role_fallback(
                            src_class.as_ref(),
                            src_method.as_ref(),
                        )
                        .filter(|defs| defs.iter().any(|d| d.is_multi))
                })();
                // Filter out invocant params from param_defs since MethodDef
                // stores only the user-visible parameters (the invocant is
                // added implicitly during dispatch).
                let filtered_param_defs: Vec<ParamDef> = sub_data
                    .param_defs
                    .iter()
                    .filter(|pd| !pd.is_invocant)
                    .cloned()
                    .collect();
                // A NAMED invocant other than `self` (`anon method (Mu \SELF:
                // |) {...}` — OO::Monitors' POPULATE hook) is dropped from the
                // params like any invocant, but the body refers to it by name,
                // so prepend a `SELF := self` binding and let the dispatch
                // recompile the adjusted body on demand.
                let named_invocant: Option<String> = sub_data
                    .param_defs
                    .iter()
                    .find(|pd| pd.is_invocant)
                    .map(|pd| pd.name.trim_start_matches(['$', '\\']).to_string())
                    .filter(|n| !n.is_empty() && n != "self");
                let (method_body, method_compiled) = match named_invocant {
                    Some(inv_name) => {
                        let mut body = vec![
                            crate::ast::Stmt::VarDecl {
                                name: inv_name.clone(),
                                expr: crate::ast::Expr::BareWord("self".to_string()),
                                type_constraint: None,
                                is_state: false,
                                is_our: false,
                                is_dynamic: false,
                                is_export: false,
                                export_tags: Vec::new(),
                                custom_traits: Vec::new(),
                                where_constraint: None,
                            },
                            crate::ast::Stmt::MarkSigillessReadonly(inv_name),
                        ];
                        body.extend(sub_data.body.iter().cloned());
                        (std::sync::Arc::new(body), None)
                    }
                    None => (sub_data.body.clone(), sub_data.compiled_code.clone()),
                };
                // The name list has to lose the invocant too, not just
                // `param_defs`: dispatch binds arguments positionally against
                // `params`, so leaving `self` in it shifted every argument by one
                // and left the last parameter undeclared — `method (Pointer:D:
                // Int $off) { … $off … }` died with "Variable 'off' is not
                // declared" (`NativeHelpers::Pointer`'s `add`).
                let invocant_names: HashSet<&str> = sub_data
                    .param_defs
                    .iter()
                    .filter(|pd| pd.is_invocant)
                    .map(|pd| pd.name.as_str())
                    .collect();
                let filtered_params: Vec<String> = sub_data
                    .params
                    .iter()
                    .filter(|p| {
                        !invocant_names.contains(p.trim_start_matches(['$', '@', '%', '&']))
                    })
                    .cloned()
                    .collect();
                let def = MethodDef {
                    lexical_package: sub_data.package.resolve(),
                    params: filtered_params,
                    param_defs: filtered_param_defs,
                    body: method_body,
                    is_rw: sub_data.is_rw,
                    is_private: false,
                    is_multi: false,
                    is_my: false,
                    role_origin: None,
                    original_role: None,
                    return_type: None,
                    compiled_code: method_compiled,
                    compiled_fns: None,
                    delegation: None,
                    is_default: false,
                    deprecated_message: None,
                    is_submethod: false,
                    // Preserve the closure literal's captured scope so a method
                    // like `method { attr.get_value(self) }` (Attribute::Predicate's
                    // `is predicate`) can still resolve `attr` after its creating
                    // sub returns. Only carried when the env actually holds captures.
                    captured_env: if sub_data.env.is_empty() {
                        None
                    } else {
                        Some(sub_data.env.clone())
                    },
                    source_file: sub_data.source_file.clone(),
                    role_param_bindings: None,
                };
                // If the class doesn't exist yet (e.g. built-in types like Rat, Int, Str),
                // create a stub ClassDef so methods can be added dynamically.
                if !self.registry().classes.contains_key(&class_name) {
                    self.registry_mut().classes.insert(
                        class_name.clone(),
                        ClassDef {
                            parents: vec![],
                            attributes: vec![],
                            attribute_types: HashMap::new(),
                            attribute_smileys: HashMap::new(),
                            attribute_built: HashMap::new(),
                            alias_attributes: HashSet::new(),
                            native_methods: HashSet::new(),
                            mro: sym_mro(&[&class_name]),
                            wildcard_handles: vec![],
                            class_level_attrs: HashMap::new(),
                        },
                    );
                }
                let defs = multi_family.unwrap_or_else(|| vec![def]);
                self.registry_mut().set_user_methods(
                    Symbol::intern(&class_name),
                    Symbol::intern(&method_name),
                    defs,
                );
                // Class shape changed (an added BUILD/TWEAK/new flips ctor
                // eligibility) — drop cached construction plans.
                self.native_ctor_plan_cache.clear();
                // Return Nil even if the class was not found (e.g. built-in types
                // like Rat that are not in the user-defined class registry).
                // Raku's add_method returns the method name; returning Nil is
                // sufficient for eval-lives-ok tests.
                Ok(Value::NIL)
            }
            "add_multi_method" if args.len() >= 3 => {
                // Same as add_method but marks the method as multi
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => {
                        return Err(RuntimeError::new(
                            "add_multi_method target must be a type object",
                        ));
                    }
                };
                let method_name = args[1].to_string_value();
                let method_value = unwrap_method_instance_callable(&args[2]);
                let ValueView::Sub(sub_data) = method_value.view() else {
                    return Ok(Value::NIL);
                };
                let def = MethodDef {
                    lexical_package: sub_data.package.resolve(),
                    params: sub_data.params.clone(),
                    param_defs: sub_data.param_defs.clone(),
                    body: sub_data.body.clone(),
                    is_rw: sub_data.is_rw,
                    is_private: false,
                    is_multi: true,
                    is_my: false,
                    role_origin: None,
                    original_role: None,
                    return_type: None,
                    compiled_code: None,
                    compiled_fns: None,
                    delegation: None,
                    is_default: false,
                    deprecated_message: None,
                    is_submethod: false,
                    captured_env: if sub_data.env.is_empty() {
                        None
                    } else {
                        Some(sub_data.env.clone())
                    },
                    source_file: sub_data.source_file.clone(),
                    role_param_bindings: None,
                };
                // `^add_multi_method` must still *error* for an unregistered
                // class -- existence keys off `classes.contains_key`, not the
                // method table (ADR-0019 F4c design note (0)(iii)).
                if self.registry().classes.contains_key(&class_name) {
                    self.registry_mut().push_user_method(
                        Symbol::intern(&class_name),
                        Symbol::intern(&method_name),
                        def,
                    );
                    self.native_ctor_plan_cache.clear();
                    return Ok(Value::NIL);
                }
                Err(RuntimeError::new(format!(
                    "Unknown class for add_multi_method: {}",
                    class_name
                )))
            }
            "add_fallback" if args.len() >= 3 => {
                // ^add_fallback($type, &condition, &calculator): register a
                // dynamic method fallback. When a method is not found on a value
                // of this class, `&condition($obj, $name)` is checked; the first
                // that returns True has `&calculator($obj, $name)` produce the
                // method body, which is then invoked with the invocant.
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => {
                        return Err(RuntimeError::new(
                            "add_fallback target must be a type object",
                        ));
                    }
                };
                let condition = args[1].clone();
                let calculator = args[2].clone();
                self.method_fallbacks
                    .entry(class_name)
                    .or_default()
                    .push((condition, calculator));
                Ok(Value::NIL)
            }
            "compose" if !args.is_empty() => {
                // ^compose recomposes the class (e.g. after add_method)
                // Rebuild the MRO for the class
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => return Ok(Value::NIL),
                };
                let mro = self.class_mro(&class_name);
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                    class_def.mro = mro;
                }
                self.native_ctor_plan_cache.clear();
                Ok(Value::NIL)
            }
            // `$type.HOW.add_parent($type, $parent)` — the native ClassHOW
            // metamethod a user HOW (`class MyHOW is Metamodel::ClassHOW`) reaches
            // via `callsame`/`nextsame` or a direct fallback. Adds `$parent` to
            // `$type`'s parent list (idempotent — mutsu's `is Parent` already
            // installs it during declaration, so a trait-driven re-add must not
            // duplicate it) and recomputes the MRO.
            "add_parent" if args.len() >= 2 => {
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => return Ok(Value::NIL),
                };
                let parent_name = match args[1].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => return Ok(Value::NIL),
                };
                let mut changed = false;
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name)
                    && !class_def.parents.contains(&parent_name)
                {
                    class_def.parents.push(parent_name.clone());
                    changed = true;
                }
                if changed {
                    let mro = self.class_mro(&class_name);
                    if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                        class_def.mro = mro;
                    }
                    self.native_ctor_plan_cache.clear();
                }
                Ok(Value::NIL)
            }
            "add_attribute" if args.len() >= 2 => {
                // ^add_attribute($type, $attr)
                // Adds an Attribute object to a dynamically created class
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => return Ok(Value::NIL),
                };
                if let ValueView::Instance {
                    class_name: attr_class,
                    attributes: attr_attrs,
                    ..
                } = args[1].view()
                    && attr_class.resolve() == "Attribute"
                {
                    let attr_name_raw = attr_attrs
                        .as_map()
                        .get("name")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    // Strip sigil+twigil prefix to get bare name (e.g. "$!inner" -> "inner")
                    let bare_name = attr_name_raw
                        .trim_start_matches(|c: char| "$.!@%&".contains(c))
                        .to_string();
                    let has_accessor = attr_attrs
                        .as_map()
                        .get("has_accessor")
                        .map(|v| v.truthy())
                        .unwrap_or(false);
                    let is_rw = attr_attrs
                        .as_map()
                        .get("rw")
                        .map(|v| v.truthy())
                        .unwrap_or(false);
                    let type_constraint =
                        attr_attrs
                            .as_map()
                            .get("type")
                            .and_then(|v| match v.view() {
                                ValueView::Package(name) => Some(name.resolve()),
                                _ => None,
                            });
                    let sigil = attr_name_raw.chars().next().unwrap_or('$');
                    // Add the attribute to the class definition
                    if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                        class_def.attributes.push(ClassAttributeDef {
                            name: bare_name.clone(),
                            is_public: has_accessor,
                            default: None,
                            is_rw,
                            is_required: None,
                            sigil,
                            where_constraint: None,
                            declared_shape: None,
                        });
                        if let Some(tc) = type_constraint {
                            class_def.attribute_types.insert(bare_name, tc);
                        }
                    }
                    // Attribute set changed — drop cached construction plans.
                    self.native_ctor_plan_cache.clear();
                }
                Ok(Value::NIL)
            }
            "methods" if !args.is_empty() => self.dispatch_classhow_methods(&args),
            "attributes" if !args.is_empty() => {
                let owner_class = self.mop_receiver_owner(&args[0]);
                let local_only = args[1..].iter().any(
                    |a| matches!(a.view(), ValueView::Pair(k, v) if k == "local" && v.truthy()),
                );
                let values = self.collect_attribute_objects(&owner_class, local_only);
                Ok(Value::array(values))
            }
            "parents" if !args.is_empty() => self.dispatch_classhow_parents(&args),
            "pun" if !args.is_empty() => {
                let role_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => args[0].to_string_value(),
                };
                self.punned_role_type_object(&role_name)
            }
            "roles" if !args.is_empty() => self.dispatch_classhow_roles(&args),
            "candidates" if !args.is_empty() && self.is_role_reference_value(&args[0]) => {
                let base_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::ParametricRole { base_name, .. } => base_name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => args[0]
                        .to_string_value()
                        .trim_start_matches('(')
                        .trim_end_matches(')')
                        .to_string(),
                };
                if let Some(candidates) = self.registry().role_candidates.get(&base_name) {
                    let values = candidates
                        .iter()
                        .enumerate()
                        .map(|(idx, cand)| {
                            // Create Instance values with candidate index so
                            // .WHY can look up per-candidate doc comments
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert(
                                "__mutsu_role_candidate_idx".to_string(),
                                Value::int(idx as i64),
                            );
                            attrs.insert(
                                "__mutsu_role_base_name".to_string(),
                                Value::str(base_name.clone()),
                            );
                            // Embed per-candidate language revision
                            let revision: String =
                                if let Some(letter) = cand.language_version.strip_prefix("6.") {
                                    letter.chars().next().unwrap_or('c').to_string()
                                } else {
                                    "c".to_string()
                                };
                            attrs.insert(
                                "__mutsu_language_revision".to_string(),
                                Value::str(revision),
                            );
                            Value::make_instance(Symbol::intern(&base_name), attrs)
                        })
                        .collect::<Vec<_>>();
                    return Ok(Value::array(values));
                }
                if self.registry().roles.contains_key(&base_name) {
                    return Ok(Value::array(vec![Value::package(Symbol::intern(
                        &base_name,
                    ))]));
                }
                Ok(Value::array(Vec::new()))
            }
            "concretization" if args.len() >= 2 => {
                let class_name = self.mop_receiver_owner(&args[0]);
                let role_name = match args[1].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::ParametricRole {
                        base_name,
                        type_args,
                    } => {
                        let args_str = type_args
                            .iter()
                            .map(|v| match v.view() {
                                ValueView::Package(n) => n.resolve(),
                                _ => v.to_string_value(),
                            })
                            .collect::<Vec<_>>()
                            .join(",");
                        format!("{}[{}]", base_name, args_str)
                    }
                    _ => args[1].to_string_value(),
                };
                let base_role_name = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role_name.as_str());
                // Check for :local named arg
                let local_only = args[2..].iter().any(
                    |a| matches!(a.view(), ValueView::Pair(k, v) if k == "local" && v.truthy()),
                );
                // Check direct composed roles and transitive sub-roles
                let check_transitive =
                    |class_composed: &rustc_hash::FxHashMap<String, Vec<String>>,
                     role_parents: &rustc_hash::FxHashMap<String, Vec<String>>,
                     cn: &str|
                     -> Option<Value> {
                        let composed = class_composed.get(cn).cloned().unwrap_or_default();
                        // Check direct matches
                        for cr in &composed {
                            let cr_base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                            if *cr == role_name || cr_base == base_role_name {
                                return Some(Value::package(Symbol::intern(cr_base)));
                            }
                        }
                        // Check transitive sub-roles
                        let mut stack: Vec<String> = composed
                            .iter()
                            .map(|cr| {
                                cr.split_once('[')
                                    .map(|(b, _)| b)
                                    .unwrap_or(cr.as_str())
                                    .to_string()
                            })
                            .collect();
                        let mut seen = std::collections::HashSet::new();
                        while let Some(rn) = stack.pop() {
                            if !seen.insert(rn.clone()) {
                                continue;
                            }
                            if let Some(rp) = role_parents.get(&rn) {
                                for p in rp {
                                    let p_base =
                                        p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                                    if p_base == base_role_name || *p == role_name {
                                        return Some(Value::package(Symbol::intern(p_base)));
                                    }
                                    stack.push(p_base.to_string());
                                }
                            }
                        }
                        None
                    };
                if let Some(result) = check_transitive(
                    &self.registry().class_composed_roles,
                    &self.registry().role_parents,
                    &class_name,
                ) {
                    return Ok(result);
                }
                if !local_only {
                    let mro = self.class_mro(&class_name);
                    for cn in mro[1..].iter().map(|s| s.as_str()) {
                        if let Some(result) = check_transitive(
                            &self.registry().class_composed_roles,
                            &self.registry().role_parents,
                            cn,
                        ) {
                            return Ok(result);
                        }
                    }
                }
                Err(RuntimeError::new(format!(
                    "No concretization of {} found for {}",
                    role_name, class_name
                )))
            }
            "curried_role" if !args.is_empty() => {
                // For a parameterized role like R[Int], return the base role R
                match args[0].view() {
                    ValueView::ParametricRole { base_name, .. } => Ok(Value::package(base_name)),
                    ValueView::Package(name) => {
                        let resolved = name.resolve();
                        let base = resolved
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(resolved.as_str());
                        Ok(Value::package(Symbol::intern(base)))
                    }
                    _ => {
                        let s = args[0].to_string_value();
                        let base = s.split_once('[').map(|(b, _)| b).unwrap_or(s.as_str());
                        Ok(Value::package(Symbol::intern(base)))
                    }
                }
            }
            // `EnumHOW.enum_values`: a Map from each value's *name* to its
            // underlying value (`Numbers.^enum_values` is `{10 => 0, 20 => 1}`).
            "enum_values" if !args.is_empty() => {
                let Some(variants) = self.enum_how_variants(&args[0]) else {
                    return Err(Self::enum_how_method_missing("enum_values", &args[0]));
                };
                let mut map = HashMap::new();
                for (key, val) in &variants {
                    map.insert(key.clone(), val.to_value());
                }
                Ok(Value::hash(map))
            }
            // `EnumHOW.elems`: how many values the enum declares. On any other
            // metaobject `elems` is just the inherited `Any.elems`, which is 1
            // (`class C {}; C.HOW.elems` is 1 in raku). Handling it here is what
            // keeps a HOW invocant out of the generic `.elems` dispatch, which
            // has no implementation for a HOW instance and used to bounce
            // between `dispatch_elems_method` and `builtin_elems` until the
            // stack overflowed.
            "elems" if !args.is_empty() => Ok(Value::int(
                self.enum_how_variants(&args[0])
                    .map_or(1, |variants| variants.len() as i64),
            )),
            // `EnumHOW.enum_from_value`: the enum value whose underlying value
            // equals the argument, or `Mu` when none does.
            "enum_from_value" if args.len() >= 2 => {
                let Some(variants) = self.enum_how_variants(&args[0]) else {
                    return Err(Self::enum_how_method_missing("enum_from_value", &args[0]));
                };
                let type_name = self.mop_receiver_owner(&args[0]);
                let wanted = &args[1];
                let found = variants
                    .iter()
                    .enumerate()
                    .find(|(_, (_, val))| val.to_value().eqv(wanted));
                Ok(match found {
                    Some((index, (key, val))) => Value::enum_parts(
                        Symbol::intern(&type_name),
                        Symbol::intern(key),
                        val.clone(),
                        index,
                    ),
                    None => Value::package(Symbol::intern("Mu")),
                })
            }
            "enum_value_list" if !args.is_empty() => {
                let type_name = match args[0].view() {
                    ValueView::Package(name) => Some(name.resolve()),
                    ValueView::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                if let Some(type_name) = type_name
                    && let Some(variants) = self.registry().enum_types.get(&type_name)
                {
                    let values: Vec<Value> = variants
                        .iter()
                        .enumerate()
                        .map(|(index, (key, val))| {
                            Value::enum_parts(
                                Symbol::intern(&type_name),
                                Symbol::intern(key),
                                val.clone(),
                                index,
                            )
                        })
                        .collect();
                    Ok(Value::array(values))
                } else {
                    Ok(Value::array(Vec::new()))
                }
            }
            "language-revision" if !args.is_empty() => {
                // Check for per-candidate language revision embedded as an
                // attribute (set by ^candidates for role candidate instances).
                if let ValueView::Instance { attributes, .. } = args[0].view()
                    && let Some(rev) = attributes.as_map().get("__mutsu_language_revision")
                {
                    return Ok(rev.clone());
                }
                // Check for language revision in Mixin metadata (from
                // parametric role pun instances).
                if let ValueView::Mixin(_, mixins) = args[0].view()
                    && let Some(rev) = mixins.get("__mutsu_language_revision")
                {
                    return Ok(rev.clone());
                }
                let type_name = self.mop_receiver_owner(&args[0]);
                if let Some(meta) = self.type_metadata.get(&type_name)
                    && let Some(rev) = meta.get("language-revision")
                {
                    return Ok(rev.clone());
                }
                // Default to current language revision
                let version = crate::parser::current_language_version();
                let letter = if let Some(rest) = version.strip_prefix("6.") {
                    rest.chars().next().unwrap_or('c').to_string()
                } else {
                    "c".to_string()
                };
                Ok(Value::str(letter))
            }
            "method_table" if !args.is_empty() => {
                let type_name = match args[0].view() {
                    ValueView::RakuAst(node) => node.class.printed_name().to_string(),
                    _ => self.mop_receiver_owner(&args[0]),
                };
                Ok(Value::hash(self.class_method_table(&type_name)))
            }
            "submethod_table" if !args.is_empty() => {
                // ADR-0019 F4c-1: enumerate via the canonical reverse index
                // instead of `class_def.methods.keys()` (zero-mismatch
                // shadow-checked across the full local `t/` suite).
                let type_name = self.mop_receiver_owner(&args[0]);
                let mut table = HashMap::new();
                let registry = self.registry();
                for name in registry.owner_method_names(&type_name) {
                    let name = name.resolve();
                    if registry
                        .user_method_overloads(&type_name, &name)
                        .is_some_and(|defs| defs.iter().any(|d| d.is_my))
                    {
                        table.insert(name.clone(), Value::str(name));
                    }
                }
                Ok(Value::hash(table))
            }
            "nativesize" if args.len() == 1 => {
                let type_name = self.mop_receiver_owner(&args[0]);
                match native_types::native_type_bits(&type_name) {
                    Some(bits) => Ok(Value::int(i64::from(bits))),
                    None => Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): nativesize",
                    )),
                }
            }
            "unsigned" if args.len() == 1 => {
                let type_name = self.mop_receiver_owner(&args[0]);
                if native_types::native_type_bits(&type_name).is_some() {
                    Ok(Value::int(i64::from(!native_types::is_signed_native(
                        &type_name,
                    ))))
                } else {
                    Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): unsigned",
                    ))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                method
            ))),
        }
    }
}
