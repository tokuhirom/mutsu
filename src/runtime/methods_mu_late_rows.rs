//! The routines behind `Mu`'s and `Cool`'s late rows (ADR-11276 §9.138): the
//! last-resort `gist`, `raku`/`perl`, `DUMP`, `isa`, `REPR`, `Str`/`Stringy`
//! and `EVAL` of a receiver no other answer took. They were the trailing arms
//! of `dispatch_instance_and_fallback`.

use super::*;

impl Interpreter {
    /// `Mu.DUMP`: a type object is `Name()`, anything else its string form.
    // Cost: O(n), n = chars of the rendered string.
    pub(crate) fn mu_dump_late(target: &Value) -> Value {
        match target.view() {
            ValueView::Package(name) => Value::str(format!("{}()", name)),
            _ => Value::str(target.to_string_value()),
        }
    }

    /// `Mu.gist`: a type object is `(ShortName)` (`()` when anonymous),
    /// anything else its string form.
    // Cost: O(n), n = chars of the rendered string.
    pub(crate) fn mu_gist_late(target: &Value) -> Value {
        match target.view() {
            ValueView::Package(name) => {
                let resolved = name.resolve();
                if crate::value::is_nameless_anon_type_name(&resolved) {
                    return Value::str_from("()");
                }
                let short =
                    crate::qualified::last_segment(crate::qualified::known_symbol(&resolved))
                        .as_str();
                Value::str(format!("({})", short))
            }
            _ => Value::str(target.to_string_value()),
        }
    }

    /// `Mu.raku`: a type object is its name, a `Junction` its kind over its
    /// values' own `raku`, anything else its string form.
    // Cost: O(v) method calls for a junction, v = its values; O(n) otherwise,
    // n = chars of the rendered string.
    pub(crate) fn mu_raku_late(&mut self, target: &Value) -> Value {
        match target.view() {
            ValueView::Package(name) => Value::str(name.resolve()),
            ValueView::Junction { kind, values } => {
                let kind_name = match kind {
                    JunctionKind::Any => "any",
                    JunctionKind::All => "all",
                    JunctionKind::One => "one",
                    JunctionKind::None => "none",
                };
                let mut parts = Vec::with_capacity(values.len());
                for value in values.iter() {
                    if let ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } = value.view()
                        && let Some((owner, method_def)) =
                            self.resolve_method_with_owner(&class_name.resolve(), "raku", &[])
                    {
                        let attrs_map = attributes.to_map();
                        // Reuse the resolved (owner, method_def) instead of
                        // re-deriving it a second time inside
                        // `run_instance_method`'s own ad-hoc walk (ADR-0019 F6).
                        if let Ok((rendered, _)) = self.run_resolved_method_compiled_or_treewalk(
                            &class_name.resolve(),
                            &owner.resolve(),
                            "raku",
                            method_def,
                            attrs_map,
                            Vec::new(),
                            None,
                        ) {
                            parts.push(rendered.to_string_value());
                            continue;
                        }
                    }
                    let rendered = self
                        .call_method_with_values(value.clone(), "raku", Vec::new())
                        .unwrap_or_else(|_| Value::str(value.to_string_value()));
                    parts.push(rendered.to_string_value());
                }
                Value::str(format!("{}({})", kind_name, parts.join(", ")))
            }
            _ => Value::str(target.to_string_value()),
        }
    }

    /// `Mu.isa` of a type object: whether the type is the argument's or
    /// derives from it, through a subset's bases, a RakuAST node's hierarchy, a
    /// role's pun or the class MRO. `None` for any other receiver.
    // Cost: O(s + m), s = subset chain length, m = MRO length.
    pub(crate) fn mu_isa_late(&mut self, target: &Value, isa_arg: &Value) -> Option<Value> {
        let ValueView::Package(name) = target.view() else {
            return None;
        };
        let pkg_name = name.resolve();
        let target_name = match isa_arg.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Str(name) => name.to_string(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            // `isa(Mu \type)`: any other value stands for its own type.
            _ => crate::value::types::what_type_name(isa_arg),
        };
        if pkg_name == target_name {
            return Some(Value::TRUE);
        }
        if pkg_name.starts_with("RakuAST::") && target_name.starts_with("RakuAST::") {
            return Some(Value::truth(crate::rakuast::type_object_isa(
                &pkg_name,
                &target_name,
            )));
        }
        if let Some(mut base) = self
            .registry()
            .subsets
            .get(&pkg_name)
            .map(|s| s.base.clone())
        {
            loop {
                if base == target_name {
                    return Some(Value::TRUE);
                }
                let Some(parent_base) = self.registry().subsets.get(&base).map(|s| s.base.clone())
                else {
                    break;
                };
                if parent_base == base {
                    break;
                }
                base = parent_base;
            }
        }
        // A role type object has no class MRO of its own — `class_mro` on a
        // role name yields just that name, so every probe answered False. raku
        // answers `.isa` for it from the chain the role's *pun* would have,
        // which is exactly `Any` then `Mu`: `Associative.isa(Any)`,
        // `Positional.isa(Mu)` and `role R {}; R.isa(Any)` are all True, while
        // `.isa` of any other type — including a role the role itself `does`
        // (`role B does A {}; B.isa(A)`) — is False. `.does` remains the
        // operator that answers role composition.
        if self.is_role_type_name(&pkg_name) {
            return Some(Value::truth(target_name == "Any" || target_name == "Mu"));
        }
        Some(Value::truth(
            self.class_mro(&pkg_name)
                .contains(&crate::symbol::Symbol::intern(&target_name)),
        ))
    }

    /// `Mu.REPR`: the receiver's representation name.
    // Cost: O(1) lookups, plus O(n) to parse a type-object name, n = chars.
    pub(crate) fn mu_repr_late(&mut self, target: &Value) -> Value {
        // CustomType/CustomTypeInstance report their stored REPR; a class
        // declared `is repr('CStruct')` (or CUnion/CPointer) reports that even
        // as a *type object*, which is where a NativeCall binding gates on it
        // (`die "Need a CStruct" unless T.REPR eq 'CStruct'`); every other type
        // is P6opaque.
        if let Some(repr) = target.custom_repr() {
            return Value::str_from(repr);
        }
        // A native type object: a `native` declaration's `is repr<...>`, or
        // the core's own `int*`/`num*`/`str` (`P6int`/`P6num`/`P6str`).
        if let ValueView::Package(name) = target.view() {
            let name = name.resolve();
            // A definite (`Foo:D`, `Foo:U`) or coercion (`Int(Str)`) type
            // object is made by `DefiniteHOW`/`CoercionHOW`, which cannot be
            // instantiated: `Uninstantiable`, whatever the base type's REPR.
            // NativeCall's `validnctype` accepts a `--> ::?CLASS:D` return by
            // it. (`Foo:_` is `Foo` itself.)
            if matches!(
                crate::runtime::types::strip_type_smiley(&name).1,
                Some(":D" | ":U")
            ) || crate::runtime::types::is_coercion_constraint(&name)
            {
                return Value::str_from("Uninstantiable");
            }
            if let Some(repr) = self.native_decl(&name).and_then(|d| d.repr) {
                return Value::str(repr);
            }
            if let Some(repr) = crate::runtime::native_decl::builtin_native_repr(&name) {
                return Value::str_from(repr);
            }
            if let Some(repr) = crate::runtime::native_decl::builtin_buf_type_repr(&name) {
                return Value::str_from(repr);
            }
            // A role type object -- `Blob`, `Positional[Int]`, a user role, a
            // curried `Buf[uint8]` -- cannot be instantiated until it is
            // punned, so it reports `Uninstantiable`, as a role's
            // `ParametricRoleGroupHOW`/`CurriedRoleHOW` does in rakudo.
            // Upstream NativeCall's `validnctype` relies on it to accept a
            // `Blob` parameter.
            if self.is_role_type_name(&Self::optional_type_object_name(&name)) {
                return Value::str_from("Uninstantiable");
            }
        }
        // Type objects only. An *instance* reaches here when it has no C
        // storage (a struct with no layout NativeCall can compute), and it must
        // keep under-reporting `P6opaque`: answering the honest name without a
        // body would make `BODY_OF` dereference whatever `.WHERE` returned. A
        // live handle, and a struct that owns its body (ADR-11209), already
        // answer `CStruct` through `try_native_handle_repr_where`. A
        // `NativeCall`- or `CStr`-REPR instance (upstream's `Callsite` and
        // `CStr`) is its body whole, so it has no body for `.REPR` to
        // under-report.
        let class = match target.view() {
            ValueView::Package(name) => Some(name.resolve()),
            ValueView::Instance { class_name, .. }
                if self.is_bodied_repr_class(class_name.as_str()) =>
            {
                Some(class_name.resolve())
            }
            _ => None,
        };
        match class.as_deref().and_then(|c| self.declared_class_repr(c)) {
            Some(repr) => Value::str_from(repr),
            None => Value::str_from("P6opaque"),
        }
    }

    /// `Mu.Str` / `Mu.Stringy` for a receiver every other answer declined.
    // Cost: O(n), n = chars of the rendered string, plus a user `Str` call.
    pub(crate) fn mu_str_late(&mut self, target: &Value, method: &str) -> Result<Value, RuntimeError> {
        match target.view() {
            ValueView::Package(name) => {
                // A bare type object stringifies to "" with rakudo's
                // "uninitialized value of type X in string context" warning
                // (matching `~Int`); a user `.Str`/`.Stringy` dispatched before
                // reaching here. `.Stringy`'s default implementation is
                // `self.Str`, so `.Stringy` on a type object with only a user
                // `.Str` delegates to it; `.Str` never falls back to `.Stringy`.
                let n = name.resolve();
                if method == "Stringy" && self.has_user_method(&n, "Str") {
                    self.call_method_with_values(target.clone(), "Str", vec![])
                } else {
                    self.warn_type_object_string_context(&n, false)
                }
            }
            // `Str.Stringy` is `self`: a `Str` subclass answers with the
            // instance itself even when it declares its own `Str` (#11026).
            ValueView::Instance { .. }
                if method == "Stringy" && self.str_subclass_stringy_payload(target).is_some() =>
            {
                Ok(target.clone())
            }
            // `Method.Str` is `Code.Str`: the name, with the coercion warning.
            ValueView::Instance { class_name, .. }
                if matches!(class_name.as_str(), "Method" | "Submethod")
                    && !self.has_user_method(&class_name.resolve(), "Str") =>
            {
                self.code_str(target)
                    .unwrap_or_else(|| Ok(Value::str(target.to_string_value())))
            }
            ValueView::Instance { class_name, .. } => {
                // Stringy defaults to Str, but Str does not default back to
                // Stringy. A class that defines only Stringy therefore keeps
                // Mu.Str's object representation for an explicit `.Str` call;
                // only the Stringy direction may delegate to the alternate
                // method.
                if method == "Stringy" && self.has_user_method(&class_name.resolve(), "Str") {
                    self.call_method_with_values(target.clone(), "Str", vec![])
                } else {
                    Ok(Value::str(target.to_string_value()))
                }
            }
            _ => Ok(Value::str(target.to_string_value())),
        }
    }
}
