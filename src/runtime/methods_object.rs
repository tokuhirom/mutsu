use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Returns `true` if `class_name` (and every user class in its MRO) can be
    /// default-constructed (`Foo.new(...)`) without running any user code:
    /// no BUILD/TWEAK/BUILDALL/custom-new and no custom per-attribute build
    /// closure, no native methods, and only public attributes that are either a
    /// `$`-scalar (optionally with a simple `type_matches_value`-checkable class
    /// constraint) or an untyped `@`/`%` without an `is Type` trait, none of
    /// them required or `where`-constrained. Parents must themselves be
    /// `Any`/`Mu`/`Cool` or another such simple class (so a plain inheritance
    /// chain like `Dog is Animal` still qualifies). Construction is then pure
    /// data: assign (sigil-coerced) named args to attributes and evaluate
    /// attribute defaults.
    pub(crate) fn is_native_default_constructible(&self, cn_resolved: &str) -> bool {
        // A parametric type name (`Hash[Int,Str]`, `array[int]`) needs the
        // interpreter's parametric construction machinery — keep it out.
        if cn_resolved.contains('[') {
            return false;
        }
        // A `::`-namespaced name is allowed: built-in exception types
        // (`X::AdHoc`, `X::TypeCheck::Binding`) and user `A::B` classes are pure
        // attribute-bag data assembly just like a non-namespaced class, so they
        // pass through the same eligibility checks below. (§D state-ownership: the
        // VM constructs them natively instead of bouncing to `dispatch_new`.) The
        // exception-`message` materialization that the interpreter applies after
        // `dispatch_new` is reproduced at the native call sites via
        // `materialize_exception_message_in_result`, so behavior stays identical.
        if !self.registry().classes.contains_key(cn_resolved) {
            return false;
        }
        // A `repr('CUnion')` class lays its native fields over shared memory, so
        // construction is a byte overlay (`construct_cunion_instance`), not plain
        // per-attribute data assignment. Keep it on the interpreter.
        if self.registry().cunion_classes.contains(cn_resolved) {
            return false;
        }
        // A same-named attribute redeclared across the hierarchy (Parent and Child
        // both `has $.x`) needs per-class private storage (`"Class\0attr"` keys),
        // which only the full constructor path builds. Such classes are rare, so
        // fall through to the interpreter rather than duplicate that logic here.
        {
            let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
            for cls in self.mro_readonly(cn_resolved) {
                if let Some(cd) = self.registry().classes.get(&cls) {
                    for attr in &cd.attributes {
                        if !seen.insert(attr.0.clone()) {
                            return false;
                        }
                    }
                }
            }
        }
        // A `has $.x does Role` attribute is allowed: the native builder mixes
        // the declared role(s) into the attribute value via the shared
        // `apply_attribute_does_role_mixins` helper (same approximation as the
        // full constructor), as a post-assembly phase.
        let mut has_attribute = false;
        // A built-in exception type (`X::AdHoc`, `X::TypeCheck::Binding`, ...) is
        // registered with NO declared attributes — its named constructor args
        // (`payload`, `got`, `expected`, ...) are stored as a generic attribute
        // bag via `is_attribute_buildable`'s undeclared-name `true` fallback, which
        // `build_native_default_instance` already honours identically to the
        // interpreter. So such a class is native-constructible even though
        // `has_attribute` stays false; track that here.
        let mut is_exception = false;
        for cls in self.mro_readonly(cn_resolved) {
            if cls == "Exception" {
                is_exception = true;
            }
            if cls == "Any" || cls == "Mu" || cls == "Cool" {
                continue;
            }
            // Single guard for the whole eligibility check (used by both the
            // class_def lookup and the nested parent contains_key — avoids a
            // same-thread recursive read lock). No user-code re-entry here.
            let registry = self.registry();
            let Some(class_def) = registry.classes.get(&cls) else {
                // A non-class entry in the MRO (e.g. a role) — be conservative.
                return false;
            };
            // `TWEAK` and `BUILD` submethods are allowed: the native path
            // assembles the instance as usual (defaults first, matching the
            // interpreter's `.new` which also applies defaults before BUILD),
            // then runs the BUILD and TWEAK phases via the shared
            // `run_build_phase` / `run_tweak_phase` helpers (same ordering /
            // dispatch / `fail` semantics as the full constructor). A custom
            // `BUILDALL` (replaces the whole build plan) or custom `new` still
            // fall through. A class with an unprovided `is required` or unset
            // class-typed attribute also falls through at build time (it may be
            // set by BUILD, which the conservative native path does not assume).
            let simple = !class_def.methods.contains_key("BUILDALL")
                && !class_def.methods.contains_key("new")
                && class_def.native_methods.is_empty()
                // An `is built(Bool)` trait only flips whether an attribute is
                // assigned from a named arg — the native builder already honours
                // it through `is_attribute_buildable` in the named-arg loop (an
                // `is built(False)` attribute is simply skipped and then gets its
                // default in the fill loop), so it stays native. A MOP
                // `Attribute.set_build` closure, however, *replaces* plain data
                // assignment with user code and must run through the full
                // constructor.
                && !registry
                    .attribute_build_overrides
                    .keys()
                    .any(|(owner, _)| owner == &cls)
                && class_def.parents.iter().all(|p| {
                    p == "Any" || p == "Mu" || p == "Cool" || registry.classes.contains_key(p)
                })
                && class_def.attributes.iter().all(
                    |(name, _, _, _is_required, type_constraint, sigil, _where_constraint)| {
                        // A constructible sigil/type shape:
                        // - `$`: a type constraint is allowed only when it is a
                        //   plain `type_matches_value`-checkable class/role/subset
                        //   type (see `is_simple_native_ctor_constraint`); native /
                        //   coercion / parametric types keep the interpreter.
                        // - `@`/`%`: only untyped with no `is Type` trait — typed
                        //   elements need container type metadata and `is Type`
                        //   builds a typed container, both interpreter-owned.
                        // `is required` is allowed through: the native builder
                        // raises `X::Attribute::Required` itself for an unprovided
                        // required attribute (before defaults when there is no
                        // BUILD, or after BUILD when there is one).
                        // A `where` clause is allowed: the native builder runs
                        // `enforce_attribute_where_constraints` as a post-assembly
                        // phase (same predicate dispatch as the full constructor).
                        match sigil {
                                '$' => match type_constraint {
                                    None => true,
                                    Some(inner) => inner.as_deref().is_some_and(|tc| {
                                        Self::is_simple_native_ctor_constraint(tc)
                                            || Self::native_scalar_default(tc).is_some()
                                            || Self::is_native_coercion_ctor_constraint(tc)
                                    }),
                                },
                                '@' | '%' => {
                                    // An `is Type` container (`has @.a is Buf`)
                                    // builds a special typed container — keep the
                                    // interpreter. Otherwise: untyped is fine, and
                                    // a typed element is allowed only when it is a
                                    // plain `type_matches_value`-checkable class
                                    // type (`has Int @.nums`); a native element
                                    // (`has int @.x` -> packed `array[int]`) or a
                                    // parametric/coercion element keeps the
                                    // interpreter's richer construction.
                                    if registry
                                        .class_attribute_is_types
                                        .contains_key(&(cls.clone(), name.clone()))
                                    {
                                        false
                                    } else {
                                        match class_def.attribute_types.get(name) {
                                            None => true,
                                            Some(et) => Self::is_simple_native_ctor_constraint(et),
                                        }
                                    }
                                }
                                _ => false,
                            }
                    },
                )
                && class_def.attribute_types.values().all(|tc| {
                    // A typed scalar `$` constraint: a plain class type
                    // (`type_matches_value`-checkable), a native scalar type
                    // (`int`/`num`/`str`, defaulting to a native zero rather than a
                    // type object), or a built-in-target coercion type (`Int()`).
                    // Typed `@`/`%` containers are already rejected by the
                    // per-attribute sigil branch above.
                    Self::is_simple_native_ctor_constraint(tc)
                        || Self::native_scalar_default(tc).is_some()
                        || Self::is_native_coercion_ctor_constraint(tc)
                });
            // A definedness smiley (`:D`/`:U`/`:_`) on an attribute is allowed:
            // the native builder runs `enforce_attribute_smiley_constraints`
            // (the same predicate the full constructor uses) post-assembly and
            // again after BUILD, matching the full path's enforcement points. A
            // bare `:D` with no default and no `is required` is already a
            // parse-time error (`X::Syntax::Variable::MissingInitializer`), so it
            // never reaches here.
            if !simple {
                return false;
            }
            has_attribute |= !class_def.attributes.is_empty();
        }
        has_attribute || is_exception
    }

    /// A type constraint the native default constructor can enforce with a plain
    /// `type_matches_value` check: a normal class/role/subset type (starts
    /// uppercase). Excludes native lowercase types (`int`/`num`/`str` — they
    /// coerce and default to `0`/`""`, not a type object), coercion types
    /// (`Int()`), and parametric types (`Positional[Int]`). Everything excluded
    /// keeps the interpreter's richer construction semantics.
    pub(crate) fn is_simple_native_ctor_constraint(tc: &str) -> bool {
        tc.starts_with(char::is_uppercase) && !tc.contains('(') && !tc.contains('[')
    }

    /// A coercion-type constraint (`Int()`, `Int(Str)`) whose target is a
    /// built-in scalar type the native constructor can coerce to without running
    /// user-defined code. A user-class coercion target (with a `COERCE` method)
    /// keeps the interpreter, which runs that method. Coercion itself is routed
    /// through the shared `coerce_value_for_constraint`, so a native-coerced value
    /// is identical to the interpreter's.
    pub(crate) fn is_native_coercion_ctor_constraint(tc: &str) -> bool {
        match crate::runtime::types::parse_coercion_type(tc) {
            Some((target, _src)) => matches!(
                target,
                "Int" | "Num" | "Rat" | "FatRat" | "Complex" | "Str" | "Bool" | "Numeric" | "Real"
            ),
            None => false,
        }
    }

    /// The default value for an uninitialized native-typed scalar attribute
    /// (`has int $.x`, `has num $.y`, `has str $.z`): the native zero/empty,
    /// not a type object. Mirrors the interpreter's `bless` default logic in
    /// `methods_dispatch_new.rs`. Returns `None` for non-native types so the
    /// caller can distinguish a native scalar constraint from a class one.
    pub(crate) fn native_scalar_default(tc: &str) -> Option<Value> {
        match tc {
            "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
            | "uint32" | "uint64" | "byte" | "atomicint" => Some(Value::Int(0)),
            "num" | "num32" | "num64" => Some(Value::Num(0.0)),
            "str" => Some(Value::str(String::new())),
            _ => None,
        }
    }

    /// True if any class in `cn`'s MRO declares a `BUILD` or `TWEAK` submethod.
    /// The native construction path (`build_native_default_instance`) runs those
    /// phases via `run_build_phase`/`run_tweak_phase`, and such a submethod body
    /// can mutate a *captured-outer* caller lexical (`my $n; submethod TWEAK {
    /// $n++ }`). So a construction of such a class is NOT env-pure — the caller's
    /// slot must be reconciled at the call site (Slice F). Used by the VM `.new`
    /// dispatch to set `method_dispatch_pure` correctly.
    pub(crate) fn mro_has_build_or_tweak(&self, cn: &str) -> bool {
        self.mro_readonly(cn).iter().any(|cls| {
            self.registry().classes.get(cls).is_some_and(|cd| {
                cd.methods.contains_key("BUILD") || cd.methods.contains_key("TWEAK")
            })
        })
    }

    /// Default-construct `class_name` natively when it is eligible (see
    /// `is_native_default_constructible`). Returns `None` for ineligible
    /// classes so callers fall through to the full constructor dispatch.
    /// This is the construction fast path shared by `dispatch_new` and the VM.
    pub(crate) fn try_native_default_construct(
        &mut self,
        class_name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn_resolved = class_name.resolve();
        // A `repr('CUnion')` class constructs via a byte overlay
        // (`construct_cunion_instance`): its native-int fields share the same
        // underlying bytes. The interpreter's `dispatch_new` does *nothing else*
        // for a CUnion class (no BUILD/TWEAK/required), so run that exact shared
        // helper here and skip the interpreter round-trip. `is_native_default_
        // constructible` still rejects CUnion classes, keeping
        // `build_native_default_instance` (plain per-attribute assignment) from
        // ever touching the byte overlay.
        if self.registry().cunion_classes.contains(&cn_resolved) {
            return Some(self.construct_cunion_instance(&cn_resolved, args));
        }
        if !self.is_native_default_constructible(&cn_resolved) {
            return None;
        }
        self.build_native_default_instance(class_name, &cn_resolved, args)
    }

    /// Apply `has $.x does Role` attribute traits: mix each declared role into
    /// the attribute's value so `$o.x` does the role and `$o.x.method` dispatches
    /// into it. (raku mixes into the Scalar *container*; mutsu has no
    /// per-attribute container, so the value carries the mixin — enough for
    /// method dispatch. This is a documented, pre-existing approximation; the
    /// native and interpreter constructors share this helper so they stay
    /// byte-identical.) Mixins are keyed `__mutsu_role__<Name>` with a marker
    /// value; method dispatch resolves the role from the registry by that name.
    pub(crate) fn apply_attribute_does_role_mixins(
        &mut self,
        class_key: &str,
        attrs: &mut HashMap<String, Value>,
    ) {
        let mro = self.class_mro(class_key);
        let does_attrs: Vec<(String, Vec<String>)> = self
            .registry()
            .class_attribute_does_roles
            .iter()
            .filter(|((c, _), _)| mro.contains(c))
            .map(|((_, a), roles)| (a.clone(), roles.clone()))
            .collect();
        for (attr_name, roles) in does_attrs {
            let Some(base) = attrs.get(&attr_name).cloned() else {
                continue;
            };
            let mut mixins = HashMap::new();
            for role in &roles {
                mixins.insert(format!("__mutsu_role__{}", role), Value::Bool(true));
            }
            attrs.insert(attr_name, Value::mixin(base, mixins));
        }
    }

    /// The `is Type` trait declared on an `@`/`%` attribute (`has @.a is Buf`),
    /// searched across the class's MRO so an inherited typed-container attribute
    /// is found. Returns the declared type name (e.g. `Buf`, `Array[Int]`).
    pub(crate) fn attribute_is_type_in_mro(&self, class_name: &str, attr: &str) -> Option<String> {
        for cls in self.mro_readonly(class_name) {
            if let Some(t) = self
                .registry()
                .class_attribute_is_types
                .get(&(cls.clone(), attr.to_string()))
            {
                return Some(t.clone());
            }
        }
        None
    }

    /// Coerce a provided constructor value to an `is Type` container attribute
    /// (`has @.a is Buf` / `has %.h is BagHash` / `has @.x is Array[Int]`). A
    /// parameterized array type (`Array[T]`) is built as pure data (typed array +
    /// element-type metadata + element type-check); any other container type is
    /// produced by dispatching to that type's `.new`, mirroring the uninitialized
    /// `is Type` path. A value that already matches the declared type is kept.
    pub(crate) fn coerce_value_to_is_type(
        &mut self,
        type_name: &str,
        sigil: char,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if self.type_matches_value(type_name, &value) {
            return Ok(value);
        }
        if let Some(inner) = type_name
            .strip_prefix("Array[")
            .or_else(|| type_name.strip_prefix("array["))
            .and_then(|s| s.strip_suffix(']'))
        {
            let inner = inner.trim().to_string();
            let items = match Self::coerce_attr_value_by_sigil(value, '@') {
                Value::Array(items, _) => (*items).clone(),
                other => crate::value::ArrayData::new(vec![other]),
            };
            if inner.starts_with(char::is_uppercase) {
                for it in &items {
                    if !matches!(it, Value::Nil) && !self.type_matches_value(&inner, it) {
                        return Err(crate::runtime::utils::type_check_element_typed_error(
                            "", &inner, it,
                        ));
                    }
                }
            }
            let mut arr = Value::real_array(items.items);
            arr = self.tag_container_metadata(
                arr,
                super::ContainerTypeInfo {
                    value_type: inner,
                    key_type: None,
                    declared_type: Some(type_name.to_string()),
                },
            );
            Ok(arr)
        } else {
            // A non-Array container type (Buf, BagHash, ...): dispatch to its
            // `.new` with the provided (sigil-coerced) value, just like the
            // uninitialized `is Type` path builds an empty one with `.new`.
            let arg = Self::coerce_attr_value_by_sigil(value, sigil);
            let type_obj = Value::Package(crate::symbol::Symbol::intern(type_name));
            self.call_method_with_values(type_obj, "new", vec![arg])
        }
    }
}
