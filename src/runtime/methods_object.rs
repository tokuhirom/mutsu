use super::*;
use crate::symbol::Symbol;
use num_traits::ToPrimitive;

/// Whether an ISO 8601 datetime string carries a numeric timezone offset
/// (`+HHMM` / `-HH:MM`, including `+0000`). The bare `Z` UTC designator is not
/// a numeric offset. Only the time portion (after `T`/`t`) is inspected so the
/// `-` separators in the date part are not mistaken for an offset sign.
fn string_has_numeric_tz_offset(s: &str) -> bool {
    match s.find(['T', 't']) {
        Some(pos) => {
            let time_part = &s[pos + 1..];
            time_part.contains('+') || time_part.contains('-')
        }
        None => false,
    }
}

fn is_datetime_constructor_named_arg(key: &str) -> bool {
    matches!(
        key,
        "year" | "month" | "day" | "hour" | "minute" | "second" | "timezone" | "date" | "formatter"
    )
}

fn is_normalized_datetime_subclass_ctor_args(args: &[Value]) -> bool {
    if !matches!(
        args.first(),
        Some(Value::Instance { class_name, .. }) if class_name == "DateTime"
    ) {
        return false;
    }
    args.iter()
        .skip(1)
        .all(|arg| matches!(arg, Value::Pair(key, _) if !is_datetime_constructor_named_arg(key)))
}

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
    fn is_simple_native_ctor_constraint(tc: &str) -> bool {
        tc.starts_with(char::is_uppercase) && !tc.contains('(') && !tc.contains('[')
    }

    /// A coercion-type constraint (`Int()`, `Int(Str)`) whose target is a
    /// built-in scalar type the native constructor can coerce to without running
    /// user-defined code. A user-class coercion target (with a `COERCE` method)
    /// keeps the interpreter, which runs that method. Coercion itself is routed
    /// through the shared `coerce_value_for_constraint`, so a native-coerced value
    /// is identical to the interpreter's.
    fn is_native_coercion_ctor_constraint(tc: &str) -> bool {
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
    fn native_scalar_default(tc: &str) -> Option<Value> {
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

    /// Returns `None` to fall through to the full constructor dispatch when a
    /// case needs the interpreter's richer semantics: a provided value that does
    /// not match its attribute's type constraint (interpreter raises
    /// `X::TypeCheck::Assignment`), a typed attribute left uninitialized (its
    /// default is the *type object*, which the interpreter synthesizes), or a
    /// typed default whose value does not match. Otherwise builds the instance as
    /// pure data and returns `Some(Ok(..))`.
    fn build_native_default_instance(
        &mut self,
        class_name: Symbol,
        cn_resolved: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // The default `.new` accepts only named arguments (`method new(*%_)`); a
        // positional argument is an error (`X::Constructor::Positional`). The
        // native path only consumes `Pair` (named) args, so a positional would be
        // silently ignored — fall through to the interpreter, whose `.new`
        // signature binding raises the proper error. (This also covers a BUILD
        // with a positional parameter: `.new` strips positionals before BUILD, so
        // `Foo.new('x', :y)` must die rather than feed 'x' to BUILD.)
        if args.iter().any(|a| !matches!(a, Value::Pair(..))) {
            return None;
        }
        let class_attrs = self.collect_class_attributes(cn_resolved);
        // Attribute type constraints (MRO-wide) live in `attribute_types`, not in
        // the `ClassAttributeDef` tuple's constraint slot — and the gate already
        // guaranteed every one is a simple `type_matches_value`-checkable class
        // type. Owned map, so it can be read while `self` is borrowed mutably.
        let type_constraints = self.collect_attribute_type_constraints(cn_resolved);

        let sigil_of = |name: &str| -> char {
            class_attrs
                .iter()
                .find(|(n, ..)| n == name)
                .map(|(_, _, _, _, _, s, _)| *s)
                .unwrap_or('$')
        };

        // A custom BUILD replaces the default named-argument → attribute binding:
        // with a BUILD present, a provided `:attr(value)` is NOT auto-assigned to
        // `$!attr` — only BUILD decides what gets set (via its signature's
        // `:$!attr` params or explicit assignment in its body). So
        // `class P { has $.x = 42; submethod BUILD() {} }; P.new(:666x).x` is 42.
        // Skip the auto-assignment loop when the class has a BUILD; defaults are
        // still filled below and BUILD runs afterward.
        let has_build = self.mro_readonly(cn_resolved).iter().any(|cls| {
            self.registry()
                .classes
                .get(cls)
                .is_some_and(|cd| cd.methods.contains_key("BUILD"))
        });

        let mut attrs = HashMap::new();
        for arg in args {
            if has_build {
                break;
            }
            if let Value::Pair(key, val) = arg
                && self.is_attribute_buildable(cn_resolved, key)
            {
                match sigil_of(key) {
                    '@' | '%' => {
                        // Coerce exactly as the interpreter's shared helper does
                        // (List/Range -> Array, array-of-Pairs -> Hash, …).
                        attrs.insert(
                            key.clone(),
                            Self::coerce_attr_value_by_sigil(*val.clone(), sigil_of(key)),
                        );
                    }
                    _ => {
                        // A provided value that does not already match its
                        // attribute's type constraint needs the interpreter
                        // (coercion or a proper X::TypeCheck::Assignment) — fall
                        // through. Native-typed attributes (`int`/`num`/`str`)
                        // are exempt: the interpreter stores the provided value
                        // as-is without coercing or type-checking it, so we do
                        // the same and never fall through on them. Coercion-typed
                        // attributes (`Int()`) are also exempt — their provided
                        // value is coerced in the final pass below, not checked.
                        if let Some(c) = type_constraints.get(key)
                            && Self::native_scalar_default(c).is_none()
                            && !Self::is_native_coercion_ctor_constraint(c)
                            && !self.type_matches_value(c, val)
                        {
                            return None;
                        }
                        attrs.insert(key.clone(), *val.clone());
                    }
                }
            }
        }
        // A required attribute not supplied as a named arg. Without a BUILD it
        // can never be set, so raise `X::Attribute::Required` now — exactly the
        // interpreter's pre-defaults required check. With a BUILD, defer: BUILD
        // may set it, and the post-BUILD required check below enforces it. The
        // required flag is tuple field 4 (`Option<Option<String>>`: `Some` when
        // `is required`, the inner value being the optional reason); field 3 is
        // `is_rw`, which must NOT gate construction (an unprovided `is rw`
        // attribute just gets its normal default, so it stays native).
        if !has_build {
            for (attr_name, _, _, _is_rw, is_required, _, _) in &class_attrs {
                if let Some(reason) = is_required
                    && !attrs.contains_key(attr_name)
                {
                    let attr_full_name = format!("$!{}", attr_name);
                    return Some(Err(RuntimeError::attribute_required(
                        &attr_full_name,
                        reason.as_deref(),
                    )));
                }
            }
        }
        // A class-typed `$` attribute that is neither provided nor defaulted is
        // stored as `Nil` (exactly as the interpreter does); the attribute
        // accessor synthesizes the declared *type object* (e.g. `Int`) from the
        // stored `Nil` at read time, so the native instance behaves identically.
        // The empty-fill loop below stores that `Nil` (native-typed `int`/`num`/
        // `str` attributes get their native zero there instead). A `where` /
        // `:D`/`:U` / element type check all skip a `Nil` value, matching the
        // interpreter, so nothing further is needed here.
        // An `@`/`%` attribute with a default expression may be shaped (the
        // shape is encoded in the default, e.g. `has @.a[2]`) or otherwise need
        // the interpreter's construction — fall through whether or not it was
        // provided (a shaped attribute must stay shaped even when assigned).
        // Only the empty-default `@`/`%` case (handled below) is native.
        for (_attr_name, _, default_expr, _, _, sigil, _) in &class_attrs {
            if matches!(sigil, '@' | '%') && default_expr.is_some() {
                return None;
            }
        }

        // Fill defaults for missing attributes. Mirror the interpreter's
        // constructor structure: a *literal* default — the common case, since the
        // parser hands every native-typed attribute a `Literal` zero — takes a
        // fast path with no `self`/env binding, while a non-literal default binds
        // `self` and the already-initialized `$!attr`/`$.attr` (so expressions
        // like `has $.c = $!a + $!b` resolve) only for the duration of that one
        // evaluation. Rebuilding `self` from the current `attrs` inside the
        // non-literal branch keeps earlier (literal or computed) defaults visible
        // to later ones without paying per-construction env churn — the absence of
        // that fast path made a 20k-iteration native-attr loop time out.
        let mut eval_error: Option<RuntimeError> = None;
        let mut typed_default_mismatch = false;
        for (attr_name, _is_public, default_expr, _, _, sigil, _) in &class_attrs {
            if attrs.contains_key(attr_name) {
                continue;
            }
            match default_expr {
                // Fast path: a literal default needs no evaluation or binding.
                // The interpreter stores it without a type check, so we do too.
                Some(Expr::Literal(lit_val)) => {
                    attrs.insert(
                        attr_name.clone(),
                        Self::coerce_attr_value_by_sigil(lit_val.clone(), *sigil),
                    );
                }
                Some(expr) => {
                    // Bind `self` and the already-set attributes, switch to the
                    // class package for class-scoped sub lookups, evaluate, then
                    // restore everything — per the interpreter's per-default setup.
                    let temp_self = Value::make_instance(class_name, attrs.clone());
                    let old_self = self.env.get("self").cloned();
                    let old_anon = self.env.get("__ANON_STATE__").cloned();
                    self.env.insert("self".to_string(), temp_self.clone());
                    self.env.insert("__ANON_STATE__".to_string(), temp_self);
                    let mut saved_attr_env: Vec<(String, Option<Value>)> = Vec::new();
                    for (a_name, a_val) in &attrs {
                        let bang = format!("!{}", a_name);
                        let dot = format!(".{}", a_name);
                        saved_attr_env.push((bang.clone(), self.env.get(&bang).cloned()));
                        saved_attr_env.push((dot.clone(), self.env.get(&dot).cloned()));
                        self.env.insert(bang, a_val.clone());
                        self.env.insert(dot, a_val.clone());
                    }
                    let saved_package = self.current_package();
                    if self.has_class_scoped_subs(cn_resolved) {
                        self.set_current_package(cn_resolved.to_string());
                    }
                    let result = self.eval_block_value(&[crate::ast::Stmt::Expr(expr.clone())]);
                    self.set_current_package(saved_package);
                    for (key, old_val) in saved_attr_env {
                        match old_val {
                            Some(v) => {
                                self.env.insert(key, v);
                            }
                            None => {
                                self.env.remove(&key);
                            }
                        }
                    }
                    match old_self {
                        Some(v) => {
                            self.env.insert("self".to_string(), v);
                        }
                        None => {
                            self.env.remove("self");
                        }
                    }
                    match old_anon {
                        Some(v) => {
                            self.env.insert("__ANON_STATE__".to_string(), v);
                        }
                        None => {
                            self.env.remove("__ANON_STATE__");
                        }
                    }
                    let val = match result {
                        Ok(val) => val,
                        Err(e) => {
                            eval_error = Some(e);
                            break;
                        }
                    };
                    // A non-native default whose value does not match the
                    // attribute's type constraint needs the interpreter — fall
                    // through. Native-typed attributes are exempt (their default
                    // is stored without a type check); coercion-typed attributes
                    // are exempt too (their default is coerced in the final pass).
                    if let Some(c) = type_constraints.get(attr_name)
                        && Self::native_scalar_default(c).is_none()
                        && !Self::is_native_coercion_ctor_constraint(c)
                        && !self.type_matches_value(c, &val)
                    {
                        typed_default_mismatch = true;
                        break;
                    }
                    attrs.insert(
                        attr_name.clone(),
                        Self::coerce_attr_value_by_sigil(val, *sigil),
                    );
                }
                None => {
                    // Uninitialized: `@` -> empty Array, `%` -> empty Hash. For
                    // `$`: a native-typed scalar gets its native zero (`int` -> 0,
                    // `num` -> 0e0, `str` -> ""); an untyped `$` gets Nil. A
                    // class-typed `$` cannot reach here — it fell through above.
                    let empty = match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash(HashMap::new()),
                        _ => type_constraints
                            .get(attr_name)
                            .and_then(|c| Self::native_scalar_default(c))
                            .unwrap_or(Value::Nil),
                    };
                    attrs.insert(attr_name.clone(), empty);
                }
            }
        }
        if let Some(e) = eval_error {
            return Some(Err(e));
        }
        if typed_default_mismatch {
            return None;
        }
        // Final pass: coerce every coercion-typed `$` attribute (`has Int() $.x`)
        // through its target type. This uniformly covers a provided value, a
        // literal/computed default, and the bare type-object default of an
        // uninitialized attribute (which coerces to itself). The shared
        // `coerce_value_for_constraint` is the exact path the interpreter uses, so
        // the result is identical; the gate already excluded user-class targets,
        // so only built-in coercion logic runs here.
        for (attr_name, _, _, _, _, sigil, _) in &class_attrs {
            if *sigil != '$' {
                continue;
            }
            if let Some(tc) = type_constraints.get(attr_name)
                && Self::is_native_coercion_ctor_constraint(tc)
                && let Some(val) = attrs.remove(attr_name)
            {
                let coerced = self.coerce_value_for_constraint(tc, val);
                attrs.insert(attr_name.clone(), coerced);
            }
        }
        // Tag typed `@`/`%` attributes (`has Int @.nums`) with element-type
        // metadata and type-check their elements, exactly as `dispatch_new` does
        // (shared `finalize_typed_container_attr`). A failing element raises the
        // same `X::TypeCheck::Assignment` the interpreter would, so the native
        // path is byte-identical. The gate already excluded `is Type` containers
        // and native/parametric element types, so only plain class elements reach
        // here. Done against the final `attrs` Arcs that move into the instance.
        for (attr_name, _, _, _, _, sigil, _) in &class_attrs {
            if !matches!(sigil, '@' | '%') {
                continue;
            }
            let Some(elem_type) = type_constraints.get(attr_name).cloned() else {
                continue;
            };
            if let Some(val) = attrs.get(attr_name).cloned() {
                match self.finalize_typed_container_attr(attr_name, *sigil, &elem_type, val) {
                    // Hashes embed the element type in `HashData`, so store the
                    // tagged value back into the attrs that move into the instance.
                    Ok(tagged) => {
                        attrs.insert(attr_name.clone(), tagged);
                    }
                    Err(e) => return Some(Err(e)),
                }
            }
        }
        // Add alias metadata for `has $x` (no twigil) attributes
        self.add_alias_attribute_metadata(cn_resolved, &mut attrs);
        // The gate (`is_native_default_constructible`) allows BUILD/TWEAK-only
        // classes; the instance is assembled (defaults first) at this point, so
        // running BUILD then TWEAK here matches the full `.new` path (which also
        // applies defaults before BUILD). `has_build` was computed above (it
        // gates the named-arg auto-assignment); the TWEAK check is cheap so the
        // common no-submethod case pays nothing extra.
        let has_tweak = self.mro_readonly(cn_resolved).iter().any(|cls| {
            self.registry()
                .classes
                .get(cls)
                .is_some_and(|cd| cd.methods.contains_key("TWEAK"))
        });
        // Enforce `where` constraints at attribute-assignment time, i.e. *before*
        // BUILD/TWEAK run (matches the interpreter's pre-BUILD enforcement): a
        // provided/defaulted value that fails its `where` is rejected here, and a
        // later BUILD/TWEAK that would "fix" it never runs. `class_attrs` is the
        // same `ClassAttributeDef` slice the interpreter uses.
        let has_where = class_attrs.iter().any(|(.., where_c)| where_c.is_some());
        if has_where
            && let Err(e) =
                self.enforce_attribute_where_constraints(cn_resolved, &class_attrs, &attrs)
        {
            return Some(Err(e));
        }
        // Enforce definedness smileys (`:D`/`:U`) at the same point as `where`
        // (post-assembly, pre-BUILD/TWEAK): a provided/defaulted value whose
        // definedness does not match its `:D`/`:U` smiley is rejected here, exactly
        // as the full constructor does (`enforce_attribute_smiley_constraints`,
        // which itself skips `is required` attributes). `:_` imposes no constraint.
        let has_smiley = self.mro_readonly(cn_resolved).iter().any(|cls| {
            self.registry()
                .classes
                .get(cls)
                .is_some_and(|cd| !cd.attribute_smileys.is_empty())
        });
        if has_smiley && let Err(e) = self.enforce_attribute_smiley_constraints(cn_resolved, &attrs)
        {
            return Some(Err(e));
        }
        if has_build {
            // Pass the original constructor args so `submethod BUILD(:$x)` binds
            // them. A `fail` inside BUILD yields a `Failure` instance to return.
            match self.run_build_phase(class_name, attrs, args) {
                Ok(Ok(updated)) => attrs = updated,
                Ok(Err(failure)) => return Some(Ok(failure)),
                Err(e) => return Some(Err(e)),
            }
            // Enforce required attributes after BUILD ran (BUILD may set them),
            // exactly where the full constructor does its post-BUILD required
            // check. A still-unset attribute (`None` or `Nil`) raises
            // `X::Attribute::Required` with the same message and reason.
            for (attr_name, _, _, _, is_required, _, _) in &class_attrs {
                if let Some(reason) = is_required {
                    let is_set = !matches!(attrs.get(attr_name), Some(Value::Nil) | None);
                    if !is_set {
                        let attr_full_name = format!("$!{}", attr_name);
                        return Some(Err(RuntimeError::attribute_required(
                            &attr_full_name,
                            reason.as_deref(),
                        )));
                    }
                }
            }
            // Re-check smileys after BUILD but BEFORE TWEAK, exactly where the full
            // constructor does (its post-BUILD pass) — a BUILD that mutates a
            // `:D`/`:U` attribute into a definedness violation is rejected
            // identically. The interpreter does NOT re-check smileys after TWEAK
            // (it relies on the assignment-time check there, which mutsu's
            // interpreter does not currently perform), so neither do we — a TWEAK
            // that violates a smiley is left untouched to match the baseline.
            if has_smiley
                && let Err(e) = self.enforce_attribute_smiley_constraints(cn_resolved, &attrs)
            {
                return Some(Err(e));
            }
        }
        if has_tweak {
            // Pass the original constructor args so `submethod TWEAK(:$y)` binds
            // them, matching the full `.new` path.
            match self.run_tweak_phase(class_name, attrs, args) {
                Ok(updated) => attrs = updated,
                Err(e) => return Some(Err(e)),
            }
        }
        // Re-check `where` after BUILD/TWEAK: the full constructor enforces
        // constraints again post-construction, so a BUILD/TWEAK that mutates an
        // attribute into a `where` violation is rejected identically.
        if has_where
            && (has_build || has_tweak)
            && let Err(e) =
                self.enforce_attribute_where_constraints(cn_resolved, &class_attrs, &attrs)
        {
            return Some(Err(e));
        }
        // Mix `has $.x does Role` roles into the final attribute values — the
        // full constructor does this last (after BUILD/TWEAK), so do the same.
        self.apply_attribute_does_role_mixins(cn_resolved, &mut attrs);
        Some(Ok(Value::make_instance(class_name, attrs)))
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

    /// True for the buf/blob type names `build_native_buf_value` constructs as a
    /// pure byte overlay — every `Buf`/`Blob` flavour (`Buf`, `buf8`..`buf64`,
    /// `Blob`, `blob8`..`blob64`, and the parametric `Buf[uintN]`/`Blob[uintN]`
    /// forms) EXCEPT `utf8`/`utf16`, which `dispatch_new` builds via a different
    /// (unmasked) arm and must keep on the interpreter. Used by the VM to route
    /// `Buf.new(...)` to the native builder instead of the generic dispatch.
    pub(crate) fn is_native_buf_constructible(cn: &str) -> bool {
        cn != "utf8" && cn != "utf16" && crate::runtime::utils::is_buf_or_blob_class(cn)
    }

    /// Build a `Buf`/`Blob` instance from `.new` arguments as pure data: flatten
    /// each argument into a byte sequence, mask every value to the element width
    /// inferred from the type name (`uint8`/`16`/`32`/`64`), and tag the
    /// canonical parametric class name (`buf8` -> `Buf[uint8]`, …). Touches no
    /// env / registry, so the VM constructs these directly (see
    /// `is_native_buf_constructible`) without entering generic dispatch, while
    /// `dispatch_new`'s Buf/Blob arm calls the same helper — keeping the two
    /// byte-identical. `utf8`/`utf16` are intentionally NOT handled here.
    pub(crate) fn build_native_buf_value(class_name: Symbol, args: &[Value]) -> Value {
        let cn = class_name.resolve();
        let raw_vals: Vec<Value> = args
            .iter()
            .flat_map(|a| match a {
                Value::Int(i) => vec![Value::Int(*i)],
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) => items.to_vec(),
                Value::Slip(items) => items.to_vec(),
                Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                Value::RangeExcl(start, end) => (*start..*end).map(Value::Int).collect(),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Buf"
                    || class_name == "Blob"
                    || class_name == "utf8"
                    || class_name == "utf16"
                    || class_name.resolve().starts_with("Buf[")
                    || class_name.resolve().starts_with("Blob[")
                    || class_name.resolve().starts_with("buf")
                    || class_name.resolve().starts_with("blob") =>
                {
                    if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                        items.to_vec()
                    } else {
                        Vec::new()
                    }
                }
                Value::BigInt(_) => vec![a.clone()],
                other => vec![Value::Int(to_int(other))],
            })
            .collect();
        // Mask values to unsigned range based on element size. For uint64, use
        // BigInt-aware conversion to preserve values > i64::MAX.
        let byte_vals: Vec<Value> = raw_vals
            .into_iter()
            .map(|v| {
                if cn.contains("64") {
                    let u = match &v {
                        Value::BigInt(n) => {
                            use num_traits::ToPrimitive;
                            n.as_ref().to_u64().unwrap_or(to_int(&v) as u64)
                        }
                        _ => to_int(&v) as u64,
                    };
                    Value::Int(u as i64)
                } else if cn.contains("32") {
                    Value::Int(to_int(&v) as u32 as i64)
                } else if cn.contains("16") {
                    Value::Int(to_int(&v) as u16 as i64)
                } else {
                    Value::Int(to_int(&v) as u8 as i64)
                }
            })
            .collect();
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(byte_vals));
        // Normalize short buf/blob names to canonical forms.
        let canonical_name = match cn.as_str() {
            "buf8" => Symbol::intern("Buf[uint8]"),
            "buf16" => Symbol::intern("Buf[uint16]"),
            "buf32" => Symbol::intern("Buf[uint32]"),
            "buf64" => Symbol::intern("Buf[uint64]"),
            "blob8" => Symbol::intern("Blob[uint8]"),
            "blob16" => Symbol::intern("Blob[uint16]"),
            "blob32" => Symbol::intern("Blob[uint32]"),
            "blob64" => Symbol::intern("Blob[uint64]"),
            _ => class_name,
        };
        Value::make_instance(canonical_name, attrs)
    }

    /// Build a `utf8`/`utf16` instance from `.new` arguments as pure data:
    /// flatten each argument into a code-unit sequence (no masking) and store it
    /// as the `bytes` attribute. Unlike `build_native_buf_value` this keeps the
    /// literal class name and drops arguments it cannot turn into code units
    /// (the interpreter's `utf8`/`utf16` arm did the same), so the two stay
    /// byte-identical.
    pub(crate) fn build_native_utf_value(class_name: Symbol, args: &[Value]) -> Value {
        let elems: Vec<Value> = args
            .iter()
            .flat_map(|a| match a {
                Value::Int(i) => vec![Value::Int(*i)],
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) | Value::Slip(items) => items.to_vec(),
                Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                Value::RangeExcl(start, end) => (*start..*end).map(Value::Int).collect(),
                Value::Instance {
                    class_name: cn,
                    attributes: ia,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&cn.resolve()) => {
                    if let Some(Value::Array(items, ..)) = ia.as_map().get("bytes") {
                        items.to_vec()
                    } else {
                        vec![]
                    }
                }
                _ => vec![],
            })
            .collect();
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(elems));
        Value::make_instance(class_name, attrs)
    }

    /// Build a `Uni` value from `.new` arguments as pure data: flatten each
    /// argument into a sequence of codepoints and collect them as a string.
    /// (Flattening now also covers `Range`s — `Uni.new(65..67)` -> "ABC" — which
    /// the old interpreter arm missed, leaving it to stringify the whole Range;
    /// the shared helper fixes that to match raku and the sibling `utf8` arm.)
    pub(crate) fn build_native_uni_value(args: &[Value]) -> Value {
        let mut flat_args: Vec<Value> = Vec::new();
        for a in args {
            match a {
                Value::Array(items, ..) => flat_args.extend(items.iter().cloned()),
                Value::Seq(items) | Value::Slip(items) => {
                    flat_args.extend(items.iter().cloned());
                }
                Value::Range(start, end) => {
                    flat_args.extend((*start..=*end).map(Value::Int));
                }
                Value::RangeExcl(start, end) => {
                    flat_args.extend((*start..*end).map(Value::Int));
                }
                other => flat_args.push(other.clone()),
            }
        }
        let text: String = flat_args
            .iter()
            .filter_map(|a| {
                let cp = match a {
                    Value::Int(i) => *i as u32,
                    Value::Num(f) => *f as u32,
                    other => other.to_string_value().parse::<u32>().unwrap_or(0),
                };
                char::from_u32(cp)
            })
            .collect();
        Value::uni(String::new(), text)
    }

    /// Coerce one `Complex.new` positional argument to its `f64` component
    /// (Int/Num/Rat directly, anything else via `to_float_value`). Mirrors the
    /// interpreter's `Complex` constructor arm exactly.
    fn complex_component(v: Option<&Value>) -> f64 {
        match v {
            Some(Value::Int(i)) => *i as f64,
            Some(Value::Num(f)) => *f,
            Some(Value::Rat(n, d)) if *d != 0 => *n as f64 / *d as f64,
            Some(v) => to_float_value(v).unwrap_or(0.0),
            _ => 0.0,
        }
    }

    /// Build a `Complex` from `.new` arguments as pure data: `Complex.new(re,
    /// im)` -> `re + im*i`, each component coerced via `complex_component`
    /// (missing components default to `0`). Behaviour matches the interpreter
    /// (which is more lenient than raku — raku requires exactly two `Real`
    /// arguments, mutsu accepts 0/1/2 — a pre-existing divergence preserved here).
    pub(crate) fn build_native_complex_value(args: &[Value]) -> Value {
        Value::Complex(
            Self::complex_component(args.first()),
            Self::complex_component(args.get(1)),
        )
    }

    /// Build an `Int` from `.new(value)` as pure data: `to_int`-coerce the
    /// argument (default `0`). A type-object argument is an error, matching the
    /// interpreter's basic-type `.new` arm.
    pub(crate) fn build_native_int_value(args: &[Value]) -> Result<Value, RuntimeError> {
        if matches!(args.first(), Some(Value::Package(_))) {
            return Err(RuntimeError::new("Cannot convert type object to Int"));
        }
        Ok(Value::Int(args.first().map_or(0, crate::runtime::to_int)))
    }

    /// Build a `Num` from `.new(value)` as pure data: coerce the argument to
    /// f64 (default `0e0`). A type-object or non-coercible argument is an error,
    /// matching the interpreter's basic-type `.new` arm.
    pub(crate) fn build_native_num_value(args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(arg) = args.first() {
            if matches!(arg, Value::Package(_)) {
                return Err(RuntimeError::new(
                    "Cannot coerce to Num: no .Num method found",
                ));
            }
            match crate::runtime::to_float_value(arg) {
                Some(f) => Ok(Value::Num(f)),
                None => Err(RuntimeError::new(
                    "Cannot coerce to Num: no .Num method found",
                )),
            }
        } else {
            Ok(Value::Num(0.0))
        }
    }

    /// Build a `Rat` from `.new(numerator, denominator)` as pure data
    /// (defaults `0/1`). A `BigInt` argument routes through `make_big_rat` to
    /// avoid truncation; otherwise the components are `to_int`-coerced.
    pub(crate) fn build_native_rat_value(args: &[Value]) -> Value {
        use num_bigint::BigInt;
        let has_big = args.iter().take(2).any(|v| matches!(v, Value::BigInt(_)));
        if has_big {
            let a = match args.first() {
                Some(Value::BigInt(bi)) => (**bi).clone(),
                Some(v) => BigInt::from(to_int(v)),
                None => BigInt::from(0),
            };
            let b = match args.get(1) {
                Some(Value::BigInt(bi)) => (**bi).clone(),
                Some(v) => BigInt::from(to_int(v)),
                None => BigInt::from(1),
            };
            return crate::value::make_big_rat(a, b);
        }
        let a = args.first().map(to_int).unwrap_or(0);
        let b = args.get(1).map(to_int).unwrap_or(1);
        make_rat(a, b)
    }

    /// Build a `FatRat` from `.new(numerator, denominator)` as pure data
    /// (defaults `0/1`), always via the BigInt path (`make_big_fat_rat`).
    pub(crate) fn build_native_fatrat_value(args: &[Value]) -> Value {
        use crate::value::make_big_fat_rat;
        use num_bigint::BigInt;
        let a = match args.first() {
            Some(Value::BigInt(bi)) => (**bi).clone(),
            Some(v) => BigInt::from(to_int(v)),
            None => BigInt::from(0),
        };
        let b = match args.get(1) {
            Some(Value::BigInt(bi)) => (**bi).clone(),
            Some(v) => BigInt::from(to_int(v)),
            None => BigInt::from(1),
        };
        match make_big_fat_rat(a, b) {
            Value::Rat(n, d) => Value::FatRat(n, d),
            Value::BigRat(n, d) => Value::BigRat(n, d),
            other => other,
        }
    }

    /// Build a `Pair` from `.new(:key, :value)` or `.new(key, value)` as pure
    /// data. A `Str` key uses `Pair` (string-keyed); any other key type uses
    /// `ValuePair`, mirroring the `=>` operator and positional `Pair.new`.
    pub(crate) fn build_native_pair_value(args: &[Value]) -> Value {
        let mut named_key: Option<Value> = None;
        let mut named_value: Option<Value> = None;
        let mut positional = Vec::new();
        for a in args {
            match a {
                Value::Pair(k, v) if k == "key" => named_key = Some((**v).clone()),
                Value::Pair(k, v) if k == "value" => named_value = Some((**v).clone()),
                _ => positional.push(a.clone()),
            }
        }
        let (key, value) = if named_key.is_some() || named_value.is_some() {
            (
                named_key.unwrap_or(Value::Nil),
                named_value.unwrap_or(Value::Nil),
            )
        } else {
            (
                positional.first().cloned().unwrap_or(Value::Nil),
                positional.get(1).cloned().unwrap_or(Value::Nil),
            )
        };
        match &key {
            Value::Str(_) => Value::Pair(key.to_string_value(), Box::new(value)),
            _ => Value::ValuePair(Box::new(key), Box::new(value)),
        }
    }

    /// Build an `IterationBuffer` from `.new(...)` as pure data: flatten each
    /// argument (an Array/Seq/Slip is spread, another IterationBuffer's items
    /// are taken, anything else is a single element) into the buffer's
    /// `__mutsu_iterationbuffer_items` array.
    pub(crate) fn build_native_iterationbuffer_value(class_name: Symbol, args: &[Value]) -> Value {
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Array(vals, ..) => items.extend(vals.iter().cloned()),
                Value::Seq(vals) | Value::Slip(vals) => items.extend(vals.iter().cloned()),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "IterationBuffer" => {
                    match attributes.as_map().get("__mutsu_iterationbuffer_items") {
                        Some(Value::Array(vals, ..)) => items.extend(vals.iter().cloned()),
                        Some(Value::Seq(vals)) | Some(Value::Slip(vals)) => {
                            items.extend(vals.iter().cloned())
                        }
                        _ => {}
                    }
                }
                other => items.push(other.clone()),
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert(
            "__mutsu_iterationbuffer_items".to_string(),
            Value::real_array(items),
        );
        Value::make_instance(class_name, attrs)
    }

    /// Build a `Date` from `.new` arguments as pure data: parse named
    /// (`year`/`month`/`day`/`formatter`) and positional args (a date string, a
    /// `DateTime`/`Instant` to take the date of, or `y, m, d`), validate, and
    /// construct the `Date` value (with any `:formatter` embedded). Returns the
    /// date plus the formatter that still needs *rendering* — rendering runs a
    /// user `Callable` (`render_date_formatter`, the only `self`-dependent step),
    /// so a `Some` formatter makes the VM fall through to the interpreter while
    /// the common no-formatter case stays native. All the parsing/validation
    /// (`temporal::*`) is free of env/registry/self. Shared by both paths.
    #[allow(clippy::type_complexity)]
    pub(crate) fn build_native_date(
        args: &[Value],
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        use crate::builtins::methods_0arg::temporal;
        let mut year: i64 = 1970;
        let mut month: i64 = 1;
        let mut day: i64 = 1;
        let mut positional = Vec::new();
        let mut has_named = false;
        let mut formatter: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Pair(key, value) => match key.as_str() {
                    "year" => {
                        year = to_int(value);
                        has_named = true;
                    }
                    "month" => {
                        month = to_int(value);
                        has_named = true;
                    }
                    "day" => {
                        day = to_int(value);
                        has_named = true;
                    }
                    "formatter" => {
                        formatter = Some(*value.clone());
                    }
                    _ => {}
                },
                other => positional.push(other),
            }
        }
        // Positional args: a date string, a DateTime/Instant, or y/m/d.
        if let Some(v) = positional.first() {
            match v {
                Value::Str(s) if positional.len() == 1 => {
                    let (y, m, d) = temporal::parse_date_string(s)?;
                    year = y;
                    month = m;
                    day = d;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "DateTime" => {
                    let (y, m, d, _, _, _, _) = temporal::datetime_attrs(&attributes.as_map());
                    year = y;
                    month = m;
                    day = d;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" => {
                    let tai = attributes
                        .as_map()
                        .get("value")
                        .and_then(crate::runtime::to_float_value)
                        .unwrap_or(0.0);
                    let posix = temporal::instant_to_posix(tai);
                    let epoch_days = (posix / 86400.0).floor() as i64;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                }
                _ => {
                    year = to_int(v);
                    if let Some(v2) = positional.get(1) {
                        month = to_int(v2);
                    }
                    if let Some(v3) = positional.get(2) {
                        day = to_int(v3);
                    }
                }
            }
        } else if !has_named {
            return Err(RuntimeError::new("Date.new requires arguments"));
        }
        temporal::validate_date(year, month, day)?;
        let date = temporal::make_date_with_formatter(year, month, day, formatter.clone());
        Ok((date, formatter))
    }

    /// Build a `DateTime` from `.new` arguments as pure data: parse named
    /// (`year`..`second`/`timezone`/`date`/`formatter`) and positional args (a
    /// datetime string, a posix epoch as Int/BigInt/Num/Rat, a `Date`/`Instant`,
    /// or `y, mo, d, h, mi, s`), validate, and construct the `DateTime`. Returns
    /// the datetime plus the formatter that still needs *rendering* (a user
    /// `Callable` — `eval_call_on_value`, the only `self`-dependent step), so a
    /// `Some` formatter makes the VM fall through while the common no-formatter
    /// case stays native. All parsing/validation (`temporal::*`) is self-free.
    #[allow(clippy::type_complexity)]
    pub(crate) fn build_native_datetime(
        args: &[Value],
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        use crate::builtins::methods_0arg::temporal;
        let mut year: i64 = 1970;
        let mut month: i64 = 1;
        let mut day: i64 = 1;
        let mut hour: i64 = 0;
        let mut minute: i64 = 0;
        let mut second: f64 = 0.0;
        let mut timezone: i64 = 0;
        let mut timezone_set = false;
        let mut formatter: Option<Value> = None;
        let mut has_component_named = false;
        let mut positional = Vec::new();
        let mut has_named = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) => match key.as_str() {
                    "year" => {
                        year = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "month" => {
                        month = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "day" => {
                        day = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "hour" => {
                        hour = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "minute" => {
                        minute = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "second" => {
                        second = to_float_value(value).unwrap_or(0.0);
                        has_named = true;
                        has_component_named = true;
                    }
                    "timezone" => {
                        timezone = to_int(value);
                        timezone_set = true;
                        has_named = true;
                    }
                    "date" => {
                        if let Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } = value.as_ref()
                            && class_name == "Date"
                        {
                            let (y, m, d) = temporal::date_attrs(&attributes.as_map());
                            year = y;
                            month = m;
                            day = d;
                            has_named = true;
                            has_component_named = true;
                        }
                    }
                    "formatter" => {
                        formatter = Some(*value.clone());
                        has_named = true;
                    }
                    _ => {}
                },
                other => positional.push(other),
            }
        }
        if has_component_named && !positional.is_empty() {
            return Err(RuntimeError::new(
                "DateTime.new cannot mix positional and component named arguments",
            ));
        }
        if positional.len() >= 6 {
            year = to_int(positional[0]);
            month = to_int(positional[1]);
            day = to_int(positional[2]);
            hour = to_int(positional[3]);
            minute = to_int(positional[4]);
            second = to_float_value(positional[5]).unwrap_or(0.0);
            has_named = true;
        } else if let Some(v) = positional.first() {
            match v {
                Value::Str(s) => {
                    if timezone_set && string_has_numeric_tz_offset(s) {
                        let message =
                            "DateTime.new(Str): :timezone argument not allowed with a timestamp offset"
                                .to_string();
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(message.clone()));
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::DateTime::TimezoneClash"),
                            attrs,
                        );
                        let mut err = RuntimeError::new(message);
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    let (y, mo, d, h, mi, sec, tz) = temporal::parse_datetime_string(s)?;
                    year = y;
                    month = mo;
                    day = d;
                    hour = h;
                    minute = mi;
                    second = sec;
                    if !timezone_set {
                        timezone = tz;
                    }
                    has_named = true;
                }
                Value::Int(epoch) => {
                    let total = *epoch as f64 + timezone as f64;
                    let total_i = total.floor() as i64;
                    let frac = total - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64 + frac;
                    has_named = true;
                }
                Value::BigInt(epoch) => {
                    let total = epoch.as_ref().clone() + num_bigint::BigInt::from(timezone);
                    let secs_per_day = num_bigint::BigInt::from(86_400i64);
                    let day_secs_big = ((&total % &secs_per_day) + &secs_per_day) % &secs_per_day;
                    let epoch_days_big = (&total - &day_secs_big) / &secs_per_day;
                    let epoch_days = epoch_days_big.to_i64().ok_or_else(|| {
                        RuntimeError::new("X::DateTime::Range: epoch day out of range".to_string())
                    })?;
                    let day_secs = day_secs_big.to_i64().ok_or_else(|| {
                        RuntimeError::new("X::DateTime::Range: day second out of range".to_string())
                    })?;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64;
                    has_named = true;
                }
                Value::Num(epoch) => {
                    let total = *epoch + timezone as f64;
                    let total_i = total.floor() as i64;
                    let frac = total - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64 + frac;
                    has_named = true;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Date" => {
                    let (y, m, d) = temporal::date_attrs(&attributes.as_map());
                    year = y;
                    month = m;
                    day = d;
                    hour = 0;
                    minute = 0;
                    second = 0.0;
                    has_named = true;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::Int(0));
                    let (tai_int, tai_frac) = match &val {
                        Value::Rat(n, d) if *d != 0 => (*n / *d, (*n % *d) as f64 / *d as f64),
                        _ => {
                            let f = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                            (f.floor() as i64, f - f.floor())
                        }
                    };
                    let (y, m, d, h, mi, s) =
                        temporal::instant_to_datetime_leap_aware_parts(tai_int, tai_frac, timezone);
                    year = y;
                    month = m;
                    day = d;
                    hour = h;
                    minute = mi;
                    second = s;
                    has_named = true;
                }
                other if other.is_numeric() => {
                    let epoch = other.to_f64() + timezone as f64;
                    let total_i = epoch.floor() as i64;
                    let frac = epoch - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64 + frac;
                    has_named = true;
                }
                _ => {}
            }
        }
        if !has_named {
            return Err(RuntimeError::new("DateTime.new requires arguments"));
        }
        temporal::validate_datetime(year, month, day, hour, minute, second, timezone)?;
        let dt = temporal::make_datetime(year, month, day, hour, minute, second, timezone);
        Ok((dt, formatter))
    }

    /// Build a `Duration` instance from `.new` arguments as pure data: the
    /// seconds argument is stored as a `Rational` (matching Rakudo, where
    /// `Duration.new(...).tai` is always a `Rat`); `Inf`/`-Inf`/`NaN` map to the
    /// degenerate Rats `1/0`/`-1/0`/`0/0`. A non-numeric string argument is an
    /// `X::Str::Numeric` error (the one fallible built-in builder).
    pub(crate) fn build_native_duration_value(args: &[Value]) -> Result<Value, RuntimeError> {
        let secs = if let Some(arg) = args.first() {
            if let Value::Str(s) = arg {
                match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => {
                        let mut err = RuntimeError::new(format!(
                            "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}'",
                            s
                        ));
                        let mut eattrs = HashMap::new();
                        eattrs.insert("source".to_string(), Value::str(s.to_string()));
                        eattrs.insert("pos".to_string(), Value::Int(0));
                        eattrs.insert(
                            "reason".to_string(),
                            Value::str(
                                "base-10 number must begin with valid digits or '.'".to_string(),
                            ),
                        );
                        err.exception = Some(Box::new(Value::make_instance(
                            Symbol::intern("X::Str::Numeric"),
                            eattrs,
                        )));
                        return Err(err);
                    }
                }
            } else {
                to_float_value(arg).unwrap_or(0.0)
            }
        } else {
            0.0
        };
        let val = if secs.is_infinite() {
            if secs > 0.0 {
                Value::Rat(1, 0)
            } else {
                Value::Rat(-1, 0)
            }
        } else if secs.is_nan() {
            Value::Rat(0, 0)
        } else {
            match args.first() {
                Some(v) => crate::builtins::arith::real_to_rat(v),
                None => crate::value::make_rat(0, 1),
            }
        };
        let mut attrs = HashMap::new();
        attrs.insert("value".to_string(), val);
        Ok(Value::make_instance(Symbol::intern("Duration"), attrs))
    }

    /// Build a `StrDistance` instance from its `before`/`after` named args as
    /// pure data.
    pub(crate) fn build_native_strdistance_value(args: &[Value]) -> Value {
        let mut before = String::new();
        let mut after = String::new();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "before" => before = value.to_string_value(),
                    "after" => after = value.to_string_value(),
                    _ => {}
                }
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert("before".to_string(), Value::str(before));
        attrs.insert("after".to_string(), Value::str(after));
        Value::make_instance(Symbol::intern("StrDistance"), attrs)
    }

    /// VM-native construction for `FakeScheduler.new` — pure data: a fresh
    /// process-global scheduler id seeded at virtual time 0.0. `next_fake_scheduler_id`
    /// / `fake_scheduler_init` only touch a process-global table (no env / registry /
    /// user code), so the VM builds it directly, byte-identical to the interpreter.
    pub(crate) fn build_native_fakescheduler_value() -> Value {
        let sched_id = super::native_methods::next_fake_scheduler_id();
        super::native_methods::fake_scheduler_init(sched_id, 0.0);
        let mut attrs = HashMap::new();
        attrs.insert("scheduler_id".to_string(), Value::Int(sched_id as i64));
        Value::make_instance(Symbol::intern("FakeScheduler"), attrs)
    }

    /// VM-native construction for `Proxy.new(:FETCH(...), :STORE(...))` — pure data
    /// assembly that wraps the (already-evaluated) FETCH/STORE callables.
    pub(crate) fn build_native_proxy_value(args: &[Value]) -> Value {
        let mut fetcher = Value::Nil;
        let mut storer = Value::Nil;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "FETCH" => fetcher = (**value).clone(),
                    "STORE" => storer = (**value).clone(),
                    _ => {}
                }
            }
        }
        Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass: None,
            decontainerized: false,
        }
    }

    /// VM-native construction for `Match.new(:orig, :from, :pos|:to, :list, :hash)` —
    /// pure data assembly: the matched substring is sliced out of `orig[from..to]`
    /// and the positional/named captures stored as instance attributes.
    pub(crate) fn build_native_match_value(args: &[Value]) -> Value {
        let mut orig = String::new();
        let mut from: i64 = 0;
        let mut to: i64 = 0;
        let mut list = Value::array(Vec::new());
        let mut hash = Value::hash(HashMap::new());
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "orig" => orig = value.to_string_value(),
                    "from" => from = to_int(value),
                    "pos" | "to" => to = to_int(value),
                    "list" => list = (**value).clone(),
                    "hash" => hash = (**value).clone(),
                    _ => {}
                }
            }
        }
        // Compute matched string from orig[from..to]
        let matched: String = orig
            .chars()
            .skip(from as usize)
            .take((to - from) as usize)
            .collect();
        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::str(matched));
        attrs.insert("from".to_string(), Value::Int(from));
        attrs.insert("to".to_string(), Value::Int(to));
        attrs.insert("orig".to_string(), Value::str(orig));
        // Convert list to positional captures
        if let Value::Array(items, ..) = &list {
            attrs.insert("list".to_string(), Value::array(items.to_vec()));
        } else {
            attrs.insert("list".to_string(), Value::array(Vec::new()));
        }
        // Convert hash (Map) to named captures
        if let Value::Hash(map, ..) = &hash {
            attrs.insert("named".to_string(), Value::hash(map.as_ref().clone()));
        } else {
            attrs.insert("named".to_string(), Value::hash(HashMap::new()));
        }
        Value::make_instance(Symbol::intern("Match"), attrs)
    }

    /// VM-native construction for an allomorph type (`IntStr`/`NumStr`/`RatStr`/
    /// `ComplexStr`) — `.new(numeric, string)` is pure data assembly: the inner
    /// numeric value (unwrapped from an allomorphic `Mixin` argument) is mixed
    /// with a `Str` override carrying the string form. No env / registry / user
    /// code. The interpreter's `dispatch_new_and_constructors` arm calls the same
    /// helper, so the native path is byte-identical.
    pub(crate) fn build_native_allomorph_value(
        type_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(format!(
                "{}.new requires two arguments (numeric, string)",
                type_name
            )));
        }
        // Unwrap allomorphic (Mixin) arguments to get the inner numeric value.
        let numeric = match &args[0] {
            Value::Mixin(inner, _) => (**inner).clone(),
            other => other.clone(),
        };
        let string = args[1].to_string_value();
        let mut mixins = HashMap::new();
        mixins.insert("Str".to_string(), Value::str(string));
        Ok(Value::mixin(numeric, mixins))
    }

    /// VM-native construction for `ObjAt`/`ValueObjAt` — `.new(which)` is pure
    /// data assembly: the first positional argument's stringification is stored
    /// as the `WHICH` attribute. A missing positional is the same arity error the
    /// interpreter raises. Shared with the interpreter's
    /// `dispatch_new_and_constructors` arm so the native path is byte-identical.
    pub(crate) fn build_native_objat_value(
        class_name: Symbol,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let positional = args
            .iter()
            .find(|a| !matches!(a, Value::Pair(_, _) | Value::ValuePair(_, _)));
        match positional {
            Some(val) => {
                let mut attrs = HashMap::new();
                attrs.insert("WHICH".to_string(), Value::str(val.to_string_value()));
                Ok(Value::make_instance(class_name, attrs))
            }
            None => Err(RuntimeError::new(
                "Too few positionals passed; expected 2 arguments but got 1".to_string(),
            )),
        }
    }

    /// VM-native gate for `IO::Path` family `.new(...)`. Returns `Some` only for
    /// the built-in `IO::Path` classes (`IO::Path`, `IO::Path::Unix`/`::Win32`/
    /// `::Cygwin`/`::QNX`), which never carry a user `new`; user subclasses fall
    /// through to the interpreter's `dispatch_new` (where the broader MRO-based
    /// `is_io_path_like` gate applies). The construction is pure path-string
    /// assembly via the shared `build_io_path_instance` impl the interpreter's
    /// `dispatch_new` arm also calls, so the native path is byte-identical. The
    /// SPEC-variant subclass is registered (parent `IO::Path`) so reflection on
    /// the result matches the interpreter, which registers it on `.new`.
    pub(crate) fn try_native_io_path_construct(
        &mut self,
        class_name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn_resolved = class_name.resolve();
        if !Self::is_io_path_lexical_class(&cn_resolved) {
            return None;
        }
        // SPEC-variant subclasses are registered (parent IO::Path) the first time
        // they are constructed, mirroring the interpreter's `dispatch_new`.
        if cn_resolved.starts_with("IO::Path::")
            && !self.registry().classes.contains_key(cn_resolved.as_str())
        {
            self.registry_mut().classes.insert(
                cn_resolved.to_string(),
                ClassDef {
                    parents: vec!["IO::Path".to_string()],
                    attributes: Vec::new(),
                    methods: HashMap::new(),
                    native_methods: std::collections::HashSet::new(),
                    mro: Vec::new(),
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: HashMap::new(),
                },
            );
        }
        Some(self.build_io_path_instance(class_name, &cn_resolved, args))
    }

    /// Pure path-string assembly for an `IO::Path` family `.new(...)`: a
    /// positional path (or an `IO::Path` instance whose `path` is reused), or a
    /// `basename`/`dirname`/`volume` triple, joined with the SPEC-derived
    /// separator, plus optional `CWD`/`SPEC` attributes. Reads only the registry
    /// (`class_mro`, for the IO::Path-instance argument case) — no FS, no cwd, no
    /// env, no user code. The single authoritative impl shared by the interpreter's
    /// `dispatch_new` arm and the VM's `try_native_io_path_construct`.
    pub(crate) fn build_io_path_instance(
        &mut self,
        class_name: Symbol,
        cn_resolved: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut positional_path: Option<String> = None;
        let mut basename_part: Option<String> = None;
        let mut dirname_part: Option<String> = None;
        let mut volume_part: Option<String> = None;
        let mut cwd_attr: Option<String> = None;
        let mut spec_attr: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "CWD" => {
                    cwd_attr = Some(value.to_string_value());
                }
                Value::Pair(key, value) if key == "SPEC" => {
                    spec_attr = Some((**value).clone());
                }
                Value::Pair(key, value) if key == "basename" => {
                    basename_part = Some(value.to_string_value());
                }
                Value::Pair(key, value) if key == "dirname" => {
                    dirname_part = Some(value.to_string_value());
                }
                Value::Pair(key, value) if key == "volume" => {
                    volume_part = Some(value.to_string_value());
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if positional_path.is_none()
                    && self
                        .class_mro(&class_name.resolve())
                        .iter()
                        .any(|n| n == "IO::Path") =>
                {
                    positional_path = Some(
                        attributes
                            .as_map()
                            .get("path")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                    );
                    if cwd_attr.is_none() {
                        cwd_attr = attributes.as_map().get("cwd").map(|v| v.to_string_value());
                    }
                }
                Value::Pair(_, _) => {}
                _ if positional_path.is_none() => {
                    positional_path = Some(arg.to_string_value());
                }
                _ => {}
            }
        }
        // Determine dir separator from SPEC (Win32 uses '\', others use '/')
        let is_win32_spec = spec_attr
            .as_ref()
            .map(|s| {
                let name = match s {
                    Value::Package(n) => n.resolve().to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve().to_string(),
                    _ => String::new(),
                };
                name == "IO::Spec::Win32" || name.ends_with("Win32")
            })
            .unwrap_or(false);
        let dir_sep = if is_win32_spec { '\\' } else { '/' };
        let path = if let Some(positional) = positional_path {
            positional
        } else if let Some(basename) = basename_part {
            let mut built = match dirname_part {
                Some(dirname) if !dirname.is_empty() => {
                    if dirname.ends_with('/') || dirname.ends_with('\\') {
                        format!("{dirname}{basename}")
                    } else {
                        format!("{dirname}{dir_sep}{basename}")
                    }
                }
                _ => basename,
            };
            if let Some(volume) = volume_part
                && !volume.is_empty()
            {
                if volume.ends_with('/') || volume.ends_with('\\') {
                    built = format!("{volume}{built}");
                } else {
                    built = format!("{volume}{dir_sep}{built}");
                }
            }
            built
        } else {
            String::new()
        };
        if path.is_empty() {
            return Err(RuntimeError::new(
                "Must specify a non-empty string as a path",
            ));
        }
        if path.contains('\0') {
            return Err(RuntimeError::new(
                "X::IO::Null: Found null byte in pathname",
            ));
        }
        let mut attrs = HashMap::new();
        attrs.insert("path".to_string(), Value::str(path));
        if let Some(cwd) = cwd_attr {
            attrs.insert("cwd".to_string(), Value::str(cwd));
        }
        if cn_resolved.starts_with("IO::Path::") {
            let spec_name = format!("IO::Spec::{}", cn_resolved.trim_start_matches("IO::Path::"));
            attrs.insert(
                "SPEC".to_string(),
                Value::Package(Symbol::intern(&spec_name)),
            );
        } else if let Some(spec) = spec_attr {
            attrs.insert("SPEC".to_string(), spec);
        }
        Ok(Value::make_instance(class_name, attrs))
    }

    /// VM-native construction for a built-in type whose `.new(...)` is pure data
    /// assembly (no env / registry / user code): `Buf`/`Blob` (byte overlay),
    /// `utf8`/`utf16` (code units), `Uni` (codepoints), `Version`, `Duration`
    /// (the one fallible builder — a bad string is `X::Str::Numeric`),
    /// `StrDistance`, `Stash` and the empty-instance schedulers/handles. Returns
    /// `Some` with the constructed value (or its error) when `class_name` is one
    /// of these, else `None` so the caller falls through to the generic
    /// constructor. The interpreter's `dispatch_new` arms call the same per-type
    /// helpers, so the native path is byte-identical.
    pub(crate) fn try_native_builtin_construct(
        class_name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn = class_name.resolve();
        if Self::is_native_buf_constructible(&cn) {
            Some(Ok(Self::build_native_buf_value(class_name, args)))
        } else if cn == "utf8" || cn == "utf16" {
            Some(Ok(Self::build_native_utf_value(class_name, args)))
        } else if cn == "Uni" {
            Some(Ok(Self::build_native_uni_value(args)))
        } else if cn == "Version" {
            Some(Ok(Self::version_from_value(
                args.first().cloned().unwrap_or(Value::Nil),
            )))
        } else if cn == "Complex" {
            Some(Ok(Self::build_native_complex_value(args)))
        } else if cn == "Int" {
            Some(Self::build_native_int_value(args))
        } else if cn == "Num" {
            Some(Self::build_native_num_value(args))
        } else if cn == "Str" {
            // The default Str constructor ignores positional args and yields the
            // empty string (mutsu is lenient where raku rejects a positional).
            // (`Bool` is intentionally NOT native-ized: it is an enum, so
            // `Bool.new` errors in `dispatch_new` before the basic-type arm —
            // the native path must preserve that, so it falls through.)
            Some(Ok(Value::str(String::new())))
        } else if cn == "Slip" {
            Some(Ok(Value::slip(args.to_vec())))
        } else if cn == "IterationBuffer" {
            Some(Ok(Self::build_native_iterationbuffer_value(
                class_name, args,
            )))
        } else if cn == "Rat" {
            Some(Ok(Self::build_native_rat_value(args)))
        } else if cn == "FatRat" {
            Some(Ok(Self::build_native_fatrat_value(args)))
        } else if cn == "Pair" {
            Some(Ok(Self::build_native_pair_value(args)))
        } else if cn == "Date" {
            // A `:formatter` renders a user Callable (`render_date_formatter`,
            // self-dependent) — fall through to the interpreter for that case;
            // the common no-formatter date is built natively.
            match Self::build_native_date(args) {
                Ok((date, None)) => Some(Ok(date)),
                Ok((_, Some(_))) => None,
                Err(e) => Some(Err(e)),
            }
        } else if cn == "DateTime" {
            // Same `:formatter` caveat as `Date` (the formatter renders a user
            // Callable via `eval_call_on_value`).
            match Self::build_native_datetime(args) {
                Ok((dt, None)) => Some(Ok(dt)),
                Ok((_, Some(_))) => None,
                Err(e) => Some(Err(e)),
            }
        } else if cn == "Duration" {
            Some(Self::build_native_duration_value(args))
        } else if cn == "StrDistance" {
            Some(Ok(Self::build_native_strdistance_value(args)))
        } else if cn == "Stash" {
            // A `Stash` is an empty Hash-typed instance.
            Some(Ok(Value::make_instance(class_name, HashMap::new())))
        } else if matches!(
            cn.as_str(),
            "ThreadPoolScheduler" | "CurrentThreadScheduler" | "Tap" | "Cancellation"
        ) {
            // These take no construction args — just an empty instance.
            Some(Ok(Value::make_instance(class_name, HashMap::new())))
        } else if matches!(cn.as_str(), "Lock" | "Lock::Async" | "Lock::Soft") {
            // A lock is pure data: a fresh global lock id (and an `async` flag for
            // `Lock::Async`). `next_lock_id` only bumps a process-global counter —
            // no env / registry / user code — so the VM builds it directly,
            // byte-identical to the interpreter's `dispatch_new` arm.
            let mut attrs = HashMap::new();
            attrs.insert(
                "lock-id".to_string(),
                Value::Int(super::native_methods::next_lock_id() as i64),
            );
            if cn == "Lock::Async" {
                attrs.insert("async".to_string(), Value::Bool(true));
            }
            Some(Ok(Value::make_instance(class_name, attrs)))
        } else if cn == "Promise" {
            // A bare `Promise.new` is an empty planned promise — pure shared
            // state, no env / registry / user code. Shared with the interpreter's
            // `dispatch_new` arm.
            Some(Ok(Value::Promise(crate::value::SharedPromise::new())))
        } else if cn == "Channel" {
            // Likewise an empty channel.
            Some(Ok(Value::Channel(crate::value::SharedChannel::new())))
        } else if matches!(cn.as_str(), "Supplier" | "Supplier::Preserving") {
            // A supplier is pure data: an empty emission log, a not-done flag, and
            // a fresh process-global supplier id. Shared with the interpreter's
            // `dispatch_new` arm.
            let mut attrs = HashMap::new();
            attrs.insert("emitted".to_string(), Value::array(Vec::new()));
            attrs.insert("done".to_string(), Value::Bool(false));
            attrs.insert(
                "supplier_id".to_string(),
                Value::Int(super::native_methods::next_supplier_id() as i64),
            );
            Some(Ok(Value::make_instance(class_name, attrs)))
        } else if cn == "Capture" {
            // The default `Capture.new` produces an *empty* Capture: named args
            // are dropped (Capture has no buildable public attributes — `bless`
            // ignores them) and positional args are rejected (`Mu.new` is
            // named-only). A *populated* Capture is built with the `\(...)`
            // literal, not `.new`. A named arg reaches here as `Value::Pair`;
            // anything else (a literal, a positional `"a" => 1` `ValuePair`) is
            // positional and dies, exactly as raku does.
            if args.iter().any(|a| !matches!(a, Value::Pair(..))) {
                Some(Err(RuntimeError::new(
                    "Default constructor for 'Capture' only takes named arguments",
                )))
            } else {
                Some(Ok(Value::Capture {
                    positional: Box::new(Vec::new()),
                    named: Box::new(HashMap::new()),
                }))
            }
        } else if cn == "FakeScheduler" {
            Some(Ok(Self::build_native_fakescheduler_value()))
        } else if cn == "Proxy" {
            Some(Ok(Self::build_native_proxy_value(args)))
        } else if cn == "Match" {
            Some(Ok(Self::build_native_match_value(args)))
        } else if matches!(cn.as_str(), "IntStr" | "NumStr" | "RatStr" | "ComplexStr") {
            // Allomorph `.new(numeric, string)` is pure data assembly (a numeric
            // value mixed with a `Str` override) — shared with the interpreter's
            // `dispatch_new_and_constructors` arm.
            Some(Self::build_native_allomorph_value(&cn, args))
        } else if matches!(cn.as_str(), "ObjAt" | "ValueObjAt") {
            // `ObjAt`/`ValueObjAt` `.new(which)` stores the stringified first
            // positional as the `WHICH` attribute — pure data assembly.
            Some(Self::build_native_objat_value(class_name, args))
        } else {
            None
        }
    }

    /// VM-native dispatch for a built-in *class* method (a method on a type
    /// object other than `.new`) whose result is pure data assembly — no env /
    /// registry / user code. Currently `Instant.from-posix(secs)`, which maps a
    /// POSIX timestamp to TAI seconds and wraps it in an `Instant`. Returns
    /// `Some` when handled, else `None` so the caller falls through to the
    /// interpreter's class-method dispatch. The interpreter calls the same arm,
    /// so the two stay byte-identical. The method name is matched dash-
    /// insensitively (`from-posix` == `from_posix`), as the interpreter does.
    pub(crate) fn try_native_builtin_class_method(
        class_name: Symbol,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn = class_name.resolve();
        if cn == "Instant" && method.replace('-', "_") == "from_posix" {
            let secs = args.first().and_then(to_float_value).unwrap_or(0.0);
            let tai = crate::builtins::methods_0arg::temporal::posix_to_instant(secs);
            let mut attrs = HashMap::new();
            attrs.insert("value".to_string(), Value::Num(tai));
            return Some(Ok(Value::make_instance(Symbol::intern("Instant"), attrs)));
        }
        None
    }

    /// Drop *named* arguments from a Hash/QuantHash constructor's argument list.
    ///
    /// A bareword `key => value` argument is a named argument that `.new`
    /// silently eats — it does NOT become an element (`Bag.new(a => 1).elems`
    /// is 0, while `Bag.new("a" => 1).elems` is 1). mutsu keeps the
    /// named/positional distinction in the value form: a named argument reaches
    /// here un-containerized as a `Value::Pair`, whereas a positional pair
    /// literal (`(a => 1)`, `"a" => 1`) is a `Value::ValuePair`, and pairs
    /// flattened out of an array/variable remain inside their `Array`. So
    /// dropping top-level `Value::Pair`s strips exactly the named arguments
    /// while preserving every positional pair.
    pub(super) fn strip_named_pair_args(args: Vec<Value>) -> Vec<Value> {
        if args.iter().any(|a| matches!(a, Value::Pair(..))) {
            args.into_iter()
                .filter(|a| !matches!(a, Value::Pair(..)))
                .collect()
        } else {
            args
        }
    }

    /// Materialize a user `message` *method* into the `message` attribute of a
    /// freshly-constructed exception. raku computes `Exception.message` lazily,
    /// but mutsu's interpreter-less stringification (`join`, `sprintf "%s"`, `~`,
    /// interpolation -> `to_string_value`) cannot dispatch a user method, so such
    /// an exception would stringify as "X::Foo with no message" outside an
    /// explicit `.Str`/`.gist` call. Running the (conventionally pure) message
    /// method once at construction and caching the result keeps every
    /// stringification path coherent. Scoped to exceptions that define `message`
    /// as a method and have no `message` attribute, so built-in and
    /// attribute-message exceptions are unaffected. Errors in the message method
    /// are swallowed (the exception is still returned un-materialized).
    pub(crate) fn materialize_exception_message_in_result(
        &mut self,
        result: Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        let Ok(Value::Instance {
            class_name,
            attributes,
            id,
        }) = result
        else {
            return result;
        };
        let cn = class_name.resolve();
        let is_exc = cn == "Exception" || cn.starts_with("X::") || cn.starts_with("CX::");
        if !is_exc
            || attributes.as_map().contains_key("message")
            || !self.has_user_method(&cn, "message")
        {
            return Ok(Value::Instance {
                class_name,
                attributes,
                id,
            });
        }
        let instance = Value::Instance {
            class_name,
            attributes: attributes.clone(),
            id,
        };
        if let Ok(msg) = self.call_method_with_values(instance.clone(), "message", vec![]) {
            let msg_str = msg.to_string_value();
            if !msg_str.is_empty() {
                let mut attrs = attributes.as_map().clone();
                attrs.insert("message".to_string(), Value::str(msg_str));
                return Ok(Value::make_instance(class_name, attrs));
            }
        }
        Ok(instance)
    }

    pub(super) fn dispatch_new(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Package(_) = &target {
            let materialized = self.materialize_default_parametric_role(target.clone())?;
            if materialized != target {
                return self.dispatch_new(materialized, args);
            }
        }
        // Collation.new — create a Collation instance with default settings
        if let Value::Package(name) = &target
            && name == "Collation"
        {
            return Ok(Self::make_collation_instance(1, 1, 1, 1));
        }
        // Calling .new() on an instance delegates to the class constructor
        if let Value::Instance { class_name, .. } = &target {
            return self.dispatch_new(Value::Package(*class_name), args);
        }
        // Calling .new() on a Mixin(Instance{..}, ..) delegates to the class constructor
        if let Value::Mixin(inner, _) = &target
            && let Value::Instance { class_name, .. } = inner.as_ref()
        {
            return self.dispatch_new(Value::Package(*class_name), args);
        }
        // Calling .new() on a concrete Array delegates to the type constructor.
        // If the array has type metadata (e.g. array[str]), use the declared type.
        if let Value::Array(..) = &target {
            let type_name = if let Some(info) = self.container_type_metadata(&target) {
                if let Some(ref dt) = info.declared_type {
                    dt.clone()
                } else if info.value_type != "Any" && info.value_type != "Mu" {
                    // Construct the parametric type name from value_type
                    format!("array[{}]", info.value_type)
                } else {
                    "Array".to_string()
                }
            } else {
                "Array".to_string()
            };
            return self.dispatch_new(Value::Package(Symbol::intern(&type_name)), args);
        }
        // Calling .new() on a concrete object hash (`%h{KeyType}`) produces a
        // new object hash of the same key/value type, not a plain Hash — mirror
        // the typed-Array `.new` path above.
        if let Value::Hash(_) = &target
            && let Some(info) = self.container_type_metadata(&target)
            && let Some(kt) = info.key_type.clone()
        {
            let vt = if info.value_type.is_empty() {
                "Any".to_string()
            } else {
                info.value_type.clone()
            };
            let pkg = format!("Hash[{},{}]", vt, kt);
            return self.dispatch_new(Value::Package(Symbol::intern(&pkg)), args);
        }
        // Calling .new() on a concrete Hash/Bag/Mix delegates to the type constructor
        {
            let type_pkg = match &target {
                Value::Hash(_) => Some("Hash"),
                Value::Bag(_, _) => Some("BagHash"),
                Value::Mix(_, _) => Some("MixHash"),
                _ => None,
            };
            if let Some(type_name) = type_pkg {
                return self.dispatch_new(Value::Package(Symbol::intern(type_name)), args);
            }
        }
        if let Value::Str(ref name) = target
            && self.registry().enum_types.contains_key(name.as_str())
        {
            let msg = format!(
                "Enum '{}' is insufficiently type-like to be instantiated.  Did you mean 'class'?",
                name
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Constructor::BadType"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = &target
        {
            let base_name_str = base_name.resolve();
            self.ensure_role_punned_to_class(&base_name_str);
            let mut selected_role = self.registry().roles.get(&base_name_str).cloned();
            let mut matched_lang_version: Option<String> = None;
            let mut selected_param_names = self
                .registry()
                .role_type_params
                .get(&base_name_str)
                .cloned()
                .unwrap_or_default();
            // Hoist clone to a `let` so the guard drops before the filter_map
            // closure re-enters (&mut self).
            let candidates = self.registry().role_candidates.get(&base_name_str).cloned();
            if let Some(candidates) = candidates {
                let mut matching: Vec<(super::RoleCandidateDef, i32, usize)> = candidates
                    .into_iter()
                    .enumerate()
                    .filter_map(|(idx, candidate)| {
                        let candidate_param_names = candidate
                            .type_param_defs
                            .iter()
                            .map(|pd| pd.name.clone())
                            .collect::<Vec<_>>();
                        let positional_params = candidate
                            .type_param_defs
                            .iter()
                            .filter(|pd| !pd.named)
                            .collect::<Vec<_>>();
                        let has_positional_slurpy = positional_params
                            .iter()
                            .any(|pd| pd.slurpy && !pd.name.starts_with('%'));
                        let required = positional_params
                            .iter()
                            .filter(|pd| !pd.slurpy && pd.default.is_none() && !pd.optional_marker)
                            .count();
                        let arity_ok = if candidate.type_param_defs.is_empty() {
                            type_args.is_empty()
                        } else {
                            type_args.len() >= required
                                && (has_positional_slurpy
                                    || type_args.len() <= positional_params.len())
                        };
                        let ok = if arity_ok {
                            let saved_env = self.env.clone();
                            let ok = self
                                .bind_function_args_values(
                                    &candidate.type_param_defs,
                                    &candidate_param_names,
                                    type_args,
                                )
                                .is_ok();
                            self.env = saved_env;
                            ok
                        } else {
                            false
                        };
                        if ok {
                            let score = candidate
                                .type_param_defs
                                .iter()
                                .filter(|pd| !pd.named)
                                .map(|pd| {
                                    let mut s = if let Some(tc) = pd.type_constraint.as_deref() {
                                        if tc.starts_with("::") || tc == "Any" || tc == "Mu" {
                                            1
                                        } else {
                                            5
                                        }
                                    } else {
                                        0
                                    };
                                    if pd.where_constraint.is_some() {
                                        s += 20;
                                    }
                                    if pd.literal_value.is_some() {
                                        s += 30;
                                    }
                                    s
                                })
                                .sum();
                            Some((candidate, score, idx))
                        } else {
                            None
                        }
                    })
                    .collect();
                matching.sort_by(|a, b| b.1.cmp(&a.1).then(b.2.cmp(&a.2)));
                if let Some((candidate, _, _)) = matching.into_iter().next() {
                    selected_param_names = candidate.type_params.clone();
                    matched_lang_version = Some(candidate.language_version.clone());
                    selected_role = Some(candidate.role_def.clone());
                }
            }
            if let Some(role) = selected_role {
                let role_id = role.role_id;
                let mut named_args: HashMap<String, Value> = HashMap::new();
                let mut positional_args: Vec<Value> = Vec::new();
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        named_args.insert(key.clone(), *value.clone());
                    } else {
                        positional_args.push(arg.clone());
                    }
                }

                let mut mixins = HashMap::new();
                mixins.insert(format!("__mutsu_role__{}", base_name), Value::Bool(true));
                mixins.insert(
                    format!("__mutsu_role_typeargs__{}", base_name),
                    Value::array(type_args.clone()),
                );
                if role_id != 0 {
                    mixins.insert(
                        format!("__mutsu_role_id__{}", base_name),
                        Value::Int(role_id as i64),
                    );
                }
                for (param_name, type_arg) in selected_param_names.iter().zip(type_args.iter()) {
                    mixins.insert(
                        format!("__mutsu_role_param__{}", param_name),
                        type_arg.clone(),
                    );
                }
                let saved_role_param_env = self.env.clone();
                for (param_name, type_arg) in selected_param_names.iter().zip(type_args.iter()) {
                    self.env.insert(param_name.clone(), type_arg.clone());
                }
                for (idx, (attr_name, _is_public, default_expr, _, _, _, _)) in
                    role.attributes.iter().enumerate()
                {
                    let value = if let Some(v) = named_args.get(attr_name) {
                        v.clone()
                    } else if let Some(v) = positional_args.get(idx) {
                        v.clone()
                    } else if let Some(expr) = default_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                    } else {
                        Value::Nil
                    };
                    mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
                }
                self.env = saved_role_param_env;
                // Embed language revision in mixin metadata so
                // ^language-revision on the punned instance returns
                // the revision of the matched candidate.
                if let Some(ref ver) = matched_lang_version {
                    let revision: String = if let Some(letter) = ver.strip_prefix("6.") {
                        letter.chars().next().unwrap_or('c').to_string()
                    } else {
                        "c".to_string()
                    };
                    mixins.insert(
                        "__mutsu_language_revision".to_string(),
                        Value::str(revision),
                    );
                }
                return Ok(Value::mixin(
                    Value::make_instance(*base_name, HashMap::new()),
                    mixins,
                ));
            }
        }

        if let Value::Package(class_name) = &target {
            let cn_resolved = class_name.resolve();
            // Fast path: user-defined class with no BUILD/TWEAK/custom new,
            // only simple parents (Any/Mu/Cool), only $-sigiled attributes,
            // and no native methods. Shared with the VM so `Foo.new(...)` for
            // such classes is constructed without entering the generic method
            // dispatch machinery (see `VM::try_native_default_construct`).
            if let Some(result) = self.try_native_default_construct(*class_name, &args) {
                return result;
            }
            let parametric = Self::parse_parametric_type_name(&cn_resolved);
            let (base_class_name, type_args) = if let Some((base, args)) = &parametric {
                (base.as_str(), Some(args.clone()))
            } else {
                (cn_resolved.as_str(), None)
            };
            if cn_resolved.starts_with("IO::Path::")
                && !self.registry().classes.contains_key(&cn_resolved)
            {
                self.registry_mut().classes.insert(
                    cn_resolved.clone(),
                    ClassDef {
                        parents: vec!["IO::Path".to_string()],
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        native_methods: std::collections::HashSet::new(),
                        mro: Vec::new(),
                        attribute_types: HashMap::new(),
                        attribute_smileys: HashMap::new(),
                        attribute_built: HashMap::new(),
                        wildcard_handles: Vec::new(),
                        alias_attributes: HashSet::new(),
                        class_level_attrs: HashMap::new(),
                    },
                );
            }
            // IO::Spec::* types: create an IO::Spec instance with the spec name
            if cn_resolved.starts_with("IO::Spec::") {
                let attrs = HashMap::new();
                return Ok(Value::make_instance(Symbol::intern(&cn_resolved), attrs));
            }
            let class_key = if self.registry().classes.contains_key(&cn_resolved) {
                cn_resolved.as_str()
            } else {
                base_class_name
            };
            let is_datetime_subclass = cn_resolved != "DateTime"
                && self
                    .class_mro(class_key)
                    .iter()
                    .any(|name| name == "DateTime");
            if is_datetime_subclass && !is_normalized_datetime_subclass_ctor_args(&args) {
                let positional_args: Vec<Value> = args
                    .iter()
                    .filter(|arg| !matches!(arg, Value::Pair(_, _)))
                    .cloned()
                    .collect();
                let mut datetime_ctor_args = Vec::new();
                if positional_args.len() >= 3 {
                    datetime_ctor_args.push(Value::Pair(
                        "year".to_string(),
                        Box::new(positional_args[0].clone()),
                    ));
                    datetime_ctor_args.push(Value::Pair(
                        "month".to_string(),
                        Box::new(positional_args[1].clone()),
                    ));
                    datetime_ctor_args.push(Value::Pair(
                        "day".to_string(),
                        Box::new(positional_args[2].clone()),
                    ));
                    if let Some(hour) = positional_args.get(3) {
                        datetime_ctor_args
                            .push(Value::Pair("hour".to_string(), Box::new(hour.clone())));
                    }
                    if let Some(minute) = positional_args.get(4) {
                        datetime_ctor_args
                            .push(Value::Pair("minute".to_string(), Box::new(minute.clone())));
                    }
                    if let Some(second) = positional_args.get(5) {
                        datetime_ctor_args
                            .push(Value::Pair("second".to_string(), Box::new(second.clone())));
                    }
                    for arg in &args {
                        if let Value::Pair(key, _) = arg
                            && is_datetime_constructor_named_arg(key)
                        {
                            datetime_ctor_args.push(arg.clone());
                        }
                    }
                } else {
                    datetime_ctor_args = args.clone();
                }
                let datetime = self.dispatch_new(
                    Value::Package(Symbol::intern("DateTime")),
                    datetime_ctor_args,
                )?;
                let mut normalized_args = vec![datetime];
                for arg in &args {
                    if let Value::Pair(key, _) = arg
                        && !is_datetime_constructor_named_arg(key)
                    {
                        normalized_args.push(arg.clone());
                    }
                }
                return self.dispatch_new(target.clone(), normalized_args);
            }
            // Date subclass handling: delegate Date-specific constructor logic
            // then merge custom attributes via the generic constructor.
            let is_date_subclass = cn_resolved != "Date"
                && self.class_mro(class_key).iter().any(|name| name == "Date");
            // Only delegate if the args don't already contain Date attrs
            // (prevents infinite recursion on second call)
            let has_date_named_args = args.iter().any(|a| {
                matches!(a, Value::Pair(k, _) if k == "year" || k == "month" || k == "day" || k == "days")
            });
            if is_date_subclass && !has_date_named_args && !self.has_user_method(class_key, "new") {
                // Build Date first to extract year/month/day/days/formatter
                let date =
                    self.dispatch_new(Value::Package(Symbol::intern("Date")), args.clone())?;
                if let Value::Instance { attributes, .. } = &date {
                    // Now build the subclass instance with all Date attrs plus any custom named attrs
                    let mut new_args = Vec::new();
                    for (k, v) in attributes.as_map().iter() {
                        new_args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                    }
                    // Add any non-Date named args from the original call
                    for arg in &args {
                        if let Value::Pair(key, _) = arg
                            && !attributes.contains_key(key.as_str())
                        {
                            new_args.push(arg.clone());
                        }
                    }
                    return self.dispatch_new(target.clone(), new_args);
                }
            }
            let is_io_path_like = base_class_name == "IO::Path"
                || self
                    .class_mro(class_key)
                    .iter()
                    .any(|name| name == "IO::Path");
            if is_io_path_like && !self.has_user_method(class_key, "new") {
                // Pure path-string assembly (registry reads only) — shared with the
                // VM's `try_native_io_path_construct`.
                return self.build_io_path_instance(*class_name, &cn_resolved, &args);
            }
            match base_class_name {
                "IterationBuffer" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_iterationbuffer_value(*class_name, &args));
                }
                "Array" | "List" | "Positional" | "array" => {
                    // Shared single implementation with the VM's native fast path.
                    return self.try_native_array_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        &args,
                    );
                }
                "Hash" | "Map" => {
                    // Shared single implementation with the VM's native fast path.
                    return self.try_native_hash_construct(*class_name, &type_args, &args);
                }
                "Uni" => {
                    // Shared with the VM's native fast path (pure codepoint build).
                    return Ok(Self::build_native_uni_value(&args));
                }
                "Seq" => {
                    // Seq.new(iterator) — pull all items from the iterator
                    if let Some(iterator) = args.first() {
                        if matches!(iterator, Value::Instance { .. })
                            && self.type_matches_value("PredictiveIterator", iterator)
                        {
                            let seq = Value::Seq(std::sync::Arc::new(Vec::new()));
                            if let Value::Seq(items) = &seq {
                                let seq_id = std::sync::Arc::as_ptr(items) as usize;
                                // Store off the scoped env so the association
                                // survives sub/block returns (see field docs).
                                self.predictive_seq_iters.insert(seq_id, iterator.clone());
                                self.env.insert(
                                    format!("__mutsu_predictive_seq_iter::{seq_id}"),
                                    iterator.clone(),
                                );
                            }
                            return Ok(seq);
                        }
                        if let Value::Instance { attributes, .. } = iterator {
                            let map = attributes.as_map();
                            if let Some(Value::Array(items, ..)) =
                                map.get("items").or_else(|| map.get("stuff"))
                            {
                                return Ok(Value::Seq(std::sync::Arc::new(items.to_vec())));
                            }
                        }
                        // Register deferred iterator: don't pull eagerly.
                        // Raku's Seq.new(iterator) creates a lazy Seq; pulling
                        // happens only when the Seq is actually consumed/iterated.
                        let seq = Value::Seq(std::sync::Arc::new(Vec::new()));
                        if let Value::Seq(items) = &seq {
                            crate::value::seq_register_deferred_iter(items, iterator.clone());
                        }
                        return Ok(seq);
                    }
                    // Seq.new() with no args creates a pre-consumed Seq.
                    // This matches Raku: Seq.new() has no iterator, so it's
                    // immediately consumed. This is what .raku returns for
                    // consumed Seqs ("Seq.new()") so the EVAL roundtrip works.
                    let seq = Value::Seq(std::sync::Arc::new(Vec::new()));
                    if let Value::Seq(items) = &seq {
                        let _ = crate::value::seq_consume(items);
                    }
                    return Ok(seq);
                }
                "Version" => {
                    let arg = args.first().cloned().unwrap_or(Value::Nil);
                    return Ok(Self::version_from_value(arg));
                }
                "Duration" => {
                    // Shared with the VM's native fast path (pure Rational build;
                    // a bad string arg is the one fallible built-in builder).
                    return Self::build_native_duration_value(&args);
                }
                "StrDistance" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_strdistance_value(&args));
                }
                "Date" => {
                    // Shared with the VM's native fast path. Only the formatter
                    // case needs `self` (it renders a user Callable).
                    let (date, formatter) = Self::build_native_date(&args)?;
                    if let Some(formatter_value) = formatter {
                        return self.render_date_formatter(date, formatter_value);
                    }
                    return Ok(date);
                }
                "DateTime" => {
                    // Shared with the VM's native fast path. Only the
                    // `:formatter` case needs `self` (it renders a user
                    // Callable); the common case is built natively.
                    let (dt, formatter) = Self::build_native_datetime(&args)?;
                    if let Some(formatter_value) = formatter
                        && let Value::Instance {
                            class_name,
                            ref attributes,
                            id,
                        } = dt
                    {
                        let mut attrs = attributes.to_map();
                        attrs.insert("formatter".to_string(), formatter_value.clone());
                        let dt_with_formatter =
                            Value::write_back_sharing(attributes, class_name, attrs, id);
                        let saved_env = self.env().clone();
                        let saved_readonly = self.save_readonly_vars();
                        let rendered = self
                            .eval_call_on_value(formatter_value, vec![dt_with_formatter.clone()])?
                            .to_string_value();
                        *self.env_mut() = saved_env;
                        self.restore_readonly_vars(saved_readonly);
                        if let Value::Instance {
                            class_name,
                            attributes,
                            id,
                        } = dt_with_formatter
                        {
                            let mut updated = attributes.to_map();
                            updated
                                .insert("__formatter_rendered".to_string(), Value::str(rendered));
                            return Ok(Value::write_back_sharing(
                                &attributes,
                                class_name,
                                updated,
                                id,
                            ));
                        }
                    }
                    return Ok(dt);
                }
                "IO::Socket::INET" => {
                    return self.dispatch_socket_inet_new(&args);
                }
                "Promise" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    return Ok(Value::Promise(SharedPromise::new()));
                }
                "Channel" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    return Ok(Value::Channel(SharedChannel::new()));
                }
                "Stash" => {
                    // Stash is essentially a Hash but with type Stash
                    return Ok(Value::make_instance(
                        Symbol::intern("Stash"),
                        HashMap::new(),
                    ));
                }
                "Supply" => return Ok(self.make_supply_instance()),
                "Supplier" | "Supplier::Preserving" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    let mut attrs = HashMap::new();
                    attrs.insert("emitted".to_string(), Value::array(Vec::new()));
                    attrs.insert("done".to_string(), Value::Bool(false));
                    attrs.insert(
                        "supplier_id".to_string(),
                        Value::Int(super::native_methods::next_supplier_id() as i64),
                    );
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                "ThreadPoolScheduler" | "CurrentThreadScheduler" | "Tap" | "Cancellation" => {
                    return Ok(Value::make_instance(*class_name, HashMap::new()));
                }
                "FakeScheduler" => {
                    // Shared single implementation with the VM's native fast path.
                    return Ok(Self::build_native_fakescheduler_value());
                }
                "Proxy" => {
                    // Shared single implementation with the VM's native fast path.
                    return Ok(Self::build_native_proxy_value(&args));
                }
                "CompUnit::DependencySpecification" => {
                    let mut short_name: Option<String> = None;
                    let mut auth_matcher: Option<String> = None;
                    let mut version_matcher: Option<String> = None;
                    let mut api_matcher: Option<String> = None;
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            match key.as_str() {
                                "short-name" => {
                                    if let Value::Str(s) = value.as_ref() {
                                        short_name = Some(s.to_string());
                                    } else {
                                        return Err(RuntimeError::new(
                                            "CompUnit::DependencySpecification.new: :short-name must be a Str",
                                        ));
                                    }
                                }
                                "auth-matcher" => {
                                    if !matches!(value.as_ref(), Value::Bool(true)) {
                                        auth_matcher = Some(value.to_string_value());
                                    }
                                }
                                "version-matcher" => {
                                    if !matches!(value.as_ref(), Value::Bool(true)) {
                                        version_matcher = Some(value.to_string_value());
                                    }
                                }
                                "api-matcher" if !matches!(value.as_ref(), Value::Bool(true)) => {
                                    api_matcher = Some(value.to_string_value());
                                }
                                _ => {}
                            }
                        }
                    }
                    let short_name = short_name.ok_or_else(|| {
                        RuntimeError::new(
                            "CompUnit::DependencySpecification.new: :short-name is required",
                        )
                    })?;
                    if auth_matcher.is_some() || version_matcher.is_some() || api_matcher.is_some()
                    {
                        let mut attrs = HashMap::new();
                        attrs.insert("short-name".to_string(), Value::str(short_name));
                        if let Some(a) = auth_matcher {
                            attrs.insert("auth-matcher".to_string(), Value::str(a));
                        }
                        if let Some(v) = version_matcher {
                            attrs.insert("version-matcher".to_string(), Value::str(v));
                        }
                        if let Some(a) = api_matcher {
                            attrs.insert("api-matcher".to_string(), Value::str(a));
                        }
                        return Ok(Value::make_instance(
                            Symbol::intern("CompUnit::DependencySpecification"),
                            attrs,
                        ));
                    }
                    return Ok(Value::CompUnitDepSpec {
                        short_name: Symbol::intern(&short_name),
                    });
                }
                "Distribution::Path" => {
                    let dir_path = args
                        .first()
                        .map(Value::to_string_value)
                        .unwrap_or_else(|| ".".to_string());
                    let meta_path = std::path::Path::new(&dir_path).join("META6.json");
                    if !meta_path.exists() {
                        return Err(RuntimeError::new(format!(
                            "No meta file located at {}",
                            meta_path.display()
                        )));
                    }
                    let meta_json = std::fs::read_to_string(&meta_path).map_err(|e| {
                        RuntimeError::new(format!("Cannot read {}: {e}", meta_path.display()))
                    })?;
                    let meta_hash = self.parse_json_to_value(&meta_json)?;
                    let files_hash = self.build_dist_files_hash(&dir_path, &meta_hash);
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(&dir_path));
                    attrs.insert("meta".to_string(), meta_hash);
                    attrs.insert("files".to_string(), files_hash);
                    return Ok(Value::make_instance(
                        Symbol::intern("Distribution::Path"),
                        attrs,
                    ));
                }
                "Distribution::Hash" => {
                    let mut meta_hash = Value::Nil;
                    let mut prefix = String::new();
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "prefix" => {
                                prefix = value.to_string_value();
                            }
                            Value::Hash(_) => {
                                if meta_hash == Value::Nil {
                                    meta_hash = arg.clone();
                                }
                            }
                            _ => {
                                if meta_hash == Value::Nil {
                                    meta_hash = arg.clone();
                                }
                            }
                        }
                    }
                    let files_hash = self.build_dist_files_hash(&prefix, &meta_hash);
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
                    attrs.insert("meta".to_string(), meta_hash);
                    attrs.insert("files".to_string(), files_hash);
                    return Ok(Value::make_instance(
                        Symbol::intern("Distribution::Hash"),
                        attrs,
                    ));
                }
                "CompUnit::Repository::Installation" => {
                    let mut prefix = String::new();
                    for arg in &args {
                        if let Value::Pair(key, value) = arg
                            && key == "prefix"
                        {
                            prefix = value.to_string_value();
                        }
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
                    attrs.insert("short-id".to_string(), Value::str_from("inst"));
                    return Ok(Value::make_instance(
                        Symbol::intern("CompUnit::Repository::Installation"),
                        attrs,
                    ));
                }
                "CompUnit::Repository::FileSystem" => {
                    let mut prefix = ".".to_string();
                    for arg in &args {
                        if let Value::Pair(key, value) = arg
                            && key == "prefix"
                        {
                            prefix = value.to_string_value();
                        }
                    }
                    let prefix_path = if prefix.is_empty() { "." } else { &prefix };
                    let canonical_prefix = std::fs::canonicalize(prefix_path)
                        .unwrap_or_else(|_| std::path::PathBuf::from(prefix_path))
                        .to_string_lossy()
                        .to_string();
                    let cache_key = format!("__mutsu_repo_fs::{}", canonical_prefix);
                    if let Some(existing) = self.env.get(&cache_key).cloned() {
                        return Ok(existing);
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "prefix".to_string(),
                        self.make_io_path_instance(&canonical_prefix),
                    );
                    attrs.insert("short-id".to_string(), Value::str_from("file"));
                    attrs.insert("__mutsu_precomp_enabled".to_string(), Value::Bool(false));
                    let repo = Value::make_instance(*class_name, attrs);
                    self.env.insert(cache_key, repo.clone());
                    return Ok(repo);
                }
                "Proc::Async" => {
                    let mut positional = Vec::new();
                    let mut w_flag = false;
                    let mut enc = Value::str_from("utf-8");
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "w" => {
                                w_flag = value.truthy();
                            }
                            Value::Pair(key, _value) if key == "out" => {}
                            Value::Pair(key, value) if key == "enc" => {
                                enc = Value::str(value.to_string_value());
                            }
                            _ => positional.push(arg.clone()),
                        }
                    }
                    let stdout_id = super::native_methods::next_supply_id();
                    let stderr_id = super::native_methods::next_supply_id();
                    let supply_id = super::native_methods::next_supply_id();
                    let stdout_descriptor = SharedPromise::new();
                    let stderr_descriptor = SharedPromise::new();
                    let mut stdout_supply_attrs = HashMap::new();
                    stdout_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stdout_id as i64));
                    stdout_supply_attrs.insert("enc".to_string(), enc.clone());
                    stdout_supply_attrs.insert(
                        "native_descriptor_promise".to_string(),
                        Value::Promise(stdout_descriptor),
                    );
                    let mut stderr_supply_attrs = HashMap::new();
                    stderr_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stderr_id as i64));
                    stderr_supply_attrs.insert("enc".to_string(), enc.clone());
                    stderr_supply_attrs.insert(
                        "native_descriptor_promise".to_string(),
                        Value::Promise(stderr_descriptor),
                    );
                    let mut merged_supply_attrs = HashMap::new();
                    merged_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    merged_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    merged_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(supply_id as i64));

                    let mut attrs = HashMap::new();
                    attrs.insert("cmd".to_string(), Value::array(positional));
                    attrs.insert("started".to_string(), Value::Bool(false));
                    attrs.insert("enc".to_string(), enc);
                    attrs.insert(
                        "stdout".to_string(),
                        Value::make_instance(Symbol::intern("Supply"), stdout_supply_attrs),
                    );
                    attrs.insert(
                        "stderr".to_string(),
                        Value::make_instance(Symbol::intern("Supply"), stderr_supply_attrs),
                    );
                    attrs.insert(
                        "supply".to_string(),
                        Value::make_instance(Symbol::intern("Supply"), merged_supply_attrs),
                    );
                    if w_flag {
                        attrs.insert("w".to_string(), Value::Bool(true));
                    }
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                "utf8" | "utf16" => {
                    // Shared with the VM's native fast path (pure code-unit build).
                    return Ok(Self::build_native_utf_value(*class_name, &args));
                }
                "Buf" | "buf8" | "Buf[uint8]" | "Blob" | "blob8" | "Blob[uint8]" | "buf16"
                | "buf32" | "buf64" | "blob16" | "blob32" | "blob64" => {
                    // Shared with the VM's native fast path (the byte-overlay
                    // build is pure data; see `build_native_buf_value`).
                    return Ok(Self::build_native_buf_value(*class_name, &args));
                }
                "Rat" => {
                    // Shared with the VM's native fast path (pure component build).
                    return Ok(Self::build_native_rat_value(&args));
                }
                "FatRat" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_fatrat_value(&args));
                }
                "Pair" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_pair_value(&args));
                }
                "Set" | "SetHash" => {
                    // Native QuantHash construction — single impl shared with the
                    // VM's `.new` fast path (`try_native_quanthash_construct`).
                    return self.try_native_quanthash_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        args,
                    );
                }
                "Bag" | "BagHash" => {
                    // Native QuantHash construction — single impl shared with the
                    // VM's `.new` fast path (`try_native_quanthash_construct`).
                    return self.try_native_quanthash_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        args,
                    );
                }
                "Mix" | "MixHash" => {
                    // Native QuantHash construction — single impl shared with the
                    // VM's `.new` fast path (`try_native_quanthash_construct`).
                    return self.try_native_quanthash_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        args,
                    );
                }
                "Complex" => {
                    // Shared with the VM's native fast path (pure component build).
                    return Ok(Self::build_native_complex_value(&args));
                }
                "Backtrace" => {
                    let file = self
                        .env
                        .get("?FILE")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let mut frame_attrs = HashMap::new();
                    frame_attrs.insert("file".to_string(), Value::str(file));
                    let frame =
                        Value::make_instance(Symbol::intern("Backtrace::Frame"), frame_attrs);
                    return Ok(Value::array(vec![frame]));
                }
                "Lock" | "Lock::Async" | "Lock::Soft" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    let mut attrs = HashMap::new();
                    let lock_id = super::native_methods::next_lock_id() as i64;
                    attrs.insert("lock-id".to_string(), Value::Int(lock_id));
                    if class_name.resolve() == "Lock::Async" {
                        attrs.insert("async".to_string(), Value::Bool(true));
                    }
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                "Slip" => {
                    // Shared with the VM's native fast path.
                    return Ok(Value::slip(args.to_vec()));
                }
                "Match" => {
                    // Shared single implementation with the VM's native fast path.
                    return Ok(Self::build_native_match_value(&args));
                }
                // Types that cannot be instantiated with .new
                "HyperWhatever" | "Whatever" | "Instant" => {
                    return Err(RuntimeError::new(format!(
                        "X::Cannot::New: Cannot create new object of type {}",
                        class_name
                    )));
                }
                "Junction" => {
                    // Junction.new(values, :type<kind>)
                    // or Junction.new("kind", values)
                    // Extract named :type from Pair args
                    let mut type_str: Option<String> = None;
                    let mut positional: Vec<&Value> = Vec::new();
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            if key == "type" {
                                type_str = Some(value.to_string_value());
                            }
                        } else {
                            positional.push(arg);
                        }
                    }
                    // If no named :type, check if first positional is a type string
                    let type_name = if let Some(t) = type_str {
                        t
                    } else if positional.len() >= 2 {
                        if let Value::Str(s) = positional[0] {
                            let name = s.to_string();
                            positional.remove(0);
                            name
                        } else {
                            "any".to_string()
                        }
                    } else {
                        "any".to_string()
                    };
                    let kind = match type_name.as_str() {
                        "all" => JunctionKind::All,
                        "one" => JunctionKind::One,
                        "none" => JunctionKind::None,
                        _ => JunctionKind::Any,
                    };
                    let values_arg = positional.first().copied();
                    let elems: Vec<Value> = match values_arg {
                        Some(Value::Array(items, ..)) => items.to_vec(),
                        Some(Value::Seq(items)) | Some(Value::Slip(items)) => items.to_vec(),
                        Some(other) => vec![other.clone()],
                        None => vec![],
                    };
                    return Ok(Value::junction(kind, elems));
                }
                _ => {}
            }
            // Parametric package handling (e.g. Array[Int], Hash[Int,Str], A[Int]).
            if let Some(type_args) = type_args.as_ref() {
                if matches!(base_class_name, "Array" | "List" | "Positional" | "array") {
                    if let Some(dims) = self.shaped_dims_from_new_args(&args)? {
                        let data = args.iter().find_map(|arg| match arg {
                            Value::Pair(name, value) if name == "data" => {
                                Some(value.as_ref().clone())
                            }
                            _ => None,
                        });
                        // If no :data, collect positional (non-Pair) args as data
                        let data = if data.is_none() {
                            let positional: Vec<Value> = args
                                .iter()
                                .filter(|arg| !matches!(arg, Value::Pair(..)))
                                .cloned()
                                .collect();
                            if positional.is_empty() {
                                None
                            } else {
                                Some(Value::array(positional))
                            }
                        } else {
                            data
                        };
                        let shaped = Self::make_shaped_array(&dims);
                        let mut result = if let Some(data_val) = data {
                            let data_items = match data_val {
                                Value::Array(items, ..) => items.to_vec(),
                                Value::Seq(items) | Value::Slip(items) => items.to_vec(),
                                other => vec![other],
                            };
                            if let Value::Array(ref items, is_arr) = shaped {
                                let mut new_items = items.as_ref().clone();
                                for (i, val) in data_items.into_iter().enumerate() {
                                    if i < new_items.len() {
                                        new_items[i] = val;
                                    }
                                }
                                let result = Value::Array(std::sync::Arc::new(new_items), is_arr);
                                crate::runtime::utils::mark_shaped_array(&result, Some(&dims));
                                result
                            } else {
                                shaped
                            }
                        } else {
                            shaped
                        };
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        result = self.tag_container_metadata(
                            result,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(if base_class_name == "array" {
                                    format!("array[{}]", type_args[0])
                                } else {
                                    class_name.resolve()
                                }),
                            },
                        );
                        return Ok(result);
                    }
                    let mut items = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Slip(vals) => items.extend(vals.iter().cloned()),
                            other => items.push(other.clone()),
                        }
                    }
                    let mut result = if matches!(base_class_name, "Array" | "array") {
                        Value::real_array(items)
                    } else {
                        Value::array(items)
                    };
                    let value_type = type_args
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Any".to_string());
                    result = self.tag_container_metadata(
                        result,
                        crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type: None,
                            declared_type: Some(if base_class_name == "array" {
                                format!("array[{}]", type_args[0])
                            } else {
                                class_name.resolve()
                            }),
                        },
                    );
                    return Ok(result);
                }
                if matches!(base_class_name, "Hash" | "Map") {
                    let mut flat = Vec::new();
                    for arg in &args {
                        flat.extend(Self::value_to_list(arg));
                    }
                    let mut map = HashMap::new();
                    let mut iter = flat.into_iter();
                    while let Some(item) = iter.next() {
                        match item {
                            Value::Pair(k, v) => {
                                map.insert(k, *v);
                            }
                            Value::ValuePair(k, v) => {
                                map.insert(k.to_string_value(), *v);
                            }
                            other => {
                                let key = other.to_string_value();
                                let value = iter.next().unwrap_or(Value::Nil);
                                map.insert(key, value);
                            }
                        }
                    }
                    let result = Value::hash(map);
                    let value_type = type_args
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Any".to_string());
                    let key_type = type_args.get(1).cloned();
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type,
                        key_type,
                        declared_type: Some(class_name.resolve()),
                    };
                    return Ok(self.tag_container_metadata(result, info));
                }
            }
            // Hoist clone to a `let` so the guard drops before the body re-enters
            // (eval_block_value for attribute defaults).
            let role = self.registry().roles.get(&class_name.resolve()).cloned();
            if let Some(role) = role {
                // Check for attribute conflicts detected during role composition
                if let Some((attr_name, role_a, role_b)) = role.attribute_conflicts.first() {
                    return Err(RuntimeError::new(format!(
                        "Attribute '$!{}' conflicts in role '{}' composition: declared in both '{}' and '{}'",
                        attr_name,
                        class_name.resolve(),
                        role_a,
                        role_b
                    )));
                }
                let mut named_args: HashMap<String, Value> = HashMap::new();
                let mut positional_args: Vec<Value> = Vec::new();
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        named_args.insert(key.clone(), *value.clone());
                    } else {
                        positional_args.push(arg.clone());
                    }
                }

                // Collect attributes from this role and all composed parent roles
                let mut all_attributes = role.attributes.clone();
                if let Some(parent_names) = self
                    .registry()
                    .role_parents
                    .get(&class_name.resolve())
                    .cloned()
                {
                    let mut role_stack: Vec<String> = parent_names;
                    let mut visited = vec![class_name.resolve()];
                    while let Some(parent_role_name) = role_stack.pop() {
                        if !visited.contains(&parent_role_name) {
                            visited.push(parent_role_name.clone());
                            if let Some(parent_role) =
                                self.registry().roles.get(&parent_role_name).cloned()
                            {
                                for attr in &parent_role.attributes {
                                    if !all_attributes.iter().any(|a| a.0 == attr.0) {
                                        all_attributes.push(attr.clone());
                                    }
                                }
                            }
                            if let Some(grandparents) =
                                self.registry().role_parents.get(&parent_role_name).cloned()
                            {
                                for gp_name in &grandparents {
                                    if !visited.contains(gp_name) {
                                        role_stack.push(gp_name.clone());
                                    }
                                }
                            }
                        }
                    }
                }

                let mut mixins = HashMap::new();
                mixins.insert(format!("__mutsu_role__{}", class_name), Value::Bool(true));
                for (idx, (attr_name, _is_public, default_expr, _, _, _, _)) in
                    all_attributes.iter().enumerate()
                {
                    let value = if let Some(v) = named_args.get(attr_name) {
                        v.clone()
                    } else if let Some(v) = positional_args.get(idx) {
                        v.clone()
                    } else if let Some(expr) = default_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                    } else {
                        Value::Nil
                    };
                    mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
                }
                // Embed language revision from the matching candidate
                // (no-params for bare role punning) so ^language-revision
                // returns the correct value for this role's origin module.
                let cn_str = class_name.resolve();
                let bare_lang_ver = self
                    .registry()
                    .role_candidates
                    .get(&cn_str)
                    .and_then(|candidates| {
                        candidates
                            .iter()
                            .find(|c| c.type_params.is_empty())
                            .map(|c| c.language_version.clone())
                    })
                    .or_else(|| {
                        self.type_metadata
                            .get(&cn_str)
                            .and_then(|m| m.get("language-revision"))
                            .map(|v| format!("6.{}", v.to_string_value()))
                    });
                if let Some(ver) = bare_lang_ver {
                    let revision: String = if let Some(letter) = ver.strip_prefix("6.") {
                        letter.chars().next().unwrap_or('c').to_string()
                    } else {
                        "c".to_string()
                    };
                    mixins.insert(
                        "__mutsu_language_revision".to_string(),
                        Value::str(revision),
                    );
                }
                return Ok(Value::mixin(
                    Value::make_instance(*class_name, HashMap::new()),
                    mixins,
                ));
            }
            // CUnion repr classes use byte-overlay construction
            if self.registry().cunion_classes.contains(&cn_resolved) {
                return self.construct_cunion_instance(&cn_resolved, &args);
            }
            // Auto-pun role to class if needed (e.g., role COERCE calling self.new)
            if !self.registry().classes.contains_key(&cn_resolved)
                && self.registry().roles.contains_key(&cn_resolved)
            {
                self.ensure_role_punned_to_class(&cn_resolved);
            }
            if self.registry().classes.contains_key(&cn_resolved)
                || type_args
                    .as_ref()
                    .is_some_and(|_| self.registry().classes.contains_key(base_class_name))
            {
                let class_key = if self.registry().classes.contains_key(&cn_resolved) {
                    cn_resolved.as_str()
                } else {
                    base_class_name
                };
                // Check if this class is a Proxy subclass
                {
                    let mro = self.class_mro(class_key);
                    if mro.iter().any(|c| c == "Proxy") {
                        return self.construct_proxy_subclass(class_key, &args);
                    }
                }
                // Semaphore takes a positional permits argument; build the
                // instance directly using the semaphore registry.
                if class_key == "Semaphore" {
                    let permits = args
                        .first()
                        .map(|v| match v {
                            Value::Int(i) => *i,
                            Value::Num(f) => *f as i64,
                            other => other.to_string_value().parse::<i64>().unwrap_or(1),
                        })
                        .unwrap_or(1)
                        .max(0);
                    let mut attrs = HashMap::new();
                    attrs.insert("permits".to_string(), Value::Int(permits));
                    attrs.insert(
                        "semaphore-id".to_string(),
                        Value::Int(super::native_methods::next_semaphore_id(permits) as i64),
                    );
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                // Check for user-defined .new method first
                if self.has_user_method(class_key, "new") {
                    let empty_attrs = HashMap::new();
                    match self.run_instance_method(
                        &class_name.resolve(),
                        empty_attrs,
                        "new",
                        args.clone(),
                        None,
                    ) {
                        Ok((result, _updated)) => return Ok(result),
                        Err(e) => {
                            // If multi dispatch failed (no matching candidate),
                            // fall through to the built-in Mu.new default constructor.
                            // This matches Raku's behavior where Mu.new(*%attrinit) is
                            // always available as a fallback multi candidate.
                            if !e.is_multi_no_match() {
                                return Err(e);
                            }
                            // Mu.new only accepts named arguments. If the call
                            // had positional args and no multi candidate matched,
                            // die like Raku does.
                            let has_positional = args
                                .iter()
                                .any(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)));
                            if has_positional {
                                // Check if this class composes Baggy/Setty role;
                                // if so, redirect to Bag-like construction
                                let cn = class_name.resolve();
                                if self.class_does_baggy_or_setty(&cn) {
                                    return self.construct_baggy_instance(&cn, &args);
                                }
                                return Err(constructor_positional_error(&class_name.resolve()));
                            }
                            // Fall through to default constructor below
                        }
                    }
                }
                let mut attrs = HashMap::new();
                let mut positional_ctor_args: Vec<Value> = Vec::new();
                let saved_default_env = self.env.clone();
                let role_bindings = {
                    let registry = self.registry();
                    registry
                        .class_role_param_bindings
                        .get(class_key)
                        .or_else(|| {
                            registry
                                .class_role_param_bindings
                                .get(&class_name.resolve())
                        })
                        .cloned()
                };
                if let Some(role_bindings) = role_bindings {
                    for (name, value) in &role_bindings {
                        self.env.insert(name.clone(), value.clone());
                    }
                }
                let class_attrs_info = self.collect_class_attributes(class_key);
                // Check required attributes BEFORE evaluating defaults
                // (required attributes are checked before defaults run)
                let provided_attr_names: std::collections::HashSet<String> = args
                    .iter()
                    .filter_map(|v| match v {
                        Value::Pair(k, _) => Some(k.clone()),
                        _ => None,
                    })
                    .collect();
                for (attr_name, _is_public, _default, _is_rw, is_required, _sigil, _) in
                    &class_attrs_info
                {
                    if let Some(reason) = is_required {
                        let has_build = self.class_has_method(class_key, "BUILD");
                        // If class has BUILD, BUILD handles attribute setting,
                        // so we skip required check here (BUILD may set defaults)
                        if !has_build && !provided_attr_names.contains(attr_name.as_str()) {
                            let attr_full_name = format!("$!{}", attr_name);
                            return Err(RuntimeError::attribute_required(
                                &attr_full_name,
                                reason.as_deref(),
                            ));
                        }
                    }
                }
                // Build a sigil map for later coercion
                let sigil_map: HashMap<String, char> = class_attrs_info
                    .iter()
                    .map(|(name, _, _, _, _, sigil, _)| (name.clone(), *sigil))
                    .collect();
                // Attribute type constraints (MRO-wide), used to coerce a provided
                // value for a coercion-typed attribute (`has Int() $.x`).
                let attr_type_constraints = self.collect_attribute_type_constraints(class_key);
                // First, collect constructor args into attrs
                self.env = saved_default_env.clone();
                let class_mro = self.class_mro(class_key);
                // When BUILD is defined, it controls attribute initialization,
                // so we skip automatic named-arg-to-attribute mapping.
                let any_build = class_mro.iter().any(|cn| {
                    cn != "Any"
                        && cn != "Mu"
                        && self
                            .registry()
                            .classes
                            .get(cn)
                            .and_then(|def| def.methods.get("BUILD"))
                            .is_some()
                });
                for val in &args {
                    match val {
                        Value::Pair(k, v) => {
                            if !any_build && self.is_attribute_buildable(class_key, k) {
                                let sigil = sigil_map.get(k).copied().unwrap_or('$');
                                let mut value = *v.clone();
                                // A coercion-typed attribute (`has Int() $.x`)
                                // coerces its provided value through the target
                                // type (built-in coercion or a user COERCE method).
                                if sigil == '$'
                                    && let Some(tc) = attr_type_constraints.get(k)
                                    && crate::runtime::types::is_coercion_constraint(tc)
                                {
                                    value = self.coerce_value_for_constraint(tc, value);
                                }
                                let coerced = Self::coerce_attr_value_by_sigil(value, sigil);
                                // An `is Type` container attribute (`has @.a is
                                // Buf`) coerces its provided value to the declared
                                // container type (Buf, BagHash, Array[T], ...).
                                let coerced = if matches!(sigil, '@' | '%')
                                    && let Some(type_name) =
                                        self.attribute_is_type_in_mro(class_key, k)
                                {
                                    self.coerce_value_to_is_type(&type_name, sigil, coerced)?
                                } else {
                                    coerced
                                };
                                attrs.insert(k.clone(), coerced);
                            }
                            // When BUILD exists, named args are passed to BUILD
                            // which controls attribute initialization directly
                        }
                        Value::Instance {
                            class_name: src_class,
                            attributes: src_attrs,
                            ..
                        } if class_mro.iter().any(|name| name == &src_class.resolve()) => {
                            for (attr, value) in src_attrs.as_map().iter() {
                                attrs.insert(attr.clone(), value.clone());
                            }
                        }
                        _ => {
                            positional_ctor_args.push(val.clone());
                        }
                    }
                }
                // Mu.new only accepts named arguments. If positional args
                // were passed and this is not a subclass that accepts them, reject.
                if !positional_ctor_args.is_empty() {
                    // Check if this class composes Baggy/Setty role;
                    // if so, redirect to Bag-like construction
                    let cn = class_name.resolve();
                    if self.class_does_baggy_or_setty(&cn) {
                        return self.construct_baggy_instance(&cn, &args);
                    }
                    let accepts_positional = class_mro
                        .iter()
                        .any(|n| n == "Array" || n == "Int" || n == "Num" || n == "Hash");
                    if !accepts_positional {
                        return Err(constructor_positional_error(&class_name.resolve()));
                    }
                }
                // For @-sigiled attributes with shaped array declarations,
                // convert user-provided values to shaped arrays preserving shape.
                for (attr_name, _is_public, default, _is_rw, _is_required, sigil, _) in
                    &class_attrs_info
                {
                    if *sigil == '@'
                        && let Some(dims) = Self::extract_shape_from_default(default.as_ref())
                        && let Some(val) = attrs.get(attr_name)
                        && !matches!(val, Value::Array(_, ArrayKind::Shaped))
                    {
                        let items = match val {
                            Value::Array(items, _) => (**items).clone(),
                            _ => crate::value::ArrayData::new(vec![val.clone()]),
                        };
                        let shaped = Value::Array(std::sync::Arc::new(items), ArrayKind::Shaped);
                        crate::runtime::utils::mark_shaped_array(&shaped, Some(&dims));
                        attrs.insert(attr_name.clone(), shaped);
                    }
                }
                self.enforce_attribute_where_constraints(class_key, &class_attrs_info, &attrs)?;
                self.enforce_attribute_smiley_constraints(class_key, &attrs)?;
                let int_ctor_val =
                    if matches!(positional_ctor_args.first(), Some(Value::Package(_))) {
                        return Err(RuntimeError::new("Cannot convert type object to Int"));
                    } else {
                        positional_ctor_args
                            .first()
                            .map_or(0, crate::runtime::to_int)
                    };
                if class_mro.iter().any(|name| name == "Array")
                    && !attrs.contains_key("__array_items")
                    && !positional_ctor_args.is_empty()
                {
                    attrs.insert(
                        "__array_items".to_string(),
                        Value::array(positional_ctor_args),
                    );
                }
                if class_mro.iter().any(|name| name == "Int")
                    && !attrs.contains_key("__mutsu_int_value")
                {
                    attrs.insert("__mutsu_int_value".to_string(), Value::Int(int_ctor_val));
                }
                // Then evaluate defaults for attributes not provided by args,
                // binding `self` so default expressions like `self.x` work.
                // Restore role parameter bindings so that default expressions
                // referencing role type parameters (e.g., `has $.x = $a`) work.
                // Also add class-qualified versions (e.g., `AP_2::a`) so that
                // bindings resolve correctly when current_package is the class.
                let role_bindings = {
                    let registry = self.registry();
                    registry
                        .class_role_param_bindings
                        .get(class_key)
                        .or_else(|| {
                            registry
                                .class_role_param_bindings
                                .get(&class_name.resolve())
                        })
                        .cloned()
                };
                if let Some(role_bindings) = role_bindings {
                    for (name, value) in &role_bindings {
                        self.env.insert(name.clone(), value.clone());
                        self.env
                            .insert(format!("{}::{}", class_key, name), value.clone());
                    }
                }
                for (attr_name, _is_public, default, _is_rw, _is_required, sigil, _) in
                    class_attrs_info.clone()
                {
                    if attrs.contains_key(&attr_name) {
                        continue;
                    }
                    // Clone the override out and drop the registry guard before
                    // call_sub_value re-enters user code (RwLock is not reentrant).
                    let build_override = self
                        .registry()
                        .attribute_build_overrides
                        .get(&(class_key.to_string(), attr_name.clone()))
                        .cloned();
                    let val = if let Some(build_override) = build_override {
                        let val = self.call_sub_value(build_override, Vec::new(), false)?;
                        Self::coerce_attr_value_by_sigil(val, sigil)
                    } else if let Some(expr) = default {
                        // Fast path: simple literal defaults (e.g. from native types
                        // like `has uint32 $.a` which generate `default: Int(0)`)
                        // don't need env manipulation or self-binding.
                        if let Expr::Literal(ref lit_val) = expr {
                            Self::coerce_attr_value_by_sigil(lit_val.clone(), sigil)
                        } else {
                            let temp_self = Value::make_instance(*class_name, attrs.clone());
                            let old_self = self.env.get("self").cloned();
                            self.env.insert("self".to_string(), temp_self);
                            // Set !attr_name and .attr_name in env so that $!a / $.a
                            // references in default expressions resolve to already-
                            // initialized attributes (e.g. `has $.c = $!a + $!b`).
                            let mut saved_attr_env: Vec<(String, Option<Value>)> = Vec::new();
                            for (a_name, a_val) in &attrs {
                                let bang = format!("!{}", a_name);
                                let dot = format!(".{}", a_name);
                                saved_attr_env.push((bang.clone(), self.env.get(&bang).cloned()));
                                saved_attr_env.push((dot.clone(), self.env.get(&dot).cloned()));
                                self.env.insert(bang, a_val.clone());
                                self.env.insert(dot, a_val.clone());
                            }
                            // Temporarily switch to the class package so that
                            // class-scoped subs (e.g. `sub inner`) are found
                            // when evaluating attribute default expressions.
                            let saved_package = self.current_package();
                            self.set_current_package(class_key.to_string());
                            let result = self.eval_block_value(&[Stmt::Expr(expr)]);
                            self.set_current_package(saved_package);
                            // Restore previous env state for attribute variables
                            for (key, old_val) in saved_attr_env {
                                if let Some(v) = old_val {
                                    self.env.insert(key, v);
                                } else {
                                    self.env.remove(&key);
                                }
                            }
                            if let Some(old) = old_self {
                                self.env.insert("self".to_string(), old);
                            } else {
                                self.env.remove("self");
                            }
                            let val = result?;
                            Self::coerce_attr_value_by_sigil(val, sigil)
                        }
                    } else {
                        match sigil {
                            '@' => {
                                // Check for `is Type` trait (e.g. `has @.a is Buf`)
                                let is_type = self
                                    .registry()
                                    .class_attribute_is_types
                                    .get(&(class_key.to_string(), attr_name.clone()))
                                    .cloned();
                                if let Some(type_name) = is_type {
                                    // For a parameterized container type (`is Array[Rat]`),
                                    // build the empty array directly with type metadata so
                                    // `.WHAT` / `~~ Array[Rat]` see the declared element type
                                    // (a `Package` built from the string name loses its
                                    // type parameter when `.new` is dispatched).
                                    if let Some(inner) = type_name
                                        .strip_prefix("Array[")
                                        .or_else(|| type_name.strip_prefix("array["))
                                        .and_then(|s| s.strip_suffix(']'))
                                    {
                                        let mut arr = Value::real_array(Vec::new());
                                        arr = self.tag_container_metadata(
                                            arr,
                                            super::ContainerTypeInfo {
                                                value_type: inner.trim().to_string(),
                                                key_type: None,
                                                declared_type: Some(type_name.clone()),
                                            },
                                        );
                                        arr
                                    } else {
                                        let type_obj = Value::Package(
                                            crate::symbol::Symbol::intern(&type_name),
                                        );
                                        match self.call_method_with_values(type_obj, "new", vec![])
                                        {
                                            Ok(v) => v,
                                            Err(_) => Value::real_array(Vec::new()),
                                        }
                                    }
                                } else {
                                    let mut arr = Value::real_array(Vec::new());
                                    // Register element type constraint for typed array attributes
                                    let tc = self
                                        .registry()
                                        .classes
                                        .get(class_key)
                                        .and_then(|cd| cd.attribute_types.get(&attr_name))
                                        .cloned();
                                    if let Some(tc) = tc {
                                        arr = self.tag_container_metadata(
                                            arr,
                                            super::ContainerTypeInfo {
                                                value_type: tc,
                                                key_type: None,
                                                declared_type: None,
                                            },
                                        );
                                    }
                                    arr
                                }
                            }
                            '%' => {
                                // Check for `is Type` trait (e.g. `has %.h is BagHash`)
                                let is_type = self
                                    .registry()
                                    .class_attribute_is_types
                                    .get(&(class_key.to_string(), attr_name.clone()))
                                    .cloned();
                                if let Some(type_name) = is_type {
                                    let type_obj =
                                        Value::Package(crate::symbol::Symbol::intern(&type_name));
                                    match self.call_method_with_values(type_obj, "new", vec![]) {
                                        Ok(v) => v,
                                        Err(_) => Value::hash(HashMap::new()),
                                    }
                                } else {
                                    let h = Value::hash(HashMap::new());
                                    // Register value type constraint for typed hash attributes
                                    let tc = self
                                        .registry()
                                        .classes
                                        .get(class_key)
                                        .and_then(|cd| cd.attribute_types.get(&attr_name))
                                        .cloned();
                                    if let Some(tc) = tc {
                                        self.tag_container_metadata(
                                            h,
                                            super::ContainerTypeInfo {
                                                value_type: tc,
                                                key_type: None,
                                                declared_type: None,
                                            },
                                        )
                                    } else {
                                        h
                                    }
                                }
                            }
                            _ => Value::Nil,
                        }
                    };
                    // A coercion-typed attribute (`has Int() $.x = "42"`) coerces
                    // its evaluated default through the target type, just like a
                    // provided value. (The bare type-object default for an
                    // uninitialized coercion attribute coerces to itself.)
                    let val = if sigil == '$'
                        && let Some(tc) = attr_type_constraints.get(&attr_name)
                        && crate::runtime::types::is_coercion_constraint(tc)
                    {
                        self.coerce_value_for_constraint(tc, val)
                    } else {
                        val
                    };
                    attrs.insert(attr_name, val);
                }
                // Add alias metadata for `has $x` (no twigil) attributes
                self.add_alias_attribute_metadata(class_key, &mut attrs);
                self.enforce_attribute_where_constraints(class_key, &class_attrs_info, &attrs)?;
                self.enforce_attribute_smiley_constraints(class_key, &attrs)?;
                // Restore env after default evaluation, but preserve side effects
                // on variables that already existed in the caller environment.
                let mut restored_env = saved_default_env.clone();
                for (key, value) in self.env.iter() {
                    if restored_env.contains_key_sym(*key) {
                        restored_env.insert_sym(*key, value.clone());
                    }
                }
                self.env = restored_env;
                // Walk MRO in reverse (base-first) and call BUILD/TWEAK
                // submethods defined directly on each class. Submethods are
                // NOT inherited, so each class's own BUILD/TWEAK is called
                // independently with the construction args.
                // Under v6.e.PREVIEW+, role submethods are also called
                // (roles' BUILD/TWEAK before the class's own).
                let mro = self.class_mro(class_key);
                // Determine the class's language revision for submethod dispatch rules.
                let class_lang_rev = self
                    .type_metadata
                    .get(&class_name.resolve())
                    .and_then(|m| m.get("language-revision"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| {
                        let version = crate::parser::current_language_version();
                        if let Some(rest) = version.strip_prefix("6.") {
                            rest.chars().next().unwrap_or('c').to_string()
                        } else {
                            "c".to_string()
                        }
                    });
                let class_is_6e = class_lang_rev != "c";
                for mro_class in mro.iter().rev() {
                    if mro_class == "Any" || mro_class == "Mu" {
                        continue;
                    }
                    // Skip role entries in MRO — they are handled separately below
                    if self.registry().roles.contains_key(mro_class)
                        && !self.registry().classes.contains_key(mro_class)
                    {
                        continue;
                    }
                    // Check if the class has its own BUILD (not from a role)
                    let class_has_own_build = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("BUILD"))
                        .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                        .unwrap_or(false);
                    // Call BUILD submethods from composed roles
                    let role_order = self.ordered_role_submethods_for_class(mro_class, "BUILD");
                    for (role_name, method_def) in role_order {
                        let role_base = role_name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(&role_name);
                        let role_lang_rev = self
                            .type_metadata
                            .get(role_base)
                            .and_then(|m| m.get("language-revision"))
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "c".to_string());
                        // In 6.c class with own BUILD: skip 6.c role submethods
                        if !class_is_6e && class_has_own_build && role_lang_rev == "c" {
                            continue;
                        }
                        let (_v, updated) = self.run_instance_method_resolved(
                            &class_name.resolve(),
                            &role_name,
                            method_def,
                            attrs.clone(),
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        )?;
                        attrs = updated;
                    }
                    // Call the class's BUILD if it has one that wasn't already handled
                    // by ordered_role_submethods_for_class. Role submethods (is_my=true,
                    // role_origin=Some) were already called above.
                    let has_non_submethod_build = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("BUILD"))
                        .map(|overloads| {
                            overloads
                                .iter()
                                .any(|md| md.role_origin.is_none() || !md.is_my)
                        })
                        .unwrap_or(false);
                    if has_non_submethod_build {
                        match self.run_instance_method(
                            mro_class,
                            attrs.clone(),
                            "BUILD",
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        ) {
                            Ok((_v, updated)) => {
                                attrs = updated;
                            }
                            Err(err) if err.is_fail => {
                                // fail in BUILD: return a Failure wrapping the exception
                                let ex = if let Some(exception) = err.exception {
                                    *exception
                                } else {
                                    let mut ex_attrs = HashMap::new();
                                    ex_attrs.insert("message".to_string(), Value::str(err.message));
                                    Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs)
                                };
                                let mut failure_attrs = HashMap::new();
                                failure_attrs.insert("exception".to_string(), ex);
                                return Ok(Value::make_instance(
                                    Symbol::intern("Failure"),
                                    failure_attrs,
                                ));
                            }
                            Err(err) => return Err(err),
                        }
                    }
                }
                // Check required attributes after all BUILDs have run
                let any_build = mro.iter().any(|cn| {
                    cn != "Any"
                        && cn != "Mu"
                        && self
                            .registry()
                            .classes
                            .get(cn)
                            .and_then(|def| def.methods.get("BUILD"))
                            .is_some()
                });
                // Also check role BUILD submethods for required attribute enforcement
                let any_role_build = mro.iter().any(|cn| {
                    cn != "Any"
                        && cn != "Mu"
                        && !self
                            .ordered_role_submethods_for_class(cn, "BUILD")
                            .is_empty()
                });
                if any_build || any_role_build {
                    for (attr_name, _is_public, _default, _is_rw, is_required, _sigil, _) in
                        &class_attrs_info
                    {
                        if let Some(reason) = is_required {
                            let attr_val = attrs.get(attr_name.as_str());
                            let is_set = !matches!(attr_val, Some(Value::Nil) | None);
                            if !is_set {
                                let attr_full_name = format!("$!{}", attr_name);
                                return Err(RuntimeError::attribute_required(
                                    &attr_full_name,
                                    reason.as_deref(),
                                ));
                            }
                        }
                    }
                    self.enforce_attribute_where_constraints(class_key, &class_attrs_info, &attrs)?;
                    self.enforce_attribute_smiley_constraints(class_key, &attrs)?;
                }
                // Walk MRO in reverse for TWEAK as well
                for mro_class in mro.iter().rev() {
                    if mro_class == "Any" || mro_class == "Mu" {
                        continue;
                    }
                    // Skip role entries in MRO
                    if self.registry().roles.contains_key(mro_class)
                        && !self.registry().classes.contains_key(mro_class)
                    {
                        continue;
                    }
                    // Check if the class has its own TWEAK (not from a role)
                    let class_has_own_tweak = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("TWEAK"))
                        .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                        .unwrap_or(false);
                    // Call TWEAK submethods from composed roles (same rules as BUILD)
                    let role_order = self.ordered_role_submethods_for_class(mro_class, "TWEAK");
                    for (role_name, method_def) in role_order {
                        let role_base = role_name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(&role_name);
                        let role_lang_rev = self
                            .type_metadata
                            .get(role_base)
                            .and_then(|m| m.get("language-revision"))
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "c".to_string());
                        // In 6.c class with own TWEAK: skip 6.c role submethods
                        if !class_is_6e && class_has_own_tweak && role_lang_rev == "c" {
                            continue;
                        }
                        let (_v, updated) = self.run_instance_method_resolved(
                            &class_name.resolve(),
                            &role_name,
                            method_def,
                            attrs.clone(),
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        )?;
                        attrs = updated;
                        self.enforce_attribute_where_constraints(
                            class_key,
                            &class_attrs_info,
                            &attrs,
                        )?;
                    }
                    // Only call the class's own TWEAK (not role-composed ones).
                    let has_own_tweak = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("TWEAK"))
                        .map(|overloads| {
                            overloads
                                .iter()
                                .any(|md| md.role_origin.is_none() || !md.is_my)
                        })
                        .unwrap_or(false);
                    if has_own_tweak {
                        let (_v, updated) = self.run_instance_method(
                            mro_class,
                            attrs.clone(),
                            "TWEAK",
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        )?;
                        attrs = updated;
                        self.enforce_attribute_where_constraints(
                            class_key,
                            &class_attrs_info,
                            &attrs,
                        )?;
                    }
                }
                // Initialize per-class private attributes: when a parent and child
                // both declare an attribute with the same name, each class gets its
                // own copy stored under a qualified key ("ClassName\0attrName").
                // Methods access $!attr via the qualified key for their owner class.
                {
                    let per_class_attrs = self.collect_per_class_attrs(class_key);
                    // Named-arg keys explicitly passed to the constructor. In Raku
                    // each class's BUILD binds its own same-named attribute from the
                    // named args (a provided named arg wins over the class's own
                    // default), so EVERY declaring class's private copy gets that
                    // value — not just the most-derived one. The most-derived value
                    // already lives in the bare key, so reuse it.
                    let provided_keys: std::collections::HashSet<String> = args
                        .iter()
                        .filter_map(|a| match a {
                            Value::Pair(k, _) => Some(k.clone()),
                            _ => None,
                        })
                        .collect();
                    for (
                        declaring_class,
                        (attr_name, _is_public, default, _is_rw, _is_required, sigil, _),
                    ) in per_class_attrs
                    {
                        let qualified_key = format!("{}\0{}", declaring_class, attr_name);
                        if attrs.contains_key(&qualified_key) {
                            continue;
                        }
                        // Constructor named arg provided → initialize this class's
                        // copy from it (per-class BUILD), regardless of which class
                        // in the hierarchy declared it.
                        if provided_keys.contains(&attr_name)
                            && let Some(val) = attrs.get(&attr_name)
                        {
                            attrs.insert(qualified_key, val.clone());
                            continue;
                        }
                        // Use the unqualified value if it was provided via constructor args
                        // and this is the most-derived class declaring this attribute
                        if declaring_class == class_key
                            && let Some(val) = attrs.get(&attr_name)
                        {
                            attrs.insert(qualified_key, val.clone());
                            continue;
                        }
                        // Evaluate the default expression for this class's attribute
                        let val = if let Some(Expr::Literal(ref lit_val)) = default {
                            // Fast path: simple literal defaults
                            Self::coerce_attr_value_by_sigil(lit_val.clone(), sigil)
                        } else if let Some(expr) = default {
                            let temp_self = Value::make_instance(*class_name, attrs.clone());
                            let old_self = self.env.get("self").cloned();
                            self.env.insert("self".to_string(), temp_self);
                            let result = self.eval_block_value(&[Stmt::Expr(expr)]);
                            if let Some(old) = old_self {
                                self.env.insert("self".to_string(), old);
                            } else {
                                self.env.remove("self");
                            }
                            match result {
                                Ok(v) => Self::coerce_attr_value_by_sigil(v, sigil),
                                Err(_) => Value::Nil,
                            }
                        } else {
                            // Check for `is Type` trait on this attribute
                            let is_type_val = self
                                .registry()
                                .class_attribute_is_types
                                .get(&(declaring_class.clone(), attr_name.clone()))
                                .cloned();
                            if let Some(type_name) = is_type_val {
                                // `is Array[T]`: build the empty array with type
                                // metadata directly (a Package built from the string
                                // name loses its type parameter on `.new`).
                                if let Some(inner) = type_name
                                    .strip_prefix("Array[")
                                    .or_else(|| type_name.strip_prefix("array["))
                                    .and_then(|s| s.strip_suffix(']'))
                                {
                                    let mut arr = Value::real_array(Vec::new());
                                    arr = self.tag_container_metadata(
                                        arr,
                                        super::ContainerTypeInfo {
                                            value_type: inner.trim().to_string(),
                                            key_type: None,
                                            declared_type: Some(type_name.clone()),
                                        },
                                    );
                                    arr
                                } else {
                                    let type_obj =
                                        Value::Package(crate::symbol::Symbol::intern(&type_name));
                                    self.call_method_with_values(type_obj, "new", vec![])
                                        .unwrap_or_else(|_| match sigil {
                                            '@' => Value::real_array(Vec::new()),
                                            '%' => Value::hash(HashMap::new()),
                                            _ => Value::Nil,
                                        })
                                }
                            } else {
                                match sigil {
                                    '@' => Value::real_array(Vec::new()),
                                    '%' => Value::hash(HashMap::new()),
                                    _ => Value::Nil,
                                }
                            }
                        };
                        attrs.insert(qualified_key, val);
                    }
                }
                // If the class inherits from Array, add backing array storage
                if self.class_mro(class_key).iter().any(|n| n == "Array")
                    && !attrs.contains_key("__mutsu_array_storage")
                {
                    attrs.insert(
                        "__mutsu_array_storage".to_string(),
                        Value::real_array(Vec::new()),
                    );
                }
                // Tag typed `@`/`%` attributes (`has Int @.nums`) with
                // element-type metadata and type-check their elements (the
                // variable path `my Int @a` does the same). The element type
                // lives in `attribute_types`; `is Type` containers carry their
                // own metadata and are skipped. Done here against the final
                // `attrs` so the Arc-pointer-keyed metadata survives into the
                // instance (a clone shares the same backing Arc).
                for (attr_name, _, _, _, _, sigil, _) in &class_attrs_info {
                    if !matches!(sigil, '@' | '%') {
                        continue;
                    }
                    let Some(elem_type) = attr_type_constraints.get(attr_name).cloned() else {
                        continue;
                    };
                    // Only plain class element types (`has Int @.nums`). Native
                    // (`has int @.x` -> packed `array[int]`), coercion, and
                    // parametric elements keep their pre-existing construction
                    // (the uninit branch already tags native packed arrays).
                    if !Self::is_simple_native_ctor_constraint(&elem_type) {
                        continue;
                    }
                    if self
                        .registry()
                        .class_attribute_is_types
                        .contains_key(&(class_key.to_string(), attr_name.clone()))
                    {
                        continue;
                    }
                    if let Some(val) = attrs.get(attr_name).cloned() {
                        let tagged =
                            self.finalize_typed_container_attr(attr_name, *sigil, &elem_type, val)?;
                        attrs.insert(attr_name.clone(), tagged);
                    }
                }
                // Apply `has $.x does Role` attribute traits (shared with the
                // native default constructor).
                self.apply_attribute_does_role_mixins(class_key, &mut attrs);
                let mut instance = Value::make_instance(*class_name, attrs);
                if let Some(type_args) = type_args.as_ref() {
                    if self.class_mro(class_key).iter().any(|n| n == "Array") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        instance = self.tag_container_metadata(
                            instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(class_name.resolve()),
                            },
                        );
                    } else if self.class_mro(class_key).iter().any(|n| n == "Hash") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        let key_type = type_args.get(1).cloned();
                        instance = self.tag_container_metadata(
                            instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type,
                                declared_type: Some(class_name.resolve()),
                            },
                        );
                    }
                }
                return Ok(instance);
            }
        }
        // Fallback .new on basic types
        match target {
            Value::Package(name) if name == "CallFrame" => {
                // CallFrame.new(depth) — equivalent to callframe(depth)
                let depth = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i as usize),
                        Value::Num(f) => Some(*f as usize),
                        _ => None,
                    })
                    .unwrap_or(0);
                self.builtin_callframe(&args, depth)
            }
            Value::Package(name) => {
                // Built-in type objects: .new creates a default defined instance
                match name.resolve().as_str() {
                    // Shared with the VM's native fast path.
                    "Int" => Self::build_native_int_value(&args),
                    "Str" => Ok(Value::str(String::new())),
                    "Num" => Self::build_native_num_value(&args),
                    "Bool" => Ok(Value::Bool(false)),
                    "Attribute" => {
                        // Attribute.new(:name<...>, :type(Int), :package<Foo>)
                        let mut attrs = HashMap::new();
                        for arg in &args {
                            if let Value::Pair(key, value) = arg {
                                match key.as_str() {
                                    "name" => {
                                        let n = value.to_string_value();
                                        attrs.insert("name".to_string(), (**value).clone());
                                        attrs
                                            .insert("__mutsu_attr_name".to_string(), Value::str(n));
                                    }
                                    "type" => {
                                        attrs.insert("type".to_string(), (**value).clone());
                                    }
                                    "package" => {
                                        attrs.insert(
                                            "__mutsu_attr_owner".to_string(),
                                            (**value).clone(),
                                        );
                                    }
                                    other => {
                                        attrs.insert(other.to_string(), (**value).clone());
                                    }
                                }
                            }
                        }
                        Ok(Value::make_instance(Symbol::intern("Attribute"), attrs))
                    }
                    "Semaphore" => {
                        let permits = args
                            .first()
                            .map(|v| match v {
                                Value::Int(i) => *i,
                                Value::Num(f) => *f as i64,
                                other => other.to_string_value().parse::<i64>().unwrap_or(1),
                            })
                            .unwrap_or(1)
                            .max(0);
                        let mut attrs = HashMap::new();
                        attrs.insert("permits".to_string(), Value::Int(permits));
                        attrs.insert(
                            "semaphore-id".to_string(),
                            Value::Int(super::native_methods::next_semaphore_id(permits) as i64),
                        );
                        Ok(Value::make_instance(name, attrs))
                    }
                    _ => Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on {}",
                        name
                    ))),
                }
            }
            Value::Str(_) => Ok(Value::str(String::new())),
            Value::Int(_) => Ok(Value::Int(0)),
            Value::Num(_) => Ok(Value::Num(0.0)),
            Value::Bool(_) => Ok(Value::Bool(false)),
            Value::Nil => Ok(Value::Nil),
            _ => Err(RuntimeError::new(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): new",
            )),
        }
    }

    fn check_attribute_where_constraint(&mut self, pred: &Expr, value: &Value) -> bool {
        let pred_val = match self.eval_block_value(&[Stmt::Expr(pred.clone())]) {
            Ok(v) => v,
            Err(_) => return false,
        };
        match self.call_sub_value(pred_val, vec![value.clone()], false) {
            Ok(result) => result.truthy(),
            Err(_) => false,
        }
    }

    /// For a given class, return the ordered list of (role_name, MethodDef) pairs
    /// for role submethods with the given name (e.g. "BUILD", "TWEAK", "DESTROY").
    /// The order respects role composition: sub-roles come before the role that
    /// composes them. Only submethods (is_my == true) are included; regular methods
    /// in roles are skipped.
    pub(super) fn ordered_role_submethods_for_class(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Vec<(String, MethodDef)> {
        let composed = match self.registry().class_composed_roles.get(class_name) {
            Some(roles) => roles.clone(),
            None => return Vec::new(),
        };
        // Build the correct order: for each directly composed role (in order),
        // recursively include parent roles (depth-first) before the role itself.
        // Deduplicate to avoid calling the same role submethod twice for the same class.
        let mut ordered = Vec::new();
        let mut seen = std::collections::HashSet::new();
        // Figure out which roles are "direct" (from `does` declarations) vs transitive.
        // Direct roles are those not reachable through another direct role's parents.
        // However, `class_composed_roles` includes both direct and transitive roles in
        // an unspecified order. We need to reconstruct the proper depth-first order.
        //
        // Strategy: for each role in composed list, expand it depth-first (parents first).
        // The composed list may have the order [R1, R0, R2] where R0 is a parent of R1.
        // We want [R0, R1, R2]. We achieve this by expanding each role and skipping
        // already-seen roles.
        for role in &composed {
            let role_base = role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(role.as_str());
            self.expand_role_depth_first(role_base, &mut ordered, &mut seen);
        }
        // Now filter to only roles that have the requested submethod
        let mut result = Vec::new();
        for role_name in &ordered {
            if let Some(role_def) = self.registry().roles.get(role_name)
                && let Some(overloads) = role_def.methods.get(method_name)
            {
                for md in overloads {
                    if md.is_my {
                        result.push((role_name.clone(), md.clone()));
                    }
                }
            }
        }
        result
    }

    /// Recursively expand a role and its parent roles in depth-first order
    /// (parent roles first, then the role itself).
    fn expand_role_depth_first(
        &self,
        role_name: &str,
        ordered: &mut Vec<String>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        if !seen.insert(role_name.to_string()) {
            return;
        }
        // First, expand parent roles
        if let Some(parents) = self.registry().role_parents.get(role_name) {
            for parent in parents {
                let parent_base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.registry().roles.contains_key(parent_base) {
                    self.expand_role_depth_first(parent_base, ordered, seen);
                }
            }
        }
        // Then add the role itself
        ordered.push(role_name.to_string());
    }

    fn collect_attribute_type_constraints(&mut self, class_name: &str) -> HashMap<String, String> {
        let mut constraints = HashMap::new();
        for owner in self.class_mro(class_name) {
            if let Some(class_def) = self.registry().classes.get(&owner) {
                for (attr_name, tc) in &class_def.attribute_types {
                    constraints
                        .entry(attr_name.clone())
                        .or_insert_with(|| tc.clone());
                }
            }
        }
        constraints
    }

    fn enforce_attribute_where_constraints(
        &mut self,
        class_name: &str,
        class_attrs_info: &[ClassAttributeDef],
        attrs: &HashMap<String, Value>,
    ) -> Result<(), RuntimeError> {
        let type_constraints = self.collect_attribute_type_constraints(class_name);
        for (attr_name, _is_public, _default, _is_rw, _is_required, sigil, where_constraint) in
            class_attrs_info
        {
            if let Some(constraint) = type_constraints.get(attr_name)
                && (constraint.starts_with(char::is_uppercase) || constraint.starts_with("::"))
                && let Some(value) = attrs.get(attr_name)
                && !matches!(value, Value::Nil)
            {
                // For array/hash attributes, the type constraint applies to
                // elements/values, not to the container itself.
                if *sigil == '@' || *sigil == '%' {
                    // Skip container-level type check for @ and % attributes;
                    // element-level checking happens at assignment time.
                } else if !self.type_matches_value(constraint, value)
                    && !self.is_container_subclass(constraint)
                {
                    return Err(RuntimeError::new(format!(
                        "Type check failed in assignment to $!{}; expected {}, got {}",
                        attr_name,
                        constraint,
                        super::value_type_name(value)
                    )));
                }
            }
            let Some(pred) = where_constraint else {
                continue;
            };
            let Some(value) = attrs.get(attr_name) else {
                continue;
            };
            if matches!(value, Value::Nil) {
                continue;
            }
            if !self.check_attribute_where_constraint(pred, value) {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to $!{}; where constraint failed",
                    attr_name
                )));
            }
        }
        Ok(())
    }

    /// Enforce type smiley constraints (`:U`, `:D`) on attribute values during `.new`.
    fn enforce_attribute_smiley_constraints(
        &mut self,
        class_name: &str,
        attrs: &HashMap<String, Value>,
    ) -> Result<(), RuntimeError> {
        // Collect smileys and required status from this class and all parent classes in the MRO
        let mut smileys: HashMap<String, String> = HashMap::new();
        let mut required_attrs: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        let mro = self.class_mro(class_name);
        for mro_class in &mro {
            if let Some(class_def) = self.registry().classes.get(mro_class) {
                for (attr_name, smiley) in &class_def.attribute_smileys {
                    smileys
                        .entry(attr_name.clone())
                        .or_insert_with(|| smiley.clone());
                }
                for (attr_name, _, _, _, is_required, _, _) in &class_def.attributes {
                    if is_required.is_some() {
                        required_attrs.insert(attr_name.clone());
                    }
                }
            }
        }

        for (attr_name, smiley) in &smileys {
            // Skip smiley check for required attributes — the required check
            // should take priority (it fires separately and produces a better error)
            if required_attrs.contains(attr_name) {
                continue;
            }
            let Some(value) = attrs.get(attr_name) else {
                continue;
            };
            match smiley.as_str() {
                "U" => {
                    // :U means the value must be undefined (type object)
                    if super::types::value_is_defined(value) {
                        let mut ex_attrs = HashMap::new();
                        ex_attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Type check failed in default value of attribute $!{}; expected {}, got {}",
                                attr_name,
                                self.registry().classes.get(class_name)
                                    .and_then(|cd| cd.attribute_types.get(attr_name))
                                    .map(|t| format!("{}:U", t))
                                    .unwrap_or_else(|| "Any:U".to_string()),
                                super::value_type_name(value),
                            )),
                        );
                        let ex = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Attribute::Default"),
                            ex_attrs,
                        );
                        let mut err = RuntimeError::new(format!(
                            "Type check failed in default value of attribute $!{}",
                            attr_name
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
                // :D means the value must be defined
                "D" if !super::types::value_is_defined(value) => {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Type check failed in default value of attribute $!{}; expected {}, got {}",
                            attr_name,
                            self.registry().classes.get(class_name)
                                .and_then(|cd| cd.attribute_types.get(attr_name))
                                .map(|t| format!("{}:D", t))
                                .unwrap_or_else(|| "Any:D".to_string()),
                            super::value_type_name(value),
                        )),
                    );
                    let ex = Value::make_instance(
                        Symbol::intern("X::TypeCheck::Attribute::Default"),
                        ex_attrs,
                    );
                    let mut err = RuntimeError::new(format!(
                        "Type check failed in default value of attribute $!{}",
                        attr_name
                    ));
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                _ => {} // "_" or anything else: no constraint
            }
        }
        Ok(())
    }

    /// Construct a Proxy subclass instance: extracts FETCH/STORE from args,
    /// initializes subclass attributes (with defaults), and returns a Proxy
    /// with shared mutable subclass attrs.
    fn construct_proxy_subclass(
        &mut self,
        class_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut fetcher = Value::Nil;
        let mut storer = Value::Nil;
        let mut extra_attrs = HashMap::new();

        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "FETCH" => fetcher = *value.clone(),
                    "STORE" => storer = *value.clone(),
                    _ => {
                        extra_attrs.insert(key.clone(), *value.clone());
                    }
                }
            }
        }

        // Initialize subclass attributes with defaults
        let class_attrs_info = self.collect_class_attributes(class_name);
        for (attr_name, _is_public, default_expr, _is_rw, _is_required, sigil, _) in
            &class_attrs_info
        {
            if !extra_attrs.contains_key(attr_name) {
                let default_val = if let Some(expr) = default_expr {
                    let result = self.eval_block_value(&[crate::ast::Stmt::Expr(expr.clone())])?;
                    Self::coerce_attr_value_by_sigil(result, *sigil)
                } else {
                    match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash(HashMap::new()),
                        _ => Value::Nil,
                    }
                };
                extra_attrs.insert(attr_name.clone(), default_val);
            }
        }
        self.enforce_attribute_where_constraints(class_name, &class_attrs_info, &extra_attrs)?;

        let subclass_attrs = std::sync::Arc::new(std::sync::Mutex::new(extra_attrs));
        Ok(Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass: Some((Symbol::intern(class_name), subclass_attrs)),
            decontainerized: false,
        })
    }

    /// Call a Date formatter and store the rendered result in `__formatter_rendered`.
    pub(super) fn render_date_formatter(
        &mut self,
        date: Value,
        formatter_value: Value,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            class_name,
            ref attributes,
            id,
        } = date
        {
            let saved_env = self.env().clone();
            let saved_readonly = self.save_readonly_vars();
            let rendered = self
                .eval_call_on_value(formatter_value, vec![date.clone()])?
                .to_string_value();
            *self.env_mut() = saved_env;
            self.restore_readonly_vars(saved_readonly);
            let mut updated = attributes.to_map();
            updated.insert("__formatter_rendered".to_string(), Value::str(rendered));
            Ok(Value::write_back_sharing(
                attributes, class_name, updated, id,
            ))
        } else {
            Ok(date)
        }
    }

    /// Create a Collation instance with the given level settings.
    pub(super) fn make_collation_instance(
        primary: i64,
        secondary: i64,
        tertiary: i64,
        quaternary: i64,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("primary".to_string(), Value::Int(primary));
        attrs.insert("secondary".to_string(), Value::Int(secondary));
        attrs.insert("tertiary".to_string(), Value::Int(tertiary));
        attrs.insert("quaternary".to_string(), Value::Int(quaternary));
        Value::make_instance(Symbol::intern("Collation"), attrs)
    }

    /// Check if a class composes the Baggy or Setty role (directly or transitively).
    fn class_does_baggy_or_setty(&self, class_name: &str) -> bool {
        // Direct builtin Setty/Baggy types
        const SETTY_BAGGY_TYPES: &[&str] = &[
            "Set",
            "SetHash",
            "Bag",
            "BagHash",
            "Mix",
            "MixHash",
            "Baggy",
            "Setty",
            "QuantHash",
        ];
        // Check the class definition's parents and MRO for Baggy/Setty
        if let Some(class_def) = self.registry().classes.get(class_name) {
            if class_def
                .parents
                .iter()
                .any(|p| SETTY_BAGGY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
            if class_def
                .mro
                .iter()
                .any(|p| SETTY_BAGGY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
        }
        // Also check composed roles
        if let Some(roles) = self.registry().class_composed_roles.get(class_name)
            && roles
                .iter()
                .any(|r| SETTY_BAGGY_TYPES.contains(&r.as_str()))
        {
            return true;
        }
        false
    }

    /// Determine whether a class inherits from a Set-like type (vs Bag-like).
    fn class_is_setty(&self, class_name: &str) -> bool {
        const SETTY_TYPES: &[&str] = &["Set", "SetHash"];
        if let Some(class_def) = self.registry().classes.get(class_name) {
            if class_def
                .parents
                .iter()
                .any(|p| SETTY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
            if class_def
                .mro
                .iter()
                .any(|p| SETTY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
        }
        if let Some(roles) = self.registry().class_composed_roles.get(class_name)
            && roles
                .iter()
                .any(|r| r == "Setty" || r == "Set" || r == "SetHash")
        {
            return true;
        }
        false
    }

    /// Construct an instance for a class that does Baggy/Setty.
    /// Positional args are counted like a Bag (or treated as Set elements),
    /// and the result is stored as an Instance with internal storage.
    fn construct_baggy_instance(
        &mut self,
        class_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let is_setty = self.class_is_setty(class_name);

        if is_setty {
            // Set-like construction: delegate to Set.new
            // TODO: properly track the subclass type on the resulting value
            self.dispatch_new(Value::Package(Symbol::intern("Set")), args.to_vec())
        } else {
            // Bag-like construction: delegate to dispatch_to_bag_with_what for
            // proper handling (pairs, hashes, etc.), then wrap as an Instance
            // so that isa-ok checks for the subclass still work.
            let items_array = Value::array(args.to_vec());
            let bag_value = self.dispatch_to_bag_with_what(items_array, "Bag")?;

            // Store the real Bag as __baggy_data__ on an Instance of the subclass
            let mut attrs = HashMap::new();
            attrs.insert("__baggy_data__".to_string(), bag_value);
            Ok(Value::make_instance(Symbol::intern(class_name), attrs))
        }
    }
}

/// Build a typed `X::Constructor::Positional` for a default constructor that
/// was handed positional arguments (e.g. `Mu.new(1)`). The `type` attribute is
/// the type object so the test matcher `type => Foo` accepts it.
fn constructor_positional_error(class_name: &str) -> RuntimeError {
    let msg = format!(
        "Default constructor for '{}' only takes named arguments",
        class_name
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert(
        "type".to_string(),
        Value::Package(Symbol::intern(class_name)),
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::Constructor::Positional"),
        attrs,
    )));
    err
}
