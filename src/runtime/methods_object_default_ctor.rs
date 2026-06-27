use super::*;

impl Interpreter {
    pub(crate) fn build_native_default_instance(
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
                    // `::?CLASS` in a default (e.g. `has $.v = ::?CLASS.^ver`)
                    // resolves through `?CLASS`; bind it to the class being built
                    // (it is otherwise unset here, leaving `::?CLASS` as `Any`).
                    let old_class = self.env.get("?CLASS").cloned();
                    self.env.insert(
                        "?CLASS".to_string(),
                        Value::Package(crate::symbol::Symbol::intern(cn_resolved)),
                    );
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
                    // Mark the class under construction so a bare nested-class
                    // type name in the default (e.g. `has Inner $.x` whose default
                    // is the `Inner` type object) resolves to `<Class>::Inner`,
                    // even though no method-class / package context is active here.
                    let saved_constructing = self.constructing_class.take();
                    self.constructing_class = Some(cn_resolved.to_string());
                    let result = self.eval_block_value(&[crate::ast::Stmt::Expr(expr.clone())]);
                    self.constructing_class = saved_constructing;
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
                    match old_class {
                        Some(v) => {
                            self.env.insert("?CLASS".to_string(), v);
                        }
                        None => {
                            self.env.remove("?CLASS");
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
}
