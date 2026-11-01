use super::*;

impl Interpreter {
    pub(crate) fn build_native_default_instance(
        &mut self,
        class_name: Symbol,
        cn_resolved: &str,
        args: &[Value],
        plan: &super::NativeCtorPlan,
    ) -> Option<Result<Value, RuntimeError>> {
        // The default `.new` accepts only named arguments (`method new(*%_)`); a
        // positional argument is an error (`X::Constructor::Positional`). The
        // native path only consumes `Pair` (named) args, so a positional would be
        // silently ignored — fall through to the interpreter, whose `.new`
        // signature binding raises the proper error. (This also covers a BUILD
        // with a positional parameter: `.new` strips positionals before BUILD, so
        // `Foo.new('x', :y)` must die rather than feed 'x' to BUILD.)
        if args.iter().any(|a| !a.is_string_pair_value()) {
            return None;
        }
        // Class-shape facts (attribute defs, MRO-wide type constraints, the
        // BUILD/TWEAK/smiley probes) come precomputed from the per-class
        // `NativeCtorPlan` instead of being re-derived on every construction.
        let class_attrs = &*plan.class_attrs;
        let type_constraints = &*plan.type_constraints;

        let has_build = plan.has_build;
        let mut attrs = match self.native_ctor_named_attrs(cn_resolved, args, plan)? {
            Ok(attrs) => attrs,
            Err(error) => return Some(Err(error)),
        };
        // A required attribute not supplied as a named arg. Without a BUILD it
        // can never be set, so raise `X::Attribute::Required` now — exactly the
        // interpreter's pre-defaults required check. With a BUILD, defer: BUILD
        // may set it, and the post-BUILD required check below enforces it. The
        // required flag is tuple field 4 (`Option<Option<String>>`: `Some` when
        // `is required`, the inner value being the optional reason); field 3 is
        // `is_rw`, which must NOT gate construction (an unprovided `is rw`
        // attribute just gets its normal default, so it stays native).
        if !has_build {
            for attr in class_attrs.iter() {
                let attr_name = &attr.name;
                if let Some(reason) = &attr.is_required
                    && !attrs.contains_key(attr_name)
                {
                    let attr_full_name = format!("{}!{}", attr.sigil, attr_name);
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
        for attr in class_attrs.iter() {
            if matches!(attr.sigil, '@' | '%') && attr.default.is_some() {
                return None;
            }
        }
        // With a BUILD phase an attribute initializer must run AFTER BUILD and
        // only where BUILD left the attribute alone (raku's order — see
        // runtime/attr_build_defaults.rs). That seed-defer-apply dance needs the
        // interpreter's constructor, so hand any such class over to it rather
        // than filling the defaults eagerly here.
        if has_build
            && class_attrs
                .iter()
                .zip(plan.attr_syms.iter())
                .any(|(attr, &sym)| attr.default.is_some() && !attrs.contains_key(sym))
        {
            return None;
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
        let mut early_inv: Option<Value> = None;
        for (attr_idx, ((attr, &attr_sym), attr_type_constraint)) in class_attrs
            .iter()
            .zip(plan.attr_syms.iter())
            .zip(plan.attr_constraints.iter())
            .enumerate()
        {
            let attr_name = &attr.name;
            let default_expr = &attr.default;
            let sigil = &attr.sigil;
            if attrs.contains_key(attr_sym) {
                continue;
            }
            if plan.attr_binds[attr_idx] && default_expr.is_some() && !attr.default_is_seed {
                let instance = early_inv.get_or_insert_with(|| {
                    Value::make_instance(class_name, attrs.clone())
                });
                let value = match self.bound_constructor_default(class_name, attr, instance, &attrs) {
                    Ok(value) => value,
                    Err(error) => return Some(Err(error)),
                };
                Self::bind_constructor_attribute(&mut attrs, attr_sym, attr_name, value);
                continue;
            }
            let default_literal = default_expr.as_ref().and_then(|arg| arg.literal());
            match default_literal {
                // Raku: assigning `Nil` resets a container to its declared
                // type's default, so `has Str $.n = Nil` holds `Str` — exactly
                // what the no-initializer form holds — not `Nil`.
                // An `is default(...)` value wins (`has $.d is default(5) = Nil`
                // holds 5), as for any `Nil` store into the attribute.
                Some(lit_val) if lit_val.is_nil() => {
                    let seeded = if *sigil == '$'
                        && self
                            .class_attribute_default_with_role_fallback(cn_resolved, attr_name)
                            .is_some()
                    {
                        self.attr_store_nil_default(cn_resolved, attr_name, '$', Value::NIL)
                    } else {
                        self.seed_attr_value(cn_resolved, attr_name, *sigil, type_constraints)
                    };
                    attrs.insert(attr_sym, seeded);
                }
                // Fast path: a literal default needs no evaluation or binding.
                // The interpreter stores it without a type check, so we do too.
                Some(lit_val) => {
                    let val = Self::itemize_attr_store_value(
                        *sigil,
                        Self::coerce_attr_value_by_sigil(lit_val.clone(), *sigil),
                    );
                    if attr.default_is_seed {
                        attrs.insert_seed(attr_sym, val);
                    } else {
                        attrs.insert(attr_sym, val);
                    }
                }
                // A seed default already evaluated for this class (see
                // `NativeCtorPlan::seed_defaults`).
                None if attr.default_is_seed
                    && let Some(seed) = plan.seed_defaults.get(attr_idx).and_then(|c| c.get()) =>
                {
                    let val = Self::itemize_attr_store_value(
                        *sigil,
                        Self::coerce_attr_value_by_sigil(seed.clone(), *sigil),
                    );
                    attrs.insert_seed(attr_sym, val);
                }
                None if default_expr.is_some() => {
                    let arg = default_expr.as_ref().unwrap();
                    // Bind `self`/`?CLASS`/the already-set attributes, switch to
                    // the class package for class-scoped sub lookups, evaluate,
                    // then restore everything — the shared per-default env-setup
                    // (ADR-0019 D2c-5) also used by `dispatch_new`/`dispatch_bless`.
                    // `self` in an initializer IS the object being built (see
                    // `dispatch_new`), made on the first initializer that can
                    // observe it and kept current with the attributes settled
                    // so far.
                    let early = early_inv
                        .get_or_insert_with(|| Value::make_instance(class_name, attrs.clone()));
                    if let Some(cell) = Self::self_instance_attrs(early) {
                        cell.commit_attrs(attrs.clone());
                    }
                    let early = early.clone();
                    let result = self.eval_attr_default_expr(
                        cn_resolved,
                        class_name,
                        arg,
                        &early,
                        &attrs,
                        crate::runtime::attr_build_defaults::AttrDeclScope::of(attr),
                    );
                    let val = match result.and_then(|val| {
                        self.reify_list_attr_value(&val, *sigil)?;
                        Ok(val)
                    }) {
                        Ok(val) => val,
                        Err(e) => {
                            eval_error = Some(e);
                            break;
                        }
                    };
                    // An initializer evaluating to `Nil` resets the Scalar to
                    // its default, as a literal `= Nil` does.
                    let val = if *sigil == '$' && val.is_nil() {
                        self.attr_store_nil_default(cn_resolved, attr_name, '$', val)
                    } else {
                        val
                    };
                    // A non-native default whose value does not match the
                    // attribute's type constraint needs the interpreter — fall
                    // through. Native-typed attributes are exempt (their default
                    // is stored without a type check); coercion-typed attributes
                    // are exempt too (their default is coerced in the final pass).
                    if let Some(c) = attr_type_constraint.as_ref()
                        && Self::native_scalar_default(c).is_none()
                        && !Self::is_native_coercion_ctor_constraint(c)
                        && !self.type_matches_value(c, &val)
                    {
                        typed_default_mismatch = true;
                        break;
                    }
                    if attr.default_is_seed
                        && matches!(val.view(), ValueView::Package(_))
                        && let Some(cell) = plan.seed_defaults.get(attr_idx)
                    {
                        let _ = cell.set(val.clone());
                    }
                    let val = Self::itemize_attr_store_value(
                        *sigil,
                        Self::coerce_attr_value_by_sigil(val, *sigil),
                    );
                    if attr.default_is_seed {
                        attrs.insert_seed(attr_sym, val);
                    } else {
                        attrs.insert(attr_sym, val);
                    }
                }
                None => {
                    // Uninitialized: `@` -> empty Array, `%` -> empty Hash. For
                    // `$`: a native-typed scalar gets its native zero (`int` -> 0,
                    // `num` -> 0e0, `str` -> ""); an untyped `$` gets the Any
                    // type object (`has $!z` reads as Any, matching raku — NOT
                    // Nil). A typed `$` keeps Nil: the accessor synthesizes the
                    // declared type object from the stored Nil at read time.
                    let empty = match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash(ValueMap::default()),
                        _ => match attr_type_constraint.as_ref() {
                            Some(c) => Self::native_scalar_default(c).unwrap_or(Value::NIL),
                            None => Value::package(crate::symbol::wk::any()),
                        },
                    };
                    attrs.insert_seed(attr_sym, empty);
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
        for (attr_idx, ((attr, &attr_sym), tc)) in class_attrs
            .iter()
            .zip(plan.attr_syms.iter())
            .zip(plan.attr_constraints.iter())
            .enumerate()
        {
            if attr.sigil != '$' || plan.attr_binds[attr_idx] {
                continue;
            }
            let Some(tc) = tc else {
                continue;
            };
            if Self::is_native_coercion_ctor_constraint(tc)
                && let Some(val) = attrs.get(attr_sym).cloned()
            {
                let coerced = self.coerce_value_for_constraint(tc, val);
                attrs.rewrite(attr_sym, coerced);
            }
        }
        // Tag typed `@`/`%` attributes (`has Int @.nums`) with element-type
        // metadata and type-check their elements, exactly as `dispatch_new` does
        // (shared `finalize_typed_container_attr`). A failing element raises the
        // same `X::TypeCheck::Assignment` the interpreter would, so the native
        // path is byte-identical. The gate already excluded `is Type` containers
        // and native/parametric element types, so only plain class elements reach
        // here. Done against the final `attrs` Arcs that move into the instance.
        // A `|@x` slip (or a `Seq`) passed to an `@`-sigiled attribute
        // (`:items(|@x)`) arrives as a `Slip`/`Seq`. Assigning a list to a `@`
        // container flattens it (just like `my @a = |@x` yields an `Array`, not a
        // `Slip`), so materialize it into a plain mutable `Array` here. Without
        // this the attribute keeps a `Slip` whose `.^name` is `Slip`.
        for (attr, &attr_sym) in class_attrs.iter().zip(plan.attr_syms.iter()) {
            if attr.sigil != '@' {
                continue;
            }
            let flat_items = match attrs.get(attr_sym).map(Value::view) {
                Some(ValueView::Slip(items)) => Some((**items).clone()),
                Some(ValueView::Seq(items)) => Some(items.to_vec()),
                _ => None,
            };
            if let Some(items) = flat_items {
                let flattened = Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::Array,
                );
                attrs.insert(attr_sym, flattened);
            }
        }
        for ((attr, &attr_sym), elem_type) in class_attrs
            .iter()
            .zip(plan.attr_syms.iter())
            .zip(plan.attr_constraints.iter())
        {
            let attr_name = &attr.name;
            let sigil = attr.sigil;
            if !matches!(sigil, '@' | '%') {
                continue;
            }
            let Some(elem_type) = elem_type else {
                continue;
            };
            if let Some(val) = attrs.get(attr_sym).cloned() {
                // `has Str:D @.e` is an `Array[Str:D]`; the smiley is recorded
                // apart from the type constraint (`attribute_smileys`).
                let elem_type =
                    self.attribute_reported_constraint(cn_resolved, attr_name, elem_type);
                match self.finalize_typed_container_attr(attr_name, sigil, &elem_type, val) {
                    // Hashes embed the element type in `HashData`, so store the
                    // tagged value back into the attrs that move into the instance.
                    Ok(tagged) => {
                        attrs.rewrite(attr_sym, tagged);
                    }
                    Err(e) => return Some(Err(e)),
                }
            }
        }
        // Embed `is default(...)` element defaults into `@`/`%` containers.
        self.apply_container_attribute_defaults(cn_resolved, &mut attrs);
        // Add alias metadata for `has $x` (no twigil) attributes
        for attr_name in plan.alias_attributes.iter() {
            attrs.insert(
                crate::meta_ns::MetaNs::AttrAlias.owned_key_for_str(attr_name),
                Value::str(attr_name.clone()),
            );
        }
        // The gate (`is_native_default_constructible`) allows BUILD/TWEAK-only
        // classes; the instance is assembled (defaults first) at this point, so
        // running BUILD then TWEAK here matches the full `.new` path (which also
        // applies defaults before BUILD). `has_build` was computed above (it
        // gates the named-arg auto-assignment); the TWEAK check is cheap so the
        // common no-submethod case pays nothing extra.
        let has_tweak = plan.has_tweak;
        // Enforce `where` constraints at attribute-assignment time, i.e. *before*
        // BUILD/TWEAK run (matches the interpreter's pre-BUILD enforcement): a
        // provided/defaulted value that fails its `where` is rejected here, and a
        // later BUILD/TWEAK that would "fix" it never runs. `class_attrs` is the
        // same `ClassAttributeDef` slice the interpreter uses.
        let has_where =
            plan.has_subset_attr || class_attrs.iter().any(|a| a.where_constraint.is_some());
        if has_where
            && let Err(e) =
                self.enforce_attribute_where_constraints(cn_resolved, class_attrs, &attrs)
        {
            return Some(Err(e));
        }
        // Enforce definedness smileys (`:D`/`:U`) at the same point as `where`
        // (post-assembly, pre-BUILD/TWEAK): a provided/defaulted value whose
        // definedness does not match its `:D`/`:U` smiley is rejected here, exactly
        // as the full constructor does (`enforce_attribute_smiley_constraints`,
        // which itself skips `is required` attributes). `:_` imposes no constraint.
        let has_smiley = plan.has_smiley;
        if has_smiley {
            // With a BUILD submethod the pre-BUILD attrs are not final — a
            // provided named arg only reaches its attribute inside BUILD — so
            // the provided-and-undefined `:D` check must wait (post-BUILD pass).
            let provided_attr_names: Option<std::collections::HashSet<String>> =
                (!has_build).then(|| {
                    args.iter()
                        .filter_map(|v| match v.view() {
                            ValueView::Pair(k, _) => Some(k.clone()),
                            _ => None,
                        })
                        .collect()
                });
            if let Err(e) = self.enforce_attribute_smiley_constraints(
                cn_resolved,
                &attrs,
                provided_attr_names.as_ref(),
            ) {
                return Some(Err(e));
            }
        }
        // Build the instance BEFORE BUILD/TWEAK and thread its shared attribute
        // cell through them (raku semantics: `self` inside BUILD/TWEAK IS the
        // constructed object). The post-phase checks re-snapshot the live cell.
        let inv = match early_inv {
            Some(inv) => {
                if let Some(cell) = Self::self_instance_attrs(&inv) {
                    cell.commit_attrs(attrs);
                }
                inv
            }
            None => Value::make_instance(class_name, attrs),
        };
        if has_build {
            // Pass the original constructor args so `submethod BUILD(:$x)` binds
            // them. A `fail` inside BUILD yields a `Failure` instance to return.
            match self.run_build_phase(class_name, &inv, args) {
                Ok(Ok(())) => {}
                Ok(Err(failure)) => return Some(Ok(failure)),
                Err(e) => return Some(Err(e)),
            }
            let attrs = Self::self_instance_attrs(&inv)
                .map(|c| c.to_map())
                .unwrap_or_default();
            // Enforce required attributes after BUILD ran (BUILD may set them),
            // exactly where the full constructor does its post-BUILD required
            // check. A still-unset attribute (`None` or `Nil`) raises
            // `X::Attribute::Required` with the same message and reason.
            for attr in class_attrs.iter() {
                let attr_name = &attr.name;
                if let Some(reason) = &attr.is_required {
                    let storage_key =
                        super::attribute_storage_key(class_attrs, attr_name, attr.sigil);
                    // The pre-BUILD seed for an unset untyped attribute is
                    // the Any type object (not Nil), so treat it as unset too.
                    let is_set = !matches!(
                        attrs.get(storage_key).map(Value::view),
                        Some(ValueView::Nil) | None
                    ) && !matches!(
                        attrs.get(storage_key).map(Value::view),
                        Some(ValueView::Package(n)) if n == "Any"
                    );
                    if !is_set {
                        let attr_full_name = format!("{}!{}", attr.sigil, attr_name);
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
                && let Err(e) = self.enforce_attribute_smiley_constraints(cn_resolved, &attrs, None)
            {
                return Some(Err(e));
            }
        }
        if has_tweak {
            // Pass the original constructor args so `submethod TWEAK(:$y)` binds
            // them, matching the full `.new` path. A `fail` inside TWEAK yields a
            // Failure to return, not an error.
            match self.run_tweak_phase(class_name, &inv, args) {
                Ok(Ok(())) => {}
                Ok(Err(failure)) => return Some(Ok(failure)),
                Err(e) => return Some(Err(e)),
            }
        }
        // Re-check `where` after BUILD/TWEAK: the full constructor enforces
        // constraints again post-construction, so a BUILD/TWEAK that mutates an
        // attribute into a `where` violation is rejected identically.
        if has_where && (has_build || has_tweak) {
            let attrs = Self::self_instance_attrs(&inv)
                .map(|c| c.to_map())
                .unwrap_or_default();
            if let Err(e) =
                self.enforce_attribute_where_constraints(cn_resolved, class_attrs, &attrs)
            {
                return Some(Err(e));
            }
        }
        // Mix `has $.x does Role` roles into the final attribute values — the
        // full constructor does this last (after BUILD/TWEAK), so do the same.
        // Gated on the program declaring ANY attribute `does`/container mixin:
        // the common case pays no cell snapshot + commit.
        if (!self.registry().class_attribute_does_roles.is_empty()
            || !self.registry().class_attribute_container_mixins.is_empty())
            && let Some(cell) = Self::self_instance_attrs(&inv)
        {
            let mut attrs = cell.to_map();
            self.apply_attribute_does_role_mixins(cn_resolved, &mut attrs);
            cell.commit_attrs(attrs);
        }
        Some(Ok(inv))
    }
}
