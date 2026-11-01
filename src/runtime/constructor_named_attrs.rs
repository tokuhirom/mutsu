use super::*;

impl Interpreter {
    // Cost: O(a), a = named arguments, plus their coercion and type checks.
    pub(super) fn native_ctor_named_attrs(
        &mut self,
        cn_resolved: &str,
        args: &[Value],
        plan: &super::NativeCtorPlan,
    ) -> Option<Result<AttrMap, RuntimeError>> {
        let class_attrs = &*plan.class_attrs;
        let type_constraints = &*plan.type_constraints;
        // One hash lookup instead of a linear `class_attrs` name scan per
        // named argument (`NativeCtorPlan::attr_index`).
        let attr_idx_of =
            |name: &str| -> Option<usize> { plan.attr_index.get(name).map(|&i| i as usize) };
        let sigil_of = |name: &str| -> char {
            attr_idx_of(name)
                .map(|i| class_attrs[i].sigil)
                .unwrap_or('$')
        };

        // A custom BUILD replaces the default named-argument → attribute binding
        // for the attributes of the MRO layer that declares it: a provided
        // `:attr(value)` for such an attribute is NOT auto-assigned — only that
        // BUILD decides what gets set (via its signature's `:$!attr` params or
        // explicit assignment in its body). So
        // `class P { has $.x = 42; submethod BUILD() {} }; P.new(:666x).x` is 42.
        // Attributes of layers WITHOUT their own BUILD still auto-assign
        // (per-class BUILDALL semantics — see `build_owning_attr_names`).
        // Defaults are still filled below and BUILD runs afterward.
        let has_build = plan.has_build;
        let build_owned_attrs = if has_build {
            let mro = self.mro_readonly(cn_resolved);
            self.build_owning_attr_names(&mro)
        } else {
            Default::default()
        };

        let mut attrs = AttrMap::with_layout(plan.layout.clone());
        for arg in args {
            let ValueView::Pair(key, val) = arg.view() else {
                continue;
            };
            let key_idx = attr_idx_of(key);
            // A declared attribute's buildability is class shape, read from
            // the plan; only an undeclared name asks the registry.
            let buildable = match key_idx {
                Some(i) => plan.attr_buildable[i],
                None => self.is_attribute_buildable(cn_resolved, key),
            };
            if !build_owned_attrs.contains(key.as_str()) && buildable {
                let key_sym = key_idx.map(|i| plan.attr_syms[i]);
                // Raku's default BUILDALL ignores a named argument that names no
                // attribute; storing it would make `eqv` compare a key the object
                // is not supposed to have. See `NativeCtorPlan::attrs_fully_known`.
                if key_sym.is_none() && plan.attrs_fully_known {
                    continue;
                }
                if let Some(i) = key_idx.filter(|&i| plan.attr_binds[i]) {
                    if let Err(error) = self.check_attr_store_type(
                        cn_resolved, key, '$', val.deref_container().clone(),
                    ) {
                        return Some(Err(error));
                    }
                    Self::bind_constructor_attribute(
                        &mut attrs, plan.attr_syms[i], key, val.clone(),
                    );
                    continue;
                }
                let val = val.deref_container();
                match sigil_of(key) {
                    '@' | '%' => {
                        // A `%`-attribute bound to a non-Hash object is
                        // list-contextualized like Raku's `Hash.STORE` (an object
                        // with a custom `.iterator`/`.list` contributes its pairs).
                        // That re-enters user code, which this native fast path
                        // must not do, so defer to the interpreter slow path.
                        if sigil_of(key) == '%'
                            && matches!(val.view(), ValueView::Instance { .. })
                            && !val.is_match_instance()
                        {
                            return None;
                        }
                        // Coerce exactly as the interpreter's shared helper does
                        // (List/Range -> Array, array-of-Pairs -> Hash, …).
                        let coerced =
                            Self::coerce_provided_attr_value_by_sigil(val.clone(), sigil_of(key));
                        match key_sym {
                            Some(s) => attrs.insert(s, coerced),
                            None => attrs.insert(key.clone(), coerced),
                        };
                    }
                    _ => {
                        // Raku: assigning `Nil` to a scalar container resets it
                        // to the container's declared type default, exactly
                        // like `has $.x = Nil` and plain `$x = Nil` do — not
                        // the provided `Nil` itself. Seed it here so the check
                        // below (and the stored value) sees that default, not
                        // a value that could spuriously fail the type check.
                        let seeded = val.is_nil().then(|| {
                            self.attr_store_nil_default(cn_resolved, key, '$', val.clone())
                        });
                        let val = seeded.as_ref().unwrap_or(&val);
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
                        // A `$` attribute IS a Scalar container: a provided
                        // Array/Hash is itemized exactly as the default-value
                        // fill below and the accessor store are (#9807).
                        let val = Self::itemize_attr_store_value('$', val.clone());
                        match key_sym {
                            Some(s) => attrs.insert(s, val),
                            None => attrs.insert(key.clone(), val),
                        };
                    }
                }
            }
        }
        Some(Ok(attrs))
    }
}
