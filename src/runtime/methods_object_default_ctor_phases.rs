//! The phases of the native default constructor that run once the instance's
//! attributes are assembled (`build_native_default_instance`): the pre-BUILD
//! `where`/smiley checks, the BUILD phase with its deferred attribute
//! initializers, the post-BUILD required/smiley checks, TWEAK, and the
//! `has $.x does Role` mixins.

use super::*;
use crate::runtime::attr_build_defaults::DeferredAttrDefault;

impl Interpreter {
    /// Run the construction phases on the assembled `attrs` and return the
    /// instance (see the module docs). `deferred` are the initializers the
    /// assembly postponed until after BUILD.
    pub(super) fn finish_native_default_instance(
        &mut self,
        class_name: Symbol,
        args: &[Value],
        plan: &super::NativeCtorPlan,
        attrs: AttrMap,
        early_inv: Option<Value>,
        deferred: &[DeferredAttrDefault],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn_resolved = class_name.as_str();
        let class_attrs = &*plan.class_attrs;
        let has_build = plan.has_build && !plan.new_skips_phases;
        // The gate (`is_native_default_constructible`) allows BUILD/TWEAK-only
        // classes; the instance is assembled (defaults first, initializers of a
        // BUILD class deferred) at this point, so running BUILD then TWEAK here
        // matches the full `.new` path.
        let has_tweak = plan.has_tweak && !plan.new_skips_phases;
        // Enforce `where` constraints at attribute-assignment time, i.e. *before*
        // BUILD/TWEAK run (matches the interpreter's pre-BUILD enforcement): a
        // provided/defaulted value that fails its `where` is rejected here, and a
        // later BUILD/TWEAK that would "fix" it never runs. Definedness smileys
        // (`:D`/`:U`) are enforced at the same point. With a BUILD submethod the
        // pre-BUILD attrs are not final -- a provided named arg only reaches its
        // attribute inside BUILD -- so the provided-and-undefined `:D` check
        // waits for the post-BUILD pass.
        let has_where =
            plan.has_subset_attr || class_attrs.iter().any(|a| a.where_constraint.is_some());
        let has_smiley = plan.has_smiley;
        if has_where || has_smiley {
            let provided_attr_names: Option<std::collections::HashSet<String>> =
                (!has_build).then(|| {
                    args.iter()
                        .filter_map(|v| match v.view() {
                            ValueView::Pair(k, _) => Some(k.clone()),
                            _ => None,
                        })
                        .collect()
                });
            if let Err(e) = self.enforce_pre_build_attr_constraints(
                cn_resolved,
                class_attrs,
                &attrs,
                deferred,
                has_where,
                has_smiley.then_some(provided_attr_names.as_ref()),
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
            // The write frame records which attributes BUILD assigned, so the
            // deferred initializers skip them (attr_build_defaults).
            if !deferred.is_empty() {
                self.push_build_write_frame(&inv);
            }
            let build_result = self.run_build_phase(class_name, &inv, args);
            let build_writes = if deferred.is_empty() {
                Default::default()
            } else {
                self.pop_build_write_frame()
            };
            match build_result {
                Ok(Ok(())) => {}
                Ok(Err(failure)) => return Some(Ok(failure)),
                Err(e) => return Some(Err(e)),
            }
            if !deferred.is_empty()
                && let Err(e) = self.apply_post_build_attr_defaults(
                    cn_resolved,
                    class_name,
                    &inv,
                    deferred,
                    &build_writes,
                    Some(plan),
                )
            {
                return Some(Err(e));
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

    /// The seed of the `$` attribute at `attr_idx` of `plan` while its
    /// initializer waits for BUILD: [`Self::seed_attr_value`], memoized in
    /// `NativeCtorPlan::deferred_seeds` unless the constraint is parameterized.
    // Cost: O(1) on a memo hit; otherwise `seed_attr_value`'s O(a) attribute
    // scan, a = attributes.
    pub(super) fn native_deferred_seed(
        &mut self,
        cn_resolved: &str,
        plan: &super::NativeCtorPlan,
        attr_idx: usize,
    ) -> Value {
        if let Some(seed) = plan.deferred_seeds.get(attr_idx).and_then(|m| m.get()) {
            return seed.clone();
        }
        let attr = &plan.class_attrs[attr_idx];
        let seed = self.seed_attr_value(cn_resolved, &attr.name, attr.sigil, &plan.type_constraints);
        let parameterized = plan
            .attr_constraints
            .get(attr_idx)
            .and_then(Option::as_deref)
            .is_some_and(|tc| tc.contains('['));
        if attr.sigil == '$'
            && !parameterized
            && let Some(memo) = plan.deferred_seeds.get(attr_idx)
        {
            let _ = memo.set(seed.clone());
        }
        seed
    }

    /// Whether `a` and `b` are the same type object.
    // Cost: O(1).
    pub(super) fn same_type_object(a: &Value, b: &Value) -> bool {
        matches!((a.view(), b.view()), (ValueView::Package(x), ValueView::Package(y)) if x == y)
    }

    /// The pre-BUILD `where` and definedness-smiley checks over the assembled
    /// attributes, shared by the full and the native default constructor. An
    /// attribute whose initializer is `deferred` still holds its seed here, so
    /// it is exempt from both checks; they run again on the final attributes
    /// after the BUILD phase (a deferred initializer implies a BUILD phase).
    /// `smiley` is `None` to skip the smiley check, else the provided-name set
    /// that `enforce_attribute_smiley_constraints` takes.
    // Cost: O(a + d), a = attributes, d = deferred initializers (plus a copy
    // of the attributes when d > 0).
    pub(crate) fn enforce_pre_build_attr_constraints(
        &mut self,
        class_key: &str,
        class_attrs: &[ClassAttributeDef],
        attrs: &AttrMap,
        deferred: &[DeferredAttrDefault],
        check_where: bool,
        smiley: Option<Option<&std::collections::HashSet<String>>>,
    ) -> Result<(), RuntimeError> {
        let pruned_info: Vec<ClassAttributeDef>;
        let pruned_attrs: AttrMap;
        let (check_info, check_attrs) = if deferred.is_empty() {
            (class_attrs, attrs)
        } else {
            let is_deferred = |name: &str, sigil: char| {
                deferred.iter().any(|d| d.name == name && d.sigil == sigil)
            };
            pruned_info = class_attrs
                .iter()
                .filter(|a| !is_deferred(&a.name, a.sigil))
                .cloned()
                .collect();
            let mut pruned = attrs.clone();
            for d in deferred {
                pruned.remove(super::attribute_storage_key(class_attrs, &d.name, d.sigil));
            }
            pruned_attrs = pruned;
            (&pruned_info[..], &pruned_attrs)
        };
        if check_where {
            self.enforce_attribute_where_constraints(class_key, check_info, check_attrs)?;
        }
        if let Some(provided) = smiley {
            self.enforce_attribute_smiley_constraints(class_key, check_attrs, provided)?;
        }
        Ok(())
    }
}
