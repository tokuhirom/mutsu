use super::*;
use crate::value::AttrMap;

/// How a `fail` raised inside a construction-phase step is surfaced to the
/// caller of the phase runner. The three modes preserve the (historically
/// asymmetric) behaviors of the pre-plan phase loops verbatim:
/// `run_tweak_phase` turned a `fail` from any step into a `Failure` return
/// value via `fail_error_to_failure_value`; `run_build_phase` propagated a
/// role-step `fail` as an error but wrapped a class-step `fail` in an inline
/// X::AdHoc `Failure` (without the pending-failure registration); the bless
/// BUILD phase propagated everything.
pub(super) enum PhaseFail {
    Propagate,
    TweakFailure,
    BuildFailure,
}

impl Interpreter {
    /// Build the pre-derived step list for one construction phase (`BUILD` or
    /// `TWEAK`) of `cn`: the base-first MRO walk, the per-level registry
    /// probes, the role-submethod ordering, and the 6.c/6.e skip decisions —
    /// everything the phase loops used to re-derive on every construction and
    /// that is a pure function of the class shape. Cached on `NativeCtorPlan`
    /// (same invalidation sites as the rest of the plan).
    ///
    /// The class/role language revisions are read at plan-build time (first
    /// construction). The parser-version fallback for a class with no
    /// `language-revision` metadata is therefore frozen at that point; that
    /// matches the class's declaration context in every non-contrived case
    /// (the revision is a property of the declaration, not the construction).
    pub(super) fn build_construction_phase_steps(
        &mut self,
        cn: &str,
        method_name: &str,
    ) -> Vec<ConstructionPhaseStep> {
        let mro = self.class_mro(cn);
        let class_lang_rev = self
            .type_metadata
            .get(cn)
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
        let mut steps = Vec::new();
        for mro_class in mro.iter().rev().map(|s| s.as_str()) {
            if mro_class == "Any" || mro_class == "Mu" {
                continue;
            }
            // Skip role entries in MRO
            if self.registry().roles.contains_key(mro_class)
                && !self.registry().classes.contains_key(mro_class)
            {
                continue;
            }
            // Does the class itself declare the submethod (not role-composed)?
            let class_has_own = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get(method_name))
                .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                .unwrap_or(false);
            // Role-composed submethods, with the same 6.c/6.e rules as the
            // former per-construction loops:
            // - Always call role submethods (both 6.c and 6.e classes)
            // - In 6.c: if the class has its own submethod, skip same-revision
            //   (6.c) role submethods, but still call 6.e+ ones
            // - In 6.e+: always call all role submethods
            let role_order = self.ordered_role_submethods_for_class(mro_class, method_name);
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
                if !class_is_6e && class_has_own && role_lang_rev == "c" {
                    continue;
                }
                steps.push(ConstructionPhaseStep::Role {
                    role_name,
                    def: method_def,
                });
            }
            // The class's own candidate at this level (own submethod, or a
            // role-composed regular method) — dispatched with `mro_class` as
            // the receiver, exactly as before.
            let has_non_submethod = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get(method_name))
                .map(|overloads| {
                    overloads
                        .iter()
                        .any(|md| md.role_origin.is_none() || !md.is_my)
                })
                .unwrap_or(false);
            if has_non_submethod {
                let pinned = self.try_pin_phase_candidate(mro_class, method_name);
                steps.push(ConstructionPhaseStep::Class {
                    mro_class: mro_class.to_string(),
                    pinned,
                });
            }
        }
        steps
    }

    /// Pin the phase candidate for a `Class` step when full method resolution
    /// is provably equivalent to "run this one def": exactly one visible
    /// non-multi candidate declared on the class itself, with none of the
    /// features the resolver's speculative match or the dispatch wrapper
    /// machinery would act on. Mirrors the resolver's single-visible-candidate
    /// fast return (`resolve_method_with_owner_impl`) plus the
    /// `run_instance_method_celled` remaining-candidates skip
    /// (`count_visible_method_candidates <= 1`). Anything more exotic returns
    /// `None` and keeps the full resolution path.
    fn try_pin_phase_candidate(&mut self, mro_class: &str, method_name: &str) -> Option<MethodDef> {
        let overloads = self
            .registry()
            .get_method_overloads(mro_class, method_name)?;
        let mut visible = overloads.iter().filter(|d| !d.is_private);
        let only = visible.next()?;
        if visible.next().is_some() {
            return None;
        }
        if only.is_multi
            || !(only.is_my || only.is_submethod)
            || only.delegation.is_some()
            || only.deprecated_message.is_some()
        {
            return None;
        }
        if !only.param_defs.iter().all(|pd| {
            pd.where_constraint.is_none()
                && pd.type_constraint.is_none()
                && pd.sub_signature.is_none()
        }) {
            return None;
        }
        // A second visible candidate anywhere in the MRO (e.g. an inherited
        // non-submethod of the same name) needs the dispatch frame for
        // `nextsame` — keep the full path.
        if self.count_visible_method_candidates(mro_class, method_name) > 1 {
            return None;
        }
        let mut def = only.clone();
        // Compile once at plan build instead of per construction.
        if def.compiled_code.is_none() {
            let dist = self.resolve_package_distribution(mro_class);
            Self::compile_method_def_in_place_with_dist(&mut def, mro_class, dist);
            def.compiled_code.as_ref()?;
        }
        Some(def)
    }

    /// Run one construction phase (`BUILD`/`TWEAK`) over the plan's pre-derived
    /// steps against `inv`'s shared attribute cell (base-first MRO order —
    /// every step sees and mutates the same live object). `args` are the
    /// constructor's named arguments, passed through to each submethod.
    ///
    /// The probe map handed to the dispatch layer is the plan's shared
    /// attribute-name skeleton (names -> Nil) whenever the live cell carries no
    /// sigilless-alias metadata: every consumer on that path reads only the key
    /// set (`attr_twigil_local`, the attr-defaults key loop, the alias scans),
    /// so the per-construction whole-cell `to_map()` value clone is skipped.
    /// With alias metadata present, the live map is re-materialized before
    /// every step, exactly as the former `refresh_probe` loops did.
    pub(super) fn run_construction_phase_steps(
        &mut self,
        class_name: Symbol,
        inv: &Value,
        args: &[Value],
        method_name: &str,
        role_fail: PhaseFail,
        class_fail: PhaseFail,
    ) -> Result<Result<(), Value>, RuntimeError> {
        let Some(cell) = Self::self_instance_attrs(inv) else {
            return Ok(Ok(()));
        };
        let plan = self.native_ctor_plan(class_name);
        let steps = if method_name == "BUILD" {
            plan.build_steps.clone()
        } else {
            plan.tweak_steps.clone()
        };
        if steps.is_empty() {
            return Ok(Ok(()));
        }
        let live_has_alias = cell
            .as_map()
            .keys()
            .any(|k| k.starts_with("__mutsu_attr_alias::"));
        let mut probe_owned: Option<AttrMap> = None;
        let cn = class_name.resolve();
        for step in steps.iter() {
            if live_has_alias {
                probe_owned = Some(cell.to_map());
            }
            let probe: &AttrMap = probe_owned.as_ref().unwrap_or(&plan.probe_skeleton);
            let (outcome, fail_mode) = match step {
                ConstructionPhaseStep::Role { role_name, def } => (
                    self.run_resolved_method_celled(
                        &cn,
                        role_name,
                        method_name,
                        def,
                        probe,
                        args.to_vec(),
                        Some(inv.clone()),
                    ),
                    &role_fail,
                ),
                ConstructionPhaseStep::Class { mro_class, pinned } => {
                    // The pinned direct run must stay equivalent to full
                    // dispatch: a wrap chain or a NativeCall method descriptor
                    // registered after plan build re-routes through the full
                    // path (both are runtime-global prefilters there too).
                    let r = if let Some(def) = pinned
                        && self.native_call_specs.is_empty()
                        && !self.has_any_wrap_chains()
                    {
                        self.push_method_samewith_context(
                            mro_class,
                            method_name,
                            args,
                            Some(inv.clone()),
                        );
                        let r = self.run_resolved_method_celled(
                            mro_class,
                            mro_class,
                            method_name,
                            def,
                            probe,
                            args.to_vec(),
                            Some(inv.clone()),
                        );
                        self.pop_method_samewith_context();
                        r
                    } else {
                        self.run_instance_method_celled(
                            mro_class,
                            probe,
                            method_name,
                            args.to_vec(),
                            Some(inv.clone()),
                        )
                    };
                    (r, &class_fail)
                }
            };
            match outcome {
                Ok((_v, updated)) => {
                    if let Some(m) = updated {
                        cell.commit_attrs(m);
                    }
                }
                Err(err) if err.is_fail() && !matches!(fail_mode, PhaseFail::Propagate) => {
                    let failure = match fail_mode {
                        PhaseFail::TweakFailure => self.fail_error_to_failure_value(&err),
                        PhaseFail::BuildFailure => {
                            // Historical `.new` BUILD behavior: inline X::AdHoc
                            // wrap without the pending-failure registration.
                            let ex = if let Some(exception) = err.exception {
                                *exception
                            } else {
                                let mut ex_attrs = HashMap::new();
                                ex_attrs.insert("message".to_string(), Value::str(err.message));
                                Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs)
                            };
                            let mut failure_attrs = HashMap::new();
                            failure_attrs.insert("exception".to_string(), ex);
                            Value::make_instance(Symbol::intern("Failure"), failure_attrs)
                        }
                        PhaseFail::Propagate => unreachable!(),
                    };
                    return Ok(Err(failure));
                }
                Err(err) => return Err(err),
            }
        }
        Ok(Ok(()))
    }
}
