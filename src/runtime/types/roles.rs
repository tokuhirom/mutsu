use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Re-apply any roles ever composed onto the named routine
    /// `package::name` (via `.^mixin(Role)` or a trait handler's `$r does
    /// Role`) to a freshly rebuilt `sub_val` for that same routine.
    ///
    /// A named routine's Sub value is rebuilt from the registry at every call
    /// and at every bare `&name` mention rather than kept as one persistent
    /// object, so a role composed onto one instance does not automatically
    /// appear on the next rebuild. `note_routine_mixin_role` (called from
    /// `compose_role_on_value` above) records which roles a given routine has
    /// ever been composed with; this restores them. Cheap no-op (a single
    /// relaxed atomic load, no allocation) when no routine anywhere has ever
    /// been mixed with a role.
    ///
    /// TODO: recorded compositions are always parameterless (`&[]`) — a
    /// routine mixed with a *parameterized* role (`$r does Role[Arg]`) would
    /// lose the type arguments on rebuild. No known caller does this yet
    /// (`Test.rakumod`'s marker roles are parameterless); extending
    /// `note_routine_mixin_role` to also record `role_args` would fix it.
    pub(crate) fn materialize_routine_mixins(
        &mut self,
        sub_val: Value,
        package: &str,
        name: &str,
    ) -> Value {
        if !crate::runtime::registration_sub::any_routine_mixin_roles() {
            return sub_val;
        }
        let qualified = format!("{package}::{name}");
        let roles = crate::runtime::registration_sub::routine_mixin_roles(&qualified);
        let mut result = sub_val;
        for role_name in roles {
            result = self
                .compose_role_on_value(result.clone(), &role_name, &[])
                .unwrap_or(result);
        }
        result
    }

    /// Lightweight `&self` counterpart to [`Self::materialize_routine_mixins`]
    /// for call sites that only hold shared access (e.g.
    /// `sub_value_from_function_def`, which `&name` code-var resolution uses
    /// and cannot take `&mut self` without a much larger signature change).
    /// Restores only the `__mutsu_role__<name>` / role-id markers, not the
    /// full composition — the role's BUILD/TWEAK submethods and deferred body
    /// already ran once, at the original `.^mixin`/`does` call that first
    /// composed it, and must not run again on every rebuild.
    pub(crate) fn materialize_routine_mixins_shared(
        &self,
        sub_val: Value,
        package: &str,
        name: &str,
    ) -> Value {
        if !crate::runtime::registration_sub::any_routine_mixin_roles() {
            return sub_val;
        }
        let qualified = format!("{package}::{name}");
        let roles = crate::runtime::registration_sub::routine_mixin_roles(&qualified);
        if roles.is_empty() {
            return sub_val;
        }
        let (inner, mut mixins) = match sub_val.view() {
            ValueView::Mixin(inner, existing) => (inner.as_ref().clone(), (**existing).clone()),
            _ => (sub_val, HashMap::new()),
        };
        for role_name in &roles {
            mixins.insert(format!("__mutsu_role__{role_name}"), Value::TRUE);
            // Preserve an already-stamped application order across rebuilds
            // (a routine rebuilds its mixin markers on every call/`&name`
            // mention); only stamp when this is the first time.
            mixins
                .entry(format!("__mutsu_role_seq__{role_name}"))
                .or_insert_with(|| Value::int(crate::value::next_instance_id() as i64));
            let role_id = self
                .registry()
                .roles
                .get(role_name)
                .map_or(0, |r| r.role_id);
            if role_id != 0 {
                mixins.insert(
                    format!("__mutsu_role_id__{role_name}"),
                    Value::int(role_id as i64),
                );
            }
        }
        Value::mixin(inner, mixins)
    }

    pub(crate) fn role_def_for_mixin_role(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        role_name: &str,
    ) -> Option<RoleDef> {
        let role_id = mixins
            .get(&format!("__mutsu_role_id__{role_name}"))
            .and_then(|value| match value.view() {
                ValueView::Int(id) if id > 0 => Some(id as u64),
                _ => None,
            });
        if let Some(role_id) = role_id
            && let Some(candidate) =
                self.registry()
                    .role_candidates
                    .get(role_name)
                    .and_then(|candidates| {
                        candidates
                            .iter()
                            .find(|candidate| candidate.role_def.role_id == role_id)
                    })
        {
            return Some(candidate.role_def.clone());
        }
        self.registry().roles.get(role_name).cloned()
    }

    pub(crate) fn resolve_parametric_role_runtime(
        &mut self,
        base_name: &str,
        type_args: &[Value],
    ) -> Option<(RoleDef, Vec<String>)> {
        let mut selected_role = self.registry().roles.get(base_name).cloned();
        let mut selected_param_names = self
            .registry()
            .role_type_params
            .get(base_name)
            .cloned()
            .unwrap_or_default();
        // Hoist clone to a `let` so the guard drops before the filter_map closure
        // re-enters (an if-let scrutinee guard otherwise lives through the block).
        let candidates = self.registry().role_candidates.get(base_name).cloned();
        if let Some(candidates) = candidates {
            let mut matching: Vec<(crate::runtime::RoleCandidateDef, i32, usize)> = candidates
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
                            && (has_positional_slurpy || type_args.len() <= positional_params.len())
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
                selected_role = Some(candidate.role_def.clone());
            }
        }
        selected_role.map(|role| (role, selected_param_names))
    }

    pub(crate) fn delegated_role_attr_key_from_mixins(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        method_name: &str,
    ) -> Option<String> {
        for role_name in mixins
            .keys()
            .filter_map(|key| key.strip_prefix("__mutsu_role__"))
        {
            let Some(role) = self.role_def_for_mixin_role(mixins, role_name) else {
                continue;
            };
            let Some(method_defs) = role.methods.get(method_name) else {
                continue;
            };
            for method_def in method_defs {
                let Some((attr_var_name, target_method)) = &method_def.delegation else {
                    continue;
                };
                if attr_var_name.starts_with('&') {
                    continue;
                }
                if target_method == method_name {
                    let attr_name = attr_var_name.trim_start_matches(['.', '!']);
                    return Some(format!("__mutsu_attr__{attr_name}"));
                }
            }
        }
        None
    }

    /// The mutating form used by the `does` operator: on a real object the role
    /// is composed into *the object* (Rakudo reblesses it into `C+{R}`), so every
    /// alias sees it. Everything else — an `Int`, a `Str`, a punned role — falls
    /// back to the copying wrapper that `but` also uses.
    pub(crate) fn eval_does_values_mutating(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(application) = self.extract_role_application(&right)
            && let Some(reblessed) = self.does_rebless_instance(&left, &[application])?
        {
            return Ok(reblessed);
        }
        self.eval_does_values(left, right)
    }

    /// The mutating form of `$obj does (RoleA, RoleB)`.
    pub(crate) fn eval_does_values_list_mutating(
        &mut self,
        left: Value,
        roles: &[Value],
    ) -> Result<Value, RuntimeError> {
        let applications: Vec<(String, Vec<Value>)> = roles
            .iter()
            .filter_map(|role| self.extract_role_application(role))
            .collect();
        if applications.len() == roles.len()
            && let Some(reblessed) = self.does_rebless_instance(&left, &applications)?
        {
            return Ok(reblessed);
        }
        self.eval_does_values_list(left, roles)
    }

    pub(crate) fn eval_does_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some((role_name, args)) = self.extract_role_application(&right) {
            let result = self.compose_role_on_value(left.clone(), &role_name, &args)?;
            // Call BUILD submethods from the composed role
            let result = self.call_role_build_submethods(result, &role_name)?;
            if let Some(target_name) = Self::var_target_name_from_value(&left) {
                self.set_var_meta_value(&target_name, result.clone());
            }
            // Persist HOW mixin: if the left is a HOW meta-object, store the
            // composed result so future `.HOW` calls return the mixed-in value.
            if let Some(how_target) = Self::how_target_from_value(&left) {
                self.registry_mut()
                    .class_how_values
                    .insert(how_target, result.clone());
            }
            return Ok(result);
        }
        let role_name = right.to_string_value();
        Ok(Value::truth(left.does_check(&role_name)))
    }

    /// Apply multiple roles at once: `$obj does (RoleA, RoleB)`
    pub(crate) fn eval_does_values_list(
        &mut self,
        left: Value,
        roles: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut result = left.clone();
        let mut composed_role_names = Vec::new();
        for role_value in roles {
            if let Some((role_name, args)) = self.extract_role_application(role_value) {
                result = self.compose_role_on_value(result, &role_name, &args)?;
                composed_role_names.push(role_name);
            }
        }
        // Call BUILD submethods for all composed roles
        for role_name in &composed_role_names {
            result = self.call_role_build_submethods(result, role_name)?;
        }
        if let Some(target_name) = Self::var_target_name_from_value(&left) {
            self.set_var_meta_value(&target_name, result.clone());
        }
        Ok(result)
    }

    fn var_target_name_from_value(value: &Value) -> Option<String> {
        match value.view() {
            ValueView::Mixin(inner, _) => Self::var_target_name_from_value(inner),
            ValueView::Instance { attributes, .. } => {
                match attributes
                    .as_map()
                    .get("__mutsu_var_target")
                    .map(|v| v.view())
                {
                    Some(ValueView::Str(name)) => Some(name.to_string()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Check if a value is a HOW meta-object and return the target class name.
    fn how_target_from_value(value: &Value) -> Option<String> {
        match value.view() {
            ValueView::Instance { attributes, .. } => {
                match attributes
                    .as_map()
                    .get("__mutsu_how_target")
                    .map(|v| v.view())
                {
                    Some(ValueView::Str(name)) => Some(name.to_string()),
                    _ => None,
                }
            }
            ValueView::Mixin(inner, _) => Self::how_target_from_value(inner),
            _ => None,
        }
    }

    /// Check if a value represents a role application (used by VM to decide
    /// whether to fall back to the interpreter for `does` operations).
    pub(crate) fn is_role_application(&self, rhs: &Value) -> bool {
        self.extract_role_application(rhs).is_some()
    }

    fn extract_role_application(&self, rhs: &Value) -> Option<(String, Vec<Value>)> {
        // A *built-in* role (`Positional`, `Associative[Int,Int]`, ...) has no
        // `RoleDef` in the registry — mutsu models its behaviour natively — but
        // it is still a role and still composes, so every arm below accepts it
        // alongside a registered one (`compose_role_on_value` already tolerates
        // a body-less builtin role name).
        let is_role = |n: &str| {
            self.registry().roles.contains_key(n) || super::type_registry::is_builtin_role_name(n)
        };
        match rhs.view() {
            ValueView::ParametricRole {
                base_name,
                type_args,
            } if is_role(&base_name.resolve()) => Some((base_name.resolve(), type_args.clone())),
            ValueView::Pair(name, boxed) if is_role(name) => {
                if let ValueView::Array(args, ..) = boxed.view() {
                    Some((name.clone(), args.as_ref().clone().into_items()))
                } else {
                    None
                }
            }
            ValueView::Package(name) if is_role(&name.resolve()) => {
                Some((name.resolve(), Vec::new()))
            }
            // A parameterised role that arrives as a bracketed *type object*
            // name rather than a `ParametricRole` view — which is how the
            // built-in parametric roles are represented
            // (`Associative[Int,Int]`, `Positional[Dog]`).
            ValueView::Package(name)
                if Self::parse_parametric_type_name(&name.resolve())
                    .is_some_and(|(base, _)| is_role(&base)) =>
            {
                let (base, args) = Self::parse_parametric_type_name(&name.resolve())?;
                Some((
                    base,
                    args.iter()
                        .map(|arg| self.type_arg_value_from_name(arg))
                        .collect(),
                ))
            }
            // An INDIVIDUAL parametric role (what a `role` declaration
            // expression evaluates to) applies as its group: composition is
            // group-keyed throughout. See `types/role_candidate.rs`.
            ValueView::Package(name) if self.role_candidate_group(&name.resolve()).is_some() => {
                Some((self.role_group_name(&name.resolve()), Vec::new()))
            }
            ValueView::Str(name) if is_role(name.as_str()) => Some((name.to_string(), Vec::new())),
            // A module-scoped role referenced by its short name at runtime
            // (`$a does NamedAttribute` inside `module NameTrait`'s
            // trait_mod:<is>, where the role registered as
            // `NameTrait::NamedAttribute`). Without this, `does` silently
            // degrades to the boolean conformance check and REBINDS the
            // variable to True/False — how JSON::Name's trait corrupted `$a`.
            ValueView::Package(name) => self.resolve_short_role_name(&name.resolve()),
            ValueView::Str(name) => self.resolve_short_role_name(name.as_str()),
            _ => None,
        }
    }

    /// Resolve a short role name to its registered (possibly package-qualified)
    /// form: the current package's `{pkg}::{name}` first (the sub executing a
    /// `does` runs with its defining module as the current package), then the
    /// general declared-type resolution. None when neither names a role.
    fn resolve_short_role_name(&self, name: &str) -> Option<(String, Vec<Value>)> {
        if !name.contains("::") {
            let qualified = format!("{}::{}", self.current_package(), name);
            if self.registry().roles.contains_key(&qualified) {
                return Some((qualified, Vec::new()));
            }
        }
        let resolved = self.resolve_declared_type_name(name);
        self.registry()
            .roles
            .contains_key(&resolved)
            .then(|| (resolved, Vec::new()))
    }

    /// Bind a parameterised role's type parameters to their DEFAULTS, for a
    /// composition that supplied no explicit arguments (`1 does R` where
    /// `role R[$p = 5]`). Rakudo instantiates the role at composition time, so
    /// the defaults are evaluated even when nothing reads them — which is what
    /// makes a default that raises (`role R[$p = fail("boom")]`) reject the
    /// composition with X::Role::Instantiation instead of silently leaving the
    /// parameter unbound.
    ///
    /// Returns `(declared parameter name, value)` pairs; empty when the role
    /// takes no type parameters or they are not all defaulted (an unsatisfiable
    /// parameterisation is left to the candidate search to report).
    ///
    /// This is the mixin-path counterpart of what the class-header path already
    /// does through `resolve_role_candidate` and what `.new` does through
    /// `materialize_default_parametric_role`.
    fn role_default_type_param_bindings(
        &mut self,
        role_name: &str,
    ) -> Result<Vec<(String, Value)>, RuntimeError> {
        let Some(candidates) = self.registry().role_candidates.get(role_name).cloned() else {
            return Ok(Vec::new());
        };
        let Some(candidate) = candidates.into_iter().find(|candidate| {
            !candidate.type_param_defs.is_empty()
                && candidate
                    .type_param_defs
                    .iter()
                    .all(|pd| pd.default.is_some() || pd.optional_marker)
        }) else {
            return Ok(Vec::new());
        };
        let sig_names: Vec<String> = candidate
            .type_param_defs
            .iter()
            .map(|pd| pd.name.clone())
            .collect();
        let saved_env = self.env.clone();
        let bound = self.bind_function_args_values(&candidate.type_param_defs, &sig_names, &[]);
        let mut bindings = Vec::new();
        if bound.is_ok() {
            for (i, sig_name) in sig_names.iter().enumerate() {
                // A TYPE-CAPTURE parameter (`role E[::T = Int]`) binds its
                // signature slot under the generated `__type_capture__T` name
                // and the captured type under the bare capture name; the role
                // body reads the latter. Same lookup `materialize_default_
                // parametric_role` performs for the `.new` path.
                let capture = candidate.type_param_defs[i]
                    .type_constraint
                    .as_deref()
                    .and_then(|constraint| constraint.strip_prefix("::"));
                let Some(value) = self
                    .env
                    .get(capture.unwrap_or(sig_name.as_str()))
                    .or_else(|| self.env.get(sig_name))
                    .cloned()
                else {
                    continue;
                };
                // Key by the role's declared parameter spelling (what the role
                // body reads), recording the signature spelling too when they
                // differ — the same convention `bind_role_type_params` uses.
                let declared = candidate.type_params.get(i).unwrap_or(sig_name);
                bindings.push((declared.clone(), value.clone()));
                if declared != sig_name {
                    bindings.push((sig_name.clone(), value));
                }
            }
        }
        self.env = saved_env;
        match bound {
            Ok(_) => Ok(bindings),
            // `fail(...)` in a default is an exception-like signal, not a scope
            // transfer: raku reports it as X::Role::Instantiation wrapping the
            // X::AdHoc, exactly like a plain `die`. Any other control signal
            // (a `return`/`last` escaping the default) travels on untouched.
            Err(err)
                if err.control.is_none()
                    || matches!(err.control, Some(crate::value::Control::Fail)) =>
            {
                Err(RuntimeError::role_instantiation(role_name, err))
            }
            Err(err) => Err(err),
        }
    }

    fn compose_role_on_value(
        &mut self,
        left: Value,
        role_name: &str,
        role_args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let role = self.registry().roles.get(role_name).cloned();
        if role.is_none()
            && !matches!(role_name, "Cool" | "Any" | "Mu")
            && !super::type_registry::is_builtin_role_name(role_name)
        {
            return Err(RuntimeError::new(format!("Unknown role: {}", role_name)));
        }

        // Mixing a role into a value is a composition, so the role's
        // non-declaration body runs — once per composed parameterisation, like
        // Rakudo (which memoises the resulting `Int+{R}` type). This is what
        // makes a guard in the body reject a bad parameterisation:
        //
        //     role Guarded[::T] { die unless T.REPR eq 'CStruct' }
        //     my $o = 5 but Guarded[Ordinary];   # X::Role::Instantiation
        //
        // A parameterised role goes through the pun class so the body sees its
        // type parameters bound; a plain one only needs the body run, and
        // punning it to a class here would change what `R.HOW` reports.
        if role.is_some() {
            if !role_args.is_empty()
                && self
                    .registry()
                    .role_type_params
                    .get(role_name)
                    .is_some_and(|params| !params.is_empty())
            {
                self.ensure_parametric_role_pun_class(role_name, role_args)?;
            } else {
                // The memo key must include the value's base type, not just
                // the role: Rakudo's memoized composed type is `Int+{R}` vs
                // `Str+{R}` — two DIFFERENT anonymous types — so `1 but R`
                // and `"x" but R` each run R's deferred body once (verified
                // against `raku`; see the case table in
                // news/2026-08/role-composition-memo-key-raku-case-table.md).
                // A role-only key would wrongly treat the second value's
                // composition as already done just because some earlier,
                // unrelated base type already composed the same role.
                let base_type = crate::value::types::what_type_name(&left);
                if self
                    .registry_mut()
                    .composed_role_bodies
                    .insert(format!("mixin:{base_type}:{role_name}"))
                {
                    let ops = role
                        .as_ref()
                        .map(|r| r.deferred_body.clone())
                        .unwrap_or_default();
                    self.run_role_body_for_composition(role_name, role_name, &ops)?;
                    self.run_composed_role_ancestor_bodies(role_name, role_name)?;
                }
            }
        }

        let (inner, mut mixins) = if let ValueView::Mixin(inner, existing) = left.view() {
            (inner.as_ref().clone(), (**existing).clone())
        } else {
            (left, HashMap::new())
        };
        mixins.insert(format!("__mutsu_role__{}", role_name), Value::TRUE);
        // A monotonic application-order stamp: Rakudo resolves a method-name
        // collision between two mixed-in roles by later-wins precedence
        // (`(0 but A) but B).m` answers from B), not alphabetically. The
        // mixin map has no inherent order, so record one at the point of
        // application; `dispatch_mixin_method_call` / `mixin_chain` sort by
        // this instead of by name. See
        // todo/tickets/mixin-role-order-not-tracked.md.
        mixins.insert(
            format!("__mutsu_role_seq__{}", role_name),
            Value::int(crate::value::next_instance_id() as i64),
        );
        // Store the type arguments so that `.does(Role[args])` can check them.
        if !role_args.is_empty() {
            mixins.insert(
                format!("__mutsu_role_typeargs__{}", role_name),
                Value::array(role_args.to_vec()),
            );
            // Store per-parameter bindings so that methods with type-parameterized
            // constraints (e.g. `method hi(vartype $foo)`) can resolve the type
            // variables during dispatch.
            let param_names = self
                .registry()
                .role_type_params
                .get(role_name)
                .cloned()
                .unwrap_or_default();
            for (param_name, type_arg) in param_names.iter().zip(role_args.iter()) {
                mixins.insert(
                    format!("__mutsu_role_param__{}", param_name),
                    type_arg.clone(),
                );
            }
        } else if role.is_some() {
            // No explicit arguments: instantiate the role at its defaults. The
            // composed name stays the unparameterised `Int+{R}` (raku agrees),
            // but the parameters ARE bound — and a default that raises rejects
            // the composition right here.
            for (param_name, value) in self.role_default_type_param_bindings(role_name)? {
                mixins.insert(format!("__mutsu_role_param__{}", param_name), value);
            }
        }
        // Store the role's unique ID so that different lexical roles with the
        // same name (e.g. two `my role A { }` in different scopes) produce
        // distinct mixin maps, making `===` return False for values mixed with
        // different role instances.
        let role_id = self
            .registry()
            .roles
            .get(role_name)
            .map_or(0, |r| r.role_id);
        if role_id != 0 {
            mixins.insert(
                format!("__mutsu_role_id__{}", role_name),
                Value::int(role_id as i64),
            );
        }

        if let Some(role) = role {
            // Supplying an initialization value (`$x but R(v)`) is only legal
            // when the role has exactly one public attribute. Type parameters
            // (`R[T]`) are not initialization values, so skip the check when the
            // role is parameterized.
            let has_type_params = self
                .registry()
                .role_type_params
                .get(role_name)
                .is_some_and(|params| !params.is_empty());
            if !role_args.is_empty() && !has_type_params {
                let public_attr_count =
                    role.attributes.iter().filter(|attr| attr.is_public).count();
                if public_attr_count != 1 {
                    return Err(RuntimeError::role_initialization(role_name));
                }
            }
            // Temporarily merge captured environment from the role definition
            // so that attribute defaults can reference closure variables.
            let saved_env = if let Some(captured) = &role.captured_env {
                let saved = self.env.clone();
                for (k, v) in captured {
                    if !self.env.contains_key(k) {
                        self.env.insert(k.clone(), v.clone());
                    }
                }
                Some(saved)
            } else {
                None
            };
            for (idx, attr) in role.attributes.iter().enumerate() {
                let attr_name = &attr.name;
                let default_expr = &attr.default;
                let sigil = &attr.sigil;
                let value = if let Some(arg) = role_args.get(idx) {
                    arg.clone()
                } else if let Some(default_arg) = default_expr {
                    let raw = self.eval_decl_trait_arg(default_arg)?;
                    Self::coerce_attr_value_by_sigil(raw, *sigil)
                } else {
                    // Default value based on sigil: @ -> [], % -> {}, $ -> Nil
                    match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash_with_data(Value::hash_arc(HashMap::new())),
                        _ => Value::NIL,
                    }
                };
                mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
            }
            if let Some(saved) = saved_env {
                self.env = saved;
            }
        }

        // A Sub value is rebuilt fresh from the registry at every call and at
        // every bare `&name` mention (see `Interpreter::materialize_routine_mixins`),
        // so composing a role onto *this* instance does not by itself make a
        // later rebuild of the same routine carry it. Record the composition
        // so those rebuild sites can re-apply it.
        if let ValueView::Sub(sub_data) = inner.view() {
            crate::runtime::registration_sub::note_routine_mixin_role(
                &format!("{}::{}", sub_data.package, sub_data.name),
                role_name,
            );
        }

        Ok(Value::mixin(inner, mixins))
    }

    /// Call BUILD and TWEAK submethods from a role after mixin composition.
    /// In Raku 6.e, when `$obj does Role` or `$obj but Role`, the BUILD and
    /// TWEAK submethods of the role are invoked on the resulting object.
    fn call_role_build_submethods(
        &mut self,
        target: Value,
        role_name: &str,
    ) -> Result<Value, RuntimeError> {
        let role = match self.registry().roles.get(role_name).cloned() {
            Some(r) => r,
            None => return Ok(target),
        };
        let mut current_target = target;
        // Run BUILD first, then TWEAK (same order as class construction)
        for submethod_name in &["BUILD", "TWEAK"] {
            let methods = role.methods.get(*submethod_name).cloned();
            if let Some(overloads) = methods {
                for def in overloads {
                    // is_my is set to true for submethods in role method registration
                    if def.is_my {
                        current_target =
                            self.run_role_submethod(current_target, role_name, &role, &def)?;
                        break;
                    }
                }
            }
        }
        Ok(current_target)
    }

    /// Run a single BUILD or TWEAK submethod from a role on a mixin value,
    /// properly propagating attribute modifications back to the mixin map.
    fn run_role_submethod(
        &mut self,
        target: Value,
        role_name: &str,
        role: &RoleDef,
        def: &crate::runtime::MethodDef,
    ) -> Result<Value, RuntimeError> {
        // Temporarily merge the role's captured environment so that
        // closure variables from the role definition scope are accessible
        // and modifications propagate back to the original scope.
        if let Some(captured) = &role.captured_env {
            for (k, v) in captured {
                if !self.env.contains_key(k) {
                    self.env.insert(k.clone(), v.clone());
                }
            }
        }
        // Set up private attribute env vars from mixin attributes so that
        // `$!foo = 42` / `@!bar.push(...)` / `%!baz<k> = v` work inside
        // BUILD/TWEAK. The env key must carry the attribute's sigil prefix
        // (`!foo`, `@!bar`, `%!baz`) to match what the compiled/interpreted
        // body actually reads/writes — a scalar-only key silently no-ops
        // array/hash attribute mutations (`@!attr`/`%!attr` resolve through
        // the sigil-prefixed key, never seeded by a bare `"!attr"` write).
        let attr_names: Vec<(String, char)> = role
            .attributes
            .iter()
            .map(|attr| (attr.name.clone(), attr.sigil))
            .collect();
        if let ValueView::Mixin(_, mixins) = target.view() {
            for (attr_name, sigil) in &attr_names {
                let key = format!("__mutsu_attr__{}", attr_name);
                if let Some(val) = mixins.get(&key) {
                    self.env
                        .insert(attr_env_key(*sigil, attr_name), val.clone());
                }
            }
        }
        // Seed the role's own type/value parameter(s) (`role RP[$v] { ... }`)
        // into env so a BUILD/TWEAK submethod reading `$v` sees the argument
        // actually supplied to `does`/`but`, not (Any). `compose_role_on_value`
        // already stored each binding on the target's own mixin map under
        // `__mutsu_role_param__{name}` (there is no class to key
        // `class_role_param_bindings` by here — this composition targets a
        // plain, non-Instance value). See
        // todo/tickets/role-submethod-runtime-does-parameterized-value.md.
        let role_param_names = self
            .registry()
            .role_type_params
            .get(role_name)
            .cloned()
            .unwrap_or_default();
        let mut saved_role_params: Vec<(String, Option<Value>)> = Vec::new();
        if !role_param_names.is_empty()
            && let ValueView::Mixin(_, mixins) = target.view()
        {
            for param_name in &role_param_names {
                let key = format!("__mutsu_role_param__{}", param_name);
                if let Some(val) = mixins.get(&key) {
                    saved_role_params.push((param_name.clone(), self.env.get(param_name).cloned()));
                    self.env.insert(param_name.clone(), val.clone());
                }
            }
        }
        // Set self for the body
        let saved_self = self.env.get("self").cloned();
        self.env.insert("self".to_string(), target.clone());
        // Execute the body directly in current scope so closure variable
        // mutations propagate to the outer scope. `$!attr` reads/writes
        // inside the compiled body resolve to `GetLocal`/`SetLocal` ops on a
        // slot named `"!attr"` (ADR-0019 D8-3): `self_instance_attrs` finds
        // no cell for a Mixin over a non-Instance `self` (the scenario here
        // — `does`/`but` on a plain value) and its cell-mirror is a silent
        // no-op both ways, so the locals<->env bridge that `run_nested`
        // performs at entry/exit is what actually threads the seeded
        // `env["!attr"]` values through. Run D8-1's precompiled chunk when
        // available instead of re-parsing/re-compiling `def.body` on every
        // composition; a method not yet compiled (e.g. installed via a
        // meta-programming hook) falls back to the raw-AST carrier.
        if let Some(cc) = &def.compiled_code {
            let empty_fns = crate::opcode::CompiledFns::default();
            let fns_ref = def.compiled_fns.as_deref().unwrap_or(&empty_fns);
            self.run_compiled_block_raw(cc, fns_ref)?;
        } else {
            self.eval_block_value(&def.body)?;
        }
        // Read back modified attribute values from env into the mixin map
        let updated_target = if let ValueView::Mixin(inner, existing_mixins) = target.view() {
            let mut mixins = (**existing_mixins).clone();
            for (attr_name, sigil) in &attr_names {
                let env_key = attr_env_key(*sigil, attr_name);
                if let Some(val) = self.env.get(&env_key) {
                    mixins.insert(format!("__mutsu_attr__{}", attr_name), val.clone());
                }
            }
            // Clean up env vars
            for (attr_name, sigil) in &attr_names {
                self.env.remove(&attr_env_key(*sigil, attr_name));
            }
            Value::mixin((**inner).clone(), mixins)
        } else {
            target
        };
        // Restore self
        if let Some(prev) = saved_self {
            self.env.insert("self".to_string(), prev);
        } else {
            self.env.remove("self");
        }
        // Restore any outer binding the role's own params seeding shadowed.
        for (param_name, prev) in saved_role_params {
            match prev {
                Some(v) => self.env.insert(param_name, v),
                None => self.env.remove(&param_name),
            };
        }
        Ok(updated_target)
    }
}

/// The private-attribute env key a compiled/interpreted body actually reads
/// and writes for `$!attr`/`@!attr`/`%!attr`, keyed by the attribute's
/// declared sigil. A scalar attribute's key has no sigil prefix (`"!attr"`);
/// array/hash attributes carry their sigil (`"@!attr"`/`"%!attr"`).
fn attr_env_key(sigil: char, attr_name: &str) -> String {
    if sigil == '$' {
        format!("!{attr_name}")
    } else {
        format!("{sigil}!{attr_name}")
    }
}
