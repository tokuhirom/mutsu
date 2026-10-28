//! Parent-role and parent-class composition for a role declaration.

use super::registration_class::{
    parse_role_type_args, should_treat_role_arg_as_type_expr, substitute_type_params_in_method,
    type_value_name,
};
use super::registration_class_decl::{BUILTIN_INHERITABLE_TYPES, BUILTIN_PARENT_TYPES};
use super::registration_role_decl::RoleDeclCx;
use super::*;

impl Interpreter {
    /// The `does` arm of the role-body walk: record parent roles/classes and
    /// compose a parent role's attributes and methods into this role.
    ///
    /// ADR-0019 D7-3: `op` is precompiled by the compiler at plan lowering
    /// (`CompiledRoleDeclPlan::parent_ops`) and read here by position via the
    /// same cursor style `role_body_method_decl` uses, instead of the
    /// runtime string-matching the `__mutsu_role_hides__`/
    /// `__mutsu_role_hidden__` marker names on the raw statement.
    pub(super) fn role_body_does_decl(
        &mut self,
        cx: &mut RoleDeclCx<'_>,
    ) -> Result<(), RuntimeError> {
        let op_idx = cx.parent_op_idx;
        cx.parent_op_idx += 1;
        let op = cx
            .parent_ops
            .get(op_idx)
            .cloned()
            .expect("parent_ops misaligned with role body walk");
        let name = cx.name;
        if op.hidden {
            cx.role_def.is_hidden = true;
            return Ok(());
        }
        if op.hides {
            // Track hidden class relationship for this role. Resolve through
            // the lexical env first (see the `is`-parent remap below) so a
            // lexical class's mangled storage name is what gets recorded,
            // not its dead bare source name.
            let parent = self.resolve_type_name_for_owner(name, op.name.resolve());
            let parent = self.lexical_env_remap_name(&parent);
            self.registry_mut()
                .role_hides
                .entry(name.to_string())
                .or_default()
                .push(parent);
            return Ok(());
        }
        // A role's `is Parent`/`does Parent` clause names its parent as
        // WRITTEN in the source (`op.name.resolve()`), but a lexical class
        // registers under a mangled storage name (ADR-0047 P1:
        // `Foo\u{0}<decl-id>`) while `env` binds the bare name written here to
        // it — the same remap `exec_register_class_op` applies to a class's
        // own `is`-parents. Without this, `role R is C2 { }` where `C2` is a
        // `my class` reported "Unknown role: C2": the bare name is in neither
        // `registry().roles` nor `registry().classes`, only its mangled
        // storage name is.
        let parent = self.resolve_type_name_for_owner(name, op.name.resolve());
        let role_name_str = self.lexical_env_remap_name(&parent);
        // While a role is being declared its own name is not yet visible to
        // its traits, so `role Exception is Exception` (or `is ::Exception`)
        // inherits the core `Exception`, not itself (#11072). A role declared
        // in a package already differs from the outer name (`M::Base` vs
        // `Base`); only a top-level role can shadow a core type this way, and
        // its parent is that core type, spelled so it cannot resolve back to
        // the role.
        // `role Iterator does Iterator` (also inside `unit class Gauge`, where
        // the role is `Gauge::Iterator`) composes the like-named core role.
        let self_short = crate::qualified::unqualified_part(Symbol::intern(name)).resolve();
        let core_self = if op.from_is {
            (crate::qualified::type_capture_name(&role_name_str).unwrap_or(&role_name_str) == name)
                .then(|| name.to_string())
        } else {
            (op.name.resolve() == self_short
                && (role_name_str == name || role_name_str == self_short))
                .then(|| self_short.to_string())
        };
        if let Some(core) = core_self {
            // Inside a package the role is `Pkg::R`, so a bare `R` is another
            // role already registered; at top level only the catalog can
            // stand behind the name.
            let core_exists = crate::builtin_types::catalog::builtin_type_info(&core).is_some()
                || crate::runtime::types::is_builtin_role_name(&core)
                || (name != core && self.registry().roles.contains_key(core.as_str()));
            if core_exists {
                self.registry_mut()
                    .role_parents
                    .entry(name.to_string())
                    .or_default()
                    // `Pkg::R` composing the plain `R`: no `CORE::` pin is
                    // needed, the two names differ.
                    .push(if name != core {
                        core.clone()
                    } else {
                        format!("CORE::{core}")
                    });
                return Ok(());
            }
            if !op.from_is {
                return Err(RuntimeError::new(format!(
                    "X::InvalidType: role '{name}' cannot compose itself"
                )));
            }
        }
        // A sibling role referenced by its short name (`role Derived
        // does Base` inside `unit module M`, where Base is registered
        // as `M::Base`) must resolve to its qualified name — the same
        // resolution the class-body DoesDecl path already does.
        // JSON::Unmarshal composes all its CustomUnmarshaller roles
        // this way.
        // A parametric application (`does Key[IO::Path]`) resolves its base the
        // same way and keeps the `[...]` suffix as written.
        let (role_base_str, role_suffix_str) = match role_name_str.find('[') {
            Some(i) => (
                role_name_str[..i].to_string(),
                role_name_str[i..].to_string(),
            ),
            None => (role_name_str.clone(), String::new()),
        };
        let role_name_str = if !self.is_role_type_name(&role_base_str)
            && !self.registry().classes.contains_key(&role_base_str)
        {
            let known = |this: &Self, n: &str| {
                this.is_role_type_name(n) || this.registry().classes.contains_key(n)
            };
            let resolved_base = if let Some((declaring_package, _)) =
                crate::qualified::split_qualified(crate::qualified::known_symbol(cx.name))
                    .map(|(head, tail)| (head.as_str(), tail.as_str()))
            {
                // A unit module's mainline runs with GLOBAL as the current
                // package, even though the compiler has already qualified
                // this role's storage name. Resolve a sibling parent from the
                // role declaration's package in that case. The same relative
                // probe handles a nested name such as `Q::R` inside the unit
                // module: first try `M::Q::R`, then leave an unrelated fully
                // qualified name untouched.
                let relative = crate::qualified::qualified_text(declaring_package, &role_base_str)
                    .as_str()
                    .to_string();
                if known(self, &relative) {
                    Some(relative)
                } else {
                    None
                }
            } else {
                None
            };
            let resolved_base = resolved_base.or_else(|| {
                let resolved = self.resolve_declared_type_name(&role_base_str);
                known(self, &resolved).then_some(resolved)
            });
            match resolved_base {
                Some(base) => format!("{base}{role_suffix_str}"),
                None => role_name_str,
            }
        } else {
            role_name_str
        };
        // A role that was called as a type object (`R.method`) also has a
        // class of the same name, its pun. That is still the role: composing
        // it must merge its attributes (#14076), not record a class parent.
        if self.registry().classes.contains_key(&role_name_str)
            && !self.registry().roles.contains_key(&role_name_str)
        {
            self.registry_mut()
                .role_parents
                .entry(name.to_string())
                .or_default()
                .push(role_name_str);
            return Ok(());
        }
        let base_role_name = role_name_str
            .split_once('[')
            .map(|(b, _)| b)
            .unwrap_or(role_name_str.as_str());
        if cx.type_params.iter().any(|tp| tp == base_role_name)
            || crate::runtime::types::is_builtin_role_name(base_role_name)
            || BUILTIN_PARENT_TYPES.contains(&base_role_name)
            || BUILTIN_INHERITABLE_TYPES.contains(&base_role_name)
        {
            self.registry_mut()
                .role_parents
                .entry(name.to_string())
                .or_default()
                .push(role_name_str);
            return Ok(());
        }
        // For a CONCRETE parametric parent (`does R1[Bool]`), resolve the
        // specific concretization's RoleDef by arity rather than the
        // by-name `roles` map — with a same-named role group (a
        // parameterized `R1[::T]` AND an unparameterized `R1`) the map
        // holds only the last-declared variant, so composing by bare name
        // would pull the wrong one and drop the parametric role's methods.
        // Skip when the application forwards a type param (`R1[::T]`),
        // which `resolve_role_candidate` rejects.
        let concretized_parent =
            if role_name_str.contains('[') && !crate::qualified::is_qualified_str(&role_name_str) {
                self.resolve_role_candidate(&role_name_str)
                    .ok()
                    .flatten()
                    .map(|(rd, _, _)| rd)
            } else {
                None
            };
        let role = match concretized_parent
            .or_else(|| self.registry().roles.get(base_role_name).cloned())
        {
            Some(r) => r,
            None => {
                // `is Foo` on a name that is NOT a known type is rakudo's
                // spelling of the named trait argument `trait_mod:<is>($type,
                // :Foo)`, exactly as on the class side
                // (`validate_class_parents`). Raku decides that from whether
                // `Foo` names a known type, never from its capitalisation, so
                // every unknown `is` parent is deferred to custom trait
                // dispatch when the program defines a `trait_mod:<is>` at all;
                // the dispatch site (`vm_typedecl_ops.rs`) turns a
                // no-matching-candidate result back into
                // `X::Inheritance::UnknownParent`, so a genuine `is` typo still
                // raises. This used to be restricted to lowercase names, which
                // made `role R is Marked { }` -- the overwhelmingly common
                // spelling of a trait -- an `Unknown role: Marked` error
                // (#8100).
                //
                // A `does` parent keeps the narrower lowercase rule: `does
                // NoSuchRole` is a typo, not a trait, and must stay the
                // `Unknown role:` error below rather than becoming an
                // unknown-*parent* one.
                if (self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>"))
                    && (op.from_is
                        || role_name_str
                            .chars()
                            .next()
                            .is_some_and(|c| c.is_ascii_lowercase()))
                {
                    cx.role_def
                        .deferred_custom_traits
                        .push(role_name_str.to_string());
                    return Ok(());
                }
                return Err(RuntimeError::new(format!(
                    "Unknown role: {}",
                    role_name_str
                )));
            }
        };
        self.registry_mut()
            .role_parents
            .entry(name.to_string())
            .or_default()
            .push(role_name_str.clone());
        // Use resolve_role_candidate to properly handle named
        // arguments and default values in parameterized role
        // composition.
        // `does R1[::T]` inside a parameterized role forwards this
        // role's type parameter into the parent role. The concrete
        // value of `::T` is not known until THIS role is itself
        // concretized, so don't resolve the parent now (that errors
        // with "cannot use ::T in role application"); record the
        // forwarding binding (parent-param -> `::T`) via the
        // role_type_params branch below and defer real composition to
        // class-application time.
        let forwards_type_param = crate::qualified::is_qualified_str(&role_name_str);
        // Evaluate this parent's precompiled bracket-argument chunks
        // (ADR-0019 D7-3), if any, instead of leaving candidate resolution
        // to re-parse the concatenated parent string — same bail-out as the
        // class-header site (D4-3): a coercion-type argument
        // (`does R[Str:D(Numeric)]`) parses cleanly as an `Expr` but must
        // NOT be evaluated as one (`should_treat_role_arg_as_type_expr`
        // turns it into a `Package` marker instead), so skip the chunk path
        // for the whole application when any raw argument would trigger it.
        let has_type_expr_arg = role_name_str
            .find('[')
            .map(|start| {
                let args_str = &role_name_str[start + 1..role_name_str.len() - 1];
                parse_role_type_args(args_str)
                    .iter()
                    .any(|a| should_treat_role_arg_as_type_expr(a))
            })
            .unwrap_or(false);
        let pre_args = if has_type_expr_arg {
            None
        } else {
            match &op.args {
                Some(chunks) => {
                    let mut values = Vec::with_capacity(chunks.len());
                    for chunk in chunks {
                        values.push(self.eval_decl_trait_arg(chunk)?);
                    }
                    Some(values)
                }
                None => None,
            }
        };
        let type_subs: Vec<(String, String)> = if !forwards_type_param
            && let Some((_, resolved_param_names, resolved_values)) =
                self.resolve_role_candidate_with_args(&role_name_str, pre_args.as_deref())?
        {
            // Store the resolved param bindings so they are
            // available when the child role is punned to a class
            // and methods referencing role params are dispatched.
            // Also inherit any bindings the PARENT role itself picked up from
            // a role IT composed (`role Kg does U["g"] {}` records "unit" ->
            // "g" under "Kg"; a further `role E does Kg {}` must carry that
            // binding forward under "E", or it is orphaned the moment a class
            // reaches U through two levels of `does` instead of one -- #9834).
            {
                let mut registry = self.registry_mut();
                let inherited = registry
                    .class_role_param_bindings
                    .get(base_role_name)
                    .cloned()
                    .unwrap_or_default();
                let bindings = registry
                    .class_role_param_bindings
                    .entry(name.to_string())
                    .or_default();
                for (p, v) in inherited {
                    bindings.entry(p).or_insert(v);
                }
                for (p, v) in resolved_param_names.iter().zip(resolved_values.iter()) {
                    bindings.insert(p.clone(), v.clone());
                }
            }
            resolved_param_names
                .iter()
                .zip(resolved_values.iter())
                .map(|(p, v)| (p.clone(), type_value_name(v)))
                .collect()
        } else if let Some(parent_type_params) =
            self.registry().role_type_params.get(base_role_name)
        {
            if let Some(bracket_start) = role_name_str.find('[') {
                let args_str = &role_name_str[bracket_start + 1..role_name_str.len() - 1];
                let type_args = parse_role_type_args(args_str);
                parent_type_params
                    .iter()
                    .zip(type_args.iter())
                    .map(|(p, a)| (p.clone(), a.clone()))
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        for attr in &role.attributes {
            if cx
                .role_def
                .attributes
                .iter()
                .any(|a| a.name == attr.name && a.sigil == attr.sigil)
            {
                // Already present. Only a real conflict if both
                // sides declared it directly (vs. inherited from
                // a shared ancestor in a diamond). Skip otherwise.
                let parent_owns = role.own_attribute_names.contains(&attr.name);
                let current_owns = cx.role_def.own_attribute_names.contains(&attr.name);
                if parent_owns && current_owns {
                    cx.role_def.attribute_conflicts.push((
                        attr.name.clone(),
                        name.to_string(),
                        base_role_name.to_string(),
                    ));
                }
            } else {
                cx.role_def.attributes.push(attr.clone());
            }
        }
        // Carry the parent role's own deferred param-type checks (#8083)
        // forward so a bogus type in ONE of its methods is still caught once
        // THIS role is eventually composed into a class -- `role.methods`
        // below is merged into `cx.role_def.methods` verbatim, but the
        // pending-check list living alongside it on the parent `RoleDef`
        // would otherwise be left behind, and no later composition would
        // ever revalidate it.
        if !role.pending_param_type_checks.is_empty() {
            cx.role_def
                .pending_param_type_checks
                .extend(role.pending_param_type_checks.iter().cloned());
        }
        for (mname, overloads) in role.methods {
            // Skip methods declared with `my` scope -- role-private
            // Submethods (is_submethod=true) ARE composed even though
            // they have is_my=true.
            let non_my_overloads: Vec<MethodDef> = overloads
                .into_iter()
                .filter(|md| !md.is_my || md.is_submethod)
                .collect();
            if non_my_overloads.is_empty() {
                continue;
            }
            let composed: Vec<MethodDef> = if type_subs.is_empty() {
                non_my_overloads
                    .into_iter()
                    .map(|mut md| {
                        if md.original_role.is_none() {
                            md.original_role = md.role_origin.clone();
                        }
                        md.role_origin = Some(base_role_name.to_string());
                        md
                    })
                    .collect()
            } else {
                non_my_overloads
                    .iter()
                    .map(|md| {
                        let mut method = substitute_type_params_in_method(md, &type_subs);
                        if method.original_role.is_none() {
                            method.original_role = method.role_origin.clone();
                        }
                        method.role_origin = Some(base_role_name.to_string());
                        method
                    })
                    .collect()
            };
            cx.role_def
                .methods
                .entry(mname)
                .or_default()
                .extend(composed);
        }
        Ok(())
    }
}
