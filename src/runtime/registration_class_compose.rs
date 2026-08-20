//! Named phases of `register_class_decl` (ADR-0019 D0): composition of the
//! roles named in the class header (`does Role` / `is Role`) into the class
//! under construction. Pure mechanical extraction from
//! `registration_class_decl.rs` — no behavior change.

use super::registration_class::{
    ResolvedRoleCandidate, parse_role_type_args, should_treat_role_arg_as_type_expr,
    substitute_type_params_in_method, type_value_name,
};
use super::registration_class_decl::BUILTIN_PARENT_TYPES;
use super::*;
use crate::symbol::Symbol;

/// Replace whole type-name tokens in `name` that exactly match a role type
/// parameter with its concrete type name. Tokens are delimited by `[`, `]`,
/// `,`, and whitespace, so `Array[TV]` with `TV -> Rat:D` becomes
/// `Array[Rat:D]` while a nested-class name like `G::A` (no embedded param) is
/// left untouched.
pub(super) fn substitute_type_param_tokens(name: &str, subs: &[(String, String)]) -> String {
    if subs.is_empty() {
        return name.to_string();
    }
    let mut result = String::with_capacity(name.len());
    let mut token = String::new();
    let flush = |token: &mut String, result: &mut String| {
        if !token.is_empty() {
            let replacement = subs
                .iter()
                .find(|(p, _)| p == token)
                .map(|(_, r)| r.as_str())
                .unwrap_or(token.as_str());
            result.push_str(replacement);
            token.clear();
        }
    };
    for ch in name.chars() {
        if matches!(ch, '[' | ']' | ',' | ' ') {
            flush(&mut token, &mut result);
            result.push(ch);
        } else {
            token.push(ch);
        }
    }
    flush(&mut token, &mut result);
    result
}

/// Everything the role-composition walk over the class-header parents
/// produces besides the mutations to the `ClassDef` itself. Consumed by the
/// binding store, the pun installer, and the composed-role recorder that run
/// right after the walk.
#[derive(Default)]
pub(super) struct RoleCompositionOutcome {
    pub(super) composed_roles_list: Vec<String>,
    /// The DIRECTLY-declared role parents (one per class-header `does`/`is`
    /// role), captured before any transitive sub-role concretizations are
    /// flattened into `composed_roles_list`. Used for qualified-call
    /// concretization resolution (see `class_direct_composed_roles`).
    pub(super) direct_composed_roles: Vec<String>,
    pub(super) punned_roles: Vec<String>,
    pub(super) hidden_punned_role_bases: HashSet<String>,
    pub(super) class_role_param_bindings: rustc_hash::FxHashMap<String, Value>,
}

/// Shared state of the role-composition phase, passed by `&mut` instead of
/// threading the individual locals through every helper (ADR-0019 D0).
pub(super) struct RoleCompositionCx<'a> {
    pub(super) name: &'a str,
    pub(super) class_lang_rev: &'a str,
    pub(super) class_def: &'a mut ClassDef,
    pub(super) out: RoleCompositionOutcome,
    /// See [`super::registration_class::ClassDeclModifiers::is_hoisted_shell`].
    pub(super) is_hoisted_shell: bool,
}

impl Interpreter {
    /// Compose roles listed in the parents (from "does Role" or "is Role" in
    /// the class header).
    pub(super) fn compose_class_parent_roles(
        &mut self,
        cx: &mut RoleCompositionCx<'_>,
        parents: &[String],
        does_parents: &[String],
        parent_pre_args: &[Option<&[crate::opcode::DeclTraitArg]>],
    ) -> Result<(), RuntimeError> {
        const BUILTIN_TYPES: &[&str] = BUILTIN_PARENT_TYPES;
        for (i, parent) in parents.iter().enumerate() {
            let resolved_parent_name = self.resolve_declared_type_name(parent);
            let base_role_name = resolved_parent_name
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(resolved_parent_name.as_str());
            // Evaluate this parent's precompiled bracket-argument chunks
            // (ADR-0019 D4-3), if any, instead of leaving candidate
            // resolution to re-parse the concatenated parent string.
            //
            // A coercion-type argument (`R[Str:D(Numeric)]`) parses cleanly
            // as an `Expr` (D4-1) — `Str:D(Numeric)` is syntactically a call
            // — but it must NOT be evaluated as one: `eval_role_arg_values`'s
            // `should_treat_role_arg_as_type_expr` heuristic exists
            // precisely to turn this shape into a `Package` marker instead
            // of calling it, and the value path has no equivalent. Reuse
            // that same classification here as a bail-out: if any raw
            // argument in this parent's bracket would trigger it, skip the
            // chunk path for the WHOLE application and fall back to the
            // string path, which already handles it correctly.
            let has_type_expr_arg = resolved_parent_name
                .find('[')
                .map(|start| {
                    let args_str = &resolved_parent_name[start + 1..resolved_parent_name.len() - 1];
                    parse_role_type_args(args_str)
                        .iter()
                        .any(|a| should_treat_role_arg_as_type_expr(a))
                })
                .unwrap_or(false);
            let pre_args = if has_type_expr_arg {
                None
            } else {
                match parent_pre_args.get(i).copied().flatten() {
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
            if let Some(resolved) =
                self.resolve_role_candidate_with_args(&resolved_parent_name, pre_args.as_deref())?
            {
                // Check if this role was specified via `is` (punning) vs `does` (composition)
                let is_punned = !does_parents.contains(parent);
                self.compose_role_into_class(
                    cx,
                    &resolved_parent_name,
                    base_role_name,
                    is_punned,
                    resolved,
                )?;
            } else if does_parents.contains(parent)
                && self.registry().enum_types.contains_key(base_role_name)
            {
                // Enum used as a role via `does`: record it for method dispatch
                self.registry_mut()
                    .class_enum_roles
                    .entry(cx.name.to_string())
                    .or_default()
                    .push(base_role_name.to_string());
            } else if does_parents.contains(parent)
                && BUILTIN_TYPES.contains(&base_role_name)
                && !self.registry().roles.contains_key(base_role_name)
                && !cx.out.composed_roles_list.contains(&resolved_parent_name)
            {
                // Built-in type used as a role via `does` (e.g., `does Numeric`,
                // `does Real`): record in composed_roles_list so that role-based
                // method dispatch (e.g., .Numeric on type objects) works correctly.
                cx.out
                    .composed_roles_list
                    .push(resolved_parent_name.clone());
            }
        }
        Ok(())
    }

    /// Compose one resolved role candidate (attributes, methods, deferred
    /// body, transitive parents) into the class under construction.
    pub(super) fn compose_role_into_class(
        &mut self,
        cx: &mut RoleCompositionCx<'_>,
        resolved_parent_name: &str,
        base_role_name: &str,
        is_punned: bool,
        resolved: ResolvedRoleCandidate,
    ) -> Result<(), RuntimeError> {
        let (role, role_param_names, role_arg_values) = resolved;
        if role.is_stub_role {
            return Err(RuntimeError::typed_msg(
                "X::Role::Parametric::NoSuchCandidate",
                "No matching candidate found for the parametric role",
            ));
        }
        // Check for attribute conflicts detected during role composition
        if let Some((attr_name, role_a, role_b)) = role.attribute_conflicts.first() {
            return Err(RuntimeError::new(format!(
                "Attribute '$!{}' conflicts in role '{}' composition: declared in both '{}' and '{}'",
                attr_name, base_role_name, role_a, role_b
            )));
        }
        if is_punned {
            cx.out.punned_roles.push(resolved_parent_name.to_string());
            if role.is_hidden {
                cx.out
                    .hidden_punned_role_bases
                    .insert(base_role_name.to_string());
            }
        }
        cx.out
            .composed_roles_list
            .push(resolved_parent_name.to_string());
        cx.out
            .direct_composed_roles
            .push(resolved_parent_name.to_string());
        // Look up the role's language revision for submethod composition rules.
        let role_lang_rev = self
            .type_metadata
            .get(base_role_name)
            .and_then(|m| m.get("language-revision"))
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "c".to_string());
        // Submethods from roles are only composed when the class is 6.c
        // AND the role is also 6.c. In 6.d+, submethods are never composed
        // from roles.
        let compose_submethods = cx.class_lang_rev == "c" && role_lang_rev == "c";
        // Collect type parameter substitutions for method type constraints.
        let type_subs: Vec<(String, String)> = role_param_names
            .iter()
            .zip(role_arg_values.iter())
            .map(|(p, v)| (p.clone(), type_value_name(v)))
            .collect();
        for (p, v) in role_param_names.iter().zip(role_arg_values.iter()) {
            cx.out
                .class_role_param_bindings
                .insert(p.clone(), v.clone());
        }
        // Per-candidate role param bindings (`T => Int`), stamped onto each
        // composed MethodDef below in addition to the flat per-class map
        // above. The flat map is last-write-wins when the same role is
        // composed twice with different type args (`does R[Int] does
        // R[Str]`), so a candidate's own body must carry its OWN binding to
        // read the right `T` at dispatch time instead of whichever
        // composition ran last (see
        // news/2026-08/role-double-parametric-multi-dispatch.md).
        let candidate_role_bindings: Option<std::sync::Arc<Vec<(String, Value)>>> =
            if role_param_names.is_empty() {
                None
            } else {
                Some(std::sync::Arc::new(
                    role_param_names
                        .iter()
                        .cloned()
                        .zip(role_arg_values.iter().cloned())
                        .collect(),
                ))
            };
        for attr in &role.attributes {
            if !cx.class_def.attributes.iter().any(|a| a.name == attr.name) {
                cx.class_def.attributes.push(attr.clone());
            }
        }
        // Carry each composed-role class-level attribute (`my $.x`/`our $.x`)
        // onto the consuming class as a class-level attribute, so the accessor
        // works on the class type object (`C.x`), matching raku. The default
        // expr is evaluated now (role param bindings, if any, are in scope).
        let role_class_level: Vec<(String, Option<crate::opcode::DeclTraitArg>)> = self
            .registry()
            .role_class_level_attrs
            .iter()
            .filter(|((r, _), _)| r == base_role_name)
            .map(|((_, attr), arg)| (attr.clone(), arg.clone()))
            .collect();
        for (attr, default) in role_class_level {
            let value = if let Some(arg) = default {
                self.eval_decl_trait_arg(&arg)?
            } else {
                Value::NIL
            };
            cx.class_def.class_level_attrs.insert(attr, value);
        }
        // Carry each composed-role attribute's deferred `is default(...)`
        // expression onto the consuming class so it can be evaluated at
        // construction with this class's type-param bindings in scope.
        let role_default_exprs: Vec<(String, crate::opcode::DeclTraitArg)> = self
            .registry()
            .role_attribute_default_exprs
            .iter()
            .filter(|((r, _), _)| r == base_role_name)
            .map(|((_, attr), arg)| (attr.clone(), arg.clone()))
            .collect();
        for (attr, arg) in role_default_exprs {
            self.registry_mut()
                .class_attribute_default_exprs
                .entry((cx.name.to_string(), attr))
                .or_insert(arg);
        }
        // Carry each composed-role attribute's `is Type` container trait
        // (`has @.a is Array[TV]`, `has @.a is G::A`) onto the consuming
        // class so its element type is enforced at construction. Type
        // parameters embedded in the type name (`Array[TV]`) are resolved
        // to their concrete args (`Array[Rat:D]`); a nested-class trait
        // (`G::A`) has no embedded param and is repointed to the
        // parameterized class by the rename pass below.
        let role_is_types: Vec<(String, String)> = self
            .registry()
            .role_attribute_is_types
            .iter()
            .filter(|((r, _), _)| r == base_role_name)
            .map(|((_, attr), ty)| (attr.clone(), substitute_type_param_tokens(ty, &type_subs)))
            .collect();
        for (attr, ty) in role_is_types {
            self.registry_mut()
                .class_attribute_is_types
                .entry((cx.name.to_string(), attr))
                .or_insert(ty);
        }
        // Carry each composed-role attribute's declared type constraint
        // (`role R { has Int $.x }`) onto the consuming class, so it is
        // enforced and introspectable exactly like a class-declared one.
        // `::?CLASS` in a role attribute names the consuming class, and
        // a role type parameter (`has T $.v`) resolves to this
        // composition's argument.
        let role_attr_types: Vec<(String, String)> = self
            .registry()
            .role_attribute_types
            .iter()
            .filter(|((r, _), _)| r == base_role_name)
            .map(|((_, attr), tc)| {
                (
                    attr.clone(),
                    substitute_type_param_tokens(&tc.replace("::?CLASS", cx.name), &type_subs),
                )
            })
            .collect();
        for (attr, tc) in role_attr_types {
            cx.class_def.attribute_types.entry(attr).or_insert(tc);
        }
        let role_attr_smileys: Vec<(String, String)> = self
            .registry()
            .role_attribute_smileys
            .iter()
            .filter(|((r, _), _)| r == base_role_name)
            .map(|((_, attr), s)| (attr.clone(), s.clone()))
            .collect();
        for (attr, s) in role_attr_smileys {
            cx.class_def.attribute_smileys.entry(attr).or_insert(s);
        }
        for (mname, overloads) in &role.methods {
            // Skip methods declared with `my` scope -- they are role-private
            // and should not be composed into consuming classes.
            // Submethods (is_submethod=true) ARE composed only when both
            // the class and role share 6.c language revision.
            let non_my_overloads: Vec<&MethodDef> = overloads
                .iter()
                .filter(|md| !md.is_my || (md.is_submethod && compose_submethods))
                .collect();
            if non_my_overloads.is_empty() {
                continue;
            }
            let composed: Vec<MethodDef> = if type_subs.is_empty() {
                non_my_overloads
                    .into_iter()
                    .map(|md| {
                        let mut method = md.clone();
                        if method.original_role.is_none() {
                            method.original_role = method.role_origin.clone();
                        }
                        method.role_origin = Some(base_role_name.to_string());
                        method.role_param_bindings = candidate_role_bindings.clone();
                        method
                    })
                    .collect()
            } else {
                non_my_overloads
                    .into_iter()
                    .map(|md| {
                        let mut method = substitute_type_params_in_method(md, &type_subs);
                        if method.original_role.is_none() {
                            method.original_role = method.role_origin.clone();
                        }
                        method.role_origin = Some(base_role_name.to_string());
                        method.role_param_bindings = candidate_role_bindings.clone();
                        method
                    })
                    .collect()
            };
            let owner = Symbol::intern(cx.name);
            let method_sym = Symbol::intern(mname);
            let mut registry = self.registry_mut();
            for def in composed {
                registry.push_user_method(owner, method_sym, def);
            }
        }
        // Transfer wildcard handles from role to class
        for wh in &role.wildcard_handles {
            if !cx.class_def.wildcard_handles.contains(wh) {
                cx.class_def.wildcard_handles.push(wh.clone());
            }
        }
        let role_param_values: HashMap<String, Value> = role_param_names
            .iter()
            .cloned()
            .zip(role_arg_values.iter().cloned())
            .collect();
        // A role's deferred body must run once per (class, role) composition,
        // not once per `register_class_decl` call. Rakudo memoises the
        // composed *type*: `class A does R {}` re-executed against the same
        // already-existing type object `A` (e.g. from inside a `for` loop
        // that redeclares the class each pass, or a re-`EVAL`) does not
        // re-run `R`'s deferred body a second time, even though `A`'s own
        // mainline statements DO re-run every pass — only role composition
        // is idempotent, not the class body itself. Two DISTINCT classes
        // composing the same role each get their own run (verified against
        // `raku`; see the case table in
        // news/2026-08/role-composition-memo-key-raku-case-table.md), so the
        // key must include the target class name, not just the role.
        //
        // A `__hoisted` forward-reference shell (see
        // `ClassDeclModifiers::is_hoisted_shell`'s doc comment) skips the
        // deferred body entirely, rather than running it (guarded or not):
        // the shell's registration is throwaway and superseded at runtime by
        // the real, source-position declaration re-registering the SAME
        // (class, role) pair later, and the shell runs in a transient
        // environment whose effects never reach the program's real state
        // anyway (methods/attributes DO need to be visible on the shell for
        // a forward reference to resolve them, which the copy above this
        // guard already handles unconditionally; the deferred body is
        // arbitrary side-effecting code, not structural declarations, so it
        // has no such forward-reference need). Two bugs came from getting
        // this wrong: memoising the shell's run under the same key as the
        // real one left the real declaration's composition silently skipped
        // (`t/run-nested-role-body.t`'s `$side` never got set); running it
        // unconditionally on every shell pass double-ran it for every
        // ordinary top-level class declaration, since a shell always
        // precedes the real pass (caught by the new
        // `t/role-body-composition-timing.t` two-distinct-classes case,
        // which counted 4 runs instead of 2).
        if !cx.is_hoisted_shell {
            let compose_key = format!("class:{}:{resolved_parent_name}", cx.name);
            if self
                .registry_mut()
                .composed_role_bodies
                .insert(compose_key.clone())
            {
                // A body that dies (a guard rejecting this parameterisation,
                // `role Guarded[::T] { die unless ... }`) must reject EVERY
                // attempt at this same composition, not just the first: the
                // key was inserted before running the body to guarantee it is
                // consumed at most once on success, but a failed attempt has
                // not actually composed anything, so its slot must be freed
                // again on error rather than permanently masking the retry
                // (`t/role-body-guard-parameterisation.t`'s `.new` on a
                // rejected parameterisation caught this).
                let run = (|| -> Result<(), RuntimeError> {
                    self.run_composed_role_deferred_body(
                        cx,
                        base_role_name,
                        &role,
                        &role_param_values,
                        &role_arg_values,
                    )?;
                    // Composing a role composes the roles it composes, so
                    // their bodies run too — nearest first, which is the
                    // order Rakudo runs them in for `role GP {...}; role P
                    // does GP {...}; class K does P { }` (P, then GP). Their
                    // methods already transit into the class below; only the
                    // bodies were missing.
                    self.run_composed_role_ancestor_bodies(base_role_name, cx.name)
                })();
                if run.is_err() {
                    self.registry_mut()
                        .composed_role_bodies
                        .remove(&compose_key);
                }
                run?;
            }
        }
        self.propagate_composed_role_parent_specs(cx, base_role_name, &role, &role_param_values);
        Ok(())
    }
}
