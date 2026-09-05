//! Named passes of `register_role_decl` (ADR-0019 D0): the `method` /
//! `submethod` arm of the role-body walk. Pure mechanical extraction from
//! `registration_role.rs` — no behavior change.

use super::registration_class::make_delegation_method;
use super::registration_role_decl::RoleDeclCx;
use super::*;
use crate::ast::HandleSpec;

impl Interpreter {
    /// The `method` arm of the role-body walk: validate the declaration and
    /// install its `MethodDef` (and `handles` forwarders) on the role.
    pub(super) fn role_body_method_decl(
        &mut self,
        cx: &mut RoleDeclCx<'_>,
    ) -> Result<(), RuntimeError> {
        // ADR-0019 D3-7: `decl` is precompiled by the compiler at plan
        // lowering (`CompiledRoleDeclPlan::method_decls`) and read here by
        // position via the same cursor `method_name_chunks` already uses
        // (D3-1/D3-3) instead of calling `CompiledMethodDecl::from_stmt` on
        // the raw statement. This walk does not read
        // `is_our`/`our_variable_form`/`custom_traits`/`is_export`/
        // `export_tags` — a role method is never `our`-registered as a
        // package sub and custom traits/exports on a role method are not
        // handled here, matching the walk's original ignored bindings.
        let chunk_idx = cx.method_name_chunk_idx;
        cx.method_name_chunk_idx += 1;
        let decl = cx
            .method_decls
            .get(chunk_idx)
            .cloned()
            .expect("method_decls misaligned with role body walk");
        let name = cx.name;
        // Validate that $!attr references in the method body are declared
        // in this role (same check as for class methods).
        Self::validate_attr_declared_in_class(&cx.attr_ctx(), &decl.body)?;
        // Validate that type constraints in method parameters are resolvable.
        // Undeclared types like A::C should throw X::Parameter::InvalidType.
        // A role nested in an enclosing package (e.g. `unit class A`)
        // registers sibling classes under the qualified name
        // (`A::Identifier`), but a method param may reference them by
        // their short name. The role's own registered name carries the
        // enclosing namespace (`SQL::Abstract::Renderer::SQL`); collect
        // every `::`-prefix of it (`SQL::Abstract::Renderer`,
        // `SQL::Abstract`, `SQL`) so an unqualified sibling type resolves
        // under whichever enclosing package actually declared it. The
        // role's own short name can itself be compound (`Renderer::SQL`),
        // so a single rsplit is not enough.
        let mut enclosing_prefixes: Vec<String> = Vec::new();
        {
            let mut rest = name;
            while let Some((pfx, _)) = rest.rsplit_once("::") {
                enclosing_prefixes.push(pfx.to_string());
                rest = pfx;
            }
        }
        // The role's registered short name may omit the enclosing
        // package (a compound role like `Renderer::SQL` is stored
        // without the `unit class` prefix), so also qualify with the
        // current package and its own `::`-prefixes.
        {
            let cur = self.current_package();
            let mut rest = cur.as_str();
            loop {
                if !rest.is_empty() && !enclosing_prefixes.iter().any(|p| p == rest) {
                    enclosing_prefixes.push(rest.to_string());
                }
                match rest.rsplit_once("::") {
                    Some((pfx, _)) => rest = pfx,
                    None => break,
                }
            }
        }
        for pd in &decl.param_defs {
            if let Some(tc) = pd.type_constraint.as_deref() {
                // Skip type captures (::T), invocant markers, and role type params
                if tc.starts_with("::")
                    || tc == "__invocant__"
                    || cx.type_params.iter().any(|tp| tp == tc)
                {
                    continue;
                }
                // Base name of the constraint: strip definedness smiley,
                // coercion `(...)`, and parameterization `[...]`.
                let tc_base = tc
                    .strip_suffix(":D")
                    .or_else(|| tc.strip_suffix(":U"))
                    .or_else(|| tc.strip_suffix(":_"))
                    .unwrap_or(tc);
                let tc_base = tc_base.split(['(', '[']).next().unwrap_or(tc_base);
                // A role may reference its own type in a method param
                // (`role R { method f(R:D $x) }`); the role is not yet in
                // the registry while its methods validate, so accept its
                // own name (full or short) explicitly.
                let self_short = name.rsplit_once("::").map(|(_, s)| s).unwrap_or(name);
                let resolvable = tc_base == name
                    || tc_base == self_short
                    // A role type parameter carrying a definiteness
                    // smiley (`role R[::T] { method f(T:D $x) }`,
                    // NativeHelpers::CStruct's `LinearArray`). The
                    // bare-`T` check above compares the whole
                    // constraint, so only the base name matches here.
                    || cx.type_params.iter().any(|tp| tp == tc_base)
                    // A type declared in this role's own body (`my enum`,
                    // `my subset`, ...) is not registered until the body
                    // runs; accept its name here.
                    || cx.body_declared_types.contains(tc_base)
                    || self.is_resolvable_type(tc)
                    || (!tc.contains("::")
                        && enclosing_prefixes
                            .iter()
                            .any(|pfx| self.is_resolvable_type(&format!("{pfx}::{tc}"))))
                    // Last resort: any registered type known by this
                    // short name. A compound role name in a `unit
                    // package` (`my role Packet::Empty`) leaves the
                    // enclosing package out of both the role name and
                    // `current_package`, so the prefix walk cannot
                    // reach a sibling like `P::DecodeBuffer`; a
                    // short-name match still finds it, mirroring how the
                    // sub pre-pass accepts any type declared in the unit.
                    || (!tc.contains("::")
                        && self.type_known_by_short_name(tc_base))
                    // A qualified type supplied by a module `use`d within
                    // this role body is not yet loaded at registration
                    // time (the body's `use` runs after this validation),
                    // so accept it if a body import could provide it. The
                    // call site resolves it for real.
                    || (tc.contains("::")
                        && cx.body_used_modules.iter().any(|m| {
                            tc_base == m
                                || tc_base.starts_with(&format!("{m}::"))
                                || m.starts_with(&format!("{tc_base}::"))
                        }));
                if !resolvable {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("type".to_string(), Value::str(tc.to_string()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Invalid typename '{}' in parameter declaration.",
                            tc
                        )),
                    );
                    return Err(RuntimeError::typed("X::Parameter::InvalidType", attrs));
                }
            }
        }
        // Stub multi methods (body is `{...}`) that use ::?CLASS
        // must be implemented by the composing class.
        // Non-stub multi methods with ::?CLASS are fine.
        let body_is_stub = {
            let filtered: Vec<_> = decl
                .body
                .iter()
                .filter(|s| !matches!(s, Stmt::SetLine(_)))
                .collect();
            filtered.len() == 1
                && matches!(
                    filtered[0],
                    Stmt::Expr(Expr::Call { name, .. })
                        if name == "__mutsu_stub_die"
                            || name == "__mutsu_stub_warn"
                )
        };
        if decl.multi
            && body_is_stub
            && (decl.param_defs.iter().any(|pd| {
                pd.type_constraint
                    .as_deref()
                    .is_some_and(|tc| tc.contains("?CLASS"))
            }) || decl
                .return_type
                .as_deref()
                .is_some_and(|rt| rt.contains("?CLASS")))
        {
            return Err(RuntimeError::typed_msg(
                "X::Role::Unimplemented::Multi",
                "Unimplemented multi method from role",
            ));
        }
        // ADR-0019 D3-1: see `class_body_method_decl`'s identical comment —
        // the compiler and this walk flatten `SyntheticBlock` identically, so
        // the chunk at this cursor position matches this statement.
        let resolved_method_name = if decl.name_expr.is_some() {
            let chunk = cx
                .method_name_chunks
                .get(chunk_idx)
                .and_then(|c| c.as_ref())
                .expect("method_name_chunks misaligned with role body walk");
            self.run_decl_expr(chunk)?.to_string_value()
        } else {
            decl.name.resolve()
        };
        // A method always carries an implicit `*%_` slurpy so callers
        // can pass (or forward) named arguments the signature does not
        // name. Class methods get this via `effective_method_param_defs`
        // at registration; role methods must too, so a role-composed
        // method absorbs stray named args the same way a class-declared
        // one does.
        let effective_param_defs =
            crate::method_signature_shared::effective_method_param_defs(&decl.param_defs, false);
        let effective_params: Vec<String> = effective_param_defs
            .iter()
            .map(|p| p.name.clone())
            .collect();
        // ADR-0019 D3-8c: install by key with the same equality guard as
        // the class walker (D3-8b, decision 4). `add_role_decl_plan`
        // compiled this body with `is_hidden: false` and no auto `@_`
        // detection (matching this walk exactly, no ::?CLASS-style
        // substitution here), so `effective_param_defs` above IS what
        // `compile_method_body` computed — no separate pre-substitution
        // snapshot needed, unlike the class side. A mismatch (or missing
        // key) leaves `compiled_code`/`compiled_fns` `None`, falling back
        // unchanged to the registration-time throwaway compile. Installing
        // here (inside `register_role_decl`, before the `role_candidates`
        // snapshot is cloned) is what makes the per-composing-class
        // recompile disappear for free (design decision 6).
        let matched_compiled_fn = decl
            .compiled_routine_key
            .and_then(|key| cx.compiled_fns.get(&key))
            .filter(|cf| {
                let expected_full_params: Vec<String> =
                    ["self", "__ANON_STATE__", "?CLASS", "?ROLE"]
                        .iter()
                        .map(|s| s.to_string())
                        .chain(effective_params.iter().cloned())
                        .collect();
                cf.params == expected_full_params
                    && format!("{:?}", cf.param_defs) == format!("{effective_param_defs:?}")
            });
        let installed_compiled_code =
            matched_compiled_fn.map(|cf| std::sync::Arc::new(cf.code.clone()));
        let installed_compiled_fns = matched_compiled_fn.and_then(|cf| cf.compiled_fns.clone());
        let def = MethodDef {
            lexical_package: self.current_package(),
            params: effective_params,
            param_defs: effective_param_defs,
            body: std::sync::Arc::new(decl.body.clone()),
            is_rw: decl.is_rw,
            is_raw: decl.is_raw,
            is_private: decl.is_private,
            is_multi: decl.multi,
            is_my: decl.is_submethod,
            role_origin: None,
            original_role: None,
            return_type: decl.return_type.clone(),
            compiled_code: installed_compiled_code,
            compiled_fns: installed_compiled_fns,
            delegation: None,
            is_default: decl.is_default_candidate,
            deprecated_message: decl.deprecated_message.clone(),
            is_submethod: decl.is_submethod,
            captured_env: None,
            source_file: self.current_source_file(),
            role_param_bindings: None,
        };
        // `my method` in roles are role-private, skip method table.
        // Submethods (is_submethod) DO get composed even though
        // is_my is true.
        let is_role_private = decl.is_my && !decl.is_submethod;
        if !is_role_private {
            if decl.multi {
                cx.role_def
                    .methods
                    .entry(resolved_method_name.clone())
                    .or_default()
                    .push(def);
            } else {
                // A public `method STORE` and a private `method !STORE`
                // share the base name but are distinct methods (dispatch
                // filters by is_private). Preserve any existing entry of
                // the opposite privacy instead of overwriting it; only a
                // genuine same-privacy redeclaration replaces the prior
                // non-multi def.
                let entry = cx
                    .role_def
                    .methods
                    .entry(resolved_method_name.clone())
                    .or_default();
                entry.retain(|d| d.is_private != def.is_private || d.is_multi);
                entry.push(def);
            }
        }
        // `handles` on a role method: synthesize forwarder methods.
        if !is_role_private && !decl.handles.is_empty() {
            let source_attr_marker = format!("&{}", resolved_method_name);
            for spec in &decl.handles {
                match spec {
                    HandleSpec::Name(target) => {
                        cx.role_def
                            .methods
                            .entry(target.clone())
                            .or_default()
                            .push(make_delegation_method(&source_attr_marker, target));
                    }
                    HandleSpec::Rename { exposed, target } => {
                        cx.role_def
                            .methods
                            .entry(exposed.clone())
                            .or_default()
                            .push(make_delegation_method(&source_attr_marker, target));
                    }
                    HandleSpec::Wildcard => {
                        cx.role_def
                            .wildcard_handles
                            .push(source_attr_marker.clone());
                    }
                    HandleSpec::Regex(pattern) => {
                        cx.role_def
                            .wildcard_handles
                            .push(format!("{}:regex:{}", source_attr_marker, pattern));
                    }
                    HandleSpec::Type(_) => {}
                }
            }
        }
        Ok(())
    }
}
