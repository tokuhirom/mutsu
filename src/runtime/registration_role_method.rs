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
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        let Stmt::MethodDecl {
            name: method_name,
            name_expr,
            params: _,
            param_defs,
            body: method_body,
            multi,
            is_rw,
            is_private,
            is_our: _,
            is_my,
            is_submethod,
            our_variable_form: _,
            return_type,
            is_default_candidate,
            deprecated_message,
            handles: method_handles,
            custom_traits: _,
            is_export: _,
            export_tags: _,
        } = stmt
        else {
            unreachable!("role_body_method_decl called on a non-MethodDecl statement");
        };
        let name = cx.name;
        // Validate that $!attr references in the method body are declared
        // in this role (same check as for class methods).
        Self::validate_attr_declared_in_class(&cx.attr_ctx(), method_body)?;
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
        for pd in param_defs {
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
            let filtered: Vec<_> = method_body
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
        if *multi
            && body_is_stub
            && (param_defs.iter().any(|pd| {
                pd.type_constraint
                    .as_deref()
                    .is_some_and(|tc| tc.contains("?CLASS"))
            }) || return_type
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
        let chunk_idx = cx.method_name_chunk_idx;
        cx.method_name_chunk_idx += 1;
        let resolved_method_name = if name_expr.is_some() {
            let chunk = cx
                .method_name_chunks
                .get(chunk_idx)
                .and_then(|c| c.as_ref())
                .expect("method_name_chunks misaligned with role body walk");
            self.run_decl_expr(chunk)?.to_string_value()
        } else {
            method_name.resolve()
        };
        // A method always carries an implicit `*%_` slurpy so callers
        // can pass (or forward) named arguments the signature does not
        // name. Class methods get this via `effective_method_param_defs`
        // at registration; role methods must too, so a role-composed
        // method absorbs stray named args the same way a class-declared
        // one does.
        let effective_param_defs = Self::effective_method_param_defs(param_defs, false);
        let effective_params: Vec<String> = effective_param_defs
            .iter()
            .map(|p| p.name.clone())
            .collect();
        let def = MethodDef {
            lexical_package: self.current_package(),
            params: effective_params,
            param_defs: effective_param_defs,
            body: std::sync::Arc::new(method_body.clone()),
            is_rw: *is_rw,
            is_private: *is_private,
            is_multi: *multi,
            is_my: *is_submethod,
            role_origin: None,
            original_role: None,
            return_type: return_type.clone(),
            compiled_code: None,
            compiled_fns: None,
            delegation: None,
            is_default: *is_default_candidate,
            deprecated_message: deprecated_message.clone(),
            is_submethod: *is_submethod,
            captured_env: None,
        };
        // `my method` in roles are role-private, skip method table.
        // Submethods (is_submethod) DO get composed even though
        // is_my is true.
        let is_role_private = *is_my && !*is_submethod;
        if !is_role_private {
            if *multi {
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
        if !is_role_private && !method_handles.is_empty() {
            let source_attr_marker = format!("&{}", resolved_method_name);
            for spec in method_handles {
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
