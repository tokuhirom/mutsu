//! Named passes of `register_role_decl` (ADR-0019 D0): the body validation
//! pre-pass, registration-state reset, the body pre-scan, the body walk
//! driver, and the candidate finalization. Pure mechanical extraction from
//! `registration_role.rs` — no behavior change.

use super::registration_class::AttrValidationCtx;
use super::*;
use crate::ast::ParamDef;

/// Shared state of the role-declaration walk, passed by `&mut` to the
/// per-statement arms instead of threading the individual locals
/// (ADR-0019 D0).
pub(super) struct RoleDeclCx<'a> {
    pub(super) name: &'a str,
    pub(super) type_params: &'a [String],
    pub(super) role_is_rw: bool,
    pub(super) role_def: RoleDef,
    /// Attribute names declared in this role body (pre-scan pass).
    pub(super) role_own_attrs: HashSet<String>,
    /// Names of modules `use`d / `need`ed inside the body (pre-scan pass).
    pub(super) body_used_modules: HashSet<String>,
    /// Types declared inside the role body itself (pre-scan pass).
    pub(super) body_declared_types: HashSet<String>,
    /// Precompiled typed descriptor for each attribute this role declares
    /// (ADR-0019 D2b remainder), keyed by attribute name; see
    /// `role_body_has_decl`.
    pub(super) attr_decls: &'a [(Symbol, crate::opcode::CompiledAttrDecl)],
    /// Precompiled runtime-resolved-name chunk for each top-level `method`/
    /// `submethod` declaration in the body (ADR-0019 D3-1), read by position
    /// via `method_name_chunk_idx`; see `role_body_method_decl`.
    pub(super) method_name_chunks: &'a [Option<crate::opcode::CompiledDeclExpr>],
    /// Precompiled typed mirror of each top-level `method`/`submethod`
    /// declaration in the body (ADR-0019 D3-7), read by position via
    /// `method_name_chunk_idx`; see `role_body_method_decl`.
    pub(super) method_decls: &'a [crate::opcode::CompiledMethodDecl],
    /// Cursor into `method_name_chunks`/`method_decls`, advanced by one on
    /// every method statement `role_body_method_decl` processes.
    pub(super) method_name_chunk_idx: usize,
    /// Typed `does`/`hides`/`is hidden` ops for this role's own body
    /// (ADR-0019 D7-3), read by position via `parent_op_idx`; see
    /// `role_body_does_decl`.
    pub(super) parent_ops: &'a [crate::opcode::RoleParentOp],
    /// Cursor into `parent_ops`, advanced by one on every `DoesDecl`
    /// statement `role_body_does_decl` processes.
    pub(super) parent_op_idx: usize,
    /// The ambient program-wide compiled-function pool (ADR-0019 D3-8c); see
    /// `role_body_method_decl`.
    pub(super) compiled_fns: &'a crate::opcode::CompiledFns,
}

impl RoleDeclCx<'_> {
    pub(super) fn attr_ctx(&self) -> AttrValidationCtx<'_> {
        AttrValidationCtx {
            attrs: &self.role_own_attrs,
            pkg_name: self.name,
            pkg_kind: "role",
        }
    }
}

impl Interpreter {
    /// Validate the role's own type-parameter list: a bare-type parameter
    /// (`role R[SomeType]`) must name a resolvable type.
    pub(super) fn check_role_type_param_validity(
        &mut self,
        type_param_defs: &[ParamDef],
    ) -> Result<(), RuntimeError> {
        for param_def in type_param_defs {
            if param_def.name == "__type_only__"
                && let Some(type_name) = param_def.type_constraint.as_deref()
                && !type_name.starts_with("::")
                && !self.is_resolvable_type(type_name)
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("type".to_string(), Value::str(type_name.to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Invalid type '{}' used in role parameter list",
                        type_name
                    )),
                );
                return Err(RuntimeError::typed("X::Parameter::InvalidType", attrs));
            }
        }
        Ok(())
    }

    /// Clean up stale registry entries for this role name before
    /// re-registration, returning the previous parents when registering a
    /// parametric variant over an existing non-parametric role (so the role
    /// group can restore them afterwards).
    pub(super) fn reset_role_registration_state(
        &mut self,
        name: &str,
        type_params: &[String],
    ) -> Option<Vec<String>> {
        // Clean up stale punned class entry for this role name.
        self.registry_mut().classes.remove(name);
        // ADR-0019 F4c-8(e): explicit mutator calls, matching
        // `__MUTSU_UNREGISTER_CLASS__`'s own F4c-8(e) fix -- this
        // `classes.remove` called no sync at all, leaving permanently stale
        // `method_entries` rows behind from the withdrawn pun.
        let owner = crate::symbol::Symbol::intern(name);
        self.registry_mut().clear_user_methods_for_owner(owner);
        self.registry_mut().sync_accessor_entries(owner);
        self.registry_mut().hidden_classes.remove(name);
        self.registry_mut().class_composed_roles.remove(name);
        // When registering a parametric variant of an existing non-parametric role
        // (forming a role group), save the non-parametric role's parents so we can
        // restore them after the parametric variant adds its own parents.
        let prev_parents = if !type_params.is_empty()
            && self
                .registry()
                .roles
                .get(name)
                .is_some_and(|existing| !existing.is_stub_role)
        {
            self.registry().role_parents.get(name).cloned()
        } else {
            None
        };
        self.registry_mut().role_parents.remove(name);
        self.registry_mut().role_hides.remove(name);
        prev_parents
    }

    /// Walk the role body, dispatching each op through the per-statement
    /// arms. Declared attributes, used modules, and body-declared types are
    /// precomputed by the compiler (ADR-0019 D2a) and already populate `cx`
    /// by the time this runs.
    ///
    /// `body_plan` (ADR-0019 D7-4/D9) is the sole driver of this walk — it
    /// is already single-level `SyntheticBlock`-flattened and classified by
    /// the compiler (`crate::opcode::role_body_plan`), so there is no
    /// runtime-side flatten to redo.
    pub(super) fn walk_role_body(
        &mut self,
        body_plan: &[crate::opcode::RoleBodyOp],
        cx: &mut RoleDeclCx<'_>,
    ) -> Result<(), RuntimeError> {
        // Attribute names, `use`d/`need`ed module names, and body-declared
        // types are precomputed by the compiler at plan lowering (ADR-0019
        // D2a) and already populate `cx.role_own_attrs`/`body_used_modules`/
        // `body_declared_types` — see `register_role_decl`. A `unit role X;
        // use A::B::C; method m(A::B::C:D $p) {...}` imports the type at
        // BEGIN time, but this registration validates method param types
        // before the body's `use` has loaded the module, so a qualified
        // imported type looks unresolvable; the precomputed used-module
        // names let the param check accept a qualified type that a body
        // import supplies (the real resolution happens at the call site
        // regardless). Types declared inside the role body itself (`my
        // enum`, `my subset`, `my class`, ...) are not yet in the registry
        // while the role's method signatures are validated, so the
        // precomputed names are accepted as parameter/attribute constraints;
        // the real registration happens when the role body runs below.
        for op in body_plan {
            match op {
                crate::opcode::RoleBodyOp::Attr { name } => {
                    self.role_body_has_decl(cx, *name)?;
                }
                crate::opcode::RoleBodyOp::Parent => {
                    self.role_body_does_decl(cx)?;
                }
                crate::opcode::RoleBodyOp::Method => {
                    self.role_body_method_decl(cx)?;
                }
                crate::opcode::RoleBodyOp::Deferred { raw } => {
                    if let Stmt::Expr(Expr::Call { name, .. }) = raw.as_ref()
                        && (name == "__mutsu_stub_die" || name == "__mutsu_stub_warn")
                    {
                        cx.role_def.is_stub_role = true;
                    }
                    // Every other statement (non-method/non-attribute/non-`does`,
                    // including `SetLine` source-line markers) is deferred to
                    // composition time, so role methods can be called from
                    // within the role block body (e.g. `role R { method foo {};
                    // R.foo }`) and, for a parameterized role, so it can be
                    // re-evaluated with concrete type bindings. The compiler's
                    // `deferred_body_ops` (ADR-0019 D8-1/D8-2) already covers
                    // running it at every composition site — nothing to record
                    // here.
                }
            }
        }
        Ok(())
    }

    /// Finalize the registration: capture the closure env when needed, record
    /// the role candidate, publish the `RoleDef`, and merge back a role
    /// group's previous parents.
    pub(super) fn finish_role_registration(
        &mut self,
        name: &str,
        type_params: &[String],
        type_param_defs: &[ParamDef],
        language_version: &str,
        mut role_def: RoleDef,
        prev_parents: Option<Vec<String>>,
    ) {
        // Capture the current environment for anonymous roles so that attribute
        // defaults referencing closure variables can be evaluated later.
        let has_expr_default = role_def
            .attributes
            .iter()
            .any(|attr| attr.default.is_some());
        if has_expr_default {
            role_def.captured_env = Some(self.env.flatten());
        }
        // Capture the parents that were added during this registration
        // (these are the parents specific to this candidate).
        let candidate_parents = self
            .registry()
            .role_parents
            .get(name)
            .cloned()
            .unwrap_or_default();
        let candidate = RoleCandidateDef {
            type_params: type_params.to_vec(),
            type_param_defs: type_param_defs.to_vec(),
            role_def: role_def.clone(),
            parents: candidate_parents,
            // The revision the role was *declared* under, snapshotted at parse
            // time (`Stmt::RoleDecl.language_version`) exactly like a class's.
            // Reading the parser global here instead would report whatever
            // revision happens to be active when the declaration executes,
            // which for a role in a `use`d module is the importer's.
            language_version: language_version.to_string(),
        };
        {
            let mut registry = self.registry_mut();
            let cands = registry
                .role_candidates
                .entry(name.to_string())
                .or_default();
            // Re-registering the same declaration (a `__hoisted` shell followed
            // by the in-place declaration, a module body re-run, a loop) must
            // REPLACE the same-signature candidate, not append a duplicate:
            // `.HOW.candidates` counts these, and two same-signature candidates
            // in one scope are impossible in Raku (roast
            // S14-roles/parameterized-basic.t counts 3, not 6).
            let sig_match = cands.iter().position(|c| {
                c.type_params == candidate.type_params
                    && format!("{:?}", c.type_param_defs)
                        == format!("{:?}", candidate.type_param_defs)
            });
            match sig_match {
                Some(i) => cands[i] = candidate,
                None => cands.push(candidate),
            }
        }
        if self
            .registry()
            .roles
            .get(name)
            .is_none_or(|existing| existing.is_stub_role || type_params.is_empty())
        {
            self.registry_mut().roles.insert(name.to_string(), role_def);
            self.registry_mut()
                .user_declared_roles
                .insert(name.to_string());
        }
        if !type_params.is_empty() && !self.registry().role_type_params.contains_key(name) {
            self.registry_mut()
                .role_type_params
                .insert(name.to_string(), type_params.to_vec());
        }
        // When a parametric variant was registered over an existing non-parametric
        // role (forming a role group), merge the previous parents back into
        // role_parents so that role_parent_args_for can find all candidates' parents.
        if let Some(prev) = prev_parents {
            let mut registry = self.registry_mut();
            let current = registry.role_parents.entry(name.to_string()).or_default();
            for p in prev {
                if !current.contains(&p) {
                    current.push(p);
                }
            }
        }
    }
}
