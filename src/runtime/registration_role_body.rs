//! Attribute declarations and eager lexical routine/subset registration for
//! the role-body walk (ADR-0019 D0).

use super::registration_role_decl::RoleDeclCx;
use super::*;

impl Interpreter {
    /// The `has` arm of the role-body walk: record the attribute (or
    /// class-level attribute) and its per-(role, attr) trait metadata.
    pub(super) fn role_body_has_decl(
        &mut self,
        cx: &mut RoleDeclCx<'_>,
        name: crate::symbol::Symbol,
        sigil: char,
    ) -> Result<(), RuntimeError> {
        // Look up this attribute's precompiled descriptor (ADR-0019 D2b
        // remainder/D10) by name — see the identical rationale in
        // `class_body_has_decl`. `compile_role_attr_decls` has always
        // covered a class-level `our`/`my` role attribute too (unlike the
        // class side before D10), so this lookup has never needed a
        // raw-statement fallback.
        let decl = cx
            .attr_decls
            .iter()
            .find(|(n, decl)| *n == name && decl.sigil == sigil)
            .map(|(_, decl)| decl.clone())
            .expect("role_body_has_decl: no attr_decls entry for this Attr op's name");
        let attr_name_str = decl.name.clone();
        // A class-level (`my $.x` / `our $.x`) role attribute is NOT a
        // per-instance attribute: it becomes a class-level attribute on the
        // consuming class (accessor on the type object, `C.x`) — exactly
        // like `class C { my $.x }`. Record its default expr keyed by
        // (role, attr) and skip the per-instance attribute registration, so
        // no per-instance accessor shadows the class-level fallback.
        if decl.is_my || decl.is_our {
            self.registry_mut().role_class_level_attrs.insert(
                (cx.name.to_string(), attr_name_str.clone()),
                decl.default.clone(),
            );
            return Ok(());
        }
        cx.role_def
            .own_attribute_names
            .insert(attr_name_str.clone());
        // Carry an `is Type` container trait (`has @.a is G::A`) so it
        // can be transferred to the consuming class at composition and
        // its element type enforced.
        if let Some(it) = &decl.is_type {
            self.registry_mut()
                .role_attribute_is_types
                .insert((cx.name.to_string(), attr_name_str.clone()), it.clone());
        }
        // The declared type of a role attribute (`has Int $.x`,
        // `has Callable %!c{Mu:U}`) is recorded per (role, attr) and
        // copied onto every consuming/punned class, since a role has
        // no class of its own to hold `attribute_types`. `::?CLASS`
        // stays unresolved here — it names the *consuming* class, so
        // it is substituted at composition.
        if let Some(tc) = &decl.type_constraint {
            self.registry_mut()
                .role_attribute_types
                .insert((cx.name.to_string(), attr_name_str.clone()), tc.clone());
        }
        if let Some(ts) = &decl.type_smiley {
            self.registry_mut()
                .role_attribute_smileys
                .insert((cx.name.to_string(), attr_name_str.clone()), ts.clone());
        }
        // `is default(...)` on a role attribute can reference the role's
        // type parameters (`is default(T)`), so it cannot be evaluated
        // until composition. Stash the expression keyed by (role, attr);
        // it is copied to the consuming class and evaluated at instance
        // construction (with type params bound).
        if let Some(def_arg) = &decl.is_default {
            self.registry_mut().role_attribute_default_exprs.insert(
                (cx.name.to_string(), attr_name_str.clone()),
                def_arg.clone(),
            );
        }
        // Check if this attribute already exists from a composed role
        if let Some(existing) = cx
            .role_def
            .attributes
            .iter()
            .find(|a| a.name == attr_name_str && a.sigil == decl.sigil)
        {
            // The attribute already exists from a parent role composition.
            // Record the conflict; the existing one came from a composed role.
            // We need to figure out which role contributed it.
            let parent_role = self
                .registry()
                .role_parents
                .get(cx.name)
                .and_then(|parents| {
                    parents.iter().find(|p| {
                        let base = p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                        self.registry().roles.get(base).is_some_and(|r| {
                            r.attributes
                                .iter()
                                .any(|a| a.name == attr_name_str && a.sigil == decl.sigil)
                        })
                    })
                })
                .cloned()
                .unwrap_or_else(|| "unknown".to_string());
            let _ = existing;
            cx.role_def.attribute_conflicts.push((
                attr_name_str.clone(),
                cx.name.to_string(),
                parent_role,
            ));
        }
        if crate::runtime::attribute_accessor_conflicts(
            &cx.role_def.attributes,
            &attr_name_str,
            decl.sigil,
            decl.is_public,
        ) {
            return Err(RuntimeError::new(format!(
                "Two or more attributes declared that both want an accessor method '{}'",
                attr_name_str
            )));
        }
        // Apply role-level `is rw`: same logic as class_is_rw
        // `is readonly` on individual attributes overrides `is rw` on the role
        let effective_is_rw =
            !decl.is_readonly && (decl.is_rw || (cx.role_is_rw && decl.is_public));
        let declared_shape = decl
            .declared_shape
            .clone()
            .or_else(|| self.resolve_dynamic_attr_shape(&decl));
        cx.role_def.attributes.push(ClassAttributeDef {
            name: attr_name_str.clone(),
            is_public: decl.is_public,
            default: decl.default.clone(),
            captured_env: self.capture_attr_decl_lexicals(decl.default.as_ref()),
            captured_unit: None,
            declaring_package: Some(crate::symbol::Symbol::intern(cx.name)),
            is_rw: effective_is_rw,
            is_required: decl.is_required.clone(),
            sigil: decl.sigil,
            type_constraint: decl.type_constraint.clone(),
            where_constraint: decl.where_constraint.clone(),
            declared_shape,
            source_line: decl.decl_line,
            source_file: self.current_source_file_sym(),
            default_is_seed: decl.default_is_seed,
            default_is_trait: decl.default_is_trait,
        });
        if decl.where_constraint.is_some() {
            Self::mark_attr_where_constraint_seen();
        }
        if let Some(built) = decl.is_built {
            cx.role_def
                .attribute_built
                .insert(attr_name_str.clone(), built);
        }
        if Self::declares_box_target(&decl) {
            self.register_box_target(cx.name, &attr_name_str);
        }
        let attr_var_name = if decl.is_public {
            format!(".{}", attr_name_str)
        } else {
            format!("!{}", attr_name_str)
        };
        self.apply_handle_specs_to_role(&decl.handles, &attr_var_name, &mut cx.role_def);
        self.record_role_attribute_traits(cx, &decl, &attr_name_str);
        Ok(())
    }


    /// Register every `is export`ed `subset` declared directly in a role body
    /// at ROLE-DECLARATION time, in addition to the composition-time run.
    ///
    /// A role body's statements are deferred to composition (see
    /// `exec_register_role_op`'s comment on why), but a `subset` is a
    /// *declaration*, and rakudo installs it — and its `is export` entry — when
    /// the role is compiled, not when some class finally composes it. Deferring
    /// it wholesale meant a module that hands its types out from a role body
    /// exported nothing until an unrelated class composed the role:
    ///
    /// ```raku
    /// # Types.rakumod
    /// role Types { my subset IndRef of Pair is export(:IndRef) where {.key eq 'ind-ref'} }
    /// # consumer
    /// use Types :IndRef;   # imported nothing; `IndRef` fell back to a bareword Str
    /// ```
    ///
    /// `PDF::COS` is exactly this shape, and `PDF::COS::Tie`'s
    /// `multi method deref(IndRef $ind-ref!)` then failed role-composition
    /// validation with `Invalid typename 'IndRef' in parameter declaration.` —
    /// six PDF-family distributions ([#7993]).
    ///
    /// [#7993]: https://github.com/tokuhirom/mutsu/issues/7993
    ///
    /// Only `is export` subsets are registered here. A role-private one is
    /// already accepted in its own body via `RoleDeclCx::body_declared_types`
    /// and has no cross-module consumer to serve, so registering it early would
    /// widen its visibility for no gain. A subset whose base type is one of the
    /// role's own type parameters (`role R[::T] { my subset S of T … }`) is
    /// skipped too: its base is not known until the role is parameterised, so
    /// composition remains the only point where it can be registered correctly.
    pub(crate) fn register_role_body_exported_subsets(
        &mut self,
        role_name: &str,
        deferred_body_ops: &[crate::opcode::DeferredBodyOp],
        type_params: &[String],
    ) {
        for op in deferred_body_ops {
            let Stmt::SubsetDecl {
                name,
                base,
                predicate,
                version,
                is_export: true,
                export_tags,
                is_my,
                ..
            } = &op.raw
            else {
                continue;
            };
            if type_params.iter().any(|tp| tp == base) {
                continue;
            }
            let resolved_name = name.resolve();
            self.register_subset_decl(
                &resolved_name,
                base,
                predicate.as_ref(),
                None,
                None,
                version,
                *is_my,
                0,
            );
            if self.module.suppress_exports {
                continue;
            }
            let (export_pkg, export_short) = match crate::qualified::split_qualified(
                crate::qualified::known_symbol(&resolved_name),
            )
            .map(|(head, tail)| (head.as_str(), tail.as_str()))
            {
                Some((pkg, short)) => (pkg.to_string(), short.to_string()),
                // A role body's own package IS the role, so an unqualified
                // subset exports from the role's name (`use PDF::COS :IndRef`).
                None => (role_name.to_string(), resolved_name.clone()),
            };
            self.register_exported_var(export_pkg, export_short, export_tags.clone());
        }
    }

    /// Register every top-level (non-method) `sub` declared directly in a role
    /// body when the role is declared, rather than waiting for a later
    /// composition.
    ///
    /// A plain `sub` in a role body is a lexical helper of the role's own
    /// compilation unit, not a composition-time effect: Rakudo makes it (and
    /// any `is export` sub that calls it) callable the moment the role loads,
    /// with no class ever composing the role. Before this ran only the
    /// `is export`ed subs early (see the historical `Red::ResultSeq` note
    /// below) — a role-private helper stayed in `deferred_body_ops`, invisible
    /// until composition, so an exported sub that forward-referenced one (the
    /// common "public wrapper calls a private `_helper`" shape,
    /// `Date::Calendar::Strftime`'s `strftime` -> `_strftime`) died with
    /// "Unknown function" the moment it was called without ever composing the
    /// role. `run_block_raw`'s ordinary `SubDecl` handling already
    /// marks a non-`is export` sub my-scoped (see
    /// `runtime_encoding::mark_my_scoped_package_item`), so registering it
    /// here does not leak it to an importer that never asked for it — it only
    /// makes it resolvable to bare-name calls from routines running under this
    /// role's own package, exactly like a module's own private top-level
    /// helper already is.
    ///
    /// `SubDecl` is the only *executable* statement widened this way — a
    /// plain (or side-effecting) statement stays composition-only, since
    /// re-running it eagerly AND again at every composition would replay its
    /// side effects. Lexical TYPE declarations (`my class`, `my grammar`, …)
    /// get the equivalent treatment in
    /// [`Self::register_role_body_lexical_types`].
    ///
    /// Like an exported subset, an exported routine is a declaration that a
    /// consumer can import immediately.  Deferring it until composition loses
    /// `use Red::ResultSeq`'s `create-resultseq` export: Red's custom HOW calls
    /// that routine while composing a model, before anything has composed the
    /// `Red::ResultSeq` role itself.
    pub(crate) fn register_role_body_exported_subs(
        &mut self,
        role_name: &str,
        deferred_body_ops: &[crate::opcode::DeferredBodyOp],
    ) -> Result<(), RuntimeError> {
        let saved_package = self.current_package().to_string();
        self.set_current_package(role_name.to_string());
        for op in deferred_body_ops {
            // An exported sub-level `proto` (`proto sub to-file(|) is
            // export(:to-file) {*}` in PDF::Filespec) declares the routine its
            // multis join, so it registers with them — once per role
            // (`role_registered_sub_protos`).
            if !matches!(
                &op.raw,
                Stmt::SubDecl { .. }
                    | Stmt::ProtoDecl {
                        is_method: false,
                        is_export: true,
                        ..
                    }
            ) {
                continue;
            }
            if let Stmt::ProtoDecl { name, .. } = &op.raw
                && self
                    .registry()
                    .role_registered_sub_protos
                    .contains(&(role_name.to_string(), name.resolve()))
            {
                continue;
            }
            if let Err(error) = self.run_block_raw(std::slice::from_ref(&op.raw)) {
                self.set_current_package(saved_package);
                return Err(error);
            }
            if let Stmt::ProtoDecl { name, .. } = &op.raw {
                self.registry_mut()
                    .role_registered_sub_protos
                    .insert((role_name.to_string(), name.resolve()));
            }
        }
        self.set_current_package(saved_package);
        Ok(())
    }
}
