//! Named phases of `register_class_decl` (ADR-0019 D0): the `has`-attribute
//! arm of the class-body walk plus the attribute-default validation it and
//! runtime `has` registration share. Pure mechanical extraction from
//! `registration_class_decl.rs` — no behavior change.

use super::registration_class_body::{ClassBodyCx, ClassBodyFlow};
use super::*;

impl Interpreter {
    /// Register an attribute onto a class whose body is still being defined,
    /// driven by a `has`-declaration that reached the VM at runtime (mainline /
    /// EVAL'd source: `class Foo { BEGIN EVAL q[has $.x] }`). This mirrors the
    /// per-instance-attribute branch of `register_class_decl` for the common
    /// case (name/type/smiley/built + accessor visibility); traits, `handles`,
    /// `where`, `is default`, and role composition are not supported here
    /// (an EVAL'd `has` carrying those is exceedingly rare).
    pub(crate) fn register_runtime_attribute(
        &mut self,
        class_name: &str,
        spec: &crate::opcode::RuntimeHasDeclSpec,
    ) -> Result<(), RuntimeError> {
        let attr_name = &spec.attr_name;
        let Some(mut class_def) = self.registry().classes.get(class_name).cloned() else {
            return Ok(());
        };
        // Already declared (e.g. a duplicate EVAL): no-op rather than abort.
        if class_def.attributes.iter().any(|a| &a.name == attr_name) {
            return Ok(());
        }
        self.validate_static_attribute_default(
            attr_name,
            spec.sigil,
            spec.default.as_ref(),
            spec.type_constraint.as_deref(),
            spec.type_smiley.as_deref(),
        )?;
        let effective_is_rw = !spec.is_readonly && spec.is_rw;
        class_def.attributes.push(ClassAttributeDef {
            name: attr_name.clone(),
            is_public: spec.is_public,
            default: spec.default.clone(),
            is_rw: effective_is_rw,
            is_required: spec.is_required.clone(),
            sigil: spec.sigil,
            where_constraint: None,
        });
        if let Some(tc) = &spec.type_constraint {
            let resolved_tc = tc.replace("::?CLASS", class_name);
            class_def
                .attribute_types
                .insert(attr_name.clone(), resolved_tc);
        }
        if let Some(ts) = &spec.type_smiley {
            class_def
                .attribute_smileys
                .insert(attr_name.clone(), ts.clone());
        }
        if let Some(built) = spec.is_built {
            class_def.attribute_built.insert(attr_name.clone(), built);
        }
        self.registry_mut()
            .classes
            .insert(class_name.to_string(), class_def);
        self.clear_private_zeroarg_method_cache();
        Ok(())
    }

    /// Rakudo decides at *compile* time that an attribute initializer can never
    /// satisfy its constraint and reports X::TypeCheck::Attribute::Default
    /// ("Can never assign default value ..."). The decidable case is a *defined*
    /// literal default: of the wrong type, or any defined value under `:U`. A
    /// type-object default is NOT decidable here — `has Int:D $.n = Int` is a
    /// construction-time X::TypeCheck::Assignment — so it is left to the
    /// smiley check that runs when the instance is built.
    fn validate_static_attribute_default(
        &mut self,
        attr_name: &str,
        sigil: char,
        default: Option<&Expr>,
        type_constraint: Option<&str>,
        type_smiley: Option<&str>,
    ) -> Result<(), RuntimeError> {
        // `@`/`%` constraints apply to the elements, not the container.
        if sigil != '$' {
            return Ok(());
        }
        let Some(Expr::Literal(val)) = default else {
            return Ok(());
        };
        if !crate::runtime::types::value_is_defined(val) {
            return Ok(());
        }
        let Some(base) = type_constraint.map(str::to_string) else {
            return Ok(());
        };
        let smiley = type_smiley.unwrap_or("_");
        if smiley != "U" && self.type_matches_value(&base, val) {
            return Ok(());
        }
        let constraint = Self::join_constraint_smiley(&base, smiley);
        Err(crate::runtime::utils::attribute_default_never_assign_error(
            attr_name,
            &constraint,
            val,
        ))
    }

    /// The `has` arm of the class-body walk: register the declared attribute
    /// (or class-level attribute) on the class under construction.
    pub(super) fn class_body_has_decl(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        stmt: &Stmt,
    ) -> Result<ClassBodyFlow, RuntimeError> {
        let Stmt::HasDecl {
            name: attr_name,
            is_public,
            default,
            handles,
            is_rw,
            is_readonly,
            type_constraint,
            type_smiley,
            is_required,
            sigil,
            where_constraint,
            is_alias,
            is_our,
            is_my,
            is_default,
            is_type,
            deprecated_message,
            is_built,
            unknown_traits,
        } = stmt
        else {
            unreachable!("class_body_has_decl called on a non-HasDecl statement");
        };
        let attr_name_str = attr_name.resolve();

        // An initializer that can never satisfy the constraint is a
        // declaration-time error in rakudo, before anything is built.
        if let Err(err) = self.validate_static_attribute_default(
            &attr_name_str,
            *sigil,
            default.as_ref(),
            type_constraint.as_deref(),
            type_smiley.as_deref(),
        ) {
            self.set_current_package(cx.saved_package.clone());
            self.env = cx.saved_env.clone();
            return Err(err);
        }

        // Handle unknown traits. If a user-defined `trait_mod:<is>`
        // (or `trait_mod:<will>`, etc.) can handle the trait, dispatch
        // to it with an Attribute introspection object; otherwise raise
        // X::Comp::Trait::Unknown. Kept in a separate method so its
        // locals don't inflate this already-large function's frame.
        if !unknown_traits.is_empty() {
            if let Err(err) = self.apply_attribute_traits(
                unknown_traits,
                &attr_name_str,
                *sigil,
                *is_public,
                cx.name,
                type_constraint.as_deref(),
            ) {
                self.set_current_package(cx.saved_package.clone());
                self.env = cx.saved_env.clone();
                return Err(err);
            }
            // A user-defined `trait_mod:<is>` may have called
            // `.^add_method` on the class currently being composed
            // (e.g. Attribute::Predicate's `is predicate` adds a
            // `has-foo` accessor). Those methods land directly in the
            // registry entry, but the local `class_def` — re-inserted
            // at the end of body processing — would clobber them.
            // Merge any registry methods not already present locally,
            // mirroring the class_def re-sync done after run_block_raw.
            if let Some(reg_cd) = self.registry().classes.get(cx.name) {
                let added: Vec<(String, Vec<MethodDef>)> = reg_cd
                    .methods
                    .iter()
                    .filter(|(mname, _)| !cx.class_def.methods.contains_key(*mname))
                    .map(|(mname, mdefs)| (mname.clone(), mdefs.clone()))
                    .collect();
                for (mname, mdefs) in added {
                    cx.class_def.methods.insert(mname, mdefs);
                }
            }
        }

        // Handle class-level attributes (our $.x / my $.x)
        if *is_our || *is_my {
            // Evaluate the default value if present
            let initial_value = if let Some(expr) = default {
                self.eval_block_value(&[Stmt::Expr(expr.clone())])?
            } else {
                Value::NIL
            };
            cx.class_def
                .class_level_attrs
                .insert(attr_name_str.clone(), initial_value);
            // Skip per-instance attribute registration
            return Ok(ClassBodyFlow::SkipTail);
        }

        // Check for duplicate attribute from role composition
        if cx
            .class_def
            .attributes
            .iter()
            .any(|a| a.name == attr_name_str)
        {
            self.set_current_package(cx.saved_package.clone());
            self.env = cx.saved_env.clone();
            return Err(RuntimeError::new(format!(
                "X::Comp::Trait::Duplicate: attribute '{}' already exists in class '{}' (possibly from role composition)",
                attr_name_str, cx.name,
            )));
        }
        let effective_is_rw = !*is_readonly && (*is_rw || (cx.class_is_rw && *is_public));
        cx.class_def.attributes.push(ClassAttributeDef {
            name: attr_name_str.clone(),
            is_public: *is_public,
            default: default.clone(),
            is_rw: effective_is_rw,
            is_required: is_required.clone(),
            sigil: *sigil,
            where_constraint: where_constraint.as_ref().map(|wc| wc.as_ref().clone()),
        });
        // Store `is default(...)` trait value for this attribute.
        // When is_default is set, the evaluated value is stored for
        // .VAR.default and Nil-restore behavior.
        // When only `default` is set (from `is default(X)` without `= value`),
        // also store it as the is_default trait value.
        if let Some(is_default_expr) = is_default {
            if let Ok(val) = self.eval_block_value(&[Stmt::Expr(is_default_expr.clone())]) {
                // Type-check the default value against the attribute's type
                // constraint. For an object hash (`%.a{KeyType}`) the
                // constraint is `ValueType{KeyType}`; the `is default`
                // value is an *element* default, so check it against the
                // value type only.
                if let Some(tc) = type_constraint {
                    let tc = tc
                        .split_once('{')
                        .map(|(value_tc, _)| value_tc)
                        .unwrap_or(tc.as_str());
                    let type_ok = if val.is_nil() {
                        // Nil is only valid for untyped or Nil-accepting attributes
                        tc == "Any" || tc == "Mu" || tc.contains("Nil")
                    } else {
                        self.type_matches_value(tc, &val)
                    };
                    if !type_ok {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Type check failed in assignment to attribute; expected {} but got {}",
                                tc, super::utils::value_type_name(&val)
                            )),
                        );
                        attrs.insert(
                            "expected".to_string(),
                            Value::package(crate::symbol::Symbol::intern(tc)),
                        );
                        attrs.insert(
                            "got".to_string(),
                            if val.is_nil() {
                                Value::NIL
                            } else {
                                val.clone()
                            },
                        );
                        let err = Value::make_instance(
                            crate::symbol::Symbol::intern("X::TypeCheck::Attribute::Default"),
                            attrs,
                        );
                        let mut runtime_err = RuntimeError::new(format!(
                            "X::TypeCheck::Attribute::Default: Type check failed for default value of attribute '{}'; expected {}, got {}",
                            attr_name_str,
                            tc,
                            super::utils::value_type_name(&val)
                        ));
                        runtime_err.exception = Some(Box::new(err));
                        self.set_current_package(cx.saved_package.clone());
                        self.env = cx.saved_env.clone();
                        return Err(runtime_err);
                    }
                }
                self.registry_mut()
                    .class_attribute_defaults
                    .insert((cx.name.to_string(), attr_name_str.clone()), val);
            }
        } else if default.is_some() {
            // No explicit `is default(X)`, but there IS a `default` expr.
            // This means either `has $.a = expr` or `has $.a is default(expr)` without `= value`.
            // We can't distinguish here, so we DON'T set class_attribute_defaults
            // (it would be wrong for `has $.a = 42` — Nil should give (Any), not 42).
        }
        if *is_alias {
            cx.class_def.alias_attributes.insert(attr_name_str.clone());
        }
        if let Some(tc) = type_constraint {
            // Resolve ::?CLASS to the current class name
            let resolved_tc = tc.replace("::?CLASS", cx.name);
            cx.class_def
                .attribute_types
                .insert(attr_name_str.clone(), resolved_tc);
        }
        if let Some(ts) = type_smiley {
            cx.class_def
                .attribute_smileys
                .insert(attr_name_str.clone(), ts.clone());
        }
        if let Some(built) = is_built {
            cx.class_def
                .attribute_built
                .insert(attr_name_str.clone(), *built);
        }
        if let Some(it) = is_type {
            self.registry_mut()
                .class_attribute_is_types
                .insert((cx.name.to_string(), attr_name_str.clone()), it.clone());
        }
        if let Some(dm) = deprecated_message {
            self.registry_mut()
                .class_attribute_deprecated
                .insert((cx.name.to_string(), attr_name_str.clone()), dm.clone());
        }
        let attr_var_name = if *is_public {
            format!(".{}", attr_name_str)
        } else {
            format!("!{}", attr_name_str)
        };
        self.apply_handle_specs(handles, &attr_var_name, &mut cx.class_def);
        Ok(ClassBodyFlow::RunTail)
    }
}
