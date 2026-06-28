//! Type-declaration registration ops: enum / class / augment / role / subset.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn exec_register_enum_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::EnumDecl {
            name,
            variants,
            is_export,
            base_type,
            language_version,
        } = stmt
        {
            let result = loan_env!(
                self,
                register_enum_decl(&name.resolve(), variants, *is_export, base_type.as_deref(),)
            )?;
            // Store language revision metadata from the version captured at parse time
            if !name.resolve().is_empty() {
                self.store_language_revision_from_version(&name.resolve(), language_version);
            }
            // For anonymous enums, push the Map result onto the stack
            if name.resolve().is_empty() {
                self.stack.push(result);
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterEnum expects EnumDecl"))
        }
    }

    pub(super) fn exec_register_class_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        // Registering a class can shadow a same-named earlier class (`my class A`
        // in a fresh lexical scope) with different method bodies/candidates, so the
        // method-resolution caches — keyed on the class NAME symbol — must be
        // invalidated, or a cached resolution from the old class would be reused for
        // the new one. (The multi-resolution cache made this observable:
        // S12-methods/multi.t reuses `my class A`/`B` with multi submethods.)
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        self.dispatch_multi_candidate.clear();
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ClassDecl {
            name,
            name_expr,
            parents,
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            repr,
            body,
            language_version,
            custom_traits,
            ..
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            let current_package = self.current_package().to_string();
            let qualified_name = if let Some(stripped) = resolved_name.strip_prefix("GLOBAL::") {
                // `class GLOBAL::Foo` declares Foo in the global namespace
                stripped.to_string()
            } else if resolved_name.contains("::")
                || current_package == "GLOBAL"
                || resolved_name == current_package
            {
                resolved_name.clone()
            } else {
                format!("{current_package}::{resolved_name}")
            };
            // If the name was previously suppressed (e.g. by a `my class` in an
            // earlier block), clear the suppression before running the class body
            // so that references to the class name inside the body can resolve.
            self.unsuppress_name(&resolved_name);
            // TODO: Detect redeclaration of package-scoped classes across
            // EVAL boundaries (X::Redeclaration). Currently deferred because
            // distinguishing EVAL re-definitions from normal re-execution
            // (e.g., anonymous classes in loops, augment) requires tracking
            // compilation unit boundaries.
            let deferred_traits = loan_env!(
                self,
                register_class_decl(
                    &qualified_name,
                    parents,
                    crate::runtime::ClassDeclModifiers {
                        class_is_rw: *class_is_rw,
                        is_hidden: *is_hidden,
                        is_lexical: *is_lexical,
                        hidden_parents,
                        does_parents,
                        language_version,
                    },
                    body,
                )
            )?;
            // Check for assignment to native read-only params before
            // compiling (X::Assignment::RO::Comp).
            if let Some(err) = self.check_class_native_readonly_param_errors(&qualified_name) {
                return Err(err);
            }
            // Compile method bodies to bytecode for the fast path
            self.compile_class_methods(&qualified_name);
            // Register CUnion repr if present
            if let Some(repr_name) = repr
                && repr_name == "CUnion"
            {
                self.register_cunion_class(&qualified_name);
            }
            // Register the class name in the lexical env so that
            // ::("ClassName") indirect lookups can find it in the current scope.
            let env = self.env_mut();
            env.insert(
                "_".to_string(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            // Always insert the class type object so that class names take
            // precedence over same-named `$`-sigiled variables (whose stripped
            // name may already be in the env).
            env.insert(
                qualified_name.clone(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            // When a nested class is registered inside another class (e.g. class B inside class A
            // becomes A::B), suppress the short name (B) so it cannot be used outside.
            // Only suppress when the parent package is itself a class, not a module.
            // Also register the short name in the lexical env so it is available
            // within the enclosing class body and its methods.
            let parent_is_class = qualified_name
                .rsplit_once("::")
                .map(|(parent, _)| self.has_class(parent))
                .unwrap_or(false);
            if qualified_name != resolved_name && !resolved_name.contains("::") && parent_is_class {
                self.suppress_name(&resolved_name);
                // Register the short name in the lexical env so it resolves
                // within the enclosing class scope (e.g. `Frog` inside `Forest`).
                let env = self.env_mut();
                env.insert(
                    resolved_name.clone(),
                    Value::Package(Symbol::intern(&qualified_name)),
                );
            }
            // When a class is declared with an already-qualified name
            // (e.g. the compiler pre-qualified `class C1` inside
            // `unit module M` to `M::C1`), also register the short name
            // `C1` in the env so that subsequent code inside the same
            // module can refer to it bare. Skip this when the parent
            // package is a class (where suppress_name semantics apply).
            if qualified_name.contains("::") && !parent_is_class {
                let short = qualified_name
                    .rsplit_once("::")
                    .map(|(_, s)| s.to_string())
                    .unwrap_or_else(|| qualified_name.clone());
                // Do not shadow built-in types (e.g. `my class X::Roast::Channel`
                // must not make the bare name `Channel` resolve to the user class).
                if !short.is_empty() && short != qualified_name && !Self::is_builtin_type(&short) {
                    self.env_mut().entry_or_insert_with(short, || {
                        Value::Package(Symbol::intern(&qualified_name))
                    });
                }
            }
            // When `my class` is used, register the class name as lexically scoped
            // so it gets suppressed when the enclosing block scope exits.
            if *is_lexical {
                self.register_lexical_class(resolved_name.clone());
                // Also mark as my-scoped so it's excluded from the parent package stash
                self.mark_my_scoped_package_item(qualified_name.clone());
            }
            // Store language revision metadata from the version captured at parse time
            self.store_language_revision_from_version(&qualified_name, language_version);

            // Dispatch custom `is` traits via trait_mod:<is> if defined.
            // Merge explicitly parsed custom_traits with deferred_traits
            // (unknown lowercase parents deferred from register_class_decl).
            let has_trait_mod =
                self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
            if has_trait_mod && (!custom_traits.is_empty() || !deferred_traits.is_empty()) {
                let type_obj = Value::Package(Symbol::intern(&qualified_name));
                // Dispatch explicitly parsed custom traits (with args)
                for (trait_name, trait_arg) in custom_traits {
                    let trait_value = if let Some(arg_expr) = trait_arg {
                        self.vm_eval_block_value(&[Stmt::Expr(arg_expr.clone())])?
                    } else {
                        Value::Bool(true)
                    };
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(trait_value));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no args)
                for trait_name in &deferred_traits {
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
            }

            // Slice F: write the deferred body's outer-lexical mutations through
            // to this caller frame's local slots (`register_class_decl` ran the
            // body via `run_block_raw`, which recorded them); keeps e.g.
            // `$tracker` coherent without the reverse pull. This op holds the
            // outer `code`.
            self.apply_pending_rw_writeback(code);

            Ok(())
        } else {
            Err(RuntimeError::new("RegisterClass expects ClassDecl"))
        }
    }

    pub(super) fn exec_augment_class_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::AugmentClass {
            name,
            body,
            is_role,
        } = stmt
        {
            let name_str = name.resolve();
            // Check MONKEY-TYPING pragma: we check if `use MONKEY-TYPING` or `use MONKEY`
            // was issued. Since the compiler simply ignores these `use` statements,
            // we track them at the interpreter level.
            if !self.monkey_typing_enabled() {
                return Err(RuntimeError::typed_msg(
                    "X::Syntax::Augment::WithoutMonkeyTyping",
                    "augment not allowed without 'use MONKEY-TYPING'",
                ));
            }
            if *is_role {
                return Err(self.augment_role_error(&name_str));
            }
            loan_env!(self, augment_class(&name_str, body))?;
            // Recompile augmented class methods for the fast path
            self.compile_class_methods(&name_str);
            Ok(())
        } else {
            Err(RuntimeError::new("AugmentClass expects AugmentClass stmt"))
        }
    }

    pub(super) fn exec_register_role_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::RoleDecl {
            name,
            type_params,
            type_param_defs,
            is_export,
            export_tags,
            body,
            is_rw,
            language_version,
            custom_traits,
        } = stmt
        {
            let name_str = name.resolve();
            let current_package = self.current_package().to_string();
            let qualified_name = if let Some(stripped) = name_str.strip_prefix("GLOBAL::") {
                stripped.to_string()
            } else if name_str.contains("::")
                || current_package == "GLOBAL"
                || name_str == current_package
            {
                name_str.clone()
            } else {
                format!("{current_package}::{name_str}")
            };
            // If the short name was suppressed by an earlier lexical type with
            // the same name, re-enable it before registering the new role.
            self.unsuppress_name(&name_str);
            loan_env!(
                self,
                register_role_decl(&qualified_name, type_params, type_param_defs, body, *is_rw,)
            )?;
            if *is_export && !self.suppress_exports {
                // The compiler may have pre-qualified the role name
                // (e.g. `R1` → `GH2613::R1`) when compiling under a
                // `unit module`. Exports use the short bare name and
                // the originating package, so split the qualified name.
                let (export_pkg, export_short) =
                    if let Some((pkg, short)) = name_str.rsplit_once("::") {
                        (pkg.to_string(), short.to_string())
                    } else {
                        (current_package.clone(), name_str.clone())
                    };
                self.register_exported_var(export_pkg, export_short, export_tags.clone());
            }
            // Store language revision metadata from the version captured at parse time
            self.store_language_revision_from_version(&qualified_name, language_version);
            // Compile role method bodies to bytecode
            self.compile_role_methods(&qualified_name);
            self.env_mut().insert(
                "_".to_string(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            self.env_mut().insert(
                qualified_name.clone(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            if qualified_name != name_str && !name_str.contains("::") {
                self.env_mut().insert(
                    name_str.clone(),
                    Value::Package(Symbol::intern(&qualified_name)),
                );
            }
            // When a role is declared with an already-qualified name
            // (e.g. the compiler pre-qualified `role R1` inside
            // `unit module GH2613` to `GH2613::R1`), also register the
            // short name `R1` in the env so subsequent code in the same
            // module can refer to it bare.
            if qualified_name.contains("::") && qualified_name == name_str {
                let short = qualified_name
                    .rsplit_once("::")
                    .map(|(_, s)| s.to_string())
                    .unwrap_or_else(|| qualified_name.clone());
                if !short.is_empty() && short != qualified_name {
                    self.env_mut().entry_or_insert_with(short, || {
                        Value::Package(Symbol::intern(&qualified_name))
                    });
                }
            }
            // Execute deferred non-declaration body statements now that the role
            // name is fully available in the environment.  This lets code like
            // `role R { method foo {}; R.foo }` work.
            if type_params.is_empty() {
                let deferred = self
                    .get_role_def(&qualified_name)
                    .map(|r| r.deferred_body_stmts.clone())
                    .unwrap_or_default();
                for stmt in &deferred {
                    self.vm_run_block_raw(std::slice::from_ref(stmt))?;
                }
                // Slice F: write the deferred body's outer-lexical mutations
                // through to this caller frame's local slots (vm_run_block_raw
                // recorded them); keeps `$side` coherent without the reverse pull.
                self.apply_pending_rw_writeback(code);
            }

            // Gather deferred custom traits from role registration
            let role_deferred = self
                .get_role_def(&qualified_name)
                .map(|r| r.deferred_custom_traits.clone())
                .unwrap_or_default();

            // Dispatch custom `is` traits via trait_mod:<is> if defined
            let has_trait_mod =
                self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
            if has_trait_mod && (!custom_traits.is_empty() || !role_deferred.is_empty()) {
                let type_obj = Value::Package(Symbol::intern(&qualified_name));
                for (trait_name, trait_arg) in custom_traits {
                    // Skip internal markers (e.g. `__my_scoped`); they are not real `is` traits.
                    if trait_name.starts_with("__") {
                        continue;
                    }
                    let trait_value = if let Some(arg_expr) = trait_arg {
                        self.vm_eval_block_value(&[Stmt::Expr(arg_expr.clone())])?
                    } else {
                        Value::Bool(true)
                    };
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(trait_value));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no args)
                for trait_name in &role_deferred {
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
            }

            Ok(())
        } else {
            Err(RuntimeError::new("RegisterRole expects RoleDecl"))
        }
    }

    pub(super) fn exec_register_subset_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubsetDecl {
            name,
            base,
            predicate,
            version,
            is_export,
            export_tags,
        } = stmt
        {
            let resolved_name = name.resolve();
            loan_env!(
                self,
                register_subset_decl(&resolved_name, base, predicate.as_ref(), version,)
            );
            // When a subset is declared `is export` inside a module, record it
            // in the export table so `import M` (and `use M`) can find it.
            // The subset type itself is already registered under its bare name
            // in the global env by `register_subset_decl`, so importing only
            // needs to make `import M` succeed (and validate export tags).
            if *is_export && !self.suppress_exports {
                let (export_pkg, export_short) =
                    if let Some((pkg, short)) = resolved_name.rsplit_once("::") {
                        (pkg.to_string(), short.to_string())
                    } else {
                        (self.current_package().to_string(), resolved_name)
                    };
                self.register_exported_var(export_pkg, export_short, export_tags.clone());
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterSubset expects SubsetDecl"))
        }
    }
}
