//! Type-declaration registration ops: enum / class / augment / role / subset.
use super::*;
use crate::ast::Expr;
use crate::symbol::Symbol;

impl Interpreter {
    /// Whether a stmt-pool entry is a `__hoisted` declaration-only shell
    /// emitted by `hoist_type_decl_shells` (compiler). Shell registration
    /// errors are swallowed at the dispatch site — the in-place declaration
    /// reports any real error.
    pub(super) fn stmt_is_hoisted_type_shell(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::ClassDecl { custom_traits, .. } | Stmt::RoleDecl { custom_traits, .. } => {
                custom_traits.iter().any(|(t, _)| t == "__hoisted")
            }
            _ => false,
        }
    }

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
            is_my: _,
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
            // Push the enum's Map value. In expression position — `my $e = enum Foo
            // <a b c>` or a bare `enum <a b c>` — this Map is the declaration's value;
            // in statement position it is a harmless sink absorbed at the frame's stack
            // base (the anonymous form has always pushed unconditionally this way, so
            // the named form is symmetric).
            self.stack.push(result);
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
        self.native_ctor_plan_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        self.func_multi_resolve_cache.clear();
        self.func_multi_type_cacheable.clear();
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
            decl_id,
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
            } else if current_package == "GLOBAL"
                || resolved_name == current_package
                || resolved_name.starts_with(&format!("{current_package}::"))
            {
                resolved_name.clone()
            } else {
                // A *nested* declared name is qualified by the enclosing package
                // like any other: `module M { class A::B { } }` declares
                // `M::A::B`, and `A::B` is not visible on its own. Registering it
                // under the bare `A::B` both leaked it into GLOBAL and left
                // `M::A::B.new` unable to find its own ClassDef (`.^name` already
                // reported the qualified name).
                format!("{current_package}::{resolved_name}")
            };
            // A lexical (`my`) class is stored in the registry under a *storage
            // name* that is normally the same as `qualified_name`. But when a
            // *different* source declaration site reuses the same name *after the
            // earlier one's scope has exited* (e.g. two `my class Foo` in
            // separate `gather` blocks), it must not clobber the first one —
            // instances of the first class still reference it by name. Such a
            // later, out-of-scope collision is stored under a mangled internal
            // name `Foo\u{0}<site-id>`. The site id is the parse-time-assigned
            // `decl_id`, stable across re-executions of the same site (a loop
            // body keeps one identity) but distinct between sites. `decl_id == 0`
            // (deserialized/synthesized node) opts out.
            //
            // A same-name declaration that is *upgrading a stub* of the same name
            // (`my class C { ... }` then `my class C { ... }`) is NOT a collision:
            // it must keep the bare name so the definition lands on the same
            // registry entry. So only mangle when the existing same-named class
            // is already a fully-defined (non-stub) class from a different site.
            let storage_name = if *is_lexical && *decl_id != 0 {
                match self.lexical_class_site_owner(&qualified_name) {
                    Some(owner)
                        if owner != *decl_id
                            && self.has_class(&qualified_name)
                            && !self.registry().class_stubs.contains(&qualified_name) =>
                    {
                        format!("{qualified_name}\u{0}{decl_id}")
                    }
                    _ => {
                        self.set_lexical_class_site_owner(qualified_name.clone(), *decl_id);
                        qualified_name.clone()
                    }
                }
            } else {
                qualified_name.clone()
            };
            // If the name was previously suppressed (e.g. by a `my class` in an
            // earlier block), clear the suppression before running the class body
            // so that references to the class name inside the body can resolve.
            self.unsuppress_name(&resolved_name);
            // Parent class references (`is Foo`) are stored by bare name, but a
            // lexical parent may live in the registry under a mangled storage
            // name (see `storage_name` above). Resolve each parent through the
            // current lexical env so `is C0` binds to the C0 visible in *this*
            // scope, not a same-named class from an unrelated earlier scope.
            let mapped_parents: Vec<String> = parents
                .iter()
                .map(|p| self.lexical_env_remap_name(p))
                // Qualify a bare parent that names a sibling class in the current
                // package but collides with a built-in namespace (`class X::Decode
                // is X` inside `module M`, where `X` is both `M::X` and the built-in
                // `X::` exception namespace). Must run here, where `current_package`
                // is the enclosing module — the child class name reaches
                // `register_class_decl` without its module prefix.
                .map(|p| self.qualify_sibling_parent_name(&p))
                // Drop the auto-added `Grammar` default parent from a genuine
                // top-level `grammar Grammar` (qualified name exactly `Grammar`,
                // which would otherwise list itself as its own parent and loop the
                // MRO walk). A module-local `grammar Grammar` qualifies to
                // `Mod::Grammar`, so its `Grammar` parent (the built-in) is NOT
                // itself and is kept — that is how the parser can unconditionally
                // add the `Grammar` default parent. An EXPLICIT `class Foo is Foo`
                // is left intact so it still raises the self-inheritance error.
                .filter(|p| !(p == "Grammar" && qualified_name == "Grammar"))
                .collect();
            let mapped_hidden_parents: Vec<String> = hidden_parents
                .iter()
                .map(|p| self.lexical_env_remap_name(p))
                .collect();
            // TODO: Detect redeclaration of package-scoped classes across
            // EVAL boundaries (X::Redeclaration). Currently deferred because
            // distinguishing EVAL re-definitions from normal re-execution
            // (e.g., anonymous classes in loops, augment) requires tracking
            // compilation unit boundaries.
            let deferred_traits = loan_env!(
                self,
                register_class_decl(
                    &storage_name,
                    &mapped_parents,
                    crate::runtime::ClassDeclModifiers {
                        class_is_rw: *class_is_rw,
                        is_hidden: *is_hidden,
                        is_lexical: *is_lexical,
                        hidden_parents: &mapped_hidden_parents,
                        does_parents,
                        language_version,
                    },
                    body,
                )
            )?;
            // Check for assignment to native read-only params before
            // compiling (X::Assignment::RO::Comp).
            if let Some(err) = self.check_class_native_readonly_param_errors(&storage_name) {
                return Err(err);
            }
            // Compile method bodies to bytecode for the fast path
            self.compile_class_methods(&storage_name);
            // Register CUnion / CStruct repr if present
            if let Some(repr_name) = repr {
                if repr_name == "CUnion" {
                    self.register_cunion_class(&storage_name);
                } else if repr_name == "CStruct" {
                    self.register_cstruct_class(&storage_name);
                } else if repr_name == "CPointer" {
                    self.register_cpointer_class(&storage_name);
                }
            }
            // Register the class name in the lexical env so that
            // ::("ClassName") indirect lookups can find it in the current scope.
            // The bare name resolves to the (possibly mangled) storage name so
            // that `Foo.new` inside this scope produces instances tagged with
            // this declaration's identity, not an earlier same-named class's.
            let env = self.env_mut();
            env.insert(
                "_".to_string(),
                Value::package(Symbol::intern(&storage_name)),
            );
            // Always insert the class type object so that class names take
            // precedence over same-named `$`-sigiled variables (whose stripped
            // name may already be in the env).
            env.insert(
                qualified_name.clone(),
                Value::package(Symbol::intern(&storage_name)),
            );
            // A *nested* declared name stays reachable under the name as written,
            // too. Rakudo installs `class X::Imported::Boom` inside
            // `unit module M` into the already-existing outer `X::` package while
            // recording `M::X::Imported::Boom` as its `.^name`, so a consumer of
            // `M` refers to it as plain `X::Imported::Boom`
            // (`t/imported-exception-when.t`, the shape Zef uses). Register the
            // written name as an alias for the qualified declaration rather than
            // modelling that installation rule, and never over an existing entry.
            if resolved_name != qualified_name && resolved_name.contains("::") {
                let storage = storage_name.clone();
                env.entry_or_insert_with(resolved_name.clone(), || {
                    Value::package(Symbol::intern(&storage))
                });
            }
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
                // ... and remember the short name permanently, so a later
                // same-named type in an unrelated module (which clears the
                // suppression) cannot steal bareword resolution from this
                // class's own methods.
                self.register_class_scoped_short_name(&resolved_name);
                // Register the short name in the lexical env so it resolves
                // within the enclosing class scope (e.g. `Frog` inside `Forest`).
                let env = self.env_mut();
                env.insert(
                    resolved_name.clone(),
                    Value::package(Symbol::intern(&storage_name)),
                );
            }
            // A lexical type declared in a ROLE body (`role R { my class Foo {} }`)
            // is private to the role, and the role's own methods must keep seeing
            // it after they are composed into an arbitrary class — even when an
            // unrelated same-named type exists at file scope or in another
            // package. Recording the short name enables the owner-package probe
            // in `resolve_suppressed_type`, which walks the composed roles of the
            // running method's class. Unlike the class case the name is NOT
            // suppressed: a role body is not a package boundary that hides an
            // outer same-named type from the rest of the file.
            let parent_is_role = qualified_name
                .rsplit_once("::")
                .map(|(parent, _)| self.is_role(parent))
                .unwrap_or(false);
            if qualified_name != resolved_name && !resolved_name.contains("::") && parent_is_role {
                self.register_class_scoped_short_name(&resolved_name);
            }
            // When a class is declared with an already-qualified name
            // (e.g. the compiler pre-qualified `class C1` inside
            // `unit module M` to `M::C1`), also register the short name
            // `C1` in the env so that subsequent code inside the same
            // module can refer to it bare. Skip this when the parent
            // package is a class (where suppress_name semantics apply).
            // TODO: this alias is global, but it belongs to the *declaring*
            // package's scope. A file-scope `class Cro::Hdr { }` makes bare `Hdr`
            // resolve to it, where raku reports `Hdr` as an undeclared name, and
            // that shadows a later same-short-name declaration in an inner scope
            // (see todo/tickets/package-short-name-alias-is-global.md). Gating it
            // on `current_package` being the declaring package is NOT enough:
            // `class URI::Path` is declared at file scope in its own module and
            // `unit class URI`'s methods legitimately name it bare.
            if qualified_name.contains("::") && !parent_is_class {
                let short = qualified_name
                    .rsplit_once("::")
                    .map(|(_, s)| s.to_string())
                    .unwrap_or_else(|| qualified_name.clone());
                // Do not shadow built-in types (e.g. `my class X::Roast::Channel`
                // must not make the bare name `Channel` resolve to the user class).
                if !short.is_empty() && short != qualified_name && !Self::is_builtin_type(&short) {
                    self.env_mut().entry_or_insert_with(short, || {
                        Value::package(Symbol::intern(&storage_name))
                    });
                }
            }
            // When `my class` is used, register the class name as lexically scoped
            // so it gets suppressed when the enclosing block scope exits.
            if *is_lexical {
                self.register_lexical_class(resolved_name.clone());
                // Also mark as my-scoped so it's excluded from the parent package stash
                self.mark_my_scoped_package_item(storage_name.clone());
            }
            // Store language revision metadata from the version captured at parse time
            self.store_language_revision_from_version(&storage_name, language_version);

            // A class declared with an EXPORTHOW::DECLARE declarator (the
            // `__mutsu_declare_how` marker trait carries the keyword): attach
            // an instance of the declarator's HOW type — installed by the
            // `use`d module as the EXPORTHOW::DECLARE::<keyword> constant — as
            // the class's meta-object, drive the HOW registration protocol
            // (`new_type`, `add_method` per declared method), and queue the
            // user `compose` to run after the custom `is` traits (same
            // protocol as the EXPORTHOW `class` metaclass mapping below).
            if let Some((_, Some(Expr::Literal(kw)))) = custom_traits
                .iter()
                .find(|(t, _)| t == "__mutsu_declare_how")
            {
                let keyword = kw.to_string_value();
                let how_type =
                    self.get_env_with_main_alias(&format!("EXPORTHOW::DECLARE::{}", keyword));
                if let Some(how_type) = how_type {
                    let has_user_compose =
                        self.install_custom_class_how(&storage_name, how_type)?;
                    self.declare_drive_how_protocol(&storage_name)?;
                    if has_user_compose {
                        self.registry_mut()
                            .pending_class_compose
                            .push(storage_name.clone());
                    }
                }
            }

            // Dispatch custom `is` traits via trait_mod:<is> if defined.
            // Merge explicitly parsed custom_traits with deferred_traits
            // (unknown lowercase parents deferred from register_class_decl).
            let has_trait_mod =
                self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
            if has_trait_mod && (!custom_traits.is_empty() || !deferred_traits.is_empty()) {
                let type_obj = Value::package(Symbol::intern(&storage_name));
                // Dispatch explicitly parsed custom traits (with args)
                for (trait_name, trait_arg) in custom_traits {
                    // Internal markers (`__mutsu_declare_how`, `__hoisted`, ...)
                    // are not user traits; never dispatch them to trait_mod:<is>.
                    if trait_name.starts_with("__") {
                        continue;
                    }
                    let trait_value = if let Some(arg_expr) = trait_arg {
                        self.vm_eval_block_value(&[Stmt::Expr(arg_expr.clone())])?
                    } else {
                        Value::TRUE
                    };
                    let named_arg = Value::pair(trait_name.clone(), trait_value);
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no args)
                for trait_name in &deferred_traits {
                    let named_arg = Value::pair(trait_name.clone(), Value::TRUE);
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
            }
            // Raku desugars `is Parent` to `trait_mod:<is>($type, Parent)`; when a
            // KNOWN-class parent matches a *typed* user candidate
            // (`multi trait_mod:<is>(Mu:U, SomeType:U)`, more specific than the
            // default add-parent), also dispatch it so the trait runs — e.g. the
            // AOP module's `add_aspect` on `class Example is LoggingAspect` where
            // `LoggingAspect` does `MethodBoundaryAspect` (`advent2011-day14`).
            // Plain inheritance already happened in `register_class_decl`; this
            // only ADDS the trait's side effect. Runs OUTSIDE the block above (a
            // class with only known-class parents has empty custom/deferred trait
            // lists). Gated on a matching typed candidate, so a parent with no
            // such candidate (ordinary inheritance) is untouched.
            if has_trait_mod {
                let type_obj = Value::package(Symbol::intern(&storage_name));
                for parent in &mapped_parents {
                    let parent_obj = Value::package(Symbol::intern(parent));
                    let call_args = vec![type_obj.clone(), parent_obj];
                    if self.typed_is_trait_candidate_matches(&call_args) {
                        self.vm_call_function("trait_mod:<is>", call_args)?;
                    }
                }
            }

            // A class installed under a custom EXPORTHOW `class` metaclass whose
            // HOW defines `compose`: run it NOW — after the custom `is` traits
            // above (which populate the HOW's state, e.g. `@!aspects`) — so the
            // user `compose` sees that state and wraps the class's methods
            // (`advent2011-day14` AOP). `compose` receives the class type object.
            let pending: Vec<String> =
                std::mem::take(&mut self.registry_mut().pending_class_compose);
            for cname in pending {
                let how_val = self.registry().class_how_values.get(&cname).cloned();
                if let Some(how_val) = how_val {
                    let type_obj = Value::package(Symbol::intern(&cname));
                    self.call_method_with_values(how_val, "compose", vec![type_obj])?;
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

    /// Whether a *typed* user `trait_mod:<is>` candidate matches `call_args`
    /// (`[class_type, parent_type]`): a candidate whose SECOND positional
    /// parameter carries a non-universal type constraint that the parent
    /// satisfies. Used to route `is KnownParent` through the trait (Raku's
    /// `is X` desugaring) only when a user overrode it for that parent type —
    /// an unconstrained `(Mu, Mu)`-style candidate (which would match every
    /// parent) is deliberately NOT counted, so ordinary inheritance is
    /// untouched. See `advent2011-day14` AOP.
    fn typed_is_trait_candidate_matches(&mut self, call_args: &[Value]) -> bool {
        let matches = self.resolve_all_matching_candidates("trait_mod:<is>", call_args);
        matches.iter().any(|def| {
            def.param_defs
                .iter()
                .filter(|p| !p.named && !p.is_invocant)
                .nth(1)
                .and_then(|p| p.type_constraint.as_deref())
                .is_some_and(|tc| {
                    let base = tc.trim_end_matches(":U").trim_end_matches(":D");
                    !matches!(base, "Mu" | "Any" | "Cool" | "")
                })
        })
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
            does_roles,
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
            let does_role_names: Vec<String> = does_roles.iter().map(|s| s.resolve()).collect();
            loan_env!(self, augment_class(&name_str, body, &does_role_names))?;
            // Recompile augmented class methods for the fast path
            self.compile_class_methods(&name_str);
            // Augment can add methods/attributes — drop cached construction plans.
            self.native_ctor_plan_cache.clear();
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
                register_role_decl(
                    &qualified_name,
                    type_params,
                    type_param_defs,
                    body,
                    *is_rw,
                    language_version,
                )
            )?;
            // Link `is Parent` references on this role to the lexical class visible
            // in this scope (which may be stored under a mangled name), matching
            // the class-parent remapping in `exec_register_class_op`.
            self.remap_role_parents_via_env(&qualified_name);
            // A role declared in a CLASS body (`unit class UA; role Connection
            // { … }`) is scoped to that class, like a nested `my class`. Record
            // the short name so `resolve_suppressed_type` resolves it through the
            // owner package chain from the class's own methods — the bare env
            // alias does not outlive the class body.
            if !name_str.contains("::") && self.has_class(&current_package) {
                self.register_class_scoped_short_name(&name_str);
            }
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
                Value::package(Symbol::intern(&qualified_name)),
            );
            self.env_mut().insert(
                qualified_name.clone(),
                Value::package(Symbol::intern(&qualified_name)),
            );
            if qualified_name != name_str && !name_str.contains("::") {
                self.env_mut().insert(
                    name_str.clone(),
                    Value::package(Symbol::intern(&qualified_name)),
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
                // Do not shadow built-in types (e.g. `role Cro::HTTP::Middleware::Pair`
                // must not make the bare name `Pair` resolve to the user role, which
                // would break every `when Pair` in the process). Mirrors the same
                // guard on the class path above.
                if !short.is_empty() && short != qualified_name && !Self::is_builtin_type(&short) {
                    self.env_mut().entry_or_insert_with(short, || {
                        Value::package(Symbol::intern(&qualified_name))
                    });
                }
            }
            // A role's non-declaration body statements are NOT run here. Rakudo
            // runs a role body once per *composition* (`class C does R`, a pun,
            // or a `but`/`does` mixin), never at the declaration itself:
            //
            //     role R { say "BODY" }               # prints nothing
            //     role R { say "BODY" }; class C does R { }   # prints BODY once
            //
            // The composition-time run lives in `registration_class_decl.rs`.

            // Gather deferred custom traits from role registration
            let role_deferred = self
                .get_role_def(&qualified_name)
                .map(|r| r.deferred_custom_traits.clone())
                .unwrap_or_default();

            // Dispatch custom `is` traits via trait_mod:<is> if defined
            let has_trait_mod =
                self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
            if has_trait_mod && (!custom_traits.is_empty() || !role_deferred.is_empty()) {
                let type_obj = Value::package(Symbol::intern(&qualified_name));
                for (trait_name, trait_arg) in custom_traits {
                    // Skip internal markers (e.g. `__my_scoped`); they are not real `is` traits.
                    if trait_name.starts_with("__") {
                        continue;
                    }
                    let trait_value = if let Some(arg_expr) = trait_arg {
                        self.vm_eval_block_value(&[Stmt::Expr(arg_expr.clone())])?
                    } else {
                        Value::TRUE
                    };
                    let named_arg = Value::pair(trait_name.clone(), trait_value);
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no args)
                for trait_name in &role_deferred {
                    let named_arg = Value::pair(trait_name.clone(), Value::TRUE);
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
            is_my,
        } = stmt
        {
            let resolved_name = name.resolve();
            let subset_package = self.current_package().to_string();
            loan_env!(
                self,
                register_subset_decl(&resolved_name, base, predicate.as_ref(), version, *is_my)
            );
            // A subset declared in a CLASS body (`class Req { subset Method of
            // Str … }`) is scoped to that class, exactly like a nested `my class`.
            // Record the short name so `resolve_suppressed_type` resolves it
            // through the owner package chain from the class's own methods and
            // during construction — the bare env alias `register_subset_decl`
            // leaves behind does not outlive the class body.
            if !resolved_name.contains("::") && self.has_class(&subset_package) {
                self.register_class_scoped_short_name(&resolved_name);
            }
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
