//! Type-declaration registration ops: enum / class / augment / role / subset.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn exec_register_decl_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        match code.decl_plans.get(idx as usize).copied() {
            Some(crate::opcode::CompiledDeclPlanRef::Sub(plan_idx)) => {
                self.exec_register_sub_op(code, plan_idx, compiled_fns)
            }
            Some(crate::opcode::CompiledDeclPlanRef::Class(plan_idx)) => {
                self.note_type_body_written_lexicals(code);
                match self.exec_register_class_op(code, plan_idx, compiled_fns) {
                    Ok(()) => Ok(()),
                    Err(_)
                        if code.class_decl_plans[plan_idx as usize]
                            .custom_traits
                            .iter()
                            .any(|(name, _)| name == "__hoisted") =>
                    {
                        Ok(())
                    }
                    Err(error) => Err(error),
                }
            }
            Some(crate::opcode::CompiledDeclPlanRef::Role(plan_idx)) => {
                self.note_type_body_written_lexicals(code);
                match self.exec_register_role_op(code, plan_idx, compiled_fns) {
                    Ok(()) => Ok(()),
                    Err(_)
                        if code.role_decl_plans[plan_idx as usize]
                            .custom_traits
                            .iter()
                            .any(|(name, _)| name == "__hoisted") =>
                    {
                        Ok(())
                    }
                    Err(error) => Err(error),
                }
            }
            Some(crate::opcode::CompiledDeclPlanRef::Proto(plan_idx)) => {
                self.exec_register_proto_sub_op(code, plan_idx, compiled_fns)
            }
            Some(crate::opcode::CompiledDeclPlanRef::ProtoToken(name)) => {
                self.register_proto_token_decl(&name.resolve());
                Ok(())
            }
            Some(crate::opcode::CompiledDeclPlanRef::Token(plan_idx)) => {
                self.exec_register_token_decl_op(code, plan_idx)
            }
            None => Err(RuntimeError::new("RegisterDecl plan index out of bounds")),
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
            is_my,
            base_type,
            language_version,
        } = stmt
        {
            let result = loan_env!(
                self,
                register_enum_decl(&name.resolve(), variants, *is_export, base_type.as_deref(),)
            )?;
            // A `my enum` is lexical: its type name and every variant name die
            // with the enclosing block, so record them the way `DeclareVar`
            // records a `my $x`. Block exit otherwise propagated the variant
            // bindings outwards and a same-named outer symbol stayed clobbered
            // for the rest of the program (`{ my enum E <Zed> }; Zed` answered
            // the enum value rather than the file-scope `class Zed`).
            if *is_my && let Some(set) = self.block_declared_vars.last_mut() {
                set.insert(*name);
                for (variant, _) in variants {
                    set.insert(crate::symbol::Symbol::intern(variant));
                }
            }
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
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        // Registering a class can shadow a same-named earlier class (`my class A`
        // in a fresh lexical scope) with different method bodies/candidates, so a
        // cached resolution from the old class must not be reused for the new one.
        // (The multi-resolution cache made this observable: S12-methods/multi.t
        // reuses `my class A`/`B` with multi submethods.) This USED to be enforced
        // by a preemptive `invalidate_method_dispatch_caches()` call right here,
        // before `register_class_decl` ran. ADR-0019 Phase F box F5 cutover: that
        // eager clear is redundant and removed. Every live path through
        // `register_class_decl` that actually changes the class calls
        // `sync_user_method_entries` unconditionally, which bumps
        // `Registry::method_generation`; every cache the eager clear used to clear
        // (`method_resolve_cache`/`fast_method_cache`/`native_ctor_plan_cache`/
        // `multi_resolve_cache`/`multi_type_cacheable`/`resolved_seq_cache`/
        // `dispatch_multi_candidate`/the private-zeroarg cache) already
        // self-refreshes at its own read site keyed on that same generation
        // (`refresh_method_caches_for_generation`), so by the time any of them is
        // read again the new class's generation has already superseded the old
        // one. The one path that does NOT bump the generation is a true no-op (a
        // stub re-declaring an already-non-stub class of the same name, which
        // leaves the class completely unchanged) and needs no invalidation either.
        // Verified via the `MUTSU_VM_STATS`-gated shadow check below across the
        // full `t/` suite (1296 checks, 1 mismatch) and a class/role/multi-heavy
        // roast whitelist subset (5 mismatches): every mismatch was exactly that
        // no-op shape. See the box's progress notes in
        // `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`.
        let f5_gen_before = self.registry().method_generation;
        if let Some(crate::opcode::CompiledClassDeclPlan {
            name,
            name_chunk,
            parents,
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            repr,
            language_version,
            custom_traits,
            decl_id,
            is_stub,
            trusts,
            own_attribute_names,
            attr_decls,
            method_name_chunks,
            method_decls,
            declared_static_names,
            parent_arg_chunks,
            body_plan,
        }) = code.class_decl_plans.get(idx as usize)
        {
            let resolved_name = if let Some(chunk) = name_chunk {
                self.run_decl_expr(chunk)?.to_string_value()
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
            // name* distinct from its source `qualified_name` (ADR-0047 D1/P1):
            // every `my`-scoped declaration site with a nonzero `decl_id` mangles
            // unconditionally to `Foo\u{0}<site-id>`. The site id is the
            // parse-time-assigned `decl_id`, stable across re-executions of the
            // same site (a loop body keeps one identity), but distinct between
            // sites. `decl_id == 0` (deserialized/synthesized node) opts out
            // and uses the bare qualified name.
            //
            // Two distinct declaration sites therefore NEVER share a registry
            // key, by construction, so a later sibling `my class Foo` cannot
            // retarget an earlier one's already-existing instances (ADR-0047 S2)
            // and scope exit never has to arbitrate a claim/release dance over a
            // bare name (that used to live in `lexical_class_sites` /
            // `lexical_class_owner_scopes`, deleted with this change). The env
            // binding for the bare name (`Foo` -> this storage name) is instead
            // scoped and restored by the ordinary block-exit machinery via
            // `register_lexical_class`/`block_declared_vars` (ADR-0047 D2/P2).
            //
            // EXCEPTION: a stub (`my class C { ... }`) and its own later full
            // definition (`my class C { method m {} }`) are two SEPARATE
            // `Stmt::ClassDecl` nodes with two different `decl_id`s, even
            // though they are one logical class. If the current scope has an
            // still-incomplete stub pending under this qualified name, this
            // declaration completes THAT stub, so it reuses its exact storage
            // name instead of mangling a new one (see
            // `lexical_class_pending_stub`'s doc comment for why this is
            // scoped to only currently-open scopes).
            //
            // The pending-stub lookup runs REGARDLESS of whether THIS
            // declaration itself is lexical: real Raku allows completing a
            // `my class A::B { ... }` stub with a plain, package-scoped
            // `class A::B { }` in the same scope (`roast/S10-packages/
            // joined-namespaces.t`, "can stub lexical classes with joined
            // namespaces") — gating the lookup on `*is_lexical` here meant the
            // non-`my` completion always mangled to bare `qualified_name`
            // (since a plain `class` never mangles), leaving the stub's
            // `Foo\u{0}<decl-id>` entry permanently registered as a stub —
            // "packages were stubbed but not defined" at program exit.
            let storage_name = match self.lexical_class_pending_stub(&qualified_name) {
                Some(pending_storage) => pending_storage,
                None if *is_lexical && *decl_id != 0 => {
                    let mangled = format!("{qualified_name}\u{0}{decl_id}");
                    self.record_lexical_class_pending(qualified_name.clone(), mangled.clone());
                    mangled
                }
                None => qualified_name.clone(),
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
            // Look up each parent's precompiled bracket-argument chunks
            // (ADR-0019 D4-3) by its ORIGINAL, pre-remap plan string — the
            // same key `parent_arg_chunks` was built from — before any of
            // the lexical/sibling remapping below can change it. Zipped
            // alongside the remap chain (rather than looked up again after)
            // so the same filter that can drop the auto-added `Grammar`
            // self-parent keeps both lists index-aligned.
            let (mapped_parents, parent_pre_args): (
                Vec<String>,
                Vec<Option<&[crate::opcode::DeclTraitArg]>>,
            ) = parents
                .iter()
                .map(|p| {
                    let pre_args = parent_arg_chunks
                        .iter()
                        .find(|(key, _)| key == p)
                        .map(|(_, args)| args.as_slice());
                    (self.lexical_env_remap_name(p), pre_args)
                })
                // Qualify a bare parent that names a sibling class in the current
                // package but collides with a built-in namespace (`class X::Decode
                // is X` inside `module M`, where `X` is both `M::X` and the built-in
                // `X::` exception namespace). Must run here, where `current_package`
                // is the enclosing module — the child class name reaches
                // `register_class_decl` without its module prefix.
                .map(|(p, pre_args)| (self.qualify_sibling_parent_name(&p), pre_args))
                // Drop the auto-added `Grammar` default parent from a genuine
                // top-level `grammar Grammar` (qualified name exactly `Grammar`,
                // which would otherwise list itself as its own parent and loop the
                // MRO walk). A module-local `grammar Grammar` qualifies to
                // `Mod::Grammar`, so its `Grammar` parent (the built-in) is NOT
                // itself and is kept — that is how the parser can unconditionally
                // add the `Grammar` default parent. An EXPLICIT `class Foo is Foo`
                // is left intact so it still raises the self-inheritance error.
                .filter(|(p, _)| !(p == "Grammar" && qualified_name == "Grammar"))
                .unzip();
            let mapped_hidden_parents: Vec<String> = hidden_parents
                .iter()
                .map(|p| self.lexical_env_remap_name(p))
                .collect();
            // Register CUnion / CStruct / CPointer repr *before* running the
            // class body: a `unit class Foo is repr('CStruct'); has ...; say
            // Foo.REPR;` folds every trailing statement (including that
            // `say`) into the class body (`parser::stmt::stmtlist`'s
            // mainline-capture), which `register_class_decl` below executes
            // as part of registration itself — so a self-referential
            // `Foo.REPR` read during the body observes whatever was
            // registered so far. Doing this only *after* `register_class_decl`
            // returns (as it used to) left it permanently P6opaque for the
            // `unit class` form, since by the time control returned here the
            // body (and its `.REPR` read) had already run. The block form
            // (`class Foo is repr(...) { ... }; say Foo.REPR;`) is unaffected
            // either way, since its `say` is a separate mainline statement
            // that only runs after this whole op completes.
            if let Some(repr_name) = repr {
                if repr_name == "CUnion" {
                    self.register_cunion_class(&storage_name);
                } else if repr_name == "CStruct" {
                    self.register_cstruct_class(&storage_name);
                } else if repr_name == "CPointer" {
                    self.register_cpointer_class(&storage_name);
                }
            }
            // TODO: Detect redeclaration of package-scoped classes across
            // EVAL boundaries (X::Redeclaration). Currently deferred because
            // distinguishing EVAL re-definitions from normal re-execution
            // (e.g., anonymous classes in loops, augment) requires tracking
            // compilation unit boundaries.
            let is_hoisted_shell = custom_traits
                .iter()
                .any(|(trait_name, _)| trait_name == "__hoisted");
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
                        is_stub: *is_stub,
                        trusts,
                        own_attribute_names,
                        attr_decls,
                        method_name_chunks,
                        method_decls,
                        declared_static_names,
                        parent_pre_args: &parent_pre_args,
                        compiled_fns,
                        body_plan,
                        is_hoisted_shell,
                    },
                )
            )?;
            // ADR-0019 Phase F box F5 shadow check: confirm this successful
            // registration bumped `Registry::method_generation` (see
            // `record_class_reg_gen_shadow_check`'s doc comment). Shadow-only
            // -- does not affect the eager `invalidate_method_dispatch_caches()`
            // call above.
            {
                let f5_gen_after = self.registry().method_generation;
                crate::vm::vm_stats::record_class_reg_gen_shadow_check(
                    f5_gen_after != f5_gen_before,
                    || format!("class={qualified_name} is_stub={is_stub}"),
                );
            }
            // Check for assignment to native read-only params before
            // compiling (X::Assignment::RO::Comp).
            if let Some(err) = self.check_class_native_readonly_param_errors(&storage_name) {
                return Err(err);
            }
            // Compile method bodies to bytecode for the fast path.
            //
            // A `__hoisted` forward-reference shell (`hoist_type_decl_shells`)
            // is skipped: its `CompiledMethodDecl`s all carry
            // `compiled_routine_key: None` (`add_class_decl_plan` computes
            // `package_name: None` for a shell), so this pass would compile
            // every method body from scratch through
            // `compile_method_def_in_place_with_dist` — and the whole
            // `MethodDef` set it fills in is discarded wholesale moments later,
            // when the real, source-position declaration re-registers the class
            // from its own (properly keyed) plan. The compiled code was never
            // read in between, making the compile 100% wasted work. If a
            // forward reference does call a method on the shell-registered type
            // before the real declaration runs, `populate_uncompiled_method`
            // compiles that one body on demand.
            if !is_hoisted_shell {
                self.compile_class_methods(&storage_name);
            }
            // Register the class name in the lexical env so that
            // ::("ClassName") indirect lookups can find it in the current scope.
            // The bare name resolves to the (possibly mangled) storage name so
            // that `Foo.new` inside this scope produces instances tagged with
            // this declaration's identity, not an earlier same-named class's.
            let env = self.env_mut();
            // NB: registering a type must NOT touch `$_`. A `class`/`role`
            // declaration is not an expression whose value becomes the topic,
            // and writing the type object there clobbered the enclosing
            // topic — `for ^3 { class C { }; say $_ }` printed the type object
            // three times instead of 0, 1, 2.
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
            // `C1` so that subsequent code declared in (or dispatched
            // through) the same package can refer to it bare. Skip this
            // when the parent package is a class (where suppress_name
            // semantics apply).
            // Package-scoped, not global: a file-scope `class Cro::Hdr { }`
            // makes bare `Hdr` resolve only from within package `Cro`
            // (`current_package`/`method_class_stack` walking the package
            // chain via `package_type_alias`), matching raku's "Undeclared
            // name" outside it and never shadowing an unrelated same-short-name
            // declaration in another scope (see
            // todo/tickets/package-short-name-alias-is-global.md).
            // `class URI::Path` declared at file scope still resolves bare
            // `Path` inside `unit class URI`'s own methods and attribute
            // defaults: `push_method_class`/`eval_attr_default_expr` already
            // anchor the lookup to the owning class unconditionally.
            // `my class` keeps the OLD env-based alias instead: it is
            // lexically scoped to its own declaring block, and the env write
            // is what `register_lexical_class`'s scope-exit restoration
            // (below) resets between re-executions of the enclosing block —
            // `package_type_aliases` has no such per-scope lifetime, so two
            // sibling subs each declaring `my class Shape` in the same module
            // would have the second call's alias silently lost to the first
            // (`entry().or_insert_with()` never overwrites), leaving the
            // second sub's `Shape.new` resolving to the first sub's class
            // (`t/module-sub-otf-interpreter-constructs.t` "same-named nested
            // class (b)").
            //
            // This alone only covers code running *inside* the declaring
            // package's own ancestor chain (including its own methods, since
            // method dispatch anchors `current_package` to the class's own
            // qualified name). A *different* package that `use`s this one and
            // references the bare name from a sibling package (a common
            // NativeCall idiom — `unit module Foo::Native; class Handle
            // is repr('CPointer') {}`, then `unit class Foo::Driver; use
            // Foo::Native; method f() { Handle.new }`) is handled separately,
            // by `load_module_inner`/`import_module` copying this same alias
            // into the *importer's* own package_type_aliases entry at `use`
            // time (see `package_type_aliases` doc comment).
            if qualified_name.contains("::") && !parent_is_class {
                let (parent, short) = qualified_name
                    .rsplit_once("::")
                    .map(|(p, s)| (p.to_string(), s.to_string()))
                    .unwrap_or_else(|| (String::new(), qualified_name.clone()));
                // Do not shadow built-in types (e.g. `my class X::Roast::Channel`
                // must not make the bare name `Channel` resolve to the user class).
                if !short.is_empty() && short != qualified_name && !Self::is_builtin_type(&short) {
                    if *is_lexical {
                        self.env_mut().entry_or_insert_with(short, || {
                            Value::package(Symbol::intern(&storage_name))
                        });
                    } else {
                        self.package_type_aliases
                            .entry(parent)
                            .or_default()
                            .entry(short)
                            .or_insert_with(|| storage_name.clone());
                    }
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
            if let Some(kw) = custom_traits
                .iter()
                .find(|(t, _)| t == "__mutsu_declare_how")
                .and_then(|(_, arg)| arg.as_ref())
                .and_then(crate::opcode::DeclTraitArg::literal)
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
                    let trait_value = match trait_arg {
                        Some(arg) => self.eval_decl_trait_arg(arg)?,
                        None => Value::TRUE,
                    };
                    let named_arg = Value::pair(trait_name.clone(), trait_value);
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no
                // args). `validate_class_parents` optimistically deferred
                // these lowercase names on seeing *any* `trait_mod:<is>`
                // proto/multi at all -- if none of its candidates actually
                // match this shape, that guess was wrong and it really was
                // an unknown parent all along (mirrors the sibling
                // variable-/attribute-trait no-candidate fallback).
                for trait_name in &deferred_traits {
                    let named_arg = Value::pair(trait_name.clone(), Value::TRUE);
                    match self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])
                    {
                        Ok(_) => {}
                        Err(err) if Self::is_trait_mod_no_candidate(&err) => {
                            return Err(self.unknown_parent_error(&storage_name, trait_name));
                        }
                        Err(err) => return Err(err),
                    }
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
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        if let Some(crate::opcode::CompiledRoleDeclPlan {
            name,
            type_params,
            type_param_defs,
            is_export,
            export_tags,
            is_rw,
            language_version,
            custom_traits,
            own_attribute_names,
            body_used_modules,
            body_declared_types,
            attr_decls,
            method_name_chunks,
            method_decls,
            is_stub,
            our_scope_violation,
            parent_ops,
            body_plan,
            deferred_body_ops,
        }) = code.role_decl_plans.get(idx as usize)
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
                    body_plan,
                    *is_rw,
                    language_version,
                    own_attribute_names,
                    body_used_modules,
                    body_declared_types,
                    attr_decls,
                    method_name_chunks,
                    method_decls,
                    *is_stub,
                    *our_scope_violation,
                    parent_ops,
                    deferred_body_ops,
                    compiled_fns,
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
            // Compile role method bodies to bytecode. A `__hoisted`
            // forward-reference shell is skipped for the same reason the class
            // side skips it (see `exec_register_class_op`): `add_role_decl_plan`
            // leaves every `compiled_routine_key` `None` for a shell, so this
            // pass would compile every body from scratch only for the real,
            // source-position declaration to replace the whole `MethodDef` set
            // moments later.
            if !custom_traits
                .iter()
                .any(|(trait_name, _)| trait_name == "__hoisted")
            {
                self.compile_role_methods(&qualified_name);
            }
            // See `exec_register_class_op`: a declaration does not set the topic.
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
            // short name `R1`, package-scoped to the declaring package
            // rather than global (mirrors the class path above — see
            // todo/tickets/package-short-name-alias-is-global.md).
            if qualified_name.contains("::") && qualified_name == name_str {
                let (parent, short) = qualified_name
                    .rsplit_once("::")
                    .map(|(p, s)| (p.to_string(), s.to_string()))
                    .unwrap_or_else(|| (String::new(), qualified_name.clone()));
                // Do not shadow built-in types (e.g. `role Cro::HTTP::Middleware::Pair`
                // must not make the bare name `Pair` resolve to the user role, which
                // would break every `when Pair` in the process). Mirrors the same
                // guard on the class path above.
                if !short.is_empty() && short != qualified_name && !Self::is_builtin_type(&short) {
                    self.package_type_aliases
                        .entry(parent)
                        .or_default()
                        .entry(short)
                        .or_insert_with(|| qualified_name.clone());
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
                    let trait_value = match trait_arg {
                        Some(arg) => self.eval_decl_trait_arg(arg)?,
                        None => Value::TRUE,
                    };
                    let named_arg = Value::pair(trait_name.clone(), trait_value);
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no
                // args); fall back to the unknown-parent diagnosis if no
                // candidate actually matches this shape (see the matching
                // comment on the class-registration site above).
                for trait_name in &role_deferred {
                    let named_arg = Value::pair(trait_name.clone(), Value::TRUE);
                    match self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])
                    {
                        Ok(_) => {}
                        Err(err) if Self::is_trait_mod_no_candidate(&err) => {
                            return Err(self.unknown_parent_error(&qualified_name, trait_name));
                        }
                        Err(err) => return Err(err),
                    }
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
