//! `subset` registration: the registry entry, the package-qualified alias
//! of an `our` subset, and the declaration-site storage name of a `my` one.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_subset_decl(
        &mut self,
        name: &str,
        base: &str,
        predicate: Option<&Expr>,
        predicate_closure: Option<Value>,
        refinement: Option<Value>,
        version: &str,
        is_my: bool,
        decl_id: u64,
    ) {
        // When the predicate is `* ~~ <expr>` (Whatever on LHS of SmartMatch),
        // the parser doesn't wrap it as WhateverCode (to avoid breaking other
        // smartmatch semantics). Convert it here to a Lambda so the subset
        // check correctly evaluates `$_ ~~ <expr>` against the candidate value.
        let predicate = predicate.map(|pred| {
            if let Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::SmartMatch,
                right, ..
            } = pred
                && matches!(left.as_ref(), Expr::Whatever)
            {
                return Expr::Lambda {
                    param: "_".to_string(),
                    body: vec![Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Var("_".to_string())),
                        op: crate::token_kind::TokenKind::SmartMatch,
                        right: right.clone(), form: Default::default(),
                    })],
                    is_whatever_code: true,
                    param_sigilless: false,
                };
            }
            // A curried predicate (`where * < 100`) is stored as the closure
            // the compiler would build for it, so a one-argument WhateverCode
            // reaches the type check's inline path — its body runs with `$_`
            // bound — instead of being built and called on every check
            // (#10107). The `Lambda` compiles to the same closure wherever the
            // predicate is used as a value.
            if let Expr::WhateverCurry(curried, ..) = pred
                && let lambda @ Expr::Lambda {
                    is_whatever_code: true,
                    ..
                } = crate::whatever_curry::build_closure(curried)
            {
                return lambda;
            }
            pred.clone()
        });
        // Whether the spelling is taken by a package (`our`) subset, which a
        // `my subset` of the same name must not overwrite (#18217).
        let shadows_package_subset = is_my
            && decl_id != 0
            && self
                .registry()
                .subsets
                .get(name)
                .is_some_and(|existing| !existing.lexical);
        // Drop any cached compiled predicate for this name so a redeclaration
        // recompiles against the new predicate (see `subset_predicate_cache`).
        self.types.subset_predicate_cache.remove(name);
        // A subset defaults to `our` scope: declared inside a package/class/
        // module it is also reachable by its qualified name (`URI::Scheme`),
        // so register that alias too — smartmatch resolves the constraint by
        // the exact name it was referenced with. A `my subset` is lexical and
        // must NOT get the package-qualified alias (S12-subset/type-subset.t).
        let pkg = self.current_package();
        // `my subset SI2 of S-Int` refines the `S-Int` visible HERE: a lexical
        // base lives under its declaration-site storage name (ADR-0047), so
        // record that identity rather than the spelling.
        let inline_predicate = predicate
            .as_ref()
            .and_then(crate::runtime::types::subset_inline_predicate);
        let predicate_inline = inline_predicate.is_some();
        let predicate_int_compare = inline_predicate.and_then(|(body, bind)| {
            crate::predicate_shape::TopicIntCompare::of_body(body, bind)
        });
        // Shared: a type check reads the definition on every call, and a
        // deep clone of it (predicate AST included) was part of that cost.
        let def = std::sync::Arc::new(SubsetDef {
            base: self.lexical_env_remap_name(base),
            predicate,
            version: version.to_string(),
            decl_package_sym: crate::symbol::Symbol::intern(&pkg),
            decl_unit: self.current_unit,
            predicate_inline,
            predicate_int_compare,
            predicate_closure,
            refinement,
            lexical: is_my && decl_id != 0,
        });
        // The qualified name is the subset's *identity* (raku reports `Foo::RM`
        // from `.^name` and in every type-check message), so the short name is
        // registered as an alias pointing at it — the same shape `class`/`role`
        // registration uses. The short key stays in `subsets` because most
        // constraint lookups are by the exact name written at the use site.
        let mut canonical = name.to_string();
        // A compound name written inside a package is still relative to that
        // package.  `subset Table::Position` inside `module M` is therefore
        // `M::Table::Position`, not a top-level `Table::Position`.  The old
        // bare-name-only qualification happened to handle `subset Small` but
        // left compound names detached from their declaring package.  That
        // made the package-qualified type object differ from the one stored
        // in a typed signature (and, in turn, caused valid subset parameters
        // to be rejected during dispatch).
        let already_qualified =
            name == pkg || name.starts_with(&format!("{}::", pkg)) || name.starts_with("GLOBAL::");
        if !is_my
            && !already_qualified
            && !crate::qualified::is_global_package(crate::qualified::known_symbol(&pkg))
            && pkg != "Main"
        {
            let qualified = crate::qualified::qualified_text(&pkg, name)
                .as_str()
                .to_string();
            self.types.subset_predicate_cache.remove(&qualified);
            self.registry_mut()
                .subsets
                .insert(qualified.clone(), def.clone());
            if !self.qualified_identity_binding_is_redundant(&qualified, &qualified) {
                self.env.insert(
                    qualified.clone(),
                    Value::package(Symbol::intern(&qualified)),
                );
            }
            canonical = qualified;
        }
        // A `my subset` has declaration-site identity (ADR-0047 P1, #9894),
        // exactly like a `my class`: it is stored under `Name\u{0}<site-id>`,
        // so two same-named lexical subsets (`my subset Op` in three sibling
        // class bodies, Java::Generate) no longer collapse into whichever was
        // registered last. Like raku's `fully_qualified_with($package)`, the
        // storage name is package-qualified, so `.^name` and type-check
        // messages report `Owner::Op` and `resolve_lexical_type_key` finds it
        // from the owning class's attribute and method type checks. The
        // mangled key is unreachable by spelling, so `M::F` from outside
        // `module M { my subset F ... }` still does not resolve. The bare
        // name is bound to the storage name in the declaring scope's env.
        if is_my && decl_id != 0 {
            // The owner is the package's source name: a role body run for its
            // pun runs in the pun's mangled package (`R\u{0}pun`), and a key
            // built on that would carry `R` before its first NUL, so marking it
            // `my`-scoped below would make `R` itself read as a lexical type
            // and hide it (#18768).
            let pkg_sym = self.current_package_sym();
            let pkg_sym = match pkg_sym.as_str().split_once('\u{0}') {
                Some((source, _)) => Symbol::intern(source),
                None => pkg_sym,
            };
            let qualified = if already_qualified
                || crate::qualified::is_global_package(pkg_sym)
                || pkg_sym == Symbol::intern("Main")
            {
                name.to_string()
            } else {
                crate::qualified::qualified(pkg_sym, Symbol::intern(name))
                    .as_str()
                    .to_string()
            };
            let storage = format!("{qualified}\u{0}{decl_id}");
            self.types.subset_predicate_cache.remove(&storage);
            self.registry_mut()
                .subsets
                .insert(storage.clone(), def.clone());
            self.mark_my_scoped_package_item(storage.clone());
            canonical = storage;
        }
        // Keep the final name available by its leaf inside the declaring
        // package.  Method signatures use that short spelling (`Position` in
        // `class StaticTable`), while the registry stores the canonical
        // package-qualified subset.  Package-key the alias just like nested
        // classes, so it remains visible to the package's methods without
        // leaking a global short name.
        if !is_my
            && !pkg.is_empty()
            && !crate::qualified::is_global_name(&pkg)
            && pkg != "Main"
            && let Some((_, short)) =
                crate::qualified::split_qualified(crate::qualified::known_symbol(&canonical))
                    .map(|(head, tail)| (head.as_str(), tail.as_str()))
            && !short.is_empty()
            && !Self::is_builtin_type(short)
        {
            crate::runtime::cow_table_mut(&mut self.types.package_type_aliases)
                .entry(pkg.clone())
                .or_default()
                .entry(short.to_string())
                .or_insert_with(|| canonical.clone());
        }
        if is_my && self.enclosing_routine_exists() {
            let depth = self.routine_stack.len();
            self.routines_mut()
                .routine_local_subsets
                .insert(name.to_string(), (def.clone(), depth));
        }
        if is_my && decl_id != 0 {
            // The block's exit reverts the spelling's env binding like a
            // `my $x`, as for a `my class` (#18178, #18217): a bare block and
            // an `EVAL` through `block_declared_vars`, a branch or loop body
            // through the saved binding.
            // Once the scope has exited, its name is suppressed, so a lookup
            // that reaches the registry's lexical index does not find the
            // block's storage name either (`pop_lexical_class_scope`).
            self.save_lexical_type_binding_for_scope_exit(name);
            self.unsuppress_name(name);
            self.register_lexical_class(name.to_string());
            // A `my subset` is reached through its storage name, which the
            // declaring scope binds its spelling to just below. It files the
            // bare key unless a package subset holds it: overwriting an outer
            // `subset S` left every check by that spelling answering the
            // block's subset after the block (#18217). Another scope's `my
            // subset` of the name is replaced, so the key is this
            // declaration's, as the routine-local record below expects
            // (`hide_dynamic_callers_local_subs`, #16099).
            if !shadows_package_subset {
                self.registry_mut().subsets.insert(name.to_string(), def);
            }
        } else {
            self.registry_mut().subsets.insert(name.to_string(), def);
        }
        if crate::runtime::types::is_core_scalar_check_name(name) {
            self.types.core_scalar_name_subset = true;
        }
        if !self.qualified_identity_binding_is_redundant(name, &canonical) {
            self.env
                .insert(name.to_string(), Value::package(Symbol::intern(&canonical)));
        }
    }

    /// Whether `name` is an `our`-scoped subset by that exact spelling: a
    /// package symbol visible to `::('name')` wherever the program runs,
    /// whichever routine or thread declared it (ADR-17216). A `my subset`
    /// also files its bare name in the registry, but always next to its
    /// declaration-site key (`Name\0<id>`, ADR-0047), which rules it out; a
    /// name declared both ways stays unresolved here (the lexical lookups
    /// before this one answer in scope).
    // Cost: O(1) expected: one probe, plus the lexical-key index probe
    // (rebuilt once per registry key change, O(t), t = type keys).
    pub(crate) fn is_package_scoped_subset(&self, name: &str) -> bool {
        let registry = self.registry();
        registry.subsets.contains_key(name) && !registry.has_lexical_type_key_for(name)
    }
}
