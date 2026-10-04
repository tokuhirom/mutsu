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
                right,
            } = pred
                && matches!(left.as_ref(), Expr::Whatever)
            {
                return Expr::Lambda {
                    param: "_".to_string(),
                    body: vec![Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Var("_".to_string())),
                        op: crate::token_kind::TokenKind::SmartMatch,
                        right: right.clone(),
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
            if let Expr::WhateverCurry(curried) = pred
                && let lambda @ Expr::Lambda {
                    is_whatever_code: true,
                    ..
                } = crate::whatever_curry::build_closure(curried)
            {
                return lambda;
            }
            pred.clone()
        });
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
        let predicate_inline = predicate
            .as_ref()
            .and_then(crate::runtime::types::subset_inline_predicate)
            .is_some();
        // Shared: a type check reads the definition on every call, and a
        // deep clone of it (predicate AST included) was part of that cost.
        let def = std::sync::Arc::new(SubsetDef {
            base: self.lexical_env_remap_name(base),
            predicate,
            version: version.to_string(),
            decl_package_sym: crate::symbol::Symbol::intern(&pkg),
            predicate_inline,
            predicate_closure,
            refinement,
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
            let pkg_sym = self.current_package_sym();
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
        self.registry_mut().subsets.insert(name.to_string(), def);
        if !self.qualified_identity_binding_is_redundant(name, &canonical) {
            self.env
                .insert(name.to_string(), Value::package(Symbol::intern(&canonical)));
        }
    }
}
