//! Whole-stash materialization for a package (`P::`, `::("P")`, `.WHO`).
use super::accessors_stash_keyed::EnvStashMember;
use super::*;
use crate::value::ValueMap;
use crate::value::ValueView;

impl Interpreter {
    // Cost: O(v), v = entries of the whole env (plus `our_vars` for GLOBAL), independent of the
    // package's own symbol count: the stash is rebuilt by scanning env on every call.
    // Rakudo: O(1) (the package's persistent Stash) -- see #9171.
    pub(crate) fn package_stash_value(&self, package: &str) -> Value {
        let package_name = Self::normalize_stash_package(package);

        // PROCESS:: pseudo-package: exposes process-level dynamic variables
        // like $*PROGRAM, $*PID, %*ENV, @*ARGS, etc.
        // PROCESS::<$PROGRAM> looks up key "$PROGRAM" in the stash.
        //
        // Dynamic vars are visible across the whole caller chain (that's what
        // makes them dynamic), not just the CURRENT frame's own `self.env` --
        // a `PROCESS::<$X> = ...` set in an outer frame must still resolve
        // from a callee (e.g. Log::Timeline's `PROCESS::<$LOG-TIMELINE-OUTPUT>`,
        // set at the mainline and read from deep inside its logging subs). Reuse
        // `dynamic_pseudo_stash_entries` (the same caller-chain walk backing
        // `DYNAMIC::`) instead of only scanning `self.env`, which silently
        // dropped every outer-frame dynamic once called from a sub.
        if package_name == "PROCESS" {
            let mut symbols: ValueMap = ValueMap::default();
            for (key, val) in self.dynamic_pseudo_stash_entries() {
                // `dynamic_pseudo_stash_entries` spells entries with the `*`
                // twigil (`$*NAME`/`@*NAME`/`%*NAME`); PROCESS::'s stash keys
                // drop it (`$NAME`/`@NAME`/`%NAME`), since the twigil is
                // implicit in the PROCESS:: package itself.
                if let Some(name) = key.strip_prefix("$*") {
                    symbols.insert(format!("${name}"), val);
                } else if let Some(name) = key.strip_prefix("@*") {
                    symbols.insert(format!("@{name}"), val);
                } else if let Some(name) = key.strip_prefix("%*") {
                    symbols.insert(format!("%{name}"), val);
                }
            }
            return Self::make_stash_instance(package, symbols);
        }

        // Bool is a built-in enum whose members are not registered through the
        // usual enum path; its stash still exposes them (`Bool::.values`).
        if package_name == "Bool" {
            let mut symbols: ValueMap = ValueMap::default();
            symbols.insert("False".to_string(), Value::FALSE);
            symbols.insert("True".to_string(), Value::TRUE);
            return Self::make_stash_instance(package, symbols);
        }

        if let Some((module, tag)) = Self::package_export_tag_parts(package) {
            let mut symbols: ValueMap = ValueMap::default();
            if let Some(subs) = self.exported_subs.get(module) {
                for (name, tags) in subs {
                    if tag != "ALL" && !tags.contains(tag) {
                        continue;
                    }
                    let fq = format!("{module}::{name}");
                    // A natively-provided module's routines are registered
                    // under their BARE name (there is no `Mod::name`
                    // `FunctionDef` to find), so the package-qualified lookup
                    // yields Nil and the stash entry would exist with no value
                    // behind it -- `::("Test::EXPORT::DEFAULT::&ok")` resolved
                    // the path and then answered Nil.
                    let mut code = self.resolve_code_var(&fq);
                    if code.is_nil() {
                        code = self.resolve_code_var(name);
                    }
                    symbols.insert(format!("&{name}"), code);
                }
            }
            if let Some(vars) = self.exported_vars.get(module) {
                for (name, tags) in vars {
                    if tag != "ALL" && !tags.contains(tag) {
                        continue;
                    }
                    let val = self.exported_var_value(module, name).unwrap_or(Value::NIL);
                    symbols.insert(name.clone(), val);
                }
            }
            return Self::make_stash_instance(package, symbols);
        }

        if let Some(module) = Self::package_export_module(&package_name) {
            let mut tags = std::collections::BTreeSet::new();
            if let Some(subs) = self.exported_subs.get(module) {
                for tagset in subs.values() {
                    tags.extend(tagset.iter().cloned());
                }
            }
            if let Some(vars) = self.exported_vars.get(module) {
                for tagset in vars.values() {
                    tags.extend(tagset.iter().cloned());
                }
            }
            // `ALL` is always a member of an EXPORT package — it is the
            // everything-tag, not a tag any symbol is declared with, so it never
            // appears in the collected tag sets. Leaving it out made
            // `::('Mod::EXPORT::ALL')` (resolved one component at a time) fail on
            // the last step even though the whole name resolves.
            tags.insert("ALL".to_string());
            let mut symbols: ValueMap = ValueMap::default();
            for tag in tags {
                symbols.insert(
                    tag.clone(),
                    Value::package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        &tag,
                    ))),
                );
            }
            return Self::make_stash_instance(package, symbols);
        }

        let mut symbols: ValueMap = ValueMap::default();
        let is_lowercase_export_stash =
            package_name == "EXPORT::all" || package_name.ends_with("::EXPORT::all");

        // Top-level `our` variables live in GLOBAL. They persist in the flat
        // `our_vars` store (keyed by bare name), separate from the env, so the
        // env scan below never sees them; add them explicitly so
        // `GLOBAL::.<$x>` / `::("GLOBAL")::('$x')` find package-scoped `our`
        // declarations. Only GLOBAL and the root get these; a named user
        // package must not vacuum up every bare `our` name.
        if package_name == "GLOBAL" || package_name.is_empty() {
            for (key, val) in self.our_vars_iter() {
                // `our_vars` carries both a bare mirror (`o`) and a
                // fully-qualified one (`GLOBAL::o`) for a root-scope `our`
                // declaration. The qualified spelling is the *same* symbol,
                // not a sub-package member named literally "GLOBAL" -- strip
                // the self-qualification before deciding what kind of member
                // this is (a genuine cross-package key like `Mod::modvar`
                // still yields its head as a sub-package, same as the env
                // scan below).
                let effective_key = key.strip_prefix("GLOBAL::").unwrap_or(key.as_str());
                if let Some((head, _)) = effective_key.split_once("::") {
                    // `our_vars` also carries internal bookkeeping markers
                    // qualified the same way a real sub-package would be
                    // (e.g. `__mutsu_sigilless_readonly::EvalPreseedTerm`);
                    // these are not user-visible symbols.
                    if head.is_empty()
                        || Self::env_tail_has_sigil(head)
                        || head.starts_with("__mutsu_")
                    {
                        continue;
                    }
                    symbols.entry(head.to_string()).or_insert_with(|| {
                        Value::package(Symbol::intern(&Self::qualify_stash_name(
                            &package_name,
                            head,
                        )))
                    });
                    continue;
                }
                symbols
                    .entry(Self::add_sigil_prefix(effective_key))
                    .or_insert_with(|| val.clone());
            }
        }

        for (key, val) in self.env.iter() {
            let key_s = key.resolve();
            // An enum key is a genuine package symbol, so it belongs in the stash
            // under its BARE name -- but it is stored in the enum-key namespace
            // (#7914), which the internal-key skip in `env_stash_member` would
            // otherwise drop. Unwrap it back to the spelling the stash publishes.
            let key_s = match key_s.strip_prefix(crate::runtime::enum_bare_names::ENUM_BARE_PREFIX)
            {
                Some(bare) => bare.to_string(),
                None => key_s,
            };
            match self.env_stash_member(&key_s, &package_name) {
                Some(EnvStashMember::Value(stash_key)) => {
                    symbols.insert(stash_key, val.clone());
                }
                Some(EnvStashMember::SubPackage(head)) => {
                    let qualified = Self::qualify_stash_name(&package_name, &head);
                    symbols
                        .entry(head)
                        .or_insert_with(|| Value::package(Symbol::intern(&qualified)));
                }
                None => {}
            }
        }

        // Code-valued `our constant &alias is export(...)` declarations are
        // stored in the export-variable table rather than the routine
        // registry.  They are still ordinary members of the defining module's
        // stash (`Module::<&alias>`), so expose them alongside exported subs.
        if let Some(vars) = self.exported_vars.get(package_name.as_str()) {
            for name in vars.keys() {
                if let Some(value) = self.exported_var_value(&package_name, name) {
                    symbols.entry(name.clone()).or_insert(value);
                }
            }
        }

        // A named package's `our` symbols also live in the flat `our_vars`
        // store -- that is where an `our` declared in a branch that never RAN
        // is pre-installed (`EndWalker::install_our_symbol`), so a dead-branch
        // `class Foo { if False { our $c = 1 } }` still lists `$c`. `or_insert`
        // so a live env value always wins over the pre-installed type object.
        if package_name != "GLOBAL" && !package_name.is_empty() {
            for (key, val) in self.our_vars_iter() {
                if let Some(stash_key) = Self::our_var_stash_member(key, &package_name) {
                    symbols.entry(stash_key).or_insert_with(|| val.clone());
                }
            }
        }

        // Enum members are stored in the enum registry as `(name, value)`
        // pairs, while the package stash is assembled from ordinary lexical
        // and package bindings. Reconstruct the enum's members here so an
        // indirect package lookup such as `EnumBits::{"OPEN_READONLY"}` can
        // traverse a type object captured through a parametric role.
        if let Some(variants) = self.registry().enum_types.get(&package_name) {
            for (index, (key, value)) in variants.iter().enumerate() {
                symbols.entry(key.clone()).or_insert_with(|| {
                    Value::enum_parts(
                        Symbol::intern(package_name.as_str()),
                        Symbol::intern(key),
                        value.clone(),
                        index,
                    )
                });
            }
        }

        for (key, def) in self.registry().functions.iter() {
            let key_s = key.resolve();
            let Some(base) = self.routine_stash_member(&key_s, &package_name, true) else {
                continue;
            };
            // A custom EXPORT hook reads the lowercase stash as a source of
            // first-class code values (`EXPORT::all::{...}:p`). Preserve the
            // compiled definition there; ordinary package stashes keep their
            // routine references and therefore retain normal import lookup.
            symbols.entry(format!("&{base}")).or_insert_with(|| {
                if is_lowercase_export_stash {
                    let candidates = self.resolve_all_multi_candidates(base);
                    if candidates.len() > 1 {
                        self.sub_value_from_multi_candidates(base, candidates)
                    } else {
                        self.sub_value_from_function_def((**def).clone())
                    }
                } else {
                    Value::routine_parts(def.package, def.name, false)
                }
            });
        }

        // A custom EXPORT hook commonly reads the lowercase stash as a
        // source of first-class code values (`EXPORT::all::{...}:p`).  The
        // registry loop above covers exported subs, but code-valued `our
        // constant &alias is export(...)` declarations live in
        // `exported_vars` instead.  Include those entries in the process-wide
        // lowercase view as well; this is the view used while a module's
        // custom EXPORT hook is running, before its module-qualified stash is
        // available through a lexical package binding.
        if is_lowercase_export_stash {
            for (module, vars) in self.exported_vars.iter() {
                for name in vars.keys() {
                    if let Some(value) = self.exported_var_value(module, name) {
                        let value = if let (true, ValueView::Sub(data)) =
                            (name.starts_with('&'), value.view())
                        {
                            // Code-valued export aliases such as `&distinct =
                            // &uniq` must point at the same first-class
                            // dispatcher as their source.  The source entry
                            // was materialized by the registry loop above;
                            // reusing it preserves `=:=` identity and the
                            // complete multi-candidate family.
                            symbols
                                .get(&format!("&{}", data.name))
                                .cloned()
                                .unwrap_or(value)
                        } else {
                            value
                        };
                        symbols.entry(name.clone()).or_insert(value);
                    }
                }
            }
        }

        // A module that exports anything has an `EXPORT` member in its stash
        // (`module Foo { our sub bar is export {} }` gives `Foo::.keys` ==
        // `(&bar EXPORT)` in Rakudo). Without it, walking a name one component
        // at a time — which is what `::('Foo::EXPORT::ALL')` does — stopped at
        // `EXPORT` even though `Foo::EXPORT::ALL` resolves when spelled whole.
        if self.exported_subs.contains_key(package_name.as_str())
            || self.exported_vars.contains_key(package_name.as_str())
        {
            symbols.entry("EXPORT".to_string()).or_insert_with(|| {
                Value::package(Symbol::intern(&format!("{package_name}::EXPORT")))
            });
        }

        // A `proto sub` is held in its own registry, not in `functions`, so the
        // loop above never saw it — `module M { our proto sub f(|) {*} }` had an
        // empty stash and `::('M::&f')` was a Failure. Its candidates are not
        // stash members in Rakudo either (a bare `multi` is lexical); the proto
        // is the one visible name, and it is visible exactly when it is `our`.
        for (key, def) in self.registry().proto_functions.iter() {
            let key_s = key.resolve();
            let Some(base) = self.routine_stash_member(&key_s, &package_name, false) else {
                continue;
            };
            symbols
                .entry(format!("&{base}"))
                .or_insert_with(|| Value::routine_parts(def.package, def.name, false));
        }

        for class_name in self.registry().classes.keys() {
            let class_short = class_name
                .rsplit_once("::")
                .map(|(_, short)| short)
                .unwrap_or(class_name.as_str());
            if (package_name == "MY" || package_name == "GLOBAL")
                && (self.need_hidden_classes.contains(class_name)
                    || self.need_hidden_classes.contains(class_short))
            {
                continue;
            }
            // Skip classes hidden from package stash lookups (transitive deps)
            if package_name != "MY"
                && package_name != "GLOBAL"
                && self.package_stash_hidden.contains(class_name)
            {
                continue;
            }
            // GLOBAL is the root package: `stash_member_tail` treats every
            // registered class as a match (there is no `GLOBAL::` prefix to
            // require), which otherwise drags every builtin type (`Int`,
            // `Promise`, `Thread`, ...) into the user's own GLOBAL stash. Real
            // Raku keeps core types in the setting, not the user's GLOBAL --
            // only a class the user actually declared (`class`/`package`/
            // `module`/`grammar`) is a genuine GLOBAL member.
            if package_name == "GLOBAL" && !self.user_declared_classes.contains(class_name) {
                continue;
            }
            // Skip my-scoped classes (they should not appear in the package stash)
            if self.is_my_scoped_package_item(class_name) {
                continue;
            }
            let Some(rest) = Self::stash_member_tail(class_name, &package_name) else {
                continue;
            };
            if rest.is_empty() {
                continue;
            }
            if let Some((head, _)) = rest.split_once("::") {
                symbols.entry(head.to_string()).or_insert_with(|| {
                    Value::package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        head,
                    )))
                });
                continue;
            }
            symbols.entry(rest.to_string()).or_insert_with(|| {
                Value::package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    rest,
                )))
            });
        }
        for role_name in self.registry().roles.keys() {
            // Skip roles hidden from package stash lookups (transitive deps)
            if package_name != "MY"
                && package_name != "GLOBAL"
                && self.package_stash_hidden.contains(role_name)
            {
                continue;
            }
            // GLOBAL is the root package: same reasoning as the classes loop
            // above -- only a role the user actually declared is a genuine
            // GLOBAL member, not every built-in role (`Positional`, `Iterable`, ...).
            if package_name == "GLOBAL" && !self.registry().user_declared_roles.contains(role_name)
            {
                continue;
            }
            let Some(rest) = Self::stash_member_tail(role_name, &package_name) else {
                continue;
            };
            if rest.is_empty() {
                continue;
            }
            if let Some((head, _)) = rest.split_once("::") {
                symbols.entry(head.to_string()).or_insert_with(|| {
                    Value::package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        head,
                    )))
                });
                continue;
            }
            symbols.entry(rest.to_string()).or_insert_with(|| {
                Value::package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    rest,
                )))
            });
        }

        if self.exported_subs.contains_key(&package_name)
            || self.exported_vars.contains_key(&package_name)
        {
            symbols.entry("EXPORT".to_string()).or_insert_with(|| {
                Value::package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    "EXPORT",
                )))
            });
        }

        Self::make_stash_instance(package, symbols)
    }
}
