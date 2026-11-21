//! Package-qualified names whose head is a lexical package (`my package tmp`,
//! `my class X`): `tmp::Foo`, `$X::v`, `@X::a`.
//!
//! A lexical package is stored under a mangled name, its spelling plus the
//! declaration's id (`tmp\0<id>`, ADR-0047), so two declarations of the same
//! spelling in different scopes are two packages with two stashes (#17449).
//! Their members are keyed under that storage name (`tmp\0<id>::Foo`). The
//! qualified spelling the program writes resolves its head lexically, as
//! rakudo does: through the `tmp` the scope binds, to that declaration's
//! storage name. Mirroring the members under the facing spelling
//! (`tmp::Foo`) instead made every declaration of the spelling share one key,
//! so the last one written won everywhere.

use super::Interpreter;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `key` (`tmp::Foo`, `$X::v`, `@X::a`) with its head replaced by the
    /// storage name of the lexical package the head is bound to here
    /// (`tmp\0<id>::Foo`); `None` when the key is not qualified or its head is
    /// not a lexical package in scope.
    // Cost: O(s) amortized, s = segments of the key (memoized splits, one env
    // probe for the head).
    pub(crate) fn lexical_package_key(&self, key: &str) -> Option<Symbol> {
        let key_sym = Symbol::intern(key);
        let var = crate::qualified::split_qualified_var(key_sym)?;
        let qualifier = Symbol::intern(var.pkg);
        let segments = crate::qualified::segments(qualifier);
        let (&head, rest) = segments.split_first()?;
        let storage = self.lexical_package_storage(head)?;
        let package = rest.iter().fold(storage, |pkg, &segment| {
            crate::qualified::qualified(pkg, segment)
        });
        let member = Symbol::intern(&format!("{}{}", var.sigil, var.bare));
        Some(crate::qualified::qualified_var(package, member))
    }

    /// The storage name (`tmp\0<id>`) of the lexical package `head` is bound
    /// to in the running scope, when it is one.
    // Cost: O(1) expected, one env probe.
    fn lexical_package_storage(&self, head: Symbol) -> Option<Symbol> {
        // The env binding only: the package-chain fallback of
        // `lexical_type_binding_sym` reads `our` variables, which ask here.
        let env = self.env();
        let bound = env
            .get_lexical_type_binding(crate::term_names::lexical_type_key_sym(head), head)
            .or_else(|| env.get_sym(head))?;
        let ValueView::Package(storage) = bound.view() else {
            return None;
        };
        let text = storage.as_str();
        (text.len() > head.as_str().len()
            && text.starts_with(head.as_str())
            && text.as_bytes()[head.as_str().len()] == 0)
            .then_some(storage)
    }

    /// The package the running scope itself declared under the bare name
    /// `head`: a lexical package's storage name (`tmp\0<id>`), or the
    /// package-qualified name a package declared inside another package
    /// gets (`my package Configuration { }` in `module M` is
    /// `M::Configuration`, as on rakudo). `None` when `head` is bound to no
    /// package here, or only to the global package of that very spelling --
    /// which a module merely loaded somewhere may have put in the env.
    // Cost: O(d) expected, d = depth of the running package (one env probe,
    // then one registry probe per enclosing package).
    pub(crate) fn own_package_binding(&self, head: Symbol) -> Option<Symbol> {
        if let Some(storage) = self.lexical_package_storage(head) {
            return Some(storage);
        }
        let env = self.env();
        let bound = env
            .get_lexical_type_binding(crate::term_names::lexical_type_key_sym(head), head)
            .or_else(|| env.get_sym(head));
        if let Some(bound) = bound
            && let ValueView::Package(package) = bound.view()
            && package != head
            && crate::qualified::unqualified_part(package).with_str(|short| {
                short.split_once('\u{0}').map_or(short, |(bare, _)| bare) == head.as_str()
            })
        {
            return Some(package);
        }
        // A package nested in the running package (or one of its enclosing
        // ones) is found through the package chain, not an env binding.
        if let Some(nested) = self.resolve_type_in_current_package(head.as_str())
            && nested != head.as_str()
        {
            return Some(Symbol::intern(&nested));
        }
        crate::qualified::package_ancestors(self.current_package_sym())
            .filter(|pkg| !crate::qualified::is_global_package(*pkg))
            .map(|pkg| crate::qualified::qualified(pkg, head))
            .find(|candidate| {
                candidate.with_str(|text| {
                    self.module.chain_declared_packages.contains(text)
                        || self.is_known_package(text)
                })
            })
    }

    /// Bind the short name of a package declared in the running package's
    /// block (`my package G { }` in `module M`, stored as `M::G`), as rakudo
    /// installs that lexical: it shadows a same-named outer type in the
    /// block, whose env tier drops the binding when it ends (#18481).
    // Cost: O(1) expected: one name split, one env probe and insert.
    pub(crate) fn bind_nested_package_short_name(
        &mut self,
        name: &str,
        value: &crate::value::Value,
    ) {
        let name_sym = Symbol::intern(name);
        if crate::qualified::package_parent(name_sym) != Some(self.current_package_sym()) {
            return;
        }
        let short = crate::qualified::unqualified_part(name_sym);
        self.save_lexical_type_binding_for_scope_exit(short.as_str());
        self.env_mut()
            .insert(short.as_str().to_string(), value.clone());
        if let ValueView::Package(storage) = value.view() {
            self.register_package_block_type(name_sym, storage);
        }
    }

    /// Record that `qualified` (`M::G`) was declared inside its package's
    /// block, stored as `storage` (see `TypeState::package_block_types`).
    // Cost: O(1) expected; a copy of the table when a thread clone shares it.
    pub(crate) fn register_package_block_type(&mut self, qualified: Symbol, storage: Symbol) {
        if self.types.package_block_types.get(&qualified) == Some(&storage) {
            return;
        }
        crate::runtime::cow_table_mut(&mut self.types.package_block_types)
            .insert(qualified, storage);
    }

    /// The storage name of the type or package `name` that the running
    /// package's block, or an enclosing package's, declared under that short
    /// name (#18481).
    // Cost: O(1) when no package block declared a type; else O(d), d = depth
    // of the running package (one qualify and one probe per package).
    pub(crate) fn resolve_package_block_type(&self, name: &str) -> Option<Symbol> {
        if self.types.package_block_types.is_empty() || name.is_empty() {
            return None;
        }
        let short = crate::qualified::known_symbol(name);
        if crate::qualified::is_qualified(short) {
            return None;
        }
        crate::qualified::package_ancestors(self.current_package_sym())
            .filter(|pkg| !crate::qualified::is_global_package(*pkg))
            .find_map(|pkg| {
                self.types
                    .package_block_types
                    .get(&crate::qualified::qualified(pkg, short))
                    .copied()
            })
    }

    /// The storage name (`Inner\0<id>`) of the lexical type (`my class` /
    /// `my role`) the running scope binds under the type-only key of `name`
    /// -- also in a closure that captured that binding from the block that
    /// declared it, after the block exited (#18177). `None` for a qualified
    /// name or one no lexical type of this spelling is bound to here.
    // Cost: O(1) expected: the key's process-wide absence probe, else one
    // env walk of the type-only key and one registry probe.
    pub(crate) fn bound_lexical_type_storage(&self, name: &str) -> Option<Symbol> {
        // A core type's spelling keeps the existing resolution: a lexical
        // refinement of it (`my class DateTime is DateTime`) is handled where
        // it is declared, and code the runtime itself composes (the slang
        // host's `grammar Raku::Grammar`, implicitly `is Grammar`) must not
        // pick up a module's file-scope `my role Grammar`.
        if name.is_empty() {
            return None;
        }
        // One lookup of the text serves both the qualification flag and the
        // binding key below.
        let head = crate::qualified::known_symbol(name);
        if crate::qualified::is_qualified(head)
            || crate::builtin_types::catalog::builtin_type_info(name).is_some()
        {
            return None;
        }
        let env = self.env();
        let bound =
            env.get_lexical_type_binding(crate::term_names::lexical_type_key_sym(head), head)?;
        let ValueView::Package(storage) = bound.view() else {
            return None;
        };
        let text = storage.as_str();
        // A declaration binds its type-only key before its own header
        // resolves (`my class DateTime is DateTime`): until the type is
        // registered the spelling still names the outer one.
        (text.len() > name.len()
            && text.starts_with(name)
            && text.as_bytes()[name.len()] == 0
            && self.has_type_direct(text))
        .then_some(storage)
    }
}
