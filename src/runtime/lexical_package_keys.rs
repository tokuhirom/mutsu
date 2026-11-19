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
        if name.is_empty()
            || crate::qualified::is_qualified_str(name)
            || crate::builtin_types::catalog::builtin_type_info(name).is_some()
        {
            return None;
        }
        let head = Symbol::intern(name);
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
