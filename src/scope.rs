//! `NameScope`: what the frontend knows about names when it classifies a bare
//! word or an operator (ADR-15760).
//!
//! One value holds what used to live in four separate thread-locals
//! (`CALLER_NAMES`, `CALLER_TRAIT_TYPES`, the `EVAL` caller's operators, and the
//! unit's own `DECLARED_NAMES`). The entry points that are not a plain parse
//! (string `EVAL`, `Str.AST`, `EVAL($ast)`) build one from the calling scope and
//! hand it to the frontend; the frontend installs it for the duration of the
//! conversion or lowering (`rakuast::frontend::with_scope`).

use std::collections::{HashMap, HashSet};

/// What a declared name renders as when it is named later.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum DeclaredKind {
    /// A `class` / `role` / `grammar` / `enum` name.
    Type,
    /// A `constant` name, an enum value the unit declares (`Red`, `Color::Red`),
    /// or a sigilless parameter (`\x`): all render as `Term::Name` (measured on
    /// rakudo 2026.09).
    Term,
    /// An `our sub` of a package, reached by its qualified name (`M::foo`):
    /// rakudo renders the bare reference as an argument-less `Call::Name`.
    Routine,
}

/// The names visible to the frontend at the unit being converted or lowered.
#[derive(Clone, Default)]
pub(crate) struct NameScope {
    /// The names the unit itself declares, filled by the frontend from the
    /// finished unit (ADR-15760 slice 1 replaces this with source order).
    declared: HashMap<String, DeclaredKind>,
    /// The names the calling scope of an `EVAL` string or a `Str.AST` declares:
    /// the unit being converted is only the string, so its own declarations
    /// cannot say what `aa` of `enum E <aa bb>; EVAL "aa"` is.
    caller_names: HashMap<String, DeclaredKind>,
    /// The type names the calling scope of a `Str.AST` declares, for the
    /// `is NAME` trait of a declaration in its source.
    caller_trait_types: HashSet<String>,
    /// The operator routines (`infix:<…>`) the calling scope makes visible.
    caller_operators: HashSet<String>,
}

impl NameScope {
    /// The caller's type names and terms. A term for which `is_setting_term` holds
    /// (`Less`, `PromiseStatus::Kept`) keeps its setting classification.
    // Cost: O(n), n = number of caller names.
    pub(crate) fn with_caller_names(
        mut self,
        types: Vec<String>,
        terms: Vec<String>,
        is_setting_term: impl Fn(&str) -> bool,
    ) -> Self {
        self.caller_names
            .extend(types.into_iter().map(|n| (n, DeclaredKind::Type)));
        self.caller_names.extend(
            terms
                .into_iter()
                .filter(|n| !is_setting_term(n))
                .map(|n| (n, DeclaredKind::Term)),
        );
        self
    }

    /// The caller's type names, for `is NAME` traits.
    // Cost: O(n), n = number of caller type names.
    pub(crate) fn with_caller_trait_types(mut self, types: Vec<String>) -> Self {
        self.caller_trait_types.extend(types);
        self
    }

    /// The operator routines the caller declares (only `infix:<…>` ones count).
    // Cost: O(n), n = number of operator names.
    pub(crate) fn with_caller_operators(mut self, operators: &[String]) -> Self {
        self.caller_operators.extend(
            operators
                .iter()
                .filter(|name| name.starts_with("infix:<") && name.ends_with('>'))
                .cloned(),
        );
        self
    }

    /// Replace the unit's own declared names.
    pub(crate) fn set_declared(&mut self, declared: HashMap<String, DeclaredKind>) {
        self.declared = declared;
    }

    /// Take the unit's own declared names out, leaving none.
    pub(crate) fn take_declared(&mut self) -> HashMap<String, DeclaredKind> {
        std::mem::take(&mut self.declared)
    }

    /// What the unit declares `name` as, then what the calling scope does.
    // Cost: O(k), k = length of `name`.
    pub(crate) fn kind(&self, name: &str) -> Option<DeclaredKind> {
        self.declared
            .get(name)
            .or_else(|| self.caller_names.get(name))
            .copied()
    }

    /// Whether the calling scope declares the type `name`, for an `is NAME` trait.
    // Cost: O(k), k = length of `name`.
    pub(crate) fn declares_trait_type(&self, name: &str) -> bool {
        self.caller_trait_types.contains(name)
    }

    /// Whether the calling scope declares the operator routine `routine`.
    // Cost: O(k), k = length of `routine`.
    pub(crate) fn caller_declares_operator(&self, routine: &str) -> bool {
        self.caller_operators.contains(routine)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn caller_terms_skip_setting_terms_and_keep_their_kind() {
        let scope = NameScope::default().with_caller_names(
            vec!["Foo".to_string()],
            vec!["aa".to_string(), "Less".to_string()],
            |n| n == "Less",
        );
        assert_eq!(scope.kind("Foo"), Some(DeclaredKind::Type));
        assert_eq!(scope.kind("aa"), Some(DeclaredKind::Term));
        assert_eq!(scope.kind("Less"), None);
    }

    #[test]
    fn the_unit_declares_before_the_caller() {
        let mut scope = NameScope::default().with_caller_names(
            vec!["N".to_string()],
            Vec::new(),
            |_| false,
        );
        scope.set_declared(HashMap::from([("N".to_string(), DeclaredKind::Term)]));
        assert_eq!(scope.kind("N"), Some(DeclaredKind::Term));
        scope.take_declared();
        assert_eq!(scope.kind("N"), Some(DeclaredKind::Type));
    }

    #[test]
    fn only_infix_routines_are_caller_operators() {
        let scope = NameScope::default()
            .with_caller_operators(&["infix:<foo>".to_string(), "prefix:<bar>".to_string()]);
        assert!(scope.caller_declares_operator("infix:<foo>"));
        assert!(!scope.caller_declares_operator("prefix:<bar>"));
    }
}
