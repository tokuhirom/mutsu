//! `NameScope`: what the frontend knows about names when it classifies a bare
//! word or an operator (ADR-15760).
//!
//! One value holds what used to live in four separate thread-locals
//! (`CALLER_NAMES`, `CALLER_TRAIT_TYPES`, the `EVAL` caller's operators, and the
//! unit's own `DECLARED_NAMES`). The entry points that are not a plain parse
//! (string `EVAL`, `Str.AST`, `EVAL($ast)`) build one from the calling scope and
//! hand it to the frontend; the frontend installs it for the duration of the
//! conversion or lowering as an explicit parameter. Its block stack and the unit's
//! own names use interior mutability, so one `&NameScope` is shared down the
//! whole walk.

use std::cell::RefCell;
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
    declared: RefCell<HashMap<String, DeclaredKind>>,
    /// The declarations of the blocks being converted, innermost last. Each
    /// holds only the names its own statement list declares: a lexical enum
    /// variable shadows a class of the same name declared further out, which the
    /// unit-wide names (one entry per name) cannot express.
    blocks: RefCell<Vec<HashMap<String, DeclaredKind>>>,
    /// The package declarations being converted, innermost last: the package
    /// a method's implicit invocant parameter is typed by.
    packages: RefCell<Vec<Option<String>>>,
    /// The names the calling scope of an `EVAL` string or a `Str.AST` declares:
    /// the unit being converted is only the string, so its own declarations
    /// cannot say what `aa` of `enum E <aa bb>; EVAL "aa"` is.
    caller_names: HashMap<String, DeclaredKind>,
    /// The type names the calling scope of a `Str.AST` declares, for the
    /// `is NAME` trait of a declaration in its source.
    caller_trait_types: HashSet<String>,
    /// The operator routines (`infix:<…>`) the calling scope makes visible.
    caller_operators: HashSet<String>,
    /// What the unit being finalized or lowered declares anywhere, installed for
    /// that unit by [`UnitGuard`] (ADR-15760 §7.5): the finalization's unit
    /// state is this value's, not a thread's, so a finalization never clobbers
    /// an enclosing one.
    unit: RefCell<UnitFacts>,
    /// The `True` / `False` terms each enclosing statement list redeclares
    /// (`constant True = 42`), innermost last.
    keyword_scopes: RefCell<Vec<HashSet<String>>>,
    /// The `infix:<…>` routines each enclosing statement list declares,
    /// innermost last.
    operator_scopes: RefCell<Vec<HashSet<String>>>,
}

/// What a unit declares anywhere that lowering resolves a name against: the
/// constants and terms that shadow a setting name, its sigilless terms, its
/// packages and its routines.
#[derive(Clone, Default, Debug)]
pub(crate) struct UnitFacts {
    /// Names whose setting meaning the unit shadows (`constant e`, `-> \pi {}`).
    pub(crate) shadowed: HashSet<String>,
    /// The sigilless terms the unit declares (`my \x`, `constant x`, `\x`).
    pub(crate) terms: HashSet<String>,
    /// The packages the unit declares (`class Nil`, `module M`).
    pub(crate) packages: HashSet<String>,
    /// The routines the unit declares (`sub f`, `my &f`, a `&f` parameter).
    pub(crate) routines: HashSet<String>,
}

/// Which per-list scope stack of a [`NameScope`] a [`ListScope`] opens.
#[derive(Clone, Copy)]
pub(crate) enum ListStack {
    /// `True` / `False` redeclarations.
    Keywords,
    /// `infix:<…>` declarations.
    Operators,
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

    /// Replace the unit's own declared names, returning the previous ones.
    // Cost: O(1).
    pub(crate) fn replace_declared(
        &self,
        declared: HashMap<String, DeclaredKind>,
    ) -> HashMap<String, DeclaredKind> {
        std::mem::replace(&mut *self.declared.borrow_mut(), declared)
    }

    /// Add `more` to the unit's own declared names (a statement reached in
    /// source order).
    // Cost: O(m), m = names in `more`.
    pub(crate) fn extend_declared(&self, more: HashMap<String, DeclaredKind>) {
        self.declared.borrow_mut().extend(more);
    }

    /// Open a block that declares `own`; [`Self::pop_block`] closes it.
    // Cost: O(1).
    pub(crate) fn push_block(&self, own: HashMap<String, DeclaredKind>) {
        self.blocks.borrow_mut().push(own);
    }

    /// Close the innermost block opened by [`Self::push_block`].
    // Cost: O(1).
    pub(crate) fn pop_block(&self) {
        self.blocks.borrow_mut().pop();
    }

    /// Enter the body of the package `name`; [`Self::pop_package`] leaves it.
    // Cost: O(1).
    pub(crate) fn push_package(&self, name: Option<String>) {
        self.packages.borrow_mut().push(name);
    }

    /// Leave the package opened by [`Self::push_package`].
    // Cost: O(1).
    pub(crate) fn pop_package(&self) {
        self.packages.borrow_mut().pop();
    }

    /// The package whose body is being converted, if any and if it types its
    /// methods' invocants (a grammar does not).
    // Cost: O(k), k = length of the name.
    pub(crate) fn enclosing_package(&self) -> Option<String> {
        self.packages.borrow().last().cloned().flatten()
    }

    /// What the innermost block that declares `name` says it is.
    // Cost: O(k + d), k = length of `name`, d = block depth.
    pub(crate) fn block_kind(&self, name: &str) -> Option<DeclaredKind> {
        self.blocks
            .borrow()
            .iter()
            .rev()
            .find_map(|m| m.get(name).copied())
    }

    /// What the unit declares `name` as, then what the calling scope does.
    // Cost: O(k), k = length of `name`.
    pub(crate) fn kind(&self, name: &str) -> Option<DeclaredKind> {
        self.declared
            .borrow()
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

impl NameScope {
    fn stack(&self, which: ListStack) -> &RefCell<Vec<HashSet<String>>> {
        match which {
            ListStack::Keywords => &self.keyword_scopes,
            ListStack::Operators => &self.operator_scopes,
        }
    }

    /// The unit facts installed by the innermost [`UnitGuard`].
    // Cost: O(1).
    pub(crate) fn unit(&self) -> std::cell::Ref<'_, UnitFacts> {
        self.unit.borrow()
    }

    /// Add the shadowed names, terms and packages of `more` to the installed
    /// unit facts (a statement reached in source order); its routines are not
    /// added, since a unit's routines are installed up front (they are hoisted).
    // Cost: O(m), m = names in `more`.
    pub(crate) fn extend_unit_in_order(&self, more: UnitFacts) {
        let mut unit = self.unit.borrow_mut();
        unit.shadowed.extend(more.shadowed);
        unit.terms.extend(more.terms);
        unit.packages.extend(more.packages);
    }

    /// Record `name` in the innermost open list of `which`.
    // Cost: O(k), k = length of `name`.
    pub(crate) fn note_in_list(&self, which: ListStack, name: String) {
        if let Some(scope) = self.stack(which).borrow_mut().last_mut() {
            scope.insert(name);
        }
    }

    /// Whether an open list of `which` holds `name`.
    // Cost: O(s), s = open lists.
    pub(crate) fn in_open_list(&self, which: ListStack, name: &str) -> bool {
        self.stack(which)
            .borrow()
            .iter()
            .any(|scope| scope.contains(name))
    }
}

/// Installs one unit's [`UnitFacts`], with empty per-list scope stacks, while
/// it lives; the enclosing unit's are restored on drop.
pub(crate) struct UnitGuard<'a> {
    names: &'a NameScope,
    facts: UnitFacts,
    keywords: Vec<HashSet<String>>,
    operators: Vec<HashSet<String>>,
}

impl<'a> UnitGuard<'a> {
    // Cost: O(1).
    pub(crate) fn enter(names: &'a NameScope, facts: UnitFacts) -> Self {
        UnitGuard {
            names,
            facts: std::mem::replace(&mut *names.unit.borrow_mut(), facts),
            keywords: std::mem::take(&mut *names.keyword_scopes.borrow_mut()),
            operators: std::mem::take(&mut *names.operator_scopes.borrow_mut()),
        }
    }
}

impl Drop for UnitGuard<'_> {
    fn drop(&mut self) {
        *self.names.unit.borrow_mut() = std::mem::take(&mut self.facts);
        *self.names.keyword_scopes.borrow_mut() = std::mem::take(&mut self.keywords);
        *self.names.operator_scopes.borrow_mut() = std::mem::take(&mut self.operators);
    }
}

/// One statement list's scope on a per-list stack of a [`NameScope`], popped
/// on drop.
pub(crate) struct ListScope<'a>(&'a NameScope, ListStack);

impl<'a> ListScope<'a> {
    // Cost: O(1).
    pub(crate) fn enter(names: &'a NameScope, which: ListStack) -> Self {
        names.stack(which).borrow_mut().push(HashSet::new());
        ListScope(names, which)
    }
}

impl Drop for ListScope<'_> {
    fn drop(&mut self) {
        self.0.stack(self.1).borrow_mut().pop();
    }
}

/// Keeps `name` as the enclosing package of [`NameScope`] while it lives.
pub(crate) struct PackageGuard<'a>(&'a NameScope);

impl<'a> PackageGuard<'a> {
    // Cost: O(1).
    pub(crate) fn enter(names: &'a NameScope, name: Option<String>) -> Self {
        names.push_package(name);
        PackageGuard(names)
    }
}

impl Drop for PackageGuard<'_> {
    fn drop(&mut self) {
        self.0.pop_package();
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
        let scope =
            NameScope::default().with_caller_names(vec!["N".to_string()], Vec::new(), |_| false);
        scope.replace_declared(HashMap::from([("N".to_string(), DeclaredKind::Term)]));
        assert_eq!(scope.kind("N"), Some(DeclaredKind::Term));
        scope.replace_declared(HashMap::new());
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
