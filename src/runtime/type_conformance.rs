//! A memo for [`Interpreter::nominal_type_conforms`] (#14924).
//!
//! Ranking a multi's candidates asks "does declared type A conform to declared
//! type B" once per ordered pair of candidates. The general answer walks
//! `type_hierarchy_distance` (several env and registry lookups, ~9k
//! instructions); with 200 candidates that is ~160k asks and ~700M
//! instructions for the first call, and a second argument-type tuple pays it
//! again although the relation between *declared* types does not depend on the
//! arguments.
//!
//! Two layers, both exact:
//!
//! - **Per-name facts, valid inside one [`ConformanceScope`].** A scope wraps a
//!   ranking, during which no user code runs, so the env and the registry are
//!   fixed. For a registered plain class `sub` the facts are its ancestor set
//!   (MRO plus composed roles); for a `sup` they are whether the lexical env
//!   leaves its name alone (no alias, constant or mixin bound to it). When both
//!   are plain, `sub` conforms to `sup` exactly when `sup` is in the set --
//!   what `type_hierarchy_distance` answers for a type object of such a class.
//!   Every other pair takes the general walk.
//! - **Pair answers across scopes**, kept only while
//!   [`Registry::type_stamp`](crate::runtime::registry::Registry::type_stamp)
//!   is unchanged. The stamp changes on every write to the registry (its
//!   `DerefMut` is the only path to a `&mut Registry`), so an answer cannot
//!   outlive a change to a class, role, enum or subset. The env half of the
//!   inputs is re-established per scope by the facts, so only pairs whose two
//!   names are plain *in this scope* are served or stored.
//!
//! Debug builds recompute every fast answer the general way and assert the two
//! agree, so a case the facts get wrong fails the `t/` suite.

use super::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::{Cell, RefCell};
use std::sync::Arc;

/// The ancestors a plain registered class conforms to: its MRO and the roles it
/// composes.
type Ancestors = Arc<FxHashSet<Symbol>>;

#[derive(Default)]
pub(crate) struct ConformanceCache {
    /// The registry stamp `pairs` was filled under.
    stamp: Cell<u64>,
    pairs: RefCell<FxHashMap<(Symbol, Symbol), bool>>,
    /// Open [`ConformanceScope`]s; the facts below are valid while it is > 0.
    scope_depth: Cell<u32>,
    subs: RefCell<FxHashMap<Symbol, Option<Ancestors>>>,
    sups: RefCell<FxHashMap<Symbol, bool>>,
}

/// While alive, per-name facts are memoised; see the module docs. Opened around
/// a ranking, never around code that can run user code.
pub(crate) struct ConformanceScope<'a> {
    cache: &'a ConformanceCache,
}

impl Drop for ConformanceScope<'_> {
    fn drop(&mut self) {
        let depth = self.cache.scope_depth.get() - 1;
        self.cache.scope_depth.set(depth);
        if depth == 0 {
            self.cache.subs.borrow_mut().clear();
            self.cache.sups.borrow_mut().clear();
        }
    }
}

impl Interpreter {
    /// Open a scope for the rankings that follow.
    // Cost: O(1).
    pub(crate) fn conformance_scope(&self) -> ConformanceScope<'_> {
        let cache = &self.caches.type_conformance;
        cache.scope_depth.set(cache.scope_depth.get() + 1);
        ConformanceScope { cache }
    }

    /// The answer of [`Self::nominal_type_conforms`] for two plain names, or
    /// `None` when either is not (or no scope is open): the caller then takes
    /// the general walk.
    // Cost: O(1) per cached pair; O(d) the first time for each name, d = the
    // class's MRO depth.
    pub(super) fn conformance_fast(&self, sub: Symbol, sup: Symbol) -> Option<bool> {
        let cache = &self.caches.type_conformance;
        if cache.scope_depth.get() == 0 {
            return None;
        }
        let ancestors = self.conformance_sub_facts(sub)?;
        if !self.conformance_sup_is_plain(sup) {
            return None;
        }
        let stamp = self.registry().type_stamp();
        if cache.stamp.get() != stamp {
            cache.pairs.borrow_mut().clear();
            cache.stamp.set(stamp);
        }
        if let Some(&known) = cache.pairs.borrow().get(&(sub, sup)) {
            return Some(known);
        }
        let answer = ancestors.contains(&sup);
        cache.pairs.borrow_mut().insert((sub, sup), answer);
        Some(answer)
    }

    /// `sub`'s ancestor set when it is a plain registered class, memoised for
    /// the open scope.
    fn conformance_sub_facts(&self, sub: Symbol) -> Option<Ancestors> {
        let cache = &self.caches.type_conformance;
        if let Some(known) = cache.subs.borrow().get(&sub) {
            return known.clone();
        }
        let facts = self.compute_conformance_sub_facts(sub);
        cache.subs.borrow_mut().insert(sub, facts.clone());
        facts
    }

    // Cost: O(d), d = the class's MRO depth.
    fn compute_conformance_sub_facts(&self, sub: Symbol) -> Option<Ancestors> {
        let name = sub.resolve();
        let name = name.as_str();
        // The shapes whose distance `type_hierarchy_distance` computes some
        // other way: a parametric name, a RakuAST node, a builtin type's
        // narrowness chain, a role, an enum.
        if name.contains('[')
            || name.starts_with("RakuAST::")
            || crate::builtin_types::ancestry::builtin_type_narrowness_chain(name).is_some()
        {
            return None;
        }
        {
            let registry = self.registry();
            if !registry.classes.contains_key(name) || registry.roles.contains_key(name) {
                return None;
            }
        }
        if self.is_int_enum_type_name(name) {
            return None;
        }
        let mut set: FxHashSet<Symbol> = self
            .mro_readonly(name)
            .iter()
            .map(|ancestor| Symbol::intern(ancestor))
            .collect();
        if let Some(roles) = self.registry().class_composed_roles.get(name) {
            set.extend(roles.iter().map(|role| Symbol::intern(role)));
        }
        Some(Arc::new(set))
    }

    /// Whether the env leaves the type name `sup` alone, so the ranking walk
    /// treats it as the type it spells; memoised for the open scope.
    fn conformance_sup_is_plain(&self, sup: Symbol) -> bool {
        let cache = &self.caches.type_conformance;
        if let Some(&known) = cache.sups.borrow().get(&sup) {
            return known;
        }
        let plain = self.compute_conformance_sup_is_plain(sup);
        cache.sups.borrow_mut().insert(sup, plain);
        plain
    }

    // Cost: O(1) lookups, see `type_name_binding`.
    fn compute_conformance_sup_is_plain(&self, sup: Symbol) -> bool {
        let name = sup.resolve();
        let name = name.as_str();
        // The names `type_hierarchy_distance` answers before any hierarchy
        // walk, and the smileys it strips.
        if name.starts_with("RakuAST::")
            || matches!(name, "Inf" | "NaN" | "Package" | "Any" | "Mu")
            || name.ends_with(":D")
            || name.ends_with(":U")
        {
            return false;
        }
        if crate::runtime::utils::is_known_type_constraint(name) {
            return true;
        }
        // An alias, a mixin or a constant bound to the name changes how the
        // walk reads it; a binding of the name to its own type object does not.
        match self.type_name_binding(name) {
            None => true,
            Some(bound) => {
                matches!(bound.view(), ValueView::Package(p) if p.with_str(|p| p == name))
            }
        }
    }
}
