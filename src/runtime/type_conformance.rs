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
//! One table of per-name facts, with two lifetimes, both exact:
//!
//! - **Registry facts, valid while
//!   [`Registry::type_stamp`](crate::runtime::registry::Registry::type_stamp)
//!   is unchanged.** For a registered plain class the ancestor set (MRO plus
//!   composed roles). The stamp changes on every write to the registry (its
//!   `DerefMut` is the only path to a `&mut Registry`), so a set cannot outlive
//!   a change to a class, role, enum or subset.
//! - **Env facts, valid inside one [`ConformanceScope`].** A scope wraps a
//!   ranking, during which no user code runs, so the env is fixed. A `sub` must
//!   not be an enum type in the env; a `sup` must be left alone by the env (no
//!   alias, constant or mixin bound to its name). Each flag carries the serial
//!   of the scope that computed it, so a new scope recomputes it on first use.
//!
//! When both names are plain, `sub` conforms to `sup` exactly when `sup` is in
//! `sub`'s set -- what `type_hierarchy_distance` answers for a type object of
//! such a class. Every other pair takes the general walk. A hit is two table
//! reads (indexed by symbol id) and one set probe; nothing is stored per pair.
//!
//! Debug builds recompute every fast answer the general way and assert the two
//! agree, so a case the facts get wrong fails the `t/` suite.

use super::*;
use std::cell::{Cell, RefCell};
use std::sync::Arc;

/// The ancestors a plain registered class conforms to: its MRO and the roles it
/// composes.
type Ancestors = Arc<[u32]>;

/// What is known about one type name. `ancestors` depends on the registry only
/// and lives as long as `ConformanceCache::stamp`; the two `*_ok` flags read
/// the lexical env and are valid for the scope serial recorded beside them.
#[derive(Default)]
struct NameFacts {
    /// `None`: not computed yet. `Some(None)`: not a plain registered class.
    ancestors: Option<Option<Ancestors>>,
    /// The env does not make this name an enum type (it may be the `sub`).
    sub_ok: bool,
    sub_serial: u32,
    /// The env leaves this name alone (it may be the `sup`).
    sup_ok: bool,
    sup_serial: u32,
}

#[derive(Default)]
pub(crate) struct ConformanceCache {
    /// The registry stamp `names` was filled under.
    stamp: Cell<u64>,
    /// Open [`ConformanceScope`]s; the env flags are valid while it is > 0.
    scope_depth: Cell<u32>,
    /// Identifies the outermost open scope; `serial 0` is never a live scope.
    scope_serial: Cell<u32>,
    names: RefCell<NameTable>,
}

/// `NameFacts` by symbol id, without hashing: `slots[id]` is `(epoch, index
/// into facts + 1)`, live only while `epoch` is the table's current one, so
/// dropping every fact is a counter bump.
const MAX_SLOTS: usize = 1 << 20;

#[derive(Default)]
struct NameTable {
    epoch: u32,
    slots: Vec<(u32, u32)>,
    facts: Vec<NameFacts>,
}

impl NameTable {
    fn clear(&mut self) {
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == 0 {
            self.slots.clear();
            self.epoch = 1;
        }
        self.facts.clear();
    }

    // Cost: O(1) amortised; the slot vector grows to the largest symbol id seen.
    #[inline(always)]
    fn get_or_default(&mut self, sym: Symbol) -> Option<&mut NameFacts> {
        let id = sym.raw() as usize;
        if let Some(&(epoch, at)) = self.slots.get(id)
            && epoch == self.epoch
            && at != 0
        {
            return Some(&mut self.facts[at as usize - 1]);
        }
        self.insert_default(sym)
    }

    #[cold]
    fn insert_default(&mut self, sym: Symbol) -> Option<&mut NameFacts> {
        let id = sym.raw() as usize;
        // A name interned this late is not a declared type of a ranked
        // candidate in any realistic program; it takes the general walk
        // rather than growing the table without bound.
        if id >= MAX_SLOTS {
            return None;
        }
        if id >= self.slots.len() {
            self.slots.resize(id + 1, (0, 0));
        }
        let (epoch, at) = self.slots[id];
        if epoch == self.epoch && at != 0 {
            return Some(&mut self.facts[at as usize - 1]);
        }
        self.facts.push(NameFacts::default());
        self.slots[id] = (self.epoch, self.facts.len() as u32);
        self.facts.last_mut()
    }
}

/// While alive, per-name env facts are memoised; see the module docs. Opened
/// around a ranking, never around code that can run user code.
pub(crate) struct ConformanceScope<'a> {
    cache: &'a ConformanceCache,
}

impl Drop for ConformanceScope<'_> {
    fn drop(&mut self) {
        self.cache.scope_depth.set(self.cache.scope_depth.get() - 1);
    }
}

impl Interpreter {
    /// Open a scope for the rankings that follow.
    // Cost: O(1).
    pub(crate) fn conformance_scope(&self) -> ConformanceScope<'_> {
        let cache = &self.caches.type_conformance;
        if cache.scope_depth.get() == 0 {
            let mut serial = cache.scope_serial.get().wrapping_add(1);
            if serial == 0 {
                serial = 1;
                cache.names.borrow_mut().clear();
            }
            cache.scope_serial.set(serial);
            let stamp = self.registry().type_stamp();
            if cache.stamp.get() != stamp {
                cache.names.borrow_mut().clear();
                cache.stamp.set(stamp);
            }
        }
        cache.scope_depth.set(cache.scope_depth.get() + 1);
        ConformanceScope { cache }
    }

    /// The answer of [`Self::nominal_type_conforms`] for two plain names, or
    /// `None` when either is not (or no scope is open): the caller then takes
    /// the general walk.
    // Cost: O(1) per name already seen in this scope; O(d) the first time a
    // name is seen under a registry stamp, d = the class's MRO depth.
    pub(super) fn conformance_fast(&self, sub: Symbol, sup: Symbol) -> Option<bool> {
        let cache = &self.caches.type_conformance;
        if cache.scope_depth.get() == 0 {
            return None;
        }
        let serial = cache.scope_serial.get();
        // Nothing below runs user code or re-enters this cache.
        let mut names = cache.names.borrow_mut();
        let sup_facts = names.get_or_default(sup)?;
        if sup_facts.sup_serial != serial {
            sup_facts.sup_ok = self.compute_conformance_sup_is_plain(sup);
            sup_facts.sup_serial = serial;
        }
        if !sup_facts.sup_ok {
            return None;
        }
        let sub_facts = names.get_or_default(sub)?;
        if sub_facts.sub_serial != serial {
            sub_facts.sub_ok = !self.is_int_enum_type_name(&sub.resolve());
            sub_facts.sub_serial = serial;
        }
        if !sub_facts.sub_ok {
            return None;
        }
        let ancestors = sub_facts
            .ancestors
            .get_or_insert_with(|| self.compute_conformance_ancestors(sub));
        Some(ancestors.as_ref()?.binary_search(&sup.raw()).is_ok())
    }

    /// `sub`'s ancestor set when it is a plain registered class. Reads the
    /// registry only, so it lasts as long as the registry stamp.
    // Cost: O(d), d = the class's MRO depth.
    fn compute_conformance_ancestors(&self, sub: Symbol) -> Option<Ancestors> {
        let name = sub.resolve();
        let name = name.as_str();
        // The shapes whose distance `type_hierarchy_distance` computes some
        // other way: a parametric name, a RakuAST node, a builtin type's
        // narrowness chain, a role.
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
        let mut ids: Vec<u32> = self
            .mro_readonly(name)
            .iter()
            .map(|ancestor| Symbol::intern(ancestor).raw())
            .collect();
        if let Some(roles) = self.registry().class_composed_roles.get(name) {
            ids.extend(roles.iter().map(|role| Symbol::intern(role).raw()));
        }
        // Sorted for the binary search; an MRO is a handful of names.
        ids.sort_unstable();
        ids.dedup();
        Some(Arc::from(ids))
    }

    /// Whether the env leaves the type name `sup` alone, so the ranking walk
    /// treats it as the type it spells.
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
