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

use super::multi_rank::{self, MultiRank};
use super::declared_index;
use super::multi_rank_tiers;
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
    /// The measured RakuAST names this name conforms to, when it is one
    /// (`None` inside: it is not). Static, like the table it comes from.
    rakuast: Option<Option<Ancestors>>,
    /// Whether this name is a RakuAST name a measured name can be compared
    /// with (no smiley), as a `sup`.
    rakuast_sup: Option<bool>,
    /// `Mu` or `Any`, whose relation to every other name is fixed.
    special: Option<Special>,
}

#[derive(Clone, Copy, PartialEq)]
enum Special {
    None,
    Mu,
    Any,
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
    /// Answers given by the registry or the env (everything but the static
    /// RakuAST / `Mu` / `Any` relations); see [`Interpreter::assign_tiers_memoised`].
    dependent_answers: Cell<u64>,
    tier_memo: RefCell<Vec<TierMemoEntry>>,
    /// `DeclaredKey` -> small id, so a candidate pair is a pair of integers.
    declared_ids: RefCell<rustc_hash::FxHashMap<multi_rank::DeclaredKey, u32>>,
    /// `multi_rank::MultiRank::declared_narrower` between two declared keys,
    /// remembered only when every relation it asked about was static (see
    /// [`Interpreter::assign_tiers_memoised`]), so it needs no invalidation.
    declared_pairs: RefCell<rustc_hash::FxHashMap<(u32, u32), bool>>,
    /// The declared-type relation of whole candidate families.
    declared_index: RefCell<super::declared_index::DeclaredIndexCache>,
}

impl ConformanceCache {
    // Cost: O(1).
    pub(super) fn note_dependent_answer(&self) {
        self.dependent_answers.set(self.dependent_answers.get() + 1);
    }
}

/// The tiers `assign_tiers` gave one candidate set, keyed by the ranks it
/// was given.
struct TierMemoEntry {
    ranks: Vec<MultiRank>,
    /// How many leading candidates' tiers `tiers` holds valid answers for.
    wanted: usize,
    tiers: Vec<(u32, bool)>,
}

/// Candidate sets below this size rank faster than they hash.
const TIER_MEMO_MIN: usize = 16;
const TIER_MEMO_CAP: usize = 16;
/// Most remembered declared-pair answers before the table is dropped (about
/// 16 bytes each).
const DECLARED_PAIR_CAP: usize = 1 << 20;

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
        // Two RakuAST names: the measured hierarchy answers, exactly as
        // `type_hierarchy_distance` does for a node or type object of the
        // `sub` class (rakudo's own `RakuAST::Deparse` ranks 267 candidates
        // by it). Not a registry fact, so independent of the stamp and scope.
        let sup_special = *names
            .get_or_default(sup)?
            .special
            .get_or_insert_with(|| Self::special_of(sup));
        let sub_special = *names
            .get_or_default(sub)?
            .special
            .get_or_insert_with(|| Self::special_of(sub));
        // `nominal_type_conforms_general`'s opening rules.
        if sup_special == Special::Mu {
            return Some(true);
        }
        if sup_special == Special::Any {
            return Some(sub_special != Special::Mu);
        }
        if sub_special != Special::None {
            return Some(false);
        }
        let sup_is_rakuast = *names
            .get_or_default(sup)?
            .rakuast_sup
            .get_or_insert_with(|| Self::is_rakuast_sup_name(sup));
        if let Some(conforms_to) = names
            .get_or_default(sub)?
            .rakuast
            .get_or_insert_with(|| Self::compute_rakuast_conformance(sub))
        {
            return sup_is_rakuast.then(|| conforms_to.contains(&sup.raw()));
        }
        cache.dependent_answers.set(cache.dependent_answers.get() + 1);
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
        let ancestors = ancestors.as_deref()?;
        let id = sup.raw();
        // An MRO is a handful of names: a scan beats a binary search.
        Some(if ancestors.len() <= 16 {
            ancestors.contains(&id)
        } else {
            ancestors.binary_search(&id).is_ok()
        })
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

    // Cost: O(1).
    fn special_of(name: Symbol) -> Special {
        name.with_str(|n| match n {
            "Mu" => Special::Mu,
            "Any" => Special::Any,
            _ => Special::None,
        })
    }

    /// [`multi_rank::assign_tiers`] over `items`, remembering the result when
    /// every relation it asked about is static (a measured RakuAST name,
    /// `Mu`, `Any`): the tiers are then a function of the ranks alone, and a
    /// later dispatch with the same ranks (the same candidates and argument
    /// types) reuses them instead of comparing every pair again. Rakudo's
    /// `RakuAST::Deparse` has 267 candidates, so the pairwise ranking cost
    /// ~20ms per node. A relation read from the registry or the env is never
    /// remembered: it can change between two dispatches.
    /// Only the tiers of `items[..wanted]` are valid afterwards (see
    /// [`multi_rank_tiers::assign_tiers_wanted`]); pass `items.len()` to read all.
    // Cost: O(n) on a hit, O(n * k) on a miss (k = candidates the tier walk
    // visits; O(n^2 * r) when it meets a cycle), n = candidates, r = one
    // `conforms` call.
    pub(crate) fn assign_tiers_memoised<T>(
        &self,
        items: &mut [T],
        wanted: usize,
        rank: impl Fn(&mut T) -> &mut MultiRank,
    ) {
        let cache = &self.caches.type_conformance;
        let _scope = self.conformance_scope();
        let conforms = |x, y| self.nominal_type_conforms(x, y);
        if items.len() < TIER_MEMO_MIN {
            multi_rank::assign_tiers(items, &rank, &conforms);
            return;
        }
        let ranks: Vec<MultiRank> = items.iter_mut().map(|item| *rank(item)).collect();
        {
            let memo = cache.tier_memo.borrow();
            if let Some(entry) = memo
                .iter()
                .find(|entry| entry.wanted >= wanted && entry.ranks == ranks)
            {
                for (item, &(tier, bind_check)) in items.iter_mut().zip(&entry.tiers) {
                    let r = rank(item);
                    r.tier = tier;
                    r.tiered = true;
                    r.tier_bind_check = bind_check;
                }
                return;
            }
        }
        let dependent_before = cache.dependent_answers.get();
        // A pair of candidates that cannot bind the call is ordered by their
        // declared types alone, which a later call with other arguments asks
        // again: `RakuAST::Deparse`'s 267 candidates are 71k such pairs per
        // call. A static answer is remembered by the two candidates'
        // declared keys; one read from the registry or the env is not.
        let ids: Vec<u32> = {
            let mut table = cache.declared_ids.borrow_mut();
            if table.len() + ranks.len() > DECLARED_PAIR_CAP {
                table.clear();
                cache.declared_pairs.borrow_mut().clear();
                cache.declared_index.borrow_mut().clear();
            }
            ranks
                .iter()
                .map(|r| {
                    let next = table.len() as u32;
                    *table.entry(r.declared_key()).or_insert(next)
                })
                .collect()
        };
        let (order, pos_of) = declared_index::canonical_order(&ids);
        let sorted_ids: Vec<u32> = order.iter().map(|&i| ids[i]).collect();
        let cached = cache.declared_index.borrow().get(&sorted_ids);
        let index = cached.unwrap_or_else(|| {
            let canonical: Vec<&MultiRank> = order.iter().map(|&i| &ranks[i]).collect();
            let before = cache.dependent_answers.get();
            let index = Arc::new(declared_index::build(&canonical, &conforms));
            if cache.dependent_answers.get() == before {
                cache.declared_index.borrow_mut().insert(sorted_ids.clone(), Arc::clone(&index));
            }
            index
        });
        let mut declared_preds = |j: usize| declared_index::preds_of(&index, &order, &pos_of, j);
        multi_rank_tiers::assign_tiers_wanted(items, wanted, &rank, &conforms, &mut |i, j, a, b| {
            let key = (ids[i], ids[j]);
            if let Some(&known) = cache.declared_pairs.borrow().get(&key) {
                return known;
            }
            let before = cache.dependent_answers.get();
            let answer = a.declared_narrower(b, &conforms);
            if cache.dependent_answers.get() == before {
                // Never cleared mid-ranking: the ids this ranking holds are
                // only meaningful against the id table it was built from.
                let mut pairs = cache.declared_pairs.borrow_mut();
                if pairs.len() < DECLARED_PAIR_CAP {
                    pairs.insert(key, answer);
                }
            }
            answer
        }, &mut declared_preds);
        if cache.dependent_answers.get() != dependent_before {
            return;
        }
        let tiers = items
            .iter_mut()
            .map(|item| {
                let r = rank(item);
                (r.tier, r.tier_bind_check)
            })
            .collect();
        let mut memo = cache.tier_memo.borrow_mut();
        if memo.len() >= TIER_MEMO_CAP {
            memo.remove(0);
        }
        memo.push(TierMemoEntry {
            ranks,
            wanted,
            tiers,
        });
    }

    /// Whether `sup` is a RakuAST name without a smiley, the shape the
    /// measured-hierarchy answer of `type_hierarchy_distance` covers.
    // Cost: O(1), a prefix and suffix test.
    fn is_rakuast_sup_name(sup: Symbol) -> bool {
        sup.with_str(|n| n.starts_with("RakuAST::") && !n.ends_with(":D") && !n.ends_with(":U"))
    }

    /// The ids of the measured RakuAST names `sub` conforms to, when `sub` is
    /// a measured name.
    // Cost: O(d + r), d = MRO depth, r = roles of `sub`.
    fn compute_rakuast_conformance(sub: Symbol) -> Option<Ancestors> {
        let names = sub.with_str(crate::rakuast::type_object_conforms_to)?;
        let mut ids: Vec<u32> = names.iter().map(|n| Symbol::intern(n).raw()).collect();
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The memoised answer for two measured RakuAST names, `Mu` and `Any`
    /// is what the general walk answers (outside a scope there is no memo).
    #[test]
    fn rakuast_and_special_names_agree_with_the_general_walk() {
        let interp = Interpreter::new();
        let names = [
            "RakuAST::IntLiteral",
            "RakuAST::Literal",
            "RakuAST::Term",
            "RakuAST::Node",
            "RakuAST::Sinkable",
            "RakuAST::Statement::Expression",
            "RakuAST::Call::BlockMethod",
            "RakuAST::Call::TermAsMethod",
            "RakuAST::IMPL",
            "RakuAST::NoSuchNode",
            "Mu",
            "Any",
            "Str",
        ];
        let general: Vec<bool> = names
            .iter()
            .flat_map(|sub| names.iter().map(move |sup| (*sub, *sup)))
            .map(|(sub, sup)| interp.nominal_type_conforms(Symbol::intern(sub), Symbol::intern(sup)))
            .collect();
        let _scope = interp.conformance_scope();
        let memoised: Vec<bool> = names
            .iter()
            .flat_map(|sub| names.iter().map(move |sup| (*sub, *sup)))
            .map(|(sub, sup)| interp.nominal_type_conforms(Symbol::intern(sub), Symbol::intern(sup)))
            .collect();
        assert_eq!(general, memoised);
        // And the relation is not trivially all-false or all-true.
        assert!(memoised.iter().any(|b| *b) && memoised.iter().any(|b| !*b));
    }
}
