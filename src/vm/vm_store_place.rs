//! The place a named element store writes (ADR-14632).
//!
//! An element store (`@a[$i] = $v`, `%h<k> = $v`) resolves its target once,
//! at the start of the store. A local the running code declares, whose reads
//! already compile to `GetLocal`, is a [`StorePlace::Slot`]; anything else
//! (a free variable, an aliased target, an `our` or package variable, an
//! `EVAL`'d or symbolic name, a dynamic, an attribute) is
//! [`StorePlace::Named`].
//!
//! The store body still resolves a `Slot` place's container by name. Before
//! that flips, `MUTSU_STORE_PLACE_CHECK=1` compares, at every store with a
//! `Slot` place, the container the slot holds with the one the by-name
//! resolution reaches, and reports each disagreement on stderr: it names a
//! writer that replaced one copy of the binding and not the other (§2.4).
//! `MUTSU_STORE_PLACE_CHECK=all` also reports every store that agrees.
//! The check covers the stores that reach these bodies; the element-store
//! fast lanes (`vm_var_assign_element_fast`, `_hash_fast`, `_nested_fast`)
//! answer most plain stores before them.

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

/// Where a named element store finds its container (ADR-14632 §2.1).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum StorePlace {
    /// A local the running code declares; `slot` indexes `locals`.
    Slot { slot: usize },
    /// A target the store resolves by name.
    Named,
}

/// What `MUTSU_STORE_PLACE_CHECK` asks for: `0` nothing (unset or `0`), `1`
/// the disagreements, `2` every `Slot` store (`all`), so a run can show the
/// check was reached at all.
// Cost: O(1) after the first call.
fn store_place_check_level() -> u8 {
    static LEVEL: std::sync::OnceLock<u8> = std::sync::OnceLock::new();
    *LEVEL.get_or_init(
        || match std::env::var("MUTSU_STORE_PLACE_CHECK").as_deref() {
            Err(_) | Ok("0") | Ok("") => 0,
            Ok("all") => 2,
            Ok(_) => 1,
        },
    )
}

/// How the slot's container and the by-name container compare.
// Cost: O(1).
fn agreement(slot: &Value, named: Option<&Value>) -> Option<&'static str> {
    let Some(named) = named else {
        return Some("name-unbound");
    };
    match (slot.view(), named.view()) {
        (ValueView::Array(a, _), ValueView::Array(b, _)) => {
            (!crate::gc::Gc::ptr_eq(&a, &b)).then_some("array-node-differs")
        }
        (ValueView::Hash(a), ValueView::Hash(b)) => {
            (!crate::gc::Gc::ptr_eq(&a, &b)).then_some("hash-node-differs")
        }
        (ValueView::Array(..) | ValueView::Hash(_), _)
        | (_, ValueView::Array(..) | ValueView::Hash(_)) => Some("kind-differs"),
        _ if slot.same_variant(named) => None,
        _ => Some("value-differs"),
    }
}

/// The representation a check report names: the variant for the aggregate
/// shapes a store distinguishes, the type name otherwise.
// Cost: O(1).
fn shape(value: &Value) -> &'static str {
    match value.view() {
        ValueView::Array(_, kind) if kind.is_lazy() => "Array(lazy)",
        ValueView::Array(..) => "Array",
        ValueView::Hash(_) => "Hash",
        ValueView::LazyList(_) => "LazyList",
        ValueView::Seq(_) => "Seq",
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => "Range",
        _ => crate::runtime::utils::value_type_name(value),
    }
}

impl Interpreter {
    /// The place a named element store into `name` targets: its declaring
    /// slot when the running code declares it, its reads go through that slot,
    /// no sigilless alias redirects it and no cross-thread lane answers its
    /// read; else the name (ADR-14632 §2.1).
    // Cost: O(1).
    pub(super) fn store_place(
        &self,
        code: &CompiledCode,
        target_slot: Option<u32>,
        aliased: bool,
        name: &str,
    ) -> StorePlace {
        let Some((_, slot)) = self.store_decl(code, target_slot, aliased) else {
            return StorePlace::Named;
        };
        let lane_may_answer = self.is_thread_clone()
            || (name.starts_with(['@', '%'])
                && crate::runtime::shared_store::atomic_lane_entries_exist());
        if lane_may_answer || !crate::env::is_plain_user_lexical(name) {
            return StorePlace::Named;
        }
        StorePlace::Slot { slot }
    }

    /// Under `MUTSU_STORE_PLACE_CHECK`, report a `Slot` place whose slot does
    /// not hold the container the by-name resolution reaches for `name`
    /// (ADR-14632 §2.4). Inert when the variable is unset.
    // Cost: O(1) when unset; else the by-name read, O(d), d = the env tiers
    // walked.
    pub(super) fn check_store_place(&self, place: StorePlace, name: &str) {
        let StorePlace::Slot { slot } = place else {
            return;
        };
        let level = store_place_check_level();
        if level == 0 {
            return;
        }
        let in_slot = self.locals[slot].deref_container();
        let named = self
            .get_env_with_main_alias(name)
            .map(|v| v.deref_container());
        let kind = match agreement(&in_slot, named.as_ref()) {
            Some(kind) => kind,
            None if level == 2 => "agrees",
            None => return,
        };
        {
            eprintln!(
                "store-place-check: {kind} {name} slot={slot} line={} slot_value={} named_value={}",
                self.cur_source_line,
                shape(&in_slot),
                named.as_ref().map_or("-", shape),
            );
        }
    }

    // The store body's container accesses (ADR-14632 §2.2). Each one takes the
    // place so the body names no lookup itself; a `Slot` place still resolves
    // by name until §4's flip, so every method answers exactly what the
    // by-name call it replaces answered.

    /// The env binding of the store's target, as `self.env().get(name)`.
    // Cost: O(d), d = the env tiers walked.
    pub(super) fn place_env_get(&self, _place: StorePlace, name: &str) -> Option<&Value> {
        self.env().get(name)
    }

    /// Whether the env binds the store's target, as `env().contains_key`.
    // Cost: O(d), d = the env tiers walked.
    pub(super) fn place_env_bound(&self, _place: StorePlace, name: &str) -> bool {
        self.env().contains_key(name)
    }

    /// The store's target through the read precedence
    /// (`get_env_with_main_alias`: unit lexical, `our` mirror, env).
    // Cost: O(d), d = the env tiers walked, plus the store probes.
    pub(super) fn place_get(&self, _place: StorePlace, name: &str) -> Option<Value> {
        self.get_env_with_main_alias(name)
    }

    /// The store's target, mutably, in the running frame's env.
    // Cost: O(d), d = the env tiers walked (a parent tier's entry is copied
    // into the running one).
    pub(super) fn place_env_get_mut(
        &mut self,
        _place: StorePlace,
        name: &str,
    ) -> Option<&mut Value> {
        self.env_mut().get_mut(name)
    }

    /// Bind the store's target in the running frame's env.
    // Cost: O(1) expected.
    pub(super) fn place_env_insert(&mut self, _place: StorePlace, name: &str, value: Value) {
        self.env_mut().insert(name.to_string(), value);
    }

    /// Write the store's target through the write precedence
    /// (`set_env_with_main_alias`).
    // Cost: O(d), d = the env tiers walked, plus the store probes.
    pub(super) fn place_set(&mut self, _place: StorePlace, name: &str, value: Value) {
        self.set_env_with_main_alias(name, value);
    }

    /// The container the store mutates, through its binding cell
    /// (`env_root_descended_mut_tracked`); `cell_addr` reports the outermost
    /// cell stepped through.
    // Cost: O(d + c), d = the env tiers walked, c = cells descended.
    pub(super) fn place_root_mut_tracked(
        &mut self,
        _place: StorePlace,
        name: &str,
        cell_addr: &mut Option<usize>,
    ) -> Option<&mut Value> {
        self.env_root_descended_mut_tracked(name, cell_addr)
    }

    /// [`Self::place_root_mut_tracked`] without the cell report.
    // Cost: as `place_root_mut_tracked`.
    pub(super) fn place_root_mut(&mut self, place: StorePlace, name: &str) -> Option<&mut Value> {
        self.place_root_mut_tracked(place, name, &mut None)
    }
}
