//! The place a named element store writes (ADR-14632).
//!
//! An element store (`@a[$i] = $v`, `%h<k> = $v`) resolves its target once,
//! at the start of the store. A local the running code declares, whose reads
//! already compile to `GetLocal`, is a [`StorePlace::Slot`]; anything else
//! (a free variable, an aliased target, an `our` or package variable, an
//! `EVAL`'d or symbolic name, a dynamic, an attribute) is
//! [`StorePlace::Named`].
//!
//! A `Slot` store mutates the slot and keeps the env mirror pointing at the
//! same container (§2.3); it never reads the mirror.
//! `MUTSU_STORE_PLACE_CHECK=1` compares, at every store with a `Slot` place,
//! the container the slot holds with the one the by-name resolution reaches,
//! and reports each disagreement on stderr: it names a writer that replaced
//! one copy of the binding and not the other (§2.4).
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
    // place so the body names no lookup itself. A `Slot` place reads and
    // mutates the slot; a write that replaces its container also updates the
    // env mirror, so the by-name readers that remain see it (§2.3). A `Named`
    // place answers what the by-name call it replaced answered.

    /// The store's target as stored: the slot of a `Slot` place, else
    /// `self.env().get(name)`.
    // Cost: O(1) for a `Slot` place; else O(d), d = the env tiers walked.
    pub(super) fn place_env_get(&self, place: StorePlace, name: &str) -> Option<&Value> {
        match place {
            StorePlace::Slot { slot } => self.locals.get(slot),
            StorePlace::Named if name == "self" => {
                self.frame_invocant_ref().or_else(|| self.env().get(name))
            }
            StorePlace::Named => self.env().get(name),
        }
    }

    /// Whether the store's target is bound: always for a declared slot, else
    /// `env().contains_key`.
    // Cost: O(1) for a `Slot` place; else O(d), d = the env tiers walked.
    pub(super) fn place_env_bound(&self, place: StorePlace, name: &str) -> bool {
        match place {
            StorePlace::Slot { .. } => true,
            StorePlace::Named if name == "self" => {
                self.frame_invocant_ref().is_some() || self.env().contains_key(name)
            }
            StorePlace::Named => self.env().contains_key(name),
        }
    }

    /// The store's target: the slot of a `Slot` place, else through the read
    /// precedence (`get_env_with_main_alias`: unit lexical, `our` mirror, env).
    // Cost: O(1) for a `Slot` place; else O(d), d = the env tiers walked,
    // plus the store probes.
    pub(super) fn place_get(&self, place: StorePlace, name: &str) -> Option<Value> {
        match place {
            StorePlace::Slot { slot } => self.locals.get(slot).cloned(),
            StorePlace::Named => self.get_env_with_main_alias(name),
        }
    }

    /// The store's target, mutably: the slot of a `Slot` place, else the
    /// running frame's env entry.
    // Cost: O(1) for a `Slot` place; else O(d), d = the env tiers walked (a
    // parent tier's entry is copied into the running one).
    pub(super) fn place_env_get_mut(
        &mut self,
        place: StorePlace,
        name: &str,
    ) -> Option<&mut Value> {
        match place {
            StorePlace::Slot { slot } => self.locals.get_mut(slot),
            StorePlace::Named if name == "self" && self.frame_invocant_ref().is_some() => {
                self.frame_invocant_mut()
            }
            StorePlace::Named => self.env_mut().get_mut(name),
        }
    }

    /// Replace the store's target with `value`, binding it in the running
    /// frame's env (and the slot of a `Slot` place).
    // Cost: O(1) expected.
    pub(super) fn place_env_insert(&mut self, place: StorePlace, name: &str, value: Value) {
        if let StorePlace::Slot { slot } = place {
            self.locals[slot] = value.clone();
        }
        self.env_mut().insert(name.to_string(), value);
    }

    /// Replace the store's target with `value` through the write precedence
    /// (`set_env_with_main_alias`), and in the slot of a `Slot` place.
    // Cost: O(d), d = the env tiers walked, plus the store probes.
    pub(super) fn place_set(&mut self, place: StorePlace, name: &str, value: Value) {
        if let StorePlace::Slot { slot } = place {
            self.locals[slot] = value.clone();
        }
        self.set_env_with_main_alias(name, value);
    }

    /// The container the store mutates, through its binding cell: the slot of
    /// a `Slot` place, else `env_root_descended_mut_tracked`'s precedence.
    /// `cell_addr` reports the outermost cell stepped through.
    // Cost: O(c) for a `Slot` place, c = cells descended; else
    // O(d + c), d = the env tiers walked.
    pub(super) fn place_root_mut_tracked(
        &mut self,
        place: StorePlace,
        name: &str,
        cell_addr: &mut Option<usize>,
    ) -> Option<&mut Value> {
        match place {
            StorePlace::Slot { slot } => {
                let root = self.locals.get_mut(slot)? as *mut Value;
                // SAFETY: `root` points into `self.locals`, which nothing else
                // borrows while the returned reference is held; a descended
                // pointer points into a cell the slot keeps alive (see
                // `descend_container_ref`).
                let descended = unsafe { Self::descend_container_ref_tracked(root, cell_addr) };
                // SAFETY: `descended` was derived from `root` just above.
                Some(unsafe { &mut *descended })
            }
            StorePlace::Named => self.env_root_descended_mut_tracked(name, cell_addr),
        }
    }

    /// [`Self::place_root_mut_tracked`] without the cell report.
    // Cost: as `place_root_mut_tracked`.
    pub(super) fn place_root_mut(&mut self, place: StorePlace, name: &str) -> Option<&mut Value> {
        self.place_root_mut_tracked(place, name, &mut None)
    }

    /// Point the env mirror of a `Slot` place's variable at the slot's value
    /// when it holds another one (§2.3): a store that rebuilt the container
    /// left the mirror on the old node. A cell in the slot is shared with the
    /// mirror already, and a name the env does not bind has no mirror.
    // Cost: O(d), d = the env tiers walked, plus the store probes on a write.
    fn mirror_slot_to_env(&mut self, slot: usize, name: &str) {
        let value = self.locals[slot].clone();
        if value.is_container_ref() {
            return;
        }
        let same = match (self.env().get(name).map(Value::view), value.view()) {
            (None, _) => return,
            (Some(ValueView::Array(a, _)), ValueView::Array(b, _)) => crate::gc::Gc::ptr_eq(&a, &b),
            (Some(ValueView::Hash(a)), ValueView::Hash(b)) => crate::gc::Gc::ptr_eq(&a, &b),
            _ => false,
        };
        if !same {
            self.set_env_with_main_alias(name, value);
        }
    }

    // The dual store's reconciliation (ADR-14632 §1.2). For a `Named` place
    // the body keeps the container in the env and copies it to and from the
    // slot around a store. For a `Slot` place the slot is the home: nothing is
    // copied into it, and after the store the env mirror is pointed at it.

    /// Before the store: when the env holds no live container for `name`
    /// (unbound, or a hoisted `Nil` / type-object placeholder), mirror the
    /// slot's value into it, so the store does not autovivify over a container
    /// that lives only in the slot (`my %s is SetHash = <a>`). Nothing for a
    /// `Slot` place, whose store works on the slot.
    // Cost: O(d), d = the env tiers walked, plus the slot search when the
    // store holds no slot (O(1) expected).
    pub(super) fn place_pull_slot(
        &mut self,
        place: StorePlace,
        code: &CompiledCode,
        slot_hint: Option<u32>,
        name: &str,
    ) {
        if let StorePlace::Slot { .. } = place {
            return;
        }
        let env_needs_slot_pull = match self.place_env_get(place, name).map(Value::view) {
            None => true,
            Some(ValueView::Nil) | Some(ValueView::Package(_)) => true,
            Some(_) => false,
        };
        if env_needs_slot_pull && let Some(slot) = self.resolve_local_slot(code, slot_hint, name) {
            let slot_val = self.locals[slot].clone();
            // Only override an existing env placeholder with a real container;
            // when env lacks the key entirely, mirror the slot as before.
            if !self.place_env_bound(place, name)
                || !matches!(slot_val.view(), ValueView::Nil | ValueView::Package(_))
            {
                self.place_set(place, name, slot_val);
            }
        }
    }

    /// After the store: copy the env's container for `name` into its slot;
    /// for a `Slot` place, point the env mirror at the slot's container.
    // Cost: O(d), d = the env tiers walked, plus the slot search when the
    // store holds no slot (O(1) expected).
    pub(super) fn place_sync_slot(
        &mut self,
        place: StorePlace,
        code: &CompiledCode,
        slot_hint: Option<u32>,
        name: &str,
    ) {
        if let StorePlace::Slot { slot } = place {
            self.mirror_slot_to_env(slot, name);
            return;
        }
        if let Some(slot) = self.resolve_local_slot(code, slot_hint, name)
            && let Some(updated) = self.place_env_get(place, name).cloned()
        {
            self.locals[slot] = updated;
        }
    }

    /// After the store: write `value` into `name`'s slot
    /// (`write_local_slot_or_name`), and for a `Slot` place the env mirror.
    // Cost: O(1) with a slot hint; else the slot search, O(1) expected.
    pub(super) fn place_write_slot(
        &mut self,
        place: StorePlace,
        code: &CompiledCode,
        slot_hint: Option<u32>,
        name: &str,
        value: Value,
    ) {
        if let StorePlace::Slot { slot } = place {
            self.locals[slot] = value;
            self.mirror_slot_to_env(slot, name);
            return;
        }
        self.write_local_slot_or_name(code, slot_hint, name, value);
    }

    /// Before a chained store mutates the env's container: drop the slot's
    /// copy of it when `release` says so, so the container's node is held
    /// once and `Gc::make_mut` mutates it in place instead of cloning it.
    /// The store's closing [`Self::place_sync_slot`] puts it back. Nothing
    /// for a `Slot` place, whose store mutates the slot's own node.
    // Cost: O(1) expected (the slot search).
    pub(super) fn place_release_slot_copy(
        &mut self,
        place: StorePlace,
        code: &CompiledCode,
        name: &str,
        release: impl Fn(&Value) -> bool,
    ) {
        if let StorePlace::Slot { .. } = place {
            return;
        }
        if let Some(slot) = self.find_local_slot(code, name)
            && release(&self.locals[slot])
        {
            self.locals[slot] = Value::NIL;
        }
    }

    /// The `IndexAssignExprNamed` / `IndexAssignLocal` store: `target_slot` is
    /// the local the compiler resolved the target to, `None` for a name it did
    /// not. Stack: `[value, index]` (or `[index, value]` with `index_first`,
    /// `[index, element, rhs]` with `concat_append`) → `[result]`.
    // Cost: O(1) for a single index/key; O(k) for a slice (see
    // exec_index_assign_expr_named_op). With `concat_append`, plus the append:
    // amortized O(m) in place, m = chars of the RHS, or O(n + m) when the
    // element's string is shared or a gate declines, n = chars already in the
    // element (see exec_index_concat_append_op).
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn exec_index_assign_store(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
        is_positional: bool,
        index_first: bool,
        concat_append: bool,
        element_share: bool,
    ) -> Result<(), crate::value::RuntimeError> {
        let appended_in_place = concat_append
            && self.exec_index_concat_append_op(code, name_idx, is_positional, target_slot)?;
        if appended_in_place {
            return Ok(());
        }
        if index_first && self.stack.len() >= 2 {
            let top = self.stack.len() - 1;
            self.stack.swap(top, top - 1);
        }
        let pre = self.attr_elem_env_snapshot(code, name_idx);
        let depth = self.stack.len();
        if let Err(err) = self.exec_index_assign_expr_named_op(
            code,
            name_idx,
            is_positional,
            target_slot,
            element_share,
        ) {
            // A refused store a handler may resume (#14153) leaves the stack
            // as before the op's two operands.
            if err.element_store_resume().is_some() {
                self.stack.truncate(depth.saturating_sub(2));
            }
            return Err(err);
        }
        self.mirror_attr_elem_env_to_cell(code, name_idx, pre);
        Ok(())
    }
}
