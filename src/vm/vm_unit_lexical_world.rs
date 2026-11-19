//! Writes to the compunit statics store `unit_lexicals`, which is one world
//! table shared by every thread (ADR-17216 §2.5, #18136).
//!
//! A module's file-scope `my` variables live in `unit_lexicals`. While that
//! table was forked per thread at spawn, a worker started before a module
//! was loaded never saw the module's statics: Test's `ok` running on Green's
//! worker read its own `$output` as the caller's same-named variable. The
//! table is now a `WorldCell`, so no `&mut` into it may escape its write lock.
//! Two write shapes remain:
//!
//! - **Replace the binding** (`:=`, a rebind to a fresh container):
//!   [`Interpreter::unit_lexical_slot_replace`] stores the new value under
//!   the table's write lock.
//! - **Mutate in place** (element stores, `push`, `:delete`):
//!   [`Interpreter::unit_lexical_slot_mut`] hands out a root pinned in this
//!   thread's pin slot, as `our_var_pinned_root` does for `our` variables. A
//!   cell or a container is shared by the table and the pin, so the mutation
//!   lands on the node every thread reads; a plain value is boxed into a cell
//!   in the table first.

use super::*;

/// Where [`Interpreter::unit_lexical_slot_mut`]'s resolution found `name`.
enum UnitLexicalTarget {
    /// A routine-nested sub's own alias binding (`vm_lexsub_aliases`).
    Alias,
    /// An entry of the world table: its bucket and its key.
    Entry(String, String),
}

/// Whether a stored value is shared by every clone of it (a cell, or an
/// array/hash node), so a mutation through a clone reaches the table's entry.
// Cost: O(1).
fn shares_node(value: &Value) -> bool {
    value.is_container_ref() || matches!(value.view(), ValueView::Array(..) | ValueView::Hash(..))
}

impl Interpreter {
    /// The package key, along `candidate`'s chain, whose bucket holds `name`:
    /// the walk [`Self::lookup_unit_lexical_chain`] does, answering where it
    /// found the entry instead of the entry.
    // Cost: O(d), d = `::` depth of `candidate`.
    fn locate_unit_lexical_chain<'c>(
        table: &crate::runtime::PackageLexicals,
        candidate: &'c str,
        name: &str,
        owner: Option<Symbol>,
    ) -> Option<&'c str> {
        if !table.contains_name(name) {
            return None;
        }
        let owner_is_ancestor = owner.is_some_and(|owner| {
            candidate == owner.as_str()
                || crate::qualified::package_ancestors(Symbol::intern(candidate))
                    .any(|package| package == owner)
        });
        let mut package = candidate;
        loop {
            if table
                .get(package)
                .is_some_and(|entries| entries.contains_key(name))
            {
                return Some(package);
            }
            if owner.is_some_and(|owner| package == owner.as_str())
                || owner.is_some() && !owner_is_ancestor
            {
                return None;
            }
            match crate::runtime::utils::rsplit_once_double_colon(package) {
                Some((parent, _)) => package = parent,
                None => return None,
            }
        }
    }

    /// The write-side resolution of `name`, identical to
    /// [`Self::unit_lexical_slot`]'s read-side one (see its doc comment).
    // Cost: as `unit_lexical_slot`.
    fn unit_lexical_target(
        &self,
        name: &str,
        name_sym: Option<Symbol>,
    ) -> Option<UnitLexicalTarget> {
        if self.env().frame_capture_holds_name(name, name_sym) {
            return None;
        }
        if self.lexsub_alias_frame_active() && self.lexsub_alias_slot(name).is_some() {
            return Some(UnitLexicalTarget::Alias);
        }
        if self.lexicals.unit_lexicals.read().is_empty() || name.is_empty() {
            return None;
        }
        if !self.unit_lexicals_may_hold(name, name_sym) {
            return None;
        }
        // An enclosing routine's own binding shadows the file-scope one.
        if self.running_code_routine_scoped(name, name_sym) {
            return None;
        }
        // A variable the running frame binds itself shadows the compunit's
        // file-scope one of the same name: `my %seen` in a module routine
        // whose file declares `%seen` too (`seed_env_from_container_slot`).
        if self.frame_binds_own(name, name_sym) {
            return None;
        }
        let lexical_owner = self.routine_stack().last().and_then(|frame| {
            self.lexical_package_for_frame(frame.def_file)
                .or(frame.lexical_package)
        });
        let split = crate::qualified::split_qualified_var(
            name_sym.unwrap_or_else(|| crate::qualified::known_symbol(name)),
        );
        if split.is_none()
            && let Some(bucket) = self.active_unit_lexical_bucket()
            && self
                .lexicals
                .unit_lexicals
                .read()
                .get(bucket)
                .is_some_and(|m| m.contains_key(name))
        {
            // See `unit_lexical_slot`: a running loop's by-name parameter
            // shadows the captured cell (#13420).
            if self.topic_state.name_bound_by_loop_param(name) {
                return None;
            }
            crate::vm::vm_stats::record_mainline_lexical_hit();
            return Some(UnitLexicalTarget::Entry(
                bucket.to_string(),
                name.to_string(),
            ));
        }
        // Same as `unit_lexical_slot`: a package-less module file's own.
        if split.is_none()
            && let Some(unit) = self.compunit_lexical_bucket(name)
        {
            return Some(UnitLexicalTarget::Entry(
                unit.as_str().to_string(),
                name.to_string(),
            ));
        }
        let cur_sym = self.current_package_sym();
        let cur: &str = cur_sym.as_str();
        let table = self.lexicals.unit_lexicals.read();
        if let Some(split) = split {
            // Only scalars are in the store (see `collect_unit_lexical_names`),
            // keyed sigil-less.
            let (pkg, bare) = Self::unit_lexical_scalar_split(split)?;
            if pkg != cur || crate::qualified::is_global_package(cur_sym) {
                return None;
            }
            let package = Self::locate_unit_lexical_chain(&table, cur, bare, lexical_owner)?;
            return Some(UnitLexicalTarget::Entry(
                package.to_string(),
                bare.to_string(),
            ));
        }
        for candidate in self.unit_lexical_candidates(cur_sym).into_iter().flatten() {
            if let Some(package) =
                Self::locate_unit_lexical_chain(&table, candidate.as_str(), name, lexical_owner)
            {
                return Some(UnitLexicalTarget::Entry(
                    package.to_string(),
                    name.to_string(),
                ));
            }
        }
        None
    }

    /// A mutable root for `name`'s compunit-lexical entry, for the in-place
    /// mutation chokepoint (`env_root_descended_mut`): every container
    /// mutation call site (element-assign, `push`/`pop`/..., hash
    /// key-set/`:delete`) goes through here for the reason
    /// [`Self::get_env_with_main_alias`] is the read-side chokepoint: a
    /// compunit's own file-scope `@`/`%` (ADR-0039 slice 1) must never be
    /// mutated through the loading scope's same-named `env` entry.
    ///
    /// The entry is pinned in this thread's pin slot (see the module docs),
    /// so replacing the root itself does not reach the table: use
    /// [`Self::unit_lexical_slot_replace`] for that.
    // Cost: as `unit_lexical_slot`; one table write for a plain value.
    pub(crate) fn unit_lexical_slot_mut(
        &mut self,
        name: &str,
        name_sym: Option<Symbol>,
    ) -> Option<&mut Value> {
        let (bucket, key) = match self.unit_lexical_target(name, name_sym)? {
            UnitLexicalTarget::Alias => return self.lexsub_alias_slot_mut(name),
            UnitLexicalTarget::Entry(bucket, key) => (bucket, key),
        };
        let stored = self
            .lexicals
            .unit_lexicals
            .read()
            .get(&bucket)
            .and_then(|m| m.get(&key))
            .cloned()?;
        let stored = if shares_node(&stored) {
            stored
        } else {
            self.unit_lexicals_write(|table| {
                let slot = table.get_value_mut(&bucket, &key)?;
                if !shares_node(slot) {
                    *slot = slot.clone().into_container_ref();
                }
                Some(slot.clone())
            })?
        };
        self.lexicals.unit_lexical_pin = stored;
        Some(&mut self.lexicals.unit_lexical_pin)
    }

    /// Replace `name`'s compunit-lexical binding with `value` (a `:=`, or a
    /// rebind to a fresh container), resolved as
    /// [`Self::unit_lexical_slot_mut`] resolves it. `false` when `name` is not
    /// such a lexical.
    // Cost: as `unit_lexical_slot`, plus one table write.
    pub(crate) fn unit_lexical_slot_replace(
        &mut self,
        name: &str,
        name_sym: Option<Symbol>,
        value: Value,
    ) -> bool {
        match self.unit_lexical_target(name, name_sym) {
            None => false,
            Some(UnitLexicalTarget::Alias) => match self.lexsub_alias_slot_mut(name) {
                Some(slot) => {
                    *slot = value;
                    true
                }
                None => false,
            },
            Some(UnitLexicalTarget::Entry(bucket, key)) => {
                self.unit_lexicals_write(|table| match table.get_value_mut(&bucket, &key) {
                    Some(slot) => {
                        *slot = value;
                        true
                    }
                    None => false,
                })
            }
        }
    }
}
