//! The match state's owner: by-name lookups of `$/` and its capture views
//! that stop at the routine owning them (ADR-12529 phase 3, the `$/` family).
//!
//! In Raku every routine declares its own `$/` (and with it `$0`, `$<name>`,
//! which are views into it), initially `Nil`; a block has none of its own and
//! uses the `$/` of the routine it is lexically inside. mutsu keeps the match
//! state as env names (`/`, `0`, `<name>`) written into whatever frame runs
//! the match, and a frame's env is chained over its caller's. A routine's
//! writes were already kept out of its caller (the return merge skips
//! [`crate::symbol::flags::ROUTINE_SCOPED_IMPLICIT`] names), but a read that
//! missed the routine's own frame walked on into the caller frames and
//! answered with the caller's match (#12398): `m:c` continued from the
//! caller's position and `$/` started as the caller's match.
//!
//! A routine's frame root is marked [`Env::mark_owns_match_state`], and a
//! lookup of a match-state name ([`Symbol::is_match_state`]) does not pass
//! it: the routine has not matched yet, so the answer is "unset" (`Nil`).
//! A block's frame root -- a closure called from somewhere else, whose
//! capture (`Env::fallback`) is its lexical scope -- answers `$/` from that
//! capture instead of from its caller, so a block passed to another routine
//! reads the `$/` of the scope it was written in. Its capture views go on to
//! the caller, where a routine running the block per match (`.subst`,
//! `.trans`) publishes them, and stop at the owning routine like any other
//! read; a miss there derives them from `$/`. The block's *writes* reach that scope
//! through the shared cell its creation boxed `$/` into
//! (`Interpreter::capture_block_match_var`), as before.
//!
//! The marking costs nothing per lookup on any other chain: the walk is
//! selected by [`Env::chain_has_match_owner`], a latch like
//! `chain_has_static_link`, and only for the handful of match-state symbols.

use super::Env;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The `$/` a [`Env::publish_match_var`] replaced: the shared cell it stored
/// through and the value the cell held, or the binding it shadowed.
pub(crate) enum SavedMatchVar {
    Cell(crate::gc::Gc<crate::value::ContainerCell>, Value),
    Binding(Option<Value>),
}

impl Env {
    /// Mark this frame root as a routine's, owning the match state: a lookup
    /// of `$/` or a capture view that misses the frame stops here.
    // Cost: O(1).
    #[inline]
    pub(crate) fn mark_owns_match_state(&mut self) {
        debug_assert!(self.frame_root, "only a frame root owns the match state");
        self.owns_match_state = true;
        self.chain_has_match_owner = true;
    }

    /// Whether this env is a frame root marked by
    /// [`Self::mark_owns_match_state`].
    // Cost: O(1).
    #[inline]
    pub(crate) fn owns_match_state(&self) -> bool {
        self.owns_match_state
    }

    /// Whether a lookup of `key` takes [`Self::get_match_state_sym`].
    // Cost: O(1).
    #[inline(always)]
    pub(super) fn takes_match_state_walk(&self, key: Symbol) -> bool {
        self.chain_has_match_owner && key.is_match_state()
    }

    /// Look up a match-state name (`/`, `0`, `<name>`) as the running code
    /// sees it: the innermost tier that binds it, but never past the frame
    /// root of the routine that owns it; and `$/` never past a block's frame
    /// root into its caller -- a block's `$/` is its lexical scope's, which
    /// its capture holds.
    // Cost: O(d), d = tiers between the lookup and the owning frame root.
    pub(crate) fn get_match_state_sym(&self, key: Symbol) -> Option<&Value> {
        let mut cur = self;
        loop {
            if let Some(v) = cur.inner.get(&key) {
                return Some(v);
            }
            if cur.is_tombstoned(key) {
                return None;
            }
            if cur.frame_root {
                if cur.owns_match_state {
                    return None;
                }
                // A block's `$/` is its lexical scope's, which its capture
                // holds. Its capture views (`$0`, `$<name>`) go on to the
                // caller: a routine that runs the block for a match (`.subst`,
                // `.trans`) publishes them there, and past the owning routine
                // the read derives them from `$/` instead.
                if let Some(fb) = &cur.fallback
                    && key == crate::symbol::wk::match_var()
                {
                    return fb.get(&key);
                }
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        // No owning frame on the way: the program scope's own tiers, which
        // the ordinary walk answers from (base tier and flat-capture layers
        // included).
        self.get_sym_walk(key)
    }

    /// Set `$/` to a match result, as the running code sees it: when the
    /// visible binding is a shared cell -- the one a block that touches the
    /// match state shares with the scope it was written in -- store through
    /// it, so the scope's `$/` and every block sharing it see the result;
    /// otherwise bind it in this frame. Binding a `$/` (a parameter, `my $/`)
    /// is not this: it always rebinds.
    // Cost: O(d), d = tiers between the write and the owning frame root.
    pub(crate) fn assign_match_var(&mut self, value: Value) {
        let key = crate::symbol::wk::match_var();
        // The common case, a frame matching again: its own tier already binds
        // a plain `$/`, which nothing below can shadow.
        if self.inner.get(&key).is_some_and(|v| !v.is_container_ref()) {
            self.insert_sym(key, value);
            return;
        }
        // `is_container_ref` first: viewing the previous `$/` would force a
        // lazy Match the program never looked at.
        if let Some(bound) = self.get_match_state_sym(key)
            && bound.is_container_ref()
            && let ValueView::ContainerRef(cell) = bound.view()
        {
            Value::store_through_cell(&cell, &value.deref_container());
            return;
        }
        self.insert_sym(key, value);
    }

    /// Publish `value` as `$/` for a block the running builtin is about to call
    /// per match (`.subst`, `.trans`), the way [`Self::assign_match_var`]
    /// sets it, and return what [`Self::restore_match_var`] needs to put the
    /// previous `$/` back afterwards.
    // Cost: O(d), as `assign_match_var`.
    pub(crate) fn publish_match_var(&mut self, value: Value) -> SavedMatchVar {
        let key = crate::symbol::wk::match_var();
        // `is_container_ref` first: viewing the previous `$/` would force a
        // lazy Match the program never looked at.
        if let Some(bound) = self.get_match_state_sym(key)
            && bound.is_container_ref()
            && let ValueView::ContainerRef(cell) = bound.view()
        {
            let old = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
            Value::store_through_cell(&cell, &value.deref_container());
            return SavedMatchVar::Cell(cell.clone(), old);
        }
        let old = self.get_sym(key).cloned();
        self.insert_sym(key, value);
        SavedMatchVar::Binding(old)
    }

    /// Undo a [`Self::publish_match_var`].
    // Cost: O(1).
    pub(crate) fn restore_match_var(&mut self, saved: SavedMatchVar) {
        let key = crate::symbol::wk::match_var();
        match saved {
            SavedMatchVar::Cell(cell, old) => Value::store_through_cell(&cell, &old),
            SavedMatchVar::Binding(Some(old)) => {
                self.insert_sym(key, old);
            }
            SavedMatchVar::Binding(None) => {
                self.remove_sym(key);
            }
        }
    }

    /// Make `flat`, a flattening of this chain, answer every match-state name
    /// as a lookup on the chain does. A flattening merges every tier's names,
    /// the callers' included, so without this a routine frame replaced by its
    /// flattening (a method call flattens the running frame) would read its
    /// caller's `$/` from then on.
    // Cost: O(c * d), c = match-state names bound in `flat` (`/` plus the
    // capture-shaped symbols, a hash probe each), d = chain tiers; nothing
    // unless the chain has an owning frame.
    pub(super) fn settle_match_state(&self, flat: &mut Env) {
        if !self.chain_has_match_owner {
            return;
        }
        let mut fixes: Vec<(Symbol, Option<Value>)> = Vec::new();
        let mut check = |key: Symbol| {
            let Some(held) = flat.inner.get(&key) else {
                return;
            };
            match self.get_match_state_sym(key) {
                Some(live) if live.same_binding(held) => {}
                // Unset and `Nil` read the same, and leaving the entry keeps
                // a flattening that shares its map (an empty frame over a
                // flat caller) from copying it.
                None if held.is_nil() => {}
                live => fixes.push((key, live.cloned())),
            }
        };
        check(crate::symbol::wk::match_var());
        crate::symbol::for_each_capture_shaped_symbol(check);
        if fixes.is_empty() {
            return;
        }
        let tier = flat.cow_mut();
        for (key, live) in fixes {
            match live {
                Some(v) => {
                    tier.insert(key, v);
                }
                None => {
                    tier.remove(&key);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn slash() -> Symbol {
        Symbol::intern("/")
    }

    #[test]
    fn a_routine_frame_does_not_see_its_callers_match() {
        let mut caller = Env::new();
        caller.insert_sym(slash(), Value::int(1));
        let mut frame = Env::scoped_child(caller.clone());
        frame.mark_owns_match_state();
        assert!(frame.get_sym(slash()).is_none());
        assert!(!frame.contains_key_sym(slash()));
        // Its own match answers.
        frame.insert_sym(slash(), Value::int(2));
        assert_eq!(frame.get_sym(slash()).cloned(), Some(Value::int(2)));
        // A frame that is not a routine's keeps reading through.
        let other = Env::scoped_child(caller);
        assert_eq!(other.get_sym(slash()).cloned(), Some(Value::int(1)));
    }

    #[test]
    fn a_block_tier_inside_the_routine_sees_the_routines_match() {
        let mut frame = Env::scoped_child(Env::new());
        frame.mark_owns_match_state();
        frame.insert_sym(slash(), Value::int(3));
        let (block, _) = Env::open_block_tier(frame).ok().unwrap();
        assert_eq!(block.get_sym(slash()).cloned(), Some(Value::int(3)));
    }
}
