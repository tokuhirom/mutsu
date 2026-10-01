//! Deferred replay of backtracked grammar reductions.
//!
//! Rakudo fires an action the moment its rule reduces, so a reduction that a
//! later alternative supersedes (same rule and span) still runs, at its true
//! chronological position: after the surviving nodes that end before it starts
//! and before the surviving nodes that start at or after it. mutsu walks the
//! finished match tree instead, so these superseded reductions are parked here
//! and flushed from inside that walk at the matching position.

use super::methods_grammar_replay_spans::maximal_span_indices;
use super::*;
use std::cell::RefCell;
use std::sync::Arc;

type Entry = (String, Arc<CapNode>);
pub(super) type DeferredSlot = Option<(String, Vec<Entry>)>;

thread_local! {
    /// `(parse text, superseded reductions not yet dispatched)`.
    static PENDING: RefCell<Option<(String, Vec<Entry>)>> = const { RefCell::new(None) };
}

impl Interpreter {
    /// Park the superseded reductions of the parse that is about to walk its
    /// match tree.
    // Cost: O(n log n), n = entries.len().
    ///
    /// Returns the slot of an enclosing parse (an action may itself call
    /// `.parse`), to be handed back to [`Self::restore_deferred_repeats`].
    pub(super) fn defer_repeated_reduce_actions(
        &mut self,
        entries: Vec<Entry>,
        text: &str,
    ) -> DeferredSlot {
        let keep = maximal_span_indices(&entries);
        let mut slots: Vec<Option<Entry>> = entries.into_iter().map(Some).collect();
        let kept: Vec<Entry> = keep.into_iter().filter_map(|i| slots[i].take()).collect();
        let fresh = (!kept.is_empty()).then(|| (text.to_string(), kept));
        PENDING.with(|p| std::mem::replace(&mut *p.borrow_mut(), fresh))
    }

    /// Put an enclosing parse's parked reductions back after this parse's walk.
    pub(super) fn restore_deferred_repeats(&mut self, saved: DeferredSlot) {
        PENDING.with(|p| *p.borrow_mut() = saved);
    }

    /// Dispatch the parked reductions for which `ready(from, to)` holds.
    // Cost: O(p) per call when entries are pending, p = pending entries; O(1) otherwise.
    pub(super) fn flush_deferred_repeats(
        &mut self,
        actions: &mut Value,
        ready: impl Fn(usize, usize) -> bool,
    ) -> Result<(), RuntimeError> {
        let taken = PENDING.with(|p| {
            let mut slot = p.borrow_mut();
            let (text, entries) = slot.as_mut()?;
            let (now, later): (Vec<Entry>, Vec<Entry>) = std::mem::take(entries)
                .into_iter()
                .partition(|(_, caps)| ready(caps.from, caps.to));
            *entries = later;
            (!now.is_empty()).then(|| (text.clone(), now))
        });
        let Some((text, now)) = taken else {
            return Ok(());
        };
        self.replay_reduce_action_entries(now, actions, None, &text, true)
    }

    /// Dispatch everything still parked.
    pub(super) fn flush_all_deferred_repeats(
        &mut self,
        actions: &mut Value,
    ) -> Result<(), RuntimeError> {
        self.flush_deferred_repeats(actions, |_, _| true)
    }
}
