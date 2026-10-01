//! The compiled engine's capture levels: one walk `CapStore` per open
//! `( … )` whose body captures, plus the pattern's own level at the bottom.
//!
//! A capture group opens a fresh level, so the captures its body takes number
//! from zero and become the group's sub-Match when it closes — the walk's
//! `capture_group_delta` over an inner walk's store. Levels are pushed and
//! popped as the program runs forward, and a backtrack can resume inside a
//! group that has already closed, so every change is journaled: a store
//! mutation records the level and its trail mark, a close keeps the popped
//! store whole. Rewinding the journal restores the stack of levels, and each
//! level's own trail restores its captures, exactly.
//!
//! A separated quantifier (`atom +% sep`) matches each atom and separator in
//! a level of its own and keeps the closed levels' captures in `collected`,
//! so that at the quantifier's end they fold side by side, as the walk's
//! chain does (`separated_capture_delta`).

use std::sync::Arc;

use super::super::regex_trail::{CapStore, Undo};
use crate::runtime::regex_types::{OuterBackrefCaps, RegexCaptures};

enum Journal {
    /// `stack[level]` was mutated from trail mark `mark`.
    Edit { level: u32, mark: usize },
    /// A level was opened.
    Opened,
    /// This level was closed.
    Closed(CapStore),
    /// A closed level's captures were appended to `collected`.
    Collected,
    /// `collected` was drained from `at`; these were the entries.
    Drained {
        at: usize,
        entries: Vec<(bool, RegexCaptures)>,
    },
}

#[derive(Default)]
pub(super) struct Levels {
    stack: Vec<CapStore>,
    journal: Vec<Journal>,
    /// A separated quantifier's per-iteration captures, in match order:
    /// `(is_separator, captures)` (see `collect`).
    collected: Vec<(bool, RegexCaptures)>,
    /// Undo trails of levels that closed for good (`close_forget`), reused by
    /// the next level opened: a grammar opens one level per subrule call, and
    /// each one's first capture allocated a trail (ADR-10488).
    spare_trails: Vec<Vec<Undo>>,
}

/// Spare trails kept past this many are dropped, so one deep parse does not
/// pin its trails for the rest of the run.
const SPARE_TRAILS: usize = 64;

impl Levels {
    /// Reset to a single, empty pattern level whose match starts at `from`.
    // Cost: O(l + j), l = levels, j = journal entries left from the last run.
    pub(super) fn reset(&mut self, from: usize) {
        self.stack.truncate(1);
        self.journal.clear();
        self.collected.clear();
        match self.stack.first_mut() {
            Some(store) => store.reset_empty(from),
            None => self.stack.push(CapStore::new(RegexCaptures {
                match_from: from,
                ..Default::default()
            })),
        }
    }

    /// The innermost open level.
    #[inline]
    pub(super) fn top(&self) -> &CapStore {
        self.stack
            .last()
            .expect("the pattern level is never closed")
    }

    /// A choice point's journal position.
    #[inline]
    pub(super) fn mark(&self) -> usize {
        self.journal.len()
    }

    /// Mutate the innermost level, journaling the mutation.
    // Cost: O(1) beyond `f` itself.
    #[inline]
    pub(super) fn edit<R>(&mut self, f: impl FnOnce(&mut CapStore) -> R) -> R {
        let level = self.stack.len() - 1;
        let store = &mut self.stack[level];
        let mark = store.mark();
        let r = f(store);
        if store.mark() != mark {
            self.journal.push(Journal::Edit {
                level: level as u32,
                mark,
            });
        }
        r
    }

    /// Open a capture group's level, its match starting at `from`. A level that
    /// is part of the same regex (`inherit`) reads the `:my` lexicals declared
    /// so far, as the walk's inline sub-patterns do through the vars seed; a
    /// capture-isolated group is a regex of its own and starts with none.
    // Cost: O(1) (the lexicals are shared, not copied).
    pub(super) fn open(&mut self, from: usize, inherit: bool) {
        let mut caps = RegexCaptures {
            match_from: from,
            ..Default::default()
        };
        if inherit {
            let vars = self.top().caps().regex_vars_shared().cloned();
            caps.set_regex_vars_shared(vars);
        }
        let store = match self.spare_trails.pop() {
            Some(trail) => CapStore::with_trail(caps, trail),
            None => CapStore::new(caps),
        };
        self.stack.push(store);
        self.journal.push(Journal::Opened);
    }

    /// Open an inline level (`inline_level_caps`): one that is part of the
    /// enclosing level's regex, so code inside it reads the enclosing view
    /// (and `extra`, with the level's own captures folded in by `fold`).
    // Cost: O(c), c = the captures visible to the enclosing level plus the
    // folded iterations (one flattened copy, as the walk's seed takes).
    pub(super) fn open_inline(
        &mut self,
        extra: Option<RegexCaptures>,
        fold: Option<(usize, usize)>,
    ) {
        let caps = inline_level_caps(self.top().caps(), extra, fold);
        self.stack.push(CapStore::new(caps));
        self.journal.push(Journal::Opened);
    }

    /// Start the pattern level from `caps` (an inline level's, as
    /// `inline_level_caps` builds it) instead of empty: a nested run that is
    /// part of the enclosing regex (a `&` conjunction's other branches).
    // Cost: O(1).
    pub(super) fn seed(&mut self, caps: RegexCaptures) {
        self.stack[0] = CapStore::new(caps);
    }

    /// Close the innermost level and hand back its captures.
    // Cost: O(c), c = the level's captures (one snapshot; the store itself is
    // kept for a backtrack into the group).
    pub(super) fn close(&mut self) -> RegexCaptures {
        let store = self.stack.pop().expect("an open capture level");
        let mut caps = store.snapshot();
        // An inline level's view of the enclosing level is read-only and never
        // travels out with its captures.
        caps.set_outer_backref(None);
        self.journal.push(Journal::Closed(store));
        caps
    }

    /// [`Self::close`] for a level nothing can resume in: the journal back to
    /// `from` (where it stood when the level opened) is forgotten along with the
    /// level's own store, so the caller's `Edit` is the only entry it leaves.
    // Cost: O(1) for the captures (moved out), plus the entries dropped (each
    // once).
    pub(super) fn close_forget(&mut self, from: usize) -> RegexCaptures {
        let Some((caps, trail)) = self.stack.pop().map(CapStore::into_parts) else {
            self.journal.truncate(from);
            return RegexCaptures::default();
        };
        self.journal.truncate(from);
        if trail.capacity() > 0 && self.spare_trails.len() < SPARE_TRAILS {
            let mut trail = trail;
            trail.clear();
            self.spare_trails.push(trail);
        }
        caps
    }

    /// Forget the whole journal: with no choice point left, nothing can rewind.
    // Cost: O(j), j = the entries dropped (each once).
    pub(super) fn clear_journal(&mut self) {
        self.journal.clear();
    }

    /// Close the innermost level and drop its captures (a capture-isolated
    /// group's: `<$rx>` is a match of its own that the caller never sees).
    // Cost: O(1) (the store itself is kept for a backtrack into the group).
    pub(super) fn discard(&mut self) {
        if let Some(store) = self.stack.pop() {
            self.journal.push(Journal::Closed(store));
        }
    }

    /// Close the innermost level and keep its captures as one iteration of a
    /// separated quantifier: an atom's, or (`sep`) a separator's.
    // Cost: O(c), as `close`.
    pub(super) fn collect(&mut self, sep: bool) {
        let caps = self.close();
        self.collected.push((sep, caps));
        self.journal.push(Journal::Collected);
    }

    /// How many iterations are collected (a separated quantifier's base).
    #[inline]
    pub(super) fn collected_len(&self) -> usize {
        self.collected.len()
    }

    /// The iterations collected since `at`, left in place.
    #[inline]
    pub(super) fn collected_since(&self, at: usize) -> &[(bool, RegexCaptures)] {
        &self.collected[at.min(self.collected.len())..]
    }

    /// Take the iterations collected since `at`.
    // Cost: O(k), k = the entries taken (each cloned once for the journal).
    pub(super) fn drain_collected(&mut self, at: usize) -> Vec<(bool, RegexCaptures)> {
        let entries = self.collected.split_off(at.min(self.collected.len()));
        self.journal.push(Journal::Drained {
            at,
            entries: entries.clone(),
        });
        entries
    }

    /// Undo everything journaled since `mark`.
    // Cost: O(k), k = the journaled changes undone (each undone once).
    pub(super) fn rewind(&mut self, mark: usize) {
        while self.journal.len() > mark {
            match self.journal.pop().expect("journal entry") {
                Journal::Edit { level, mark } => self.stack[level as usize].rewind(mark),
                Journal::Opened => {
                    self.stack.pop();
                }
                Journal::Closed(store) => self.stack.push(store),
                Journal::Collected => {
                    self.collected.pop();
                }
                Journal::Drained { at, entries } => {
                    self.collected.truncate(at);
                    self.collected.extend(entries);
                }
            }
        }
    }
}

/// The captures an inline level starts with. Inline code reads them through
/// `RegexCaptures::inline_capture_view`, as it reads a walk sub-pattern's
/// outer-captures seed: the enclosing level's whole view (its captures and
/// whatever it sees itself), then `extra` (a separated quantifier's iterations
/// folded so far, or the captures of a `&` conjunction's earlier branches).
/// With `fold = (offset, stride)`, the level's own first `stride` positional
/// captures fold into the slots `extra` adds from `offset` on
/// (`merge_positional`), as one more iteration: an atom's at offset 0, a
/// separator's after the atom's slots. `extra`'s slots are the enclosing
/// level's own captures from there on, so when the enclosing level is itself
/// an iteration of an outer separated quantifier they fold into the outer
/// iteration's slots instead of sitting after them (rakudo has one slot for a
/// capture group under nested quantifiers). The level shares the enclosing
/// `:my` lexicals and match start: `$/` in its code spans from where the
/// enclosing regex's match began.
// Cost: O(c), c = the captures visible to `enclosing` plus `extra`'s.
pub(super) fn inline_level_caps(
    enclosing: &RegexCaptures,
    extra: Option<RegexCaptures>,
    fold: Option<(usize, usize)>,
) -> RegexCaptures {
    let mut view = enclosing.inline_capture_view();
    let own = enclosing.positional.len();
    let place = enclosing.inline_view_fold();
    let merge_positional = fold.map(|(offset, stride)| (place.slot(own + offset), stride));
    if let Some(mut extra) = extra {
        let slots = std::mem::take(&mut extra.positional);
        let mut store = CapStore::new(view);
        store.merge_delta(extra);
        view = store.into_caps();
        for (j, slot) in slots.iter().enumerate() {
            place.place(&mut view.positional, own + j, slot);
        }
    }
    let outer = OuterBackrefCaps {
        named: view.named,
        positional: view.positional,
        parent: None,
        merge_positional,
        match_from: enclosing.match_from,
    };
    let mut caps = RegexCaptures {
        match_from: enclosing.match_from,
        ..Default::default()
    };
    caps.set_regex_vars_shared(enclosing.regex_vars_shared().cloned());
    caps.set_outer_backref(Some(Arc::new(outer)));
    caps
}
