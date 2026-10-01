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

use super::super::regex_trail::CapStore;
use crate::runtime::regex_types::RegexCaptures;

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
}

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
        self.stack.push(CapStore::new(caps));
        self.journal.push(Journal::Opened);
    }

    /// Close the innermost level and hand back its captures.
    // Cost: O(c), c = the level's captures (one snapshot; the store itself is
    // kept for a backtrack into the group).
    pub(super) fn close(&mut self) -> RegexCaptures {
        let store = self.stack.pop().expect("an open capture level");
        let caps = store.snapshot();
        self.journal.push(Journal::Closed(store));
        caps
    }

    /// Close the innermost level and drop its captures (a capture-isolated
    /// group's: `<$rx>` is a match of its own that the caller never sees).
    // Cost: O(1) (the store itself is kept for a backtrack into the group).
    pub(super) fn discard(&mut self) {
        let store = self.stack.pop().expect("an open capture level");
        self.journal.push(Journal::Closed(store));
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
