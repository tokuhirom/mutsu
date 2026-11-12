//! The readonly marks a `CATCH` handler run inline at a deeper throw site sees.

use super::*;

impl Interpreter {
    /// Swap in the readonly marks of the frame that installed a `CATCH`
    /// region, for running its handler inline at a deeper throw site
    /// (ADR-0072). The frames between the region and the throw are still on
    /// the stack, and their parameters' marks are journaled above
    /// `installing_journal`: left in place, the thrower's `Str $reason`
    /// parameter made the handler's own `my $reason = .reason` read-only (Red's
    /// `red-compile`). The marks are rebuilt by undoing, on a copy, every
    /// journal entry made since the region was entered; the caller restores
    /// the live state with [`Self::restore_readonly_state`] after the handler.
    ///
    /// `None` when nothing was journaled since the region was entered (a throw
    /// in the installing frame, or through calls that marked nothing).
    // Cost: O(1) when nothing was journaled; otherwise O(r + j), r = readonly
    // names live on the stack (the copy), j = journal entries above the mark.
    pub(super) fn enter_handler_readonly_state(
        &mut self,
        installing_journal: usize,
    ) -> Option<SavedReadonlyState> {
        if self.lexicals.readonly_undo.borrow().len() <= installing_journal {
            return None;
        }
        let saved = self.take_readonly_state();
        let mut vars = saved.vars.clone();
        for entry in saved.undo[installing_journal..].iter().rev() {
            match entry {
                ReadonlyUndo::Marked(sym) => {
                    vars.remove(sym);
                }
                ReadonlyUndo::Unmarked(sym, kind) | ReadonlyUndo::Rekinded(sym, kind) => {
                    vars.insert(*sym, *kind);
                }
                ReadonlyUndo::Scope => {}
            }
        }
        *self.lexicals.readonly_vars.borrow_mut() = vars;
        Some(saved)
    }
}
