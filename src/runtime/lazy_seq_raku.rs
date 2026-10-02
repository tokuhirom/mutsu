//! `.raku` of a lazy `Seq`.
//!
//! Rakudo's `Seq.raku` on a lazy sequence reifies a bounded prefix, renders it
//! and marks the rest: `(2, 4, 6, ...).lazy.Seq`, with a trailing `...` glued
//! to the last element when more remain. Reifying a mapped pipe runs its
//! callback, so this needs the interpreter and cannot live in the pure
//! `raku_value`.

use super::*;

/// How many elements `Seq.raku` reifies from a lazy sequence.
const LAZY_SEQ_RAKU_PREFIX: usize = 100;

impl Interpreter {
    /// Whether `.raku`/`.perl` of `ll` renders through [`Self::lazy_seq_raku`]:
    /// a genuinely lazy bare `Seq`. A lazy `@` array keeps the `[...]`
    /// placeholder.
    pub(crate) fn lazy_seq_raku_applies(ll: &crate::value::LazyList) -> bool {
        ll.renders_lazy_placeholder() && !ll.in_array_context()
    }

    /// The `.raku` text of a lazy bare `Seq`: its first 100 elements, `...`
    /// when more remain, then `.lazy.Seq`.
    // Cost: O(p) plus the cost of pulling the prefix, p = LAZY_SEQ_RAKU_PREFIX
    // (a constant 100) elements; the whole sequence is never reified.
    pub(crate) fn lazy_seq_raku(
        &mut self,
        ll: &crate::value::LazyList,
    ) -> Result<String, RuntimeError> {
        let mut items = self.force_lazy_list_vm_n(ll, LAZY_SEQ_RAKU_PREFIX + 1)?;
        let more = items.len() > LAZY_SEQ_RAKU_PREFIX;
        items.truncate(LAZY_SEQ_RAKU_PREFIX);
        let body = items
            .iter()
            .map(|item| self.raku_element_repr(item))
            .collect::<Vec<_>>()
            .join(", ");
        Ok(format!(
            "({body}{}).lazy.Seq",
            if more { "..." } else { "" }
        ))
    }
}
