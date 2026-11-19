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
        ll.renders_lazy_now() && !ll.in_array_context()
    }

    /// The `.raku` text of a lazy bare `Seq`: its first 100 elements, `...`
    /// when more remain, then `.lazy.Seq` (`.lazy` for a `List` view; a finite one renders every element,
    /// a single one without a trailing comma: `(5).lazy.Seq`).
    // Cost: O(p) plus the cost of pulling the prefix, p = LAZY_SEQ_RAKU_PREFIX
    // (a constant 100) elements; the whole sequence is never reified.
    pub(crate) fn lazy_seq_raku(
        &mut self,
        ll: &crate::value::LazyList,
    ) -> Result<String, RuntimeError> {
        let mut items = self.force_lazy_list_vm_n(ll, LAZY_SEQ_RAKU_PREFIX + 1)?;
        let more = items.len() > LAZY_SEQ_RAKU_PREFIX;
        items.truncate(LAZY_SEQ_RAKU_PREFIX);
        // Rakudo renders the prefix through `my @elements = self.head(101)`
        // (`List.raku`), so each element is read back out of an Array's
        // `Scalar`: a list, array, hash or Seq renders itemized (`$(1, 3)`,
        // `${:a(1)}`) and `Nil` as the element default `Any` (#16139).
        let body = items
            .iter()
            .map(|item| self.raku_element_repr(&as_array_element(item)))
            .collect::<Vec<_>>()
            .join(", ");
        let dots = if more { "..." } else { "" };
        // A lazy `List` (`.List`, `.list`, `.cache` of a lazy Seq) is not a
        // `Seq`: Rakudo renders `(1, 2, 3).lazy`, itemized as `$(1, 2, 3).lazy`.
        if ll.in_list_context() {
            let item = if ll.is_itemized() { "$" } else { "" };
            return Ok(format!("{item}({body}{dots}).lazy"));
        }
        let text = format!("({body}{dots}).lazy.Seq");
        // A `$`-held Seq renders itemized, as a non-lazy one does:
        // `my $s = (1, 2).lazy; $s.raku` is `$((1, 2).lazy.Seq)`.
        Ok(if ll.is_itemized() {
            format!("$({text})")
        } else {
            text
        })
    }
}

/// `value` as an Array element reads back: a positional, a hash or a `Seq`
/// is held in a `Scalar` (itemized), `Nil` becomes the element default `Any`,
/// anything else is itself.
// Cost: O(1).
fn as_array_element(value: &Value) -> Value {
    match value.view() {
        ValueView::Nil => Value::package(crate::symbol::wk::any()),
        ValueView::Array(..) | ValueView::Hash(_) | ValueView::Seq(_) => value.clone().item(),
        _ => value.clone(),
    }
}
