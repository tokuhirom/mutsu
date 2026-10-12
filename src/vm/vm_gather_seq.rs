//! The interpreter half of a plain `gather` as a `Seq`
//! (docs/adr/11678-gather-is-a-deferred-seq-source.md): driving the
//! `SeqSource::Gather` coroutine a prefix at a time, and handing its inner
//! list to the consumers that already stream a `LazyList`.

use super::*;

impl Interpreter {
    /// The first `needed` elements of a gather's inner coroutine list
    /// (fewer when it finishes first), running the body only as far as
    /// that takes. The prefix half of [`crate::value::SeqSource::Gather`]'s
    /// pull.
    // Cost: O(needed) for the copy, plus running the body to its
    // `needed`-th `take`.
    pub(crate) fn pull_gather_prefix(
        &mut self,
        list: &Value,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        match list.view() {
            ValueView::LazyList(ll) => self.force_lazy_list_vm_n(&ll, needed),
            _ => Ok(Vec::new()),
        }
    }

    /// An operand a consumer reads by streaming a `LazyList` (the zip and
    /// cross metaoperators): a plain gather `Seq` is handed over as its inner
    /// coroutine list, which those consumers already pull on demand. The
    /// consumer consumes the Seq, as Rakudo's iterating operators do
    /// ([`crate::value::SeqBody::take_gather_source`]); a cached Seq shares
    /// its list instead. Every other value is returned unchanged.
    // Cost: O(1).
    pub(crate) fn gather_operand(&self, value: Value) -> Value {
        if !value.is_seq_value() {
            return value;
        }
        match value.view() {
            ValueView::Seq(body) => body
                .take_gather_source()
                .or_else(|| body.shared_gather_source())
                .unwrap_or(value),
            _ => value,
        }
    }
}
