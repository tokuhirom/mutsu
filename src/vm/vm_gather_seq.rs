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

    /// A plain gather `Seq` handed to a builtin routine as its inner
    /// coroutine list, without consuming the Seq: the builtins read a
    /// `LazyList` lazily. The Seq stays readable afterwards (Rakudo would
    /// consume it for an iterating builtin; mutsu is lenient there). Every
    /// other value is returned unchanged.
    // Cost: O(1).
    pub(crate) fn shared_gather_list(&self, value: Value) -> Value {
        if !value.is_seq_value() {
            return value;
        }
        match value.view() {
            ValueView::Seq(body) => body.gather_source().unwrap_or(value),
            _ => value,
        }
    }

    /// Whether `value` is a plain gather `Seq` still pulling its coroutine.
    // Cost: O(1).
    pub(crate) fn is_gather_seq(value: &Value) -> bool {
        value.is_seq_value()
            && matches!(value.view(), ValueView::Seq(body) if body.is_gather_source())
    }

    /// [`Self::shared_gather_list`] over a builtin's argument list, borrowing
    /// it unchanged when no argument is a gather (the common case).
    // Cost: O(n), n = arguments.
    pub(crate) fn shared_gather_args<'a>(
        &self,
        args: &'a [Value],
    ) -> std::borrow::Cow<'a, [Value]> {
        if !args.iter().any(Self::is_gather_seq) {
            return std::borrow::Cow::Borrowed(args);
        }
        std::borrow::Cow::Owned(
            args.iter()
                .cloned()
                .map(|a| self.shared_gather_list(a))
                .collect(),
        )
    }

    /// Run a plain gather `Seq` to completion before a reader that renders it
    /// through pure code (`say`, `put`, `print`, `note`): Rakudo's rendering
    /// iterates the whole Seq too. A no-op for every other value.
    // Cost: runs the gather body to completion.
    pub(crate) fn reify_gather_seq(&mut self, value: &Value) -> Result<(), RuntimeError> {
        if let ValueView::Seq(body) = value.view()
            && body.is_gather_source()
        {
            let body = std::sync::Arc::clone(&body);
            self.reify_seq_body(&body)?;
        }
        Ok(())
    }
}
