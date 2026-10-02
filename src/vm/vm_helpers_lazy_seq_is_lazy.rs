use super::*;
use crate::value::SeqBody;

impl Interpreter {
    /// A Seq's `is-lazy` is its iterator's (Rakudo's `Seq.is-lazy` delegates
    /// to `$!iter.is-lazy`). A built-in iterator's laziness is known when
    /// `Seq.new` builds the body, but a user `does Iterator` class answers
    /// through its own `method is-lazy`, which is user code and so is asked
    /// here, when a reader needs the answer (`.is-lazy`, `.gist`, `say`), and
    /// not inside `Seq.new`. A `True` answer marks the body lazy, which makes
    /// `.is-lazy` report it and `.gist` render the `(...)` placeholder instead
    /// of pulling forever (#10864).
    ///
    /// A no-op for a body that is already lazy, has been pulled, or wraps an
    /// iterator without a user `is-lazy` (the role's default is `False`).
    // Cost: O(1) plus one user `is-lazy` call when the iterator declares one.
    pub(crate) fn resolve_seq_iterator_laziness(
        &mut self,
        body: &SeqBody,
    ) -> Result<(), RuntimeError> {
        if body.is_lazy() {
            return Ok(());
        }
        let Some(iterator) = body.unpulled_iterator() else {
            return Ok(());
        };
        let class_name = match iterator.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => return Ok(()),
        };
        // mutsu's own iterators carry their laziness as data, read when the
        // Seq was built (`try_native_seq_construct`, `Seq.from-loop`).
        if matches!(class_name.as_str(), "Iterator" | "FromLoopIterator")
            || !self.class_has_user_method(&class_name, "is-lazy")
        {
            return Ok(());
        }
        // TODO: compile to bytecode — a user method on an arbitrary instance is
        // invoked through the same generic method call the Seq's `pull-one`
        // pulls go through (`pull_iterator_prefix_to_vec`).
        if self
            .call_method_with_values(iterator, "is-lazy", Vec::new())?
            .truthy()
        {
            body.mark_lazy();
        }
        Ok(())
    }
}
