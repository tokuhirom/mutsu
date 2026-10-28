//! `@`-array assignment from a user iterator that reports `is-lazy` (#15305).
//!
//! Rakudo reifies such an array on demand, so `my @a = Seq.new($it)` or an
//! `Iterable` instance assigned to `@a` must not drain a never-ending
//! iterator. The array holds a [`crate::value::LazyList`] whose pulls call
//! `pull-one` ([`crate::value::CatPullMode::UserIterator`]).

use super::*;
use crate::value::SeqBody;

impl Interpreter {
    /// Whether `iterator` is a user-defined `does Iterator` instance whose own
    /// `is-lazy` answers True. mutsu's built-in iterators carry their
    /// laziness as data and are handled by the Seq itself.
    // Cost: O(1) plus one user `is-lazy` call.
    fn is_lazy_user_iterator(&mut self, iterator: &Value) -> Result<bool, RuntimeError> {
        let class_name = match iterator.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => return Ok(false),
        };
        if matches!(class_name.as_str(), "Iterator" | "FromLoopIterator")
            || !self.class_has_user_method(&class_name, "is-lazy")
        {
            return Ok(false);
        }
        Ok(self
            .try_compiled_method_or_interpret(iterator.clone(), "is-lazy", Vec::new())?
            .truthy())
    }

    /// The lazy array value over `iterator`, pulled on demand.
    // Cost: O(1).
    fn lazy_list_over_user_iterator(iterator: Value) -> Value {
        let mut list =
            crate::value::LazyList::new_cat_pull(iterator, crate::value::CatPullMode::UserIterator);
        list.mark_lazy();
        Value::lazy_list(crate::gc::Gc::new(list))
    }

    /// `my @a = <Seq>`: when the Seq is an unpulled `Seq.new($it)` over a lazy
    /// user iterator, take the iterator and answer the lazy array over it.
    /// `None` leaves the Seq untouched for the ordinary reify.
    // Cost: O(1) plus one user `is-lazy` call.
    pub(crate) fn lazy_array_from_seq(
        &mut self,
        body: &SeqBody,
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(iterator) = body.unpulled_iterator() else {
            return Ok(None);
        };
        if !self.is_lazy_user_iterator(&iterator)? {
            return Ok(None);
        }
        Ok(body
            .take_iterator_source()?
            .map(Self::lazy_list_over_user_iterator))
    }

    /// `my @a = $iterable` for an instance with its own `iterator`: the lazy
    /// array over it when that iterator is lazy, else its drained elements.
    // Cost: O(n) for a finite iterator (n = elements), O(1) for a lazy one.
    pub(crate) fn array_from_iterable_instance(
        &mut self,
        iterable: &Value,
    ) -> Result<Value, RuntimeError> {
        let iterator = if let Some(result) =
            self.dispatch_mixin_method_call(iterable, "iterator", Vec::new())
        {
            result?
        } else {
            self.try_compiled_method_or_interpret(iterable.clone(), "iterator", vec![])?
        };
        if self.is_lazy_user_iterator(&iterator)? {
            return Ok(Self::lazy_list_over_user_iterator(iterator));
        }
        let (items, _) = self.drive_iterator_value_items(iterator)?;
        Ok(crate::runtime::coerce_to_array(Value::real_array(items)))
    }
}
