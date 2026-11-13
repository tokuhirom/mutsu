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
    pub(crate) fn is_lazy_user_iterator(&mut self, iterator: &Value) -> Result<bool, RuntimeError> {
        let class_name = match iterator.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => return Ok(false),
        };
        if matches!(
            class_name.as_str(),
            "Iterator" | "FromLoopIterator" | "HeadIterator"
        ) || !self.class_has_user_method(&class_name, "is-lazy")
        {
            return Ok(false);
        }
        Ok(self
            .try_compiled_method_or_interpret(iterator.clone(), "is-lazy", Vec::new())?
            .truthy())
    }

    /// The lazy array value over `iterator`, pulled on demand.
    // Cost: O(1).
    pub(super) fn lazy_list_over_user_iterator(iterator: Value) -> Value {
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

impl Interpreter {
    /// `for $instance -> ...` over a bare instance whose class has its own
    /// `iterator` (`for.map` reads `self.iterator`): the lazy list over it
    /// when that iterator is lazy, so a never-ending iterator is pulled one
    /// iteration at a time, else the list of its drained items; `None` when
    /// `iterable` is anything else.
    // Cost: O(1) for a lazy iterator, O(n) pulls (n = items) for a finite one,
    // plus one user `iterator` call and one `is-lazy` call.
    pub(crate) fn lazy_for_source_of_instance(
        &mut self,
        iterable: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = iterable.view()
        else {
            return Ok(None);
        };
        let cn = class_name.resolve();
        if attributes.contains_key(crate::symbol::wk::array_storage())
            || !self.has_user_method(&cn, "iterator")
        {
            return Ok(None);
        }
        let iterator =
            self.try_compiled_method_or_interpret(iterable.clone(), "iterator", vec![])?;
        if self.is_lazy_user_iterator(&iterator)? {
            return Ok(Some(Self::lazy_list_over_user_iterator(iterator)));
        }
        // A finite iterator is drained here: it was already obtained, and
        // asking the class for `iterator` again could repeat its side effects.
        let (items, _) = self.drive_iterator_value_items(iterator)?;
        Ok(Some(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(items)),
            crate::value::ArrayKind::List,
        )))
    }

    /// Reify a whole `Seq.new($iterator)` the way rakudo's
    /// `List.from-iterator` does when the iterator's class writes its own
    /// `push-all`: one `push-all` into a fresh `IterationBuffer`, whose
    /// elements are the Seq's. ParaSeq's `BufferIterator.push-all` splices its
    /// buffer and hands the rest to its source's `push-all`, where pulling one
    /// element at a time cost a user method call per element (#17337).
    ///
    /// `false` (nothing pulled) when the iterator is a built-in one, or its
    /// `push-all` is the `Iterator` role's default: that one pulls one at a
    /// time, which is what the caller's own loop then does.
    // Cost: O(1) plus the user `push-all` and O(n) to read back its n elements.
    pub(crate) fn push_all_user_iterator_into(
        &mut self,
        iterator: &Value,
        out: &mut crate::value::PullOut,
    ) -> Result<bool, RuntimeError> {
        let class_name = match iterator.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => return Ok(false),
        };
        if matches!(
            class_name.as_str(),
            "Iterator" | "FromLoopIterator" | "HeadIterator"
        ) || !self.class_has_user_method(&class_name, "push-all")
        {
            return Ok(false);
        }
        let target = self.nqp_create(Value::package(Symbol::intern("IterationBuffer")))?;
        let Some((_, def)) =
            self.resolve_method_with_owner(&class_name, "push-all", std::slice::from_ref(&target))
        else {
            return Ok(false);
        };
        let from_role_default = [def.original_role.as_deref(), def.role_origin.as_deref()]
            .into_iter()
            .flatten()
            .any(|role| {
                matches!(
                    role.strip_prefix("GLOBAL::").unwrap_or(role),
                    "Iterator" | "PredictiveIterator"
                )
            });
        if from_role_default {
            return Ok(false);
        }
        self.try_compiled_method_or_interpret(iterator.clone(), "push-all", vec![target.clone()])?;
        out.produced
            .extend(Self::nqp_elems_of(&target).unwrap_or_default());
        Ok(true)
    }
}
