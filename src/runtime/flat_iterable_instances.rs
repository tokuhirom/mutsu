//! `flat` over a list that holds `Iterable` instances (#15322).
//!
//! `(C.new, 9).flat` reads each element's own `iterator` when `C` does
//! `Iterable` (or is an `is Range` / `is Seq` subclass), as `for`, `.map` and
//! `my @a =` already do ([`Interpreter::try_iterable_instance_items`]). The
//! flattener itself (`builtins::flat_val`) is pure and cannot ask an instance
//! for its elements, so the interpreter-aware entries replace each such
//! element with a `List` of its elements first.

use super::*;
use crate::value::ArrayKind;

impl Interpreter {
    /// `target` (a list-like value) with every `Iterable` instance among the
    /// elements `flat` descends into replaced by a `List` of its elements;
    /// `None` when there is no such instance. `Array` elements are items
    /// (itemized) and are not descended into.
    // Cost: O(n) tag scan over the n elements `flat` would visit, plus the
    // `iterator` of each instance found. Rakudo: O(1) -- `flat` is lazy there;
    // see the `todo:perf` issue for this scan.
    pub(crate) fn expand_iterable_instances(
        &mut self,
        target: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        match target.view() {
            ValueView::Array(items, kind @ (ArrayKind::List | ArrayKind::ItemList)) => Ok(self
                .expand_instance_items(items.as_slice())?
                .map(|expanded| {
                    Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(expanded)),
                        kind,
                    )
                })),
            ValueView::Seq(items) => Ok(self
                .expand_instance_items(&items)?
                .map(Value::seq)),
            _ => Ok(None),
        }
    }

    /// The items with instances expanded, or `None` when none needed it.
    fn expand_instance_items(
        &mut self,
        items: &[Value],
    ) -> Result<Option<Vec<Value>>, RuntimeError> {
        if !items.iter().any(Self::may_hold_iterable_instance) {
            return Ok(None);
        }
        let mut out = Vec::with_capacity(items.len());
        let mut changed = false;
        for item in items {
            match item.view() {
                ValueView::Instance { .. } | ValueView::Mixin(..) => {
                    if let Some(elements) = self.try_iterable_instance_items(item)? {
                        let elements = self
                            .expand_instance_items(&elements)?
                            .unwrap_or(elements);
                        out.push(Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(elements)),
                            ArrayKind::List,
                        ));
                        changed = true;
                        continue;
                    }
                    out.push(item.clone());
                }
                ValueView::Array(inner, ArrayKind::List) => {
                    match self.expand_instance_items(inner.as_slice())? {
                        Some(expanded) => {
                            out.push(Value::array_with_kind(
                                crate::gc::Gc::new(crate::value::ArrayData::new(expanded)),
                                ArrayKind::List,
                            ));
                            changed = true;
                        }
                        None => out.push(item.clone()),
                    }
                }
                ValueView::Slip(inner) => match self.expand_instance_items(&inner)? {
                    Some(expanded) => {
                        out.push(Value::slip(expanded));
                        changed = true;
                    }
                    None => out.push(item.clone()),
                },
                _ => out.push(item.clone()),
            }
        }
        Ok(changed.then_some(out))
    }

    /// Whether a list-like `target` has an element `flat` would have to ask
    /// an instance about ([`Self::expand_iterable_instances`] answers it).
    // Cost: O(n) tag scan over the elements `flat` would visit.
    pub(crate) fn flat_may_hold_iterable_instance(target: &Value) -> bool {
        match target.view() {
            ValueView::Array(items, ArrayKind::List | ArrayKind::ItemList) => {
                items.iter().any(Self::may_hold_iterable_instance)
            }
            ValueView::Seq(items) => items.iter().any(Self::may_hold_iterable_instance),
            _ => false,
        }
    }

    /// Whether `item` is, or contains where `flat` descends, an instance.
    // Cost: O(1) for a scalar, O(m) for a nested list of m elements.
    fn may_hold_iterable_instance(item: &Value) -> bool {
        match item.view() {
            ValueView::Instance { .. } | ValueView::Mixin(..) => true,
            ValueView::Array(inner, ArrayKind::List) => {
                inner.iter().any(Self::may_hold_iterable_instance)
            }
            ValueView::Slip(inner) => inner.iter().any(Self::may_hold_iterable_instance),
            _ => false,
        }
    }
}
