//! The value of an element-bind expression (#18260).
//!
//! `(%h<k> := (1, 2).Seq)` is the bound `Seq` itself, as the element now is.
//! The store keeps a bound element in a shared cell (a `ContainerRef`), which
//! reads back as a `Scalar` holding the value, so handing that cell out as the
//! expression's value itemized it: `$((1, 2).Seq)`, and a routine returning
//! `%cache{$k} := $seq` gave its caller one item instead of a list.

use super::*;

impl Interpreter {
    /// The value of an element-bind expression whose
    /// [`Self::bind_index_marker`] is `val`: the bound value, when no source
    /// names a container. `None` when `val` is no marker, or when a source is a
    /// container (`%h<k> := $x`): the expression is then that container, which
    /// the store already answers, so `(%h<k> := $x) = 7` writes through to `$x`.
    // Cost: O(s), s = number of sources.
    pub(crate) fn bind_expression_value(val: &Value) -> Option<Value> {
        if !Self::is_bind_index_value(val) {
            return None;
        }
        let (value, sources) = Self::split_bind_index_marker(val.clone()).ok()?;
        if sources.iter().any(Option::is_some)
            || value.is_container_ref()
            || value.as_varref().is_some()
        {
            return None;
        }
        Some(value)
    }

    /// [`Self::bind_expression_value`] for a store whose value sits under
    /// `indices` subscripts on the stack (`%h<a><b> := v`).
    /// Only a single element answers it: a slice bind (`@a[0, 1] := ...`,
    /// `%h<a b> := ...`, a Range or `*` subscript) answers the bound slice.
    // Cost: O(s + d), as `bind_expression_value`, d = `indices`.
    pub(crate) fn nested_bind_expression_value(&self, indices: usize) -> Option<Value> {
        let at = self.stack.len().checked_sub(indices + 1)?;
        if !self.stack[at + 1..].iter().all(Self::is_single_element_subscript) {
            return None;
        }
        Self::bind_expression_value(&self.stack[at])
    }

    /// Whether `idx` names exactly one element: an integer, string or number
    /// key (or a variable holding one), not a list, Range, Whatever, Junction or code subscript.
    // Cost: O(1).
    fn is_single_element_subscript(idx: &Value) -> bool {
        matches!(
            idx.deref_container().view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Rat(..)
                | ValueView::Str(_)
        )
    }

    /// Replace a finished store's result with the bound value, when there is
    /// one (see [`Self::bind_expression_value`]).
    // Cost: O(1).
    pub(crate) fn answer_bind_expression_value(&mut self, bound: Option<Value>) {
        if let Some(bound) = bound
            && let Some(result) = self.stack.last_mut()
        {
            *result = bound;
        }
    }
}
