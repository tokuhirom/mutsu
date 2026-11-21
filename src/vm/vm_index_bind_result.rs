//! The value of an element-bind expression (#18260, #18540).
//!
//! `(%h<k> := (1, 2).Seq)` is the bound `Seq` itself, as the element now is.
//! The store keeps a bound element in a shared cell (a `ContainerRef`), which
//! reads back as a `Scalar` holding the value, so handing that cell out as the
//! expression's value itemized it: `$((1, 2).Seq)`, and a routine returning
//! `%cache{$k} := $seq` gave its caller one item instead of a list.
//!
//! `(%h<k> := $x)` is `$x`'s container, which the element and `$x` now share:
//! `my $r := (%h<k> := $x)` aliases `$x`, and its `.VAR` is a `Scalar`.

use super::*;

/// What an element-bind expression answers, decided before the store runs.
pub(crate) enum BindAnswer {
    /// The bound value itself.
    Value(Value),
    /// The container of the named source variable, which the store rebinds
    /// to the cell it seats at the element.
    Source(String),
}

impl Interpreter {
    /// What the element-bind expression whose [`Self::bind_index_marker`] is
    /// `val` answers: the bound value when no source names a variable, the
    /// variable's (shared) container when one does. `None` when `val` is no
    /// marker, or for a source the store does not share a cell with (an
    /// element, an `@`/`%` aggregate): the store's own result stands.
    // Cost: O(s), s = number of sources.
    pub(crate) fn bind_expression_answer(val: &Value) -> Option<BindAnswer> {
        if !Self::is_bind_index_value(val) {
            return None;
        }
        let (value, sources) = Self::split_bind_index_marker(val.clone()).ok()?;
        match sources.as_slice() {
            [Some(name)] if !name.contains('\0') && !name.starts_with(['@', '%', '&']) => {
                return Some(BindAnswer::Source(name.clone()));
            }
            _ => {}
        }
        if sources.iter().any(Option::is_some)
            || value.is_container_ref()
            || value.as_varref().is_some()
        {
            return None;
        }
        Some(BindAnswer::Value(value))
    }

    /// [`Self::bind_expression_answer`] for a store whose value sits under
    /// `indices` subscripts on the stack (`%h<a><b> := v`).
    /// Only a single element answers it: a slice bind (`@a[0, 1] := ...`,
    /// `%h<a b> := ...`, a Range or `*` subscript) answers the bound slice.
    // Cost: O(s + d), as `bind_expression_answer`, d = `indices`.
    pub(crate) fn nested_bind_expression_value(&self, indices: usize) -> Option<BindAnswer> {
        let at = self.stack.len().checked_sub(indices + 1)?;
        if !self.stack[at + 1..]
            .iter()
            .all(Self::is_single_element_subscript)
        {
            return None;
        }
        Self::bind_expression_answer(&self.stack[at])
    }

    /// Whether `idx` names exactly one element: an integer, string or number
    /// key (or a variable holding one), not a list, Range, Whatever, Junction
    /// or code subscript.
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

    /// Replace a finished store's result with what the bind answers (see
    /// [`Self::bind_expression_answer`]). A source variable's container is
    /// the cell the store rebound it to; a source the store did not rebind to
    /// a cell leaves the result alone.
    // Cost: O(1) plus one by-name env read for a source.
    pub(crate) fn answer_bind_expression_value(&mut self, bound: Option<BindAnswer>) {
        let answer = match bound {
            Some(BindAnswer::Value(value)) => value,
            Some(BindAnswer::Source(name)) => match self.env().get(&name) {
                Some(cell) if cell.is_container_ref() => cell.clone(),
                _ => return,
            },
            None => return,
        };
        if let Some(result) = self.stack.last_mut() {
            *result = answer;
        }
    }
}
