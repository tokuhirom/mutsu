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
}
