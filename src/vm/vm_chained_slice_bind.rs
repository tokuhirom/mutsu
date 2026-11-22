//! `:=` into a chained subscript that holds a slice (`@s[0][0,1] := (7,8)`,
//! `@s[0,1][0] := 7`, #18535).
//!
//! Rakudo binds a last-level slice element by element into the container the
//! inner subscripts reach, when that container takes the bind: a real `Array`
//! for a positional slice, a `Hash` for an associative one. Every other
//! receiver -- an `Int`, a `List`, a `Hash` asked for a positional slice --
//! and a slice at an inner level throw `X::Bind` ("Cannot use bind operator
//! with this left-hand side").

use super::*;

/// What a chained bind with a slice does.
pub(super) enum ChainedSliceBind {
    /// No level is a slice: the ordinary single-element bind applies.
    NotASlice,
    /// The bind is refused with `X::Bind`.
    Refused,
    /// Bind each `(key, value)` at the last level.
    PerKey(Vec<(Value, Value)>),
}

/// Whether a subscript value names several elements.
// Cost: O(1).
fn is_slice(index: &Value) -> bool {
    matches!(index.view(), ValueView::Array(..))
}

impl Interpreter {
    /// Classify a chained `:=` whose indices are `indices` (innermost first),
    /// rooted at `root`, binding the marker `bind` (see
    /// [`Self::bind_index_marker`]).
    // Cost: O(d + k), d = subscript levels, k = keys of the last-level slice.
    pub(super) fn classify_chained_slice_bind(
        &mut self,
        root: Option<Value>,
        indices: &[Value],
        positional_flags: &[bool],
        bind: &Value,
    ) -> Result<ChainedSliceBind, RuntimeError> {
        let Some((last, inner)) = indices.split_last() else {
            return Ok(ChainedSliceBind::NotASlice);
        };
        if inner.iter().any(is_slice) {
            return Ok(ChainedSliceBind::Refused);
        }
        let ValueView::Array(keys, ..) = last.view() else {
            return Ok(ChainedSliceBind::NotASlice);
        };
        let mut container = root;
        for (level, index) in inner.iter().enumerate() {
            let positional = positional_flags.get(level).copied().unwrap_or(true);
            let key = if positional {
                Self::positional_index_as_int(index.clone()).to_string_value()
            } else {
                index.to_string_value()
            };
            container = container
                .as_ref()
                .and_then(|c| Self::subscript_peek_step(c, &key, positional));
        }
        let positional = positional_flags.get(inner.len()).copied().unwrap_or(true);
        let takes_bind = container.is_some_and(|c| {
            match c.deref_container().descalarize().view() {
                ValueView::Array(_, kind) => {
                    positional && !matches!(kind, crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList)
                }
                ValueView::Hash(_) => !positional,
                _ => false,
            }
        });
        if !takes_bind {
            return Ok(ChainedSliceBind::Refused);
        }
        let (value, _) = Self::unwrap_bind_index_value(bind.clone());
        let values = self.assignment_rhs_values(&value)?;
        let keys = keys.items().to_vec();
        let pairs = keys
            .into_iter()
            .enumerate()
            .map(|(i, key)| {
                // Each element binds the value, not a source variable's
                // container: rakudo leaves `@s[0][0,1] := ($a, $b)` unaliased.
                let value = values.get(i).map_or(Value::NIL, Value::deref_container);
                (key, Self::bind_index_marker(value, Value::array(Vec::new())))
            })
            .collect();
        Ok(ChainedSliceBind::PerKey(pairs))
    }
}
