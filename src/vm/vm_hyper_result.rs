use super::*;

impl Interpreter {
    /// The container a list-shaped hyper result is returned in — shared by the
    /// built-in operator hyper (`@a >>+<< @b`) and the code-ref hyper
    /// (`@a >>[&op]<< @b`) so the two cannot disagree.
    ///
    /// A `List` when neither operand is an `Array`. Otherwise an `Array`, of
    /// the same parameterization as the operand that gives the result its
    /// shape — the left one, unless it is a scalar being distributed
    /// (`1 <<+<< @i`) — as long as every result still satisfies that element
    /// type (`my Int @foo; @foo >>*>> 4` is an `Array[Int]`). If one does
    /// not, the result degrades to a plain `List` rather than raising a
    /// type-check error (rakudo#5778: `@foo >>/>> 4` is `(0.25, 0.5, 0.75)`).
    // Cost: O(n) type checks, n = results.len(), when the shape side is typed;
    // else O(1).
    pub(super) fn hyper_list_result(
        &mut self,
        left: &Value,
        right: &Value,
        results: Vec<Value>,
    ) -> Value {
        let is_array = |v: &Value| {
            matches!(
                v.view(),
                ValueView::Array(_, crate::value::ArrayKind::Array)
            )
        };
        let shape_side = if Self::is_listy(left) { left } else { right };
        let (value_type, declared_type) = match shape_side.view() {
            ValueView::Array(data, crate::value::ArrayKind::Array) => {
                (data.value_type.clone(), data.declared_type.clone())
            }
            _ => (None, None),
        };
        let fits_declared_type = value_type
            .as_deref()
            .is_none_or(|t| results.iter().all(|v| self.type_matches_value(t, v)));
        if (!is_array(left) && !is_array(right)) || !fits_declared_type {
            return Value::array(results);
        }
        let mut data = crate::value::ArrayData::new(results);
        data.value_type = value_type;
        data.declared_type = declared_type;
        Value::array_with_kind(crate::gc::Gc::new(data), crate::value::ArrayKind::Array)
    }
}
