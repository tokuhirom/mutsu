use super::*;

impl Value {
    /// `ArgList.replace-args(@args)`: the arguments of the node shared by
    /// every holder become the list's elements; answers `Nil`. `None` when the
    /// invocant is not a RakuAST node.
    // Cost: O(f + n), f = fields of the node, n = elements of the list.
    pub(crate) fn rakuast_replace_args(&self, list: &Value) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { attributes, .. } = self.view()
            && let Some(base) = attributes.rakuast_base()
        {
            return base.rakuast_replace_args(list);
        }
        self.0
            .with_rakuast_inplace(|node| crate::rakuast::arg_invocant::replace_args(node, list))
            .map(|done| done.map(|()| Value::NIL))
    }

    /// `ArgList.set-arg-at-pos($pos, $arg)`: write one argument in place;
    /// answers `Nil`. `None` when the invocant is not a RakuAST node.
    // Cost: O(f + p), f = fields of the node, p = the position written.
    pub(crate) fn rakuast_set_arg_at_pos(
        &self,
        pos: &Value,
        arg: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { attributes, .. } = self.view()
            && let Some(base) = attributes.rakuast_base()
        {
            return base.rakuast_set_arg_at_pos(pos, arg);
        }
        self.0
            .with_rakuast_inplace(|node| {
                crate::rakuast::arg_invocant::set_arg_at_pos(node, pos, arg)
            })
            .map(|done| done.map(|()| Value::NIL))
    }
}
