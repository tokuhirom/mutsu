use super::*;

impl Value {
    /// `Block.replace-body($new-body)`: swap the body of the node shared by
    /// every holder, as rakudo's mutator does, and answer `Nil` (rakudo's
    /// answer is `Any`, what a `Nil` reads as). `None` when the invocant is
    /// not a RakuAST node.
    // Cost: O(f), f = fields of the node (a handful).
    pub(crate) fn rakuast_replace_body(
        &self,
        new_body: Value,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { attributes, .. } = self.view()
            && let Some(base) = attributes.rakuast_base()
        {
            return base.rakuast_replace_body(new_body);
        }
        self.0
            .with_rakuast_inplace(|node| crate::rakuast::block_queries::replace_body(node, new_body))
            .map(|done| done.map(|()| Value::NIL))
    }
}
