use super::*;

impl Value {
    /// `ApplyInfix.add-colonpair($pair)` / `ApplyListInfix.add-colonpair($pair)`:
    /// append an adverb to the node shared by every holder, as rakudo's
    /// mutator does, and answer `Nil`. `None` when the invocant is not a
    /// RakuAST node.
    // Cost: O(1) amortized.
    pub(crate) fn rakuast_add_colonpair(&self, pair: Value) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { attributes, .. } = self.view()
            && let Some(base) = attributes.rakuast_base()
        {
            return base.rakuast_add_colonpair(pair);
        }
        self.0
            .with_rakuast_inplace(|node| crate::rakuast::add_colonpair(node, pair))
            .map(|done| done.map(|()| Value::NIL))
    }
}
