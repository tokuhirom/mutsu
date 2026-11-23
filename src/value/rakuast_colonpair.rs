use super::*;

impl Value {
    /// `ApplyInfix.add-colonpair($pair)` / `ApplyListInfix.add-colonpair($pair)`:
    /// append an adverb to the node shared by every holder, as rakudo's
    /// mutator does, and answer `Nil` (a `Name` answers the pair). `None` when the invocant is not a
    /// RakuAST node.
    // Cost: O(1) amortized.
    pub(crate) fn rakuast_add_colonpair(&self, pair: Value) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { attributes, .. } = self.view()
            && let Some(base) = attributes.rakuast_base()
        {
            return base.rakuast_add_colonpair(pair);
        }
        // A `Name` answers the pair it was given, an application `Nil`, as
        // rakudo's mutators do (#18976).
        self.0.with_rakuast_inplace(|node| {
            let answer = if node.class == crate::rakuast::RakuAstClass::Name {
                pair.clone()
            } else {
                Value::NIL
            };
            crate::rakuast::add_colonpair(node, pair).map(|()| answer)
        })
    }
}
