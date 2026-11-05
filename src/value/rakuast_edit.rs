use super::*;

impl Value {
    /// Run `edit` on the RakuAST node shared by every holder of this value
    /// (the node itself, or the base of an instance wrapping one) and answer
    /// what it returns. `None` when the value is not a RakuAST node. The
    /// in-place builders of the model (`add-atom`, `set-opener`, ...) are this.
    // Cost: O(1) plus the cost of `edit`.
    pub(crate) fn rakuast_edit(
        &self,
        edit: impl FnOnce(&mut crate::rakuast::RakuAstNode) -> Result<Value, RuntimeError>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { attributes, .. } = self.view()
            && let Some(base) = attributes.rakuast_base()
        {
            return base.rakuast_edit(edit);
        }
        self.0.with_rakuast_inplace(edit)
    }
}
