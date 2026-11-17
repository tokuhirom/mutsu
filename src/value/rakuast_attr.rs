//! `nqp::getattr` / `nqp::bindattr` on a RakuAST node value (#17327): the
//! attribute is the node's model field, and a bind writes through the node
//! shared by every holder ([`crate::rakuast::attr_bind`]).

use super::{Value, ValueView};

impl Value {
    /// The attribute `attr` (`$!left`) of this RakuAST node: `None` when this
    /// is no node or the attribute is not one of its class's model fields.
    // Cost: as [`crate::rakuast::attr_bind::get_attribute`].
    pub(crate) fn rakuast_get_attribute(&self, attr: &str) -> Option<Value> {
        match self.view() {
            ValueView::RakuAst(node) => crate::rakuast::attr_bind::get_attribute(node, attr),
            _ => None,
        }
    }

    /// Bind the attribute `attr` of this RakuAST node to `value`, in place:
    /// `false` when this is no node or the attribute is not one of its class's
    /// model fields.
    // Cost: as [`crate::rakuast::attr_bind::bind_attribute`].
    pub(crate) fn rakuast_bind_attribute(&self, attr: &str, value: Value) -> bool {
        self.0
            .with_rakuast_inplace(|node| crate::rakuast::attr_bind::bind_attribute(node, attr, value))
            .unwrap_or(false)
    }
}
