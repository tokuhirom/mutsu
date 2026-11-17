//! In-place edits of a RakuAST node value that write through the node shared
//! by every holder: `nqp::getattr` / `nqp::bindattr` of a node's attribute
//! (#17327, [`crate::rakuast::attr_bind`]) and `StatementList.insert-statement`
//! (#14916, [`crate::rakuast::statement_insert`]).

use super::{RuntimeError, Value, ValueView};

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
            .with_rakuast_inplace(|node| {
                crate::rakuast::attr_bind::bind_attribute(node, attr, value)
            })
            .unwrap_or(false)
    }

    /// `StatementList.insert-statement($index, $statement)`, in place. `None`
    /// when this is no RakuAST node.
    // Cost: as [`crate::rakuast::statement_insert::insert_statement`].
    pub(crate) fn rakuast_insert_statement(
        &self,
        index: &Value,
        statement: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { attributes, .. } = self.view()
            && let Some(base) = attributes.rakuast_base()
        {
            return base.rakuast_insert_statement(index, statement);
        }
        let index = crate::value::numeric_coerce::to_int(index);
        self.0.with_rakuast_inplace(|node| {
            crate::rakuast::statement_insert::insert_statement(node, index, statement.clone())
        })
    }
}
