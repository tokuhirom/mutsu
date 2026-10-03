//! `Metamodel::AttributeContainer` lookups over a class's *own* attributes:
//! `.^attribute_table` and `.^get_attribute_for_usage`.

use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// The attributes declared directly on the `.^` receiver, keyed by their
    /// full name (`$!a`, `@!b`), in declaration order.
    fn own_attribute_table(&mut self, receiver: &Value) -> ValueMap {
        let owner_class = self.mop_receiver_owner(receiver);
        let mut table = ValueMap::default();
        for value in self.collect_attribute_objects(&owner_class, true) {
            let name = match value.view() {
                ValueView::Instance { attributes, .. } => {
                    attributes.as_map().get("name").map(Value::to_string_value)
                }
                _ => None,
            };
            if let Some(name) = name {
                table.insert(name, value);
            }
        }
        table
    }

    /// `.^attribute_table`: the receiver's own attributes as a name-keyed Hash.
    // Cost: O(a), a = attributes declared on the receiver.
    pub(super) fn classhow_attribute_table(&mut self, receiver: &Value) -> Value {
        Value::hash(self.own_attribute_table(receiver))
    }

    /// `.^get_attribute_for_usage($name)`: the receiver's own Attribute named
    /// `$name` (twigil included). Like Rakudo it does not consult parents and
    /// dies with "No $name attribute in Type" when there is no such attribute.
    // Cost: O(a), a = attributes declared on the receiver.
    pub(super) fn classhow_get_attribute_for_usage(
        &mut self,
        receiver: &Value,
        name: &Value,
    ) -> Result<Value, RuntimeError> {
        let name = name.to_string_value();
        if let Some(attr) = self.own_attribute_table(receiver).get(&name) {
            return Ok(attr.clone());
        }
        let type_name = self.mop_receiver_owner(receiver);
        Err(RuntimeError::new(format!(
            "No {name} attribute in {type_name}"
        )))
    }
}
