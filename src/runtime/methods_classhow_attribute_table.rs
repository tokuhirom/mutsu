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
        // A role, or one candidate of a role group (`R.^candidates[0]`):
        // its own attributes, which the class registry does not hold
        // (Config::BINDish's `Container.^candidates[0]
        // .^get_attribute_for_usage('$!payload')`, #14186).
        let own = match self.role_candidate_receiver(receiver, &owner_class) {
            Some(candidate) => self.role_candidate_own_attribute_objects(&owner_class, candidate),
            None => self.collect_attribute_objects(&owner_class, true),
        };
        for value in own {
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
        // A role mixed into the object at run time (`$attr does R`, `$o but R`)
        // contributes its attributes to the object's own: Rakudo lists them
        // (`$o.^attributes`) and `.^get_attribute_for_usage` finds them
        // (AttrX::Mooish reads its role's `$!lazy` that way).
        if let ValueView::Mixin(_, mixins) = receiver.view() {
            for role in crate::value::types::mixin_roles_applied_last_first(mixins, &owner_class) {
                for attr in self.role_mixin_attribute_objects(&role) {
                    let name = match attr.view() {
                        ValueView::Instance { attributes, .. } => {
                            attributes.as_map().get("name").map(Value::to_string_value)
                        }
                        _ => None,
                    };
                    if let Some(name) = name {
                        table.entry(name).or_insert(attr);
                    }
                }
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
