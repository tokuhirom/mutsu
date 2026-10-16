use super::*;
use crate::value::ValueView;

impl Interpreter {
    pub(crate) fn dispatch_collate(
        &mut self,
        target: Value,
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::collation::{CollationSettings, collate_sort};

        // Get $*COLLATION settings
        let settings = self
            .get_dynamic_var("*COLLATION")
            .ok()
            .or_else(|| self.env.get("$*COLLATION").cloned())
            .map(|v| CollationSettings::from_value(&v))
            .unwrap_or_default();

        match target.view() {
            ValueView::Package(class_name) if class_name == "Supply" => {
                Ok(Value::seq(vec![Value::package(class_name)]))
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let values = match attributes.as_map().get("values").map(|v| v.view()) {
                    Some(ValueView::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let sorted = collate_sort(values, &settings);
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(sorted));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("live".to_string(), Value::FALSE);
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            ValueView::Array(items, ..) => Ok(Value::seq(collate_sort(items.to_vec(), &settings))),
            _ => {
                let values = Self::value_to_list(&target);
                Ok(Value::seq(collate_sort(values, &settings)))
            }
        }
    }
}
