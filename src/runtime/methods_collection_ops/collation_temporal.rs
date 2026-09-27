use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Dispatch methods on Collation instances (set, primary, secondary, tertiary, quaternary).
    pub(in crate::runtime) fn dispatch_collation_method(
        &mut self,
        target: Value,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let ValueView::Instance {
            class_name,
            attributes,
            id,
        } = target.view()
        else {
            return Err(RuntimeError::new(
                "Collation method called on non-Collation value",
            ));
        };
        debug_assert!(class_name == "Collation");

        match method {
            "primary" | "secondary" | "tertiary" | "quaternary" if args.is_empty() => {
                Ok(attributes
                    .as_map()
                    .get(method)
                    .cloned()
                    .unwrap_or(Value::int(1)))
            }
            "set" => {
                // .set accepts named arguments: primary, secondary, tertiary, quaternary
                // Each can be -1, 0, 1, or Bool (False=0, True=1)
                let mut new_attrs = attributes.to_map();

                for arg in args {
                    if let ValueView::Pair(key, val) = arg.view() {
                        let int_val = match val.view() {
                            ValueView::Bool(b) => {
                                if b {
                                    1
                                } else {
                                    0
                                }
                            }
                            ValueView::Int(n) => n,
                            _ => crate::runtime::utils::to_int(val),
                        };
                        match key.as_str() {
                            "primary" | "secondary" | "tertiary" | "quaternary" => {
                                new_attrs.insert(key.clone(), Value::int(int_val));
                            }
                            _ => {}
                        }
                    }
                }

                let result = Value::write_back_sharing(&attributes, class_name, new_attrs, id);
                // Also update the target in-place (Collation.set mutates and returns self)
                Ok(result)
            }
            "gist" => {
                let settings = crate::builtins::collation::CollationSettings::from_value(&target);
                let level = settings.collation_level();
                Ok(Value::str(format!(
                    "collation-level => {}, Country => International, Language => None, primary => {}, secondary => {}, tertiary => {}, quaternary => {}",
                    level,
                    settings.primary,
                    settings.secondary,
                    settings.tertiary,
                    settings.quaternary
                )))
            }
            // Rakudo's `Collation` holds one `$!collation-level` attribute and
            // derives `primary`/`secondary`/`tertiary`/`quaternary` from its
            // bits, so its `.raku` is `Collation.new(collation-level => N)`.
            // mutsu stores the four levels separately, so the generic
            // instance `.raku` would list all four (and, with no declared
            // attributes on the class, actually listed none). Re-encode the
            // level here, exactly as the `gist` arm above does.
            "raku" => {
                let settings = crate::builtins::collation::CollationSettings::from_value(&target);
                Ok(Value::str(format!(
                    "Collation.new(collation-level => {})",
                    settings.collation_level()
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "Unknown Collation method '{}'",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn dispatch_collate(
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
