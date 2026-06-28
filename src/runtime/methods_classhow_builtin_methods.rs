use super::*;

impl Interpreter {
    pub(super) fn dispatch_classhow_methods(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let invocant = &args[0];
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };

        // Parse named arguments
        let mut local = false;
        let mut all = false;
        let mut private = false;
        let mut tree = false;
        for arg in &args[1..] {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "local" => local = value.truthy(),
                    "all" => all = value.truthy(),
                    "private" => private = value.truthy(),
                    "tree" => tree = value.truthy(),
                    _ => {}
                }
            }
        }

        if tree {
            return self.classhow_methods_tree(&class_name, private);
        }

        let mut result = Vec::new();

        // Extract mixin role names from the invocant for runtime role method collection
        let mixin_role_names: Vec<String> = if let Value::Mixin(_, mixins) = invocant {
            mixins
                .keys()
                .filter_map(|key| key.strip_prefix("__mutsu_role__").map(String::from))
                .collect()
        } else {
            Vec::new()
        };

        if local {
            // Only methods defined directly on this class
            self.collect_class_methods(&class_name, private, &mut result);
            // Also include methods from runtime-mixed-in roles
            for role_name in &mixin_role_names {
                self.collect_role_methods(role_name, private, &mut result);
            }
        } else {
            // Walk MRO (already includes the class itself)
            let mro = self.class_mro(&class_name);

            for cn in &mro {
                if !all && (cn == "Any" || cn == "Mu") {
                    continue;
                }
                self.collect_class_methods(cn, private, &mut result);
            }

            // For built-in types that don't have class defs, add known methods
            if result.is_empty() || !self.registry().classes.contains_key(&class_name) {
                self.collect_builtin_type_methods(&class_name, &mut result);
                if all {
                    self.collect_builtin_type_methods("Any", &mut result);
                    self.collect_builtin_type_methods("Mu", &mut result);
                }
            }
        }

        Ok(Value::array(result))
    }

    pub(super) fn collect_builtin_type_methods(&self, type_name: &str, result: &mut Vec<Value>) {
        // Method names are *derived from the real native dispatch* for concrete
        // types (probe a sample value against `METHOD_UNIVERSE`), falling back to
        // a declared list only for abstract/sample-less types. See
        // `builtins::builtin_type_methods` for the rationale.
        let methods =
            crate::builtins::builtin_type_methods::introspected_type_method_names(type_name);
        for name in &methods {
            if !result.iter().any(|v| {
                if let Value::Instance { attributes, .. } = v {
                    attributes
                        .as_map()
                        .get("name")
                        .map(|n| n.to_string_value())
                        .as_deref()
                        == Some(name)
                } else {
                    false
                }
            }) {
                result.push(self.make_native_method_object(name));
            }
        }
    }
}
