//! Shared return-type lookup for code objects and live dispatchers.
use super::*;

impl Interpreter {
    // Cost: O(r), r = bytes in the return-type spelling.
    pub(crate) fn callable_return_type(&self, callable: &Value) -> Option<String> {
        match callable.view() {
            ValueView::Sub(data)
                if data.env.contains_key("__mutsu_multi_dispatch_name")
                    || data.env.contains_key("__mutsu_routine_name") =>
            {
                self.dispatcher_return_type(data.package, data.name)
            }
            ValueView::Sub(data) => match data.env.get("__mutsu_return_type").map(Value::view) {
                Some(ValueView::Str(rt)) => Some(rt.to_string()),
                _ => None,
            },
            ValueView::Routine { package, name, .. } => self.dispatcher_return_type(package, name),
            _ => None,
        }
    }

    // Cost: O(r), r = bytes in the return-type spelling.
    fn dispatcher_return_type(
        &self,
        package: crate::symbol::Symbol,
        name: crate::symbol::Symbol,
    ) -> Option<String> {
        let key = crate::qualified::qualified(package, name);
        self.registry()
            .proto_functions
            .get(&key)
            .and_then(|proto| proto.return_type.clone())
    }

    pub(crate) fn routine_return_spec_by_name(&self, name: &str) -> Option<String> {
        let code_key = format!("&{}", name);
        for key in [code_key.as_str(), name] {
            if let Some(ValueView::Sub(data)) = self.env.get(key).map(Value::view)
                && let Some(ValueView::Str(spec)) =
                    data.env.get("__mutsu_return_type").map(Value::view)
            {
                return Some(spec.to_string());
            }
        }
        // Also check the FunctionDef registry
        if let Some(def) = self.resolve_function(name)
            && let Some(ref rt) = def.return_type
        {
            return Some(rt.clone());
        }
        if let Some(def) = self.resolve_proto_function(name)
            && let Some(ref rt) = def.return_type
        {
            return Some(rt.clone());
        }
        None
    }
}
