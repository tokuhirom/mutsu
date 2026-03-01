use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, next_instance_id};

use super::{CustomTypeData, Interpreter};

impl Interpreter {
    /// Dispatch methods on Metamodel::Primitives.
    pub(super) fn metamodel_primitives_dispatch(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "create_type" => self.mp_create_type(args),
            "configure_type_checking" => self.mp_configure_type_checking(args),
            "compose_type" => self.mp_compose_type(args),
            "rebless" => self.mp_rebless(args),
            "is_type" => self.mp_is_type(args),
            _ => Err(RuntimeError::new(format!(
                "X::Method::NotFound: No such method '{}' for Metamodel::Primitives",
                method
            ))),
        }
    }

    /// Metamodel::Primitives.create_type($how, $repr?, :$mixin)
    fn mp_create_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let how = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("create_type requires a HOW argument"))?;

        let mut repr = "P6opaque".to_string();
        let mut is_mixin = false;

        for arg in args.iter().skip(1) {
            match arg {
                Value::Str(s) => repr = s.clone(),
                Value::Pair(k, v) if k == "mixin" => {
                    is_mixin = v.truthy();
                }
                _ => {}
            }
        }

        let id = next_instance_id();
        self.custom_type_data.insert(
            id,
            CustomTypeData {
                type_check_cache: None,
                authoritative: false,
                call_accepts: false,
                composed: false,
                is_mixin,
            },
        );

        Ok(Value::CustomType {
            how: Box::new(how),
            repr,
            name: Symbol::intern(""),
            id,
        })
    }

    /// Metamodel::Primitives.configure_type_checking($type, @cache, :$authoritative, :$call_accepts)
    fn mp_configure_type_checking(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let type_val = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("configure_type_checking requires a type argument"))?;

        let id = match &type_val {
            Value::CustomType { id, .. } => *id,
            _ => {
                return Err(RuntimeError::new(
                    "configure_type_checking: first argument must be a custom type",
                ));
            }
        };

        // Second arg is the type check cache (array of types)
        let cache = if let Some(cache_val) = args.get(1) {
            match cache_val {
                Value::Array(items, _) => Some(items.as_ref().clone()),
                _ => None,
            }
        } else {
            None
        };

        let mut authoritative = false;
        let mut call_accepts = false;

        for arg in args.iter().skip(2) {
            if let Value::Pair(k, v) = arg {
                match k.as_str() {
                    "authoritative" => authoritative = v.truthy(),
                    "call_accepts" => call_accepts = v.truthy(),
                    _ => {}
                }
            }
        }

        if let Some(data) = self.custom_type_data.get_mut(&id) {
            data.type_check_cache = cache;
            data.authoritative = authoritative;
            data.call_accepts = call_accepts;
        }

        Ok(type_val)
    }

    /// Metamodel::Primitives.compose_type($type, $configuration)
    fn mp_compose_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let type_val = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("compose_type requires a type argument"))?;

        let id = match &type_val {
            Value::CustomType { id, .. } => *id,
            _ => {
                return Err(RuntimeError::new(
                    "compose_type: first argument must be a custom type",
                ));
            }
        };

        if let Some(data) = self.custom_type_data.get_mut(&id) {
            data.composed = true;
        }

        Ok(type_val)
    }

    /// Metamodel::Primitives.rebless($obj, $type)
    fn mp_rebless(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("rebless requires two arguments"));
        }

        let obj = &args[0];
        let target_type = &args[1];

        match (obj, target_type) {
            (Value::CustomTypeInstance { id, .. }, Value::CustomType { how, .. }) => {
                // Store the new HOW in the rebless map so .HOW returns the new type
                self.rebless_map.insert(*id, *how.clone());
                Ok(obj.clone())
            }
            _ => Ok(args[0].clone()),
        }
    }

    /// Metamodel::Primitives.is_type($obj, $type)
    fn mp_is_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("is_type requires two arguments"));
        }

        let obj = &args[0];
        let type_check = &args[1];

        // Use the general smartmatch type checking
        let result = self.smart_match(obj, type_check);
        Ok(Value::Bool(result))
    }

    /// Check if a value matches a CustomType (used by smartmatch).
    /// Implements the Raku type checking protocol:
    /// - With cache: check cache directly, then call_accepts if set
    /// - Without cache (before compose): return false (default ACCEPTS behavior)
    pub(super) fn custom_type_check(&mut self, lhs: &Value, rhs_id: u64, rhs_how: &Value) -> bool {
        let data = self.custom_type_data.get(&rhs_id).cloned();
        if let Some(ref data) = data
            && let Some(ref cache) = data.type_check_cache
        {
            // If call_accepts is set, delegate to HOW.accepts_type
            if data.call_accepts
                && let Ok(result) = self.call_method_with_values(
                    rhs_how.clone(),
                    "accepts_type",
                    vec![Value::Nil, lhs.clone()],
                )
            {
                return result.truthy();
            }

            // Cache-only check: identity-based matching
            for cached_type in cache {
                if lhs == cached_type {
                    return true;
                }
                if let Value::Package(type_name) = cached_type
                    && lhs.isa_check(&type_name.resolve())
                {
                    return true;
                }
            }

            if data.authoritative {
                return false;
            }
        }

        // No cache (before compose): call find_method for ACCEPTS on the HOW
        // (this is what Raku's MOP does), but the default ACCEPTS returns False.
        let _ = self.call_method_with_values(
            rhs_how.clone(),
            "find_method",
            vec![Value::Nil, Value::Str("ACCEPTS".to_string())],
        );
        false
    }
}
