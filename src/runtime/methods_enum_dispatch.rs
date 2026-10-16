use super::*;
use crate::symbol::Symbol;


impl Interpreter {
    /// `my Array enum E (:a[1, 2])`: a member is also an instance of the base
    /// type, so the base's collection methods answer. The base value when `target`
    /// is such a member and `method` is one of those methods.
    // Cost: O(1).
    pub(crate) fn enum_aggregate_base(target: &Value, method: &str) -> Option<Value> {
        if !matches!(
            method,
            "elems" | "end" | "list" | "List" | "Array" | "max" | "min" | "map" | "grep" | "first"
                | "flat" | "join" | "reverse" | "sort" | "keys" | "values" | "head" | "tail" | "sum"
                | "pairs" | "iterator" | "AT-POS" | "EXISTS-POS" | "Seq" | "rotate" | "batch"
                | "produce" | "reduce" | "unique" | "squish" | "pick" | "roll"
        ) {
            return None;
        }
        let ValueView::Enum {
            value: EnumValue::Generic(inner),
            ..
        } = target.view()
        else {
            return None;
        };
        matches!(inner.view(), ValueView::Array(..) | ValueView::Hash(_))
            .then(|| inner.as_ref().clone())
    }

    /// The methods of an enum member: its base type's collection methods for
    /// an `Array`/`Hash`-valued member ([`Self::enum_aggregate_base`]), then
    /// the rows of the roles Rakudo composes into every enum type
    /// (`Enumeration`, `NumericEnumeration`, `StringyEnumeration`; ADR-11276
    /// §9.125), found by their owner. `None` when neither answers.
    // Cost: O(o) to find the row, o = the member's roles, plus the row's own.
    pub(super) fn dispatch_enum_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let owners = crate::builtins::method_table::enumeration_owners_of(target)?;
        if let Some(base) = Self::enum_aggregate_base(target, method) {
            return Some(self.call_method_with_values(base, method, args.to_vec()));
        }
        crate::builtins::method_table::invoke_owner(self, owners, method, args, || {
            target.clone()
        })
    }

    /// Dispatch collection methods (.pairs, .keys, .values, .kv, .antipairs, .invert)
    /// on an enum type object (Package value).
    pub(super) fn dispatch_enum_type_collection(
        &self,
        method: &str,
        variants: &[(String, EnumValue)],
    ) -> Result<Value, RuntimeError> {
        match method {
            "pairs" => {
                // ADR-0021 I2: data-minted pairs default positional.
                let pairs: Vec<Value> = variants
                    .iter()
                    .map(|(k, v)| Value::value_pair(Value::str(k.clone()), v.to_value()))
                    .collect();
                Ok(Value::array(pairs))
            }
            "keys" => {
                let keys: Vec<Value> = variants
                    .iter()
                    .map(|(k, _)| Value::str(k.clone()))
                    .collect();
                Ok(Value::array(keys))
            }
            "values" => {
                let values: Vec<Value> = variants.iter().map(|(_, v)| v.to_value()).collect();
                Ok(Value::array(values))
            }
            "kv" => {
                let mut kv = Vec::with_capacity(variants.len() * 2);
                for (k, v) in variants {
                    kv.push(Value::str(k.clone()));
                    kv.push(v.to_value());
                }
                Ok(Value::array(kv))
            }
            "antipairs" => {
                let pairs: Vec<Value> = variants
                    .iter()
                    .map(|(k, v)| Value::value_pair(v.to_value(), Value::str(k.clone())))
                    .collect();
                Ok(Value::array(pairs))
            }
            "invert" => {
                let mut pairs = Vec::new();
                for (k, v) in variants {
                    let val = v.to_value();
                    match val.view() {
                        ValueView::Array(items, _) => {
                            for item in items.as_ref() {
                                pairs.push(Value::value_pair(item.clone(), Value::str(k.clone())));
                            }
                        }
                        _ => {
                            pairs.push(Value::value_pair(val.clone(), Value::str(k.clone())));
                        }
                    }
                }
                Ok(Value::array(pairs))
            }
            _ => Ok(Value::array(Vec::new())),
        }
    }
}
