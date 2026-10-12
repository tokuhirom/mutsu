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

    /// Dispatch methods on Enum values.
    /// Returns Some(result) if handled, None if not an enum method.
    pub(super) fn dispatch_enum_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Enum {
            enum_type,
            key,
            value,
            index,
        } = target.view()
        else {
            return None;
        };
        if let Some(base) = Self::enum_aggregate_base(target, method) {
            return Some(self.call_method_with_values(base, method, args.to_vec()));
        }

        match method {
            "key" => Some(Ok(Value::str(key.resolve()))),
            "value" => Some(Ok(value.to_value())),
            // A numeric coercion of an enum member acts on its underlying value:
            // `cool.Int` for `enum Numbers (cool => '42')` is `'42'.Int` == 42,
            // and `sqrt-n-one.Real` for a `'i'` value raises the string-numeric
            // error, exactly as raku does.
            "Int" | "Numeric" | "Real" | "Num" | "Rat" | "Complex" => {
                let underlying = value.to_value();
                Some(self.call_method_with_values(underlying, method, args.to_vec()))
            }
            "WHAT" => Some(Ok(Value::package(enum_type))),
            "raku" | "perl" => Some(Ok(Value::str(
                crate::builtins::methods_0arg::raku_repr::enum_raku_repr(
                    &enum_type.resolve(),
                    &key.resolve(),
                ),
            ))),
            "gist" => Some(Ok(Value::str(key.resolve()))),
            "Str" => match value {
                EnumValue::Int(_) => Some(Ok(Value::str(key.resolve()))),
                EnumValue::Str(s) => Some(Ok(Value::str(s.clone()))),
                EnumValue::Generic(_) => Some(Ok(Value::str(key.resolve()))),
            },
            "kv" => {
                let val = value.to_value();
                Some(Ok(Value::array(vec![Value::str(key.resolve()), val])))
            }
            "pair" => {
                let val = value.to_value();
                // ADR-0021 I2: data-minted pairs default positional.
                Some(Ok(Value::value_pair(Value::str(key.resolve()), val)))
            }
            "ACCEPTS" => {
                if args.is_empty() {
                    return Some(Err(RuntimeError::new("ACCEPTS requires an argument")));
                }
                let other = &args[0];
                Some(Ok(Value::truth(match other.view() {
                    ValueView::Enum {
                        enum_type: other_type,
                        key: other_key,
                        ..
                    } => enum_type == other_type && key == other_key,
                    _ => false,
                })))
            }
            "pred" => {
                if index == 0 {
                    // .pred on first element returns itself
                    return Some(Ok(target.clone()));
                }
                if let Some(variants) = self.registry().enum_types.get(&enum_type.resolve())
                    && let Some((prev_key, prev_val)) = variants.get(index - 1)
                {
                    return Some(Ok(Value::enum_parts(
                        enum_type,
                        Symbol::intern(prev_key),
                        prev_val.clone(),
                        index - 1,
                    )));
                }
                // Fallback: return self if variants not found
                Some(Ok(target.clone()))
            }
            "succ" => {
                if let Some(variants) = self.registry().enum_types.get(&enum_type.resolve()) {
                    if let Some((next_key, next_val)) = variants.get(index + 1) {
                        return Some(Ok(Value::enum_parts(
                            enum_type,
                            Symbol::intern(next_key),
                            next_val.clone(),
                            index + 1,
                        )));
                    }
                    // .succ on last element returns itself
                    return Some(Ok(target.clone()));
                }
                // Fallback: return self if variants not found
                Some(Ok(target.clone()))
            }
            _ => None,
        }
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
