use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch .Seq coercion method
    pub(super) fn dispatch_seq_coercion(&mut self, target: Value) -> Result<Value, RuntimeError> {
        Ok(match target {
            Value::Seq(_) => target,
            Value::Array(items, ..) => Value::Seq(items),
            Value::Slip(items) => Value::Seq(items),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    self.supply_emit_buffer.pop().unwrap_or_default()
                } else if attributes.get("values").is_some() {
                    self.supply_list_values(&attributes, true)?
                } else {
                    Vec::new()
                };
                Value::Seq(std::sync::Arc::new(values))
            }
            Value::LazyList(ll) => {
                let items = self.force_lazy_list_bridge(&ll)?;
                Value::Seq(std::sync::Arc::new(items))
            }
            other @ (Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. }) => {
                let items = Self::value_to_list(&other);
                Value::Seq(std::sync::Arc::new(items))
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if {
                let cn = class_name.resolve();
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn == "utf16"
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
            } =>
            {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    Value::Seq(items.clone())
                } else {
                    Value::Seq(std::sync::Arc::new(Vec::new()))
                }
            }
            other => Value::Seq(std::sync::Arc::new(vec![other])),
        })
    }

    /// Dispatch .List coercion method
    pub(super) fn dispatch_list_coercion(&self, target: Value) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } = target
        {
            let cn = class_name.resolve();
            if cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
            {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    return Ok(Value::Array(items.clone(), crate::value::ArrayKind::List));
                }
                return Ok(Value::Array(
                    std::sync::Arc::new(Vec::new()),
                    crate::value::ArrayKind::List,
                ));
            }
        }
        let items = Self::value_to_list(&target);
        Ok(Value::Array(
            std::sync::Arc::new(items),
            crate::value::ArrayKind::List,
        ))
    }

    /// Dispatch .Setty / .Baggy / .Mixy coercion method
    pub(super) fn dispatch_setty_baggy_mixy(
        &self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // Role-ish quant hash family conversion on type objects.
        // Raku maps Set/Bag/Mix families to the corresponding family,
        // preserving hash flavor for *Hash type objects.
        let source_type = match target {
            Value::Package(name) => Some(name.resolve()),
            Value::Set(_, _) => Some("Set".to_string()),
            Value::Bag(_, _) => Some("Bag".to_string()),
            Value::Mix(_, _) => Some("Mix".to_string()),
            _ => None,
        };
        if let Some(source_type) = source_type
            && matches!(
                source_type.as_str(),
                "Set" | "SetHash" | "Bag" | "BagHash" | "Mix" | "MixHash"
            )
        {
            let hashy = matches!(source_type.as_str(), "SetHash" | "BagHash" | "MixHash");
            let mapped = match method {
                "Setty" => {
                    if hashy {
                        "SetHash"
                    } else {
                        "Set"
                    }
                }
                "Baggy" => {
                    if hashy {
                        "BagHash"
                    } else {
                        "Bag"
                    }
                }
                _ => {
                    if hashy {
                        "MixHash"
                    } else {
                        "Mix"
                    }
                }
            };
            return Some(Ok(Value::Package(Symbol::intern(mapped))));
        }
        None
    }
}
