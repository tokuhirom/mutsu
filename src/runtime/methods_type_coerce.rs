use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch .Seq coercion method
    pub(super) fn dispatch_seq_coercion(&mut self, target: Value) -> Result<Value, RuntimeError> {
        // Structural receivers (Seq/Array/Slip/Range/bare scalar) share the pure
        // impl with the VM native path; Supply/LazyList/Instance need state and
        // are handled below.
        if let Some(seq) = crate::builtins::seq_coerce::to_seq_structural(&target) {
            return Ok(seq);
        }
        Ok(match target.view() {
            ValueView::Seq(_) => target.clone(),
            ValueView::Array(items, ..) => Value::seq(items.to_vec()),
            ValueView::Slip(items) => Value::seq_arc(items.clone()),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let values =
                    if let Some(on_demand_cb) = attributes.as_map().get("on_demand_callback") {
                        let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                            let mut a = HashMap::new();
                            a.insert("emitted".to_string(), Value::array(Vec::new()));
                            a.insert("done".to_string(), Value::FALSE);
                            a
                        });
                        self.supply_emit_buffer.push(Vec::new());
                        let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                        self.supply_emit_buffer.pop().unwrap_or_default()
                    } else if attributes.as_map().get("values").is_some() {
                        self.supply_list_values(&attributes.as_map(), true)?
                    } else {
                        Vec::new()
                    };
                Value::seq(values)
            }
            ValueView::LazyList(ll) => {
                let items = self.force_lazy_list_bridge(ll)?;
                Value::seq(items)
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => {
                let items = Self::value_to_list(&target);
                Value::seq(items)
            }
            ValueView::Instance {
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
                if let Some(ValueView::Array(items, ..)) =
                    attributes.as_map().get("bytes").map(Value::view)
                {
                    Value::seq(items.clone().to_vec())
                } else {
                    Value::seq(Vec::new())
                }
            }
            _ => Value::seq(vec![target.clone()]),
        })
    }

    /// `.List`/`.Array` on a shaped array flattens all dimensions to a plain
    /// sequence of elements, with uninitialized (`Nil`) slots replaced by the
    /// container's type-default (`Any`, or the native/typed element default):
    /// `my @a[2;2]` → `(Any, Any, Any, Any)`, `my Int @a[3]` → `(Int, Int, Int)`.
    /// Returns `None` when `target` is not a shaped array (normal coercion runs).
    pub(super) fn shaped_flatten_with_default(&mut self, target: &Value) -> Option<Vec<Value>> {
        let depth = crate::runtime::utils::shaped_array_shape(target)?.len();
        let default = self.typed_container_default(target);
        let mut out = Vec::new();
        Self::flatten_shaped_dims(target, depth, &default, &mut out);
        Some(out)
    }

    fn flatten_shaped_dims(v: &Value, depth: usize, default: &Value, out: &mut Vec<Value>) {
        match v {
            Value::Array(items, _) if depth > 0 => {
                for it in items.iter() {
                    Self::flatten_shaped_dims(it, depth - 1, default, out);
                }
            }
            Value::Nil => out.push(default.clone()),
            other => out.push(other.clone()),
        }
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
                if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                    return Ok(Value::Array(items.clone(), crate::value::ArrayKind::List));
                }
                return Ok(Value::Array(
                    crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                    crate::value::ArrayKind::List,
                ));
            }
        }
        // A genuinely-lazy list (`(1…∞).List`) must STAY lazy: materializing it
        // would lose the infinite tail (and `.elems`/`eqv` must still throw
        // X::Cannot::Lazy). Tag it as `.List`-coerced so `.WHAT` reports `List`
        // while the generator is untouched.
        if let Value::LazyList(ll) = &target
            && ll.is_genuinely_lazy()
        {
            return Ok(Value::LazyList(crate::gc::Gc::new(ll.with_list_context())));
        }
        let items = Self::value_to_list(&target);
        Ok(Value::Array(
            crate::gc::Gc::new(crate::value::ArrayData::new(items)),
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
