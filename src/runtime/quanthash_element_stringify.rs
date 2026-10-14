//! Stringifying a `Set`/`Bag`/`Mix` whose elements define their own `Str`/`gist`.
//!
//! The pure renderers (`Value::to_string_value`, `setbagmix_gist_named`)
//! cannot call a method, so an `Instance` element printed as `F<16>`.
//! Rakudo's `Setty.Str` calls each element's `Str`, and `Baggy.Str`,
//! `Mixy.Str` and every `.gist` call each element's `gist`.
//!
//! The same mechanism as `list_element_stringify.rs`: the object elements are
//! resolved in place -- each replaced by the string its class dispatches --
//! and the rebuilt quant hash goes to the same pure renderer. A plain `Str`
//! element renders bare in both `Str` and `gist`, so the substitution is exact.

use crate::value::{RuntimeError, Value, ValueView};

/// Whether a store key's element object has a user-definable stringifier.
fn is_object_element(v: &Value) -> bool {
    matches!(v.view(), ValueView::Instance { .. } | ValueView::Mixin(..))
}

impl crate::Interpreter {
    /// Whether this value is a quant hash holding an `Instance`/`Mixin` element.
    // Cost: O(n), n = elements; allocation-free after the tag probe.
    pub(crate) fn quanthash_needs_element_dispatch(value: &Value) -> bool {
        match value.view() {
            ValueView::Set(s, _) => {
                s.original_keys.is_some() && s.iter().any(|k| is_object_element(&s.typed_key(k)))
            }
            ValueView::Bag(b, _) => {
                b.original_keys.is_some() && b.keys().any(|k| is_object_element(&b.typed_key(k)))
            }
            ValueView::Mix(m, _) => {
                m.original_keys.is_some() && m.keys().any(|k| is_object_element(&m.typed_key(k)))
            }
            _ => false,
        }
    }

    /// The method Rakudo calls on each element when `method` (`Str`/`Stringy`
    /// or `gist`) renders this quant hash: `Setty.Str` uses `Str`, the rest `gist`.
    fn quanthash_element_method(value: &Value, method: &str) -> &'static str {
        if matches!(value.view(), ValueView::Set(..)) && method != "gist" {
            "Str"
        } else {
            "gist"
        }
    }

    /// Replace every object element of a quant hash by the string its class's
    /// `Str`/`gist` dispatches. Anything else is returned unchanged.
    // Cost: O(n) element-method calls, n = elements.
    pub(crate) fn resolve_quanthash_element_stringifiers(
        &mut self,
        value: &Value,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        if !Self::quanthash_needs_element_dispatch(value) {
            return Ok(value.clone());
        }
        let call = Self::quanthash_element_method(value, method);
        match value.view() {
            ValueView::Set(data, is_mut) => {
                let mut rebuilt = crate::value::SetData::clone(&data);
                let keys: Vec<String> = data.iter().cloned().collect();
                for k in keys {
                    let typed = data.typed_key(&k);
                    if !is_object_element(&typed) {
                        continue;
                    }
                    let new_key = self.element_str_key(typed, call)?;
                    rebuilt.elements.remove(&k);
                    if let Some(o) = rebuilt.original_keys.as_mut() {
                        o.remove(&k);
                    }
                    rebuilt.elements.insert(new_key);
                }
                Ok(Value::set_parts(crate::gc::Gc::new(rebuilt), is_mut))
            }
            ValueView::Bag(data, is_mut) => {
                let mut rebuilt = crate::value::BagData::clone(&data);
                let keys: Vec<String> = data.keys().cloned().collect();
                for k in keys {
                    let typed = data.typed_key(&k);
                    if !is_object_element(&typed) {
                        continue;
                    }
                    let new_key = self.element_str_key(typed, call)?;
                    let n = rebuilt.counts.remove(&k).unwrap_or_default();
                    if let Some(o) = rebuilt.original_keys.as_mut() {
                        o.remove(&k);
                    }
                    *rebuilt.counts.entry(new_key).or_default() += n;
                }
                Ok(Value::bag_parts(crate::gc::Gc::new(rebuilt), is_mut))
            }
            ValueView::Mix(data, is_mut) => {
                let mut rebuilt = crate::value::MixData::clone(&data);
                let keys: Vec<String> = data.keys().cloned().collect();
                for k in keys {
                    let typed = data.typed_key(&k);
                    if !is_object_element(&typed) {
                        continue;
                    }
                    let new_key = self.element_str_key(typed, call)?;
                    let w = rebuilt.weights.remove(&k).unwrap_or_default();
                    if let Some(o) = rebuilt.original_keys.as_mut() {
                        o.remove(&k);
                    }
                    let e = rebuilt.weights.entry(new_key).or_default();
                    *e = crate::builtins::mix_weight::add(e.clone(), w);
                }
                Ok(Value::mix_parts(crate::gc::Gc::new(rebuilt), is_mut))
            }
            _ => Ok(value.clone()),
        }
    }

    /// The store key of a plain `Str` element holding `elem.<call>()`.
    fn element_str_key(&mut self, elem: Value, call: &str) -> Result<String, RuntimeError> {
        let s = self.call_method_with_values_unviewed(elem, call, vec![])?;
        Ok(format!("Str|{}", s.to_string_value()))
    }
}
