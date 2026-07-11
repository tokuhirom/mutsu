use super::*;
use crate::symbol::Symbol;

const SELF_HASH_REF_SENTINEL: &str = "__mutsu_self_hash_ref";
const SELF_ARRAY_REF_SENTINEL: &str = "__mutsu_self_array_ref";

impl Interpreter {
    pub(super) fn range_end_is_unbounded(end: i64) -> bool {
        end == i64::MAX
    }

    /// When binding a Proxy to a variable, update the captured envs of its
    /// FETCH/STORE closures to include the Proxy itself under the variable name.
    /// This simulates capture-by-reference for the common Proxy binding pattern:
    ///   `my $proxy := Proxy.new(STORE => -> $, \v { $proxy.VAR... })`
    pub(super) fn update_proxy_closure_envs(val: Value, var_name: &str) -> Value {
        if !matches!(val.view(), ValueView::Proxy { .. }) {
            return val;
        }
        let Some((new_fetcher, mut new_storer, subclass, _)) = val.into_proxy_parts() else {
            unreachable!("probed as Proxy above");
        };
        let mut storer_updated = false;

        // Only update the STORE closure (not FETCH) — STORE is the one that
        // typically references the Proxy variable (e.g., $proxy.VAR.history.push(...))
        if let ValueView::Sub(data) = new_storer.view()
            && data.env.contains_key(var_name)
        {
            storer_updated = true;
        }

        if !storer_updated {
            return Value::proxy_parts(new_fetcher, new_storer, subclass, false);
        }

        // Build the final Proxy value with updated closures.
        // We need two passes: first update the closures, then create the Proxy
        // that references the updated closures.
        if storer_updated {
            // Build a proxy reference to inject into the STORE closure.
            // Use the original (unmodified) closures to create a stable reference.
            let proxy_for_env = Value::proxy_parts(
                new_fetcher.clone(),
                new_storer.clone(),
                subclass.clone(),
                false,
            );
            new_storer.with_sub_mut(|data| {
                let data = crate::gc::Gc::make_mut(data);
                data.env.insert(var_name.to_string(), proxy_for_env);
            });
        }

        Value::proxy_parts(new_fetcher, new_storer, subclass, false)
    }

    pub(super) fn self_hash_ref_marker() -> Value {
        Value::pair(SELF_HASH_REF_SENTINEL.to_string(), Value::TRUE)
    }

    pub(super) fn resolve_hash_entry(
        &self,
        items: &crate::gc::Gc<crate::value::HashData>,
        key: &str,
    ) -> Value {
        match items.get(key) {
            Some(value) => match value.view() {
                ValueView::Pair(name, _) if name == SELF_HASH_REF_SENTINEL => {
                    Value::hash_with_data(items.clone())
                }
                // Phase 2 element container: a `:=`-bound entry holds a shared
                // `ContainerRef` cell; decontainerize on read (the chokepoint).
                ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
                _ => value.clone(),
            },
            None => Value::NIL,
        }
    }

    /// Check if a hash contains any sentinel entries (bound refs or self-refs)
    /// that need resolution before the hash can be iterated.
    pub(super) fn hash_has_sentinels(items: &HashMap<String, Value>) -> bool {
        items
            .values()
            .any(|v| matches!(v.view(), ValueView::Pair(name, _) if name == SELF_HASH_REF_SENTINEL))
    }

    /// Resolve all sentinel entries in a hash, returning a new hash with
    /// bound variable references replaced by their current values.
    pub(super) fn resolve_hash_for_iteration(
        &self,
        items: &crate::gc::Gc<crate::value::HashData>,
    ) -> Value {
        let mut resolved = HashMap::new();
        for (key, value) in items.iter() {
            let resolved_value = match value.view() {
                ValueView::Pair(name, _) if name == SELF_HASH_REF_SENTINEL => {
                    Value::hash_with_data(items.clone())
                }
                // Decont a `:=`-bound shared cell: the resolved copy
                // snapshots the current value (assignment semantics).
                ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
                _ => value.clone(),
            };
            resolved.insert(key.clone(), resolved_value);
        }
        Value::hash_with_data(Value::hash_arc(resolved))
    }

    pub(super) fn self_array_ref_marker() -> Value {
        Value::pair(SELF_ARRAY_REF_SENTINEL.to_string(), Value::TRUE)
    }

    pub(super) fn resolve_array_entry(
        &self,
        items: &crate::gc::Gc<crate::value::ArrayData>,
        kind: ArrayKind,
        idx: usize,
        default: Value,
    ) -> Value {
        match items.get(idx) {
            Some(value) => match value.view() {
                ValueView::Pair(name, _) if name == SELF_ARRAY_REF_SENTINEL => {
                    Value::array_with_kind(items.clone(), kind)
                }
                // If the element is a hole (Package("Any") from deletion or
                // uninitialized gap) and a non-Nil default is available,
                // return the default instead of the hole value.
                ValueView::Package(name) if name == "Any" && !default.is_nil() => default,
                // Shaped arrays are pre-allocated with Nil placeholders; an
                // uninitialized in-range slot reads as the element default
                // (e.g. 0 for `array[int]`). Non-shaped arrays may legitimately
                // hold Nil values, so this only applies to Shaped arrays.
                ValueView::Nil if kind == ArrayKind::Shaped && !default.is_nil() => default,
                // Phase 2 element container: a `:=`-bound element holds a shared
                // `ContainerRef` cell. Reading the element decontainerizes it (the
                // single read chokepoint), so value contexts never see the cell.
                ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
                _ => value.clone(),
            },
            None => default,
        }
    }

    fn is_method_not_found(err: &RuntimeError) -> bool {
        err.is_method_not_found()
    }

    fn call_exists_pos(&mut self, instance: &Value, idx: Value) -> Result<bool, RuntimeError> {
        // Choose the associative vs positional existence method by the index type,
        // mirroring the AT-KEY/AT-POS read dispatch (vm_var_index_ops): a string
        // index is an associative subscript (`$o<k>` / `$o{'k'}`) -> EXISTS-KEY;
        // any other (typically Int) index is positional (`$o[i]`) -> EXISTS-POS,
        // falling back to EXISTS-KEY for an object that only does Associative
        // (e.g. zef's config object, which `handles <... EXISTS-KEY ...>` to a Hash
        // attribute and defines no EXISTS-POS).
        let method_order: &[&str] = match idx.view() {
            ValueView::Str(_) => &["EXISTS-KEY"],
            _ => &["EXISTS-POS", "EXISTS-KEY"],
        };
        for method in method_order {
            match self.try_compiled_method_or_interpret(instance.clone(), method, vec![idx.clone()])
            {
                Ok(value) => return Ok(value.truthy()),
                Err(err) if Self::is_method_not_found(&err) => continue,
                Err(err) => return Err(err),
            }
        }
        Ok(false)
    }

    pub(super) fn instance_exists_pos_result(
        &mut self,
        instance: &Value,
        idx: &Value,
        effective_negated: bool,
        adverb_bits: u32,
    ) -> Result<Option<Value>, RuntimeError> {
        let pairs: Vec<(Value, bool)> = match idx.view() {
            ValueView::Array(items, ..) => {
                if let [only] = items.as_slice()
                    && matches!(only.view(), ValueView::Whatever)
                {
                    let elems = self
                        .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                        .unwrap_or(Value::int(0));
                    let len = crate::runtime::to_int(&elems).max(0) as usize;
                    let mut pairs = Vec::with_capacity(len);
                    for i in 0..len {
                        let key = Value::int(i as i64);
                        pairs.push((key.clone(), self.call_exists_pos(instance, key)?));
                    }
                    pairs
                } else if let [only] = items.as_slice()
                    && let ValueView::Num(f) = only.view()
                {
                    if f.is_infinite() && f.is_sign_positive() {
                        let elems = self
                            .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                            .unwrap_or(Value::int(0));
                        let len = crate::runtime::to_int(&elems).max(0) as usize;
                        let mut pairs = Vec::with_capacity(len);
                        for i in 0..len {
                            let key = Value::int(i as i64);
                            pairs.push((key.clone(), self.call_exists_pos(instance, key)?));
                        }
                        pairs
                    } else {
                        let mut pairs = Vec::with_capacity(items.len());
                        for key in items.iter() {
                            pairs.push((key.clone(), self.call_exists_pos(instance, key.clone())?));
                        }
                        pairs
                    }
                } else {
                    let mut pairs = Vec::with_capacity(items.len());
                    for key in items.iter() {
                        pairs.push((key.clone(), self.call_exists_pos(instance, key.clone())?));
                    }
                    pairs
                }
            }
            ValueView::Whatever => {
                let elems = self
                    .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                    .unwrap_or(Value::int(0));
                let len = crate::runtime::to_int(&elems).max(0) as usize;
                let mut pairs = Vec::with_capacity(len);
                for i in 0..len {
                    let key = Value::int(i as i64);
                    pairs.push((key.clone(), self.call_exists_pos(instance, key)?));
                }
                pairs
            }
            ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                let elems = self
                    .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                    .unwrap_or(Value::int(0));
                let len = crate::runtime::to_int(&elems).max(0) as usize;
                let mut pairs = Vec::with_capacity(len);
                for i in 0..len {
                    let key = Value::int(i as i64);
                    pairs.push((key.clone(), self.call_exists_pos(instance, key)?));
                }
                pairs
            }
            _ => vec![(idx.clone(), self.call_exists_pos(instance, idx.clone())?)],
        };

        let is_multi = pairs.len() != 1
            || matches!(idx.view(), ValueView::Array(..) | ValueView::Whatever)
            || matches!(idx.view(), ValueView::Num(f) if f.is_infinite() && f.is_sign_positive());

        let result = if !is_multi {
            Value::truth(pairs[0].1 ^ effective_negated)
        } else {
            match adverb_bits {
                0 | 5 => Value::array(
                    pairs
                        .iter()
                        .map(|(_, exists)| Value::truth(*exists ^ effective_negated))
                        .collect(),
                ),
                1 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        if *exists {
                            vals.push(key.clone());
                            vals.push(Value::truth(*exists ^ effective_negated));
                        }
                    }
                    Value::array(vals)
                }
                2 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        vals.push(key.clone());
                        vals.push(Value::truth(*exists ^ effective_negated));
                    }
                    Value::array(vals)
                }
                3 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        if *exists {
                            vals.push(Value::value_pair(
                                key.clone(),
                                Value::truth(*exists ^ effective_negated),
                            ));
                        }
                    }
                    Value::array(vals)
                }
                4 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        vals.push(Value::value_pair(
                            key.clone(),
                            Value::truth(*exists ^ effective_negated),
                        ));
                    }
                    Value::array(vals)
                }
                _ => Value::NIL,
            }
        };

        Ok(Some(result))
    }

    pub(crate) fn typed_container_default(&mut self, target: &Value) -> Value {
        // Check for explicit `is default(...)` on the container first.
        if let Some(def) = self.container_default(target) {
            return def.clone();
        }
        if let Some(info) = self.container_type_metadata(target) {
            // Native typed arrays default their elements to the native type's
            // zero value rather than the (uninstantiable) type object:
            // int -> 0, num -> 0e0, str -> "".
            if let Some(def) = native_element_default(&info.value_type) {
                return def;
            }
            Value::package(Symbol::intern(&info.value_type))
        } else if matches!(target.view(), ValueView::Hash(_))
            || matches!(target.view(), ValueView::Array(_, kind) if kind.is_real_array())
        {
            // An untyped Array/Hash defaults its missing elements to the `Any`
            // type object (raku: `my @a; @a[5]` is `Any`), not `Nil`. A *List*
            // (`(1,2,3)[11]`, `()[0]`) is out-of-range → `Nil`, so only a real
            // `Array` (`[...]`/`my @a`) gets the `Any` default.
            Value::package(Symbol::intern("Any"))
        } else {
            Value::NIL
        }
    }

    pub(super) fn anon_state_key(name: &str) -> Option<String> {
        if name.starts_with("__ANON_STATE_") {
            Some(format!("__anon_state::{name}"))
        } else {
            None
        }
    }

    /// Convert a Failure's exception Value into a RuntimeError.
    fn failure_to_error(exception: &Value) -> RuntimeError {
        let message = if let ValueView::Instance { attributes, .. } = exception.view() {
            attributes
                .as_map()
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Died".to_string())
        } else {
            "Died".to_string()
        };
        let mut err = RuntimeError::new(&message);
        err.exception = Some(Box::new(exception.clone()));
        err
    }

    /// If the value is a Failure, throw its contained exception.
    pub(super) fn throw_if_failure(value: &Value) -> Result<(), RuntimeError> {
        match value.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                if let Some(ex) = attributes.as_map().get("exception") {
                    return Err(Self::failure_to_error(ex));
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn anon_state_value(&self, name: &str) -> Option<Value> {
        let key = Self::anon_state_key(name)?;
        self.get_state_var(&key).cloned()
    }

    pub(super) fn sync_anon_state_value(&mut self, name: &str, value: &Value) {
        if let Some(key) = Self::anon_state_key(name) {
            loan_env!(self, set_state_var(key, value.clone()));
        }
    }

    pub(super) fn term_symbol_from_name(name: &str) -> Option<&str> {
        let bytes = name.as_bytes();
        if bytes.is_empty() {
            return None;
        }
        let starts_like_term = if bytes[0] == b't' {
            name.starts_with("term:<")
        } else if bytes[0] == b'&' {
            name.starts_with("&term:<")
        } else {
            false
        };
        if !starts_like_term {
            return None;
        }
        name.strip_prefix("term:<")
            .or_else(|| name.strip_prefix("&term:<"))
            .and_then(|s| s.strip_suffix('>'))
    }
}

/// The default ("zero") value for a native array element type.
/// Native integer types default to 0, native floats (num/num32/num64) to 0e0,
/// and native `str` to the empty string. Returns `None` for non-native types
/// (e.g. `Int`, `Str`), which keep their type-object default.
pub(super) fn native_element_default(value_type: &str) -> Option<Value> {
    if crate::runtime::native_types::is_native_int_type(value_type) {
        Some(Value::int(0))
    } else if matches!(value_type, "num" | "num32" | "num64") {
        Some(Value::num(0.0))
    } else if value_type == "str" {
        Some(Value::str_from(""))
    } else {
        None
    }
}
