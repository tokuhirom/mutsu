use super::*;
use crate::symbol::Symbol;
use num_traits::Zero;
use std::sync::Arc;

const SELF_HASH_REF_SENTINEL: &str = "__mutsu_self_hash_ref";
pub(super) const BOUND_HASH_REF_SENTINEL: &str = "__mutsu_bound_hash_ref";
const SELF_ARRAY_REF_SENTINEL: &str = "__mutsu_self_array_ref";
pub(super) const BOUND_ARRAY_REF_SENTINEL: &str = "__mutsu_bound_array_ref";

impl VM {
    pub(super) fn range_end_is_unbounded(end: i64) -> bool {
        end == i64::MAX
    }

    /// When binding a Proxy to a variable, update the captured envs of its
    /// FETCH/STORE closures to include the Proxy itself under the variable name.
    /// This simulates capture-by-reference for the common Proxy binding pattern:
    ///   `my $proxy := Proxy.new(STORE => -> $, \v { $proxy.VAR... })`
    pub(super) fn update_proxy_closure_envs(val: Value, var_name: &str) -> Value {
        if let Value::Proxy {
            fetcher,
            storer,
            subclass,
            ..
        } = val
        {
            let new_fetcher = *fetcher;
            let mut new_storer = *storer;
            let mut storer_updated = false;

            // Only update the STORE closure (not FETCH) — STORE is the one that
            // typically references the Proxy variable (e.g., $proxy.VAR.history.push(...))
            if let Value::Sub(ref data) = new_storer
                && data.env.contains_key(var_name)
            {
                storer_updated = true;
            }

            if !storer_updated {
                return Value::Proxy {
                    fetcher: Box::new(new_fetcher),
                    storer: Box::new(new_storer),
                    subclass,
                    decontainerized: false,
                };
            }

            // Build the final Proxy value with updated closures.
            // We need two passes: first update the closures, then create the Proxy
            // that references the updated closures.
            if storer_updated {
                // Build a proxy reference to inject into the STORE closure.
                // Use the original (unmodified) closures to create a stable reference.
                let proxy_for_env = Value::Proxy {
                    fetcher: Box::new(new_fetcher.clone()),
                    storer: Box::new(new_storer.clone()),
                    subclass: subclass.clone(),
                    decontainerized: false,
                };
                if let Value::Sub(ref mut data) = new_storer {
                    let data = Arc::make_mut(data);
                    data.env.insert(var_name.to_string(), proxy_for_env);
                }
            }

            Value::Proxy {
                fetcher: Box::new(new_fetcher),
                storer: Box::new(new_storer),
                subclass,
                decontainerized: false,
            }
        } else {
            val
        }
    }

    pub(super) fn self_hash_ref_marker() -> Value {
        Value::Pair(
            SELF_HASH_REF_SENTINEL.to_string(),
            Box::new(Value::Bool(true)),
        )
    }

    pub(super) fn resolve_hash_entry(
        &self,
        items: &Arc<HashMap<String, Value>>,
        key: &str,
    ) -> Value {
        match items.get(key) {
            Some(Value::Pair(name, _)) if name == SELF_HASH_REF_SENTINEL => {
                Value::Hash(items.clone())
            }
            Some(Value::Pair(name, source)) if name == BOUND_HASH_REF_SENTINEL => {
                let source_name = source.to_string_value();
                self.interpreter
                    .env()
                    .get(&source_name)
                    .cloned()
                    .unwrap_or(Value::Nil)
            }
            Some(value) => value.clone(),
            None => Value::Nil,
        }
    }

    /// Check if a hash contains any sentinel entries (bound refs or self-refs)
    /// that need resolution before the hash can be iterated.
    pub(super) fn hash_has_sentinels(items: &HashMap<String, Value>) -> bool {
        items.values().any(|v| {
            matches!(v, Value::Pair(name, _) if name == SELF_HASH_REF_SENTINEL || name == BOUND_HASH_REF_SENTINEL)
        })
    }

    /// Resolve all sentinel entries in a hash, returning a new hash with
    /// bound variable references replaced by their current values.
    pub(super) fn resolve_hash_for_iteration(&self, items: &Arc<HashMap<String, Value>>) -> Value {
        let mut resolved = HashMap::new();
        for (key, value) in items.iter() {
            let resolved_value = match value {
                Value::Pair(name, _) if name == SELF_HASH_REF_SENTINEL => {
                    Value::Hash(items.clone())
                }
                Value::Pair(name, source) if name == BOUND_HASH_REF_SENTINEL => {
                    let source_name = source.to_string_value();
                    self.interpreter
                        .env()
                        .get(&source_name)
                        .cloned()
                        .unwrap_or(Value::Nil)
                }
                other => other.clone(),
            };
            resolved.insert(key.clone(), resolved_value);
        }
        Value::Hash(Arc::new(resolved))
    }

    pub(super) fn self_array_ref_marker() -> Value {
        Value::Pair(
            SELF_ARRAY_REF_SENTINEL.to_string(),
            Box::new(Value::Bool(true)),
        )
    }

    pub(super) fn resolve_array_entry(
        &self,
        items: &Arc<Vec<Value>>,
        kind: ArrayKind,
        idx: usize,
        default: Value,
    ) -> Value {
        match items.get(idx) {
            Some(Value::Pair(name, _)) if name == SELF_ARRAY_REF_SENTINEL => {
                Value::Array(items.clone(), kind)
            }
            Some(Value::Pair(name, source)) if name == BOUND_ARRAY_REF_SENTINEL => {
                let source_name = source.to_string_value();
                self.interpreter
                    .env()
                    .get(&source_name)
                    .cloned()
                    .unwrap_or(Value::Nil)
            }
            // If the element is a hole (Package("Any") from deletion or
            // uninitialized gap) and a non-Nil default is available,
            // return the default instead of the hole value.
            Some(Value::Package(name)) if name == "Any" && !matches!(default, Value::Nil) => {
                default
            }
            Some(value) => value.clone(),
            None => default,
        }
    }

    fn is_method_not_found(err: &RuntimeError) -> bool {
        matches!(
            err.exception.as_deref(),
            Some(Value::Instance { class_name, .. }) if class_name == "X::Method::NotFound"
        )
    }

    fn call_exists_pos(&mut self, instance: &Value, idx: Value) -> Result<bool, RuntimeError> {
        match self.try_compiled_method_or_interpret(instance.clone(), "EXISTS-POS", vec![idx]) {
            Ok(value) => Ok(value.truthy()),
            Err(err) if Self::is_method_not_found(&err) => Ok(false),
            Err(err) => Err(err),
        }
    }

    pub(super) fn instance_exists_pos_result(
        &mut self,
        instance: &Value,
        idx: &Value,
        effective_negated: bool,
        adverb_bits: u32,
    ) -> Result<Option<Value>, RuntimeError> {
        let pairs: Vec<(Value, bool)> = match idx {
            Value::Array(items, ..) => {
                if let [Value::Whatever] = items.as_slice() {
                    let elems = self
                        .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                        .unwrap_or(Value::Int(0));
                    let len = crate::runtime::to_int(&elems).max(0) as usize;
                    let mut pairs = Vec::with_capacity(len);
                    for i in 0..len {
                        let key = Value::Int(i as i64);
                        pairs.push((key.clone(), self.call_exists_pos(instance, key)?));
                    }
                    pairs
                } else if let [Value::Num(f)] = items.as_slice() {
                    if f.is_infinite() && f.is_sign_positive() {
                        let elems = self
                            .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                            .unwrap_or(Value::Int(0));
                        let len = crate::runtime::to_int(&elems).max(0) as usize;
                        let mut pairs = Vec::with_capacity(len);
                        for i in 0..len {
                            let key = Value::Int(i as i64);
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
            Value::Whatever => {
                let elems = self
                    .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                    .unwrap_or(Value::Int(0));
                let len = crate::runtime::to_int(&elems).max(0) as usize;
                let mut pairs = Vec::with_capacity(len);
                for i in 0..len {
                    let key = Value::Int(i as i64);
                    pairs.push((key.clone(), self.call_exists_pos(instance, key)?));
                }
                pairs
            }
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                let elems = self
                    .try_compiled_method_or_interpret(instance.clone(), "elems", vec![])
                    .unwrap_or(Value::Int(0));
                let len = crate::runtime::to_int(&elems).max(0) as usize;
                let mut pairs = Vec::with_capacity(len);
                for i in 0..len {
                    let key = Value::Int(i as i64);
                    pairs.push((key.clone(), self.call_exists_pos(instance, key)?));
                }
                pairs
            }
            _ => vec![(idx.clone(), self.call_exists_pos(instance, idx.clone())?)],
        };

        let is_multi = pairs.len() != 1
            || matches!(idx, Value::Array(..) | Value::Whatever)
            || matches!(idx, Value::Num(f) if f.is_infinite() && f.is_sign_positive());

        let result = if !is_multi {
            Value::Bool(pairs[0].1 ^ effective_negated)
        } else {
            match adverb_bits {
                0 | 5 => Value::array(
                    pairs
                        .iter()
                        .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                        .collect(),
                ),
                1 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        if *exists {
                            vals.push(key.clone());
                            vals.push(Value::Bool(*exists ^ effective_negated));
                        }
                    }
                    Value::array(vals)
                }
                2 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        vals.push(key.clone());
                        vals.push(Value::Bool(*exists ^ effective_negated));
                    }
                    Value::array(vals)
                }
                3 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        if *exists {
                            vals.push(Value::ValuePair(
                                Box::new(key.clone()),
                                Box::new(Value::Bool(*exists ^ effective_negated)),
                            ));
                        }
                    }
                    Value::array(vals)
                }
                4 => {
                    let mut vals = Vec::new();
                    for (key, exists) in &pairs {
                        vals.push(Value::ValuePair(
                            Box::new(key.clone()),
                            Box::new(Value::Bool(*exists ^ effective_negated)),
                        ));
                    }
                    Value::array(vals)
                }
                _ => Value::Nil,
            }
        };

        Ok(Some(result))
    }

    pub(super) fn typed_container_default(&self, target: &Value) -> Value {
        // Check for explicit `is default(...)` on the container first.
        if let Some(def) = self.interpreter.container_default(target) {
            return def.clone();
        }
        if let Some(info) = self.interpreter.container_type_metadata(target) {
            Value::Package(Symbol::intern(&info.value_type))
        } else if matches!(target, Value::Hash(_)) {
            Value::Package(Symbol::intern("Any"))
        } else {
            Value::Nil
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
        let message = if let Value::Instance { attributes, .. } = exception {
            attributes
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
        match value {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                if let Some(ex) = attributes.get("exception") {
                    return Err(Self::failure_to_error(ex));
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn anon_state_value(&self, name: &str) -> Option<Value> {
        let key = Self::anon_state_key(name)?;
        self.interpreter.get_state_var(&key).cloned()
    }

    pub(super) fn sync_anon_state_value(&mut self, name: &str, value: &Value) {
        if let Some(key) = Self::anon_state_key(name) {
            self.interpreter.set_state_var(key, value.clone());
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

    pub(super) fn array_depth(value: &Value) -> usize {
        if let Some(shape) = crate::runtime::utils::shaped_array_shape(value)
            && !shape.is_empty()
        {
            return shape.len();
        }
        match value {
            Value::Array(items, ..) => {
                let child = items
                    .first()
                    .map(Self::array_depth)
                    .filter(|d| *d > 0)
                    .unwrap_or(0);
                1 + child
            }
            _ => 0,
        }
    }

    pub(super) fn index_to_usize(idx: &Value) -> Option<usize> {
        match idx {
            Value::Int(i) if *i >= 0 => Some(*i as usize),
            Value::Num(f) if *f >= 0.0 => Some(*f as usize),
            Value::Rat(n, d) if *d != 0 => {
                let f = *n as f64 / *d as f64;
                (f >= 0.0).then_some(f as usize)
            }
            Value::FatRat(n, d) if *d != 0 => {
                let f = *n as f64 / *d as f64;
                (f >= 0.0).then_some(f as usize)
            }
            Value::BigRat(_, d) if !d.is_zero() => {
                let f = runtime::to_float_value(idx)?;
                (f >= 0.0).then_some(f as usize)
            }
            _ => idx.to_string_value().parse::<usize>().ok(),
        }
    }

    fn not_enough_dimensions_error(operation: &str, got: usize, needed: usize) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("operation".to_string(), Value::str(operation.to_string()));
        attrs.insert("got-dimensions".to_string(), Value::Int(got as i64));
        attrs.insert("needed-dimensions".to_string(), Value::Int(needed as i64));
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Not enough dimensions: got {}, needed {}",
                got, needed
            )),
        );
        let mut err = RuntimeError::new("X::NotEnoughDimensions");
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::NotEnoughDimensions"),
            attrs,
        )));
        err
    }

    pub(super) fn index_array_multidim(
        target: &Value,
        indices: &[Value],
        strict_oob: bool,
    ) -> Result<Value, RuntimeError> {
        if indices.is_empty() {
            return Ok(target.clone());
        }
        let head = &indices[0];
        if matches!(head, Value::Whatever)
            || matches!(head, Value::Num(f) if f.is_infinite() && *f > 0.0)
        {
            if indices.len() > 1 && crate::runtime::utils::is_shaped_array(target) {
                return Err(RuntimeError::typed_msg("X::NYI", "Not yet implemented"));
            }
            let Value::Array(items, ..) = target else {
                return Ok(Value::Nil);
            };
            let mut out = Vec::with_capacity(items.len());
            for item in items.iter() {
                out.push(Self::index_array_multidim(item, &indices[1..], strict_oob)?);
            }
            return Ok(Value::array(out));
        }
        let Some(i) = Self::index_to_usize(head) else {
            return Ok(Value::Nil);
        };
        let Value::Array(items, ..) = target else {
            return Ok(Value::Nil);
        };
        if i >= items.len() {
            if strict_oob {
                return Err(RuntimeError::new("Index out of bounds"));
            }
            return Ok(Value::Nil);
        }
        Self::index_array_multidim(&items[i], &indices[1..], strict_oob)
    }

    pub(super) fn assign_array_multidim(
        target: &mut Value,
        indices: &[Value],
        val: Value,
    ) -> Result<(), RuntimeError> {
        let shape = crate::runtime::utils::shaped_array_shape(target);
        let depth = Self::array_depth(target);
        if indices.len() < depth && depth > 1 {
            return Err(Self::not_enough_dimensions_error(
                "assign to",
                indices.len(),
                depth,
            ));
        }
        if indices.is_empty() {
            return Err(RuntimeError::new("Index out of bounds"));
        }
        let Some(i) = Self::index_to_usize(&indices[0]) else {
            return Err(RuntimeError::new("Index out of bounds"));
        };
        let Value::Array(items, ..) = target else {
            return Err(RuntimeError::new("Index out of bounds"));
        };
        if i >= items.len() {
            return Err(RuntimeError::new("Index out of bounds"));
        }
        let arr = Arc::make_mut(items);
        if indices.len() == 1 {
            arr[i] = val;
        } else {
            Self::assign_array_multidim(&mut arr[i], &indices[1..], val)?;
        }
        if let Some(shape) = shape.as_deref() {
            crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
        }
        Ok(())
    }

    pub(super) fn delete_array_multidim(
        target: &mut Value,
        indices: &[Value],
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        let shape = crate::runtime::utils::shaped_array_shape(target);
        let depth = Self::array_depth(target);
        if indices.len() < depth && depth > 1 {
            return Err(Self::not_enough_dimensions_error(
                "delete from",
                indices.len(),
                depth,
            ));
        }
        let hole_value = || Value::Package(Symbol::intern(hole_type));
        if indices.is_empty() {
            return Ok(hole_value());
        }
        let Some(i) = Self::index_to_usize(&indices[0]) else {
            return Ok(hole_value());
        };
        let Value::Array(items, ..) = target else {
            return Ok(hole_value());
        };
        if i >= items.len() {
            return Ok(hole_value());
        }
        let arr = Arc::make_mut(items);
        if indices.len() == 1 {
            let prev = arr[i].clone();
            arr[i] = hole_value();
            if let Some(shape) = shape.as_deref() {
                crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
            }
            return Ok(prev);
        }
        let deleted = Self::delete_array_multidim(&mut arr[i], &indices[1..], hole_type)?;
        if let Some(shape) = shape.as_deref() {
            crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
        }
        Ok(deleted)
    }

    pub(super) fn encode_bound_index(idx: &Value) -> String {
        match idx {
            Value::Array(indices, ..) => indices
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(";"),
            _ => idx.to_string_value(),
        }
    }

    pub(super) fn is_bound_index(&self, var_name: &str, encoded: &str) -> bool {
        let key = format!("__mutsu_bound_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.interpreter.env().get(&key) {
            map.contains_key(encoded)
        } else {
            false
        }
    }

    pub(super) fn mark_bound_index(&mut self, var_name: &str, encoded: String) {
        let key = format!("__mutsu_bound_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.interpreter.env_mut().get_mut(&key) {
            Arc::make_mut(map).insert(encoded, Value::Bool(true));
            return;
        }
        let mut map = std::collections::HashMap::new();
        map.insert(encoded, Value::Bool(true));
        self.interpreter.env_mut().insert(key, Value::hash(map));
    }

    pub(super) fn mark_initialized_index(&mut self, var_name: &str, encoded: String) {
        let key = format!("__mutsu_initialized_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.interpreter.env_mut().get_mut(&key) {
            Arc::make_mut(map).insert(encoded, Value::Bool(true));
            return;
        }
        let mut map = std::collections::HashMap::new();
        map.insert(encoded, Value::Bool(true));
        self.interpreter.env_mut().insert(key, Value::hash(map));
    }

    /// Remove deleted indices from the bound-index tracking set.
    /// This must be called after array element deletion to sever bindings.
    pub(super) fn unmark_bound_indices(&mut self, var_name: &str, idx: &Value) {
        let key = format!("__mutsu_bound_index::{}", var_name);
        let Some(Value::Hash(map)) = self.interpreter.env_mut().get_mut(&key) else {
            return;
        };
        let m = Arc::make_mut(map);
        Self::unmark_index_entries(m, idx);
    }

    /// Remove deleted indices from the initialized-index tracking set.
    /// This must be called after array element deletion and before
    /// `trim_trailing_array_holes` so that deleted slots are recognized
    /// as holes and can be trimmed.
    pub(super) fn unmark_initialized_indices(&mut self, var_name: &str, idx: &Value) {
        let key = format!("__mutsu_initialized_index::{}", var_name);
        let Some(Value::Hash(map)) = self.interpreter.env_mut().get_mut(&key) else {
            return;
        };
        let m = Arc::make_mut(map);
        Self::unmark_index_entries(m, idx);
    }

    fn unmark_index_entries(map: &mut std::collections::HashMap<String, Value>, idx: &Value) {
        match idx {
            Value::Array(items, ..) => {
                for item in items.iter() {
                    Self::unmark_index_entries(map, item);
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                let expanded = crate::runtime::utils::value_to_list(idx);
                for item in &expanded {
                    Self::unmark_index_entries(map, item);
                }
            }
            Value::Int(i) => {
                map.remove(&i.to_string());
            }
            Value::Num(f) => {
                map.remove(&(*f as i64).to_string());
            }
            _ => {
                map.remove(&idx.to_string_value());
            }
        }
    }
}
