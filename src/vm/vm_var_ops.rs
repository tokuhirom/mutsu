use super::*;
use crate::symbol::Symbol;
use num_traits::Zero;
use std::sync::Arc;

const SELF_HASH_REF_SENTINEL: &str = "__mutsu_self_hash_ref";
pub(super) const BOUND_HASH_REF_SENTINEL: &str = "__mutsu_bound_hash_ref";
const SELF_ARRAY_REF_SENTINEL: &str = "__mutsu_self_array_ref";

impl VM {
    fn range_end_is_unbounded(end: i64) -> bool {
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

    fn resolve_hash_entry(&self, items: &Arc<HashMap<String, Value>>, key: &str) -> Value {
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

    fn resolve_array_entry(
        items: &Arc<Vec<Value>>,
        kind: ArrayKind,
        idx: usize,
        default: Value,
    ) -> Value {
        match items.get(idx) {
            Some(Value::Pair(name, _)) if name == SELF_ARRAY_REF_SENTINEL => {
                Value::Array(items.clone(), kind)
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

    fn instance_exists_pos_result(
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
    fn throw_if_failure(value: &Value) -> Result<(), RuntimeError> {
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

    fn index_array_multidim(
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

    fn delete_array_multidim(
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

    /// Remove deleted indices from the initialized-index tracking set.
    /// This must be called after array element deletion and before
    /// `trim_trailing_array_holes` so that deleted slots are recognized
    /// as holes and can be trimmed.
    fn unmark_initialized_indices(&mut self, var_name: &str, idx: &Value) {
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

    pub(super) fn exec_get_bare_word_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let val = if name == "Bool::True" {
            Value::Bool(true)
        } else if name == "Bool::False" {
            Value::Bool(false)
        } else if name == "Order::Less" {
            Value::Enum {
                enum_type: Symbol::intern("Order"),
                key: Symbol::intern("Less"),
                value: EnumValue::Int(-1),
                index: 0,
            }
        } else if name == "Order::Same" {
            Value::Enum {
                enum_type: Symbol::intern("Order"),
                key: Symbol::intern("Same"),
                value: EnumValue::Int(0),
                index: 1,
            }
        } else if name == "Order::More" {
            Value::Enum {
                enum_type: Symbol::intern("Order"),
                key: Symbol::intern("More"),
                value: EnumValue::Int(1),
                index: 2,
            }
        } else if (name.starts_with("infix:<")
            || name.starts_with("prefix:<")
            || name.starts_with("postfix:<"))
            && name.ends_with('>')
        {
            Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(name),
                is_regex: false,
            }
        } else if self.interpreter.is_name_suppressed(name) {
            // If we are inside the parent class of the suppressed nested class,
            // resolve the short name to the qualified name (e.g. Frog -> Forest::Frog).
            if let Some(qualified) = self.interpreter.resolve_suppressed_type(name) {
                Value::Package(Symbol::intern(&qualified))
            } else {
                return Err(RuntimeError::new(format!(
                    "X::Undeclared::Symbols: Undeclared name:\n    {} used at line 1",
                    name,
                )));
            }
        } else if let Some((pkg, sym)) = name.rsplit_once("::")
            && let Some(stripped_sym) = sym.strip_prefix('&')
        {
            let qualified_name = format!("{pkg}::{stripped_sym}");
            if self.interpreter.has_function(&qualified_name)
                || self.interpreter.has_multi_function(&qualified_name)
            {
                Value::Routine {
                    package: Symbol::intern(pkg),
                    name: Symbol::intern(&qualified_name),
                    is_regex: false,
                }
            } else {
                Value::str(name.to_string())
            }
        } else if let Some(v) = self.interpreter.env().get(name) {
            if matches!(v, Value::Enum { .. } | Value::Nil) {
                v.clone()
            } else if self.interpreter.has_type(name) || Self::is_builtin_type(name) {
                Value::Package(Symbol::intern(Self::resolve_type_alias(name)))
            } else if name.contains("::")
                && !name.starts_with('$')
                && !name.starts_with('@')
                && !name.starts_with('%')
                && matches!(v, Value::Routine { .. } | Value::Sub(_) | Value::WeakSub(_))
            {
                // Try compiled function dispatch first, then fall back to interpreter.
                // Note: interpreter.call_function handles pseudo-package resolution
                // (SETTING::, OUTER::, etc.) which vm_call_on_value does not.
                if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                    let pkg = self.interpreter.current_package().to_string();
                    let result = self.call_compiled_function_named(
                        cf,
                        Vec::new(),
                        compiled_fns,
                        &pkg,
                        name,
                    )?;
                    self.env_dirty = true;
                    result
                } else {
                    let result = self.interpreter.call_function(name, Vec::new())?;
                    self.env_dirty = true;
                    result
                }
            } else if !name.starts_with('$') && !name.starts_with('@') && !name.starts_with('%') {
                v.clone()
            } else {
                Value::str(name.to_string())
            }
        } else if name.contains("::")
            && let Some(def) = self.interpreter.resolve_function_with_types(name, &[])
        {
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = def.package.resolve();
                let result =
                    self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?;
                self.env_dirty = true;
                result
            } else {
                let result = self.interpreter.call_function_def(&def, &[])?;
                self.env_dirty = true;
                result
            }
        } else if self.interpreter.has_type(name)
            || Self::is_builtin_type(name)
            || Self::is_type_with_smiley(name, &self.interpreter)
        {
            Value::Package(Symbol::intern(Self::resolve_type_alias(name)))
        } else if let Some(callable) = self.interpreter.env().get(&format!("&{name}")).cloned()
            && matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            )
        {
            let result = self
                .interpreter
                .call_sub_value(callable, Vec::new(), false)?;
            self.env_dirty = true;
            result
        } else if let Some(sub_id) = self.interpreter.wrap_sub_id_for_name(name)
            && !self.interpreter.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.interpreter.get_wrapped_sub(name)
        {
            let result = self
                .interpreter
                .call_sub_value(sub_val, Vec::new(), false)?;
            self.env_dirty = true;
            result
        } else if Interpreter::is_test_function_name(name)
            && self.interpreter.test_mode_active()
            // Only try test function dispatch for hyphenated names (e.g.
            // `make-temp-dir`, `done-testing`). Single-word names like
            // `run`, `is`, `ok` are either builtins or need args, so they
            // should go through the normal function resolution path.
            && name.contains('-')
        {
            let result = self.interpreter.call_function(name, Vec::new())?;
            self.env_dirty = true;
            result
        } else if self.interpreter.has_function(name)
            || Interpreter::is_implicit_zero_arg_builtin(name)
            || self.interpreter.has_multi_function(name)
        {
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = self.interpreter.current_package().to_string();
                let result =
                    self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?;
                self.env_dirty = true;
                result
            } else if let Some(native_result) =
                self.try_native_function(crate::symbol::Symbol::intern(name), &[])
            {
                native_result?
            } else {
                let result = self.interpreter.call_function(name, Vec::new())?;
                self.env_dirty = true;
                result
            }
        } else if name == "callsame"
            || name == "nextsame"
            || name == "callwith"
            || name == "nextwith"
            || name == "nextcallee"
        {
            let result = self.interpreter.call_function(name, Vec::new())?;
            self.env_dirty = true;
            result
        } else if name == "i" {
            Value::Complex(0.0, 1.0)
        } else if name == "NaN" {
            Value::Num(f64::NAN)
        } else if name == "Inf" {
            Value::Num(f64::INFINITY)
        } else if name == "Empty" {
            Value::Slip(std::sync::Arc::new(vec![]))
        } else if name.starts_with("Metamodel::") {
            // Meta-object protocol type objects
            Value::Package(Symbol::intern(name))
        } else if name.contains("::") {
            // Check if this is an access to a non-existent enum variant
            if let Some((pkg, sym)) = name.rsplit_once("::")
                && self.interpreter.has_enum_type(pkg)
                && !self.interpreter.has_enum_variant(pkg, sym)
            {
                return Err(RuntimeError::new(format!(
                    "Could not find symbol '&{}' in '{}'",
                    sym, pkg,
                )));
            }
            Value::Package(Symbol::intern(name))
        } else if name.chars().count() == 1 {
            // Single unicode character — check for vulgar fractions etc.
            let ch = name.chars().next().unwrap();
            if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
                Value::Rat(n, d)
            } else if let Some(val) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
                Value::Int(val)
            } else {
                Value::str(name.to_string())
            }
        } else if name == "self" {
            // `self` used outside of a method context
            let mut err = RuntimeError::new("'self' used where no object is available".to_string());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str("'self' used where no object is available".to_string()),
            );
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Syntax::Self::WithoutObject"),
                attrs,
            )));
            return Err(err);
        } else {
            Value::str(name.to_string())
        };
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_index_op(&mut self) -> Result<(), RuntimeError> {
        let index = self.stack.pop().unwrap();
        let mut target = self.stack.pop().unwrap();
        if let Value::Junction { kind, values } = &target {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                self.stack.push(value.clone());
                self.stack.push(index.clone());
                self.exec_index_op()?;
                results.push(self.stack.pop().unwrap_or(Value::Nil));
            }
            self.stack.push(Value::junction(kind.clone(), results));
            return Ok(());
        }
        if let Value::Junction { kind, values } = &index {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                self.stack.push(target.clone());
                self.stack.push(value.clone());
                self.exec_index_op()?;
                results.push(self.stack.pop().unwrap_or(Value::Nil));
            }
            self.stack.push(Value::junction(kind.clone(), results));
            return Ok(());
        }
        // If target is a Failure, propagate it (// will catch it as undefined)
        if matches!(&target, Value::Instance { class_name, .. } if class_name == "Failure") {
            self.stack.push(target);
            return Ok(());
        }
        if let Value::LazyList(ref ll) = target {
            let forced = if matches!(
                ll.env.get("__mutsu_lazylist_from_gather"),
                Some(Value::Bool(true))
            ) {
                match &index {
                    Value::Int(i) if *i >= 0 => self
                        .interpreter
                        .force_lazy_list_prefix_bridge(ll, (*i as usize).saturating_add(1))?,
                    Value::Range(_, end) if *end >= 0 => self
                        .interpreter
                        .force_lazy_list_prefix_bridge(ll, (*end as usize).saturating_add(1))?,
                    Value::RangeExcl(_, end) if *end > 0 => self
                        .interpreter
                        .force_lazy_list_prefix_bridge(ll, *end as usize)?,
                    _ => self.force_lazy_list_vm(ll)?,
                }
            } else {
                self.force_lazy_list_vm(ll)?
            };
            target = Value::array(forced);
        }
        // Normalize Slip target to List for uniform handling
        if let Value::Slip(items) = target {
            target = Value::Array(items, crate::value::ArrayKind::List);
        }
        // Normalize index: convert Seq/LazyList indices to Array for
        // uniform handling in the match below.
        let is_lazy_index = matches!(&index, Value::LazyList(..));
        let index = if let Value::LazyList(ref ll) = index {
            let items = self.force_lazy_list_vm(ll)?;
            Value::array(items)
        } else if let Value::Seq(items) = index {
            Value::Array(items, crate::value::ArrayKind::List)
        } else {
            index
        };
        let result = match (target, index) {
            (Value::Array(items, is_arr), Value::Int(i)) => {
                if i < 0 {
                    // Return a Failure wrapping X::OutOfRange — `//` treats it as
                    // undefined but any further use (e.g. subscripting) will throw.
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("what".to_string(), Value::str_from("Index"));
                    attrs.insert("got".to_string(), Value::Int(i));
                    attrs.insert("range".to_string(), Value::str_from("0..^Inf"));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Index out of range. Is: {}, should be in 0..^Inf",
                            i
                        )),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
                } else if is_arr == crate::value::ArrayKind::Shaped && (i as usize) >= items.len() {
                    // Shaped arrays have fixed size; accessing beyond bounds dies
                    return Err(RuntimeError::new(format!(
                        "Index {} for dimension 1 out of range (must be 0..{})",
                        i,
                        items.len() - 1
                    )));
                } else {
                    let default =
                        self.typed_container_default(&Value::Array(items.clone(), is_arr));
                    Self::resolve_array_entry(&items, is_arr, i as usize, default)
                }
            }
            (target @ Value::Array(..), Value::Array(indices, ..)) => {
                let depth = Self::array_depth(&target);
                if depth <= 1 && indices.len() > 1 {
                    // Positional slice: @a[0,1,2] returns (@a[0], @a[1], @a[2])
                    let Value::Array(items, kind) = &target else {
                        unreachable!()
                    };
                    let mut out = Vec::with_capacity(indices.len());
                    for idx in indices.iter() {
                        if let Some(i) = Self::index_to_usize(idx) {
                            if is_lazy_index && i >= items.len() {
                                // Lazy index: stop at array boundary
                                break;
                            }
                            out.push(Self::resolve_array_entry(
                                items,
                                *kind,
                                i,
                                self.typed_container_default(&target),
                            ));
                        } else if !is_lazy_index {
                            out.push(self.typed_container_default(&target));
                        }
                    }
                    Value::array(out)
                } else {
                    let strict_oob = indices.len() > 1;
                    let indexed =
                        Self::index_array_multidim(&target, indices.as_ref(), strict_oob)?;
                    if matches!(indexed, Value::Nil) {
                        self.typed_container_default(&target)
                    } else {
                        indexed
                    }
                }
            }
            (Value::Array(items, kind), Value::Range(a, b)) => {
                let start = a.max(0) as usize;
                let end = if Self::range_end_is_unbounded(b) {
                    items.len().saturating_sub(1)
                } else {
                    b.max(-1) as usize
                };
                let default = self.typed_container_default(&Value::Array(items.clone(), kind));
                let mut slice = Vec::new();
                for i in start..=end {
                    slice.push(Self::resolve_array_entry(&items, kind, i, default.clone()));
                }
                if kind.is_real_array() {
                    Value::array(slice)
                } else {
                    Value::Seq(Arc::new(slice))
                }
            }
            (Value::Array(items, kind), Value::RangeExcl(a, b)) => {
                let start = a.max(0) as usize;
                let end_excl = if Self::range_end_is_unbounded(b) {
                    items.len()
                } else {
                    b.max(0) as usize
                };
                if start >= end_excl {
                    if kind.is_real_array() {
                        Value::array(Vec::new())
                    } else {
                        Value::Seq(Arc::new(Vec::new()))
                    }
                } else {
                    let default = self.typed_container_default(&Value::Array(items.clone(), kind));
                    let mut slice = Vec::with_capacity(end_excl - start);
                    for i in start..end_excl {
                        slice.push(Self::resolve_array_entry(&items, kind, i, default.clone()));
                    }
                    if kind.is_real_array() {
                        Value::array(slice)
                    } else {
                        Value::Seq(Arc::new(slice))
                    }
                }
            }
            (Value::Array(items, is_arr), Value::Num(n)) => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                if n < 0.0 {
                    default
                } else {
                    Self::resolve_array_entry(&items, is_arr, n as usize, default)
                }
            }
            (Value::Array(items, is_arr), Value::Rat(n, d)) if d != 0 => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                let i = (n as f64 / d as f64) as usize;
                Self::resolve_array_entry(&items, is_arr, i, default)
            }
            (Value::Array(items, is_arr), Value::FatRat(n, d)) if d != 0 => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                let i = (n as f64 / d as f64) as usize;
                Self::resolve_array_entry(&items, is_arr, i, default)
            }
            (Value::Array(items, is_arr), Value::BigRat(n, d)) if !d.is_zero() => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                let idx = runtime::to_float_value(&Value::BigRat(n, d)).unwrap_or(0.0);
                if idx < 0.0 {
                    default
                } else {
                    Self::resolve_array_entry(&items, is_arr, idx as usize, default)
                }
            }
            (Value::Seq(items), Value::Int(i)) => {
                if i < 0 {
                    Value::Nil
                } else {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            (Value::Seq(items), Value::Range(a, b)) => {
                let start = a.max(0) as usize;
                let end = b.max(-1) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end = end.min(items.len().saturating_sub(1));
                    items[start..=end].to_vec()
                };
                Value::Seq(Arc::new(slice))
            }
            (Value::Seq(items), Value::RangeExcl(a, b)) => {
                let start = a.max(0) as usize;
                let end_excl = b.max(0) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end_excl = end_excl.min(items.len());
                    if start >= end_excl {
                        Vec::new()
                    } else {
                        items[start..end_excl].to_vec()
                    }
                };
                Value::Seq(Arc::new(slice))
            }
            // WhateverCode index on Seq: (1,2,3).Seq[*-1]
            (Value::Seq(items), Value::Sub(ref data)) => {
                let len = items.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 => items.get(i as usize).cloned().unwrap_or(Value::Nil),
                    Some(i) if i < 0 => Self::make_out_of_range_failure(i),
                    _ => Value::Nil,
                }
            }
            (Value::Hash(items), Value::Whatever) => {
                Value::array(items.values().cloned().collect())
            }
            (Value::Hash(items), Value::Num(f)) if f.is_infinite() && f > 0.0 => {
                Value::array(items.values().cloned().collect())
            }
            (Value::Hash(items), Value::Nil) => Value::Hash(items),
            (Value::Hash(items), Value::Array(keys, ..)) => {
                let default = self.typed_container_default(&Value::Hash(items.clone()));
                Value::array(
                    keys.iter()
                        .map(|k| {
                            let v = self.resolve_hash_entry(&items, &k.to_string_value());
                            if matches!(v, Value::Nil) {
                                default.clone()
                            } else {
                                v
                            }
                        })
                        .collect(),
                )
            }
            (Value::Hash(items), Value::Str(key)) => {
                let default = self.typed_container_default(&Value::Hash(items.clone()));
                let v = self.resolve_hash_entry(&items, &key);
                if matches!(v, Value::Nil) { default } else { v }
            }
            (Value::Hash(items), Value::Int(key)) => {
                let default = self.typed_container_default(&Value::Hash(items.clone()));
                let v = self.resolve_hash_entry(&items, &key.to_string());
                if matches!(v, Value::Nil) { default } else { v }
            }
            (Value::Hash(items), key) => {
                let default = self.typed_container_default(&Value::Hash(items.clone()));
                let v = self.resolve_hash_entry(&items, &key.to_string_value());
                if matches!(v, Value::Nil) { default } else { v }
            }
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Str(key),
            ) if class_name == "Match" => {
                if let Some(Value::Hash(named)) = attributes.get("named") {
                    named.get(key.as_str()).cloned().unwrap_or(Value::Nil)
                } else {
                    Value::Nil
                }
            }
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Int(i),
            ) if class_name == "Match" => {
                if i < 0 {
                    Value::Nil
                } else if let Some(Value::Array(items, ..)) = attributes.get("list") {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                } else {
                    Value::Nil
                }
            }
            (instance @ Value::Instance { .. }, Value::Str(key)) => {
                let default = self.typed_container_default(&instance);
                let result = self
                    .try_compiled_method_or_interpret(
                        instance,
                        "AT-KEY",
                        vec![Value::Str(key.clone())],
                    )
                    .unwrap_or(Value::Nil);
                if matches!(result, Value::Nil) {
                    default
                } else {
                    result
                }
            }
            (instance @ Value::Instance { .. }, Value::Int(i)) => {
                let default = self.typed_container_default(&instance);
                let fallback = instance.clone();
                let result = self
                    .try_compiled_method_or_interpret(instance, "AT-POS", vec![Value::Int(i)])
                    .or_else(|_| {
                        self.try_compiled_method_or_interpret(
                            fallback,
                            "AT-KEY",
                            vec![Value::Int(i)],
                        )
                    })
                    .unwrap_or(Value::Nil);
                if matches!(result, Value::Nil) {
                    default
                } else {
                    result
                }
            }
            (instance @ Value::Instance { .. }, Value::Array(keys, ..)) => {
                let mut results = Vec::with_capacity(keys.len());
                for k in keys.iter().cloned() {
                    results.push(
                        self.try_compiled_method_or_interpret(instance.clone(), "AT-KEY", vec![k])
                            .unwrap_or(Value::Nil),
                    );
                }
                Value::array(results)
            }
            (Value::Str(_), Value::Str(_)) => {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str_from("Type Str does not support associative indexing."),
                );
                let ex = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
                let mut failure_attrs = std::collections::HashMap::new();
                failure_attrs.insert("exception".to_string(), ex);
                Value::make_instance(Symbol::intern("Failure"), failure_attrs)
            }
            (Value::Set(s), Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .map(|k| Value::Bool(s.contains(&k.to_string_value())))
                    .collect(),
            ),
            (Value::Set(s), Value::Str(key)) => Value::Bool(s.contains(key.as_str())),
            (Value::Set(s), idx) => Value::Bool(s.contains(&idx.to_string_value())),
            (Value::Bag(b), Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .map(|k| Value::Int(*b.get(&k.to_string_value()).unwrap_or(&0)))
                    .collect(),
            ),
            (Value::Bag(b), Value::Str(key)) => Value::Int(*b.get(key.as_str()).unwrap_or(&0)),
            (Value::Bag(b), idx) => Value::Int(*b.get(&idx.to_string_value()).unwrap_or(&0)),
            (Value::Mix(m), Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .map(|k| {
                        Self::mix_weight_as_value(*m.get(&k.to_string_value()).unwrap_or(&0.0))
                    })
                    .collect(),
            ),
            (Value::Mix(m), Value::Str(key)) => {
                Self::mix_weight_as_value(*m.get(key.as_str()).unwrap_or(&0.0))
            }
            (Value::Mix(m), idx) => {
                Self::mix_weight_as_value(*m.get(&idx.to_string_value()).unwrap_or(&0.0))
            }
            // Range indexing (supports infinite ranges)
            (ref range, Value::Int(i)) if range.is_range() => {
                if let Some((start, _end, _excl_start, _excl_end)) = range_params(range) {
                    if i < 0 {
                        Value::Nil
                    } else {
                        Value::Int(start + i)
                    }
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    if i < 0 {
                        Value::Nil
                    } else {
                        items.get(i as usize).cloned().unwrap_or(Value::Nil)
                    }
                }
            }
            (ref range, Value::RangeExcl(a, b)) if range.is_range() => {
                if let Some((start, end, _excl_start, excl_end)) = range_params(range) {
                    let actual_end = if excl_end { end - 1 } else { end };
                    let mut result = Vec::new();
                    for i in a..b {
                        let val = start + i;
                        if val > actual_end {
                            break;
                        }
                        result.push(Value::Int(val));
                    }
                    Value::array(result)
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    let start = a.max(0) as usize;
                    let end_excl = b.max(0) as usize;
                    if start >= items.len() {
                        Value::array(Vec::new())
                    } else {
                        let end_excl = end_excl.min(items.len());
                        if start >= end_excl {
                            Value::array(Vec::new())
                        } else {
                            Value::array(items[start..end_excl].to_vec())
                        }
                    }
                }
            }
            (ref range, Value::Range(a, b)) if range.is_range() => {
                if let Some((start, end, _excl_start, excl_end)) = range_params(range) {
                    let actual_end = if excl_end { end - 1 } else { end };
                    let mut result = Vec::new();
                    for i in a..=b {
                        let val = start + i;
                        if val > actual_end {
                            break;
                        }
                        result.push(Value::Int(val));
                    }
                    Value::array(result)
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    let start = a.max(0) as usize;
                    let end = b.max(-1) as usize;
                    if start >= items.len() {
                        Value::array(Vec::new())
                    } else {
                        let end = end.min(items.len().saturating_sub(1));
                        Value::array(items[start..=end].to_vec())
                    }
                }
            }
            (ref range, Value::Array(indices, ..)) if range.is_range() => {
                if let Some((start, _end, _excl_start, _excl_end)) = range_params(range) {
                    let result: Vec<Value> = indices
                        .iter()
                        .map(|idx| match idx {
                            Value::Int(i) => Value::Int(start + i),
                            _ => Value::Nil,
                        })
                        .collect();
                    Value::array(result)
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    let result: Vec<Value> = indices
                        .iter()
                        .map(|idx| match idx {
                            Value::Int(i) if *i >= 0 => {
                                items.get(*i as usize).cloned().unwrap_or(Value::Nil)
                            }
                            _ => Value::Nil,
                        })
                        .collect();
                    Value::array(result)
                }
            }
            // WhateverCode index: @a[*-1] → evaluate the lambda with array length
            (Value::Array(ref items, ..), Value::Sub(ref data)) => {
                let len = items.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    Value::Rat(n, d) => {
                        if *d != 0 {
                            Some((*n as f64 / *d as f64).floor() as i64)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 => items.get(i as usize).cloned().unwrap_or(Value::Nil),
                    Some(i) if i < 0 => Self::make_out_of_range_failure(i),
                    _ => Value::Nil,
                }
            }
            // WhateverCode index on Instance (e.g. Buf): $buf[*-1]
            (
                Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                },
                Value::Sub(ref data),
            ) => {
                // Get element count from the instance
                let len = if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) {
                    if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                        bytes.len() as i64
                    } else {
                        0
                    }
                } else {
                    0
                };
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    Value::Rat(n, d) => {
                        if *d != 0 {
                            Some((*n as f64 / *d as f64).floor() as i64)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 => {
                        if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                            bytes.get(i as usize).cloned().unwrap_or(Value::Nil)
                        } else {
                            Value::Nil
                        }
                    }
                    _ => Value::Nil,
                }
            }
            // GenericRange with WhateverCode endpoint: @a[0..*-2]
            (
                Value::Array(ref items, ..),
                Value::GenericRange {
                    ref start,
                    ref end,
                    excl_start,
                    excl_end,
                },
            ) => {
                let len = items.len() as i64;
                let mut resolve_endpoint = |val: &Value| -> i64 {
                    match val {
                        Value::Int(i) => *i,
                        Value::Sub(data) => {
                            let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                            let mut sub_env = data.env.clone();
                            sub_env.insert(param.to_string(), Value::Int(len));
                            let saved_env = std::mem::take(self.interpreter.env_mut());
                            *self.interpreter.env_mut() = sub_env;
                            let result = self
                                .interpreter
                                .eval_block_value(&data.body)
                                .unwrap_or(Value::Nil);
                            *self.interpreter.env_mut() = saved_env;
                            match result {
                                Value::Int(i) => i,
                                _ => 0,
                            }
                        }
                        _ => match val {
                            Value::Num(f) => *f as i64,
                            _ => 0,
                        },
                    }
                };
                let s = resolve_endpoint(start);
                let e = resolve_endpoint(end);
                let actual_start = if excl_start { s + 1 } else { s }.max(0) as usize;
                let actual_end = if excl_end { e } else { e + 1 }.max(0) as usize;
                let actual_end = actual_end.min(items.len());
                let slice = if actual_start >= actual_end {
                    Vec::new()
                } else {
                    items[actual_start..actual_end].to_vec()
                };
                Value::array(slice)
            }
            // Uni/NFC/NFD/NFKC/NFKD indexing: returns integer codepoint values
            (Value::Uni { ref text, .. }, Value::Int(i)) => {
                let chars: Vec<char> = text.chars().collect();
                if i < 0 || (i as usize) >= chars.len() {
                    Value::Nil
                } else {
                    Value::Int(chars[i as usize] as i64)
                }
            }
            (Value::Uni { ref text, .. }, Value::Sub(ref data)) => {
                let chars: Vec<char> = text.chars().collect();
                let len = chars.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 && (i as usize) < chars.len() => {
                        Value::Int(chars[i as usize] as i64)
                    }
                    _ => Value::Nil,
                }
            }
            (Value::Uni { ref text, .. }, Value::Array(indices, ..)) => {
                let chars: Vec<char> = text.chars().collect();
                Value::array(
                    indices
                        .iter()
                        .map(|idx| {
                            if let Value::Int(i) = idx {
                                if *i >= 0 && (*i as usize) < chars.len() {
                                    Value::Int(chars[*i as usize] as i64)
                                } else {
                                    Value::Nil
                                }
                            } else {
                                Value::Nil
                            }
                        })
                        .collect(),
                )
            }
            // Capture indexing: $capture<key> (named) or $capture[idx] (positional)
            (
                Value::Capture {
                    positional: _,
                    named,
                },
                Value::Str(key),
            ) => named.get(key.as_str()).cloned().unwrap_or(Value::Nil),
            (Value::Capture { positional, .. }, Value::Int(i)) => {
                if i < 0 {
                    Value::Nil
                } else {
                    positional.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            // Role parameterization: e.g. R1[C1] → ParametricRole
            (Value::Package(name), idx) if self.interpreter.is_role(&name.resolve()) => {
                let type_args = match idx {
                    Value::Array(items, ..) => items.as_ref().clone(),
                    other => vec![other],
                };
                Value::ParametricRole {
                    base_name: name,
                    type_args,
                }
            }
            // Type parameterization: e.g. Array[Int] or Hash[Int,Str]
            (Value::Package(name), idx) => {
                let type_args = match idx {
                    Value::Array(items, ..) => items.as_ref().clone(),
                    other => vec![other],
                };
                let args = type_args
                    .into_iter()
                    .map(|v| match v {
                        Value::Package(name) => name.resolve(),
                        other => {
                            let s = other.to_string_value();
                            s.trim_start_matches('(').trim_end_matches(')').to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Value::Package(Symbol::intern(&format!("{}[{}]", name, args)))
            }
            // Pair subscript: $pair<key> returns value if key matches, Nil otherwise
            (Value::Pair(key, value), Value::Str(idx)) => {
                if key.as_str() == idx.as_str() {
                    *value
                } else {
                    Value::Nil
                }
            }
            (Value::ValuePair(key, value), Value::Str(idx)) => {
                if key.to_string_value() == **idx {
                    *value
                } else {
                    Value::Nil
                }
            }
            // Array + Str: coerce numeric string to Int for positional indexing
            (Value::Array(items, is_arr), Value::Str(ref s)) => {
                if let Ok(i) = s.trim().parse::<i64>() {
                    if i < 0 {
                        Self::make_out_of_range_failure(i)
                    } else {
                        let default =
                            self.typed_container_default(&Value::Array(items.clone(), is_arr));
                        Self::resolve_array_entry(&items, is_arr, i as usize, default)
                    }
                } else {
                    Self::make_assoc_indexing_failure("Array")
                }
            }
            // Associative indexing on non-hash types returns a Failure
            (ref target, Value::Str(_))
                if matches!(
                    target,
                    Value::Int(_)
                        | Value::BigInt(_)
                        | Value::Num(_)
                        | Value::Rat(..)
                        | Value::Bool(_)
                ) =>
            {
                let type_name = crate::value::types::what_type_name(target);
                Self::make_assoc_indexing_failure(&type_name)
            }
            // Scalar value with integer index: treat as single-element list
            (ref val, Value::Int(0))
                if !matches!(val, Value::Array(..) | Value::Hash(_) | Value::Nil) =>
            {
                val.clone()
            }
            (ref val, Value::Int(i))
                if !matches!(val, Value::Array(..) | Value::Hash(_)) && i > 0 =>
            {
                Value::Nil
            }
            // Scalar value with WhateverCode index: treat as single-element list
            (ref val, Value::Sub(ref data))
                if !matches!(
                    val,
                    Value::Array(..) | Value::Hash(_) | Value::Instance { .. }
                ) =>
            {
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(1)); // elems = 1
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    Value::Rat(n, d) => {
                        if *d != 0 {
                            Some((*n as f64 / *d as f64).floor() as i64)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                match i {
                    Some(0) => val.clone(),
                    _ => Value::Nil,
                }
            }
            _ => Value::Nil,
        };
        self.stack.push(result);
        Ok(())
    }

    /// Create a Failure for "Type X does not support associative indexing."
    fn make_assoc_indexing_failure(type_name: &str) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from(&format!(
                "Type {} does not support associative indexing.",
                type_name
            )),
        );
        let ex = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    /// Create a Failure wrapping X::OutOfRange for effective negative indices.
    fn make_out_of_range_failure(effective_index: i64) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from(&format!(
                "Effective index out of range. Is: {}, should be in 0..^Inf",
                effective_index
            )),
        );
        attrs.insert("got".to_string(), Value::Int(effective_index));
        attrs.insert("range".to_string(), Value::str_from("0..^Inf"));
        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    /// Resolve WhateverCode indices for array deletion.
    /// Converts `*-N` style closures to concrete integer indices.
    fn resolve_delete_index_for_array(&mut self, idx: Value, container: &Value) -> Value {
        let arr_len = match container {
            Value::Array(items, ..) => items.len(),
            _ => return idx,
        };
        match &idx {
            Value::Sub(data) => {
                let len = arr_len as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let resolved = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                resolved
            }
            Value::Array(items, ..) => {
                // Array of indices: resolve each element
                let resolved: Vec<Value> = items
                    .iter()
                    .map(|v| self.resolve_delete_index_for_array(v.clone(), container))
                    .collect();
                Value::array(resolved)
            }
            _ => idx,
        }
    }

    /// Trim trailing "holes" from a named array variable after deletion.
    /// A hole is either `Value::Nil` (deleted slot) or an uninitialized
    /// `Value::Package("Any")` slot (auto-vivified gap).  Explicitly
    /// assigned slots are tracked via `__mutsu_initialized_index::` metadata
    /// and are NOT trimmed.
    fn trim_trailing_array_holes(&mut self, var_name: &str) {
        let init_key = format!("__mutsu_initialized_index::{}", var_name);
        // Clone the initialized set to avoid borrow conflicts
        let initialized: std::collections::HashSet<String> = self
            .interpreter
            .env()
            .get(&init_key)
            .and_then(|v| {
                if let Value::Hash(map) = v {
                    Some(map.keys().cloned().collect())
                } else {
                    None
                }
            })
            .unwrap_or_default();
        // Get the type constraint for typed arrays (e.g. "Int" for `my Int @a`)
        let type_constraint = self
            .interpreter
            .var_type_constraint(var_name)
            .unwrap_or_default();
        let env = self.interpreter.env_mut();
        let Some(container) = env.get_mut(var_name) else {
            return;
        };
        let Value::Array(items, ..) = container else {
            return;
        };
        let arr = Arc::make_mut(items);
        while let Some(last) = arr.last() {
            let idx_str = (arr.len() - 1).to_string();
            let is_hole = match last {
                Value::Nil => true,
                Value::Package(name) if name == "Any" => !initialized.contains(&idx_str),
                // For typed arrays (e.g. `my Int @a`), the type object is also a hole
                Value::Package(name)
                    if !type_constraint.is_empty() && name == type_constraint.as_str() =>
                {
                    !initialized.contains(&idx_str)
                }
                _ => false,
            };
            if is_hole {
                arr.pop();
            } else {
                break;
            }
        }
    }

    pub(super) fn exec_delete_index_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let declared_type_del = self
            .interpreter
            .env()
            .get(&var_name)
            .and_then(|v| self.interpreter.container_type_metadata(v))
            .and_then(|info| info.declared_type);
        let target_is_mixhash = declared_type_del.as_deref().is_some_and(|t| t == "MixHash");
        let _target_is_baghash = declared_type_del.as_deref().is_some_and(|t| t == "BagHash");
        let _target_is_sethash = declared_type_del.as_deref().is_some_and(|t| t == "SetHash");
        // Note: Bag/Set immutability checks for :delete are intentionally
        // omitted here because Bag/BagHash and Set/SetHash share the same
        // Value variants and the declared_type metadata is not always
        // available (e.g., in set operator internals). The Mix check below
        // is kept because Mix operations are less commonly used internally.
        // Sync OS environment and $*HOME when deleting from %*ENV
        if var_name == "%*ENV" {
            // Remove from OS environment
            #[cfg(not(target_family = "wasm"))]
            match &idx {
                Value::Array(keys, ..) => {
                    for k in keys.iter() {
                        let key_str = k.to_string_value();
                        // SAFETY: mutsu is single-threaded
                        unsafe {
                            std::env::remove_var(&key_str);
                        }
                    }
                }
                _ => {
                    let key_str = idx.to_string_value();
                    // SAFETY: mutsu is single-threaded
                    unsafe {
                        std::env::remove_var(&key_str);
                    }
                }
            }
            let deletes_home = match &idx {
                Value::Array(keys, ..) => keys.iter().any(|k| k.to_string_value() == "HOME"),
                _ => idx.to_string_value() == "HOME",
            };
            if deletes_home {
                self.interpreter
                    .env_mut()
                    .insert("$*HOME".to_string(), Value::Nil);
                self.interpreter
                    .env_mut()
                    .insert("*HOME".to_string(), Value::Nil);
            }
        }
        // Save type metadata before delete (Arc::make_mut may change pointer)
        let saved_meta = self
            .interpreter
            .env()
            .get(&var_name)
            .and_then(|v| self.interpreter.container_type_metadata(v));
        // Resolve WhateverCode indices (e.g. *-1) for array targets
        let idx = if let Some(container) = self.interpreter.env().get(&var_name).cloned() {
            self.resolve_delete_index_for_array(idx, &container)
        } else {
            idx
        };
        // For typed arrays (e.g. `my Int @a`), deleted elements become
        // the type object (e.g. `Int`) instead of `Any`.
        let hole_type = self
            .interpreter
            .var_type_constraint(&var_name)
            .unwrap_or_else(|| "Any".to_string());
        // Save idx for unmark step (idx is consumed by delete_from_container)
        let idx_for_unmark = idx.clone();
        let result = if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
            if matches!(container, Value::Mix(_)) && !target_is_mixhash {
                return Err(RuntimeError::immutable("Mix", "delete"));
            }
            Self::delete_from_container(container, idx, &hole_type)?
        } else {
            Self::delete_from_missing_container(idx)
        };
        // Remove deleted indices from the initialized-index tracking set
        // so that trim_trailing_array_holes recognizes them as holes.
        self.unmark_initialized_indices(&var_name, &idx_for_unmark);
        // Trim trailing holes from arrays after deletion.
        // A "hole" is either Nil (deleted) or an uninitialized Package("Any") slot.
        self.trim_trailing_array_holes(&var_name);
        // If the deleted value is a hole (Nil or type object like Package("Any")),
        // substitute the container's default value if one was set via `is default(...)`.
        let result = if matches!(&result, Value::Nil | Value::Package(_)) {
            if let Some(def) = self.interpreter.var_default(&var_name) {
                def.clone()
            } else {
                result
            }
        } else {
            result
        };
        // Re-register type metadata if it was lost due to Arc::make_mut
        if let Some(info) = saved_meta
            && let Some(container) = self.interpreter.env().get(&var_name)
            && self
                .interpreter
                .container_type_metadata(container)
                .is_none()
        {
            let container = container.clone();
            self.interpreter
                .register_container_type_metadata(&container, info);
        }
        // Re-register container default if it was lost due to Arc::make_mut.
        // Use var_default (name-based, survives mutations) as the source of
        // truth, and sync it to the current container's pointer-based default.
        if let Some(def) = self.interpreter.var_default(&var_name).cloned()
            && let Some(container) = self.interpreter.env().get(&var_name).cloned()
        {
            if self.interpreter.container_default(&container).is_none() {
                self.interpreter
                    .set_container_default(&container, def.clone());
            }
            // Sync env value to locals so reads through locals see the
            // updated container with its default.
            self.locals_set_by_name(code, &var_name, container);
        }
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_delete_index_expr_op(&mut self) -> Result<(), RuntimeError> {
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let mut target = self.stack.pop().unwrap_or(Value::Nil);
        // Note: We cannot distinguish Bag from BagHash or Set from SetHash
        // in the expression form (no variable metadata), so immutability
        // checks for Bag/Set are only in the named op path.
        let result = Self::delete_from_container(&mut target, idx, "Any")?;
        self.stack.push(result);
        Ok(())
    }

    /// Multi-dimensional indexing: @a[$x;$y;$z]
    /// Stack: [target, dim0, dim1, ..., dimN-1] → [result]
    pub(super) fn exec_multi_dim_index_op(&mut self, ndims: u32) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        dims.reverse();
        let target = self.stack.pop().unwrap_or(Value::Nil);

        let result = self.multi_dim_index_read(&target, &dims)?;
        self.stack.push(result);
        Ok(())
    }

    /// Read a value from a nested array using multi-dimensional indices.
    /// Each dimension can be:
    /// - A scalar (Int, Str, Num, Rat, WhateverCode) — index into that level
    /// - Whatever (*) — iterate all elements at that level
    /// - An array/list — iterate specified indices at that level
    fn multi_dim_index_read(
        &mut self,
        target: &Value,
        dims: &[Value],
    ) -> Result<Value, RuntimeError> {
        if dims.is_empty() {
            return Ok(target.clone());
        }
        let dim = &dims[0];
        let rest = &dims[1..];

        match dim {
            Value::Whatever => {
                // Iterate all elements at this level
                let items = match target {
                    Value::Array(items, ..) => items,
                    _ => return Ok(Value::Nil),
                };
                let has_more_multi = rest
                    .iter()
                    .any(|v| matches!(v, Value::Whatever | Value::Array(..)));
                let mut out = Vec::with_capacity(items.len());
                for item in items.iter() {
                    let result = self.multi_dim_index_read(item, rest)?;
                    if has_more_multi {
                        // Flatten intermediate array results from deeper * or list dims
                        if let Value::Array(inner, ..) = &result {
                            out.extend(inner.iter().cloned());
                        } else {
                            out.push(result);
                        }
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            Value::Array(indices, ..) => {
                // Multiple indices at this dimension level
                let items = match target {
                    Value::Array(items, ..) => items,
                    _ => return Ok(Value::Nil),
                };
                let has_more_multi = rest
                    .iter()
                    .any(|v| matches!(v, Value::Whatever | Value::Array(..)));
                let mut out = Vec::with_capacity(indices.len());
                for idx in indices.iter() {
                    let result = if let Some(i) = Self::index_to_usize(idx) {
                        if i < items.len() {
                            self.multi_dim_index_read(&items[i], rest)?
                        } else {
                            self.multi_dim_index_read(&Value::Nil, rest)?
                        }
                    } else {
                        Value::Nil
                    };
                    if has_more_multi {
                        if let Value::Array(inner, ..) = &result {
                            out.extend(inner.iter().cloned());
                        } else {
                            out.push(result);
                        }
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            _ => {
                // Scalar index — resolve WhateverCode first
                let resolved = self.resolve_whatever_code_index(dim, target);
                let idx = resolved.as_ref().unwrap_or(dim);
                if let Some(i) = Self::index_to_usize(idx) {
                    let items = match target {
                        Value::Array(items, ..) => items,
                        _ => return Ok(Value::Nil),
                    };
                    if i < items.len() {
                        self.multi_dim_index_read(&items[i], rest)
                    } else {
                        // Out of bounds — return Nil for scalar index
                        Ok(Value::Nil)
                    }
                } else {
                    // Non-numeric index (e.g., string "0")
                    let i = idx.to_string_value().parse::<usize>().ok();
                    if let Some(i) = i {
                        let items = match target {
                            Value::Array(items, ..) => items,
                            _ => return Ok(Value::Nil),
                        };
                        if i < items.len() {
                            self.multi_dim_index_read(&items[i], rest)
                        } else {
                            Ok(Value::Nil)
                        }
                    } else {
                        Ok(Value::Nil)
                    }
                }
            }
        }
    }

    /// Resolve WhateverCode (e.g., *-1) or numeric coercion for a dimension index.
    fn resolve_whatever_code_index(&mut self, dim: &Value, target: &Value) -> Option<Value> {
        if let Value::Sub(data) = dim {
            let len = match target {
                Value::Array(items, ..) => items.len() as i64,
                _ => 0,
            };
            let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
            let mut sub_env = data.env.clone();
            sub_env.insert(param.to_string(), Value::Int(len));
            let saved_env = std::mem::take(self.interpreter.env_mut());
            *self.interpreter.env_mut() = sub_env;
            let result = self
                .interpreter
                .eval_block_value(&data.body)
                .unwrap_or(Value::Nil);
            *self.interpreter.env_mut() = saved_env;
            return Some(result);
        }
        if let Value::Rat(n, d) = dim {
            return Some(Value::Int(*n / *d));
        }
        if let Value::Num(f) = dim {
            return Some(Value::Int(*f as i64));
        }
        if let Value::Str(s) = dim
            && let Ok(i) = s.parse::<i64>()
        {
            return Some(Value::Int(i));
        }
        None
    }

    /// Multi-dimensional index assignment with named target.
    /// Stack: [value, dim0, dim1, ..., dimN-1]
    pub(super) fn exec_multi_dim_index_assign_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        ndims: u32,
    ) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        dims.reverse();
        let value = self.stack.pop().unwrap_or(Value::Nil);

        let var_name = Self::const_str(code, name_idx).to_string();

        // Resolve WhateverCode indices
        let target_val = self
            .interpreter
            .env()
            .get(&var_name)
            .cloned()
            .unwrap_or(Value::Nil);
        let dims = self.resolve_multidim_indices_for_assign(&target_val, &dims)?;

        // Get mutable reference to the target variable
        if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
            Self::multi_dim_assign(container, &dims, value.clone())?;
        }

        self.stack.push(value);
        Ok(())
    }

    /// Multi-dimensional index assignment with generic (expression) target.
    /// Stack: [target, dim0, ..., dimN-1, value]
    pub(super) fn exec_multi_dim_index_assign_generic_op(
        &mut self,
        ndims: u32,
    ) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        dims.reverse();
        let mut target = self.stack.pop().unwrap_or(Value::Nil);
        Self::multi_dim_assign(&mut target, &dims, value.clone())?;
        self.stack.push(value);
        Ok(())
    }

    /// Recursively assign a value into a nested array at the given dimension indices.
    fn multi_dim_assign(
        target: &mut Value,
        dims: &[Value],
        value: Value,
    ) -> Result<(), RuntimeError> {
        if dims.is_empty() {
            *target = value;
            return Ok(());
        }

        let dim = &dims[0];
        let rest = &dims[1..];

        // For multi-index dimensions (arrays), need to distribute values
        if let Value::Array(indices, ..) = dim {
            if let Value::Array(values, ..) = &value {
                // Distribute values to indices
                for (i, idx) in indices.iter().enumerate() {
                    let val = values.get(i).cloned().unwrap_or(Value::Nil);
                    let ii = Self::index_to_usize(idx).unwrap_or(0);
                    Self::ensure_array_size(target, ii + 1);
                    if let Value::Array(items, ..) = target {
                        let items = std::sync::Arc::make_mut(items);
                        Self::multi_dim_assign(&mut items[ii], rest, val)?;
                    }
                }
            } else {
                // Single value assigned to multiple indices
                for idx in indices.iter() {
                    let ii = Self::index_to_usize(idx).unwrap_or(0);
                    Self::ensure_array_size(target, ii + 1);
                    if let Value::Array(items, ..) = target {
                        let items = std::sync::Arc::make_mut(items);
                        Self::multi_dim_assign(&mut items[ii], rest, value.clone())?;
                    }
                }
            }
            return Ok(());
        }

        // Scalar index
        let Some(i) = Self::index_to_usize(dim) else {
            return Err(RuntimeError::new("Invalid index for multi-dim assignment"));
        };

        Self::ensure_array_size(target, i + 1);

        if let Value::Array(items, ..) = target {
            let items = std::sync::Arc::make_mut(items);
            Self::multi_dim_assign(&mut items[i], rest, value)?;
        }

        Ok(())
    }

    /// Resolve WhateverCode indices for multidim assignment.
    fn resolve_multidim_indices_for_assign(
        &mut self,
        target: &Value,
        indices: &[Value],
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut resolved = Vec::with_capacity(indices.len());
        let mut current = target.clone();
        for idx in indices {
            if let Value::Sub(..) = idx {
                let len = match &current {
                    Value::Array(items, ..) => Value::Int(items.len() as i64),
                    _ => Value::Int(0),
                };
                let result = self
                    .interpreter
                    .call_sub_value(idx.clone(), vec![len], false)?;
                current =
                    Self::index_array_multidim(&current, std::slice::from_ref(&result), false)
                        .unwrap_or(Value::Nil);
                resolved.push(result);
            } else {
                current = Self::index_array_multidim(&current, std::slice::from_ref(idx), false)
                    .unwrap_or(Value::Nil);
                resolved.push(idx.clone());
            }
        }
        Ok(resolved)
    }

    /// Ensure the target is an array with at least `min_size` elements.
    fn ensure_array_size(target: &mut Value, min_size: usize) {
        match target {
            Value::Array(items, ..) => {
                if items.len() < min_size {
                    let items = std::sync::Arc::make_mut(items);
                    items.resize(
                        min_size,
                        Value::Package(crate::symbol::Symbol::intern("Any")),
                    );
                }
            }
            Value::Nil | Value::Package(..) => {
                let mut items = Vec::with_capacity(min_size);
                items.resize(
                    min_size,
                    Value::Package(crate::symbol::Symbol::intern("Any")),
                );
                *target = Value::real_array(items);
            }
            _ => {}
        }
    }

    /// Rich :exists adverb handler supporting negation, parameterized arg,
    /// zen slice, and secondary adverbs (:kv, :!kv, :p, :!p, :!v).
    pub(super) fn exec_exists_index_adv_op(
        &mut self,
        flags: u32,
    ) -> Result<(), crate::value::RuntimeError> {
        let negated_flag = flags & 1 != 0;
        let has_arg = flags & 2 != 0;
        let is_zen = flags & 4 != 0;
        let adverb_bits = (flags >> 4) & 0xF;

        // Pop arg if present (it's on top of stack)
        let arg_val = if has_arg {
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Nil
        };

        // Determine effective negation
        let effective_negated = if has_arg {
            !arg_val.truthy() // falsy arg => negate
        } else {
            negated_flag
        };

        // Check for invalid adverb combos — die at runtime
        match adverb_bits {
            6 | 7 => {
                // InvalidK, InvalidNotK
                return Err(crate::value::RuntimeError::new(
                    "Unsupported combination of :exists and :k adverbs".to_string(),
                ));
            }
            8 => {
                // InvalidV
                return Err(crate::value::RuntimeError::new(
                    "Unsupported combination of :exists and :v adverbs".to_string(),
                ));
            }
            _ => {}
        }

        let (target, indices) = if is_zen {
            let target = self.stack.pop().unwrap_or(Value::Nil);
            Self::throw_if_failure(&target)?;
            let len = match &target {
                Value::Array(items, ..) => items.len(),
                _ => 0,
            };
            let idxs: Vec<i64> = (0..len as i64).collect();
            (target, idxs)
        } else {
            let idx = self.stack.pop().unwrap_or(Value::Nil);
            let target = self.stack.pop().unwrap_or(Value::Nil);
            Self::throw_if_failure(&target)?;
            if let Some(map) = match &target {
                Value::Hash(map) => Some(map.clone()),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => match attributes.get("symbols") {
                    Some(Value::Hash(map)) => Some(map.clone()),
                    _ => None,
                },
                _ => None,
            } {
                if adverb_bits == 5 {
                    return Err(crate::value::RuntimeError::new(
                        "Unsupported combination of :exists and :v adverbs".to_string(),
                    ));
                }
                match &idx {
                    Value::Array(items, ..) => {
                        // Multi-dimensional hash path (%h{a;b;c}:exists): if we can
                        // traverse through nested hashes, treat this as a single exists.
                        if items.len() > 1 {
                            let mut cur: &Value = &target;
                            let mut traversed_nested = false;
                            let mut path_exists: Option<bool> = None;
                            for (i, key) in items.iter().enumerate() {
                                if let Value::Hash(cur_map) = cur {
                                    let key_s = key.to_string_value();
                                    if let Some(next) = cur_map.get(&key_s) {
                                        if i + 1 == items.len() {
                                            path_exists = Some(!matches!(next, Value::Nil));
                                        } else if matches!(next, Value::Hash(_)) {
                                            traversed_nested = true;
                                            cur = next;
                                        } else if traversed_nested {
                                            path_exists = Some(false);
                                            break;
                                        } else {
                                            break;
                                        }
                                    } else {
                                        if traversed_nested {
                                            path_exists = Some(false);
                                        }
                                        break;
                                    }
                                } else if traversed_nested {
                                    path_exists = Some(false);
                                    break;
                                } else {
                                    break;
                                }
                            }
                            if traversed_nested {
                                let exists = path_exists.unwrap_or(false);
                                self.stack.push(Value::Bool(exists ^ effective_negated));
                                return Ok(());
                            }
                        }
                        let pairs: Vec<(Value, bool)> = items
                            .iter()
                            .map(|k| {
                                let key = k.to_string_value();
                                (k.clone(), map.contains_key(&key))
                            })
                            .collect();
                        let result = match adverb_bits {
                            0 => Value::array(
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
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::Nil,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    // Whatever (*) — all hash keys.
                    Value::Whatever => {
                        let pairs: Vec<(Value, bool)> =
                            map.keys().map(|k| (Value::str(k.clone()), true)).collect();
                        let result = match adverb_bits {
                            0 => Value::array(
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
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::Nil,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                        let pairs: Vec<(Value, bool)> =
                            map.keys().map(|k| (Value::str(k.clone()), true)).collect();
                        let result = match adverb_bits {
                            0 => Value::array(
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
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::Nil,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    _ => {
                        let exists = map.contains_key(&idx.to_string_value());
                        let result_bool = exists ^ effective_negated;
                        let key = idx.clone();
                        let result = match adverb_bits {
                            0 | 5 => Value::Bool(result_bool),
                            // :kv — filter by actual existence, not negated result
                            1 => {
                                if exists {
                                    Value::array(vec![key, Value::Bool(result_bool)])
                                } else {
                                    Value::array(Vec::new())
                                }
                            }
                            2 => Value::array(vec![key, Value::Bool(result_bool)]),
                            // :p — filter by actual existence, not negated result
                            3 => {
                                if exists {
                                    Value::ValuePair(
                                        Box::new(key),
                                        Box::new(Value::Bool(result_bool)),
                                    )
                                } else {
                                    Value::array(Vec::new())
                                }
                            }
                            4 => {
                                Value::ValuePair(Box::new(key), Box::new(Value::Bool(result_bool)))
                            }
                            _ => Value::Bool(result_bool),
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                }
            }
            if let Value::Set(set) = &target {
                let exists_for_key = |key: &Value| set.contains(&key.to_string_value());
                let result = match &idx {
                    Value::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::Bool(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::Bool(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let Value::Bag(bag) = &target {
                let exists_for_key = |key: &Value| bag.contains_key(&key.to_string_value());
                let result = match &idx {
                    Value::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::Bool(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::Bool(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let Value::Mix(mix) = &target {
                let exists_for_key = |key: &Value| mix.contains_key(&key.to_string_value());
                let result = match &idx {
                    Value::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::Bool(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::Bool(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            {
                let is_stash = class_name == "Stash" && attributes.contains_key("symbols");
                if !is_stash
                    && let Some(result) = self.instance_exists_pos_result(
                        &target,
                        &idx,
                        effective_negated,
                        adverb_bits,
                    )?
                {
                    self.stack.push(result);
                    return Ok(());
                }
            }
            let idxs = match &idx {
                Value::Int(i) => vec![*i],
                Value::Array(items, ..) if crate::runtime::utils::is_shaped_array(&target) => {
                    // Shaped array: multi-dimensional exists (e.g. @arr[0;0]:exists)
                    let exists = Self::index_array_multidim(&target, items.as_ref(), false)
                        .ok()
                        .is_some_and(|v| !matches!(v, Value::Nil));
                    let result = Value::Bool(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
                Value::Array(items, ..) => items
                    .iter()
                    .map(|v| match v {
                        Value::Int(i) => *i,
                        _ => {
                            let n = v.to_bigint();
                            n.try_into().unwrap_or(0)
                        }
                    })
                    .collect(),
                // Whatever (*) — treat as zen slice (all elements)
                Value::Whatever => {
                    let len = match &target {
                        Value::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    let len = match &target {
                        Value::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                _ => {
                    // For hash access, delegate to single key exists
                    let exists = match (&target, &idx) {
                        (Value::Hash(map), Value::Str(key)) => map.contains_key(key.as_str()),
                        (Value::Hash(map), _) => map.contains_key(&idx.to_string_value()),
                        (Value::Set(set), Value::Str(key)) => set.contains(key.as_str()),
                        (Value::Set(set), other) => set.contains(&other.to_string_value()),
                        (Value::Bag(bag), Value::Str(key)) => bag.contains_key(key.as_str()),
                        (Value::Bag(bag), other) => bag.contains_key(&other.to_string_value()),
                        (Value::Mix(mix), Value::Str(key)) => mix.contains_key(key.as_str()),
                        (Value::Mix(mix), other) => mix.contains_key(&other.to_string_value()),
                        (
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            },
                            Value::Str(key),
                        ) if class_name == "Stash" => {
                            if let Some(Value::Hash(symbols)) = attributes.get("symbols") {
                                symbols.contains_key(key.as_str())
                            } else {
                                false
                            }
                        }
                        (
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            },
                            other,
                        ) if class_name == "Stash" => {
                            if let Some(Value::Hash(symbols)) = attributes.get("symbols") {
                                symbols.contains_key(&other.to_string_value())
                            } else {
                                false
                            }
                        }
                        _ => false,
                    };
                    let result = Value::Bool(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
            };
            (target, idxs)
        };

        // Uni: check exists based on codepoint count
        if let Value::Uni { text, .. } = &target {
            let len = text.chars().count() as i64;
            if indices.len() == 1 && !is_zen {
                let i = indices[0];
                let exists = i >= 0 && i < len;
                let result = exists ^ effective_negated;
                self.stack.push(Value::Bool(result));
                return Ok(());
            }
            let vals: Vec<Value> = indices
                .iter()
                .map(|&i| {
                    let exists = i >= 0 && i < len;
                    Value::Bool(exists ^ effective_negated)
                })
                .collect();
            self.stack.push(Value::array(vals));
            return Ok(());
        }

        let items = match &target {
            Value::Array(items, ..) => items.as_ref(),
            _ => &[] as &[Value],
        };

        let is_multi = indices.len() != 1 || is_zen;

        if !is_multi {
            // Single index
            let i = indices[0];
            let exists = i >= 0
                && items
                    .get(i as usize)
                    .is_some_and(|v| !matches!(v, Value::Nil));
            let result_bool = exists ^ effective_negated;
            let key = Value::Int(i);
            let result = match adverb_bits {
                0 | 5 => Value::Bool(result_bool),
                // :kv — return (index, bool) if exists, else ()
                1 => {
                    if exists {
                        Value::array(vec![key, Value::Bool(result_bool)])
                    } else {
                        Value::array(Vec::new())
                    }
                }
                // :!kv — always return (index, bool)
                2 => Value::array(vec![key, Value::Bool(result_bool)]),
                // :p — return Pair if exists, else ()
                3 => {
                    if exists {
                        Value::ValuePair(Box::new(key), Box::new(Value::Bool(result_bool)))
                    } else {
                        Value::array(Vec::new())
                    }
                }
                // :!p — always return Pair
                4 => Value::ValuePair(Box::new(key), Box::new(Value::Bool(result_bool))),
                _ => Value::Bool(result_bool),
            };
            self.stack.push(result);
            return Ok(());
        }

        // Multi-index: compute (index, exists_bool) pairs
        let pairs: Vec<(i64, bool)> = indices
            .iter()
            .map(|&i| {
                let exists = i >= 0
                    && items
                        .get(i as usize)
                        .is_some_and(|v| !matches!(v, Value::Nil));
                (i, exists)
            })
            .collect();

        let result = match adverb_bits {
            0 => {
                // Plain :exists — list of Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::Bool(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            1 => {
                // :kv — filter by original exists, interleave (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::Int(*i));
                        vals.push(Value::Bool(*exists ^ effective_negated));
                    }
                }
                Value::array(vals)
            }
            2 => {
                // :!kv — no filter, interleave all (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::Int(*i));
                    vals.push(Value::Bool(*exists ^ effective_negated));
                }
                Value::array(vals)
            }
            3 => {
                // :p — filter by original exists, return Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::ValuePair(
                            Box::new(Value::Int(*i)),
                            Box::new(Value::Bool(*exists ^ effective_negated)),
                        ));
                    }
                }
                Value::array(vals)
            }
            4 => {
                // :!p — no filter, return all Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::ValuePair(
                        Box::new(Value::Int(*i)),
                        Box::new(Value::Bool(*exists ^ effective_negated)),
                    ));
                }
                Value::array(vals)
            }
            5 => {
                // :!v — no filter, return all Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::Bool(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            _ => Value::Nil,
        };

        self.stack.push(result);
        Ok(())
    }

    /// Delete element(s) from an array container.
    /// When idx is a single value, delete that one element.
    /// When idx is an Array, delete each element (slice delete).
    /// When idx is a Range, expand to indices and delete each.
    /// Delete element(s) from an array container.
    /// When idx is a single value, delete that one element.
    /// When idx is an Array, delete each element (slice delete).
    /// When idx is a Range, expand to indices and delete each.
    fn delete_from_array(
        container: &mut Value,
        idx: Value,
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        match idx {
            Value::Array(indices, ..) => {
                let indices_vec: Vec<Value> = indices.to_vec();
                let mut results = Vec::with_capacity(indices_vec.len());
                for i in indices_vec {
                    let r =
                        Self::delete_array_multidim(container, std::slice::from_ref(&i), hole_type)
                            .unwrap_or(Value::Nil);
                    results.push(r);
                }
                Ok(Value::array(results))
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                let expanded = crate::runtime::utils::value_to_list(&idx);
                let mut results = Vec::with_capacity(expanded.len());
                for i in expanded {
                    let r =
                        Self::delete_array_multidim(container, std::slice::from_ref(&i), hole_type)
                            .unwrap_or(Value::Nil);
                    results.push(r);
                }
                Ok(Value::array(results))
            }
            _ => {
                let r =
                    Self::delete_array_multidim(container, std::slice::from_ref(&idx), hole_type)?;
                Ok(r)
            }
        }
    }

    fn delete_from_missing_container(idx: Value) -> Value {
        match idx {
            Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
            _ => Value::Nil,
        }
    }

    fn delete_from_container(
        container: &mut Value,
        idx: Value,
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        Ok(match container {
            Value::Hash(hash) => match idx {
                Value::Whatever => {
                    let h = Arc::make_mut(hash);
                    let removed: Vec<Value> = h.values().cloned().collect();
                    h.clear();
                    Value::array(removed)
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    let h = Arc::make_mut(hash);
                    let removed: Vec<Value> = h.values().cloned().collect();
                    h.clear();
                    Value::array(removed)
                }
                Value::Array(keys, ..) => {
                    let h = Arc::make_mut(hash);
                    let removed = keys
                        .iter()
                        .map(|key| h.remove(&key.to_string_value()).unwrap_or(Value::Nil))
                        .collect();
                    Value::array(removed)
                }
                _ => Arc::make_mut(hash)
                    .remove(&idx.to_string_value())
                    .unwrap_or(Value::Nil),
            },
            Value::Package(type_name) if type_name == "Hash" || type_name == "Hash:U" => {
                match idx {
                    Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
                    _ => Value::Nil,
                }
            }
            Value::Array(..) => Self::delete_from_array(container, idx, hole_type)?,
            Value::Set(set) => match idx {
                Value::Array(keys, ..) => {
                    let s = Arc::make_mut(set);
                    let removed = keys
                        .iter()
                        .map(|key| Value::Bool(s.remove(&key.to_string_value())))
                        .collect();
                    Value::array(removed)
                }
                _ => Value::Bool(Arc::make_mut(set).remove(&idx.to_string_value())),
            },
            Value::Bag(bag) => match idx {
                Value::Array(keys, ..) => {
                    let b = Arc::make_mut(bag);
                    let removed = keys
                        .iter()
                        .map(|key| Value::Int(b.remove(&key.to_string_value()).unwrap_or(0)))
                        .collect();
                    Value::array(removed)
                }
                _ => Value::Int(
                    Arc::make_mut(bag)
                        .remove(&idx.to_string_value())
                        .unwrap_or(0),
                ),
            },
            Value::Mix(mix) => match idx {
                Value::Array(keys, ..) => {
                    let m = Arc::make_mut(mix);
                    let removed = keys
                        .iter()
                        .map(|key| Value::Num(m.remove(&key.to_string_value()).unwrap_or(0.0)))
                        .collect();
                    Value::array(removed)
                }
                _ => Value::Num(
                    Arc::make_mut(mix)
                        .remove(&idx.to_string_value())
                        .unwrap_or(0.0),
                ),
            },
            _ => match idx {
                Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
                _ => Value::Nil,
            },
        })
    }

    fn mix_weight_as_value(weight: f64) -> Value {
        if weight.is_finite() && weight.fract() == 0.0 {
            Value::Int(weight as i64)
        } else {
            Value::Num(weight)
        }
    }
}

/// Extract (start, end, excl_start, excl_end) from a Range value.
fn range_params(v: &Value) -> Option<(i64, i64, bool, bool)> {
    match v {
        Value::Range(a, b) => Some((*a, *b, false, false)),
        Value::RangeExcl(a, b) => Some((*a, *b, false, true)),
        Value::RangeExclStart(a, b) => Some((*a + 1, *b, true, false)),
        Value::RangeExclBoth(a, b) => Some((*a + 1, *b, true, true)),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            if !start.is_numeric() || !end.is_numeric() {
                return None;
            }
            let s = start.to_f64() as i64;
            let e = end.to_f64() as i64;
            if start.to_f64().is_nan() || end.to_f64().is_nan() {
                return None;
            }
            let s = if *excl_start { s + 1 } else { s };
            Some((s, e, *excl_start, *excl_end))
        }
        _ => None,
    }
}
