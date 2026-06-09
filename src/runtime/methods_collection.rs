use super::*;

impl Interpreter {
    pub(super) fn dispatch_to_set(&self, target: Value) -> Result<Value, RuntimeError> {
        crate::builtins::quanthash_coerce::to_set(target)
    }

    /// Check if a value is lazy/infinite and cannot be coerced to a QuantHash.
    pub(crate) fn is_lazy_for_coerce(value: &Value) -> bool {
        match value {
            Value::LazyList(_) => true,
            Value::Array(_, kind) if kind.is_lazy() => true,
            Value::GenericRange { start, end, .. } => {
                Self::is_infinite_endpoint(start) || Self::is_infinite_endpoint(end)
            }
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            _ => false,
        }
    }

    /// Operations that need the full (finite) list cannot run on a lazy or
    /// infinite source. Returns `Some(X::Cannot::Lazy)` when `method` is such an
    /// operation and `target` is lazy/infinite, else `None`. Matches raku, which
    /// throws rather than hanging while materializing the whole sequence.
    /// `.head`/`.first`/`.map`/`[]` handle laziness and are NOT listed here.
    pub(crate) fn lazy_guard_error(method: &str, target: &Value) -> Option<RuntimeError> {
        if !Self::is_lazy_for_coerce(target) {
            return None;
        }
        match method {
            "classify" | "categorize" => Some(RuntimeError::cannot_lazy_with_action(
                method,
                "Hash[Any,Mu]",
            )),
            "sort" | "combinations" | "permutations" => Some(RuntimeError::cannot_lazy(method)),
            _ => None,
        }
    }

    /// Whether `.map`/`.grep` on `target` should produce a truly lazy pipeline
    /// stage (a `LazyList` carrying a [`crate::value::MapGrepSpec`]) instead of
    /// materializing the source. Intentionally narrow to keep the blast radius
    /// small: only an infinite integer `Range` (the headline crash/slow case)
    /// and continuation of an existing lazy pipeline (chained `.map`/`.grep`).
    /// Gathers / sequence / scan lists keep their current dispatch unchanged.
    pub(crate) fn is_lazy_pipe_source(target: &Value) -> bool {
        match target {
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            Value::LazyList(ll) => ll.lazy_pipe.is_some(),
            _ => false,
        }
    }

    /// Build a lazy `map`/`grep` pipeline stage over `target` if `func` is
    /// eligible for single-element pull (arity-1, no full-binding signature and
    /// no grep adverbs handled by the caller). Returns `None` to fall back to
    /// the eager path.
    pub(crate) fn make_lazy_pipe(target: Value, func: Value, is_grep: bool) -> Option<Value> {
        // Only callbacks that consume one element per call can be pulled lazily.
        // A multi-arity block (`-> $a, $b { }`) or a slurpy param (`*@a`)
        // consumes a chunk of the source per call, which single-element pull
        // cannot reproduce; defer those to the eager path (unchanged behaviour).
        // Other signature features (types, defaults, where, …) still bind one
        // element per call in `eval_map_over_items`, so they stay eligible.
        if let Value::Sub(data) = &func {
            let arity = data
                .params
                .len()
                .saturating_sub(data.assumed_positional.len());
            let has_slurpy = data.param_defs.iter().any(|pd| pd.slurpy);
            if arity > 1 || has_slurpy {
                return None;
            }
        }
        Some(Value::LazyList(std::sync::Arc::new(
            crate::value::LazyList::new_pipe(target, func, is_grep),
        )))
    }

    fn is_infinite_endpoint(v: &Value) -> bool {
        match v {
            Value::Num(n) => n.is_infinite(),
            Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
            Value::Whatever | Value::HyperWhatever => true,
            _ => false,
        }
    }

    pub(super) fn dispatch_to_bag_with_what(
        &self,
        target: Value,
        what: &str,
    ) -> Result<Value, RuntimeError> {
        crate::builtins::quanthash_coerce::to_bag(target, what)
    }

    /// Check if a value is a lazy iterable (infinite range, lazy list, etc.)
    pub(crate) fn is_lazy_for_set_ops(v: &Value) -> bool {
        match v {
            Value::LazyList(_) => true,
            Value::Array(_, kind) if kind.is_lazy() => true,
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            Value::GenericRange { end, .. } => {
                let end_f = end.to_f64();
                end_f.is_infinite() && end_f.is_sign_positive()
            }
            _ => false,
        }
    }

    pub(super) fn dispatch_to_mix(&self, target: Value) -> Result<Value, RuntimeError> {
        crate::builtins::quanthash_coerce::to_mix(target)
    }

    pub(super) fn dispatch_to_map(&self, target: Value) -> Result<Value, RuntimeError> {
        // Map is stored as Hash internally in mutsu.
        // When converting from Hash to Map, decontainerize all values
        // (Map values are not wrapped in Scalar containers).
        match target {
            Value::Hash(ref map) => {
                // Check if already a Map (same identity)
                if self
                    .container_type_metadata(&target)
                    .and_then(|info| info.declared_type)
                    .is_some_and(|dt| dt == "Map")
                {
                    return Ok(target);
                }
                // Decontainerize values for the new Map
                let deconted: HashMap<String, Value> = map
                    .iter()
                    .map(|(k, v)| {
                        let deconted = match v {
                            Value::Scalar(inner) => (**inner).clone(),
                            other => other.clone(),
                        };
                        (k.clone(), deconted)
                    })
                    .collect();
                Ok(Value::hash(deconted))
            }
            _ => self.dispatch_to_hash_impl(target, true),
        }
    }

    pub(super) fn dispatch_to_hash(&self, target: Value) -> Result<Value, RuntimeError> {
        self.dispatch_to_hash_impl(target, true)
    }

    fn dispatch_to_hash_impl(&self, target: Value, check_odd: bool) -> Result<Value, RuntimeError> {
        match target {
            Value::Hash(_) => Ok(target),
            Value::Array(items, ..) => Self::items_to_hash(items.as_ref(), check_odd),
            Value::Seq(items) | Value::Slip(items) => {
                Self::items_to_hash(items.as_ref(), check_odd)
            }
            Value::Set(s, _) => {
                let mut map = HashMap::new();
                let mut original_keys = HashMap::new();
                let mut has_typed = false;
                for k in s.iter() {
                    map.insert(k.clone(), Value::Bool(true));
                    let typed = s.typed_key(k);
                    if !matches!(&typed, Value::Str(sv) if sv.as_ref() == k) {
                        has_typed = true;
                        original_keys.insert(k.clone(), typed);
                    }
                }
                let result = Value::hash(map);
                if has_typed {
                    original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                    super::utils::register_hash_original_keys(&result, original_keys);
                }
                Ok(result)
            }
            Value::Bag(b, _) => {
                let mut map = HashMap::new();
                let mut original_keys = HashMap::new();
                let mut has_typed = false;
                for (k, v) in b.iter() {
                    map.insert(k.clone(), Value::Int(*v));
                    let typed = b.typed_key(k);
                    if !matches!(&typed, Value::Str(sv) if sv.as_ref() == k) {
                        has_typed = true;
                        original_keys.insert(k.clone(), typed);
                    }
                }
                let result = Value::hash(map);
                if has_typed {
                    original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                    super::utils::register_hash_original_keys(&result, original_keys);
                }
                Ok(result)
            }
            Value::Mix(m, _) => {
                let mut map = HashMap::new();
                let mut original_keys = HashMap::new();
                let mut has_typed = false;
                for (k, v) in m.iter() {
                    map.insert(k.clone(), crate::value::mix_weight_to_value(*v));
                    let typed = m.typed_key(k);
                    if !matches!(&typed, Value::Str(sv) if sv.as_ref() == k) {
                        has_typed = true;
                        original_keys.insert(k.clone(), typed);
                    }
                }
                let result = Value::hash(map);
                if has_typed {
                    original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                    super::utils::register_hash_original_keys(&result, original_keys);
                }
                Ok(result)
            }
            Value::Instance {
                ref class_name,
                ref attributes,
                ..
            } if class_name == "Match" => {
                // %($/) returns the named captures hash
                if let Some(named) = attributes.get("named") {
                    Ok(named.clone())
                } else {
                    Ok(Value::hash(HashMap::new()))
                }
            }
            other => {
                // Single non-Pair scalar: this is 1 element (odd), so throw
                if check_odd {
                    if let Value::Pair(k, v) = other {
                        let mut map = HashMap::new();
                        map.insert(k.to_string(), *v);
                        return Ok(Value::hash(map));
                    }
                    return Err(Self::make_odd_number_error(&[other]));
                }
                let mut map = HashMap::new();
                map.insert(other.to_string_value(), Value::Bool(true));
                Ok(Value::hash(map))
            }
        }
    }

    /// Convert a slice of items to a Hash, optionally checking for odd element count.
    fn items_to_hash(items: &[Value], check_odd: bool) -> Result<Value, RuntimeError> {
        if check_odd {
            // Count non-Pair elements to detect odd count
            let non_pair_count = items
                .iter()
                .filter(|v| !matches!(v, Value::Pair(..) | Value::ValuePair(..)))
                .count();
            if non_pair_count % 2 != 0 {
                return Err(Self::make_odd_number_error(items));
            }
        }
        let mut map = HashMap::new();
        let mut iter = items.iter();
        while let Some(item) = iter.next() {
            match item {
                Value::Pair(k, v) => {
                    map.insert(k.clone(), *v.clone());
                }
                Value::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), *v.clone());
                }
                other => {
                    let key = other.to_string_value();
                    let value = iter.next().cloned().unwrap_or(Value::Nil);
                    map.insert(key, value);
                }
            }
        }
        Ok(Value::hash(map))
    }

    /// Create an X::Hash::Store::OddNumber error.
    fn make_odd_number_error(items: &[Value]) -> RuntimeError {
        let count = items.len();
        let last = items
            .last()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let message = if count == 1 {
            format!(
                "Odd number of elements found where hash initializer expected:\n\
                 Only saw: {}",
                last
            )
        } else {
            format!(
                "Odd number of elements found where hash initializer expected:\n\
                 Found {} (implicit) elements:\nLast element seen: {}",
                count, last
            )
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(
            crate::symbol::Symbol::intern("X::Hash::Store::OddNumber"),
            attrs,
        );
        RuntimeError {
            exception: Some(Box::new(ex)),
            ..RuntimeError::new(message)
        }
    }
}
