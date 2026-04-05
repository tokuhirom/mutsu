use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch remaining methods by name (Supply, networking, temporal, misc).
    /// Returns Some(result) if the method was handled, None to fall through.
    #[allow(clippy::too_many_lines)]
    pub(super) fn dispatch_method_by_name_3(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "from-list" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    return Some(self.dispatch_supply_from_list(&args));
                }
                None
            }
            "repository-for-name" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "CompUnit::RepositoryRegistry"
                {
                    let name = args.first().map(Value::to_string_value).unwrap_or_default();
                    if let Some(prefix) = name.strip_prefix("file#") {
                        let new_args = vec![Value::Pair(
                            "prefix".to_string(),
                            Box::new(Value::str(prefix.to_string())),
                        )];
                        return Some(self.call_method_with_values(
                            Value::Package(Symbol::intern("CompUnit::Repository::FileSystem")),
                            "new",
                            new_args,
                        ));
                    }
                    return Some(Ok(Value::Nil));
                }
                None
            }
            "signal" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    return Some(self.dispatch_supply_signal(&args));
                }
                None
            }
            "on-demand" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    return Some(self.dispatch_supply_on_demand(&args));
                }
                if let Value::Instance { ref class_name, .. } = target
                    && class_name == "Supply"
                {
                    return Some(Err(RuntimeError::new(
                        "Cannot call on-demand on a Supply instance",
                    )));
                }
                None
            }
            "find" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Encoding::Registry"
                {
                    return Some(self.dispatch_encoding_registry_find(&args));
                }
                None
            }
            "register" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Encoding::Registry"
                {
                    return Some(self.dispatch_encoding_registry_register(&args));
                }
                None
            }
            "connect" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::INET"
                {
                    return Some(self.dispatch_socket_inet_connect(&args));
                }
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::Async"
                {
                    return Some(self.dispatch_socket_async_connect(&args));
                }
                None
            }
            "listen" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::INET"
                {
                    return Some(self.dispatch_socket_inet_listen(&args));
                }
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::Async"
                {
                    return Some(self.dispatch_socket_async_listen(&args));
                }
                None
            }
            "now" => self.dispatch_datetime_now(&target, &args),
            "Date" if args.is_empty() => {
                if let Value::Package(ref class_name) = target {
                    let cn = class_name.resolve();
                    let mro = self.class_mro(&cn);
                    if mro.iter().any(|name| name == "DateTime")
                        || mro.iter().any(|name| name == "Date")
                    {
                        return Some(Ok(Value::Package(Symbol::intern("Date"))));
                    }
                }
                None
            }
            "DateTime" if args.is_empty() => {
                if let Value::Package(ref class_name) = target {
                    let cn = class_name.resolve();
                    let mro = self.class_mro(&cn);
                    if mro.iter().any(|name| name == "DateTime")
                        || mro.iter().any(|name| name == "Date")
                    {
                        return Some(Ok(Value::Package(Symbol::intern("DateTime"))));
                    }
                }
                None
            }
            "today" if args.is_empty() => {
                if let Value::Package(ref class_name) = target {
                    let cn = class_name.resolve();
                    let is_date_like =
                        cn == "Date" || self.class_mro(&cn).iter().any(|name| name == "Date");
                    if is_date_like {
                        use crate::builtins::methods_0arg::temporal;
                        let secs = crate::value::current_time_secs_f64() as i64;
                        let epoch_days = secs.div_euclid(86400);
                        let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                        if cn == "Date" {
                            return Some(Ok(temporal::make_date(y, m, d)));
                        }
                        // For Date subclasses, construct via the subclass constructor
                        let date_args = vec![
                            Value::Pair("year".to_string(), Box::new(Value::Int(y))),
                            Value::Pair("month".to_string(), Box::new(Value::Int(m))),
                            Value::Pair("day".to_string(), Box::new(Value::Int(d))),
                        ];
                        return Some(self.dispatch_new(target.clone(), date_args));
                    }
                }
                None
            }
            "grab" => self.dispatch_grab_method(target, args),
            "grabpairs" => self.dispatch_grabpairs_method(target, args),
            "skip" => Some(self.dispatch_skip_method(target, args)),
            "join" if args.len() <= 1 => self.dispatch_join_method(target, args),
            "grep" => Some(self.dispatch_grep(target, &args)),
            "toggle" => Some(self.dispatch_toggle(target, &args)),
            "eager" if args.is_empty() => Some(self.dispatch_eager_method(target)),
            "is-lazy" if args.is_empty() => Some(Ok(self.dispatch_is_lazy_method(&target))),
            "first" if !args.is_empty() => Some(self.dispatch_first(target, &args)),
            "first" if args.is_empty() => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return Some(self.dispatch_first(target, &args));
                }
                None
            }
            "tree" if !args.is_empty() => Some(self.dispatch_tree(target, &args)),
            "keys" if args.is_empty() => self.dispatch_keys_method(target),
            "values" if args.is_empty() => Some(self.dispatch_values_method(target)),
            "AT-KEY" if args.len() == 1 => self.dispatch_at_key_method(&target, &args),
            "rotate" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    Some(self.dispatch_supply_transform(target, "rotate", &args))
                } else {
                    Some(self.dispatch_rotate(target, &args))
                }
            }
            _ => None,
        }
    }

    /// Dispatch the "grab" method (Supply.grab, MixHash.grab).
    fn dispatch_grab_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } = target
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_grab(attributes, &args));
        }
        if let Value::Package(ref class_name) = target
            && class_name == "Supply"
        {
            return Some(Err(RuntimeError::new(
                "Cannot call .grab on a Supply type object",
            )));
        }
        // MixHash.grab: select and remove random element(s)
        if let Value::Mix(ref mix, ..) = target {
            let count = if args.is_empty() {
                1usize
            } else {
                match &args[0] {
                    Value::Whatever => mix.len(),
                    v => v.to_f64().max(0.0) as usize,
                }
            };
            if mix.is_empty() || count == 0 {
                return Some(Ok(Value::Nil));
            }
            use crate::builtins::rng::builtin_rand;
            let mut grabbed = Vec::new();
            let mut remaining = (**mix).clone();
            for _ in 0..count {
                if remaining.is_empty() {
                    break;
                }
                let ks: Vec<String> = remaining.keys().cloned().collect();
                let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
                let key = ks[idx].clone();
                remaining.remove(&key);
                grabbed.push(Value::str(key));
            }
            return Some(Ok(if grabbed.len() == 1 && args.is_empty() {
                grabbed.into_iter().next().unwrap()
            } else {
                Value::Seq(Arc::new(grabbed))
            }));
        }
        // BagHash.grab: select and remove random element(s)
        if let Value::Bag(ref bag, true) = target {
            return Some(self.dispatch_bag_grab(&bag.counts, &args));
        }
        None
    }

    /// Dispatch the "grabpairs" method (MixHash.grabpairs, BagHash.grabpairs).
    fn dispatch_grabpairs_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // BagHash.grabpairs
        if let Value::Bag(ref bag, true) = target {
            return Some(self.dispatch_bag_grabpairs(&bag.counts, &args));
        }
        let Value::Mix(ref mix, ..) = target else {
            return None;
        };
        let count = if args.is_empty() {
            1usize
        } else {
            match &args[0] {
                Value::Whatever => mix.len(),
                v => v.to_f64().max(0.0) as usize,
            }
        };
        if mix.is_empty() || count == 0 {
            return Some(Ok(Value::Seq(Arc::new(Vec::new()))));
        }
        use crate::builtins::rng::builtin_rand;
        let mut grabbed = Vec::new();
        let mut remaining = (**mix).clone();
        for _ in 0..count {
            if remaining.is_empty() {
                break;
            }
            let ks: Vec<String> = remaining.keys().cloned().collect();
            let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
            let key = ks[idx].clone();
            let weight = remaining.remove(&key).unwrap_or(0.0);
            let weight_val = if (weight - (weight as i64 as f64)).abs() < f64::EPSILON {
                Value::Int(weight as i64)
            } else {
                Value::Num(weight)
            };
            grabbed.push(Value::Pair(key, Box::new(weight_val)));
        }
        // TODO: compile to bytecode - should mutate the original variable
        Some(Ok(Value::Seq(Arc::new(grabbed))))
    }

    /// Helper for BagHash.grab (works on a clone, does not mutate the original).
    fn dispatch_bag_grab(
        &mut self,
        bag: &std::collections::HashMap<String, i64>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::rng::builtin_rand;
        let count = if args.is_empty() {
            1usize
        } else {
            match &args[0] {
                Value::Whatever => bag.values().sum::<i64>() as usize,
                v => v.to_f64().max(0.0) as usize,
            }
        };
        if bag.is_empty() || count == 0 {
            if args.is_empty() {
                return Ok(Value::Nil);
            }
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }
        let mut grabbed = Vec::new();
        let mut remaining = bag.clone();
        for _ in 0..count {
            if remaining.is_empty() {
                break;
            }
            let total: i64 = remaining.values().sum();
            if total <= 0 {
                break;
            }
            let r = (builtin_rand() * total as f64) as i64;
            let mut cumulative = 0i64;
            let mut chosen_key = String::new();
            for (k, v) in &remaining {
                cumulative += v;
                if r < cumulative {
                    chosen_key = k.clone();
                    break;
                }
            }
            if let Some(c) = remaining.get_mut(&chosen_key) {
                *c -= 1;
                if *c <= 0 {
                    remaining.remove(&chosen_key);
                }
            }
            grabbed.push(Value::str(chosen_key));
        }
        Ok(if grabbed.len() == 1 && args.is_empty() {
            grabbed.into_iter().next().unwrap()
        } else {
            Value::Seq(Arc::new(grabbed))
        })
    }

    /// Helper for BagHash.grabpairs (works on a clone, does not mutate the original).
    fn dispatch_bag_grabpairs(
        &mut self,
        bag: &std::collections::HashMap<String, i64>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::rng::builtin_rand;
        let count = if args.is_empty() {
            1usize
        } else {
            match &args[0] {
                Value::Whatever => bag.len(),
                v => v.to_f64().max(0.0) as usize,
            }
        };
        if bag.is_empty() || count == 0 {
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }
        let mut grabbed = Vec::new();
        let mut remaining = bag.clone();
        for _ in 0..count {
            if remaining.is_empty() {
                break;
            }
            let ks: Vec<String> = remaining.keys().cloned().collect();
            let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
            let key = ks[idx].clone();
            let val = remaining.remove(&key).unwrap_or(0);
            grabbed.push(Value::Pair(key, Box::new(Value::Int(val))));
        }
        Ok(Value::Seq(Arc::new(grabbed)))
    }

    /// Dispatch the "skip" method.
    fn dispatch_skip_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } = target
            && class_name == "Supply"
        {
            return self.dispatch_supply_skip(attributes, &args);
        }
        let n = if args.is_empty() {
            1usize
        } else {
            args[0].to_f64().max(0.0) as usize
        };
        let items = crate::runtime::utils::value_to_list(&target);
        let result: Vec<Value> = items.into_iter().skip(n).collect();
        Ok(Value::Seq(Arc::new(result)))
    }

    /// Dispatch the "join" method.
    fn dispatch_join_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            target,
            Value::Array(..)
                | Value::Seq(..)
                | Value::Slip(..)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }
        ) {
            return None;
        }
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let items = if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else if let Some(inner) = target.as_list_items() {
            inner.to_vec()
        } else {
            Self::value_to_list(&target)
        };
        let mut parts = Vec::with_capacity(items.len());
        for v in &items {
            if matches!(v, Value::Instance { .. }) {
                let s = match self.call_method_with_values(v.clone(), "Str", vec![]) {
                    Ok(s) => s,
                    Err(e) => return Some(Err(e)),
                };
                parts.push(s.to_string_value());
            } else {
                parts.push(v.to_string_value());
            }
        }
        let joined = parts.join(&sep);
        Some(Ok(Value::str(joined)))
    }

    /// Dispatch the "eager" method.
    fn dispatch_eager_method(&mut self, target: Value) -> Result<Value, RuntimeError> {
        match target {
            Value::LazyList(list) => Ok(Value::array(self.force_lazy_list_bridge(&list)?)),
            Value::Array(..) | Value::Seq(..) | Value::Slip(..) => Ok(target),
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                Ok(Value::array(crate::runtime::utils::value_to_list(&target)))
            }
            other => Ok(other),
        }
    }

    /// Dispatch the "is-lazy" method.
    fn dispatch_is_lazy_method(&self, target: &Value) -> Value {
        let value_is_lazy = |v: &Value| match v {
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
        };
        let is_lazy = if value_is_lazy(target) {
            true
        } else if let Some(items) = target.as_list_items() {
            items.iter().any(value_is_lazy)
        } else {
            false
        };
        Value::Bool(is_lazy)
    }

    /// Dispatch "keys" method.
    fn dispatch_keys_method(&mut self, target: Value) -> Option<Result<Value, RuntimeError>> {
        match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => {
                let keys = match attributes.get("symbols") {
                    Some(Value::Hash(map)) => {
                        map.keys().cloned().map(Value::str).collect::<Vec<Value>>()
                    }
                    _ => Vec::new(),
                };
                Some(Ok(Value::array(keys)))
            }
            Value::Hash(ref map) => {
                if let Some(info) = self.container_type_metadata(&target)
                    && let Some(key_constraint) = info.key_type
                {
                    let mut keys = Vec::with_capacity(map.len());
                    for key in map.keys() {
                        let key_value = Value::str(key.clone());
                        let coerced = self
                            .try_coerce_value_for_constraint(&key_constraint, key_value)
                            .unwrap_or_else(|_| Value::str(key.clone()));
                        keys.push(coerced);
                    }
                    return Some(Ok(Value::array(keys)));
                }
                let keys = map.keys().cloned().map(Value::str).collect::<Vec<Value>>();
                Some(Ok(Value::array(keys)))
            }
            Value::Mixin(inner, _) if matches!(inner.as_ref(), Value::Hash(_)) => {
                Some(self.call_method_with_values(inner.as_ref().clone(), "keys", vec![]))
            }
            _ => None,
        }
    }

    /// Dispatch "values" method.
    fn dispatch_values_method(&self, target: Value) -> Result<Value, RuntimeError> {
        match target {
            Value::Capture { positional, named } => {
                let mut vals = positional.clone();
                vals.extend(named.values().cloned());
                Ok(Value::array(vals))
            }
            Value::Array(items, ..) => Ok(Value::array(items.to_vec())),
            Value::Hash(map) => Ok(Value::array(map.values().cloned().collect())),
            Value::Pair(_, value) => Ok(Value::array(vec![*value.clone()])),
            Value::ValuePair(_, value) => Ok(Value::array(vec![*value.clone()])),
            _ => Ok(Value::array(Vec::new())),
        }
    }

    /// Dispatch "AT-KEY" method.
    fn dispatch_at_key_method(
        &self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match (target, &args[0]) {
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                idx,
            ) if class_name == "Stash" => {
                if let Some(Value::Hash(symbols)) = attributes.get("symbols") {
                    let stash_lookup = |raw_key: &str| {
                        if let Some(value) = symbols.get(raw_key) {
                            return Some(value.clone());
                        }
                        if !raw_key.starts_with('$')
                            && !raw_key.starts_with('@')
                            && !raw_key.starts_with('%')
                            && !raw_key.starts_with('&')
                        {
                            let scalar = format!("${raw_key}");
                            if let Some(value) = symbols.get(&scalar) {
                                return Some(value.clone());
                            }
                        }
                        None
                    };

                    if let Value::Array(items, ..) = idx {
                        let values = items
                            .iter()
                            .map(|item| {
                                let key = item.to_string_value();
                                stash_lookup(&key).unwrap_or(Value::Nil)
                            })
                            .collect::<Vec<_>>();
                        return Some(Ok(Value::array(values)));
                    }

                    let key = idx.to_string_value();
                    if let Some(value) = stash_lookup(&key) {
                        return Some(Ok(value));
                    }
                }
                Some(Ok(Value::Nil))
            }
            (Value::Pair(key, value), idx) => {
                if key == &idx.to_string_value() {
                    Some(Ok(*value.clone()))
                } else {
                    Some(Ok(Value::Nil))
                }
            }
            (Value::ValuePair(key, value), idx) => {
                if key.to_string_value() == idx.to_string_value() {
                    Some(Ok(*value.clone()))
                } else {
                    Some(Ok(Value::Nil))
                }
            }
            _ => None,
        }
    }
}
