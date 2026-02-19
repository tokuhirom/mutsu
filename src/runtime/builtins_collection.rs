use super::*;

impl Interpreter {
    pub(super) fn builtin_elems(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Array(items)) => Value::Int(items.len() as i64),
            Some(Value::LazyList(list)) => Value::Int(self.force_lazy_list(&list)?.len() as i64),
            Some(Value::Hash(items)) => Value::Int(items.len() as i64),
            Some(Value::Str(s)) => Value::Int(s.chars().count() as i64),
            _ => Value::Int(0),
        })
    }

    pub(super) fn builtin_set(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut elems = HashSet::new();
        for arg in args {
            match arg {
                Value::Array(items) => {
                    for item in items {
                        elems.insert(item.to_string_value());
                    }
                }
                other => {
                    elems.insert(other.to_string_value());
                }
            }
        }
        Ok(Value::Set(elems))
    }

    pub(super) fn builtin_bag(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut counts: HashMap<String, i64> = HashMap::new();
        for arg in args {
            match arg {
                Value::Array(items) => {
                    for item in items {
                        *counts.entry(item.to_string_value()).or_insert(0) += 1;
                    }
                }
                other => {
                    *counts.entry(other.to_string_value()).or_insert(0) += 1;
                }
            }
        }
        Ok(Value::Bag(counts))
    }

    pub(super) fn builtin_mix(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut weights: HashMap<String, f64> = HashMap::new();
        for arg in args {
            match arg {
                Value::Array(items) => {
                    for item in items {
                        *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                    }
                }
                other => {
                    *weights.entry(other.to_string_value()).or_insert(0.0) += 1.0;
                }
            }
        }
        Ok(Value::Mix(weights))
    }

    pub(super) fn builtin_hash(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut flat_values = Vec::new();
        for arg in args {
            flat_values.extend(Self::value_to_list(arg));
        }
        let mut map = HashMap::new();
        let mut iter = flat_values.into_iter();
        while let Some(item) = iter.next() {
            match item {
                Value::Pair(key, boxed_val) => {
                    map.insert(key, *boxed_val);
                }
                other => {
                    let key = other.to_string_value();
                    let value = iter.next().unwrap_or(Value::Nil);
                    map.insert(key, value);
                }
            }
        }
        Ok(Value::Hash(map))
    }

    pub(super) fn builtin_junction(
        &self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let kind = match name {
            "any" => JunctionKind::Any,
            "all" => JunctionKind::All,
            "one" => JunctionKind::One,
            _ => JunctionKind::None,
        };
        let mut elems = Vec::new();
        for arg in args {
            match arg {
                Value::Array(items) => elems.extend(items),
                other => elems.push(other),
            }
        }
        Ok(Value::Junction {
            kind,
            values: elems,
        })
    }

    pub(super) fn builtin_pair(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let key = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let val = args.get(1).cloned().unwrap_or(Value::Nil);
        Ok(Value::Pair(key, Box::new(val)))
    }

    pub(super) fn builtin_keys(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Hash(items)) => {
                Value::Array(items.keys().map(|k| Value::Str(k.clone())).collect())
            }
            _ => Value::Array(Vec::new()),
        })
    }

    pub(super) fn builtin_values(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Hash(items)) => Value::Array(items.values().cloned().collect()),
            _ => Value::Array(Vec::new()),
        })
    }

    pub(super) fn builtin_abs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Int(i)) => Value::Int(i.abs()),
            Some(Value::Num(f)) => Value::Num(f.abs()),
            _ => Value::Int(0),
        })
    }

    pub(super) fn builtin_min(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(args
            .iter()
            .cloned()
            .min_by(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => a.to_string_value().cmp(&b.to_string_value()),
            })
            .unwrap_or(Value::Nil))
    }

    pub(super) fn builtin_max(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(args
            .iter()
            .cloned()
            .max_by(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => a.to_string_value().cmp(&b.to_string_value()),
            })
            .unwrap_or(Value::Nil))
    }

    pub(super) fn builtin_shift(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items)) => {
                if items.is_empty() {
                    Value::Nil
                } else {
                    items.remove(0)
                }
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_pop(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items)) => items.pop().unwrap_or(Value::Nil),
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_join(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let list = args.get(1).cloned();
        Ok(match list {
            Some(Value::Array(items)) => {
                let joined = items
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join(&sep);
                Value::Str(joined)
            }
            _ => Value::Str(String::new()),
        })
    }

    pub(super) fn builtin_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::Array(result))
    }

    pub(super) fn builtin_flat(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();
        for arg in args {
            Self::flat_into(arg, &mut result);
        }
        Ok(Value::Array(result))
    }

    pub(crate) fn flat_into(val: &Value, out: &mut Vec<Value>) {
        match val {
            Value::Array(items) => {
                for item in items {
                    Self::flat_into(item, out);
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..) => {
                out.extend(Self::value_to_list(val));
            }
            other => out.push(other.clone()),
        }
    }

    pub(super) fn builtin_slip(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Array(elems) => items.extend(elems.clone()),
                Value::Slip(elems) => items.extend(elems.clone()),
                other => items.push(other.clone()),
            }
        }
        Ok(Value::Slip(items))
    }

    pub(super) fn builtin_reverse(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Array(mut items)) => {
                items.reverse();
                Value::Array(items)
            }
            Some(Value::Str(s)) => Value::Str(s.chars().rev().collect()),
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_sort(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Array(mut items)) => {
                items.sort_by_key(|a| a.to_string_value());
                Value::Array(items)
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_map(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                Value::Array(items) => list_items.extend(items.clone()),
                Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                Value::RangeExclStart(a, b) => list_items.extend((*a + 1..=*b).map(Value::Int)),
                Value::RangeExclBoth(a, b) => list_items.extend((*a + 1..*b).map(Value::Int)),
                other => list_items.push(other.clone()),
            }
        }
        self.eval_map_over_items(func, list_items)
    }

    pub(super) fn builtin_grep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                Value::Array(items) => list_items.extend(items.clone()),
                Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                other => list_items.push(other.clone()),
            }
        }
        self.eval_grep_over_items(func, list_items)
    }

    pub(super) fn builtin_classify(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let func = match args.first() {
            Some(value) => value.clone(),
            None => return Ok(Value::Hash(HashMap::new())),
        };
        let mut buckets: HashMap<String, Vec<Value>> = HashMap::new();
        for item in args.iter().skip(1) {
            let keys = match self.call_lambda_with_arg(&func, item.clone()) {
                Ok(Value::Array(values)) => values,
                Ok(value) => vec![value],
                Err(_) => vec![Value::Nil],
            };
            let target_keys = if name == "classify" {
                if keys.is_empty() {
                    vec![Value::Nil]
                } else {
                    vec![keys[0].clone()]
                }
            } else {
                keys
            };
            for key in target_keys {
                let bucket_key = key.to_string_value();
                buckets.entry(bucket_key).or_default().push(item.clone());
            }
        }
        let hash_map = buckets
            .into_iter()
            .map(|(k, v)| (k, Value::Array(v)))
            .collect();
        Ok(Value::Hash(hash_map))
    }
}
